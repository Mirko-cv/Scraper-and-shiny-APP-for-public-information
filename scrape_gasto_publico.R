library(rvest)
library(dplyr)
library(purrr)

##############################################################
# FUNCION: scrape_gasto_publico
##############################################################
# Descarga y consolida los reportes de gasto público por
# departamento (FUA) del portal de Transparencia del MEF
# (https://apps5.mineco.gob.pe/transparencia/...).
#
# Parámetros:
#   anios       : vector numérico con los años a descargar, ej. c(2022, 2023)
#   trimestres  : vector numérico con los trimestres a descargar (1,2,3,4).
#                 Por defecto descarga los 4. Internamente cada trimestre
#                 se traduce al mes que usa la URL del MEF:
#                   Q1 -> t=3   Q2 -> t=6   Q3 -> t=9   Q4 -> t=12
#   pausa_seg   : segundos de espera entre cada request (para no saturar
#                 el servidor). Por defecto 1 segundo.
#   verbose     : si TRUE (default), imprime mensajes de progreso.
#
# Retorna:
#   Un data.frame único con todos los años/trimestres solicitados,
#   con columnas: DEPARTAMENTO, <pliegos/categorías>..., TOTAL,
#   ANIO, TRIMESTRE.
#
# Ejemplo de uso:
#   datos <- scrape_gasto_publico(anios = 2025:2026)
#   datos <- scrape_gasto_publico(anios = 2023, trimestres = c(2, 4))
##############################################################

scrape_gasto_publico <- function(anios,
                                  trimestres = 1:4,
                                  pausa_seg = 1,
                                  verbose = TRUE) {

  # --- Validaciones básicas ---
  if (missing(anios) || length(anios) == 0) {
    stop("Debes indicar al menos un año en 'anios', ej. anios = 2023")
  }
  if (!all(trimestres %in% 1:4)) {
    stop("'trimestres' solo puede contener valores entre 1 y 4")
  }

  # Mapeo trimestre -> mes usado en la URL del MEF
  mapa_mes <- c(`1` = 3, `2` = 6, `3` = 9, `4` = 12)

  # Todas las combinaciones año-trimestre a descargar
  combinaciones <- expand.grid(anio = anios, trimestre = trimestres) %>%
    arrange(anio, trimestre)

  # --- Función interna: descarga y parsea UN año-trimestre ---
  descargar_uno <- function(anio, trimestre) {

    mes <- mapa_mes[[as.character(trimestre)]]
    url <- sprintf(
      "https://apps5.mineco.gob.pe/transparencia/Reportes/RptDEPxFUA.aspx?y=%d&t=%d",
      anio, mes
    )

    if (verbose) {
      message(sprintf("Descargando %d/Q%d ... ", anio, trimestre), appendLF = FALSE)
    }

    resultado <- tryCatch({

      pagina <- read_html(url)

      tabla_cruda <- pagina %>%
        html_nodes("#Pnl0 td") %>%
        html_text()

      # Quitamos el segundo elemento (celda vacía/duplicada que trae el reporte)
      tabla_cruda <- tabla_cruda[-2]

      n_col <- 27
      df <- tabla_cruda %>%
        matrix(ncol = n_col, byrow = TRUE) %>%
        as.data.frame(stringsAsFactors = FALSE)

      # Nombres de columna: quitamos el prefijo de 4 caracteres que trae
      # el encabezado (ej. "001 Categoria" -> "Categoria")
      nombres_cols <- c("DEPARTAMENTO", substring(df[1, 2:(n_col - 1)], 5), "TOTAL")
      names(df) <- nombres_cols

      # Limpiamos el prefijo también en la columna DEPARTAMENTO
      df[, 1] <- substring(df[, 1], 5)

      # Quitamos la fila de encabezado (ya usada para nombres) y convertimos
      # a numérico todas las columnas excepto DEPARTAMENTO
      df <- df[-1, ] %>%
        mutate(across(
          !matches("DEPARTAMENTO"),
          ~ as.numeric(gsub(",", "", .))
        )) %>%
        mutate(ANIO = anio, TRIMESTRE = paste0("Q", trimestre))

      if (verbose) message("OK (", nrow(df), " filas)")
      df

    }, error = function(e) {
      if (verbose) message("FALLÓ -> ", conditionMessage(e))
      NULL
    })

    Sys.sleep(pausa_seg)
    resultado
  }

  # --- Recorremos todas las combinaciones ---
  lista_resultados <- pmap(
    list(combinaciones$anio, combinaciones$trimestre),
    descargar_uno
  )

  # Quitamos los que fallaron (NULL) y unimos todo
  lista_resultados <- lista_resultados[!sapply(lista_resultados, is.null)]

  if (length(lista_resultados) == 0) {
    warning("No se pudo descargar ningún año/trimestre solicitado.")
    return(data.frame())
  }

  datos_finales <- bind_rows(lista_resultados)

  if (verbose) {
    message(sprintf(
      "\nListo: %d combinación(es) año/trimestre descargadas correctamente.",
      length(lista_resultados)
    ))
  }

  datos_finales
}

##############################################################
# EJEMPLOS DE USO
##############################################################

# 1) Descargar solo el 4to trimestre de 2023 (equivalente al script original)
# datos_2023_q4 <- scrape_gasto_publico(anios = 2023, trimestres = 4)

# 2) Descargar todos los trimestres de varios años
# datos_2021_2023 <- scrape_gasto_publico(anios = 2021:2023)

# 3) Descargar trimestres específicos de varios años
# datos_mixto <- scrape_gasto_publico(anios = c(2022, 2023), trimestres = c(1,2,3, 4))

#Prueba:
#scrape_gasto_publico(anios = 2021:2026) -> data
# data %>% filter(!(ANIO == 2026 & TRIMESTRE %in% c("Q3","Q4") )) -> data1

saveRDS(data1,".\\data\\gasto_publico.rds")
