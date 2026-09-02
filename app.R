##############################################################
# app.R — "La Boleta del Estado"
# Dashboard del gasto público por departamento (MEF - Perú)
##############################################################
# Versión simplificada: KPIs arriba, mapa a la izquierda y
# "Top 15 categorías de gasto" a la derecha. Sin pestañas
# adicionales (serie histórica, tabla, etc. fueron removidas).
#
# Estructura esperada de datos (data/gasto_publico.rds):
#   DEPARTAMENTO | <categorías de gasto...> | TOTAL | ANIO | TRIMESTRE
# generado con scrape_gasto_publico() (ver scrape_gasto_publico.R)
#
# Estructura esperada del geojson (data/peru_departamentos.geojson):
#   feature$properties$NOMBDEP  -> nombre de departamento (sin tilde, mayúsculas)
##############################################################
#setwd("C:\\Users\\Usuario\\Documents\\app_gasto_publico\\app_gasto_publico")
source("global.R")

  ##############################################################
# 1. CARGA DE DATOS (una sola vez al iniciar la app)
##############################################################

ruta_datos <- "data/gasto_publico.rds"
ruta_geo   <- "data/peru_departamentos.geojson"

if (!file.exists(ruta_datos)) {
  stop(
    "No se encontró 'data/gasto_publico.rds'. ",
    "Corre primero scrape_gasto_publico() y guarda el resultado con ",
    "saveRDS(datos, 'data/gasto_publico.rds')."
  )
}

gasto <- readRDS(ruta_datos)
gasto$DEPARTAMENTO <- normalizar_departamento(gasto$DEPARTAMENTO)

# Columnas de categoría de gasto (todo lo que no sea metadata)
cols_meta <- c("DEPARTAMENTO", "TOTAL", "ANIO", "TRIMESTRE")
categorias_gasto <- setdiff(names(gasto), cols_meta)

anios_disponibles <- sort(unique(gasto$ANIO))
trimestres_disponibles <- sort(unique(gasto$TRIMESTRE))

# --- Geometría de departamentos (simplificada para que cargue rápido) ---
mapa_peru <- st_read(ruta_geo, quiet = TRUE)

# --- Geometría de departamentos ---
mapa_peru <- st_read(ruta_geo, quiet = TRUE)
mapa_peru$NOMBDEP <- normalizar_departamento(mapa_peru$NOMBDEP)

##############################################################
# 2. UI
##############################################################
# Una sola hoja: KPIs arriba (fila angosta), mapa dominante a la
# izquierda y "Top 15 categorías de gasto" a la derecha.
##############################################################

panel_controles <- sidebar(
  width = 280,
  class = "panel-control",
  bg = "transparent",
  open = "always",
  
  selectInput(
    "anio", "Año",
    choices = anios_disponibles,
    selected = max(anios_disponibles)
  ),
  
  selectInput(
    "trimestre", "Trimestre",
    choices = trimestres_disponibles,
    selected = max(trimestres_disponibles)
  ),
  
  selectInput(
    "categoria", "Categoría de gasto",
    choices = c("TOTAL (todas las categorías)" = "TOTAL", categorias_gasto),
    selected = "TOTAL"
  ),
  
  hr(style = "border-color:#DDD5C7;"),
  
  radioButtons(
    "vista_mapa", "Mostrar en el mapa",
    choices = c("Monto ejecutado" = "monto",
                "Variación vs. periodo anterior" = "variacion"),
    selected = "monto"
  ),
  
  hr(style = "border-color:#DDD5C7;"),
  
  div(
    class = "pie-fuente",
    "Los montos están expresados en soles (PEN), a valores corrientes ",
    "del periodo reportado por el MEF."
  )
)

ui <- page_fillable(
  theme = tema_boleta,
  title = "La Boleta del Estado · Gasto Público Perú",
  padding = c(14, 18, 10, 18),
  fillable_mobile = TRUE,
  
  div(
    class = "membrete",
    div(class = "membrete-eyebrow", "TRANSPARENCIA ECONÓMICA · MEF"),
    div(
      class = "membrete-sub",
      "Gasto público ejecutado por departamento, función y periodo. ",
      "Fuente: Portal de Transparencia Económica, Ministerio de Economía y Finanzas del Perú."
    )
  ),
  
  layout_sidebar(
    sidebar = panel_controles,
    fillable = TRUE,
    
    # --- Fila de KPIs (angosta, fija) ---
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      height = "118px",
      uiOutput("kpi_total"),
      uiOutput("kpi_departamento_top"),
      uiOutput("kpi_departamento_bottom"),
      uiOutput("kpi_promedio")
    ),
    
    # --- Mapa dominante + Top 15 categorías al costado ---
    layout_columns(
      col_widths = c(8, 4),
      height = "calc(100% - 130px)",
      
      card(
        full_screen = TRUE,
        card_header("Distribución territorial del gasto"),
        leafletOutput("mapa", height = "100%")
      ),
      
      card(
        full_screen = TRUE,
        card_header("Top 15 categorías de gasto"),
        plotOutput("composicion", height = "100%")
      )
    )
  )
)

##############################################################
# 3. SERVER
##############################################################

server <- function(input, output, session) {
  
  ##########################################################
  # 3.1 Datos reactivos base
  ##########################################################
  
  # Periodo seleccionado actualmente
  datos_periodo <- reactive({
    gasto %>%
      filter(ANIO == as.numeric(input$anio), TRIMESTRE == input$trimestre)
  })
  
  # Periodo inmediatamente anterior disponible en los datos (para variación)
  periodo_anterior <- reactive({
    secuencia <- gasto %>%
      distinct(ANIO, TRIMESTRE) %>%
      mutate(orden = ANIO * 10 + as.numeric(gsub("Q", "", TRIMESTRE))) %>%
      arrange(orden)
    
    actual_orden <- as.numeric(input$anio) * 10 + as.numeric(gsub("Q", "", input$trimestre))
    candidatos <- secuencia %>% filter(orden < actual_orden)
    
    if (nrow(candidatos) == 0) return(NULL)
    
    fila <- candidatos %>% filter(orden == max(orden))
    
    gasto %>% filter(ANIO == fila$ANIO[1], TRIMESTRE == fila$TRIMESTRE[1])
  })
  
  # Columna de monto según categoría elegida, ya resuelta a un solo valor
  # numérico por departamento para el periodo seleccionado
  monto_por_departamento <- reactive({
    req(input$categoria)
    df <- datos_periodo()
    col <- input$categoria
    df %>%
      transmute(DEPARTAMENTO, MONTO = .data[[col]])
  })
  
  monto_por_departamento_anterior <- reactive({
    req(input$categoria)
    df <- periodo_anterior()
    if (is.null(df)) return(NULL)
    col <- input$categoria
    df %>%
      transmute(DEPARTAMENTO, MONTO = .data[[col]])
  })
  
  # Variación porcentual vs. periodo anterior, por departamento
  variacion_por_departamento <- reactive({
    actual <- monto_por_departamento()
    anterior <- monto_por_departamento_anterior()
    
    if (is.null(anterior)) {
      return(actual %>% mutate(VARIACION = NA_real_))
    }
    
    actual %>%
      left_join(anterior, by = "DEPARTAMENTO", suffix = c("", "_ANT")) %>%
      mutate(
        VARIACION = ifelse(
          is.na(MONTO_ANT) | MONTO_ANT == 0,
          NA_real_,
          (MONTO - MONTO_ANT) / MONTO_ANT * 100
        )
      ) %>%
      select(DEPARTAMENTO, MONTO, VARIACION)
  })
  
  ##########################################################
  # 3.2 KPIs (tarjetas tipo "línea de boleta")
  ##########################################################
  
  kpi_card <- function(label, value, delta_html = NULL) {
    div(
      class = "kpi-card",
      div(class = "kpi-label", label),
      div(class = "kpi-value", value),
      if (!is.null(delta_html)) delta_html
    )
  }
  
  output$kpi_total <- renderUI({
    total <- sum(monto_por_departamento()$MONTO, na.rm = TRUE)
    kpi_card(
      paste0("GASTO TOTAL · ", input$anio, " ", input$trimestre),
      fmt_soles_compacto(total)
    )
  })
  
  output$kpi_departamento_top <- renderUI({
    df <- monto_por_departamento() %>% arrange(desc(MONTO)) %>% slice(1)
    kpi_card(
      "MAYOR EJECUCIÓN",
      df$DEPARTAMENTO,
      div(class = "kpi-delta", fmt_soles_compacto(df$MONTO))
    )
  })
  
  output$kpi_departamento_bottom <- renderUI({
    df <- monto_por_departamento() %>%
      filter(MONTO > 0) %>%
      arrange(MONTO) %>%
      slice(1)
    kpi_card(
      "MENOR EJECUCIÓN",
      df$DEPARTAMENTO,
      div(class = "kpi-delta", fmt_soles_compacto(df$MONTO))
    )
  })
  
  output$kpi_promedio <- renderUI({
    var_df <- variacion_por_departamento()
    var_prom <- mean(var_df$VARIACION, na.rm = TRUE)
    promedio_monto <- mean(monto_por_departamento()$MONTO, na.rm = TRUE)
    
    if (is.nan(var_prom) || is.na(var_prom)) {
      valor_principal <- "—"
      delta <- div(class = "kpi-delta", "Sin periodo previo para comparar")
    } else {
      clase <- if (var_prom >= 0) "up" else "down"
      signo <- if (var_prom >= 0) "+" else ""
      valor_principal <- paste0(signo, round(var_prom, 1), "%")
      delta <- div(
        class = paste("kpi-delta", clase),
        paste("Promedio departamental:", fmt_soles_compacto(promedio_monto))
      )
    }
    
    kpi_card(
      "VARIACIÓN PROMEDIO",
      valor_principal,
      delta
    )
  })
  
  ##########################################################
  # 3.3 Mapa (leaflet)
  ##########################################################
  
  output$mapa <- renderLeaflet({
    
    if (input$vista_mapa == "monto") {
      datos_mapa <- monto_por_departamento()
      etiqueta_valor <- fmt_soles_compacto
      titulo_leyenda <- "Monto ejecutado"
    } else {
      datos_mapa <- variacion_por_departamento() %>% select(DEPARTAMENTO, MONTO = VARIACION)
      etiqueta_valor <- function(x) paste0(round(x, 1), "%")
      titulo_leyenda <- "Variación vs. periodo anterior"
    }
    
    geo <- mapa_peru %>%
      left_join(datos_mapa, by = c("NOMBDEP" = "DEPARTAMENTO"))
    
    paleta <- colorNumeric(
      palette = if (input$vista_mapa == "monto") {
        PALETA_MAPA(100)
      } else {
        colorRampPalette(c(PAL$cobre, "#F3E9DD", PAL$oliva))(100)
      },
      domain = geo$MONTO,
      na.color = "#E5E0D6"
    )
    
    etiquetas <- sprintf(
      "<div class='popup-boleta'><div class='popup-depto'>%s</div><div class='popup-monto'>%s</div></div>",
      geo$NOMBDEP,
      sapply(geo$MONTO, function(v) if (is.na(v)) "Sin datos" else etiqueta_valor(v))
    ) %>% lapply(htmltools::HTML)
    
    leaflet(geo, options = leafletOptions(zoomControl = TRUE, attributionControl = FALSE)) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      setView(lng = -75.5, lat = -9.2, zoom = 5) %>%
      addPolygons(
        fillColor = ~paleta(MONTO),
        fillOpacity = 0.85,
        color = PAL$tinta,
        weight = 0.6,
        opacity = 0.6,
        highlightOptions = highlightOptions(
          weight = 2, color = PAL$cobre, fillOpacity = 0.95, bringToFront = TRUE
        ),
        label = etiquetas,
        labelOptions = labelOptions(
          style = list("font-family" = "Inter"),
          textsize = "12px"
        )
      ) %>%
      addLegend(
        position = "bottomright",
        pal = paleta,
        values = ~MONTO,
        title = titulo_leyenda,
        labFormat = labelFormat(suffix = if (input$vista_mapa == "variacion") "%" else "")
      )
  })
  
  ##########################################################
  # 3.4 Top 15 categorías de gasto (barras horizontales)
  ##########################################################
  
  output$composicion <- renderPlot({
    df <- datos_periodo() %>%
      select(DEPARTAMENTO, all_of(categorias_gasto)) %>%
      pivot_longer(-DEPARTAMENTO, names_to = "CATEGORIA", values_to = "MONTO") %>%
      group_by(CATEGORIA) %>%
      summarise(MONTO = sum(MONTO, na.rm = TRUE), .groups = "drop") %>%
      filter(MONTO > 0) %>%
      arrange(desc(MONTO)) %>%
      mutate(
        CATEGORIA = factor(CATEGORIA, levels = rev(CATEGORIA)),
        PARTICIPACION = MONTO / sum(MONTO)
      ) %>%
      slice_head(n = 15)
    
    ggplot2::ggplot(df, ggplot2::aes(x = MONTO, y = CATEGORIA)) +
      ggplot2::geom_col(fill = PAL$oliva, width = 0.7) +
      ggplot2::geom_text(
        ggplot2::aes(label = scales::percent(PARTICIPACION, accuracy = 0.1)),
        hjust = -0.1, size = 5, family = "mono", color = PAL$tinta
      ) +
      ggplot2::scale_x_continuous(labels = fmt_soles_compacto, expand = c(0, 0, 0.18, 0)) +
      ggplot2::labs(x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_family = "sans", base_size = 10) +
      ggplot2::theme(
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor    = ggplot2::element_blank(),
        panel.grid.major.x  = ggplot2::element_line(color = PAL$linea, linewidth = 0.3),
        plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
      )
  }, bg = "transparent")
}

##############################################################
# 4. RUN APP
##############################################################

shinyApp(ui = ui, server = server)