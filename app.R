##############################################################
# app.R — Dashboard de Seguimiento de Gasto Público (MEF - Perú)
##############################################################

source("global.R")
library(shinycssloaders) # Para spinners de carga elegantes

##############################################################
# 1. CARGA Y PREPARACIÓN DE DATOS
##############################################################

ruta_datos <- "data/gasto_publico.rds"

if (!file.exists(ruta_datos)) {
  stop("No se encontró 'data/gasto_publico.rds'. Asegúrate de generar el RDS.")
}

gasto_raw <- readRDS(ruta_datos)

# Limpieza básica de nombres de departamento
gasto_raw$DEPARTAMENTO <- toupper(trimws(gasto_raw$DEPARTAMENTO))

# Identificación de columnas meta y categorías de gasto
COLS_META  <- c("DEPARTAMENTO", "ANIO", "TRIMESTRE", "TOTAL")
CATS_GASTO <- setdiff(names(gasto_raw), COLS_META) # Las 25 funciones/categorías

ANIOS_DISP <- sort(unique(gasto_raw$ANIO))
TRIMS_DISP <- sort(unique(gasto_raw$TRIMESTRE))

##############################################################
# 2. UI (INTERFAZ DE USUARIO)
##############################################################

ui <- fluidPage(
  theme = tema_dashboard,
  tags$head(
    tags$link(rel = "stylesheet", href = "estilos.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel  = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap"
    )
  ),
  
  div(class = "pagina-completa",
      
      # ── BARRA SUPERIOR / FILTROS ──────────────────────────────
      div(class = "barra-filtros",
          
          div(class = "filtro-grupo",
              tags$label("Año"),
              selectInput("anio", NULL, choices = ANIOS_DISP, selected = max(ANIOS_DISP), width = "110px")
          ),
          
          div(class = "filtro-grupo",
              tags$label("Trimestre"),
              selectInput("trimestre", NULL, choices = TRIMS_DISP, selected = max(TRIMS_DISP), width = "90px")
          ),
          
          div(class = "filtro-grupo",
              tags$label("Categoría / Función"),
              selectInput(
                "categoria", NULL, 
                choices = c("Todas las categorías (TOTAL)" = "TOTAL", sort(CATS_GASTO)), 
                selected = "TOTAL", 
                width = "250px"
              )
          ),
          
          div(class = "filtro-grupo",
              tags$label("Ordenar por"),
              selectInput("orden_ranking", NULL, choices = c("Mayor gasto" = "desc", "Menor gasto" = "asc"), selected = "desc", width = "140px")
          ),
          
          div(class = "filtro-grupo", style = "margin-top: auto; margin-bottom: 5px;",
              downloadButton("descargar_reporte", " Exportar Data", class = "btn btn-sm btn-outline-light")
          ),
          
          div(class = "titulo-dashboard",
              tags$h5("Seguimiento de Gasto Público"),
              tags$span("MEF · Transparencia Económica · Perú")
          )
      ),
      
      # ── KPIs ─────────────────────────────────────────────────
      fluidRow(
        style = "margin-bottom: 20px;",
        column(3, uiOutput("kpi_total", style = "width: 100%; display: block;")),
        column(3, uiOutput("kpi_variacion", style = "width: 100%; display: block;")),
        column(3, uiOutput("kpi_top_dept", style = "width: 100%; display: block;")),
        column(3, uiOutput("kpi_rezago", style = "width: 100%; display: block;"))
      ),
      
      # ── CUERPO PRINCIPAL ──────────────────────────────────────
      div(class = "cuerpo-principal",
          
          div(class = "panel-dash",
              div(class = "panel-header",
                  tags$p(class = "panel-titulo", "Gasto ejecutado por departamento"),
                  tags$span(class = "panel-subtitulo", "Soles corrientes · periodo seleccionado")
              ),
              div(class = "panel-body",
                  withSpinner(plotlyOutput("ranking", height = "100%"), color = "#4A9E2B", type = 4)
              )
          ),
          
          div(class = "panel-dash",
              div(class = "panel-header",
                  tags$p(class = "panel-titulo", "Semáforo de ejecución"),
                  tags$span(class = "panel-subtitulo", "vs. promedio nacional del periodo")
              ),
              div(class = "panel-body",
                  div(class = "tabla-semaforo",
                      tableOutput("semaforo")
                  )
              )
          )
      ),
      
      # ── FILA INFERIOR ─────────────────────────────────────────
      div(class = "fila-inferior",
          
          div(class = "panel-dash",
              div(class = "panel-header",
                  tags$p(class = "panel-titulo", "Evolución histórica"),
                  tags$span(class = "panel-subtitulo", "Gasto nacional agregado por trimestre")
              ),
              div(class = "panel-body",
                  withSpinner(plotlyOutput("serie", height = "100%"), color = "#4A9E2B", type = 4)
              )
          ),
          
          div(class = "panel-dash",
              div(class = "panel-header",
                  tags$p(class = "panel-titulo", "Composición del gasto (Funciones)"),
                  tags$span(class = "panel-subtitulo", "Top 10 categorías · periodo seleccionado")
              ),
              div(class = "panel-body",
                  withSpinner(plotlyOutput("categorias", height = "100%"), color = "#4A9E2B", type = 4)
              )
          )
      )
  )
)

##############################################################
# SERVER CON VARIACIÓN ANUAL Y ESTADOS REVISADOS
##############################################################

server <- function(input, output, session) {
  
  # ── Reactivos de Selección de Periodo ─────────────────────
  periodo_actual <- reactive({
    res <- gasto_raw %>%
      filter(ANIO == as.numeric(input$anio),
             TRIMESTRE == input$trimestre)
    validate(need(nrow(res) > 0, "No se encontraron datos para el periodo seleccionado."))
    res
  })
  
  # Variación Homogénea: Mismo trimestre del año anterior (YoY)
  periodo_anio_anterior <- reactive({
    anio_previo <- as.numeric(input$anio) - 1
    trim_actual <- input$trimestre
    
    res <- gasto_raw %>%
      filter(ANIO == anio_previo, TRIMESTRE == trim_actual)
    
    if (nrow(res) == 0) return(NULL)
    res
  })
  
  col_activa <- reactive({
    req(input$categoria)
    input$categoria
  })
  
  # ── Limpieza de Datos: Filtro Excluyendo 'TOTAL' ───────────
  montos_actual <- reactive({
    req(col_activa())
    periodo_actual() %>%
      transmute(DEPARTAMENTO, MONTO = .data[[col_activa()]]) %>%
      filter(!is.na(MONTO), MONTO >= 0, !toupper(DEPARTAMENTO) %in% c("TOTAL", "TOTAL GENERAL", "NACIONAL"))
  })
  
  montos_anio_anterior <- reactive({
    df <- periodo_anio_anterior()
    if (is.null(df)) return(NULL)
    req(col_activa())
    df %>%
      transmute(DEPARTAMENTO, MONTO = .data[[col_activa()]]) %>%
      filter(!is.na(MONTO), MONTO >= 0, !toupper(DEPARTAMENTO) %in% c("TOTAL", "TOTAL GENERAL", "NACIONAL"))
  })
  
  # ── Cálculo Centralizado de Métricas / Umbrales ───────────
  metricas_periodo <- reactive({
    df <- montos_actual()
    req(nrow(df) > 0)
    
    promedio     <- mean(df$MONTO, na.rm = TRUE)
    mediana      <- median(df$MONTO, na.rm = TRUE)
    umbral_low   <- promedio * 0.50 # Umbral bajo (<50% del promedio regional)
    umbral_high  <- promedio * 1.25 # Umbral alto (>125% del promedio regional)
    
    list(
      df          = df,
      promedio    = promedio,
      mediana     = mediana,
      umbral_low  = umbral_low,
      umbral_high = umbral_high
    )
  })
  
  # ── Exportación de Reporte ────────────────────────────────
  output$descargar_reporte <- downloadHandler(
    filename = function() {
      paste0("gasto_publico_", input$anio, "_", input$trimestre, ".csv")
    },
    content = function(file) {
      write.csv(periodo_actual(), file, row.names = FALSE)
    }
  )
  
  # ── Helper UI para KPIs ───────────────────────────────────
  # ── Helper UI para KPIs (Optimizado para Web / ShinyApps) ─────────
  kpi_ui <- function(etiqueta, numero, sub = NULL, sub_clase = NULL, acento = PAL$azul_claro) {
    div(
      class = "kpi-card",
      style = paste0(
        "border-left: 4px solid ", acento, "; ",
        "background-color: #1A1D24; ",
        "border-radius: 6px; ",
        "padding: 16px; ",
        "min-height: 110px; ",
        "display: flex; ",
        "flex-direction: column; ",
        "justify-content: space-between; ",
        "box-shadow: 0 4px 6px rgba(0, 0, 0, 0.2); ",
        "margin-bottom: 12px;" # Espaciado si la pantalla se vuelve pequeña
      ),
      div(
        style = "font-size: 0.78rem; color: #9DA8B6; text-transform: uppercase; letter-spacing: 0.5px; font-weight: 600;",
        etiqueta
      ),
      div(
        style = ifelse(nchar(numero) > 8, 
                       "font-size: 1.3rem; font-weight: 700; color: #FFFFFF; margin: 6px 0;", 
                       "font-size: 1.6rem; font-weight: 700; color: #FFFFFF; margin: 6px 0;"),
        numero
      ),
      if (!is.null(sub)) {
        col_sub <- case_when(
          sub_clase == "verde" ~ PAL$verde,
          sub_clase == "rojo"  ~ PAL$rojo,
          sub_clase == "ambar" ~ PAL$ambar,
          TRUE                ~ PAL$texto_suave
        )
        div(
          style = paste0("font-size: 0.8rem; font-weight: 500; color: ", col_sub, ";"),
          sub
        )
      }
    )
  }
  
  # ── KPIs ──────────────────────────────────────────────────
  output$kpi_total <- renderUI({
    total <- sum(montos_actual()$MONTO, na.rm = TRUE)
    label <- if(input$categoria == "TOTAL") "Gasto Total Ejecutado" else paste("Gasto en", input$categoria)
    kpi_ui(label, fmt_millon(total),
           sub = paste(input$anio, input$trimestre), acento = PAL$azul_claro)
  })
  
  # KPI Modificado: Comparación vs. Mismo Trimestre del Año Anterior
  output$kpi_variacion <- renderUI({
    total_act <- sum(montos_actual()$MONTO, na.rm = TRUE)
    df_ant    <- montos_anio_anterior()
    
    if (is.null(df_ant)) {
      return(kpi_ui("Variación vs. año anterior", "—", 
                    sub = paste("Sin datos para", input$trimestre, as.numeric(input$anio) - 1), 
                    acento = PAL$texto_suave))
    }
    
    total_ant <- sum(df_ant$MONTO, na.rm = TRUE)
    var_pct   <- if(total_ant > 0) (total_act - total_ant) / total_ant * 100 else 0
    signo     <- if (var_pct >= 0) "+" else ""
    color     <- if (var_pct >= 0) PAL$verde else PAL$rojo
    clase     <- if (var_pct >= 0) "verde" else "rojo"
    
    kpi_ui("Variación vs. año anterior",
           paste0(signo, round(var_pct, 1), "%"),
           sub       = paste0(input$trimestre, " ", as.numeric(input$anio) - 1, ": ", fmt_millon(total_ant)),
           sub_clase = clase, acento = color)
  })
  
  output$kpi_top_dept <- renderUI({
    df <- montos_actual() %>% arrange(desc(MONTO)) %>% slice(1)
    validate(need(nrow(df) > 0, ""))
    kpi_ui("Región con mayor gasto", df$DEPARTAMENTO, sub = fmt_millon(df$MONTO), acento = PAL$verde)
  })
  
  # KPI Modificado: Regiones en Nivel Bajo (<50% del promedio)
  output$kpi_rezago <- renderUI({
    m         <- metricas_periodo()
    cant_bajo <- m$df %>% filter(MONTO < m$umbral_low) %>% nrow()
    
    clase <- if (cant_bajo == 0) "verde" else if (cant_bajo <= 3) "ambar" else "rojo"
    color <- if (cant_bajo == 0) PAL$verde else if (cant_bajo <= 3) PAL$ambar else PAL$rojo
    
    kpi_ui("Regiones con nivel bajo", as.character(cant_bajo),
           sub = paste0("Gasto inferior a ", fmt_millon(m$umbral_low), " (<50% prom)"), 
           sub_clase = clase, acento = color)
  })
  
  # ── GRÁFICO 1: Ranking Departamental con Zonas de Ejecución ─
  output$ranking <- renderPlotly({
    m  <- metricas_periodo()
    df <- m$df %>% arrange(if (input$orden_ranking == "desc") MONTO else desc(MONTO))
    
    df <- df %>%
      mutate(
        COLOR = case_when(
          MONTO < m$umbral_low   ~ PAL$rojo,
          MONTO >= m$umbral_high ~ PAL$verde,
          TRUE                   ~ PAL$azul_claro
        ),
        TEXTO  = fmt_millon_gg(MONTO),
        DEPT_F = factor(DEPARTAMENTO, levels = DEPARTAMENTO)
      )
    
    p <- plot_ly(df,
                 x           = ~MONTO,
                 y           = ~DEPT_F,
                 type        = "bar",
                 orientation = "h",
                 showlegend  = FALSE,
                 name        = "Gasto Regional",
                 marker      = list(color = ~COLOR, line = list(width = 0)),
                 text        = ~TEXTO,
                 textposition = "outside",
                 textfont    = list(color = "#E8EDF2", size = 10),
                 hovertemplate = paste0("<b>%{y}</b><br>Gasto: %{text}<br><extra></extra>")
    ) %>%
      # Línea Vertical: Promedio Nacional
      add_segments(
        x = m$promedio, xend = m$promedio,
        y = 0, yend = nrow(df) + 1,
        line = list(color = PAL$ambar, width = 1.5, dash = "dash"),
        name = "Promedio Nacional", inherit = FALSE
      ) %>%
      # Línea Vertical: Umbral Bajo (50%)
      add_segments(
        x = m$umbral_low, xend = m$umbral_low,
        y = 0, yend = nrow(df) + 1,
        line = list(color = PAL$rojo, width = 1.5, dash = "dot"),
        name = "Umbral Bajo (50%)", inherit = FALSE
      ) %>%
      layout(
        paper_bgcolor = PAL$panel, plot_bgcolor = PAL$panel,
        margin = list(l = 5, r = 60, t = 10, b = 10),
        xaxis = list(color = PAL$texto_suave, gridcolor = PAL$borde, tickfont = list(size = 9), showgrid = TRUE),
        yaxis = list(color = PAL$texto, tickfont = list(size = 9), showgrid = FALSE, automargin = TRUE),
        showlegend = TRUE,
        legend = list(x = 0.55, y = 0.05, font = list(color = PAL$texto_suave, size = 9), bgcolor = "rgba(0,0,0,0.5)"),
        bargap = 0.3,
        shapes = list(
          list(
            type = "rect",
            x0 = 0, x1 = m$umbral_low,
            y0 = 0, y1 = nrow(df) + 1,
            fillcolor = PAL$rojo, opacity = 0.08,
            line = list(width = 0)
          )
        )
      ) %>%
      config(displayModeBar = FALSE)
    
    p
  })
  
  # ── TABLA: Semáforo de Ejecución Regional ───────────────
  output$semaforo <- renderTable({
    m       <- metricas_periodo()
    df      <- m$df
    df_ant  <- montos_anio_anterior()
    
    # Nuevas Categorías: ALTO / MEDIO / BAJO
    df <- df %>%
      arrange(desc(MONTO)) %>%
      mutate(
        Estado = case_when(
          MONTO < m$umbral_low   ~ paste0("<span style='color:", PAL$rojo, ";'>● BAJO</span>"),
          MONTO >= m$umbral_high ~ paste0("<span style='color:", PAL$verde, ";'>● ALTO</span>"),
          TRUE                   ~ paste0("<span style='color:", PAL$azul_claro, ";'>● MEDIO</span>")
        )
      )
    
    if (!is.null(df_ant)) {
      df <- df %>%
        left_join(df_ant %>% rename(MONTO_ANT = MONTO), by = "DEPARTAMENTO") %>%
        mutate(
          VAR_PCT = round((MONTO - MONTO_ANT) / MONTO_ANT * 100, 1),
          `Var. % (YoY)` = ifelse(is.na(VAR_PCT), "—", paste0(ifelse(VAR_PCT >= 0, "+", ""), VAR_PCT, "%"))
        )
    } else {
      df$`Var. % (YoY)` <- "—"
    }
    
    df %>%
      transmute(
        Departamento  = DEPARTAMENTO,
        `Gasto (S/M)` = round(MONTO / 1e6, 1),
        `Var. % (YoY)`,
        Estado
      )
  }, sanitize.text.function = identity, hover = TRUE, bordered = FALSE, width = "100%", na = "—")
  
  # ── GRÁFICO 2: Evolución Histórica (Mismo Trimestre) ──────
  output$serie <- renderPlotly({
    req(col_activa(), input$trimestre)
    col <- col_activa()
    trim_sel <- input$trimestre
    
    serie <- gasto_raw %>%
      filter(!toupper(DEPARTAMENTO) %in% c("TOTAL", "TOTAL GENERAL", "NACIONAL")) %>%
      filter(TRIMESTRE == trim_sel) %>%
      mutate(PERIODO_LAB = paste0(ANIO, " ", TRIMESTRE)) %>%
      group_by(ANIO, TRIMESTRE, PERIODO_LAB) %>%
      summarise(MONTO = sum(.data[[col]], na.rm = TRUE), .groups = "drop") %>%
      arrange(ANIO)
    
    validate(need(nrow(serie) > 0, paste("Sin datos históricos para el", trim_sel)))
    
    anio_sel <- as.numeric(input$anio)
    
    plot_ly(serie) %>%
      add_trace(
        x = ~ANIO, y = ~MONTO, type = "scatter", mode = "lines+markers",
        line = list(color = PAL$azul_claro, width = 2),
        marker = list(color = PAL$azul_claro, size = 6, line = list(color = PAL$panel, width = 1.5)),
        fill = "tozeroy", fillcolor = "rgba(74,158,219,0.08)",
        hovertemplate = paste0("<b>%{customdata}</b><br>Gasto: S/ %{y:,.0f}<br><extra></extra>"),
        customdata = ~PERIODO_LAB, name = paste("Histórico", trim_sel)
      ) %>%
      add_trace(
        data = serie %>% filter(ANIO == anio_sel),
        x = ~ANIO, y = ~MONTO, type = "scatter", mode = "markers",
        marker = list(color = PAL$verde, size = 11, line = list(color = PAL$panel, width = 2)),
        hovertemplate = paste0("<b>Año seleccionado (%{customdata})</b><br>Gasto: S/ %{y:,.0f}<br><extra></extra>"),
        customdata = ~PERIODO_LAB, name = "Selección actual"
      ) %>%
      layout(
        paper_bgcolor = PAL$panel, plot_bgcolor = PAL$panel,
        margin = list(l = 5, r = 10, t = 10, b = 10),
        xaxis = list(
          title = "",
          color = PAL$texto_suave, gridcolor = PAL$borde, tickfont = list(size = 9),
          dtick = 1
        ),
        yaxis = list(color = PAL$texto_suave, gridcolor = PAL$borde, tickfont = list(size = 9), tickformat = ",.0f"),
        showlegend = TRUE,
        legend = list(x = 0.01, y = 0.99, font = list(color = PAL$texto_suave, size = 9), bgcolor = "rgba(0,0,0,0)")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── GRÁFICO 3: Composición del Gasto (Top 10 Categorías) ──
  output$categorias <- renderPlotly({
    df_periodo_limpio <- periodo_actual() %>%
      filter(!toupper(DEPARTAMENTO) %in% c("TOTAL", "TOTAL GENERAL", "NACIONAL"))
    
    df <- df_periodo_limpio %>%
      select(DEPARTAMENTO, all_of(CATS_GASTO)) %>%
      pivot_longer(-DEPARTAMENTO, names_to = "CATEGORIA", values_to = "MONTO") %>%
      group_by(CATEGORIA) %>%
      summarise(MONTO = sum(MONTO, na.rm = TRUE), .groups = "drop") %>%
      filter(MONTO > 0) %>%
      arrange(MONTO) %>%
      slice_tail(n = 10) %>%
      mutate(
        PARTICIPACION = MONTO / sum(df_periodo_limpio$TOTAL, na.rm = TRUE),
        TEXTO = paste0(round(PARTICIPACION * 100, 1), "%"),
        CAT_CORTA = stringr::str_trunc(CATEGORIA, 28, ellipsis = "…"),
        CAT_F = factor(CAT_CORTA, levels = CAT_CORTA)
      )
    
    validate(need(nrow(df) > 0, "Sin datos de categorías para mostrar."))
    
    plot_ly(df,
            x = ~MONTO, y = ~CAT_F, type = "bar", orientation = "h",
            marker = list(color = PAL$verde, opacity = 0.85),
            text = ~TEXTO, textposition = "outside",
            textfont = list(color = PAL$texto_suave, size = 10),
            hovertemplate = paste0("<b>%{y}</b><br>Gasto: S/ %{x:,.0f}<br>Participación nacional: %{text}<br><extra></extra>")
    ) %>%
      layout(
        paper_bgcolor = PAL$panel, plot_bgcolor = PAL$panel,
        margin = list(l = 5, r = 55, t = 10, b = 10),
        xaxis = list(color = PAL$texto_suave, gridcolor = PAL$borde, tickfont = list(size = 9), tickformat = ",.0f"),
        yaxis = list(color = PAL$texto, tickfont = list(size = 9), automargin = TRUE),
        bargap = 0.3, showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
}

##############################################################
# 4. LANZAMIENTO
##############################################################

shinyApp(ui = ui, server = server)
