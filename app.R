##############################################################
# app.R — Dashboard de Seguimiento de Gasto Público (MEF - Perú)
# Monito Presupuestal
##############################################################

source("global.R")
library(shinycssloaders)
library(stringr)

##############################################################
# 1. CARGA Y PREPARACIÓN DE DATOS
##############################################################

ruta_datos <- "data/gasto_publico.rds"
if (!file.exists(ruta_datos))
  stop("No se encontró 'data/gasto_publico.rds'.")

gasto_raw <- readRDS(ruta_datos)
gasto_raw$DEPARTAMENTO <- toupper(trimws(gasto_raw$DEPARTAMENTO))

DEPTS_DISP <- sort(unique(gasto_raw$DEPARTAMENTO[
  !gasto_raw$DEPARTAMENTO %in% c("TOTAL","TOTAL GENERAL","NACIONAL")]))
COLS_META  <- c("DEPARTAMENTO","ANIO","TRIMESTRE","TOTAL")
CATS_GASTO <- setdiff(names(gasto_raw), COLS_META)
ANIOS_DISP <- sort(unique(gasto_raw$ANIO))
TRIMS_DISP <- sort(unique(gasto_raw$TRIMESTRE))

##############################################################
# 2. HELPERS COMPARTIDOS
##############################################################

fmt_millon <- function(x)
  paste0("S/ ", scales::comma(round(x / 1e6, 1)), "M")

fmt_millon_gg <- function(x)
  dplyr::case_when(
    abs(x) >= 1e9 ~ paste0("S/ ", round(x/1e9,1), "B"),
    abs(x) >= 1e6 ~ paste0("S/ ", round(x/1e6,1), "M"),
    TRUE          ~ paste0("S/ ", scales::comma(round(x/1e3,0)), "K"))

# Estilos reutilizables
PANEL_STYLE <- paste0(
  "background:#1A1D24; border-radius:8px; padding:16px; ",
  "margin-bottom:20px; border:1px solid #2B303C;")

lbl_style <- paste0(
  "color:#9DA8B6; font-size:0.72rem; font-weight:700; ",
  "text-transform:uppercase; letter-spacing:0.4px; ",
  "margin-bottom:4px;")

# Encabezado de sección reutilizable (UI estática)
panel_header <- function(titulo, subtitulo) {
  div(style = paste0("margin-bottom:12px; border-bottom:1px solid #2B303C; ",
                     "padding-bottom:8px;"),
      div(style = "color:#FFFFFF; font-weight:700; font-size:0.92rem;",
          titulo),
      div(style = "color:#9DA8B6; font-size:0.73rem; margin-top:2px;",
          subtitulo))
}

# KPI card
kpi_card <- function(etiqueta, numero, sub = NULL, acento = "#4A9E2B") {
  div(style = paste0(
    "border:1px solid #2B303C; ",          # <- primero el borde general
    "border-left:4px solid ", acento, "; ", # <- luego sobreescribe solo el izquierdo
    "background:#1A1D24; ",
    "border-radius:6px; padding:14px; ",
    "min-height:95px; margin-bottom:16px;"),
    div(style = lbl_style, etiqueta),
    div(style = "font-size:1.4rem; font-weight:700; color:#FFF; margin:4px 0;",
        numero),
    if (!is.null(sub))
      div(style = "font-size:0.73rem; color:#9DA8B6;", sub))
}

##############################################################
# 3. UI
##############################################################

ui <- fluidPage(
  theme = tema_dashboard,
  tags$head(
    tags$link(rel = "stylesheet", href = "estilos.css"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;700&display=swap"),
    tags$style(HTML("
      body { background-color:#0E1015; color:#FFF;
             font-family:'DM Sans',sans-serif; }
      /* Inputs oscuros en todos los contextos */
      .selectize-input {
        background:#242832 !important; color:#FFF !important;
        border:1px solid #323846 !important; font-size:0.82rem !important;
        min-height:32px !important; padding:4px 8px !important; }
      .selectize-dropdown {
        background:#242832 !important; color:#FFF !important; }
      .selectize-dropdown .option:hover,
      .selectize-dropdown .active { background:#323846 !important; }
      .form-group { margin-bottom:0 !important; }
      select.form-control {
        background:#242832 !important; color:#FFF !important;
        border:1px solid #323846 !important; font-size:0.82rem; }
      /* Botón CSV */
      .btn-csv { background:#242832; color:#9DA8B6; border:1px solid #323846;
                 font-size:0.72rem; padding:6px 12px; border-radius:4px;
                 cursor:pointer; width:100%; margin-top:20px; }
      .btn-csv:hover { background:#323846; color:#FFF; }
      /* Tabla semáforo */
      table { width:100%; border-collapse:collapse; font-size:0.78rem; }
      table th { color:#9DA8B6; font-size:0.68rem; font-weight:700;
                 padding:6px 8px; border-bottom:1px solid #2B303C;
                 text-align:left; }
      table td { padding:5px 8px; border-bottom:1px solid #1E2330;
                 color:#E8EDF2; }
      table tr:last-child td { border-bottom:none; }
      table tr:hover td { background:rgba(255,255,255,0.03); }
    "))
  ),
  
  div(style = "padding:18px 22px; max-width:1600px; margin:0 auto;",
      
      # ── CABECERA CON FILTROS (UI ESTÁTICA) ─────────────────────
      div(style = PANEL_STYLE,
          fluidRow(
            # Título del dashboard
            column(2,
                   div(style = "padding-top:4px;",
                       div(style = "color:#FFF; font-weight:700; font-size:1.05rem;
                         line-height:1.2;", "Monitor Presupuestal"),
                       div(style = "color:#4A9E2B; font-size:0.72rem; font-weight:600;
                         margin-top:2px;", "MEF · Transparencia Económica"))),
            
            # Año
            column(1,
                   div(style = lbl_style, "Año"),
                   selectInput("anio", NULL, ANIOS_DISP,
                               max(ANIOS_DISP), width = "100%")),
            
            # Trimestre
            column(1,
                   div(style = lbl_style, "Trimestre"),
                   selectInput("trimestre", NULL, TRIMS_DISP,
                               max(TRIMS_DISP), width = "100%")),
            
            # Categoría
            column(3,
                   div(style = lbl_style, "Categoría / Función"),
                   selectInput("categoria", NULL,
                               c("Todas las categorías (TOTAL)" = "TOTAL",
                                 sort(CATS_GASTO)),
                               "TOTAL", width = "100%")),
            
            # Departamento ranking
            column(2,
                   div(style = lbl_style, "Departamento"),
                   selectInput("dept_ranking", NULL,
                               c("Todos (Nacional)" = "TODOS", DEPTS_DISP),
                               "TODOS", width = "100%")),
            
            # Ordenar
            column(1,
                   div(style = lbl_style, "Ordenar"),
                   selectInput("orden_ranking", NULL,
                               c("Mayor → menor" = "desc", "Menor → mayor" = "asc"),
                               "desc", width = "100%")),
            
            # CSV
            column(1,
                   div(style = lbl_style, "Exportar"),
                   downloadButton("descargar_reporte", "↓ CSV",
                                  class = "btn-csv"))
          )
      ),
      
      # ── KPIs ─────────────────────────────────────────────────
      fluidRow(style = "margin-bottom:4px;",
               column(3, uiOutput("kpi_total")),
               column(3, uiOutput("kpi_variacion")),
               column(3, uiOutput("kpi_top_dept")),
               column(3, uiOutput("kpi_rezago"))),
      
      
      # ── RANKING + SEMÁFORO ────────────────────────────────────
      fluidRow(
        column(8,
               div(style = PANEL_STYLE,
                   panel_header(
                     "Gasto ejecutado por departamento",
                     "Soles corrientes · color = nivel vs. promedio nacional"),
                   div(style = "height:420px;",
                       withSpinner(plotlyOutput("ranking", height = "100%"),
                                   color = "#4A9E2B", type = 4)))),
        column(4,
               div(style = PANEL_STYLE,
                   panel_header(
                     "Semáforo de ejecución",
                     "vs. promedio nacional · variación anual"),
                   div(style = "height:420px; overflow-y:auto;",
                       tableOutput("semaforo"))))),
      
      # ── SERIE HISTÓRICA + COMPOSICIÓN ────────────────────────
      # Filtros propios para los 2 gráficos inferiores
      div(style = paste0(PANEL_STYLE, "padding:12px 16px; margin-bottom:12px;"),
          fluidRow(
            column(1,
                   div(style = lbl_style, "Vista serie")),
            column(3,
                   selectInput("dept_serie", NULL,
                               c("Nacional (todos)" = "TODOS", DEPTS_DISP),
                               "TODOS", width = "100%")),
            column(1,
                   div(style = lbl_style, "Vista composición")),
            column(3,
                   selectInput("dept_cat", NULL,
                               c("Nacional (todos)" = "TODOS", DEPTS_DISP),
                               "TODOS", width = "100%")),
            column(4,
                   div(style = "color:#9DA8B6; font-size:0.7rem; padding-top:6px;",
                       "⬤ Filtros independientes para los gráficos de línea e ",
                       "composición · el ranking usa su propio filtro arriba"))
          )
      ),
      
      fluidRow(
        column(6,
               div(style = PANEL_STYLE,
                   panel_header(
                     "Evolución histórica por año",
                     textOutput("subtitulo_serie", inline = TRUE)),
                   div(style = "height:440px;",
                       withSpinner(plotlyOutput("serie", height = "100%"),
                                   color = "#4A9E2B", type = 4)))),
        column(6,
               div(style = PANEL_STYLE,
                   panel_header(
                     "Composición del gasto — Top 10",
                     textOutput("subtitulo_cat", inline = TRUE)),
                   div(style = "height:440px;",
                       withSpinner(plotlyOutput("categorias", height = "100%"),
                                   color = "#4A9E2B", type = 4))))),
      # ── PIE DE PÁGINA ─────────────────────────────────────────
      div(style = paste0(
        "margin-top:24px; padding:16px 20px; ",
        "border-top:1px solid #2B303C; "),
        
        div(style = "color:#6B7A99; font-size:0.72rem; line-height:1.7;",
            
            div(style = "color:#9DA8B6; font-size:0.78rem; font-weight:600;
                     margin-bottom:6px;",
                "Fuente de información"),
            
            tags$p(style = "margin:0 0 4px;",
                   "Los datos presentados provienen del ",
                   tags$strong(style = "color:#9DA8B6;",
                               "Portal de Transparencia Económica del Ministerio de Economía y Finanzas (MEF) del Perú"),
                   " — Consulta Amigable de Gasto Público (",
                   tags$a(href  = "https://apps5.mineco.gob.pe/transparencia/",
                          target = "_blank",
                          style  = "color:#4A9E2B;",
                          "apps5.mineco.gob.pe/transparencia"),
                   "). La información corresponde al gasto ejecutado por departamento,",
                   " función y periodo, expresado en soles corrientes (PEN)."
            ),
            
            div(style = "color:#9DA8B6; font-size:0.78rem; font-weight:600;
                     margin-top:14px; margin-bottom:6px;",
                "Limitaciones y consideraciones"),
            
            tags$ul(style = "margin:0; padding-left:18px; list-style:disc;",
                    tags$li(style = "margin-bottom:3px;",
                            "Los datos se obtienen mediante scraping del portal del MEF y reflejan ",
                            "el estado de la información al momento de la descarga. ",
                            "Pueden existir diferencias con cifras oficiales publicadas posteriormente."),
                    tags$li(style = "margin-bottom:3px;",
                            "El gasto reportado corresponde al ",
                            tags$strong(style = "color:#9DA8B6;", "devengado"),
                            ", no al pagado ni al comprometido, por lo que no representa ",
                            "necesariamente desembolsos efectivos de recursos."),
                    tags$li(style = "margin-bottom:3px;",
                            "La clasificación departamental sigue la estructura territorial ",
                            "del SIAF-MEF y puede no coincidir con otras fuentes estadísticas ",
                            "como las del INEI."),
                    tags$li(style = "margin-bottom:3px;",
                            "Los umbrales del semáforo (50 % y 25 % del promedio nacional) ",
                            "son referenciales y no constituyen indicadores oficiales ",
                            "de eficiencia presupuestal."),
                    tags$li(style = "margin-bottom:3px;",
                            "La variación interanual compara el mismo trimestre entre años consecutivos. ",
                            "Cambios metodológicos en la clasificación funcional del MEF ",
                            "entre periodos pueden afectar la comparabilidad de las series."),
                    tags$li(
                      "Este panel es de carácter informativo y no constituye ",
                      "una fuente oficial.")
            ),
            
            div(style = "margin-top:14px; color:#4B5568;",
                paste0("Última actualización de datos: ",
                       format(Sys.Date(), "%d de %B de %Y"), " · ",
                       "Dashboard elaborado con R Shiny · MEF Perú"))
        )
      )
  )
)

##############################################################
# 4. SERVER
##############################################################

server <- function(input, output, session) {
  
  # ── Reactivos base ──────────────────────────────────────────
  
  col_activa <- reactive({ req(input$categoria); input$categoria })
  
  periodo_actual <- reactive({
    req(input$anio, input$trimestre)
    res <- gasto_raw %>%
      filter(ANIO == as.numeric(input$anio),
             TRIMESTRE == input$trimestre,
             !toupper(DEPARTAMENTO) %in%
               c("TOTAL","TOTAL GENERAL","NACIONAL"))
    validate(need(nrow(res) > 0, "Sin datos para este periodo."))
    res
  })
  
  periodo_anio_ant <- reactive({
    req(input$anio, input$trimestre)
    gasto_raw %>%
      filter(ANIO == as.numeric(input$anio) - 1,
             TRIMESTRE == input$trimestre,
             !toupper(DEPARTAMENTO) %in%
               c("TOTAL","TOTAL GENERAL","NACIONAL"))
  })
  
  montos_actual <- reactive({
    req(col_activa())
    df <- periodo_actual() %>%
      transmute(DEPARTAMENTO, MONTO = .data[[col_activa()]]) %>%
      filter(!is.na(MONTO), MONTO >= 0)
    if (input$dept_ranking != "TODOS")
      df <- df %>% filter(DEPARTAMENTO == input$dept_ranking)
    df
  })
  
  montos_ant <- reactive({
    df <- periodo_anio_ant()
    if (nrow(df) == 0) return(NULL)
    req(col_activa())
    df %>%
      transmute(DEPARTAMENTO, MONTO_ANT = .data[[col_activa()]]) %>%
      filter(!is.na(MONTO_ANT), MONTO_ANT >= 0)
  })
  
  metricas <- reactive({
    df <- montos_actual()
    prom <- mean(df$MONTO, na.rm = TRUE)
    list(df = df, prom = prom,
         low  = prom * 0.50,
         high = prom * 1.25)
  })
  
  # ── KPIs ────────────────────────────────────────────────────
  
  output$kpi_total <- renderUI({
    total <- sum(montos_actual()$MONTO, na.rm = TRUE)
    lbl   <- if (col_activa() == "TOTAL") "Gasto Total Ejecutado"
    else str_trunc(col_activa(), 32)
    kpi_card(lbl, fmt_millon(total),
             sub = paste(input$anio, input$trimestre))
  })
  
  output$kpi_variacion <- renderUI({
    t_act <- sum(montos_actual()$MONTO, na.rm = TRUE)
    df_a  <- montos_ant()
    if (is.null(df_a) || nrow(df_a) == 0)
      return(kpi_card("Variación vs. año anterior", "—",
                      sub = "Sin datos previos", acento = "#9DA8B6"))
    t_ant <- sum(df_a$MONTO_ANT, na.rm = TRUE)
    var   <- if (t_ant > 0) (t_act - t_ant) / t_ant * 100 else 0
    col   <- if (var >= 0) "#4A9E2B" else "#E55353"
    kpi_card("Variación Anual",
             paste0(if (var >= 0) "+" else "", round(var, 1), "%"),
             sub = paste("Año anterior:", fmt_millon(t_ant)),
             acento = col)
  })
  
  output$kpi_top_dept <- renderUI({
    df <- montos_actual() %>% arrange(desc(MONTO)) %>% slice(2)
    validate(need(nrow(df) > 0, ""))
    kpi_card("2° Departamento Mayor Gasto", df$DEPARTAMENTO,
             sub = fmt_millon(df$MONTO))
  })
  
  output$kpi_rezago <- renderUI({
    m    <- metricas()
    n    <- m$df %>% filter(MONTO < m$low) %>% nrow()
    col  <- if (n == 0) "#4A9E2B" else if (n <= 3) "#F39C12" else "#E55353"
    kpi_card("Regiones Nivel Bajo", as.character(n),
             sub = paste0("< 50% del promedio · S/ ",
                          round(m$low/1e6, 1), "M"),
             acento = col)
  })
  
  # ── Ranking ─────────────────────────────────────────────────
  
    output$ranking <- renderPlotly({
      m  <- metricas()
      df <- m$df %>%
        arrange(if (input$orden_ranking == "desc") MONTO
                else desc(MONTO)) %>%
        mutate(
          COLOR = case_when(
            MONTO < m$low   ~ "#E55353",
            MONTO >= m$high ~ "#4A9E2B",
            TRUE            ~ "#2980B9"),
          NIVEL = case_when(
            MONTO < m$low   ~ "Nivel bajo",
            MONTO >= m$high ~ "Nivel alto",
            TRUE            ~ "Nivel medio"),
          DF = factor(DEPARTAMENTO, levels = DEPARTAMENTO))
      
      plot_ly() %>%
    # Barras primero (quedan debajo)
    add_bars(
      data = df,
      x = ~MONTO, y = ~DF,
      orientation   = "h",
      showlegend    = FALSE,
      marker        = list(color = ~COLOR, line = list(width = 0)),
      text          = ~fmt_millon_gg(MONTO),
      textposition  = "outside",
      cliponaxis    = FALSE,
      textfont      = list(color = "#E8EDF2", size = 9),
      hovertemplate = "<b>%{y}</b><br>%{text}<br>%{customdata}<extra></extra>",
      customdata    = ~NIVEL) %>%
    # Traces fantasma para leyenda
    add_bars(
      x = NA_real_, y = NA_character_,
      name = "Nivel alto", showlegend = TRUE,
      marker = list(color = "#4A9E2B"), inherit = FALSE) %>%
    add_bars(
      x = NA_real_, y = NA_character_,
      name = "Nivel medio", showlegend = TRUE,
      marker = list(color = "#2980B9"), inherit = FALSE) %>%
    add_bars(
      x = NA_real_, y = NA_character_,
      name = "Nivel bajo", showlegend = TRUE,
      marker = list(color = "#E55353"), inherit = FALSE) %>%
    # Línea de promedio
    add_segments(
      x    = m$prom, xend = m$prom,
      y    = -0.5,   yend = nrow(df) - 0.5,
      line = list(
        color = "#F4A942",
        width = 2.5,          
        dash  = "dot"   
      ),
      name       = "Promedio nacional",
      showlegend = TRUE,
      inherit    = FALSE) %>%
    layout(
      paper_bgcolor = "#1A1D24",
      plot_bgcolor  = "#1A1D24",
      margin = list(l = 8, r = 80, t = 8, b = 45),
      xaxis = list(
        title      = list(
          text = "Gasto ejecutado (S/)",
          font = list(color = "#9DA8B6", size = 10)),
        color      = "#9DA8B6",
        showgrid   = TRUE,
        gridcolor  = "#2B303C",
        zeroline   = FALSE,
        tickformat = ",.0f",
        tickfont   = list(color = "#9DA8B6", size = 9),
        automargin = TRUE),
      yaxis = list(
        title         = list(
          text     = "Departamento",
          font     = list(color = "#9DA8B6", size = 10),
          standoff = 8),
        color         = "#FFF",
        showgrid      = FALSE,
        automargin    = TRUE,
        tickfont      = list(color = "#E8EDF2", size = 9),
        categoryorder = "array",
        categoryarray = levels(df$DF)),
      showlegend = TRUE,
      bargap = 0.28,
      legend = list(
        x       = 0.55, y = 0.02,
        font    = list(color = "#9DA8B6", size = 9),
        bgcolor = "rgba(0,0,0,0)")) %>%
    config(displayModeBar = FALSE)
    })
  
  # ── Tabla semáforo ───────────────────────────────────────────
  
  output$semaforo <- renderTable({
    m    <- metricas()
    df_a <- montos_ant()
    
    df <- m$df %>% arrange(desc(MONTO))
    
    if (!is.null(df_a) && nrow(df_a) > 0) {
      df <- df %>%
        left_join(df_a, by = "DEPARTAMENTO") %>%
        mutate(
          VAR = round((MONTO - MONTO_ANT) / MONTO_ANT * 100, 1),
          `Var.` = ifelse(is.na(VAR), "—",
                          paste0(if_else(VAR >= 0, "+", ""), VAR, "%")))
    } else {
      df$`Var.` <- "—"
    }
    
    df %>% mutate(
      `Gasto (M)` = round(MONTO / 1e6, 1),
      Estado = case_when(
        MONTO < m$low  ~
          "<span style='color:#E55353;font-weight:600'>● Bajo</span>",
        MONTO >= m$high ~
          "<span style='color:#4A9E2B;font-weight:600'>● Alto</span>",
        TRUE ~
          "<span style='color:#2980B9;font-weight:600'>● Medio</span>")
    ) %>%
      transmute(Departamento = DEPARTAMENTO,
                `Gasto (M)`, `Var.`, Estado)
  },
  sanitize.text.function = identity,
  bordered = FALSE, width = "100%", na = "—")
  
  # ── Subtítulos dinámicos ─────────────────────────────────────
  
  output$subtitulo_serie <- renderText({
    dept <- input$dept_serie
    lbl  <- if (col_activa() == "TOTAL") "Gasto total"
    else str_trunc(col_activa(), 35)
    if (dept == "TODOS") paste(lbl, "· nacional")
    else paste(lbl, "·", dept)
  })
  
  output$subtitulo_cat <- renderText({
    dept <- input$dept_cat
    if (dept == "TODOS") "Nacional · participación %"
    else paste(dept, "· participación %")
  })
  
  # ── Serie histórica (filtro dept_serie) ─────────────────────
  
  output$serie <- renderPlotly({
    req(col_activa(), input$trimestre, input$dept_serie)
    col  <- col_activa()
    dept <- input$dept_serie
    
    base <- gasto_raw %>%
      filter(!toupper(DEPARTAMENTO) %in%
               c("TOTAL","TOTAL GENERAL","NACIONAL"),
             TRIMESTRE == input$trimestre)
    
    if (dept != "TODOS")
      base <- base %>% filter(DEPARTAMENTO == dept)
    
    serie <- base %>%
      group_by(ANIO) %>%
      summarise(MONTO = sum(.data[[col]], na.rm = TRUE),
                .groups = "drop") %>%
      arrange(ANIO)
    
    anio_act <- as.numeric(input$anio)
    
    plot_ly(serie) %>%
      add_trace(
        x = ~ANIO, y = ~MONTO,
        type = "scatter", mode = "lines+markers",
        line   = list(color = "#2980B9", width = 2),
        marker = list(color = "#2980B9", size = 6,
                      line = list(color = "#0E1015", width = 1.5)),
        fill = "tozeroy",
        fillcolor = "rgba(41,128,185,0.08)",
        hovertemplate = "<b>%{x}</b><br>S/ %{y:,.0f}<extra></extra>",
        name = "Gasto anual") %>%
      # Punto resaltado del año seleccionado
      add_trace(
        data = serie %>% filter(ANIO == anio_act),
        x = ~ANIO, y = ~MONTO,
        type = "scatter", mode = "markers",
        marker = list(color = "#4A9E2B", size = 11,
                      line = list(color = "#0E1015", width = 2)),
        name = paste("Año", anio_act),
        hovertemplate =
          paste0("<b>Periodo activo: ", anio_act, "</b><br>",
                 "S/ %{y:,.0f}<extra></extra>")) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        
        # ── Aquí está el margen corregido ──
        # l más grande para que el área del plot no quede chica
        # vs. el espacio que ocupan los tick labels del eje Y
        margin = list(l = 60, r = 15, t = 10, b = 40),
        
        xaxis = list(
          # ── Label del eje X ──
          title = list(
            text = "Año",
            font = list(color = "#9DA8B6", size = 10)
          ),
          dtick = 1,
          color      = "#9DA8B6",
          gridcolor  = "#2B303C",
          zeroline   = FALSE,
          tickfont   = list(color = "#9DA8B6", size = 9),
          automargin = TRUE          # <- evita que los labels se corten
        ),
        
        yaxis = list(
          # ── Label del eje Y ──
          title = list(
            text = "Gasto ejecutado (S/)",
            font = list(color = "#9DA8B6", size = 10),
            standoff = 12            # distancia entre el label y los ticks
          ),
          tickformat = ",.0f",
          color      = "#9DA8B6",
          gridcolor  = "#2B303C",
          zeroline   = FALSE,
          tickfont   = list(color = "#9DA8B6", size = 9),
          automargin = TRUE          # <- este es el que resuelve el fondo chico
        ),
        
        showlegend = TRUE,
        legend = list(
          orientation = "h", x = 0, y = 1.12,
          font = list(color = "#9DA8B6", size = 9),
          bgcolor = "rgba(0,0,0,0)")
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Composición categorías (filtro dept_cat) ─────────────────
  
  output$categorias <- renderPlotly({
    req(input$dept_cat)
    dept <- input$dept_cat
    
    base <- periodo_actual()
    if (dept != "TODOS")
      base <- base %>% filter(DEPARTAMENTO == dept)
    
    df <- base %>%
      select(all_of(CATS_GASTO)) %>%
      summarise(across(everything(), ~sum(.x, na.rm = TRUE))) %>%
      pivot_longer(everything(),
                   names_to = "CATEGORIA", values_to = "MONTO") %>%
      filter(MONTO > 0) %>%
      arrange(MONTO) %>%
      slice_tail(n = 10) %>%
      mutate(
        PART  = MONTO / sum(MONTO),
        TEXTO = paste0(round(PART * 100, 1), "%"),
        CAT_F = factor(str_trunc(CATEGORIA, 26),
                       levels = str_trunc(CATEGORIA, 26)))
    
    plot_ly(df,
            x = ~MONTO, y = ~CAT_F,
            type = "bar", orientation = "h",
            marker = list(color = "#4A9E2B", opacity = 0.80,
                          line = list(width = 0)),
            text = ~TEXTO, textposition = "outside",
            textfont = list(color = "#9DA8B6", size = 10),
            hovertemplate = paste0(
              "<b>%{y}</b><br>S/ %{x:,.0f}<br>%{text}<extra></extra>")) %>%
      layout(
        paper_bgcolor = "transparent",
        plot_bgcolor  = "transparent",
        margin = list(l = 4, r = 48, t = 6, b = 6),
        xaxis = list(
          title ="Gasto ejecutado (S/)",
          tickformat = ",.0f", color = "#9DA8B6",
          gridcolor = "#2B303C", zeroline = FALSE,
          tickfont = list(color = "#9DA8B6", size = 9)),
        yaxis = list(
          title ="",
          color = "#FFF", automargin = TRUE, showgrid = FALSE,
          tickfont = list(color = "#E8EDF2", size = 9)),
        bargap = 0.28, showlegend = FALSE) %>%
      config(displayModeBar = FALSE)
  })
  
  # ── Descarga CSV ────────────────────────────────────────────
  
  output$descargar_reporte <- downloadHandler(
    filename = function()
      paste0("gasto_", input$anio, "_", input$trimestre, ".csv"),
    content  = function(file)
      write.csv(periodo_actual(), file, row.names = FALSE)
  )
}



##############################################################
# 5. LANZAMIENTO
##############################################################
shinyApp(ui = ui, server = server)
