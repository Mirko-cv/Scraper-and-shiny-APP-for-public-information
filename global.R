##############################################################
# global.R — Dashboard de Seguimiento de Gasto Público
# Dependencias: shiny, bslib, dplyr, tidyr, ggplot2,
#               scales, DT, plotly
##############################################################

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(DT)
library(plotly)
library(apexcharter)

# --- Paleta institucional noche ---
PAL <- list(
  fondo        = "#0F1923",
  panel        = "#162030",
  panel2       = "#1C2A3A",
  borde        = "#253447",
  texto        = "#E8EDF2",
  texto_suave  = "#7A90A8",
  verde        = "#00C896",   # ejecución alta / buena
  ambar        = "#F4A942",   # ejecución media / alerta
  rojo         = "#E05252",   # ejecución baja / crítica
  azul_claro   = "#4A9EDB"    # acento neutral / serie histórica
)

# --- Semáforo: clasifica ejecución relativa al promedio ---
# Verde: >= 90% del promedio | Ámbar: 70-90% | Rojo: < 70%
semaforo_color <- function(monto, promedio) {
  ratio <- ifelse(promedio == 0, 0, monto / promedio)
  dplyr::case_when(
    ratio >= 0.90 ~ PAL$verde,
    ratio >= 0.70 ~ PAL$ambar,
    TRUE          ~ PAL$rojo
  )
}

semaforo_label <- function(monto, promedio) {
  ratio <- ifelse(promedio == 0, 0, monto / promedio)
  dplyr::case_when(
    ratio >= 0.90 ~ "En ritmo",
    ratio >= 0.70 ~ "Rezago leve",
    TRUE          ~ "Rezago crítico"
  )
}

# --- Formato de montos ---
fmt_millon <- function(x) {
  paste0("S/ ", scales::comma(round(x / 1e6, 1)), "M")
}

fmt_millon_gg <- function(x) {
  dplyr::case_when(
    abs(x) >= 1e9 ~ paste0("S/ ", round(x / 1e9, 1), "B"),
    abs(x) >= 1e6 ~ paste0("S/ ", round(x / 1e6, 1), "M"),
    TRUE          ~ paste0("S/ ", scales::comma(round(x / 1e3, 0)), "K")
  )
}

# --- Tema ggplot2 oscuro coherente con el panel ---
tema_oscuro <- function(base_size = 11) {
  theme_minimal(base_size = base_size, base_family = "sans") +
    theme(
      plot.background    = element_rect(fill = PAL$panel, color = NA),
      panel.background   = element_rect(fill = PAL$panel, color = NA),
      panel.grid.major   = element_line(color = PAL$borde, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_blank(),
      text               = element_text(color = PAL$texto),
      axis.text          = element_text(color = PAL$texto_suave, size = 9),
      axis.title         = element_blank(),
      plot.title         = element_text(color = PAL$texto, size = 13,
                                        face = "bold", margin = margin(b = 8)),
      plot.subtitle      = element_text(color = PAL$texto_suave, size = 10,
                                        margin = margin(b = 12)),
      legend.background  = element_rect(fill = PAL$panel, color = NA),
      legend.text        = element_text(color = PAL$texto_suave, size = 9),
      legend.title       = element_text(color = PAL$texto, size = 9),
      plot.margin        = margin(12, 16, 12, 16)
    )
}

# --- Tema bslib mínimo (sin Google Fonts, evita problemas offline) ---
tema_dashboard <- bs_theme(
  version   = 5,
  bg        = PAL$fondo,
  fg        = PAL$texto,
  primary   = PAL$verde,
  secondary = PAL$azul_claro,
  "font-size-base" = "0.9rem"
)
