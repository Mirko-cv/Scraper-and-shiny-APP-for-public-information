##############################################################
# global.R — tema visual, paquetes y utilidades compartidas
##############################################################
# Dashboard: "La Boleta del Estado"
# Concepto: el gasto público leído como un recibo nacional —
# cada departamento, una línea de gasto; cada año, una boleta.
##############################################################

# --- Paquetes ---
library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(scales)
library(DT)
library(tidyr)

# --- Paleta de la "Boleta del Estado" ---
# Papel cálido + tinta cobre, como una boleta fiscal impresa
PAL <- list(
  papel       = "#FAF7F2",  # fondo
  papel_panel = "#FFFFFF",
  tinta       = "#1F1B16",  # texto principal
  tinta_suave = "#5C5347",  # texto secundario
  cobre       = "#B5482F",  # acento principal (gasto, alerta)
  oliva       = "#3A4A30",  # acento secundario (territorio, mapa alto)
  oliva_claro = "#7C8D6B",
  linea       = "#DDD5C7",  # bordes / separadores
  ambar       = "#C68A2E"   # acento terciario (resaltes puntuales)
)

# Escala secuencial para el mapa (papel -> cobre, como tinta absorbiéndose)
PALETA_MAPA <- colorRampPalette(c("#F3E9DD", PAL$cobre, "#5C1C0F"))

# --- Tema bslib (Bootstrap 5) ---
# Nota: esta ruta es relativa a la raíz de la app (estándar en Shiny,
# ya sea ejecutando runApp("app_gasto_publico") o abriendo el .Rproj).
tema_boleta <- bs_theme(
  version     = 5,
  bg          = PAL$papel,
  fg          = PAL$tinta,
  primary     = PAL$cobre,
  secondary   = PAL$oliva,
  base_font     = font_google("Inter"),
  heading_font  = font_google("Source Serif 4"),
  code_font     = font_google("IBM Plex Mono"),
  "font-size-base" = "0.95rem"
) %>%
  bs_add_rules(sass::sass_file("www/estilos.scss"))

# --- Utilidades de formato ---
fmt_soles <- function(x, escala = 1e6, sufijo = " mill.") {
  paste0("S/ ", scales::comma(round(x / escala, 1), accuracy = 0.1), sufijo)
}

fmt_soles_compacto <- function(x) {
  scales::label_number(
    scale_cut = scales::cut_short_scale(),
    prefix = "S/ "
  )(x)
}

# Departamentos donde "LIMA" en datos MEF puede incluir o no Callao;
# se deja como utilidad para que el usuario ajuste si su fuente separa
# "LIMA METROPOLITANA" / "LIMA PROVINCIAS" en vez de "LIMA" + "CALLAO".
normalizar_departamento <- function(x) {
  x <- toupper(trimws(x))
  x <- gsub("Á", "A", x); x <- gsub("É", "E", x); x <- gsub("Í", "I", x)
  x <- gsub("Ó", "O", x); x <- gsub("Ú", "U", x); x <- gsub("Ñ", "N", x)
  x
}
