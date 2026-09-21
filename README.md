# Monitor Presupuestal — MEF Perú

Dashboard interactivo desarrollado en **R Shiny** para explorar y monitorear el gasto público del Perú a partir de información del **Portal de Transparencia Económica del MEF**.

## Descripción

El proyecto automatiza la obtención de información mediante **web scraping**, procesa los datos y los presenta en un dashboard interactivo que permite analizar el gasto ejecutado por:

* Departamento
* Año
* Trimestre
* Función
* Actividad
* Proyecto

El dashboard cuenta con dos vistas:

* **FUA:** Funciones y Actividades
* **FUP:** Funciones y Proyectos

## Dashboard

El aplicativo permite consultar:

* Gasto total ejecutado.
* Variación interanual.
* Ranking de departamentos.
* Semáforo de nivel de gasto.
* Evolución histórica.
* Composición del gasto por categorías.
* Exportación de resultados en formato CSV.

## Flujo del proyecto

```text
Portal de Transparencia Económica — MEF
              ↓
        Web Scraping
              ↓
      Limpieza y procesamiento
              ↓
        Archivos RDS
        ├── gasto_fua.rds
        └── gasto_fup.rds
              ↓
          R Shiny
              ↓
      Dashboard interactivo
```

## Tecnologías

* **R**
* **R Shiny**
* **Tidyverse**
* **Plotly**
* **Shinycssloaders**
* **Web scraping**
* **Git / GitHub**

## Estructura

```text
├── app.R
├── global.R
├── estilos.css
├── data/
│   ├── gasto_fua.rds
│   └── gasto_fup.rds
└── scraper/
    └── ...
```

## Fuente de información

Los datos provienen del **Portal de Transparencia Económica del Ministerio de Economía y Finanzas del Perú**, específicamente de la Consulta Amigable de Gasto Público.

> Los datos son obtenidos mediante scraping y pueden presentar diferencias respecto de cifras oficiales actualizadas posteriormente por el MEF.

## Ejecución local

Instalar las dependencias:

```r
install.packages(c(
  "shiny",
  "shinycssloaders",
  "tidyverse",
  "plotly",
  "stringr",
  "scales"
))
```

Luego ejecutar:

```r
shiny::runApp()
```

## Autor

**Mirko Caja Ventura**

Economista | Analista de Datos | Business Intelligence

[GitHub](https://github.com/Mirko-cv)
[Dashboard] (https://8ezlpo-mirko-caja0ventura.shinyapps.io/app_gasto_publico/)
