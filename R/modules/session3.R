# R/modules/session3.R
library(dplyr)
library(ggplot2)
library(moments)
library(patchwork)
library(grid)

session3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 3: Probabilidad y Distribuciones Esenciales")
    ),
    tags$style(HTML("
      .r-code {
        background: #f5f5f5;
        padding: 12px;
        border-radius: 6px;
        font-family: 'Courier New', monospace;
        line-height: 1.4;
        white-space: pre-wrap;
        border: 1px solid #e0e0e0;
        margin: 10px 0;
      }
    ")),

    # Usar navset_tab con nav_panel de bslib
    navset_tab(
      # ——————————————
      # PESTAÑA: TEMARIO
      # ——————————————
      nav_panel(title = "Temario",
        h4(class = "section-header", "Temario"),

        # Tabla de actividades
        tags$table(class = "table activity-table",
          tags$thead(tags$tr(
            tags$th("Segmento"),
            tags$th("Tiempo"),
            tags$th("Actividad")
          )),
          tags$tbody(
            tags$tr(
              tags$td("1 Teoría de probabilidad"),
              tags$td("0–15 min"),
              tags$td("Revisar eventos simples y condicionales")
            ),
            tags$tr(
              tags$td("2 Definición y simulación"),
              tags$td("15–45 min"),
              tags$td(HTML(
                "Binomial (infestación de plagas, <code>rbinom()</code>)<br/>",
                "Poisson (lesiones foliares, <code>rpois()</code>)<br/>",
                "Normal (variabilidad de rendimiento, <code>rnorm()</code>)"
              ))
            ),
            tags$tr(
              tags$td("3 Graficar distribuciones"),
              tags$td("45–75 min"),
              tags$td("Densidades y barras de frecuencia con <code>ggplot2</code>")
            ),
            tags$tr(
              tags$td("4 Discusión de elección"),
              tags$td("75–95 min"),
              tags$td("Comparar ajustes de distribuciones con datos reales")
            ),
            tags$tr(
              tags$td("5 Resultado esperado"),
              tags$td("95–120 min"),
              tags$td("Scripts de simulación y gráficos interpretados")
            )
          )
        )
      )
      # — aquí podrían agregarse más nav_panel() para ejercicios, teoría, etc.
    )
  )
}

session3Server <- function(input, output, session) {
  # Lógica de simulación y gráficos se implementará aquí
}
