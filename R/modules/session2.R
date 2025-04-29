# R/modules/session2.R
library(ggplot2)

session2UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 2: Estadística Descriptiva Avanzada")
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
            tags$table(class = "table activity-table",
            tags$thead(tags$tr(tags$th("Segmento"), tags$th("Tiempo"), tags$th("Actividad"))),
            tags$tbody(
                tags$tr(tags$th("Tiempo"), tags$th("Actividad")),
                tags$tr(tags$td("0–20 min"), tags$td("Repaso rápido media, mediana, rango.")),
                tags$tr(tags$td("20–45 min"), tags$td("Cálculo de varianza y sd con mean(), sd().")),
                tags$tr(tags$td("45–70 min"), tags$td("Curtosis y asimetría con moments::kurtosis()/skewness().")),
                tags$tr(tags$td("70–95 min"), tags$td("Agrupación con group_by() + summarise().")),
                tags$tr(tags$td("95–115 min"), tags$td("Gráficos de caja por tratamiento con geom_boxplot().")),
                tags$tr(tags$td("115–120 min"), tags$td("Discusión e interpretación agronómica."))
                )
            ),
        )
        # ——————————————
        # PESTAÑA: TEMARIO
        # ——————————————
    )
  )
}

session2Server <- function(input, output, session) {
  # No reactivos necesarios por ahora
}
