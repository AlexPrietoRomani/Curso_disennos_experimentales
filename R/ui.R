# R/ui.R
library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      .session-title { margin-top: 20px; margin-bottom: 15px; }
      .section-header { margin-top: 15px; margin-bottom: 5px; }
      .activity-table th, .activity-table td { padding: 8px; vertical-align: top; }
      .plot-box, .image-box { border: 1px solid #ddd; padding: 10px; border-radius: 5px; }
    "))
  ),
  
  titlePanel("📊 Temario de R para Estadística Agrícola"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Navegación"),
      radioButtons("parte", "Parte:", choices = names(sesiones)),
      uiOutput("sesion_ui")
    ),
    mainPanel(
      width = 9,
      uiOutput("contenido_ui")
    )
  )
)