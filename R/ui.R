# R/ui.R
library(shiny)
library(shinythemes)

ui <- fluidPage(
  withMathJax(),
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    includeCSS("www/css/custom.css"),
    includeScript("www/js/custom.js")
  ),

  titlePanel("📊 Temario de R para Estadística Agrícola"),

  sidebarLayout(
    sidebarPanel(
      width = 2,
      h4("Navegación"),
      selectInput("curso", "Curso:", choices = names(estructura_cursos), selected = curso_predeterminado),
      radioButtons("parte", "Parte:", choices = names(estructura_cursos[[curso_predeterminado]]), selected = partes_predeterminadas[1]),
      uiOutput("sesion_ui")
    ),
    mainPanel(
      width = 10,
      uiOutput("contenido_ui")
    )
  )
)
