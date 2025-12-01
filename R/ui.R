# R/ui.R
library(shiny)
library(shinythemes)
library(bslib)

ui <- fluidPage(
  withMathJax(),
  shinyjs::useShinyjs(),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#2F855A",
    secondary = "#2B6CB0",
    success = "#38A169",
    base_font = font_google("Poppins"),
    heading_font = font_google("Montserrat"),
    bg = "#F7FAF2",
    fg = "#1F2933"
  ),
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
    includeCSS("www/css/custom.css"),
    includeScript("www/js/custom.js")
  ),
  uiOutput("main_ui")
)
