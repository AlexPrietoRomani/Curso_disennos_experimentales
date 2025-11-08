# app.R
library(shiny)

options(encoding = "UTF-8")

# Cargar módulos y definiciones globales
source("R/global.R")
source("R/ui.R")
source("R/server.R")

# Inicializar la aplicación
shinyApp(ui = ui, server = server)