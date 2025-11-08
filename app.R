# app.R
library(shiny)

options(encoding = "UTF-8")

# --- Cargador seguro para paquetes opcionales
safe_library <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Paquete opcional no disponible: %s (la app seguirá sin este módulo).", pkg))
    return(FALSE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
  TRUE
}

# Cargar módulos y definiciones globales
source("R/global.R")
source("R/ui.R")
source("R/server.R")

# Inicializar la aplicación
shinyApp(ui = ui, server = server)