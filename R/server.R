# R/server.R
server <- function(input, output, session) {
  observeEvent(input$curso, {
    req(input$curso)
    partes <- names(estructura_cursos[[input$curso]])
    updateRadioButtons(session, "parte", choices = partes, selected = partes[1])
  })

  output$sesion_ui <- renderUI({
    req(input$curso, input$parte)
    sesiones <- estructura_cursos[[input$curso]][[input$parte]]$sesiones
    selectInput("sesion", "Sesión:", choices = names(sesiones), selected = names(sesiones)[1])
  })

  output$contenido_ui <- renderUI({
    req(input$curso, input$parte, input$sesion)
    info <- obtener_info_sesion(input$curso, input$parte, input$sesion)
    modulo <- info$module

    if (!is.null(modulo)) {
      ui_fun_name <- paste0(modulo, "UI")
      if (exists(ui_fun_name, mode = "function")) {
        ui_fun <- get(ui_fun_name, mode = "function")
        return(ui_fun(info$id))
      }
    }

    tagList(
      h3(input$sesion),
      div(class = "alert alert-info", "Contenido próximamente disponible para esta sesión.")
    )
  })

  for (module_name in names(module_registry)) {
    server_fun_name <- paste0(module_name, "Server")
    if (exists(server_fun_name, mode = "function")) {
      server_fun <- get(server_fun_name, mode = "function")
      lapply(module_registry[[module_name]], function(id) callModule(server_fun, id))
    }
  }
}
