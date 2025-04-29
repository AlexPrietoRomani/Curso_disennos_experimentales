# R/server.R
server <- function(input, output, session) {
  output$sesion_ui <- renderUI({
    req(input$parte)
    selectInput("sesion", "Sesión:", choices = sesiones[[input$parte]])
  })
  
  output$contenido_ui <- renderUI({
    req(input$sesion)
    switch(input$sesion,
      "Sesión 1" = session1UI("s1"),
      "Sesión 2" = session2UI("s2")
      # … resto de sesiones …
    )
  })
  
  callModule(session1Server, "s1")
  callModule(session2Server, "s2")
  # … resto de módulos …
}
