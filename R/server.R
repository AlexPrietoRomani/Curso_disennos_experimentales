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
      "Sesión 2" = session2UI("s2"),
      "Sesión 3" = session3UI("s3"),
      "Sesión 4" = session4UI("s4"),
      "Sesión 5" = session5UI("s5"),
      #"Sesión 6" = session6UI("s6"),
    )
  })
  
  callModule(session1Server, "s1")
  callModule(session2Server, "s2")
  callModule(session3Server, "s3")
  callModule(session4Server, "s4")
  callModule(session5Server, "s5")
  #callModule(session6Server, "s6")
}
