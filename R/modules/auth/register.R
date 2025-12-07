# R/modules/auth/register.R

registerUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "container",
      style = "max-width: 500px; margin-top: 50px;",
      div(
        class = "card shadow-sm",
        div(
          class = "card-body p-4",
          h3("Crear nueva cuenta", class = "card-title text-center mb-4"),
          
          textInput(ns("reg_username"), "Usuario", placeholder = "Elige un nombre de usuario"),
          textInput(ns("reg_email"), "Correo electrónico", placeholder = "nombre@ejemplo.com"),
          div(
            class = "row",
            div(class = "col-md-6", textInput(ns("reg_first_name"), "Nombre")),
            div(class = "col-md-6", textInput(ns("reg_last_name"), "Apellido"))
          ),
          passwordInput(ns("reg_password"), "Contraseña", placeholder = "Mínimo 8 caracteres"),
          passwordInput(ns("reg_password_confirm"), "Confirmar contraseña"),
          
          uiOutput(ns("reg_error_msg")),
          
          div(
            class = "d-grid gap-2 mt-4",
            actionButton(ns("do_register"), "Registrarme", class = "btn btn-primary btn-lg")
          ),
          
          div(
            class = "text-center mt-3",
            actionLink(ns("go_to_login"), "Ya tengo cuenta, iniciar sesión")
          )
        )
      )
    )
  )
}

registerServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observeEvent(input$do_register, {
      # 1. Validate inputs
      if (input$reg_username == "" || input$reg_email == "" || 
          input$reg_password == "" || input$reg_first_name == "") {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", "Por favor completa todos los campos obligatorios.")
        })
        return()
      }
      
      if (input$reg_password != input$reg_password_confirm) {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", "Las contraseñas no coinciden.")
        })
        return()
      }
      
      # 2. Validate password complexity
      complexity <- validate_password_complexity(input$reg_password)
      if (!complexity$valid) {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", complexity$message)
        })
        return()
      }
      
      # 3. Create user in DB
      result <- create_user(
        username = input$reg_username,
        email = input$reg_email,
        password = input$reg_password,
        first_name = input$reg_first_name,
        last_name = input$reg_last_name
      )
      
      if (result$success) {
        output$reg_error_msg <- renderUI({
          div(
            class = "alert alert-success text-center", 
            h4(icon("check-circle"), " ¡Cuenta creada!"),
            p("Tu registro ha sido exitoso."),
            p("Ya puedes iniciar sesión con tu usuario y contraseña.")
          )
        })
        
        # Hide form elements to prevent resubmission and clarify state
        shinyjs::hide("reg_username")
        shinyjs::hide("reg_email")
        shinyjs::hide("reg_first_name")
        shinyjs::hide("reg_last_name")
        shinyjs::hide("reg_password")
        shinyjs::hide("reg_password_confirm")
        shinyjs::hide("do_register")
        
      } else {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", result$message)
        })
      }
    })
    
    observeEvent(input$go_to_login, {
      # Signal parent to switch view
      parent_session$userData$view_state("login")
    })
  })
}
