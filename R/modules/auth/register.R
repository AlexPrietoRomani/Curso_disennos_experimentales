# R/modules/auth/register.R

registerUI <- function(id) {
  ns <- NS(id)
  
  div(
    class = "login-section",
    div(
      class = "login-wrapper",
      # Slightly wider wrapper for registration if needed, but 440px is standard mobile-first
      div(
        class = "login-card", 
        
        # Header
        div(
          class = "login-header",
          div(
            class = "login-icon-circle",
            icon("user-plus", class = "fa-lg")
          ),
          tags$h2(class = "login-title", "Crear Cuenta"),
          tags$p(class = "login-subtitle", "Únete a la plataforma educativa")
        ),
        
        # Body
        div(
          class = "login-body",
          
          div(
            class = "login-form-group",
            tags$label("Usuario"),
            textInput(ns("reg_username"), label = NULL, placeholder = "Elige un usuario único", width = "100%")
          ),
          
          div(
            class = "login-form-group",
            tags$label("Correo Electrónico"),
            textInput(ns("reg_email"), label = NULL, placeholder = "nombre@ejemplo.com", width = "100%")
          ),
          
          # Row for Names
          div(
            class = "row",
            div(
              class = "col-6",
              div(
                class = "login-form-group",
                tags$label("Nombre"),
                textInput(ns("reg_first_name"), label = NULL, width = "100%")
              )
            ),
            div(
              class = "col-6",
              div(
                class = "login-form-group",
                tags$label("Apellido"),
                textInput(ns("reg_last_name"), label = NULL, width = "100%")
              )
            )
          ),
          
          div(
            class = "login-form-group",
            tags$label("Contraseña"),
            passwordInput(ns("reg_password"), label = NULL, placeholder = "Mínimo 8 caracteres", width = "100%")
          ),
          
          div(
            class = "login-form-group mb-4",
            tags$label("Confirmar Contraseña"),
            passwordInput(ns("reg_password_confirm"), label = NULL, placeholder = "Repite la contraseña", width = "100%")
          ),
          
          uiOutput(ns("reg_error_msg")),
          
          actionButton(
            ns("do_register"), 
            "REGISTRARME", 
            class = "btn-login-main mb-4"
          ),
          
          div(
            class = "text-center",
            span(class = "text-muted", " ¿Ya tienes cuenta? "),
            actionLink(ns("go_to_login"), "Iniciar sesión", class = "login-link")
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
          div(class = "alert alert-danger", icon("exclamation-circle"), " Completa todos los campos obligatorios.")
        })
        return()
      }
      
      if (input$reg_password != input$reg_password_confirm) {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", icon("exclamation-circle"), " Las contraseñas no coinciden.")
        })
        return()
      }
      
      # 2. Validate password complexity
      complexity <- validate_password_complexity(input$reg_password)
      if (!complexity$valid) {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", icon("shield-alt"), " ", complexity$message)
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
            tags$h4(icon("check-circle"), " ¡Cuenta creada!"),
            tags$p("Tu registro ha sido exitoso."),
            tags$p("Ya puedes iniciar sesión con tu usuario.")
          )
        })
        
        # Hide form elements
        shinyjs::hide("reg_username")
        shinyjs::hide("reg_email")
        shinyjs::hide("reg_first_name")
        shinyjs::hide("reg_last_name")
        shinyjs::hide("reg_password")
        shinyjs::hide("reg_password_confirm")
        shinyjs::hide("do_register")
        
      } else {
        output$reg_error_msg <- renderUI({
          div(class = "alert alert-danger", icon("times-circle"), " ", result$message)
        })
      }
    })
    
    observeEvent(input$go_to_login, {
      # Signal parent to switch view
      parent_session$userData$view_state("login")
    })
  })
}
