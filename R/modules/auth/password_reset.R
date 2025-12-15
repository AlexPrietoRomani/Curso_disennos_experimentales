# R/modules/auth/password_reset.R

passwordResetUI <- function(id) {
  ns <- NS(id)
  div(
    class = "login-section",
    div(
      class = "login-wrapper",
      div(
        class = "login-card",
        # Header
        div(
          class = "login-header",
          div(
            class = "login-icon-circle",
            icon("key", class = "fa-lg")
          ),
          tags$h2(class = "login-title", "Recuperar Acceso"),
          tags$p(class = "login-subtitle", "Restablece tu contraseña de forma segura")
        ),
        
        # Body
        div(
          class = "login-body",
          
          # Step 1: Request Link
          div(
            id = ns("step_request"),
            tags$p(class = "text-muted mb-4 small text-center", 
                   "Ingresa tu correo electrónico asociado y te enviaremos las instrucciones."),
            
            div(
              class = "login-form-group",
              tags$label("Correo Electrónico"),
              textInput(ns("reset_email"), label = NULL, placeholder = "nombre@ejemplo.com", width = "100%")
            ),
            
            uiOutput(ns("request_msg")),
            
            actionButton(
              ns("do_send_link"), 
              "ENVIAR ENLACE", 
              class = "btn-login-main mb-3"
            )
          ),
          
          # Step 2: Set New Password (hidden by default)
          shinyjs::hidden(
            div(
              id = ns("step_reset"),
              tags$p(class = "text-muted mb-4 small text-center", "Crea una nueva contraseña para tu cuenta."),
              
              div(
                class = "login-form-group",
                tags$label("Nueva contraseña"),
                passwordInput(ns("new_password"), label = NULL, placeholder = "Mínimo 8 caracteres", width = "100%")
              ),
              
              div(
                class = "login-form-group",
                tags$label("Confirmar contraseña"),
                passwordInput(ns("new_password_confirm"), label = NULL, placeholder = "Repite la contraseña", width = "100%")
              ),
              
              uiOutput(ns("reset_msg")),
              
              actionButton(
                ns("do_reset_pw"), 
                "CAMBIAR CONTRASEÑA", 
                class = "btn-login-main mb-3"
              )
            )
          ),
          
          # Footer Links
          div(
            class = "text-center mt-2",
            actionLink(ns("back_to_login"), "Volver al inicio de sesión", class = "login-link")
          )
        )
      )
    )
  )
}

passwordResetServer <- function(id, parent_session, url_params = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Check if we are in "reset mode" (token present in URL)
    observe({
      params <- url_params()
      if (!is.null(params$token) && !is.null(params$email)) {
        # Verify token immediately
        valid <- verify_reset_token(params$email, params$token)
        
        if (valid) {
          shinyjs::hide("step_request")
          shinyjs::show("step_reset")
        } else {
          output$request_msg <- renderUI({
            div(class = "alert alert-danger", "El enlace de restablecimiento es inválido o ha expirado.")
          })
        }
      }
    })
    
    # Handle "Send Link"
    observeEvent(input$do_send_link, {
      email <- input$reset_email
      if (email == "") return()
      
      token <- generate_token(32)
      save_reset_token(email, token)
      
      # Construct link
      base_url <- paste0(
        session$clientData$url_protocol, "//",
        session$clientData$url_hostname,
        if (session$clientData$url_port != "") paste0(":", session$clientData$url_port) else ""
      )
      
      reset_link <- paste0(base_url, "/?action=reset&email=", email, "&token=", token)
      
      tryCatch({
        send_password_reset_email(email, reset_link)
        output$request_msg <- renderUI({
          div(class = "alert alert-success", "Si el correo existe, se ha enviado un enlace de recuperación.")
        })
      }, error = function(e) {
        output$request_msg <- renderUI({
          div(class = "alert alert-warning", "Error al enviar el correo. Inténtalo más tarde.")
        })
      })
    })
    
    # Handle "Change Password"
    observeEvent(input$do_reset_pw, {
      params <- url_params()
      new_pw <- input$new_password
      confirm_pw <- input$new_password_confirm
      
      if (new_pw != confirm_pw) {
        output$reset_msg <- renderUI({
          div(class = "alert alert-danger", "Las contraseñas no coinciden.")
        })
        return()
      }
      
      complexity <- validate_password_complexity(new_pw)
      if (!complexity$valid) {
        output$reset_msg <- renderUI({
          div(class = "alert alert-danger", complexity$message)
        })
        return()
      }
      
      # Update password
      update_password(params$email, new_pw)
      
      output$reset_msg <- renderUI({
        div(class = "alert alert-success", "Contraseña actualizada. Ya puedes iniciar sesión.")
      })
      
      shinyjs::disable("do_reset_pw")
      
      # Redirect to login after a delay? Or just let user click "Back"
    })
    
    observeEvent(input$back_to_login, {
      parent_session$userData$view_state("login")
    })
  })
}
