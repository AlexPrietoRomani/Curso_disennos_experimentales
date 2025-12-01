# R/modules/auth/password_reset.R

passwordResetUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "container",
      style = "max-width: 500px; margin-top: 50px;",
      div(
        class = "card shadow-sm",
        div(
          class = "card-body p-4",
          h3("Recuperar contraseña", class = "card-title text-center mb-4"),
          
          # Step 1: Request Link
          div(
            id = ns("step_request"),
            p("Ingresa tu correo electrónico y te enviaremos un enlace para restablecer tu contraseña."),
            textInput(ns("reset_email"), "Correo electrónico"),
            uiOutput(ns("request_msg")),
            div(
              class = "d-grid gap-2 mt-3",
              actionButton(ns("do_send_link"), "Enviar enlace", class = "btn btn-primary")
            )
          ),
          
          # Step 2: Set New Password (hidden by default, shown via server logic if token present)
          shinyjs::hidden(
            div(
              id = ns("step_reset"),
              p("Ingresa tu nueva contraseña."),
              passwordInput(ns("new_password"), "Nueva contraseña"),
              passwordInput(ns("new_password_confirm"), "Confirmar nueva contraseña"),
              uiOutput(ns("reset_msg")),
              div(
                class = "d-grid gap-2 mt-3",
                actionButton(ns("do_reset_pw"), "Cambiar contraseña", class = "btn btn-primary")
              )
            )
          ),
          
          div(
            class = "text-center mt-3",
            actionLink(ns("back_to_login"), "Volver al inicio de sesión")
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
      
      # Check if user exists (optional, to avoid leaking info, but useful for UX)
      # For security, usually we say "If the email exists, we sent a link."
      
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
