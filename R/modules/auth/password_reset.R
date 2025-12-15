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
          tags$p(class = "login-subtitle", "Restablece tu contraseña")
        ),
        
        # Body
        div(
          class = "login-body",
          
          # Container for dynamic steps
          uiOutput(ns("dynamic_content")),
          
          # Footer Links
          div(
            class = "text-center mt-3",
            actionLink(ns("back_to_login"), "Volver al inicio de sesión", class = "login-link")
          )
        )
      )
    )
  )
}

passwordResetServer <- function(id, parent_session) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive state for the wizard
    # Steps: "email" -> "verify_code" -> "new_password" -> "success"
    current_step <- reactiveVal("email")
    user_email <- reactiveVal(NULL)
    
    # --- UI Rendering for Steps ---
    
    output$dynamic_content <- renderUI({
      step <- current_step()
      
      if (step == "email") {
        tagList(
          tags$p(class = "text-muted mb-4 small text-center", 
                 "Ingresa tu correo electrónico asociado y te enviaremos un código de verificación."),
          div(
            class = "login-form-group",
            tags$label("Correo Electrónico"),
            textInput(ns("reset_email"), label = NULL, placeholder = "nombre@ejemplo.com", width = "100%")
          ),
          uiOutput(ns("msg_email")),
          actionButton(ns("do_send_code"), "ENVIAR CÓDIGO", class = "btn-login-main mb-3")
        )
        
      } else if (step == "verify_code") {
        tagList(
          tags$div(
            class = "alert alert-success small mb-4", 
            icon("paper-plane"),
            paste(" Código enviado a:", user_email())
          ),
          tags$p(class = "text-muted mb-4 small text-center", 
                 "Ingresa el código de 6 dígitos que enviamos a tu correo."),
          div(
            class = "login-form-group",
            tags$label("Código de Verificación"),
            textInput(ns("otp_code"), label = NULL, placeholder = "Ej. 123456", width = "100%")
          ),
          uiOutput(ns("msg_verify")),
          actionButton(ns("do_verify"), "VERIFICAR CÓDIGO", class = "btn-login-main mb-3"),
          actionLink(ns("resend_code"), "Reenviar código", class = "d-block text-center small text-muted")
        )
        
      } else if (step == "new_password") {
        tagList(
          tags$p(class = "text-muted mb-4 small text-center", "Código verificado. Crea tu nueva contraseña."),
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
          uiOutput(ns("msg_reset")),
          actionButton(ns("do_reset_pw"), "CAMBIAR CONTRASEÑA", class = "btn-login-main mb-3")
        )
        
      } else if (step == "success") {
        div(
          class = "text-center py-4",
          div(class = "mb-3", style = "color: var(--agro-green); font-size: 3rem;", icon("check-circle")),
          h4("¡Contraseña Actualizada!"),
          p("Ya puedes iniciar sesión con tu nueva clave."),
          actionButton(ns("go_login_success"), "Ir a Iniciar Sesión", class = "btn btn-outline-primary mt-3")
        )
      }
    })
    
    # --- Logic Steps ---
    
    # Step 1: Send Code
    observeEvent(input$do_send_code, {
      email <- trimws(input$reset_email)
      if (email == "") {
         output$msg_email <- renderUI(div(class = "text-danger small mb-3", "Ingresa un correo válido."))
         return()
      }
      
      # Generate OTP
      otp <- sprintf("%06d", sample(0:999999, 1))
      
      # Save to DB (email, code)
      # Note: We rely on save_reset_token from db.R which handles persistence
      tryCatch({
        save_reset_token(email, otp) # This saves OTP as the token and sets expiry
        
        # Send Email
        send_password_reset_code(email, otp)
        
        user_email(email)
        current_step("verify_code")
        
      }, error = function(e) {
        output$msg_email <- renderUI(div(class = "alert alert-danger small", "Error al procesar. Intenta más tarde."))
      })
    })
    
    # Handle Resend
    observeEvent(input$resend_code, {
      req(user_email())
      # Re-trigger generation/send logic... simplified here by just calling same logic or notifying user
      output$msg_verify <- renderUI(div(class = "text-info small mb-3", "Solicita un nuevo código desde el inicio si expiró."))
    })
    
    # Step 2: Verify Code
    observeEvent(input$do_verify, {
      code <- trimws(input$otp_code)
      email <- user_email()
      
      valid <- verify_reset_token(email, code)
      
      if (valid) {
        current_step("new_password")
      } else {
        output$msg_verify <- renderUI(div(class = "alert alert-danger small mb-3", "Código incorrecto o expirado."))
      }
    })
    
    # Step 3: Change Password
    observeEvent(input$do_reset_pw, {
      new_pw <- input$new_password
      confirm_pw <- input$new_password_confirm
      email <- user_email()
      
      if (new_pw != confirm_pw) {
        output$msg_reset <- renderUI(div(class = "alert alert-danger small mb-3", "Las contraseñas no coinciden."))
        return()
      }
      
      complexity <- validate_password_complexity(new_pw)
      if (!complexity$valid) {
        output$msg_reset <- renderUI(div(class = "alert alert-danger small mb-3", complexity$message))
        return()
      }
      
      # Update Password
      update_password(email, new_pw)
      current_step("success")
    })
    
    # Navigation
    observeEvent(input$back_to_login, { parent_session$userData$view_state("login") })
    observeEvent(input$go_login_success, { parent_session$userData$view_state("login") })
  })
}
