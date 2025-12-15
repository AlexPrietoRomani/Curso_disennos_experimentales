
# R/modules/auth/login_ui.R

build_login_section <- function(error_message = NULL) {
  div(
    class = "login-section",
    div(
      class = "login-wrapper",
      div(
        class = "login-card",
        # Header Visual
        div(
          class = "login-header",
          div(
            class = "login-icon-circle",
            icon("seedling", class = "fa-lg")
          ),
          tags$h2(class = "login-title", "Acceso a Cursos"),
          tags$p(class = "login-subtitle", "Autoria: Alex Prieto Romani")
        ),
        
        # Form Body
        div(
          class = "login-body",
          
          if (!is.null(error_message)) {
            div(
              class = "alert alert-danger d-flex align-items-center mb-4", 
              role = "alert", 
              icon("exclamation-triangle", class="me-2"),
              div(error_message)
            )
          },
          
          div(
            class = "login-form-group",
            tags$label("Usuario"),
            textInput("login_username", label = NULL, placeholder = "Ej. usuario123", width = "100%")
          ),
          
          div(
            class = "login-form-group mb-4",
            tags$label("Contraseña"),
            passwordInput("login_password", label = NULL, placeholder = "••••••••", width = "100%")
          ),
          
          actionButton(
            "login_submit", 
            label = "INICIAR SESIÓN", 
            class = "btn-login-main mb-3"
          ),
          
          div(
            class = "text-center",
            actionLink("go_to_reset", "¿Olvidaste tu contraseña?", class = "login-link small")
          )
        ),
        
        # Footer / Secondary Actions
        div(
          class = "login-footer",
          div(
            class = "d-grid gap-2",
            actionButton(
              "go_to_register", 
              label = "Crear Cuenta Nueva", 
              class = "btn btn-register-secondary"
            )
          ),
          
          div(
            class = "login-support-block",
            span("¿Tienes problemas?"),
            tags$a(
              href = support_mailto_link(subject = "Soporte Acceso"),
              class = "login-link ms-1",
              "Contactar soporte"
            )
          ),
          
          div(
            class = "login-secure-note",
            icon("lock"), span("Acceso seguro cifrado")
          )
        )
      )
    )
  )
}
