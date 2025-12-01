
# R/modules/auth/login_ui.R

build_login_section <- function(error_message = NULL) {
  div(
    id = "courses",
    class = "section courses-section login-section",
    div(
      class = "container",
      tags$h2(class = "section-title", "Acceso a cursos"),
      tags$p(
        class = "section-intro",
        "Inicia sesión con tus credenciales para acceder a los contenidos protegidos."
      ),
      div(
        class = "login-card shadow-sm p-4 bg-white rounded",
        style = "max-width: 500px; margin: 0 auto;", # Center and constrain width
        if (!is.null(error_message)) {
          div(class = "alert alert-danger", role = "alert", error_message)
        },
        textInput("login_username", "Usuario", placeholder = "usuario"),
        passwordInput("login_password", "Contraseña", placeholder = "********"),
        div(
          class = "mt-4",
          div(
            class = "d-grid gap-2 d-md-flex justify-content-md-between",
            actionButton("login_submit", label = tagList(icon("lock"), span(" Iniciar sesión")), class = "btn btn-primary flex-grow-1 me-md-2"),
            actionButton("go_to_register", label = tagList(icon("user-plus"), span(" Crear cuenta nueva")), class = "btn btn-outline-primary flex-grow-1")
          ),
          div(
            class = "text-center mt-3",
            actionLink("go_to_reset", "¿Olvidaste tu contraseña?", class = "text-decoration-none text-muted")
          )
        ),
        tags$div(
          class = "support-contact mt-4 text-center",
          tags$p(class = "mb-2", "¿Necesitas ayuda con tus credenciales?"),
          tags$a(
            href = support_mailto_link(subject = "Soporte Cursos - Acceso"),
            class = "btn btn-link",
            icon("envelope"),
            " Contactar soporte"
          )
        ),
        tags$p(
          class = "text-muted small mt-3 text-center",
          "Las contraseñas se almacenan cifradas por su seguridad."
        )
      )
    )
  )
}
