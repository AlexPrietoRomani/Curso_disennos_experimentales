
# R/modules/shared/navbar.R

build_navbar <- function(authenticated = FALSE, user = NULL) {
  tags$nav(
    class = "primary-nav",
    div(
      class = "container d-flex align-items-center justify-content-between",
      
      # Brand / Logo
      tags$a(
        href = "https://alexprietoromani.github.io/",
        class = "brand-link d-flex align-items-center gap-2",
        target = "_blank",
        icon("leaf", class = "text-success"), # Added an icon for professionalism
        span("Alex Prieto Romani")
      ),
      
      # Mobile Toggle (hidden on desktop via CSS usually)
      tags$button(
        type = "button",
        class = "nav-toggle",
        `aria-expanded` = "false",
        `aria-label` = "Abrir menú",
        tags$span(class = "nav-toggle-bar"),
        tags$span(class = "nav-toggle-bar"),
        tags$span(class = "nav-toggle-bar")
      ),
      
      # Right Links
      div(
        class = "nav-links d-flex align-items-center gap-3",
        if (authenticated) {
          tagList(
            
            # User Greetings Badge
            div(
              class = "d-none d-md-flex align-items-center gap-2 px-3 py-1 rounded-pill bg-light text-muted border",
              icon("user-circle"),
              span(
                class = "fw-bold small",
                sanitize_credential_input(user$username %||% "Usuario")
              )
            ),
            
            # Logout Button (Styled as a ghost button or similar)
            actionLink(
              "logout",
              label = tagList(icon("power-off"), span(" Salir")),
              class = "btn btn-sm btn-outline-danger border-0 fw-semibold d-flex align-items-center gap-2"
            )
          )
        }
      )
    )
  )
}
