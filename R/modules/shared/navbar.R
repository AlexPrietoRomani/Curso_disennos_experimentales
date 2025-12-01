
# R/modules/shared/navbar.R

build_navbar <- function(authenticated = FALSE, user = NULL) {
  tags$nav(
    class = "primary-nav",
    div(
      class = "container d-flex align-items-center justify-content-between",
      tags$a(
        href = "#home",
        class = "brand-link",
        onclick = "Shiny.setInputValue('nav_target', 'home', {priority: 'event'});",
        "Alex Prieto Romani"
      ),
      tags$button(
        type = "button",
        class = "nav-toggle",
        `aria-expanded` = "false",
        `aria-label` = "Abrir menú de navegación",
        tags$span(class = "nav-toggle-bar"),
        tags$span(class = "nav-toggle-bar"),
        tags$span(class = "nav-toggle-bar")
      ),
      tags$ul(
        class = "nav-links",
        tags$li(tags$a(href = "#courses", class = "nav-link", onclick = "Shiny.setInputValue('nav_target', 'courses', {priority: 'event'});", "Cursos")),
        tags$li(tags$a(href = "#projects", class = "nav-link", onclick = "Shiny.setInputValue('nav_target', 'projects', {priority: 'event'});", "Proyectos")),
        tags$li(tags$a(href = "#cv", class = "nav-link", onclick = "Shiny.setInputValue('nav_target', 'cv', {priority: 'event'});", "Descarga mi CV")),
        tags$li(tags$a(href = "#contact", class = "nav-link", onclick = "Shiny.setInputValue('nav_target', 'contact', {priority: 'event'});", "Contáctame")),
        if (authenticated) {
          tagList(
            tags$li(
              class = "nav-item nav-user",
              tags$span(
                class = "nav-link disabled",
                sprintf("Hola, %s", sanitize_credential_input(user$username %||% "usuario"))
              )
            ),
            tags$li(
              actionLink(
                "logout",
                label = tagList(icon("right-from-bracket"), span(" Cerrar sesión")),
                class = "nav-link nav-link-action"
              )
            )
          )
        }
      )
    )
  )
}
