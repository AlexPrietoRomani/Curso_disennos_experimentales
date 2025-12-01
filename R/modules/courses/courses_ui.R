
# R/modules/courses/courses_ui.R

build_course_cards <- function() {
  lapply(names(estructura_cursos), function(nombre_curso) {
    partes <- estructura_cursos[[nombre_curso]]
    total_partes <- length(partes)
    total_sesiones <- sum(vapply(partes, function(x) length(x$sesiones), numeric(1)))
    course_id <- sanitize_id(nombre_curso)
    tags$button(
      id = paste0("course_", course_id),
      type = "button",
      class = "course-card action-button",
      style = sprintf("--card-image: url('images/courses/%s.jpg');", course_id),
      tags$div(class = "course-card-overlay"),
      tags$div(
        class = "course-card-body",
        tags$span(class = "course-card-kicker", "Curso especializado"),
        tags$h3(class = "course-card-title", nombre_curso),
        tags$p(
          class = "course-card-meta",
          sprintf("%d parte%s · %d sesión%s", total_partes, ifelse(total_partes == 1, "", "s"), total_sesiones, ifelse(total_sesiones == 1, "", "es"))
        )
      )
    )
  })
}

build_courses_section <- function(course_cards = NULL, entry_button = NULL) {
  div(
    id = "courses",
    class = "section courses-section",
    div(
      class = "container",
      tags$h2(class = "section-title", "Cursos disponibles"),
      tags$p(
        class = "section-intro",
        "Selecciona un curso para explorar las partes y sesiones disponibles."
      ),
      if (!is.null(entry_button)) {
        div(
          class = "d-flex justify-content-center mt-4",
          entry_button
        )
      } else {
        div(class = "course-grid", course_cards)
      }
    )
  )
}
