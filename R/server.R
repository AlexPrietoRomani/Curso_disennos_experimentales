# R/server.R
server <- function(input, output, session) {
  sanitize_id <- function(text) {
    id <- iconv(text, from = "", to = "ASCII//TRANSLIT")
    if (is.na(id)) {
      id <- text
    }
    id <- tolower(id)
    id <- gsub("[^a-z0-9]+", "_", id)
    id <- gsub("(^_|_$)", "", id)
    id
  }

  selected_course <- reactiveVal(NULL)
  selected_part <- reactiveVal(NULL)
  selected_session <- reactiveVal(NULL)

  observeEvent(input$reset_course, {
    selected_course(NULL)
    selected_part(NULL)
    selected_session(NULL)
  })

  for (curso in names(estructura_cursos)) {
    local({
      course_name <- curso
      course_id <- paste0("course_", sanitize_id(course_name))
      observeEvent(input[[course_id]], {
        selected_course(course_name)
      }, ignoreNULL = TRUE)
    })
  }

  observeEvent(selected_course(), {
    curso <- selected_course()
    if (is.null(curso)) {
      selected_part(NULL)
      selected_session(NULL)
      return()
    }

    partes <- names(estructura_cursos[[curso]])
    if (length(partes) == 0) {
      selected_part(NULL)
      selected_session(NULL)
      return()
    }

    if (!is.null(selected_part()) && selected_part() %in% partes) {
      part_actual <- selected_part()
    } else {
      part_actual <- partes[1]
    }

    selected_part(part_actual)
  }, ignoreNULL = FALSE)

  observeEvent(selected_part(), {
    curso <- selected_course()
    parte <- selected_part()

    if (is.null(curso) || is.null(parte)) {
      selected_session(NULL)
      return()
    }

    sesiones <- names(estructura_cursos[[curso]][[parte]]$sesiones)
    if (length(sesiones) == 0) {
      selected_session(NULL)
      return()
    }

    if (!is.null(selected_session()) && selected_session() %in% sesiones) {
      sesion_actual <- selected_session()
    } else {
      sesion_actual <- sesiones[1]
    }

    selected_session(sesion_actual)
  }, ignoreNULL = FALSE)

  observe({
    curso <- selected_course()
    parte <- selected_part()
    if (is.null(curso) || is.null(parte)) {
      return()
    }

    partes <- names(estructura_cursos[[curso]])
    if (!is.null(input$part_selector)) {
      updateSelectInput(
        session,
        "part_selector",
        choices = partes,
        selected = parte
      )
    }
  })

  observeEvent(input$part_selector, {
    req(input$part_selector)
    if (!identical(selected_part(), input$part_selector)) {
      selected_part(input$part_selector)
    }
  })

  for (curso in names(estructura_cursos)) {
    partes <- estructura_cursos[[curso]]
    for (parte in names(partes)) {
      sesiones <- partes[[parte]]$sesiones
      for (sesion in names(sesiones)) {
        local({
          curso_name <- curso
          parte_name <- parte
          sesion_name <- sesion
          button_id <- paste0(
            "session_",
            sanitize_id(paste(curso_name, parte_name, sesion_name))
          )
          observeEvent(input[[button_id]], {
            selected_course(curso_name)
            selected_part(parte_name)
            selected_session(sesion_name)
          }, ignoreNULL = TRUE)
        })
      }
    }
  }

  output$main_ui <- renderUI({
    curso_actual <- selected_course()

    if (is.null(curso_actual)) {
      course_cards <- lapply(names(estructura_cursos), function(nombre_curso) {
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

      return(
        div(
          class = "landing-page",
          div(
            class = "container",
            div(
              class = "landing-intro text-center",
              tags$span(class = "landing-kicker", "Explora el temario digital"),
              tags$h1(
                class = "landing-title",
                "Diseños experimentales para la agro data science"
              ),
              tags$p(
                class = "landing-subtitle",
                "Selecciona primero un curso para descubrir las sesiones disponibles, diseñadas con un enfoque minimalista y profesional."
              )
            ),
            div(class = "course-grid", course_cards)
          )
        )
      )
    }

    partes <- names(estructura_cursos[[curso_actual]])

    tagList(
      div(
        class = "course-page",
        div(
          class = "course-hero",
          div(
            class = "container",
            tags$span(class = "course-hero-kicker", "Curso seleccionado"),
            tags$h1(class = "course-hero-title", curso_actual),
            tags$p(
              class = "course-hero-subtitle",
              "Elige la parte y la sesión que deseas explorar. Cada módulo combina estadística, agro y ciencia de datos."
            )
          )
        ),
        div(
          class = "container course-controls",
          div(
            class = "row align-items-center gy-3",
            div(
              class = "col-md-6",
              actionLink(
                "reset_course",
                label = tagList(icon("arrow-left"), span(" Elegir otro curso")),
                class = "reset-course"
              )
            ),
            div(
              class = "col-md-6 text-md-end",
              selectInput(
                "part_selector",
                "Parte del temario",
                choices = partes,
                selected = selected_part(),
                width = "100%"
              )
            )
          )
        ),
        div(
          class = "container session-grid-container",
          uiOutput("session_cards")
        ),
        div(
          class = "container session-content-container",
          uiOutput("contenido_ui")
        )
      )
    )
  })

  output$session_cards <- renderUI({
    req(selected_course(), selected_part())
    curso_actual <- selected_course()
    parte_actual <- selected_part()
    sesiones <- estructura_cursos[[curso_actual]][[parte_actual]]$sesiones

    if (length(sesiones) == 0) {
      return(div(class = "empty-state", "Pronto agregaremos sesiones para esta parte."))
    }

    cards <- lapply(names(sesiones), function(nombre_sesion) {
      session_id <- sanitize_id(paste(curso_actual, parte_actual, nombre_sesion))
      button_id <- paste0("session_", session_id)
      activa <- identical(selected_session(), nombre_sesion)
      tags$button(
        id = button_id,
        type = "button",
        class = paste("session-card action-button", if (activa) "active"),
        style = sprintf("--session-image: url('images/sesiones/%s.jpg');", session_id),
        tags$div(class = "session-card-overlay"),
        tags$div(
          class = "session-card-body",
          tags$span(class = "session-card-label", parte_actual),
          tags$h4(class = "session-card-title", nombre_sesion)
        )
      )
    })

    div(class = "session-grid", cards)
  })

  output$contenido_ui <- renderUI({
    req(selected_course(), selected_part(), selected_session())
    info <- obtener_info_sesion(selected_course(), selected_part(), selected_session())
    modulo <- info$module

    contenido <- NULL

    if (!is.null(modulo)) {
      ui_fun_name <- paste0(modulo, "UI")
      if (exists(ui_fun_name, mode = "function")) {
        ui_fun <- get(ui_fun_name, mode = "function")
        contenido <- ui_fun(info$id)
      }
    }

    if (is.null(contenido)) {
      contenido <- div(
        class = "alert alert-info",
        "Contenido próximamente disponible para esta sesión."
      )
    }

    tagList(
      div(
        class = "session-detail-header",
        tags$span(class = "session-detail-pill", selected_part()),
        tags$h2(class = "session-detail-title", selected_session())
      ),
      div(class = "session-detail-body", contenido)
    )
  })

  for (module_name in names(module_registry)) {
    server_fun_name <- paste0(module_name, "Server")
    if (exists(server_fun_name, mode = "function")) {
      server_fun <- get(server_fun_name, mode = "function")
      lapply(module_registry[[module_name]], function(id) callModule(server_fun, id))
    }
  }
}
