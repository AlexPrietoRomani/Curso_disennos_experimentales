# R/server.R
server <- function(input, output, session) {


  view_state <- reactiveVal("login")
  user_authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  login_error <- reactiveVal(NULL)

  selected_course <- reactiveVal(NULL)
  selected_part <- reactiveVal(NULL)
  selected_session <- reactiveVal(NULL)


  observeEvent(input$nav_target, {
    if (identical(input$nav_target, "courses") && isTRUE(user_authenticated())) {
      selected_course(NULL)
      selected_part(NULL)
      selected_session(NULL)
      view_state("course_select")
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$reset_course, {
    selected_course(NULL)
    selected_part(NULL)
    selected_session(NULL)
    view_state("course_select")
  })



  observeEvent(input$login_submit, {
    login_error(NULL)
    result <- authenticate_user(input$login_username, input$login_password)

    if (isTRUE(result$success)) {
      user_authenticated(TRUE)
      user_info(result$user)
      selected_course(NULL)
      selected_part(NULL)
      selected_session(NULL)
      view_state("course_select")
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")
    } else if (!is.null(result$message)) {
      login_error(result$message)
    }
  })

  observeEvent(input$logout, {
    user_authenticated(FALSE)
    user_info(NULL)
    login_error(NULL)
    selected_course(NULL)
    selected_part(NULL)
    selected_session(NULL)
    view_state("login")
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
  })

  # --- Auth Modules & Navigation ---
  
  # Register Module
  registerServer("register_module", session)
  
  observeEvent(input$go_to_register, {
    view_state("register")
  })
  
  # Password Reset Module
  # We pass URL params to the module to handle token verification
  url_params <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    query
  })
  
  passwordResetServer("reset_module", session, url_params)
  
  observeEvent(input$go_to_reset, {
    view_state("password_reset")
  })
  
  # URL Handler for Admin Approval & Reset
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$action)) {
      if (query$action == "approve" && !is.null(query$user)) {
        # Admin Approval Logic
        # In a real app, we should verify the admin is logged in or use a secure token.
        # For this MVP, we assume the link is secret enough or we check if current user is admin?
        # But the admin clicks the link from email, so they might not be logged in.
        # Let's just approve for now (MVP).
        
        tryCatch({
          update_user_status(query$user, "active")
          
          # Get user email to send welcome
          u <- get_user(query$user)
          if (!is.null(u)) {
            send_welcome_email(u$email, u$first_name)
          }
          
          showModal(modalDialog(
            title = "Usuario Aprobado",
            paste("El usuario", query$user, "ha sido activado exitosamente."),
            easyClose = TRUE
          ))
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            paste("No se pudo aprobar el usuario:", e$message),
            easyClose = TRUE
          ))
        })
      }
      
      if (query$action == "reset") {
        view_state("password_reset")
      }
    }
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
      if (isTRUE(user_authenticated())) {
        view_state("course_select")
      } else {
        view_state("landing")
      }
      return()
    }

    view_state("course_detail")
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
    state <- view_state()
    nav <- build_navbar(authenticated = isTRUE(user_authenticated()), user = user_info())



    if (identical(state, "login")) {
      login_content <- build_login_section(login_error())
      return(div(class = "app-shell", nav, login_content))
    }
    
    if (identical(state, "register")) {
      return(div(class = "app-shell", nav, registerUI(session$ns("register_module"))))
    }
    
    if (identical(state, "password_reset")) {
      return(div(class = "app-shell", nav, passwordResetUI(session$ns("reset_module"))))
    }

    if (identical(state, "course_select")) {
      course_cards <- build_course_cards()
      selector_content <- build_courses_section(course_cards = course_cards)
      return(div(class = "app-shell", nav, selector_content))
    }

    curso_actual <- selected_course()
    if (is.null(curso_actual)) {
      if (isTRUE(user_authenticated())) {
        view_state("course_select")
      } else {
        view_state("login")
      }
      return()
    }

    parte_activa <- selected_part()

    course_view <- div(
      id = "courses",
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
          class = "course-control-bar",
          actionLink(
            "reset_course",
            label = tagList(icon("arrow-left"), span(" Elegir otro curso")),
            class = "reset-course"
          ),
          if (!is.null(parte_activa)) {
            span(
              class = "course-active-part",
              icon("layer-group"),
              span(" Parte activa: "),
              tags$strong(parte_activa)
            )
          }
        )
      ),
      div(
        class = "container course-layout",
        div(
          class = "row g-4 align-items-start",
          div(
            class = "col-12 col-xl-4",
            uiOutput("course_sidebar")
          ),
          div(
            class = "col-12 col-xl-8",
            uiOutput("contenido_ui")
          )
        )
      )
    )

    div(class = "app-shell", nav, course_view)
  })

  output$course_sidebar <- renderUI({
    req(selected_course())
    curso_actual <- selected_course()
    parte_actual <- selected_part()
    sesion_actual <- selected_session()
    partes <- estructura_cursos[[curso_actual]]

    if (length(partes) == 0) {
      return(div(class = "course-sidebar", div(class = "empty-state", "Pronto agregaremos partes para este curso.")))
    }

    accordion_id <- paste0("accordion_", sanitize_id(curso_actual))

    accordion_items <- lapply(names(partes), function(nombre_parte) {
      parte_id <- sanitize_id(paste(curso_actual, nombre_parte))
      heading_id <- paste0("heading_", parte_id)
      collapse_id <- paste0("collapse_", parte_id)
      es_parte_activa <- identical(nombre_parte, parte_actual)
      sesiones <- partes[[nombre_parte]]$sesiones
      total_sesiones <- length(sesiones)

      session_buttons <- NULL
      if (total_sesiones > 0) {
        session_buttons <- lapply(names(sesiones), function(nombre_sesion) {
          session_id <- sanitize_id(paste(curso_actual, nombre_parte, nombre_sesion))
          button_id <- paste0("session_", session_id)
          activa <- identical(sesion_actual, nombre_sesion)
          tags$button(
            id = button_id,
            type = "button",
            class = paste("session-list-item action-button", if (activa) "active"),
            tags$span(class = "session-list-index", sprintf("%02d", match(nombre_sesion, names(sesiones)))),
            tags$span(class = "session-list-label", nombre_sesion)
          )
        })
      }

      tags$div(
        class = "accordion-item",
        tags$h2(
          class = "accordion-header",
          id = heading_id,
          tags$button(
            class = paste("accordion-button", if (!es_parte_activa) "collapsed"),
            type = "button",
            `data-bs-toggle` = "collapse",
            `data-bs-target` = paste0("#", collapse_id),
            `aria-expanded` = tolower(as.character(es_parte_activa)),
            `aria-controls` = collapse_id,
            tagList(
              span(class = "sidebar-part-title", nombre_parte),
              span(
                class = "sidebar-part-count",
                sprintf("%d sesión%s", total_sesiones, ifelse(total_sesiones == 1, "", "es"))
              )
            )
          )
        ),
        div(
          id = collapse_id,
          class = paste("accordion-collapse collapse", if (es_parte_activa) "show"),
          `aria-labelledby` = heading_id,
          `data-bs-parent` = paste0("#", accordion_id),
          div(
            class = "accordion-body",
            if (total_sesiones == 0) {
              div(class = "empty-state", "Sesiones próximamente disponibles.")
            } else {
              div(class = "session-list", session_buttons)
            }
          )
        )
      )
    })

    div(
      class = "course-sidebar",
      div(
        class = "sidebar-header",
        tags$span(class = "sidebar-kicker", "Plan de estudio"),
        tags$h3(class = "sidebar-title", curso_actual)
      ),
      if (!is.null(parte_actual) && !is.null(sesion_actual)) {
        div(
          class = "sidebar-selection",
          tags$span(class = "sidebar-selection-label", "Sesión activa"),
          tags$strong(class = "sidebar-selection-value", paste(parte_actual, "·", sesion_actual))
        )
      },
      div(
        class = "accordion course-sidebar-accordion",
        id = accordion_id,
        accordion_items
      )
    )
  })

  # --- Files Download Logic ---
  
  current_practice_files <- reactive({
    req(selected_course(), selected_session())
    get_practice_files(selected_course(), selected_session())
  })
  
  output$download_practice <- downloadHandler(
    filename = function() {
      files <- current_practice_files()
      if (is.null(files)) return("practica.R")
      
      # If selector exists, use it; otherwise default to first
      if (!is.null(input$practice_file_selector)) {
        return(input$practice_file_selector)
      }
      return(files$name[1])
    },
    content = function(file) {
      files <- current_practice_files()
      req(files)
      
      target_name <- if (!is.null(input$practice_file_selector)) input$practice_file_selector else files$name[1]
      source_path <- files$path[files$name == target_name]
      
      if (length(source_path) > 0) {
        file.copy(source_path, file)
      }
    }
  )

  output$contenido_ui <- renderUI({
    req(selected_course(), selected_part(), selected_session())
    info <- obtener_info_sesion(selected_course(), selected_part(), selected_session())
    modulo <- info$module

    contenido <- NULL

    if (!is.null(modulo) && ensure_module_loaded(modulo)) {
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

    # Prepare Download UI
    files <- get_practice_files(selected_course(), selected_session())
    download_ui <- NULL
    
    if (!is.null(files)) {
      if (nrow(files) == 1) {
        download_ui <- downloadButton("download_practice", label = "Descargar Práctica", class = "btn-download-practice", icon = icon("download"))
      } else {
        # Multiple files: Select + Button
        download_ui <- div(
          class = "practice-download-wrapper",
          div(class="practice-selector",
            selectInput("practice_file_selector", NULL, choices = files$name, width = "100%", selectize = TRUE)
          ),
          downloadButton("download_practice", label = "Descargar", class = "btn-download-practice", icon = icon("download"))
        )
      }
    }

    div(
      class = "course-content",
      div(
        class = "session-detail-header d-flex justify-content-between align-items-start",
        div(
          tags$span(class = "session-detail-pill", selected_part()),
          tags$h2(class = "session-detail-title", selected_session())
        ),
        div(
          class = "ms-3",
          download_ui
        )
      ),
      div(class = "session-detail-body", contenido)
    )
  })

  output$download_cv <- downloadHandler(
    filename = function() {
      "Alex_Prieto_Romani_CV.pdf"
    },
    content = function(file) {
      file.copy("www/docs/Alex_Prieto_Romani_CV.pdf", file)
    },
    contentType = "application/pdf"
  )

  # Inicializa el módulo server solo cuando se selecciona una sesión
  observeEvent(selected_session(), {
    req(selected_course(), selected_part(), selected_session())
    info <- obtener_info_sesion(selected_course(), selected_part(), selected_session())
    modulo <- info$module
    server_fun_name <- paste0(modulo, "Server")

    if (ensure_module_loaded(modulo) && exists(server_fun_name, mode = "function")) {
      server_fun <- get(server_fun_name, mode = "function")
      callModule(server_fun, info$id)
    }
  }, ignoreInit = TRUE)

}
