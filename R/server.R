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

  view_state <- reactiveVal("landing")
  user_authenticated <- reactiveVal(FALSE)
  user_info <- reactiveVal(NULL)
  login_error <- reactiveVal(NULL)

  selected_course <- reactiveVal(NULL)
  selected_part <- reactiveVal(NULL)
  selected_session <- reactiveVal(NULL)

  projects_info <- list(
    list(
      name = "Predicción de viento con series temporales",
      description = "Aplicación Shiny para pronosticar velocidad y dirección del viento usando modelos de series temporales y visualizaciones interactivas.",
      link = "https://github.com/AlexPrietoRomani/app_viento",
      tags = c("Series de tiempo", "Pronóstico", "Shiny")
    ),
    list(
      name = "Detección de enfermedades en café",
      description = "Entrenamiento y despliegue de un modelo YOLO ajustado para reconocer enfermedades en hojas de café a partir de imágenes etiquetadas.",
      link = "https://github.com/AlexPrietoRomani/detection-diseases-coffee",
      tags = c("Visión computacional", "YOLO", "Agtech")
    ),
    list(
      name = "Generación y clasificación de imágenes",
      description = "Suite en Streamlit para generar imágenes con modelos de difusión locales y clasificar resultados mediante modelos pre-entrenados.",
      link = "https://github.com/AlexPrietoRomani/Generacion-Clasificacion-Imagenes-Streamlit",
      tags = c("IA Generativa", "Clasificación", "Streamlit")
    ),
    list(
      name = "DengAI: Predicción de brotes",
      description = "Modelado predictivo para la competencia DengAI, estimando la incidencia de dengue combinando clima y series históricas.",
      link = "https://github.com/AlexPrietoRomani/DengAI-Predicting-Disease-Spread",
      tags = c("Competencia", "Modelado", "Salud pública")
    )
  )

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

  build_hero_section <- function() {
    div(
      id = "home",
      class = "hero-section section",
      div(
        class = "container",
        div(
          class = "row align-items-center gy-5",
          div(
            class = "col-lg-7",
            tags$span(class = "landing-kicker", "Agriculture Data Science"),
            tags$h1(class = "hero-title", "Alex Prieto Romani"),
            tags$p(
              class = "hero-subtitle",
              "Ingeniero Agrónomo y científico de datos enfocado en agricultura de precisión, modelos predictivos y analítica aplicada al sector agroindustrial."
            ),
            tags$ul(
              class = "hero-highlights",
              tags$li("Agriculture Data Science en Hortifrut SA"),
              tags$li("Maestría en Big Data y Data Science - VIU"),
              tags$li("Consultor y formador en analítica aplicada al agro")
            ),
            div(
              class = "hero-cta",
              tags$a(
                href = "https://www.linkedin.com/in/alex-prieto-romani/",
                target = "_blank",
                class = "btn btn-primary",
                icon("linkedin"),
                " Conecta en LinkedIn"
              ),
              tags$a(
                href = "https://github.com/AlexPrietoRomani",
                target = "_blank",
                class = "btn btn-outline-primary",
                icon("github"),
                " Explora mi GitHub"
              )
            )
          ),
          div(
            class = "col-lg-5 text-center",
            tags$img(
              src = "https://media.licdn.com/dms/image/v2/D4E03AQHe7d1z40IRvw/profile-displayphoto-shrink_800_800/B4EZdXkQIzHQAc-/0/1749520811003?e=1764201600&v=beta&t=Or660-mvTYk_XE7ZldpeFH0al8WJuOseQYe1SawSKpA",
              class = "hero-stat",
              alt = "Imagen de perfíl linkedin"
            )
          )
        )
      )
    )
  }

  build_about_section <- function() {
    div(
      class = "section about-section",
      div(
        class = "container",
        tags$h2(class = "section-title", "Sobre mí"),
        div(
          class = "bio-grid",
          div(
            class = "bio-card",
            tags$h3("🇬🇧 About Me"),
            tags$p("I am an Agricultural Engineer pursuing a Master's in Big Data & Data Science. I specialise in precision agriculture and AI models that enhance decision-making, optimise resources and unlock sustainable farming solutions.")
          ),
          div(
            class = "bio-card",
            tags$h3("🇵🇪 Sobre Mí"),
            tags$p("Soy Ingeniero Agrónomo y estudiante de Big Data & Data Science. Aplico ciencia de datos y modelos de IA para optimizar la agricultura de precisión, automatizar análisis y potenciar decisiones estratégicas en el agro.")
          )
        ),
        tags$h3(class = "section-subtitle", "Focos actuales"),
        tags$ul(
          class = "focus-list",
          tags$li(strong("🌱 Agricultura de precisión:"), " monitoreo satelital, GIS y análisis multivariante para cultivos."),
          tags$li(strong("🤖 Modelos predictivos:"), " estimación de rendimiento, detección de plagas y pronóstico climático."),
          tags$li(strong("📊 Storytelling con datos:"), " dashboards en Power BI y Streamlit, junto con entrenamiento especializado.")
        )
      )
    )
  }

  build_skills_section <- function() {
    div(
      class = "section skills-section",
      div(
        class = "container",
        tags$h2(class = "section-title", "Tecnologías y herramientas"),
        div(
          class = "row gy-4",
          div(
            class = "col-md-6",
            tags$h3("Lenguajes"),
            tags$p("Python, R, Java, SQL, MongoDB"),
            tags$h3("Ciencia de datos"),
            tags$p("Pandas, NumPy, Scikit-learn, TensorFlow, PyTorch, Tidyverse, ggplot2")
          ),
          div(
            class = "col-md-6",
            tags$h3("Visualización y Apps"),
            tags$p("Matplotlib, Seaborn, Plotly, Streamlit, Power BI, Shiny"),
            tags$h3("GIS & Cloud"),
            tags$p("QGIS, ArcGIS Pro, Google Earth Engine, Azure, AWS, GCP (en progreso)")
          )
        )
      )
    )
  }

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
          class = "login-card",
          if (!is.null(error_message)) {
            div(class = "alert alert-danger", role = "alert", error_message)
          },
          textInput("login_username", "Usuario", placeholder = "usuario"),
          passwordInput("login_password", "Contraseña", placeholder = "********"),
          div(
            class = "d-flex flex-column flex-sm-row gap-2 mt-3",
            actionButton("login_submit", label = tagList(icon("lock"), span(" Iniciar sesión")), class = "btn btn-primary"),
            tags$button(
              type = "button",
              class = "btn btn-outline-secondary disabled",
              disabled = "disabled",
              icon("key"),
              " Cambiar contraseña (en desarrollo)"
            )
          ),
          tags$div(
            class = "support-contact mt-4",
            tags$p(class = "mb-2", "¿Necesitas ayuda con tus credenciales?"),
            tags$a(
              href = support_mailto_link(subject = "Soporte Cursos - Acceso"),
              class = "btn btn-link",
              icon("envelope"),
              " Contactar soporte"
            )
          ),
          tags$p(
            class = "text-muted small mt-3",
            "Las contraseñas se almacenan cifradas por su seguridad."
          )
        )
      )
    )
  }

  build_projects_section <- function() {
    project_cards <- lapply(projects_info, function(project) {
      tags$article(
        class = "project-card",
        tags$h3(class = "project-title", project$name),
        tags$p(class = "project-description", project$description),
        div(
          class = "project-tags",
          lapply(project$tags, function(tag) tags$span(class = "tag", tag))
        ),
        tags$a(href = project$link, target = "_blank", class = "project-link", "Ver en GitHub")
      )
    })

    div(
      id = "projects",
      class = "section projects-section",
      div(
        class = "container",
        tags$h2(class = "section-title", "Proyectos destacados"),
        tags$p(class = "section-intro", "Iniciativas personales y de investigación que combinan ciencia de datos, IA y experiencia agronómica."),
        div(class = "projects-grid", project_cards)
      )
    )
  }

  build_cv_section <- function() {
    div(
      id = "cv",
      class = "section cv-section",
      div(
        class = "container text-center",
        tags$h2(class = "section-title", "Descarga mi CV"),
        tags$p(class = "section-intro", "Obtén una copia actualizada de mi experiencia profesional y logros."),
        downloadButton("download_cv", "Descargar CV", class = "btn btn-primary btn-lg")
      )
    )
  }

  build_contact_section <- function() {
    div(
      id = "contact",
      class = "section contact-section",
      div(
        class = "container",
        tags$h2(class = "section-title", "Contáctame"),
        div(
          class = "contact-card",
          tags$p("¿Quieres colaborar, conocer más sobre mis cursos o invitarme a un proyecto?"),
          tags$ul(
            class = "contact-list",
            tags$li(tags$strong("Correo:"), tags$a(href = "mailto:alexprieto1997@gmail.com", " alexprieto1997@gmail.com")),
            tags$li(tags$strong("LinkedIn:"), tags$a(href = "https://www.linkedin.com/in/alex-prieto-romani/", target = "_blank", " www.linkedin.com/in/alex-prieto-romani/")),
            tags$li(tags$strong("GitHub:"), tags$a(href = "https://github.com/AlexPrietoRomani", target = "_blank", " github.com/AlexPrietoRomani"))
          )
        )
      )
    )
  }

  observeEvent(input$nav_target, {
    if (identical(input$nav_target, "courses")) {
      selected_course(NULL)
      selected_part(NULL)
      selected_session(NULL)
      view_state(if (isTRUE(user_authenticated())) "course_select" else "login")
    } else {
      view_state("landing")
    }
  }, ignoreNULL = TRUE)

  observeEvent(input$reset_course, {
    selected_course(NULL)
    selected_part(NULL)
    selected_session(NULL)
    view_state("course_select")
  })

  observeEvent(input$open_course_login, {
    if (isTRUE(user_authenticated())) {
      view_state("course_select")
    } else {
      view_state("login")
    }
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
    view_state("landing")
    updateTextInput(session, "login_username", value = "")
    updateTextInput(session, "login_password", value = "")
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

    if (identical(state, "landing")) {
      landing_content <- tagList(
        build_hero_section(),
        build_about_section(),
        build_skills_section(),
        build_courses_section(
          entry_button = actionButton(
            "open_course_login",
            label = tagList(icon("graduation-cap"), span(" Ingresar a cursos")),
            class = "btn btn-primary btn-lg"
          )
        ),
        build_projects_section(),
        build_cv_section(),
        build_contact_section()
      )

      return(div(class = "app-shell", nav, landing_content))
    }

    if (identical(state, "login")) {
      login_content <- build_login_section(login_error())
      return(div(class = "app-shell", nav, login_content))
    }

    if (identical(state, "course_select")) {
      course_cards <- build_course_cards()
      selector_content <- build_courses_section(course_cards = course_cards)
      return(div(class = "app-shell", nav, selector_content))
    }

    curso_actual <- selected_course()
    if (is.null(curso_actual)) {
      view_state("landing")
      landing_content <- tagList(
        build_hero_section(),
        build_about_section(),
        build_skills_section(),
        build_courses_section(
          entry_button = actionButton(
            "open_course_login",
            label = tagList(icon("graduation-cap"), span(" Ingresar a cursos")),
            class = "btn btn-primary btn-lg"
          )
        ),
        build_projects_section(),
        build_cv_section(),
        build_contact_section()
      )
      return(div(class = "app-shell", nav, landing_content))
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

    div(
      class = "course-content",
      div(
        class = "session-detail-header",
        tags$span(class = "session-detail-pill", selected_part()),
        tags$h2(class = "session-detail-title", selected_session())
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
