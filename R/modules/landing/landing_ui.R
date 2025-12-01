
# R/modules/landing/landing_ui.R

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
            src = "https://media.licdn.com/dms/image/v2/D4E03AQHe7d1z40IRvw/profile-displayphoto-shrink_800_800/B4EZdXkQIzHQAc-/0/1749520811003?e=1766016000&v=beta&t=55huZY-aTj78-q1f0IArMj4OtqL29xIBRG_pHxKjAe0",
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

build_projects_section <- function(projects_info) {
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
