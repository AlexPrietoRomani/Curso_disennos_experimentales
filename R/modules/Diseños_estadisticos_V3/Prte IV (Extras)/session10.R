# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session10.R
# Sesión 10: MANOVA

# -------------------------------------------------------------------------
# UI per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (MANOVA)
pestanna1_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto (MANOVA)",
    h4(class = "section-header", "¿Cuándo necesitamos una MANOVA?"),
    div(
      class = "alert alert-info",
      p(
        "La MANOVA extiende el ANOVA a situaciones con ",
        strong("múltiples respuestas correlacionadas"),
        " (ej. rendimiento, Brix y firmeza medidas en la misma parcela)."
      )
    ),
    tags$ul(
      tags$li("Objetivo: probar el efecto de tratamientos sobre un vector de respuestas, no una sola Y."),
      tags$li("Ventaja: aprovecha la correlación entre variables de respuesta."),
      tags$li("Conexión con el curso: misma lógica de diseños de campo, pero con respuesta multivariada.")
    )
  )
}

# Pestaña 2: Conceptos clave MANOVA
pestanna2_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave & modelo",
    h4("Modelo MANOVA básico"),
    withMathJax(
      div(
        class = "alert alert-info",
        p("Modelo vectorial (simplificado):"),
        helpText("$$\\mathbf{Y}_i = \\boldsymbol{\\mu} + \\boldsymbol{\\alpha}_{g(i)} + \\boldsymbol{\\epsilon}_i$$"),
        p("donde ", em("\\(\\mathbf{Y}_i\\)"),
          " es el vector de respuestas para la unidad i, y ",
          em("\\(\\boldsymbol{\\alpha}_{g(i)}\\)"),
          " es el efecto del grupo/tratamiento.")
      )
    ),
    fluidRow(
      column(
        width = 6,
        h5("Elementos de la MANOVA"),
        tags$ul(
          tags$li("Múltiples Y cuantitativas medidas en la misma unidad experimental."),
          tags$li("Uno o más factores categóricos como efectos fijos (ej. tratamiento, variedad).")
        )
      ),
      column(
        width = 6,
        h5("Estadísticos globales"),
        tags$ul(
          tags$li("Wilks' Lambda, Pillai, Hotelling-Lawley, Roy."),
          tags$li("Sirven para probar H0: el vector de medias es igual entre tratamientos.")
        )
      )
    )
  )
}

# Pestaña 3: Ejemplo aplicado (estructura)
pestanna3_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Ejemplo aplicado",
    p("Ejemplo de MANOVA con 2–3 respuestas continuas (placeholder)."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Fuente de datos"),
        radioButtons(
          ns("manova_example_source"),
          "Selecciona los datos de ejemplo:",
          choices = c("Dataset simulado (interno)" = "sim",
                      "Cargar un CSV propio"      = "upload")
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("manova_example_source")),
          fileInput(ns("manova_file"), "Cargar archivo CSV:", accept = ".csv"),
          helpText("Se espera: columnas de factor (grupo/tratamiento) y varias Y cuantitativas.")
        )
      ),
      mainPanel(
        width = 8,
        h5("Vista previa de datos"),
        DT::dataTableOutput(ns("manova_example_data")),
        hr(),
        h5("Resumen MANOVA (placeholder)"),
        verbatimTextOutput(ns("manova_example_model")),
        hr(),
        h5("Alguna visualización básica (placeholder)"),
        plotOutput(ns("manova_example_plot"), height = "320px")
      )
    )
  )
}

# Pestaña 4: Simulación MANOVA
pestanna4_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & correlación entre respuestas",
    p("Simulación conceptual para ilustrar cómo la correlación entre respuestas influye en la MANOVA."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Parámetros de simulación (placeholder)"),
        numericInput(ns("manova_n"), "N° observaciones por tratamiento:", value = 30, min = 6),
        numericInput(ns("manova_rho"), "Correlación entre Y1 y Y2:", value = 0.5, min = -0.9, max = 0.9, step = 0.1),
        actionButton(ns("btn_sim_manova"), "Simular y ajustar MANOVA", class = "btn btn-success w-100 mt-2")
      ),
      mainPanel(
        width = 8,
        h5("Resultados de simulación (placeholder)"),
        verbatimTextOutput(ns("manova_sim_results")),
        hr(),
        h5("Gráfico de dispersión Y1 vs Y2 por tratamiento (placeholder)"),
        plotOutput(ns("manova_sim_plot"), height = "320px")
      )
    )
  )
}

# Pestaña 5: Protocolo datos reales MANOVA
pestanna5_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo datos reales",
    h4("Checklist para MANOVA con datos reales"),
    tags$ol(
      tags$li("Identificar un conjunto de respuestas cuantitativas medidas en la misma unidad."),
      tags$li("Verificar correlaciones entre respuestas (matriz de correlación, diagramas de dispersión)."),
      tags$li("Ajustar un modelo MANOVA básico y revisar los tests globales (Wilks, Pillai, etc.)."),
      tags$li("Si hay efecto global significativo, pasar a ANOVAs univariados y comparaciones post-hoc."),
      tags$li("Integrar resultados en una interpretación agronómica multivariada.")
    )
  )
}

# Pestaña 6: Ejercicios
pestanna6_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios & tareas",
    tags$ol(
      tags$li("Tomar un dataset con varias respuestas (ej. rendimiento, tamaño, color) y un factor de tratamiento."),
      tags$li("Ajustar una MANOVA y contrastar los resultados con ANOVAs separados para cada respuesta."),
      tags$li("Explorar cómo cambia el test global si se elimina una de las respuestas."),
      tags$li("Preparar un breve reporte integrando la evidencia multivariada.")
    )
  )
}

# Pestaña 7: Referencias
pestanna7_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    h5("Referencias para MANOVA"),
    tags$ul(
      tags$li("Capítulos de MANOVA en textos de análisis multivariado."),
      tags$li("Notas de cursos que expliquen Wilks' Lambda y otros criterios."),
      tags$li("Ejemplos en R usando la función manova() y summary().")
    )
  )
}

# -------------------------------------------------------------------------
# Main UI Sesión 10
# -------------------------------------------------------------------------

session10_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 10: MANOVA")
    ),
    navset_tab(
      pestanna1_session10_v3UI(ns),
      pestanna2_session10_v3UI(ns),
      pestanna3_session10_v3UI(ns),
      pestanna4_session10_v3UI(ns),
      pestanna5_session10_v3UI(ns),
      pestanna6_session10_v3UI(ns),
      pestanna7_session10_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server per Tab Sesión 10
# -------------------------------------------------------------------------

pestanna1_session10_v3_server <- function(input, output, session) {}
pestanna2_session10_v3_server <- function(input, output, session) {}

pestanna3_session10_v3_server <- function(input, output, session) {
  output$manova_example_data <- DT::renderDataTable({
    # Placeholder simple
    df <- data.frame(
      tratamiento = rep(LETTERS[1:3], each = 5),
      Y1 = rnorm(15, 100, 10),
      Y2 = rnorm(15, 50, 5)
    )
    DT::datatable(df, options = list(pageLength = 5, scrollX = TRUE))
  })
  
  output$manova_example_model <- renderPrint({
    cat("Aquí irá el resumen de una MANOVA (manova(), summary(), etc.)\n")
  })
  
  output$manova_example_plot <- renderPlot({
    plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
    text(1, 1, "Gráfico Y1 vs Y2 por tratamiento (placeholder)", cex = 1)
  })
}

pestanna4_session10_v3_server <- function(input, output, session) {
  observeEvent(input$btn_sim_manova, {
    showNotification("Simulación MANOVA: lógica por implementar.", type = "message", duration = 3)
  })
  
  output$manova_sim_results <- renderPrint({
    cat("Aquí se mostrarán los tests globales de MANOVA en la simulación (placeholder).\n")
  })
  
  output$manova_sim_plot <- renderPlot({
    plot(1, type = "n", xlab = "", ylab = "", axes = FALSE)
    text(1, 1, "Gráfico de simulación MANOVA (placeholder)", cex = 1)
  })
}

pestanna5_session10_v3_server <- function(input, output, session) {}
pestanna6_session10_v3_server <- function(input, output, session) {}
pestanna7_session10_v3_server <- function(input, output, session) {}

# -------------------------------------------------------------------------
# Main Server Sesión 10
# -------------------------------------------------------------------------

session10_v3Server <- function(input, output, session) {
  # has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
  
  pestanna1_session10_v3_server(input, output, session)
  pestanna2_session10_v3_server(input, output, session)
  pestanna3_session10_v3_server(input, output, session)
  pestanna4_session10_v3_server(input, output, session)
  pestanna5_session10_v3_server(input, output, session)
  pestanna6_session10_v3_server(input, output, session)
  pestanna7_session10_v3_server(input, output, session)
}
