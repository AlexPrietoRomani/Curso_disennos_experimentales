# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session7.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Conceptos
pestanna1_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Conceptos",
    h4(class = "section-header", "¿Qué es un diseño Fila-Columna?"),
    div(class = "alert alert-info",
        p(
          strong("Definición:"), " Diseño donde el bloqueo se realiza en ",
          em("dos direcciones perpendiculares"), " simultáneamente (filas y columnas) ",
          "para controlar gradientes espaciales complejos."
        )
    ),
    fluidRow(
      column(
        width = 6,
        h5("Cuadrado Latino vs. Row-Column General"),
        tags$ul(
          tags$li(strong("Cuadrado Latino:"), " Restricción fuerte: N° tratamientos = N° filas = N° columnas. Muy rígido."),
          tags$li(strong("Row-Column (General):"), " Más flexible. Permite N° tratamientos ≠ N° filas/columnas. ",
                  "Usualmente se generan diseños 'alpha-latinizados' o resolubles.")
        ),
        h5("Modelo LMM típico"),
        withMathJax(
          helpText("$$y_{ijk} = \\mu + \\tau_k + \\text{Fila}_i + \\text{Columna}_j + \\epsilon_{ijk}$$"),
          p("Donde Fila y Columna suelen modelarse como ", strong("efectos aleatorios"),
            " para recuperar información inter-bloque (REML).")
        )
      ),
      column(
        width = 6,
        h5("Ventajas en campo"),
        tags$ul(
          tags$li("Controla heterogeneidad en dos direcciones (ej. pendiente y fertilidad)."),
          tags$li("Mayor precisión que RCBD si hay gradientes bidireccionales."),
          tags$li("Común en mejoramiento genético (ensayos grandes).")
        ),
        div(class = "note-cloud",
            p("Herramienta clave: ", code("FielDHub"), " o ", code("agricolae"), " para generar; ",
              code("lme4/lmerTest"), " para analizar.")
        )
      )
    )
  )
}

# Pestaña 2: Generar Diseño
pestanna2_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Generar Diseño",
    p("Genera un diseño Row-Column optimizado usando ", code("FielDHub::row_column"), "."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        numericInput(ns("n_trts"), "Número de Tratamientos:", value = 20, min = 4, step = 1),
        helpText("Se intentará ajustar una grilla rectangular (R x C) cercana."),
        numericInput(ns("n_rows"), "Número de Filas (Rows):", value = 4, min = 2),
        numericInput(ns("n_cols"), "Número de Columnas (Cols):", value = 5, min = 2),
        numericInput(ns("n_reps"), "Número de Repeticiones (Copias del diseño):", value = 1, min = 1),
        actionButton(ns("btn_gen_rc"), "Generar Diseño", class = "btn btn-primary w-100"),
        hr(),
        uiOutput(ns("ui_dl_rc"))
      ),
      mainPanel(
        width = 8,
        tags$h5("Mapa del diseño generado"),
        plotOutput(ns("plot_rc_map"), height = "400px"),
        hr(),
        tags$h5("Fieldbook (primeras filas)"),
        DT::dataTableOutput(ns("tbl_rc_book"))
      )
    )
  )
}

# Pestaña 3: Análisis (Simulación)
pestanna3_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Análisis (Simulación)",
    p("Simula datos con gradientes de fila y columna para comparar RCBD vs. Row-Column (LMM)."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros de simulación"),
        numericInput(ns("sim_mu"), "Media general:", 100),
        numericInput(ns("sim_sigma_g"), "SD Genotipo (Señal):", 5),
        numericInput(ns("sim_sigma_row"), "SD Fila (Ruido):", 8),
        numericInput(ns("sim_sigma_col"), "SD Columna (Ruido):", 8),
        numericInput(ns("sim_sigma_e"), "SD Residual:", 4),
        actionButton(ns("btn_sim_run"), "Simular y Analizar", class = "btn btn-success w-100")
      ),
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          bslib::nav_panel("Visualización Espacial",
                    plotOutput(ns("plot_sim_spatial"), height = "350px"),
                    helpText("Muestra la respuesta simulada (Y) en el campo.")
          ),
          bslib::nav_panel("Comparación Modelos",
                    tags$h5("Modelo 1: RCBD (Ignora filas/cols, solo usa rep)"),
                    verbatimTextOutput(ns("out_rcbd")),
                    tags$hr(),
                    tags$h5("Modelo 2: Row-Column LMM (Fila + Columna aleatorias)"),
                    verbatimTextOutput(ns("out_rclmm")),
                    tags$div(class="alert alert-warning",
                             "Observa la reducción del error estándar de las diferencias (SED) o el AIC si corresponde.")
          ),
          bslib::nav_panel("VarCorr & Heredabilidad",
                    tags$h5("Componentes de Varianza (LMM)"),
                    verbatimTextOutput(ns("out_vc")),
                    tags$p("Si Fila/Columna capturan mucha varianza, el diseño fue exitoso.")
          )
        )
      )
    )
  )
}

# Pestaña 4: Ejercicios
pestanna4_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Ejercicios",
    tags$ol(
      tags$li("Genera un diseño para 24 tratamientos en una grilla de 4x6."),
      tags$li("Simula una situación con fuerte gradiente de columna (SD Columna = 15) y poco efecto fila."),
      tags$li("Ajusta el modelo Row-Column y verifica en 'VarCorr' que la varianza de Columna sea alta."),
      tags$li("Compara con un RCBD simple: ¿cuánto mejora el error estándar de los genotipos al usar Row-Column?"),
      tags$li("Exporta el fieldbook y practícalo en tu propio software si lo deseas.")
    )
  )
}

# Pestaña 5: Referencias
pestanna5_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("FielDHub: A Shiny App for Design of Experiments in Agriculture. ",
              tags$a(href="https://cran.r-project.org/package=FielDHub", target="_blank", "CRAN Link")),
      tags$li("John, J.A. & Williams, E.R. (1995). Cyclic and Computer Generated Designs."),
      tags$li("Piepho, H.P. et al. (2003). A comparison of results from a resolvable row-column design... ",
              em("Theoretical and Applied Genetics."))
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session7_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 7: Diseños Fila-Columna (Row-Column Designs)")
    ),
    navset_tab(
      pestanna1_session7_v3UI(ns),
      pestanna2_session7_v3UI(ns),
      pestanna3_session7_v3UI(ns),
      pestanna4_session7_v3UI(ns),
      pestanna5_session7_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

pestanna1_session7_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna2_session7_v3_server <- function(input, output, session, rc_design, has_pkg) {
  ns <- session$ns
  
  observeEvent(input$btn_gen_rc, {
    if (!has_pkg("FielDHub")) {
      showNotification("Instale 'FielDHub' para usar esta función.", type="error")
      return()
    }

    nt <- input$n_trts
    nr <- input$n_rows
    nc <- input$n_cols
    reps <- input$n_reps

    if (nt > nr*nc) {
      showNotification("Error: N tratamientos > N celdas (Filas x Cols). Aumente filas o columnas.", type="error")
      return()
    }

    tryCatch({
      des <- FielDHub::row_column(t = nt, nrows = nr, ncols = nc, plotNumber = 101)
      rc_design(des)
      showNotification("Diseño generado con FielDHub.", type="message")

    }, error = function(e) {
      showNotification(paste("FielDHub error/no encontrado. Usando generador simple.", e$message), type="warning")

      trts <- paste0("T", 1:nt)
      n_cells <- nr * nc
      trts_full <- c(trts, rep("Blank", n_cells - nt))
      layout_vec <- sample(trts_full)
      book <- expand.grid(Row = 1:nr, Col = 1:nc)
      book$Trt <- layout_vec
      full_book <- data.frame()
      for (r in 1:reps) {
        b <- book
        b$Rep <- r
        b$Trt <- sample(trts_full)
        full_book <- rbind(full_book, b)
      }
      rc_design(list(fieldBook = full_book))
    })
  })

  output$plot_rc_map <- renderPlot({
    des <- rc_design(); shiny::req(des)
    fb <- des$fieldBook
    names(fb) <- toupper(names(fb))
    c_row <- grep("ROW", names(fb), value=TRUE)[1]
    c_col <- grep("COL", names(fb), value=TRUE)[1]
    c_trt <- grep("TRT|TREAT|ENTRY", names(fb), value=TRUE)[1]
    shiny::req(c_row, c_col, c_trt)

    library(ggplot2)
    ggplot(fb, aes_string(x = c_col, y = c_row, fill = c_trt, label = c_trt)) +
      geom_tile(color = "white") +
      geom_text(size = 3) +
      scale_y_reverse() +
      facet_wrap(~REP) +
      theme_minimal() +
      guides(fill = "none") +
      labs(title = "Mapa del Diseño (Por Repetición)")
  })

  output$tbl_rc_book <- DT::renderDataTable({
    des <- rc_design(); shiny::req(des)
    DT::datatable(des$fieldBook, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$ui_dl_rc <- renderUI({
    shiny::req(rc_design())
    downloadButton(ns("dl_rc_csv"), "Descargar Fieldbook", class = "btn-secondary w-100")
  })

  output$dl_rc_csv <- downloadHandler(
    filename = function() { "row_col_design.csv" },
    content = function(file) {
      write.csv(rc_design()$fieldBook, file, row.names = FALSE)
    }
  )
}

pestanna3_session7_v3_server <- function(input, output, session, rc_design, sim_data) {
  output$plot_sim_spatial <- renderPlot({
    sim <- sim_data(); shiny::req(sim)
    df <- sim$data
    c_row <- sim$cols$row
    c_col <- sim$cols$col

    ggplot2::ggplot(df, ggplot2::aes_string(x = c_col, y = c_row, fill = "Y")) +
      ggplot2::geom_tile() +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(title = "Respuesta Simulada (Y) en Campo") +
      ggplot2::theme_minimal()
  })

  output$out_rcbd <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df <- sim$data
    c_trt <- sim$cols$trt
    if(!"REP" %in% names(df)) df$REP <- "1"

    fmla <- as.formula(paste("Y ~", c_trt, "+ (1|REP)"))
    if (length(unique(df$REP)) < 2) {
      cat("Solo 1 repetición: Ajustando CRD (Y ~ Trt)\n")
      m <- lm(as.formula(paste("Y ~", c_trt)), data=df)
      print(anova(m))
    } else {
      library(lmerTest)
      m <- lmer(fmla, data=df)
      print(anova(m))
    }
  })

  output$out_rclmm <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df <- sim$data
    c_trt <- sim$cols$trt
    c_row <- sim$cols$row
    c_col <- sim$cols$col
    if(!"REP" %in% names(df)) df$REP <- "1"

    library(lmerTest)
    df[[c_row]] <- factor(df[[c_row]])
    df[[c_col]] <- factor(df[[c_col]])

    if (length(unique(df$REP)) < 2) {
      fmla <- as.formula(paste("Y ~", c_trt, "+ (1|", c_row, ") + (1|", c_col, ")"))
    } else {
      fmla <- as.formula(paste("Y ~", c_trt, "+ (1|REP) + (1|REP:", c_row, ") + (1|REP:", c_col, ")"))
    }

    m <- lmer(fmla, data=df)
    print(anova(m))
    cat("\n--- Componentes de Varianza ---\n")
    print(lme4::VarCorr(m))
  })

  output$out_vc <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df <- sim$data
    c_trt <- sim$cols$trt
    c_row <- sim$cols$row
    c_col <- sim$cols$col
    if(!"REP" %in% names(df)) df$REP <- "1"
    df[[c_row]] <- factor(df[[c_row]])
    df[[c_col]] <- factor(df[[c_col]])

    if (length(unique(df$REP)) < 2) {
      fmla <- as.formula(paste("Y ~", c_trt, "+ (1|", c_row, ") + (1|", c_col, ")"))
    } else {
      fmla <- as.formula(paste("Y ~", c_trt, "+ (1|REP) + (1|REP:", c_row, ") + (1|REP:", c_col, ")"))
    }
    m <- lmerTest::lmer(fmla, data=df)
    print(summary(m)$varcor)
  })
}

pestanna4_session7_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna5_session7_v3_server <- function(input, output, session) {
  # No server logic needed
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session7_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  has_pkg <- function(p) requireNamespace(p, quietly=TRUE)

  # Reactives
  rc_design <- reactiveVal(NULL)

  sim_data <- eventReactive(input$btn_sim_run, {
    des <- rc_design()
    if (is.null(des)) {
      nr=4; nc=5; nt=20
      book <- expand.grid(Row=1:nr, Col=1:nc)
      book$Trt <- paste0("T", sample(1:nt))
      book$Rep <- 1
      fb <- book
    } else {
      fb <- des$fieldBook
    }

    names(fb) <- toupper(names(fb))
    c_row <- grep("ROW", names(fb), value=TRUE)[1]
    c_col <- grep("COL", names(fb), value=TRUE)[1]
    c_trt <- grep("TRT|TREAT|ENTRY", names(fb), value=TRUE)[1]

    mu <- input$sim_mu
    s_g <- input$sim_sigma_g
    s_r <- input$sim_sigma_row
    s_c <- input$sim_sigma_col
    s_e <- input$sim_sigma_e

    trts <- unique(fb[[c_trt]])
    eff_t <- rnorm(length(trts), 0, s_g); names(eff_t) <- trts

    rows <- unique(fb[[c_row]])
    eff_r <- rnorm(length(rows), 0, s_r); names(eff_r) <- rows

    cols <- unique(fb[[c_col]])
    eff_c <- rnorm(length(cols), 0, s_c); names(eff_c) <- cols

    fb$Y <- NA
    for(i in 1:nrow(fb)) {
      t_val <- as.character(fb[[c_trt]][i])
      r_val <- as.character(fb[[c_row]][i])
      c_val <- as.character(fb[[c_col]][i])

      et <- if(t_val %in% names(eff_t)) eff_t[t_val] else 0

      fb$Y[i] <- mu + et + eff_r[r_val] + eff_c[c_val] + rnorm(1, 0, s_e)
    }

    list(data = fb, cols = list(row=c_row, col=c_col, trt=c_trt))
  })

  # Call tab servers
  pestanna1_session7_v3_server(input, output, session)
  pestanna2_session7_v3_server(input, output, session, rc_design, has_pkg)
  pestanna3_session7_v3_server(input, output, session, rc_design, sim_data)
  pestanna4_session7_v3_server(input, output, session)
  pestanna5_session7_v3_server(input, output, session)
}
