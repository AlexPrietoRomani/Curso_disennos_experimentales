# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session8.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Conceptos
pestanna1_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Conceptos",
    h4(class = "section-header", "¿Qué es un Diseño de Bloques Aumentados (DBA)?"),
    div(class = "alert alert-info",
        p(
          strong("Contexto:"), " Tienes muchos tratamientos nuevos (genotipos) con ",
          em("poca semilla"), " (solo alcanza para 1 repetición) y unos pocos ",
          strong("testigos (checks)"), " con suficiente semilla para repetirse."
        )
    ),
    fluidRow(
      column(
        width = 6,
        h5("Estructura"),
        tags$ul(
          tags$li(strong("Testigos (Checks):"), " Se repiten en todos (o casi todos) los bloques. Permiten estimar el error y el efecto de bloque."),
          tags$li(strong("Nuevos (Entries):"), " Aparecen solo una vez en todo el ensayo. Se 'ajustan' usando la información de los testigos.")
        ),
        h5("Análisis"),
        p("Históricamente se usaba un ajuste intra-bloque manual. Hoy, el estándar es ",
          strong("LMM (Modelos Mixtos)"), " donde los bloques son aleatorios y los tratamientos pueden ser fijos o aleatorios (BLUPs para selección).")
      ),
      column(
        width = 6,
        h5("Ventajas"),
        tags$ul(
          tags$li("Permite evaluar cientos de líneas en etapas tempranas."),
          tags$li("Uso eficiente del espacio y semilla."),
          tags$li("Flexible en tamaño de bloque.")
        ),
        div(class = "note-cloud",
            p("Paquete clave: ", code("agricolae"), " (función design.dau) o ", code("FielDHub"), ".")
        )
      )
    )
  )
}

# Pestaña 2: Generar Diseño
pestanna2_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Generar Diseño",
    p("Genera un DBA usando ", code("agricolae::design.dau"), "."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        textInput(ns("checks_txt"), "Testigos (separados por coma):", value = "CheckA, CheckB, CheckC"),
        numericInput(ns("n_new"), "Número de Nuevos Tratamientos:", value = 90, min = 10),
        numericInput(ns("n_blocks"), "Número de Bloques:", value = 5, min = 2),
        actionButton(ns("btn_gen_dba"), "Generar DBA", class = "btn btn-primary w-100"),
        hr(),
        uiOutput(ns("ui_dl_dba"))
      ),
      mainPanel(
        width = 8,
        tags$h5("Resumen del Diseño"),
        verbatimTextOutput(ns("out_dba_summary")),
        tags$h5("Fieldbook (primeras filas)"),
        DT::dataTableOutput(ns("tbl_dba_book")),
        hr(),
        tags$h5("Distribución en Bloques"),
        plotOutput(ns("plot_dba_map"), height = "350px")
      )
    )
  )
}

# Pestaña 3: Análisis (Simulación)
pestanna3_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Análisis (Simulación)",
    p("Simula datos para un DBA y compáralo con un análisis naive."),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros"),
        numericInput(ns("sim_mu"), "Media General:", 100),
        numericInput(ns("sim_sigma_g"), "SD Genotipos (Nuevos):", 10),
        numericInput(ns("sim_sigma_b"), "SD Bloques:", 15),
        numericInput(ns("sim_sigma_e"), "SD Residual:", 5),
        actionButton(ns("btn_sim_dba"), "Simular y Analizar", class = "btn btn-success w-100")
      ),
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          bslib::nav_panel("Análisis Clásico (augmentedRCBD)",
                    p("Usa el paquete ", code("augmentedRCBD"), " (si disponible) o ANOVA ajustado."),
                    verbatimTextOutput(ns("out_aug_classic"))
          ),
          bslib::nav_panel("Análisis LMM (lme4)",
                    p("Modelo: Y ~ Tipo + (1|Bloque) + (1|Genotipo:Tipo) ... o similar."),
                    tags$div(class="alert alert-secondary",
                             "Aquí asumimos Genotipos como Aleatorios (BLUPs) para ranking."),
                    verbatimTextOutput(ns("out_aug_lmm")),
                    plotOutput(ns("plot_blups"), height = "300px")
          )
        )
      )
    )
  )
}

# Pestaña 4: Ejercicios
pestanna4_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Ejercicios",
    tags$ol(
      tags$li("Diseña un ensayo para 200 líneas nuevas y 4 testigos en 10 bloques."),
      tags$li("¿Cuántas parcelas totales necesitas? (Verifica en la pestaña 2)."),
      tags$li("Simula datos con alta variabilidad de bloque (SD=20)."),
      tags$li("Corre el análisis LMM y extrae los BLUPs de los genotipos nuevos."),
      tags$li("Identifica el 'Top 5' de genotipos superiores a los testigos.")
    )
  )
}

# Pestaña 5: Referencias
pestanna5_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("Federer, W.T. (1956). Augmented (or hoonuiaku) designs."),
      tags$li("agricolae: Statistical Procedures for Agricultural Research."),
      tags$li("augmentedRCBD: Analysis of Augmented Randomised Complete Block Designs. ",
              tags$a(href="https://cran.r-project.org/package=augmentedRCBD", target="_blank", "CRAN Link"))
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session8_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 8: Diseños de Bloques Aumentados (DBA)")
    ),
    navset_tab(
      pestanna1_session8_v3UI(ns),
      pestanna2_session8_v3UI(ns),
      pestanna3_session8_v3UI(ns),
      pestanna4_session8_v3UI(ns),
      pestanna5_session8_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

pestanna1_session8_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna2_session8_v3_server <- function(input, output, session, dba_design, has_pkg) {
  ns <- session$ns
  
  observeEvent(input$btn_gen_dba, {
    if (!has_pkg("agricolae")) {
      showNotification("Instale 'agricolae' para usar esta función.", type="error")
      return()
    }

    checks <- strsplit(input$checks_txt, "\\s*,\\s*")[[1]]
    checks <- checks[checks != ""]
    n_new <- input$n_new
    n_blocks <- input$n_blocks

    if (length(checks) < 1) {
      showNotification("Ingrese al menos 1 testigo.", type="error")
      return()
    }

    new_trts <- paste0("New", 1:n_new)
    trts <- c(checks, new_trts)

    tryCatch({
      des <- agricolae::design.dau(trt1 = checks, trt2 = new_trts, r = n_blocks, serie = 2, seed = 123)
      dba_design(des)
      showNotification("Diseño DBA generado.", type="message")
    }, error = function(e) {
      showNotification(paste("Error generando diseño:", e$message), type="error")
    })
  })

  output$out_dba_summary <- renderPrint({
    des <- dba_design(); shiny::req(des)
    cat("Parámetros del diseño:\n")
    print(des$parameters)
    cat("\nDimensiones del Fieldbook:\n")
    print(dim(des$book))
  })

  output$tbl_dba_book <- DT::renderDataTable({
    des <- dba_design(); shiny::req(des)
    DT::datatable(des$book, options = list(pageLength = 5, scrollX = TRUE))
  })

  output$plot_dba_map <- renderPlot({
    des <- dba_design(); shiny::req(des)
    fb <- des$book
    library(ggplot2)
    fb <- fb %>% dplyr::group_by(block) %>% dplyr::mutate(pos = 1:dplyr::n())

    checks <- des$parameters$trt1
    fb$Type <- ifelse(fb$trt %in% checks, "Check", "New")

    ggplot(fb, aes(x = factor(block), y = pos, fill = Type, label = trt)) +
      geom_tile(color = "white") +
      geom_text(size = 3) +
      scale_fill_manual(values = c("Check" = "#ff7f0e", "New" = "#1f77b4")) +
      labs(x = "Bloque", y = "Posición en Bloque", title = "Mapa del Diseño DBA") +
      theme_minimal()
  })

  output$ui_dl_dba <- renderUI({
    shiny::req(dba_design())
    downloadButton(ns("dl_dba_csv"), "Descargar Fieldbook", class = "btn-secondary w-100")
  })

  output$dl_dba_csv <- downloadHandler(
    filename = function() { "dba_design.csv" },
    content = function(file) {
      write.csv(dba_design()$book, file, row.names = FALSE)
    }
  )
}

pestanna3_session8_v3_server <- function(input, output, session, dba_design, sim_dba_data, has_pkg) {
  output$out_aug_classic <- renderPrint({
    df <- sim_dba_data(); shiny::req(df)
    if (has_pkg("augmentedRCBD")) {
      tbl <- table(df$trt)
      checks <- names(tbl)[tbl > 1]
      cat("Checks detectados:", paste(checks, collapse=", "), "\n\n")

      res <- augmentedRCBD::augmentedRCBD(df$block, df$trt, df$Y, method.comp = "lsd", alpha=0.05, group=FALSE, console=TRUE)
      print(res$ANOVA)
    } else {
      cat("Paquete 'augmentedRCBD' no instalado. Mostrando ANOVA simple (lm).\n")
      m <- lm(Y ~ factor(block) + factor(trt), data=df)
      print(anova(m))
    }
  })

  output$out_aug_lmm <- renderPrint({
    df <- sim_dba_data(); shiny::req(df)
    library(lmerTest)
    m <- lmer(Y ~ (1|block) + (1|trt), data=df)
    print(summary(m))

    session$userData$dba_lmm <- m
  })

  output$plot_blups <- renderPlot({
    m <- session$userData$dba_lmm; shiny::req(m)
    ran <- lme4::ranef(m)$trt
    ran$Trt <- rownames(ran)
    names(ran)[1] <- "BLUP"

    top <- ran %>% dplyr::arrange(desc(BLUP)) %>% head(20)

    ggplot2::ggplot(top, ggplot2::aes(x = reorder(Trt, BLUP), y = BLUP)) +
      ggplot2::geom_col(fill = "steelblue") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Genotipo", y = "BLUP (Efecto Estimado)", title = "Top 20 Genotipos (BLUPs)") +
      ggplot2::theme_minimal()
  })
}

pestanna4_session8_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna5_session8_v3_server <- function(input, output, session) {
  # No server logic needed
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session8_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  has_pkg <- function(p) requireNamespace(p, quietly=TRUE)

  # Reactives
  dba_design <- reactiveVal(NULL)

  sim_dba_data <- eventReactive(input$btn_sim_dba, {
    des <- dba_design()
    if (is.null(des)) {
      checks <- c("C1", "C2")
      new_trts <- paste0("N", 1:20)
      des <- agricolae::design.dau(checks, new_trts, r=4, serie=2, seed=123)
    }
    fb <- des$book

    mu <- input$sim_mu
    s_g <- input$sim_sigma_g
    s_b <- input$sim_sigma_b
    s_e <- input$sim_sigma_e

    trts <- unique(fb$trt)
    eff_t <- rnorm(length(trts), 0, s_g); names(eff_t) <- trts

    blks <- unique(fb$block)
    eff_b <- rnorm(length(blks), 0, s_b); names(eff_b) <- blks

    fb$Y <- NA
    for(i in 1:nrow(fb)) {
      t_val <- as.character(fb$trt[i])
      b_val <- as.character(fb$block[i])
      fb$Y[i] <- mu + eff_t[t_val] + eff_b[b_val] + rnorm(1, 0, s_e)
    }
    fb
  })

  # Call tab servers
  pestanna1_session8_v3_server(input, output, session)
  pestanna2_session8_v3_server(input, output, session, dba_design, has_pkg)
  pestanna3_session8_v3_server(input, output, session, dba_design, sim_dba_data, has_pkg)
  pestanna4_session8_v3_server(input, output, session)
  pestanna5_session8_v3_server(input, output, session)
}
