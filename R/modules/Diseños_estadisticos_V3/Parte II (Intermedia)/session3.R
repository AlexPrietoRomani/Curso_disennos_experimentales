# R/modules/Diseños_estadisticos_V3/Parte II (Intermedia)/session3.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: MRLS & Datos
pestanna1_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) MRLS & Datos",
    tags$div(class = "mb-3",
      tags$p(
        "Objetivo: Ajustar e interpretar el Modelo de Regresión Lineal Simple (MRLS), ",
        "construir un dataset práctico (simular o cargar CSV) y fijar la línea base (β₀, β₁, R², p-valor). ",
        "Seguiremos buenas prácticas de inferencia y diagnóstico antes de concluir. ",
        "Fundamentos en MRLS e inferencia (Gelman & Hill, 2007)."
      )
    ),

    bslib::card(
      bslib::card_header("Datos de práctica"),
      fluidRow(
        column(
          width = 4,
          radioButtons(ns("s3_data_mode"), "Fuente de datos:",
                       choices = c("Simular" = "sim", "Cargar CSV" = "csv"),
                       selected = "sim"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sim'", ns("s3_data_mode")),
            sliderInput(ns("s3_n"),
                        "n (observaciones):", min = 20, max = 500, value = 100, step = 10),
            sliderInput(ns("s3_beta0"), "β₀ (intercepto):", min = -200, max = 200, value = 500, step = 5),
            sliderInput(ns("s3_beta1"), "β₁ (pendiente):", min = -10, max = 10, value = 2, step = 0.1),
            sliderInput(ns("s3_sigma"), "σ (ruido):", min = 1, max = 100, value = 40, step = 1),
            checkboxInput(ns("s3_quadratic_truth"),
                          "Generar verdad con curvatura suave (parabólica)", value = TRUE),
            sliderInput(ns("s3_beta2_truth"), "β₂ (curvatura 'verdad'):",
                        min = -0.05, max = 0.05, value = -0.01, step = 0.001),
            actionButton(ns("s3_gen"), "Generar datos", icon = icon("random"),
                         class = "btn-primary w-100 mt-2")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'csv'", ns("s3_data_mode")),
            fileInput(ns("s3_file"), "Sube un CSV con columnas 'x' y 'y'",
                      accept = c(".csv", "text/csv")),
            checkboxInput(ns("s3_has_header"), "CSV con cabecera", TRUE),
            selectInput(ns("s3_sep"), "Separador", choices = c("," = ",", ";" = ";", "\t" = "\t"), selected = ","),
            checkboxInput(ns("s3_show_preview"), "Previsualizar primeras filas", TRUE)
          )
        ),
        column(
          width = 8,
          bslib::card(
            bslib::card_header("Exploración"),
            DT::dataTableOutput(ns("s3_tbl_preview")),
            plotOutput(ns("s3_scatter_base"), height = "330px")
          )
        )
      )
    ),

    bslib::card(
      bslib::card_header("MRLS: Ajuste e interpretación"),
      fluidRow(
        column(
          width = 6,
          tags$p(
            "Ajustamos \\( y = \\beta_0 + \\beta_1 x + \\varepsilon \\) por Mínimos Cuadrados. ",
            "Interpretación estándar de β₀ (cuando x=0) y β₁ (cambio medio en y por unidad de x). ",
            "R² y p-valores guían evidencia de asociación; revisar supuestos antes de concluir ",
            "(Gelman & Hill, 2007)."
          ),
          verbatimTextOutput(ns("s3_lm_summary"))
        ),
        column(
          width = 6,
          plotOutput(ns("s3_fit_plot"), height = "330px"),
          tags$small(
            "Sombreado: IC (95%) del MRLS. Visualización con ", code("ggplot2"), 
            " para evaluar tendencia inicial. ", 
            "Documentación de ", code("geom_smooth()"), " y fórmulas personalizadas. "
          )
        )
      )
    )
  )
}

# Pestaña 2: Diagnóstico
pestanna2_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Diagnóstico",
    tags$p(
      "Las inferencias (p-valores/IC) suponen residuos aproximadamente normales, independencia y varianza constante. ",
      "Se inspeccionan residuos vs. ajustados (homocedasticidad) y Q-Q Normal (normalidad). ",
      "Para heterocedasticidad por grupos, Levene es útil (Fox & Weisberg)."
    ),

    fluidRow(
      column(
        width = 4,
        bslib::card(
          bslib::card_header("Configuración de diagnóstico"),
          selectInput(ns("s3_diag_model"), "Modelo para diagnosticar:",
                      choices = c("Lineal (y ~ x)" = "lin")),
          sliderInput(ns("s3_bins_groups"),
                      "Agrupar x en k quantiles (para Levene):", min = 2, max = 8, value = 4),
          actionButton(ns("s3_run_diag"), "Ejecutar diagnóstico", class = "btn-secondary w-100")
        ),
        bslib::card(
          bslib::card_header("Prueba formal (opcional)"),
          tags$p(
            "Si está disponible el paquete ", code("car"), ", se ejecuta ",
            code("leveneTest"), "."
          ),
          verbatimTextOutput(ns("s3_levene_out"))
        )
      ),
      column(
        width = 8,
        bslib::card(
          bslib::card_header("Gráficos de diagnóstico"),
          fluidRow(
            column(6, plotOutput(ns("s3_resid_fitted"), height = "280px")),
            column(6, plotOutput(ns("s3_qqplot"), height = "280px"))
          ),
          tags$small(
            "Criterio visual: Nube aleatoria alrededor de 0 (varianza constante); ",
            "Q-Q cercano a la línea (residuos ~ normales). ",
            "Ante heterocedasticidad: considerar transformaciones o ponderación; interpretar como hallazgo ",
            "biológico (variabilidad que crece con el nivel del insumo) antes de 'corregir'."
          )
        )
      )
    )
  )
}

# Pestaña 3: No lineales & Selección
pestanna3_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) No lineales & Selección",
    tags$p(
      "Muchas relaciones agrobiológicas son no lineales (rendimientos decrecientes, óptimos). ",
      "Comparamos modelos lineal, cuadrático y logarítmico con el mismo subconjunto de datos ",
      "(si hay x≤0, se filtran para el log). Selección con AIC (parsimonia) y R² ajustado."
    ),

    bslib::card(
      bslib::card_header("Ajuste paralelo y comparación"),
      fluidRow(
        column(
          width = 4,
          checkboxGroupInput(
            ns("s3_models_to_fit"),
            "Modelos a considerar",
            choices = c(
              "Lineal: y ~ x" = "lin",
              "Cuadrático: y ~ x + I(x^2)" = "quad",
              "Logarítmico: y ~ log(x)" = "log"
            ),
            selected = c("lin", "quad", "log")
          ),
          checkboxInput(ns("s3_lock_same_data"),
                        "Forzar misma base de datos para todos (usa x>0 si incluye log)",
                        value = TRUE),
          actionButton(ns("s3_run_compare"), "Ajustar y comparar", class = "btn-primary w-100")
        ),
        column(
          width = 8,
          DT::dataTableOutput(ns("s3_tbl_models")),
          tags$small(
            "AIC más bajo ⇒ modelo más parsimonioso (Akaike, 1974; Burnham & Anderson, 2004). ",
            "R²aj penaliza predictores innecesarios; útil pero centrado en ajuste retrospectivo. ",
            "Elegir con criterio biológico cuando AIC sea cercano."
          )
        )
      )
    ),

    bslib::card(
      bslib::card_header("Comparación visual de curvas"),
      plotOutput(ns("s3_compare_plot"), height = "360px"),
      tags$small(
        "Curvas superpuestas con ", code("ggplot2::geom_smooth"), 
        ". Si el logarítmico está seleccionado, se traza sobre x>0."
      )
    ),

    bslib::card(
      bslib::card_header("Notas de interpretabilidad"),
      tags$ul(
        tags$li("Cuadrático: permite óptimo \\( x^* = -\\beta_1 / (2\\beta_2) \\); útil en densidad o dosis con saturación/caída (Yahuza, 2011)."),
        tags$li("Logarítmico: rendimientos decrecientes y asintóticos; precursor de Gompertz/logístico usados en crecimiento (Besteiro et al., 2023)."),
        tags$li("Documentación AIC en R: ", code("stats::AIC"), " y ", code("extractAIC"), ".")
      )
    )
  )
}

# Pestaña 4: Ejercicios prácticos
pestanna4_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Ejercicios prácticos",
    bslib::card(
      bslib::card_header("Checklist de tareas"),
      tags$ol(
        tags$li("Carga o simula un conjunto de datos con columnas ", code("x"), " (insumo) y ", code("y"), " (respuesta)."),
        tags$li("Ajusta MRLS y verifica supuestos (residuos vs. ajustados; Q-Q)."),
        tags$li("Ajusta modelos cuadrático y logarítmico."),
        tags$li("Compara R²aj y AIC; justifica selección con criterio biológico."),
        tags$li("Grafica las curvas elegidas sobre los datos."),
        tags$li("Reporta β, IC95%, R²aj, AIC y una conclusión agronómica.")
      )
    ),

    bslib::card(
      bslib::card_header("Panel de trabajo"),
      fluidRow(
        column(
          width = 4,
          selectInput(ns("s3_ex_model"),
                      "Modelo focal para reporte:",
                      choices = c("Lineal" = "lin", "Cuadrático" = "quad", "Logarítmico" = "log"),
                      selected = "quad"),
          checkboxInput(ns("s3_ex_use_weights"),
                        "Usar ponderación 1/ŷ (demo de WLS)*", value = FALSE),
          tags$small(
            "* Demo sencilla de ponderación tipo varianza ∝ ŷ². Use con cautela; base conceptual fuera del alcance. ",
            "Revise diagnóstico antes."
          ),
          downloadButton(ns("s3_dl_report"), "Descargar resumen (.txt)",
                         class = "btn-success w-100 mt-2")
        ),
        column(
          width = 8,
          verbatimTextOutput(ns("s3_explain_model")),
          plotOutput(ns("s3_ex_plot"), height = "320px")
        )
      )
    )
  )
}

# Pestaña 5: Referencias
pestanna5_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("Akaike, H. (1974). A new look at the statistical model identification. ", tags$em("IEEE Transactions on Automatic Control"), ". ",
              tags$a(href="https://eclass.uoa.gr/modules/document/file.php/MATH452/%CE%86%CF%81%CE%B8%CF%81%CE%B1/Akaike_1974.pdf", "PDF"), " ", tags$span(style="opacity:.6;","(AIC)")),
      tags$li("Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: understanding AIC and BIC in model selection. ",
              tags$em("Sociological Methods & Research"), ". ",
              tags$a(href="https://sites.warnercnr.colostate.edu/wp-content/uploads/sites/73/2017/05/Burnham-and-Anderson-2004-SMR.pdf", "PDF")),
      tags$li("Burnham, K. P., & Anderson, D. R. (2002). ", tags$em("Model Selection and Multimodel Inference (2nd ed.)"), ". ",
              tags$a(href="https://link.springer.com/content/pdf/10.1007/b97636.pdf", "Springer PDF (preview)")),
      tags$li("Gelman, A., & Hill, J. (2007). ", tags$em("Data Analysis Using Regression and Multilevel/Hierarchical Models"), ". ",
              tags$a(href="https://api.pageplace.de/preview/DT0400.9780511266836_A23690811/preview-9780511266836_A23690811.pdf", "Preview PDF")),
      tags$li("Fox, J., & Weisberg, S. (2019/2024). ", tags$em("An R Companion to Applied Regression"), " & manual del paquete ", code("car"), ". ",
              tags$a(href="https://cran.r-project.org/package=car/car.pdf", "Manual (CRAN)")),
      tags$li("Wickham, H. (2016/2023). ", tags$em("ggplot2: Elegant Graphics for Data Analysis"), ". ",
              tags$a(href="https://ggplot2-book.org/", "Libro online"), " | ",
              tags$a(href="https://ggplot2.tidyverse.org/reference/geom_smooth.html", "geom_smooth()")),
      tags$li("Yahuza, I. (2011). Yield-density equations and their application for agronomic research: a review. ",
              tags$a(href="https://www.cabidigitallibrary.org/doi/pdf/10.5555/20113335598", "PDF CABI")),
      tags$li("Besteiro, R., et al. (2023/2024). Linear and Nonlinear Mixed Models to Determine the Growth Curves of Weaned Piglets. ",
              tags$a(href="https://www.mdpi.com/2077-0472/14/1/79", "MDPI link")),
      tags$li("R base docs: ", code("stats::AIC"), " y ", code("stats::extractAIC"), ". ",
              tags$a(href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC", "AIC"), " | ",
              tags$a(href="https://stat.ethz.ch/R-manual//R-patched/library/stats/help/extractAIC.html", "extractAIC"))
    ),
    tags$hr(),
    tags$small("Estas referencias sustentan la selección por AIC/parsimonia, el uso de MRLS y el diagnóstico estándar en regresión; se recomiendan para profundizar.")
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session3_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(class = "session-title", "Sesión 3: Análisis de Regresión (Lineal y No Lineal)"),
    withMathJax(),
    bslib::navset_tab(
      pestanna1_session3_v3UI(ns),
      pestanna2_session3_v3UI(ns),
      pestanna3_session3_v3UI(ns),
      pestanna4_session3_v3UI(ns),
      pestanna5_session3_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

pestanna1_session3_v3_server <- function(input, output, session, df_rv, make_sim_data, read_csv_xy, s3_model_lin) {
  observeEvent(input$s3_gen, {
    df_rv(
      make_sim_data(
        n     = input$s3_n,
        b0    = input$s3_beta0,
        b1    = input$s3_beta1,
        sigma = input$s3_sigma,
        quad  = input$s3_quadratic_truth,
        b2    = input$s3_beta2_truth
      )
    )
  })

  observeEvent(input$s3_file, {
    req(input$s3_data_mode == "csv")
    f <- input$s3_file$datapath
    tryCatch({
      df_rv(read_csv_xy(f, header = isTRUE(input$s3_has_header), sep = input$s3_sep))
    }, error = function(e) {
      showModal(modalDialog(title = "Error al leer CSV", paste(e), easyClose = TRUE))
    })
  })

  output$s3_tbl_preview <- DT::renderDataTable({
    df <- df_rv(); req(nrow(df) > 1)
    if (isTRUE(input$s3_show_preview)) {
      DT::datatable(head(df, 10), rownames = FALSE,
                    options = list(dom = "tip", pageLength = 10))
    } else {
      DT::datatable(df, rownames = FALSE,
                    options = list(pageLength = 10))
    }
  })

  output$s3_scatter_base <- renderPlot({
    df <- df_rv(); req(nrow(df) > 1)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.65, size = 2.8) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Datos de práctica (x ~ insumo; y ~ respuesta)",
                    x = "x (insumo)", y = "y (respuesta)")
  })

  output$s3_lm_summary <- renderPrint({
    m <- s3_model_lin(); summary(m)
  })

  output$s3_fit_plot <- renderPlot({
    df <- df_rv(); req(nrow(df) > 1)
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "MRLS: y ~ x (con IC95%)", x = "x", y = "y")
  })
}

pestanna2_session3_v3_server <- function(input, output, session, df_rv, s3_model_lin) {
  observeEvent(input$s3_run_diag, {
    invisible(TRUE)
  })

  output$s3_resid_fitted <- renderPlot({
    req(input$s3_run_diag)
    m <- s3_model_lin()
    df_plot <- tibble::tibble(fitted = stats::fitted(m), resid = stats::residuals(m))
    ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Residuos vs. Ajustados", x = "Valores ajustados", y = "Residuos")
  })

  output$s3_qqplot <- renderPlot({
    req(input$s3_run_diag)
    m <- s3_model_lin()
    df <- tibble::tibble(resid = stats::residuals(m))
    ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq() + ggplot2::stat_qq_line(color = "red") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Q-Q Normal de Residuos")
  })

  output$s3_levene_out <- renderPrint({
    req(input$s3_run_diag)
    df <- df_rv(); req(nrow(df) > 3)
    k <- input$s3_bins_groups
    bins <- cut(df$x, breaks = stats::quantile(df$x, probs = seq(0, 1, length.out = k + 1),
                                               na.rm = TRUE), include.lowest = TRUE)
    m <- s3_model_lin()
    resid <- stats::residuals(m)
    tmp <- tibble::tibble(resid = resid, bin = bins)
    if (requireNamespace("car", quietly = TRUE)) {
      car::leveneTest(resid ~ bin, data = tmp, center = median)
    } else {
      cat("Paquete 'car' no disponible. Instale 'car' para ejecutar Levene:\n",
          "install.packages('car')\n")
    }
  })
}

pestanna3_session3_v3_server <- function(input, output, session, df_rv) {
  safe_lm <- function(formula, data) {
    tryCatch(stats::lm(formula, data = data), error = function(e) NULL)
  }

  s3_fit_models <- eventReactive(input$s3_run_compare, {
    df <- df_rv(); req(nrow(df) > 3)
    want <- input$s3_models_to_fit
    if (length(want) == 0) {
      showModal(modalDialog(title = "Sin modelos", "Seleccione al menos un modelo.", easyClose = TRUE))
      return(NULL)
    }

    if (isTRUE(input$s3_lock_same_data) && "log" %in% want) {
      df_use <- dplyr::filter(df, is.finite(x), x > 0)
    } else {
      df_use <- df
    }
    if (nrow(df_use) < 5) {
      showModal(modalDialog(title = "Datos insuficientes",
                            "Menos de 5 observaciones tras filtrado (x>0).",
                            easyClose = TRUE))
      return(NULL)
    }

    fits <- list()
    if ("lin" %in% want)  fits$lin  <- safe_lm(y ~ x, data = df_use)
    if ("quad" %in% want) fits$quad <- safe_lm(y ~ x + I(x^2), data = df_use)
    if ("log" %in% want)  fits$log  <- safe_lm(y ~ log(x), data = dplyr::filter(df_use, x > 0))

    res <- purrr::imap_dfr(fits, function(mod, name) {
      if (is.null(mod)) return(NULL)
      sm <- summary(mod)
      tibble::tibble(
        Modelo     = name,
        `R2_aj`    = unname(sm$adj.r.squared),
        AIC        = tryCatch(stats::AIC(mod), error = function(e) NA_real_),
        n          = stats::nobs(mod),
        `β0`       = unname(stats::coef(mod)[1]),
        `β1`       = unname(stats::coef(mod)[2]),
        `β2`       = ifelse(name == "quad" && length(stats::coef(mod)) >= 3,
                            unname(stats::coef(mod)[3]), NA_real_)
      )
    })
    if (nrow(res)) {
      res <- dplyr::mutate(res, Modelo = dplyr::recode(Modelo,
                              lin = "Lineal (y~x)",
                              quad = "Cuadrático (y~x+I(x^2))",
                              log = "Logarítmico (y~log(x))"))
      res <- dplyr::arrange(res, AIC)
    }
    list(fits = fits, table = res, df_use = df_use)
  })

  output$s3_tbl_models <- DT::renderDataTable({
    fm <- s3_fit_models(); req(!is.null(fm), nrow(fm$table) > 0)
    best_row <- which.min(fm$table$AIC)
    out <- fm$table
    out$`← Mejor (AIC)` <- ""
    out$`← Mejor (AIC)`[best_row] <- "★"
    DT::datatable(out, rownames = FALSE, options = list(pageLength = 5))
  })

  output$s3_compare_plot <- renderPlot({
    fm <- s3_fit_models(); req(!is.null(fm), nrow(fm$table) > 0)
    df <- df_rv()
    want <- input$s3_models_to_fit
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Comparación visual de modelos", x = "x", y = "y")

    if ("lin" %in% want && !is.null(fm$fits$lin)) {
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x,
                                    color = "#1f77b4")
    }
    if ("quad" %in% want && !is.null(fm$fits$quad)) {
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ x + I(x^2),
                                    color = "#d62728")
    }
    if ("log" %in% want && !is.null(fm$fits$log)) {
      p <- p + ggplot2::geom_smooth(method = "lm", se = FALSE, formula = y ~ log(x),
                                    data = dplyr::filter(df, x > 0),
                                    color = "#2ca02c")
    }
    p
  })
}

pestanna4_session3_v3_server <- function(input, output, session, df_rv) {
  s3_fit_focal <- reactive({
    df <- df_rv(); req(nrow(df) > 3)
    exm <- input$s3_ex_model
    if (isTRUE(input$s3_ex_use_weights)) {
      m0 <- stats::lm(y ~ x, data = df)
      yhat <- pmax(fitted(m0), .Machine$double.eps)
      w <- 1 / (yhat^2)
      mod <- switch(
        exm,
        lin  = stats::lm(y ~ x, data = df, weights = w),
        quad = stats::lm(y ~ x + I(x^2), data = df, weights = w),
        log  = stats::lm(y ~ log(x), data = dplyr::filter(df, x > 0), weights = w[df$x > 0]),
        NULL
      )
    } else {
      mod <- switch(
        exm,
        lin  = stats::lm(y ~ x, data = df),
        quad = stats::lm(y ~ x + I(x^2), data = df),
        log  = stats::lm(y ~ log(x), data = dplyr::filter(df, x > 0), weights = NULL),
        NULL
      )
    }
    mod
  })

  output$s3_explain_model <- renderPrint({
    m <- s3_fit_focal(); req(!is.null(m))
    sm <- summary(m)
    aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)
    cat("== Resumen del modelo focal ==\n")
    print(sm$call); cat("\n")
    cat(sprintf("n = %d | R2 adj = %.4f | AIC = %.2f\n\n",
                stats::nobs(m), sm$adj.r.squared, aic))
    print(sm$coefficients)
    if (length(stats::coef(m)) >= 3 && grepl("I\\(x\\^2\\)", names(stats::coef(m))[3], fixed = TRUE)) {
      b1 <- unname(stats::coef(m)[2]); b2 <- unname(stats::coef(m)[3])
      if (!is.na(b2) && b2 != 0) {
        x_opt <- -b1 / (2*b2)
        cat(sprintf("\nÓptimo parabólico (vértice): x* = %.3f\n", x_opt))
      }
    }
  })

  output$s3_ex_plot <- renderPlot({
    df <- df_rv(); req(nrow(df) > 3)
    exm <- input$s3_ex_model
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Modelo focal sobre datos", x = "x", y = "y")
    if (exm == "lin") {
      p + ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = TRUE, color = "#1f77b4")
    } else if (exm == "quad") {
      p + ggplot2::geom_smooth(method = "lm", formula = y ~ x + I(x^2), se = TRUE, color = "#d62728")
    } else {
      p + ggplot2::geom_smooth(method = "lm", formula = y ~ log(x), se = TRUE,
                               data = dplyr::filter(df, x > 0), color = "#2ca02c")
    }
  })

  output$s3_dl_report <- downloadHandler(
    filename = function() {
      paste0("Sesion3_regresion_resumen_", Sys.Date(), ".txt")
    },
    content = function(file) {
      m <- s3_fit_focal(); req(!is.null(m))
      sm <- summary(m)
      aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)
      sink(file)
      cat("Sesión 3 – Resumen de modelo focal\n")
      cat("-----------------------------------\n\n")
      print(sm$call); cat("\n")
      cat(sprintf("n = %d\nR2 ajustado = %.4f\nAIC = %.2f\n\n",
                  stats::nobs(m), sm$adj.r.squared, aic))
      cat("Coeficientes:\n")
      print(sm$coefficients); cat("\n")
      if (length(stats::coef(m)) >= 3 && grepl("I\\(x\\^2\\)", names(stats::coef(m))[3], fixed = TRUE)) {
        b1 <- unname(stats::coef(m)[2]); b2 <- unname(stats::coef(m)[3])
        if (!is.na(b2) && b2 != 0) {
          x_opt <- -b1 / (2*b2)
          cat(sprintf("Óptimo (parabólico): x* = %.3f\n\n", x_opt))
        }
      }
      cat("Notas:\n")
      cat("- Selección de modelo por AIC (menor mejor; parsimonia). ",
          "Referencias: Akaike (1974); Burnham & Anderson (2004, 2002).\n")
      cat("- Verificar supuestos con diagnóstico gráfico (residuos vs ajustados; Q-Q). ",
          "Si hay heterocedasticidad marcada, considere transformaciones o modelos ponderados.\n")
      sink()
    }
  )
}

pestanna5_session3_v3_server <- function(input, output, session) {
  # No server logic needed
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session3_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  make_sim_data <- function(n, b0, b1, sigma, quad, b2) {
    set.seed(123)
    x <- sort(runif(n, min = 1, max = 200))
    mu <- b0 + b1 * x
    if (isTRUE(quad)) mu <- mu + b2 * x^2
    y <- mu + rnorm(n, 0, sigma)
    tibble::tibble(x = x, y = y)
  }

  read_csv_xy <- function(path, header = TRUE, sep = ",") {
    df <- utils::read.table(path, header = header, sep = sep, dec = ".", quote = "\"'",
                            comment.char = "", stringsAsFactors = FALSE)
    if (!all(c("x", "y") %in% names(df))) {
      stop("El CSV debe incluir columnas 'x' y 'y'.")
    }
    df <- df[, c("x", "y")]
    df$x <- suppressWarnings(as.numeric(df$x))
    df$y <- suppressWarnings(as.numeric(df$y))
    df <- stats::na.omit(df)
    tibble::as_tibble(df)
  }

  # Reactives
  df_rv <- reactiveVal({
    make_sim_data(
      n     = 100,
      b0    = 500,
      b1    = 2,
      sigma = 40,
      quad  = TRUE,
      b2    = -0.01
    )
  })

  s3_model_lin <- reactive({
    df <- df_rv(); req(nrow(df) > 1)
    stats::lm(y ~ x, data = df)
  })

  # Call tab servers
  pestanna1_session3_v3_server(input, output, session, df_rv, make_sim_data, read_csv_xy, s3_model_lin)
  pestanna2_session3_v3_server(input, output, session, df_rv, s3_model_lin)
  pestanna3_session3_v3_server(input, output, session, df_rv)
  pestanna4_session3_v3_server(input, output, session, df_rv)
  pestanna5_session3_v3_server(input, output, session)
}
