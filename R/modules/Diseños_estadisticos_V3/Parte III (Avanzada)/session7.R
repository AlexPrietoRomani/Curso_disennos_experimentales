# session7.R
# Sesión 7: Diseños de Filas y Columnas (Control Espacial Avanzado)
# Requiere: shiny, bslib, FielDHub, DT, dplyr, tidyr, purrr, lme4, lmerTest, broom.mixed, ggplot2

# ------------------------------
# UI
# ------------------------------
session7_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 7: Diseños de Filas y Columnas (Control Espacial Avanzado)")
    ),
    # Navegación principal
    navset_tab(
      # -------- Pestaña 1: Fundamento & objetivo ----------
      nav_panel(
        title = "Fundamento & objetivo",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("¿Cuándo usar Row–Column (R-C)?"),
            p("Cuando existen gradientes espaciales en dos direcciones (por ejemplo Norte–Sur y Este–Oeste) ",
              "que inflan el error si solo bloqueas en una sola dirección (RCBD). R-C implementa ",
              strong("bloqueo doble (fila y columna)"), " y el análisis debe reflejarlo con ",
              code("efectos aleatorios cruzados"), " en el LMM: ",
              code("~ genotipo + (1|fila) + (1|columna)"),
              "."
            ),
            tags$ul(
              tags$li("Generación del diseño optimizado (dos etapas) con ", code("FielDHub::row_column()"), "."),
              tags$li("FieldBook trae coordenadas ", code("ROW"), " y ", code("COLUMN"), ", ",
                      "número de parcela ", code("PLOT"), " y asignación de ", code("TREATMENT"), "."),
              tags$li("Visualización con ", code("plot(rcd)"), " y exportación del FieldBook.")
            ),
            p(em("Base teórica y funciones verificadas en la viñeta oficial y referencias de FielDHub."))
          ),
          card(
            card_header("Objetivo de aprendizaje"),
            tags$ol(
              tags$li("Generar un diseño R-C resoluble, ajustar un LMM con efectos cruzados y comparar precisión."),
              tags$li("Cuantificar la varianza espacial capturada por filas y columnas vía ", code("VarCorr()"), "."),
              tags$li("Explorar decisiones de diseño (nº filas, réplicas) y su impacto en la precisión.")
            ),
            p(
              "Recursos: viñeta ", code("Row-Column Design"),
              " (algoritmo de dos etapas, parámetros y estructura del field book), ",
              code("row_column()"), " y ", code("plot.FielDHub"), "."
            ),
            tags$details(
              summary("Notas de referencia"),
              HTML(paste0(
                "• FielDHub Row–Column vignette (optimización filas/columnas, iteraciones): ",
                tags$code("CRAN FielDHub – Row-Column Design"), "<br/>",
                "• Función ", code("row_column()"), " (argumentos, retorno con ", code("$fieldBook"), "): ",
                tags$code("rdrr.io – row_column"), "<br/>",
                "• Método ", code("plot.FielDHub"), " (dibuja el layout y requiere coords en field book): ",
                tags$code("rdrr.io – plot.FielDHub"), "<br/>",
                "• ", code("VarCorr()"), " en ", code("lme4"), " para extraer componentes de varianza."
              ))
            )
          )
        )
      ),

      # -------- Pestaña 2: Generar diseño (FielDHub) ----------
      nav_panel(
        title = "Generar diseño",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Parámetros del diseño R-C"),
            numericInput(ns("t"), "Nº de tratamientos (t):", value = 45, min = 3, step = 1),
            numericInput(ns("nrows"), "Filas por réplica (nrows):", value = 5, min = 2, step = 1),
            numericInput(ns("r"), "Nº de réplicas (r):", value = 3, min = 2, step = 1),
            numericInput(ns("l"), "Nº de localidades (l):", value = 1, min = 1, step = 1),
            textInput(ns("locname"), "Nombre de localidad:", value = "FARGO_2025"),
            numericInput(ns("plotStart"), "Plot inicial:", value = 101, min = 1, step = 1),
            numericInput(ns("iters"), "Iteraciones de optimización:", value = 1000, min = 100, step = 100),
            numericInput(ns("seed"), "Seed:", value = 1244, min = 1, step = 1),
            actionButton(ns("goDesign"), "Generar diseño", class = "btn-primary")
          ),
          card(
            card_header("Resumen de diseño"),
            verbatimTextOutput(ns("designSummary")),
            div(class = "mt-2",
                p("Este diseño está optimizado en dos etapas: primero columnas (bloques incompletos), ",
                  "luego filas (A-Efficiency) con permutas dentro de columnas; por defecto ~1000 iteraciones. ",
                  "Ver viñeta oficial Row–Column para detalles del algoritmo.")
            )
          )
        )
      ),

      # -------- Pestaña 3: Mapa & FieldBook ----------
      nav_panel(
        title = "Mapa & FieldBook",
        navs_tab(
          nav_panel(
            "Mapa",
            p("Visualiza el layout del campo generado. Puedes usar esta figura para validar distribución."),
            plotOutput(ns("fieldPlot"), height = "560px")
          ),
          nav_panel(
            "FieldBook",
            p("Libro de campo con coordenadas y tratamientos. Descárgalo para siembra y recojo de datos."),
            DT::DTOutput(ns("fieldTable")),
            downloadButton(ns("dlFieldBook"), "Descargar CSV")
          )
        )
      ),

      # -------- Pestaña 4: LMM & VarCorr ----------
      nav_panel(
        title = "LMM & VarCorr",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Datos de respuesta"),
            p("O bien sube el fieldbook con tu respuesta medida, o simula una respuesta basada en gradientes."),
            fileInput(ns("respFile"), "Cargar CSV con respuesta", accept = c(".csv")),
            checkboxInput(ns("simulateResp"), "Simular respuesta (si no cargas CSV)", value = TRUE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("simulateResp")),
              sliderInput(ns("mu"), "Media base:", min = 50, max = 500, value = 300, step = 5),
              sliderInput(ns("grad_row"), "Gradiente por fila (efecto lineal):", min = 0, max = 10, value = 4, step = 0.5),
              sliderInput(ns("grad_col"), "Gradiente por columna (efecto lineal):", min = 0, max = 10, value = 2, step = 0.5),
              sliderInput(ns("sd_eps"), "σ residual (ruido):", min = 1, max = 50, value = 12, step = 1)
            ),
            hr(),
            selectInput(ns("col_gen"), "Columna genotipo:", choices = NULL),
            selectInput(ns("col_row"), "Columna fila:", choices = NULL),
            selectInput(ns("col_col"), "Columna columna:", choices = NULL),
            selectInput(ns("col_y"), "Columna respuesta:", choices = NULL),
            actionButton(ns("fitLMM"), "Ajustar LMM", class = "btn-success")
          ),
          card(
            card_header("Resultados del modelo"),
            navs_tab(
              nav_panel("ANOVA (fijos)",
                        verbatimTextOutput(ns("anovaTxt"))
              ),
              nav_panel("VarCorr & proporciones",
                        tableOutput(ns("varcompTbl")),
                        verbatimTextOutput(ns("varNarrative"))
              ),
              nav_panel("Diagnóstico rápido",
                        plotOutput(ns("diagQQ"), height = "280px"),
                        plotOutput(ns("diagResFit"), height = "280px")
              )
            )
          )
        )
      ),

      # -------- Pestaña 5: Ejercicios prácticos ----------
      nav_panel(
        title = "Ejercicios prácticos",
        navs_tab(
          nav_panel(
            "Experimento 1: Gradientes 2D",
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Configura gradientes"),
                sliderInput(ns("ex1_grad_row"), "Gradiente fila (fuerte = 2D claro):", min = 0, max = 12, value = 6, step = 0.5),
                sliderInput(ns("ex1_grad_col"), "Gradiente columna:", min = 0, max = 12, value = 4, step = 0.5),
                sliderInput(ns("ex1_sd"), "σ residual:", min = 2, max = 40, value = 10, step = 1),
                actionButton(ns("runEx1"), "Comparar RCBD vs R-C", class = "btn-primary")
              ),
              card(
                card_header("Resultado comparativo"),
                tableOutput(ns("ex1CompareTbl")),
                p(em("Observa cómo el LMM con efectos cruzados reduce σ² residual frente a RCBD cuando hay gradiente 2D."))
              )
            )
          ),
          nav_panel(
            "Experimento 2: Filas y réplicas",
            layout_columns(
              col_widths = c(4, 8),
              card(
                card_header("Parámetros de diseño"),
                sliderInput(ns("ex2_rows"), "Filas por réplica:", min = 3, max = 12, value = 5, step = 1),
                sliderInput(ns("ex2_reps"), "Réplicas:", min = 2, max = 6, value = 3, step = 1),
                actionButton(ns("runEx2"), "Re-diseñar y evaluar", class = "btn-secondary")
              ),
              card(
                card_header("Impacto en varianzas"),
                tableOutput(ns("ex2VarTbl")),
                p(em("Más réplicas reducen la incertidumbre global; más filas ayudan cuando el gradiente N–S domina."))
              )
            )
          )
        )
      ),

      # -------- Pestaña 6: Referencias ----------
      nav_panel(
        title = "Referencias",
        card(
          card_header("Fuentes clave (APA breve)"),
          HTML(paste(
            "<ul>",
            "<li>Murillo, D. A., Gezan, S. A., Heilman, A. M., Walk, T. C., Aparicio, J. S., & Horsley, R. D. ",
            "(2021). <i>FielDHub: A Shiny App for Design of Experiments in Life Sciences</i>. JOSS, 6(61), 3122.",
            " (ver viñetas CRAN de Row–Column y funciones).</li>",
            "<li>FielDHub – <code>row_column()</code> (man page) y método <code>plot.FielDHub</code> (referencias en línea).</li>",
            "<li>Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). <i>Fitting Linear Mixed-Effects Models Using lme4</i>. JSS, 67(1). ",
            "Uso de <code>VarCorr()</code> para extraer componentes de varianza.</li>",
            "</ul>"
          ))
        )
      )
    )
  )
}

# ------------------------------
# SERVER
# ------------------------------
session7_v3Server <- function(input, output, session) {
  ns <- session$ns
  pkgs <- c("FielDHub","DT","dplyr","tidyr","purrr","lme4","lmerTest","broom.mixed","ggplot2")
  to_load <- pkgs[!pkgs %in% (.packages())]
  if (length(to_load)) {
    suppressPackageStartupMessages(lapply(to_load, require, character.only = TRUE))
  }

  # ---- 2) Generar diseño ----
  rcd <- eventReactive(input$goDesign, {
    validate(
      need(requireNamespace("FielDHub", quietly = TRUE),
           "Instala el paquete FielDHub.")
    )
    showNotification("Generando diseño Row–Column optimizado…", type = "message", duration = 2)
    FielDHub::row_column(
      t = input$t,
      nrows = input$nrows,
      r = input$r,
      l = input$l,
      plotNumber = input$plotStart,
      locationNames = input$locname,
      seed = input$seed,
      iterations = input$iters
    )
  })

  output$designSummary <- renderPrint({
    req(rcd())
    print(rcd())
  })

  # ---- 3) Mapa & FieldBook ----
  output$fieldPlot <- renderPlot({
    req(rcd())
    plot(rcd())  # método S3 plot.FielDHub
  })

  fb <- reactive({
    req(rcd())
    # En viñeta/refs, el fieldBook contiene: ID, LOCATION, PLOT, REP, ROW, COLUMN, ENTRY, TREATMENT
    as.data.frame(rcd()$fieldBook)
  })

  output$fieldTable <- DT::renderDT({
    req(fb())
    DT::datatable(fb(), options = list(pageLength = 10, scrollX = TRUE))
  }, server = FALSE)

  output$dlFieldBook <- downloadHandler(
    filename = function() sprintf("FieldBook_%s.csv", gsub("\\s+","_", input$locname)),
    content = function(file) {
      req(fb())
      write.csv(fb(), file, row.names = FALSE)
    }
  )

  # ---- 4) LMM & VarCorr ----
  # Actualiza choices de columnas al cargar CSV o cuando hay fieldbook generado
  observe({
    df <- NULL
    if (!is.null(input$respFile$datapath)) {
      df <- tryCatch(read.csv(input$respFile$datapath, check.names = FALSE), error = function(e) NULL)
    } else if (!is.null(fb())) {
      df <- fb()
    }
    if (is.null(df)) return()
    cols <- names(df)
    updateSelectInput(session, "col_gen", choices = cols, selected = intersect(cols, c("TREATMENT","ENTRY","GENOTIPO"))[1])
    updateSelectInput(session, "col_row", choices = cols, selected = intersect(cols, c("ROW","Fila","fila"))[1])
    updateSelectInput(session, "col_col", choices = cols, selected = intersect(cols, c("COLUMN","Col","columna"))[1])
    updateSelectInput(session, "col_y",   choices = cols, selected = intersect(cols, c("RENDIMIENTO","Yield","Y"))[1])
  })

  # arma dataset de trabajo (carga o simula respuesta)
  data_for_model <- reactive({
    df <- NULL
    if (!is.null(input$respFile$datapath)) {
      df <- read.csv(input$respFile$datapath, check.names = FALSE)
    } else if (!is.null(fb())) {
      df <- fb()
    }
    req(df)

    # si se solicita simulación de respuesta, genera Y en función de gradientes
    if (isTRUE(input$simulateResp)) {
      req(all(c("ROW","COLUMN") %in% names(df)))
      # Efectos lineales por fila/columna + ruido
      df <- df |>
        dplyr::mutate(
          .row_i = as.numeric(ROW),
          .col_j = as.numeric(COLUMN),
          Y = input$mu + input$grad_row * scale(.row_i)[,1] + input$grad_col * scale(.col_j)[,1] +
            rnorm(dplyr::n(), sd = input$sd_eps)
        )
      # si había una col de respuesta ya en el CSV, la dejamos y añadimos Y; usuario elegirá cual.
      if (!("RENDIMIENTO" %in% names(df))) {
        df$RENDIMIENTO <- df$Y
      }
    }
    df
  })

  # Ajuste del modelo
  fit_obj <- eventReactive(input$fitLMM, {
    df <- data_for_model()
    req(df, input$col_gen, input$col_row, input$col_col, input$col_y)
    validate(
      need(all(c(input$col_gen, input$col_row, input$col_col, input$col_y) %in% names(df)),
           "Verifica que las columnas seleccionadas existen.")
    )
    # Prepara factores y respuesta
    df <- df |>
      dplyr::mutate(
        GEN = factor(.data[[input$col_gen]]),
        Fila = factor(.data[[input$col_row]]),
        Col  = factor(.data[[input$col_col]]),
        Resp = as.numeric(.data[[input$col_y]])
      )

    # Modelo LMM con efectos cruzados fila/columna
    # Nota: usamos lmerTest para F y p-val aproximados (Satterthwaite)
    mdl <- lmerTest::lmer(Resp ~ GEN + (1|Fila) + (1|Col), data = df)
    list(
      df = df,
      mdl = mdl
    )
  })

  output$anovaTxt <- renderPrint({
    req(fit_obj())
    # ANOVA tipo III para efectos fijos (GEN)
    print(anova(fit_obj()$mdl, type = 3))
  })

  output$varcompTbl <- renderTable({
    req(fit_obj())
    vc <- lme4::VarCorr(fit_obj()$mdl)
    vc_df <- as.data.frame(vc)
    # Mantener solo varianzas (no correl)
    out <- vc_df |>
      dplyr::select(grp = grp, var1, vcov) |>
      dplyr::mutate(
        componente = dplyr::case_when(
          grp == "Fila" ~ "Var(Fila)",
          grp == "Col"  ~ "Var(Columna)",
          grp == "Residual" ~ "Var(Residual)",
          TRUE ~ grp
        )
      ) |>
      dplyr::select(Componente = componente, Grupo = grp, Variable = var1, Varianza = vcov)
    # Añade proporciones
    total <- sum(out$Varianza, na.rm = TRUE)
    out$Proporcion <- out$Varianza / total
    out
  }, digits = 4)

  output$varNarrative <- renderPrint({
    req(fit_obj())
    vc <- lme4::VarCorr(fit_obj()$mdl)
    vc_df <- as.data.frame(vc)
    v_row <- vc_df$vcov[vc_df$grp == "Fila"]
    v_col <- vc_df$vcov[vc_df$grp == "Col"]
    v_res <- vc_df$vcov[vc_df$grp == "Residual"]
    total <- sum(c(v_row, v_col, v_res), na.rm = TRUE)
    pr <- function(x) sprintf("%.1f%%", 100*x/total)
    cat("Descomposición de varianza (aprox.):\n")
    cat(sprintf("  • Var(Fila)      = %.3f  (%s)\n", v_row, pr(v_row)))
    cat(sprintf("  • Var(Columna)   = %.3f  (%s)\n", v_col, pr(v_col)))
    cat(sprintf("  • Var(Residual)  = %.3f  (%s)\n", v_res, pr(v_res)))
    cat("\nInterpretación:\n")
    cat("- La suma Var(Fila)+Var(Columna) cuantifica ruido espacial estructurado capturado por el bloqueo.\n")
    cat("- Var(Residual) es el ruido no estructurado restante; reducirla mejora potencia.\n")
  })

  # Diagnósticos simples
  output$diagQQ <- renderPlot({
    req(fit_obj())
    e <- resid(fit_obj()$mdl)
    qqnorm(e); qqline(e)
  })

  output$diagResFit <- renderPlot({
    req(fit_obj())
    df <- data.frame(fit = fitted(fit_obj()$mdl), res = resid(fit_obj()$mdl))
    ggplot(df, aes(fit, res)) +
      geom_point(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = 2) +
      labs(x = "Ajustados", y = "Residuos") +
      theme_minimal()
  })

  # ---- 5) Ejercicios prácticos ----

  # Experimento 1: Comparar RCBD vs R-C bajo gradientes 2D
  observeEvent(input$runEx1, {
    req(rcd())
    base <- fb()
    # Simulación: efecto fila/col + residual
    set.seed(123)
    d <- base |>
      dplyr::mutate(
        GEN = factor(TREATMENT),
        Fila = factor(ROW),
        Col  = factor(COLUMN),
        y    = 300 +
          input$ex1_grad_row * scale(as.numeric(ROW))[,1] +
          input$ex1_grad_col * scale(as.numeric(COLUMN))[,1] +
          rnorm(dplyr::n(), sd = input$ex1_sd)
      )

    # Modelo RCBD (bloque = REP) para comparar
    m_rcbd <- lmerTest::lmer(y ~ GEN + (1|REP), data = d)
    # Modelo R-C con efectos cruzados
    m_rc <- lmerTest::lmer(y ~ GEN + (1|Fila) + (1|Col), data = d)

    vc_rcbd <- as.data.frame(lme4::VarCorr(m_rcbd))
    vc_rc   <- as.data.frame(lme4::VarCorr(m_rc))

    get_res <- function(vcdf) vcdf$vcov[vcdf$grp == "Residual"]

    comp <- data.frame(
      Modelo = c("RCBD: y ~ GEN + (1|REP)", "R-C: y ~ GEN + (1|Fila) + (1|Col)"),
      Sigma2_residual = c(get_res(vc_rcbd), get_res(vc_rc))
    )
    output$ex1CompareTbl <- renderTable(comp, digits = 3)
  })

  # Experimento 2: Cambiar filas y réplicas, observar varianzas
  observeEvent(input$runEx2, {
    req(input$ex2_rows, input$ex2_reps)
    # re-generar diseño con mismos t y l, pero nrows y r variables
    rcd2 <- FielDHub::row_column(
      t = 45, nrows = input$ex2_rows, r = input$ex2_reps, l = 1,
      plotNumber = 101, locationNames = "TEST",
      seed = 2025, iterations = 800
    )
    d2 <- as.data.frame(rcd2$fieldBook) |>
      dplyr::mutate(
        GEN = factor(TREATMENT),
        Fila = factor(ROW),
        Col  = factor(COLUMN),
        y    = 300 +
          6 * scale(as.numeric(ROW))[,1] + 4 * scale(as.numeric(COLUMN))[,1] +
          rnorm(dplyr::n(), sd = 10)
      )
    m2 <- lmerTest::lmer(y ~ GEN + (1|Fila) + (1|Col), data = d2)
    vc2 <- as.data.frame(lme4::VarCorr(m2))
    out <- vc2 |>
      dplyr::select(Componente = grp, Varianza = vcov) |>
      dplyr::mutate(
        Param = paste0("nrows=", input$ex2_rows, ", r=", input$ex2_reps)
      )
    output$ex2VarTbl <- renderTable(out, digits = 3)
  })
}
