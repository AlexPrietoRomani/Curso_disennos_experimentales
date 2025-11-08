# ===============================================================
# session8.R  —  Sesión 8: Diseños de Bloques Aumentados (DBA)
# Módulo Shiny: UI + Server
# Requisitos sugeridos: bslib, shiny, DT, dplyr, readr, ggplot2,
#                       augmentedRCBD, lmerTest, emmeans, agricolae (opcional)
# ===============================================================

# ---------------------------
# UI
# ---------------------------
session8_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 8: Diseños de Bloques Aumentados")
    ),
    # Contenedor principal con pestañas
    navset_tab(
      # ====== Pestaña 1: Contexto & Objetivos ======
      nav_panel(
        title = "1) Contexto & objetivos",
        layout_columns(
          col_widths = c(6,6),
          card(
            h4("¿Cuándo usar un Diseño de Bloques Aumentados (DBA)?"),
            tags$ul(
              tags$li("Fases tempranas de fitomejoramiento con muchas líneas y poca semilla."),
              tags$li("Se replican solo los ", strong("testigos"), " (checks); las nuevas entradas van ", strong("sin replicar"), "."),
              tags$li("Los testigos actúan como 'sensores' para estimar efectos de ", em("bloque"),
                      " y el ", em("CME"), " (error experimental) que se usan para ajustar las medias.")
            ),
            p("Enfoque clásico (Federer, 1961): ANOVA con testigos → efectos de bloque → ",
              strong("medias ajustadas"), " para las nuevas entradas."),
            p("Enfoque moderno (LMM): trata ", em("bloque"), " como aleatorio y permite ",
              em("recuperar información inter-bloque"), " para estimaciones más precisas de medias (BLUEs) o predicciones (BLUPs)."),
            tags$hr(),
            p(em("Aprenderás a ejecutar ambos enfoques y compararlos de forma transparente."))
          ),
          card(
            h4("Objetivos de aprendizaje"),
            tags$ol(
              tags$li("Explicar la estructura del DBA: testigos vs. nuevas entradas en bloques."),
              tags$li("Ejecutar el análisis clásico con ", code("augmentedRCBD()"), "."),
              tags$li("Ajustar un LMM con ", code("lmerTest::lmer()"), " y obtener LS-means con ", code("emmeans"), "."),
              tags$li("Comparar rankings y entender por qué LMM suele ser más robusto."),
              tags$li("Explorar con simulaciones cómo los parámetros del ensayo afectan al ajuste.")
            ),
            div(style="font-size: 0.92rem;",
                tags$details(
                  tags$summary("Notas rápidas"),
                  tags$ul(
                    tags$li("Si no declaras los testigos, ", code("augmentedRCBD()"),
                            " puede inferirlos por replicación."),
                    tags$li("Varianza de bloque grande = ajuste importante; con LMM puedes 'tomar prestada' información entre bloques."),
                    tags$li("Para BLUPs, modela ", code("gen"), " como aleatorio: ", code("yield ~ (1|gen) + (1|block)"))
                  )
                )
            )
          )
        )
      ),

      # ====== Pestaña 2: Estructura & Diseño ======
      nav_panel(
        title = "2) Estructura & diseño",
        layout_columns(
          col_widths = c(4,8),
          card(
            h4("Fuente de datos"),
            radioButtons(ns("data_source"), NULL,
                         choices = c("Usar ejemplo (Pattersen1994)" = "example",
                                     "Cargar CSV propio" = "upload"),
                         selected = "example"),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'upload'", ns("data_source")),
              fileInput(ns("file_csv"), "Sube tu CSV", accept = c(".csv"))
            ),
            tags$hr(),
            h5("Mapeo de columnas"),
            uiOutput(ns("ui_colmap")),
            tags$hr(),
            checkboxInput(ns("manual_checks"), "Especificar testigos manualmente", FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("manual_checks")),
              textAreaInput(ns("checks_text"),
                            "Nombres de testigos (separados por coma):",
                            placeholder = "Ej.: st, ci, wa")
            ),
            tags$hr(),
            checkboxInput(ns("use_agricolae_gen"),
                          "Generar diseño de ejemplo (opcional, agricolae::design.dau)", FALSE),
            conditionalPanel(
              condition = sprintf("input['%s']", ns("use_agricolae_gen")),
              numericInput(ns("gen_b"), "N° de bloques (b)", 6, min = 2, step = 1),
              numericInput(ns("gen_c"), "N° de testigos (c)", 3, min = 1, step = 1),
              numericInput(ns("gen_k"), "Entradas nuevas por bloque (k)", 5, min = 1, step = 1),
              actionButton(ns("btn_gen"), "Generar diseño")
            )
          ),
          card(
            h4("Vista del libro de campo / datos"),
            div(style="margin-bottom:8px;",
                helpText("Se espera al menos: una columna de bloque, una de genotipo y una de respuesta (rendimiento).")
            ),
            DT::DTOutput(ns("tbl_preview")),
            tags$hr(),
            fluidRow(
              column(6, verbatimTextOutput(ns("txt_summary_short"))),
              column(6, verbatimTextOutput(ns("txt_checks_info")))
            )
          )
        )
      ),

      # ====== Pestaña 3: Análisis clásico (Federer) ======
      nav_panel(
        title = "3) Análisis clásico (augmentedRCBD)",
        layout_columns(
          col_widths = c(4,8),
          card(
            h4("Parámetros"),
            selectInput(ns("classic_method"),
                        "Método de comparación múltiple",
                        choices = c("lsd", "tukey", "none"),
                        selected = "lsd"),
            checkboxInput(ns("classic_group"), "Agrupar por letras (CLD)", TRUE),
            actionButton(ns("btn_run_classic"), "Ejecutar análisis clásico")
          ),
          card(
            h4("Resultados"),
            tabsetPanel(
              tabPanel("ANOVA (solo testigos / ajuste)", verbatimTextOutput(ns("out_classic_anova"))),
              tabPanel("Efectos de bloque", verbatimTextOutput(ns("out_block_eff"))),
              tabPanel("Medias ajustadas (todas)", DT::DTOutput(ns("tbl_classic_means"))),
              tabPanel("Comparaciones (si aplica)", DT::DTOutput(ns("tbl_classic_comparisons")))
            )
          )
        )
      ),

      # ====== Pestaña 4: LMM integrado ======
      nav_panel(
        title = "4) LMM integrado (lmerTest + emmeans)",
        layout_columns(
          col_widths = c(4,8),
          card(
            h4("Modelo"),
            radioButtons(ns("lmm_gen_role"), "Tratamiento (gen):",
                         choices = c("Fijo (BLUEs / LS-means)" = "fixed",
                                     "Aleatorio (BLUPs)" = "random"),
                         selected = "fixed"),
            actionButton(ns("btn_run_lmm"), "Ajustar LMM")
          ),
          card(
            h4("Salida"),
            tabsetPanel(
              tabPanel("Resumen modelo", verbatimTextOutput(ns("out_lmm_summary"))),
              tabPanel("VarCorr (componentes de varianza)", verbatimTextOutput(ns("out_varcorr"))),
              tabPanel("LS-means (si gen = fijo)", DT::DTOutput(ns("tbl_lsmeans"))),
              tabPanel("BLUPs (si gen = aleatorio)", DT::DTOutput(ns("tbl_blups")))
            )
          )
        )
      ),

      # ====== Pestaña 5: Comparativa & ranking ======
      nav_panel(
        title = "5) Comparativa & ranking",
        layout_columns(
          col_widths = c(4,8),
          card(
            h4("Opciones"),
            numericInput(ns("topN"), "Top N para mostrar", value = 15, min = 3, step = 1)
          ),
          card(
            h4("Comparativa medias ajustadas"),
            plotOutput(ns("plt_compare"), height = "340px"),
            DT::DTOutput(ns("tbl_compare_top"))
          )
        )
      ),

      # ====== Pestaña 6: Ejercicios prácticos ======
      nav_panel(
        title = "6) Ejercicios prácticos (simulación)",
        layout_columns(
          col_widths = c(4,8),
          card(
            h4("Parámetros de simulación"),
            numericInput(ns("sim_b"), "Bloques (b)", 6, min = 2),
            numericInput(ns("sim_c"), "Testigos (c)", 3, min = 1),
            numericInput(ns("sim_k"), "Entradas nuevas por bloque (k)", 5, min = 1),
            numericInput(ns("sim_mu"), "Media general (μ)", 2800, step = 10),
            numericInput(ns("sim_sd_block"), "Desv.Est. de bloque", 120, min = 0),
            numericInput(ns("sim_sd_res"), "Desv.Est. residual", 180, min = 0),
            actionButton(ns("btn_sim"), "Simular y analizar")
          ),
          card(
            h4("Resultados de la simulación"),
            p("Se ejecuta el análisis clásico (augmentedRCBD) y el LMM de forma automática para el set simulado."),
            DT::DTOutput(ns("tbl_sim_compare")),
            plotOutput(ns("plt_sim_compare"), height = "320px")
          )
        )
      ),

      # ====== Pestaña 7: Referencias ======
      nav_panel(
        title = "7) Referencias (APA)",
        card(
          h4("Fuentes clave"),
          tags$ul(
            tags$li("Federer, W. T. (1961). Augmented designs with one-way elimination of heterogeneity. ",
                    em("Biometrics, 17(3)"), " 447–473. doi:10.2307/2527837"),
            tags$li("Yates, F. (1940). The recovery of inter-block information in balanced incomplete block designs. ",
                    em("Annals of Eugenics, 10(1)"), " 317–325."),
            tags$li("Paquete ", code("augmentedRCBD"), " (CRAN): ",
                    a("https://cran.r-project.org/package=augmentedRCBD",
                      href="https://cran.r-project.org/package=augmentedRCBD", target="_blank")),
            tags$li(code("augmentedRCBD()"), " (Referencia): ",
                    a("aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.html",
                      href="https://aravind-j.github.io/augmentedRCBD/reference/augmentedRCBD.html", target="_blank")),
            tags$li("Vignette: Data Analysis with augmentedRCBD: ",
                    a("aravind-j.github.io/.../Data_Analysis_with_augmentedRCBD.html",
                      href="https://aravind-j.github.io/augmentedRCBD/articles/Data_Analysis_with_augmentedRCBD.html", target="_blank")),
            tags$li("Tutorial DSFAIR (Pattersen1994): ",
                    a("schmidtpaul.github.io/DSFAIR/augmented_Pattersen1994.html",
                      href="https://schmidtpaul.github.io/DSFAIR/augmented_Pattersen1994.html", target="_blank")),
            tags$li(code("agricolae::design.dau"), " (manual): ",
                    a("rdrr.io/cran/agricolae/man/design.dau.html",
                      href="https://rdrr.io/cran/agricolae/man/design.dau.html", target="_blank")),
            tags$li("lmerTest (CRAN): ",
                    a("https://cran.r-project.org/package=lmerTest",
                      href="https://cran.r-project.org/package=lmerTest", target="_blank")),
            tags$li("emmeans (CRAN): ",
                    a("https://cran.r-project.org/package=emmeans",
                      href="https://cran.r-project.org/package=emmeans", target="_blank")),
            tags$li("lme4::VarCorr (help): ",
                    a("search.r-project.org/CRAN/refmans/lme4/html/VarCorr.html",
                      href="https://search.r-project.org/CRAN/refmans/lme4/html/VarCorr.html", target="_blank"))
          )
        )
      )
    )
  )
}

# ---------------------------
# SERVER
# ---------------------------
session8_v3Server <- function(input, output, session) {

  # ---- Helpers de disponibilidad de paquetes ----
  .need_pkg <- function(pkgs) {
    missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(missing)) {
      stop(sprintf("Faltan paquetes: %s\nInstálalos con install.packages(c(%s))",
                   paste(missing, collapse = ", "),
                   paste(sprintf('"%s"', missing), collapse = ", ")),
           call. = FALSE)
    }
  }

  # ---- Datos (carga / mapeo de columnas) ----
  # Reactivo: dataset base
  dat_base <- reactive({
    if (input$data_source == "example") {
      # Pattersen1994 (DSFAIR)
      # Si falla la descarga, produce error claro
      url <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Pattersen1994.csv"
      df <- try(suppressWarnings(read.csv(url, stringsAsFactors = FALSE)), silent = TRUE)
      if (inherits(df, "try-error") || !nrow(df)) {
        stop("No se pudo leer el dataset de ejemplo desde DSFAIR. Verifica tu conexión.")
      }
      # Esperado: columnas block, gen, yield
      df
    } else {
      file <- input$file_csv
      validate(need(!is.null(file), "Sube un CSV para continuar."))
      df <- try(suppressWarnings(read.csv(file$datapath, stringsAsFactors = FALSE)), silent = TRUE)
      if (inherits(df, "try-error") || !nrow(df)) {
        stop("No se pudo leer el CSV subido.")
      }
      df
    }
  })

  # UI dinámico para mapear columnas
  output$ui_colmap <- renderUI({
    df <- dat_base()
    cols <- names(df)
    ns <- session$ns
    tagList(
      selectInput(ns("col_block"), "Columna de bloque", choices = cols,
                  selected = if ("block" %in% cols) "block" else cols[1]),
      selectInput(ns("col_gen"), "Columna de genotipo (tratamiento)", choices = cols,
                  selected = if ("gen" %in% cols) "gen" else cols[2]),
      selectInput(ns("col_y"), "Columna de respuesta (rendimiento)", choices = cols,
                  selected = if ("yield" %in% cols) "yield" else cols[3])
    )
  })

  # Dataset armonizado (block, gen, yield)
  dat_harmonized <- reactive({
    df <- dat_base()
    req(input$col_block, input$col_gen, input$col_y)
    # Selección y renombre seguro
    df <- df[, c(input$col_block, input$col_gen, input$col_y), drop = FALSE]
    names(df) <- c("block", "gen", "yield")
    # Coerción a tipos esperados
    df$block <- as.factor(df$block)
    df$gen   <- as.factor(df$gen)
    df$yield <- suppressWarnings(as.numeric(df$yield))
    validate(need(!anyNA(df$yield), "Hay NA en 'yield' tras coerción. Revisa tu columna de respuesta."))
    df
  })

  # Vista previa + resumen corto
  output$tbl_preview <- DT::renderDT({
    DT::datatable(head(dat_harmonized(), 20), options = list(pageLength = 10), rownames = FALSE)
  })

  output$txt_summary_short <- renderPrint({
    df <- dat_harmonized()
    cat("Observaciones:", nrow(df), "\n")
    cat("Bloques:", nlevels(df$block), "— niveles:", paste(levels(df$block), collapse = ", "), "\n")
    cat("Genotipos:", nlevels(df$gen), "— algunos:", paste(head(levels(df$gen), 8), collapse = ", "), "...\n")
    s <- summary(df$yield)
    print(s)
  })

  # Identificación de testigos
  # - Manual (texto) o automática por replicación total en todos los bloques
  auto_checks <- reactive({
    df <- dat_harmonized()
    # frecuencia por bloque
    tab <- table(df$gen, df$block)
    # testigo = aparece en todos los bloques (replicado en cada bloque)
    gens_all_blocks <- rownames(tab)[apply(tab > 0, 1, all)]
    sort(gens_all_blocks)
  })

  checks_vec <- reactive({
    if (isTRUE(input$manual_checks) && nzchar(input$checks_text)) {
      ch <- trimws(unlist(strsplit(input$checks_text, ",")))
      as.character(ch[ch != ""])
    } else {
      auto_checks()
    }
  })

  output$txt_checks_info <- renderPrint({
    ch <- checks_vec()
    cat("Testigos detectados:", if (length(ch)) paste(ch, collapse = ", ") else "(ninguno)")
  })

  # ---- (Opcional) Generación de diseño con agricolae::design.dau ----
  observeEvent(input$btn_gen, {
    .need_pkg(c("agricolae", "dplyr"))
    b <- input$gen_b; cks <- input$gen_c; k <- input$gen_k
    validate(need(b > 1 && cks >= 1 && k >= 1, "Parámetros inválidos."))

    # Creamos etiquetas
    checks <- paste0("T", seq_len(cks))
    n_new  <- b * k
    news   <- paste0("N", seq_len(n_new))

    # Agricolae genera diseño base de aumentados (checks replicados; new sin replicar)
    set.seed(123)
    des <- agricolae::design.dau(trt1 = checks, r = b, trt2 = news, seed = 123)

    # Construimos data.frame con estructura block, gen, yield (yield vacío)
    fb <- des$book
    names(fb) <- tolower(names(fb))
    # Esperados en book: block, plots, trt1/trt2
    # Unificamos tratamiento en 'gen'
    fb$gen <- ifelse(!is.na(fb$trt1), fb$trt1, fb$trt2)
    df <- fb[, c("block", "gen")]
    df$yield <- NA_real_

    showModal(modalDialog(
      title = "Diseño generado",
      div("Se generó un diseño aumentado con:",
          tags$ul(
            tags$li(sprintf("%d bloques", b)),
            tags$li(sprintf("%d testigos", cks)),
            tags$li(sprintf("%d entradas nuevas (total)", n_new))
          ),
          "Puedes exportar el 'book' original desde agricolae si lo deseas."
      ),
      easyClose = TRUE
    ))

    # Sobrescribir fuente de datos (solo en memoria de esta sesión)
    # Nota: no sustituimos dat_base(); mostramos en preview
    output$tbl_preview <- DT::renderDT({
      DT::datatable(head(df, 25), options = list(pageLength = 10), rownames = FALSE)
    })

  }, ignoreInit = TRUE)

  # =========================================================
  #  Pestaña 3 — Análisis clásico: augmentedRCBD
  # =========================================================
  classic_fit <- eventReactive(input$btn_run_classic, {
    .need_pkg(c("augmentedRCBD"))
    df <- dat_harmonized()
    ch <- checks_vec()
    method <- input$classic_method
    group  <- isTRUE(input$classic_group)

    validate(need(nlevels(df$block) >= 2, "Se requieren ≥ 2 bloques"))
    validate(need(nlevels(df$gen)   >= 3, "Se requieren ≥ 3 genotipos"))

    # augmentedRCBD infiere checks si no se pasan
    out <- augmentedRCBD::augmentedRCBD(
      block     = df$block,
      treatment = df$gen,
      y         = df$yield,
      checks    = if (length(ch)) ch else NULL,
      method.comp = method,
      group       = group,
      simplify    = TRUE,
      console     = FALSE
    )
    out
  })

  # Salidas clásicas
  output$out_classic_anova <- renderPrint({
    out <- classic_fit()
    cat("ANOVA (Treatment Adjusted / Block Adjusted)\n")
    # El objeto simplificado devuelve data.frames; mostramos ambos si existen
    if (!is.null(out$`ANOVA, Treatment Adjusted`)) {
      cat("\n--- ANOVA, Treatment Adjusted ---\n")
      print(out$`ANOVA, Treatment Adjusted`)
    }
    if (!is.null(out$`ANOVA, Block Adjusted`)) {
      cat("\n--- ANOVA, Block Adjusted ---\n")
      print(out$`ANOVA, Block Adjusted`)
    }
    if (!is.null(out$`Overall adjusted mean`)) {
      cat("\nOverall adjusted mean:", out$`Overall adjusted mean`, "\n")
    }
    if (!is.null(out$`CV`)) {
      cat("CV (%):", round(as.numeric(out$`CV`), 3), "\n")
    }
  })

  output$out_block_eff <- renderPrint({
    out <- classic_fit()
    if (!is.null(out$`Block effects`)) {
      cat("Efectos de bloque (factor de ajuste por bloque):\n")
      print(out$`Block effects`)
    } else {
      cat("No disponible en salida.")
    }
  })

  output$tbl_classic_means <- DT::renderDT({
    out <- classic_fit()
    req(out$Means)
    DT::datatable(out$Means, options = list(pageLength = 10), rownames = FALSE)
  })

  output$tbl_classic_comparisons <- DT::renderDT({
    out <- classic_fit()
    if (!is.null(out$Comparisons)) {
      DT::datatable(out$Comparisons, options = list(pageLength = 10), rownames = FALSE)
    }
  })

  # =========================================================
  #  Pestaña 4 — LMM integrado: lmerTest + emmeans
  # =========================================================
  lmm_fit <- eventReactive(input$btn_run_lmm, {
    .need_pkg(c("lmerTest"))
    df <- dat_harmonized()
    validate(need(nlevels(df$block) >= 2, "Se requieren ≥ 2 bloques"))

    if (identical(input$lmm_gen_role, "fixed")) {
      # gen fijo → BLUEs via emmeans
      fit <- lmerTest::lmer(yield ~ gen + (1|block), data = df)
      return(list(model = fit, role = "fixed"))
    } else {
      # gen aleatorio → BLUPs
      fit <- lmerTest::lmer(yield ~ 1 + (1|gen) + (1|block), data = df)
      return(list(model = fit, role = "random"))
    }
  })

  output$out_lmm_summary <- renderPrint({
    obj <- lmm_fit(); fit <- obj$model
    print(summary(fit))
  })

  output$out_varcorr <- renderPrint({
    obj <- lmm_fit(); fit <- obj$model
    print(lme4::VarCorr(fit), comp = "Variance")
  })

  output$tbl_lsmeans <- DT::renderDT({
    obj <- lmm_fit(); fit <- obj$model
    req(obj$role == "fixed")
    .need_pkg(c("emmeans"))
    em <- emmeans::emmeans(fit, ~ gen)
    DT::datatable(as.data.frame(em), options = list(pageLength = 10), rownames = FALSE)
  })

  output$tbl_blups <- DT::renderDT({
    obj <- lmm_fit(); fit <- obj$model
    req(obj$role == "random")
    re <- lme4::ranef(fit)
    # BLUPs de gen
    if (!is.null(re$gen)) {
      bl <- data.frame(gen = rownames(re$gen), BLUP = re$gen[,"(Intercept)"], row.names = NULL)
      DT::datatable(bl, options = list(pageLength = 10), rownames = FALSE)
    }
  })

  # =========================================================
  #  Pestaña 5 — Comparativa & ranking
  # =========================================================
  comp_tbl <- reactive({
    out_c <- classic_fit()
    obj_l <- lmm_fit()

    # Clásico: columna "Adjusted Means"
    cm <- out_c$Means
    validate(need(!is.null(cm) && "Adjusted Means" %in% names(cm),
                  "No encuentro 'Adjusted Means' en salida clásica."))
    df_c <- cm[, c("Treatment", "Adjusted Means")]
    names(df_c) <- c("gen", "classic_adj")

    # LMM:
    if (obj_l$role == "fixed") {
      .need_pkg(c("emmeans"))
      em <- as.data.frame(emmeans::emmeans(obj_l$model, ~ gen))
      df_l <- em[, c("gen", "emmean")]
      names(df_l) <- c("gen", "lmm_est")
    } else {
      bl <- lme4::ranef(obj_l$model)$gen
      df_l <- data.frame(gen = rownames(bl), lmm_est = bl[, "(Intercept)"], row.names = NULL)
    }

    # Unimos
    merge(df_c, df_l, by = "gen", all = TRUE)
  })

  output$plt_compare <- renderPlot({
    .need_pkg(c("ggplot2"))
    tb <- comp_tbl()
    tb <- tb[complete.cases(tb[, c("classic_adj", "lmm_est")]), ]
    ggplot2::ggplot(tb, ggplot2::aes(x = classic_adj, y = lmm_est)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
      ggplot2::labs(x = "Clásico: Media ajustada (augmentedRCBD)",
                    y = if (identical(lmm_fit()$role, "fixed")) "LMM: LS-mean (emmeans)"
                         else "LMM: BLUP (random gen)",
                    title = "Comparativa Clásico vs. LMM") +
      ggplot2::theme_minimal(base_size = 12)
  })

  output$tbl_compare_top <- DT::renderDT({
    tb <- comp_tbl()
    # ranking por LMM
    tb <- tb[order(-tb$lmm_est), ]
    DT::datatable(head(tb, input$topN), options = list(pageLength = 10), rownames = FALSE)
  })

  # =========================================================
  #  Pestaña 6 — Ejercicios prácticos (simulación)
  # =========================================================
  sim_compare <- eventReactive(input$btn_sim, {
    .need_pkg(c("dplyr", "augmentedRCBD", "lmerTest", "emmeans"))

    b  <- input$sim_b
    c0 <- input$sim_c
    k  <- input$sim_k
    mu <- input$sim_mu
    sd_block <- input$sim_sd_block
    sd_res   <- input$sim_sd_res

    validate(need(b >= 2 && c0 >= 1 && k >= 1, "Parámetros inválidos"))

    set.seed(4242)
    # Construimos bloques y testigos
    blocks <- factor(seq_len(b))
    checks <- paste0("st", seq_len(c0))

    # Entradas nuevas: b*k en total
    news   <- paste0("N", seq_len(b * k))

    # Libro de campo simulado: cada bloque contiene todos los testigos + k nuevas
    df_list <- lapply(seq_len(b), function(j) {
      data.frame(
        block = factor(j, levels = seq_len(b)),
        gen   = c(checks, news[((j-1)*k + 1):(j*k)]),
        stringsAsFactors = FALSE
      )
    })
    df <- dplyr::bind_rows(df_list)
    df$gen <- factor(df$gen)

    # Efectos simulados
    eff_block <- rnorm(b, mean = 0, sd = sd_block)
    names(eff_block) <- levels(blocks)

    # Pequeño efecto de gen para ilustrar (media 0)
    eff_gen <- rnorm(nlevels(df$gen), 0, sd = sd_res/4)
    names(eff_gen) <- levels(df$gen)

    # Respuesta
    df$yield <- mu + eff_block[as.character(df$block)] + eff_gen[as.character(df$gen)] + rnorm(nrow(df), 0, sd_res)

    # --- Análisis clásico
    out_c <- augmentedRCBD::augmentedRCBD(
      block = df$block, treatment = df$gen, y = df$yield,
      checks = checks, method.comp = "lsd", group = TRUE, simplify = TRUE, console = FALSE
    )

    # --- LMM (gen fijo)
    fit_lmm <- lmerTest::lmer(yield ~ gen + (1|block), data = df)
    em <- as.data.frame(emmeans::emmeans(fit_lmm, ~ gen))

    # Unión
    cm <- out_c$Means[, c("Treatment", "Adjusted Means")]
    names(cm) <- c("gen", "classic_adj")
    tb <- merge(cm, em[, c("gen", "emmean")], by = "gen", all = TRUE)
    names(tb)[names(tb) == "emmean"] <- "lmm_est"
    list(df = df, classic = out_c, lmm = fit_lmm, table = tb)
  })

  output$tbl_sim_compare <- DT::renderDT({
    out <- sim_compare()
    DT::datatable(head(out$table[order(-out$table$lmm_est), ], 20),
                  options = list(pageLength = 10), rownames = FALSE)
  })

  output$plt_sim_compare <- renderPlot({
    .need_pkg(c("ggplot2"))
    out <- sim_compare(); tb <- out$table
    ggplot2::ggplot(tb, ggplot2::aes(x = classic_adj, y = lmm_est)) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.6) +
      ggplot2::labs(x = "Clásico: Media ajustada (augmentedRCBD)",
                    y = "LMM: LS-mean (emmeans)",
                    title = "Simulación: efecto del ruido de bloque vs. residual") +
      ggplot2::theme_minimal(base_size = 12)
  })

}
