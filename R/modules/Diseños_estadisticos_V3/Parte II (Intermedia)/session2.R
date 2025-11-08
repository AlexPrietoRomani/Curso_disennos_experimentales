# R/modules/session2.R
# Sesión 2: Diseño Completamente al Azar (DCA/CRD) y Bloques Completos al Azar (RCBD)
# Requiere: bslib, ggplot2, dplyr, broom, effectsize, car, agricolae, RColorBrewer, patchwork

session2_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 2: Diseño Completamente al Azar (DCA) y Bloques Completos al Azar (RCBD)")
    ),
    # ---- Navegación principal ----
    navset_tab(

      # ============== PESTAÑA 1: Contexto y Objetivos ==============
      nav_panel(
        title = "1. Contexto y objetivos",
        h4(class = "section-header", "¿Cuándo DCA y cuándo RCBD?"),
        p(
          "El ", strong("Diseño Completamente al Azar (DCA/CRD)"),
          " asigna tratamientos de forma totalmente aleatoria en un ambiente ", strong("homogéneo"),
          " (modelo: ", HTML("&mu; + efecto de tratamiento + error"), "). Referencia NIST (modelo y lógica)."
        ),
        tags$small(em("NIST/SEMATECH e-Handbook — Completely Randomized Design (CRD).")), br(),
        tags$small("Fuente: "), tags$small(a("NIST e-Handbook (CRD)", href="https://www.itl.nist.gov/div898/handbook/pri/section3/pri3331.htm", target="_blank")),
        br(), br(),
        p(
          "Cuando existe heterogeneidad sistemática (p. ej., gradientes de suelo/pendiente), el ",
          strong("RCBD"),
          " incorpora ", strong("bloques"), " para controlar esa variación entre-subgrupos y comparar tratamientos ",
          em("dentro de bloques"), "."
        ),
        tags$small(em("Curso STAT (Penn State Online): lecciones CRD y RCBD.")), br(),
        tags$small(a("STAT 502/503 Índice (RCBD/CRD)", href="https://online.stat.psu.edu/stat502/", target="_blank")),
        br(), br(),
        h4(class="section-header", "Supuestos y tamaño del efecto"),
        tags$ul(
          tags$li("ANOVA asume residuos aproximadamente normales, varianzas homogéneas e independencia. Verificación con diagnósticos estándar (residuos vs ajustados, QQ-plot) y pruebas (Shapiro, Levene)."),
          tags$li("Reportar, además del p-valor, un ", strong("tamaño del efecto (η²)"), " ayuda a evaluar relevancia práctica.")
        ),
        tags$small(a("afex: ANOVA en R y supuestos", href="https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html", target="_blank")), br(),
        tags$small(a("effectsize::eta_squared()", href="https://cran.r-project.org/web/packages/effectsize/index.html", target="_blank")),
        br(), br(),
        hr(),
        h5(strong("Resultado de aprendizaje:")),
        tags$ul(
          tags$li("Generar y analizar un DCA (CRD) con R base: aleatorización con sample(), ANOVA con aov()."),
          tags$li("Simular heterogeneidad y justificar RCBD."),
          tags$li("Generar un RCBD con agricolae::design.rcbd(), simular y analizar con aov()."),
          tags$li("Comparar CM(Error) y Eficiencia Relativa (RE) entre DCA y RCBD.")
        )
      ),

      # ============== PESTAÑA 2: DCA — diseño y análisis ==============
      nav_panel(
        title = "2. DCA: diseño y análisis",
        h4(class = "section-header", "Constructor y análisis de un DCA (CRD)"),
        sidebarLayout(
          sidebarPanel(width = 3,
            numericInput(ns("dca_k"), "N° tratamientos (k):", value = 3, min = 2, max = 12, step = 1),
            numericInput(ns("dca_r"), "Replicaciones por tratamiento (r):", value = 5, min = 2, max = 30, step = 1),
            sliderInput(ns("dca_diff"), "Rango de diferencias de medias (0 = iguales):", min = 0, max = 5, value = 1.5, step = 0.1),
            sliderInput(ns("dca_sigma"), "Desv. estándar del error (σ):", min = 0.1, max = 5, value = 1, step = 0.1),
            numericInput(ns("dca_mu"), "Media base (μ):", value = 20, min = -1e3, max = 1e3),
            actionButton(ns("dca_go"), "Generar & Analizar", icon = icon("play"))
          ),
          mainPanel(width = 9,
            fluidRow(
              column(6, plotOutput(ns("dca_layout_plot"), height = "260px")),
              column(6, plotOutput(ns("dca_box"), height = "260px"))
            ),
            hr(),
            h5("ANOVA (aov) y tamaño del efecto (η²)"),
            tableOutput(ns("dca_anova_tbl")),
            verbatimTextOutput(ns("dca_eta")),
            hr(),
            h5("Diagnóstico de supuestos"),
            fluidRow(
              column(6, plotOutput(ns("dca_resid_vs_fit"), height = "260px")),
              column(6, plotOutput(ns("dca_qq"), height = "260px"))
            ),
            fluidRow(
              column(6, verbatimTextOutput(ns("dca_shapiro"))),
              column(6, verbatimTextOutput(ns("dca_levene")))
            )
          )
        )
      ),

      # ============== PESTAÑA 3: Heterogeneidad simulada ==============
      nav_panel(
        title = "3. Heterogeneidad simulada (¿por qué RCBD?)",
        h4(class = "section-header", "Compara DCA (ignora bloques) vs RCBD (controla bloques)"),
        sidebarLayout(
          sidebarPanel(width = 3,
            numericInput(ns("het_k"), "N° tratamientos (k):", value = 4, min = 2, max = 10),
            numericInput(ns("het_b"), "N° bloques (b):", value = 4, min = 2, max = 12),
            sliderInput(ns("het_grad"), "Fuerza del gradiente de bloque:", min = 0, max = 10, value = 4, step = 0.5),
            sliderInput(ns("het_diff"), "Rango de diferencias de tratamiento:", min = 0, max = 5, value = 2, step = 0.1),
            sliderInput(ns("het_sigma"), "Desv. estándar del error (σ):", min = 0.1, max = 5, value = 1.2, step = 0.1),
            actionButton(ns("het_go"), "Simular & Comparar", icon = icon("chart-line"))
          ),
          mainPanel(width = 9,
            fluidRow(
              column(6, plotOutput(ns("het_heat"), height = "280px")),
              column(6, plotOutput(ns("het_box_by_trt"), height = "280px"))
            ),
            hr(),
            h5("Comparación de modelos"),
            tags$table(class = "table table-bordered",
              tags$thead(
                tags$tr(
                  tags$th("Modelo"),
                  tags$th("F (trat)"),
                  tags$th("p-valor (trat)"),
                  tags$th("CM(Error)")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("DCA: aov(y ~ tratamiento)"),
                  tags$td(textOutput(ns("het_F_dca"))),
                  tags$td(textOutput(ns("het_p_dca"))),
                  tags$td(textOutput(ns("het_mse_dca")))
                ),
                tags$tr(
                  tags$td("RCBD: aov(y ~ tratamiento + bloque)"),
                  tags$td(textOutput(ns("het_F_rcbd"))),
                  tags$td(textOutput(ns("het_p_rcbd"))),
                  tags$td(textOutput(ns("het_mse_rcbd")))
                )
              )
            ),
            hr(),
            h5("Diagnóstico (modelo RCBD)"),
            fluidRow(
              column(6, plotOutput(ns("het_resid_vs_fit_rcbd"), height = "260px")),
              column(6, plotOutput(ns("het_qq_rcbd"), height = "260px"))
            )
          )
        )
      ),

      # ============== PESTAÑA 4: RCBD — diseño y análisis ==============
      nav_panel(
        title = "4. RCBD: diseño y análisis",
        h4(class = "section-header", "Aleatorización con agricolae::design.rcbd() + ANOVA"),
        p("Se usa agricolae para generar el plan; el campo se visualiza como una matriz por bloques."),
        sidebarLayout(
          sidebarPanel(width = 3,
            numericInput(ns("rcbd_k"), "N° tratamientos (k):", value = 4, min = 2, max = 12),
            numericInput(ns("rcbd_b"), "N° bloques (b):", value = 4, min = 2, max = 12),
            sliderInput(ns("rcbd_diff"), "Rango de diferencias de tratamiento:", min = 0, max = 5, value = 2, step = 0.1),
            sliderInput(ns("rcbd_sigma"), "Desv. estándar del error (σ):", min = 0.1, max = 5, value = 1.2, step = 0.1),
            actionButton(ns("rcbd_go"), "Generar & Analizar", icon = icon("shuffle"))
          ),
          mainPanel(width = 9,
            uiOutput(ns("rcbd_layout_ui")),
            hr(),
            h5("ANOVA (RCBD) y post-hoc"),
            tableOutput(ns("rcbd_anova_tbl")),
            verbatimTextOutput(ns("rcbd_eta")),
            plotOutput(ns("rcbd_tukey_plot"), height = "280px"),
            verbatimTextOutput(ns("rcbd_tukey_text"))
          )
        ),
        p(class="small text-muted",
          "Nota: El flujo muestra el uso de ", code("design.rcbd()"),
          " y la tabla ", code("$book"), " típica en estos diseños."
        )
      ),

      # ============== PESTAÑA 5: Eficiencia y varianzas ==============
      nav_panel(
        title = "5. Eficiencia y varianzas",
        h4(class = "section-header", "¿Valió la pena bloquear?"),
        p("Se contrasta la precisión de RCBD vs el DCA hipotético con los mismos datos y tamaño, usando CM(Error) y una métrica de ",
          strong("Eficiencia Relativa (RE)"), "."),
        fluidRow(
          column(6,
            tableOutput(ns("eff_table"))
          ),
          column(6,
            div(class="text-center", style="padding: 18px; border: 1px solid #ddd; border-radius: 10px;",
              h5("Eficiencia Relativa (RE)"),
              span(style="font-size: 2.4em; font-weight: bold;", textOutput(ns("eff_re"))),
              p(textOutput(ns("eff_msg")))
            )
          )
        )
      ),

      # ============== PESTAÑA 6: Scripts listos para copiar ==============
      nav_panel(
        title = "6. Scripts listos para copiar",
        h4(class="section-header", "DCA (CRD) con sample() + aov()"),
        pre(class="r-code",
"
# --- DCA mínimo reproducible ---
set.seed(123)
k <- 3; r <- 5; mu <- 20; diff <- 1.5; sigma <- 1
trts <- paste0('T', 1:k)
# Aleatoriza el orden (CRD)
plan <- data.frame(trat = factor(sample(rep(trts, each = r), size = k*r)))
# Efectos lineales 0..diff
ef <- seq(0, diff, length.out = k); names(ef) <- trts
y <- mu + ef[plan$trat] + rnorm(nrow(plan), 0, sigma)
mod_dca <- aov(y ~ trat, data = transform(plan, y = y))
summary(mod_dca)
effectsize::eta_squared(mod_dca)
"
        ),
        h4(class="section-header", "RCBD con agricolae::design.rcbd() + aov()"),
        pre(class="r-code",
"
# --- RCBD mínimo reproducible ---
set.seed(123)
k <- 4; b <- 4; mu <- 20; diff <- 2; sigma <- 1.2
trts <- paste0('T', 1:k)
out <- agricolae::design.rcbd(trts, r = b)           # plan de campo
book <- out$book                                     # block, plot, tratamientos
ef <- seq(0, diff, length.out = k); names(ef) <- trts
ef_b <- seq(0, 5, length.out = b)                    # gradiente de bloque ilustrativo
y <- mu + ef[book$tratamientos] + ef_b[book$block] + rnorm(nrow(book), 0, sigma)
mod_rcbd <- aov(y ~ tratamientos + factor(block), data = transform(book, y = y))
summary(mod_rcbd)
effectsize::eta_squared(mod_rcbd, partial = TRUE)
"
        )
      ),

      # ============== PESTAÑA 7: Referencias ==============
      nav_panel(
        title = "Referencias",
        tags$ul(
          tags$li("NIST/SEMATECH (CRD): ",
                  a("Completely Randomized Design", href="https://www.itl.nist.gov/div898/handbook/pri/section3/pri3331.htm", target="_blank")),
          tags$li("Penn State (STAT Online): ",
                  a("Índice de lecciones (incluye CRD y RCBD)", href="https://online.stat.psu.edu/stat502/", target="_blank")),
          tags$li("afex (vignette): ",
                  a("ANOVA example & assumptions", href="https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html", target="_blank")),
          tags$li("effectsize (CRAN): ",
                  a("eta_squared()", href="https://cran.r-project.org/web/packages/effectsize/index.html", target="_blank")),
          tags$li("Uso de design.rcbd(): ",
                  a("agricolaeplotr docs (ejemplo con design.rcbd)", href="https://kwstat.github.io/agricolaeplotr/reference/plot_rcbd.html", target="_blank"))
        ),
        p(class="text-muted small",
          "La justificación conceptual de bloqueo (comparar lo similar y remover variación no interesante) y sus efectos sobre CM(Error) y potencia se alinea con cursos estándar de diseños experimentales (p. ej., STAT Online, Penn State)."
        )
      )
    )
  )
}

session2_v3Server <- function(input, output, session) {
  ns <- session$ns
  # ---- Dependencias (cargar aquí por comodidad del módulo) ----
  req <- shiny::req
  library(ggplot2); library(dplyr); library(broom)
  library(effectsize); library(car)
  library(RColorBrewer); library(patchwork)

  # ===================== PESTAÑA 2: DCA =====================
  dca_data <- eventReactive(input$dca_go, {
    k <- input$dca_k; r <- input$dca_r
    trts <- paste0("T", seq_len(k))
    # plano CRD (solo orden aleatorio)
    set.seed(as.integer(Sys.time()))
    plan <- data.frame(
      parcela = seq_len(k * r),
      tratamiento = factor(sample(rep(trts, each = r), size = k * r))
    )
    # efectos lineales 0..diff
    ef_t <- seq(0, input$dca_diff, length.out = k); names(ef_t) <- trts
    y <- input$dca_mu + ef_t[plan$tratamiento] + rnorm(nrow(plan), 0, input$dca_sigma)
    plan$y <- as.numeric(y)
    plan
  }, ignoreInit = TRUE)

  output$dca_layout_plot <- renderPlot({
    df <- req(dca_data())
    ggplot(df, aes(x = parcela, y = 1, fill = tratamiento)) +
      geom_tile(color = "white") +
      scale_fill_brewer(palette = "Set2") +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = "Layout CRD (orden aleatorio de tratamientos)")
  })

  output$dca_box <- renderPlot({
    df <- req(dca_data())
    ggplot(df, aes(tratamiento, y, fill = tratamiento)) +
      geom_boxplot(alpha = 0.8, outlier.alpha = 0.5, show.legend = FALSE) +
      theme_minimal(base_size = 12) +
      labs(title = "Respuesta por tratamiento (CRD)", y = "Respuesta", x = NULL)
  })

  dca_model <- reactive({
    df <- req(dca_data())
    aov(y ~ tratamiento, data = df)
  })

  output$dca_anova_tbl <- renderTable({
    broom::tidy(req(dca_model())) %>%
      select(term, df, sumsq, meansq, statistic, p.value) %>%
      rename(`Fuente`=term, `gl`=df, `SC`=sumsq, `CM`=meansq, `F`=statistic, `p`=p.value)
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$dca_eta <- renderPrint({
    es <- effectsize::eta_squared(req(dca_model()))
    print(es)
  })

  output$dca_resid_vs_fit <- renderPlot({
    m <- req(dca_model())
    plot(m, which = 1, main = "Residuos vs Ajustados (CRD)")
    abline(h = 0, lty = 2, col = "red")
  })
  output$dca_qq <- renderPlot({
    m <- req(dca_model()); rr <- residuals(m)
    qqnorm(rr, main = "QQ-plot de residuos (CRD)"); qqline(rr, col = "red", lwd = 2)
  })
  output$dca_shapiro <- renderPrint({
    m <- req(dca_model()); shapiro.test(residuals(m))
  })
  output$dca_levene <- renderPrint({
    df <- req(dca_data()); car::leveneTest(y ~ tratamiento, data = df)
  })

  # ===================== PESTAÑA 3: HETEROGENEIDAD =====================
  het_res <- eventReactive(input$het_go, {
    k <- input$het_k; b <- input$het_b
    trts <- paste0("T", seq_len(k))
    set.seed(as.integer(Sys.time()))
    # Esqueleto RCBD (cada bloque contiene todos los tratamientos)
    base <- expand.grid(bloque = factor(seq_len(b)),
                        tratamiento = factor(trts, levels = trts))
    # Efectos
    ef_t <- seq(0, input$het_diff, length.out = k); names(ef_t) <- trts
    ef_b <- seq(0, input$het_grad, length.out = b)
    # Respuesta
    y <- 20 + ef_t[base$tratamiento] + ef_b[as.integer(base$bloque)] +
      rnorm(nrow(base), 0, input$het_sigma)
    df <- transform(base, y = as.numeric(y))
    # Modelos
    m_dca  <- aov(y ~ tratamiento, data = df)                 # ignora bloques
    m_rcbd <- aov(y ~ tratamiento + bloque, data = df)        # controla bloques

    list(
      df = df,
      m_dca = m_dca,
      m_rcbd = m_rcbd,
      aov_dca = broom::tidy(m_dca),
      aov_rcbd = broom::tidy(m_rcbd)
    )
  }, ignoreInit = TRUE)

  output$het_heat <- renderPlot({
    res <- req(het_res())
    ggplot(res$df, aes(x = bloque, y = tratamiento, fill = y)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title = "Mapa de calor (gradiente por bloque)", x = "Bloque", y = "Tratamiento", fill = "y")
  })

  output$het_box_by_trt <- renderPlot({
    res <- req(het_res())
    ggplot(res$df, aes(tratamiento, y, fill = tratamiento)) +
      geom_boxplot(show.legend = FALSE) +
      theme_minimal() +
      labs(title = "Distribución por tratamiento (con gradiente en el campo)", y = "y", x = NULL)
  })

  # Métricas comparativas
  output$het_F_dca <- renderText({
    res <- req(het_res()); fmtF(res$aov_dca)
  })
  output$het_p_dca <- renderText({
    res <- req(het_res()); fmtp(res$aov_dca)
  })
  output$het_mse_dca <- renderText({
    res <- req(het_res()); ms_error(res$m_dca)
  })

  output$het_F_rcbd <- renderText({
    res <- req(het_res()); fmtF(res$aov_rcbd)
  })
  output$het_p_rcbd <- renderText({
    res <- req(het_res()); fmtp(res$aov_rcbd)
  })
  output$het_mse_rcbd <- renderText({
    res <- req(het_res()); ms_error(res$m_rcbd)
  })

  output$het_resid_vs_fit_rcbd <- renderPlot({
    res <- req(het_res())
    plot(res$m_rcbd, which = 1, main = "Residuos vs Ajustados (RCBD)")
    abline(h = 0, lty = 2, col = "red")
  })
  output$het_qq_rcbd <- renderPlot({
    res <- req(het_res()); rr <- residuals(res$m_rcbd)
    qqnorm(rr, main = "QQ-plot de residuos (RCBD)"); qqline(rr, col = "red", lwd = 2)
  })

  # ===================== PESTAÑA 4: RCBD =====================
  rcbd_res <- eventReactive(input$rcbd_go, {
    k <- input$rcbd_k; b <- input$rcbd_b; trts <- paste0("T", seq_len(k))
    set.seed(as.integer(Sys.time()))
    out <- agricolae::design.rcbd(trts, r = b)
    book <- out$book # columnas esperadas: plots, block, tratamientos
    # Simular respuesta con gradiente de bloque (suave) + efecto de tratamiento
    ef_t <- seq(0, input$rcbd_diff, length.out = k); names(ef_t) <- trts
    ef_b <- seq(0, 5, length.out = b)
    y <- 20 + ef_t[book$tratamientos] + ef_b[book$block] + rnorm(nrow(book), 0, input$rcbd_sigma)
    df <- transform(book, y = as.numeric(y), bloque = factor(block), tratamiento = factor(tratamientos, levels = trts))
    m  <- aov(y ~ tratamiento + bloque, data = df)

    # Tukey solo si tratamientos significativos
    p_trat <- broom::tidy(m) %>% filter(term == "tratamiento") %>% pull(p.value)
    tuk <- if (!is.na(p_trat) && p_trat < 0.05) TukeyHSD(m, which = "tratamiento") else NULL

    list(out = out, df = df, model = m, aov = broom::tidy(m), eta = effectsize::eta_squared(m, partial = TRUE), tukey = tuk)
  }, ignoreInit = TRUE)

  output$rcbd_layout_ui <- renderUI({
    res <- req(rcbd_res())
    book <- res$out$book
    k <- length(unique(book$tratamientos)); b <- length(unique(book$block))
    cols <- brewer.pal(max(3, min(8, k)), "Set2")
    names(cols) <- unique(book$tratamientos)

    # Tabla bloques como columnas: cada fila = posición dentro del bloque
    filas <- lapply(seq_len(k), function(i) {
      celdas <- lapply(seq_len(b), function(j) {
        tr <- book$tratamientos[book$block == j][i]
        tags$td(style = paste0("background-color:", cols[tr], "; border:1px solid #e5e7eb; font-weight:bold;"),
                tr)
      })
      tags$tr(celdas)
    })
    header <- tags$tr(lapply(seq_len(b), function(j) tags$th(style="border:1px solid #e5e7eb; background:#f8fafc;", paste("Bloque", j))))

    tagList(
      p(em("Layout RCBD: columnas = bloques, filas = posiciones dentro del bloque.")),
      tags$table(class="table table-bordered text-center", style="width:100%; table-layout:fixed;",
        tags$thead(header),
        tags$tbody(filas)
      )
    )
  })

  output$rcbd_anova_tbl <- renderTable({
    res <- req(rcbd_res())
    res$aov %>%
      select(term, df, sumsq, meansq, statistic, p.value) %>%
      rename(`Fuente`=term, `gl`=df, `SC`=sumsq, `CM`=meansq, `F`=statistic, `p`=p.value)
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$rcbd_eta <- renderPrint({
    res <- req(rcbd_res()); print(res$eta)
  })

  output$rcbd_tukey_plot <- renderPlot({
    res <- req(rcbd_res())
    if (!is.null(res$tukey)) {
      plot(res$tukey, las = 1)
      abline(v = 0, lty = 2, col = "red")
    }
  })
  output$rcbd_tukey_text <- renderPrint({
    res <- req(rcbd_res())
    if (is.null(res$tukey)) {
      cat("Tratamientos no significativos en RCBD (p ≥ 0.05) — no se aplica Tukey.")
    } else {
      print(res$tukey)
    }
  })

  # ===================== PESTAÑA 5: EFICIENCIA =====================
  # Se construye tomando el último resultado de Heterogeneidad (DCA vs RCBD)
  output$eff_table <- renderTable({
    res <- req(het_res())
    tibble::tibble(
      Modelo = c("DCA (ignora bloques)", "RCBD (con bloques)"),
      `CM(Error)` = c(ms_error(res$m_dca), ms_error(res$m_rcbd))
    )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$eff_re <- renderText({
    res <- req(het_res())
    re <- as.numeric(ms_error(res$m_dca)) / as.numeric(ms_error(res$m_rcbd))
    sprintf("%.2f", re)
  })

  output$eff_msg <- renderText({
    res <- req(het_res())
    re <- as.numeric(ms_error(res$m_dca)) / as.numeric(ms_error(res$m_rcbd))
    if (is.finite(re) && re > 1.1) {
      paste0("¡Excelente! El RCBD fue ~", round((re - 1) * 100), "% más eficiente que el DCA.")
    } else if (is.finite(re) && re > 1) {
      "El bloqueo fue marginalmente útil."
    } else {
      "El bloqueo no aportó ganancia clara de precisión en esta simulación."
    }
  })

  # ===================== Helpers internos =====================
  fmtF <- function(tidy_aov_df) {
    row <- tidy_aov_df[ tidy_aov_df$term %in% c("tratamiento","group","treatments","trat"), ]
    if (!nrow(row)) return("—")
    sprintf("%.3f", row$statistic[1])
  }
  fmtp <- function(tidy_aov_df) {
    row <- tidy_aov_df[ tidy_aov_df$term %in% c("tratamiento","group","treatments","trat"), ]
    if (!nrow(row)) return("—")
    p <- row$p.value[1]; if (is.na(p)) return("—")
    if (p < 1e-4) "< 0.0001" else sprintf("%.4f", p)
  }
  ms_error <- function(model) {
    # extrae CM de residuos desde summary.aov
    an <- anova(model)
    as.numeric(an["Residuals","Mean Sq"])
  }
}
