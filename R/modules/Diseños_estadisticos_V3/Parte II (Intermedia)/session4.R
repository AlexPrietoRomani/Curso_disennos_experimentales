# R/modules/session4.R
# ------------------------------------------------------------------------------
# Sesión 4: De ANOVA a Modelos Lineales Mixtos (LMM)
# Módulo Shiny (UI + Server) para docencia y práctica aplicada en agronomía.
#
# Requisitos sugeridos: shiny, bslib, ggplot2, dplyr, tidyr, DT, glue,
#                       broom, broom.mixed, lme4, lmerTest, car
#
# Referencias clave (véase también la pestaña "Referencias" en la UI):
# - Yang (2010) resalta limitaciones de ANOVA/GLM y la necesidad de mixtos en agro. :contentReference[oaicite:0]{index=0}
# - Bates et al. (2015) lme4: estimación ML/REML para LMM en R. :contentReference[oaicite:1]{index=1}
# - Kuznetsova et al. (2017) lmerTest: F y p con Satterthwaite/Kenward-Roger. :contentReference[oaicite:2]{index=2}
# - Gbur et al. (2012) guía en GLMM/LMM para ciencias agrarias; robustez ante desbalance. :contentReference[oaicite:3]{index=3}
# - Hurlbert (1984) pseudorreplicación y no-independencia en campo. :contentReference[oaicite:4]{index=4}
# - car::leveneTest para homocedasticidad (Fox & Weisberg, manual del paquete). :contentReference[oaicite:5]{index=5}
# ------------------------------------------------------------------------------

session4_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 4: De ANOVA a Modelos Lineales Mixtos (LMM)")
    ),
    withMathJax(),
    bslib::navset_tab(
      # ====== PESTAÑA 1: CONTEXTO Y LIMITACIONES ======
      bslib::nav_panel(
        title = "1) Contexto y Limitaciones",
        tags$div(class = "lead",
          p("El ANOVA clásico (", code("aov()"), ") es una forma de GLM que funciona bien en diseños balanceados y con todos los efectos tratados como fijos. En campo, los ",
            strong("datos desbalanceados"), " y la presencia de ", strong("factores aleatorios"), " (bloque, año, sitio) son la norma — lo que motiva el uso de ",
            strong("Modelos Lineales Mixtos (LMM)"), "."),
          tags$ul(
            tags$li(HTML("Desbalance: en ANOVA, las Sumas de Cuadrados Tipo I dependen del <em>orden</em> de los términos; la inferencia puede cambiar con la misma data.")),
            tags$li("Factores de bloqueo como aleatorios: tratarlos como fijos produce SE y pruebas F inadecuadas cuando la intención es generalizar (bloques como muestra de la heterogeneidad)."),
            tags$li("LMM (REML) maneja desbalance y separa efectos fijos (tratamientos) de aleatorios (bloque/año/sitio).")
          ),
          tags$div(class="alert alert-info",
            strong("Idea central:"),
            " en RCBD, el contraste primario (tratamientos) suele ser similar entre ANOVA balanceado y LMM; la ventaja de LMM emerge con desbalance y para inferencia generalizable."
          ),
          tags$p(class="small",
                 "Lecturas recomendadas: Yang (2010), Bates et al. (2015), lmerTest (Kuznetsova et al., 2017), Gbur et al. (2012).")
        )
      ),

      # ====== PESTAÑA 2: FIJOS vs ALEATORIOS ======
      bslib::nav_panel(
        title = "2) Fijos vs Aleatorios",
        h4("Decisión conceptual: ¿qué quiero inferir?"),
        fluidRow(
          column(6,
            tags$div(class="card h-100",
              tags$div(class="card-body",
                h5("Efectos Fijos (Fixed Effects)"),
                tags$ul(
                  tags$li("Niveles específicos de interés (p. ej., 0, 50, 100 kg N/ha)."),
                  tags$li("Objetivo: estimar y comparar medias entre niveles."),
                  tags$li("En R: el término aparece fuera de paréntesis en la fórmula.")
                ),
                code("rend ~ tratamiento + (1|bloque)")
              )
            )
          ),
          column(6,
            tags$div(class="card h-100",
              tags$div(class="card-body",
                h5("Efectos Aleatorios (Random Effects)"),
                tags$ul(
                  tags$li("Niveles como muestra de una población mayor (bloque, año, sitio)."),
                  tags$li("Objetivo: cuantificar varianza entre niveles (heterogeneidad)."),
                  tags$li("En R: ", code("(1 | factor_aleatorio)"), " agrega intercepto aleatorio.")
                ),
                code("rend ~ tratamiento + (1|bloque)")
              )
            )
          )
        ),
        hr(),
        h5("Matriz de decisión (resumen)"),
        tags$table(class="table table-sm table-bordered",
          tags$thead(tags$tr(
            tags$th("Característica"), tags$th("Fijos"), tags$th("Aleatorios")
          )),
          tags$tbody(
            tags$tr(tags$td("Inferencia"),
                    tags$td("Entre niveles específicos"),
                    tags$td("A población de niveles")),
            tags$tr(tags$td("Ejemplos agro"),
                    tags$td("Dosis, variedad puntual, labranza"),
                    tags$td("Bloque, año, localidad, potrero")),
            tags$tr(tags$td("Sintaxis"),
                    tags$td(code("x")), tags$td(code("(1|g)"))),
            tags$tr(tags$td("Pregunta"),
                    tags$td("¿A ≠ B?"),
                    tags$td("¿Cuánta varianza aporta g?"))
          )
        )
      ),

      # ====== PESTAÑA 3: RCBD — aov() vs lmer() ======
      bslib::nav_panel(
        title = "3) RCBD: aov() vs lmer()",
        sidebarLayout(
          sidebarPanel(width = 4,
            tags$h5("Simulador RCBD (rend ~ tratamiento + bloque)"),
            numericInput(ns("n_trt"), "N tratamientos", value = 3, min = 2, max = 10),
            numericInput(ns("n_blk"), "N bloques", value = 4, min = 2, max = 20),
            numericInput(ns("mu_base"), "Media base (μ)", value = 100, min = -1e4, max = 1e4),
            sliderInput(ns("efecto_lineal"),
                        "Efecto lineal de tratamiento (incremento por nivel)", min = -50, max = 50, value = 10, step = 1),
            sliderInput(ns("sd_blk"), "SD bloque (σ_block)", min = 0, max = 50, value = 12, step = 1),
            sliderInput(ns("sd_res"), "SD residual (σ_res)", min = 1, max = 60, value = 15, step = 1),
            sliderInput(ns("perc_missing"), "% celdas perdidas (desbalance)", min = 0, max = 40, value = 0, step = 1),
            checkboxInput(ns("aov_trt_first"),
                          "ANOVA (Tipo I) con orden 'tratamiento + bloque' (si NO, 'bloque + tratamiento')",
                          value = TRUE),
            actionButton(ns("run_sim"), "Generar datos y ajustar modelos",
                         class = "btn btn-primary w-100 mt-2")
          ),
          mainPanel(width = 8,
            tags$div(class="alert alert-secondary",
              p(strong("Objetivo:"),
                " comparar la sensibilidad de ANOVA Tipo I (", code("aov()"), ") al orden de términos bajo desbalance vs la robustez de LMM (",
                code("lmer()"), ", REML) con ", code("(1|bloque)"), ".")
            ),
            h5("Datos simulados"),
            DT::dataTableOutput(ns("tab_datos")),
            hr(),
            h5("ANOVA clásico (aov) — Tipo I (secuencial)"),
            fluidRow(
              column(6,
                tags$h6("Orden:", textOutput(ns("lbl_aov1"), inline = TRUE)),
                verbatimTextOutput(ns("out_aov1"))
              ),
              column(6,
                tags$h6("Orden inverso:", textOutput(ns("lbl_aov2"), inline = TRUE)),
                verbatimTextOutput(ns("out_aov2"))
              )
            ),
            hr(),
            h5("Modelo lineal mixto (lmerTest::lmer)"),
            tags$p("Tabla ANOVA (Tipo III) con ddf = Satterthwaite; los p-valores no dependen del orden."),
            verbatimTextOutput(ns("out_lmer_anova")),
            fluidRow(
              column(6,
                h6("Componentes de varianza (VarCorr)"),
                verbatimTextOutput(ns("out_varcorr"))
              ),
              column(6,
                h6("ICC = σ²_bloque / (σ²_bloque + σ²_residual)"),
                verbatimTextOutput(ns("out_icc"))
              )
            ),
            hr(),
            h5("Diagnósticos (LMM)"),
            fluidRow(
              column(6, plotOutput(ns("plot_resid_fitted"), height = "250px")),
              column(6, plotOutput(ns("plot_qq"), height = "250px"))
            ),
            hr(),
            h5("BLUPs de bloque (interceptos aleatorios)"),
            plotOutput(ns("plot_ranef"), height = "280px")
          )
        )
      ),

      # ====== PESTAÑA 4: EJERCICIOS PRÁCTICOS ======
      bslib::nav_panel(
        title = "4) Ejercicios prácticos",
        tags$ol(
          tags$li("Genera un RCBD con 5 tratamientos y 6 bloques; fija σ_block = 10 y σ_res = 12; sin desbalance."),
          tags$li("Repite con 20% de celdas perdidas. Compara p(tratamiento) en:", code("aov"), " (dos órdenes) vs ", code("lmer"), "."),
          tags$li("Calcula VarCorr y el ICC. Interpreta: ¿el bloqueo fue relevante?"),
          tags$li("Aplica ", code("car::leveneTest(rend ~ tratamiento)"), " a los residuales del modelo lineal (sin bloque) y comenta homocedasticidad."),
          tags$li("Guarda el dataset generado y reprodúcelo en un documento Quarto usando el código guía.")
        ),
        hr(),
        downloadButton(ns("dl_csv"), "Descargar datos simulados (CSV)", class = "btn btn-success"),
        tags$h5("Plantilla Quarto (copia y pega)"),
        pre(class="r-code", style="white-space: pre-wrap;",
          HTML(
'```{r}
library(dplyr); library(ggplot2); library(lme4); library(lmerTest); library(car)
# 1) Explorar
d <- read.csv("datos_rcbd.csv")
str(d); d %>% count(tratamiento, bloque)

# 2) ANOVA (Tipo I, dependiente del orden cuando hay desbalance)
mA <- aov(rend ~ tratamiento + bloque, data = d)
mB <- aov(rend ~ bloque + tratamiento, data = d)
summary(mA); summary(mB)

# 3) LMM (bloque aleatorio)
mL <- lmer(rend ~ tratamiento + (1|bloque), data = d, REML = TRUE)
anova(mL, type = 3, ddf = "Satterthwaite")

# 4) VarCorr e ICC
vc <- as.data.frame(VarCorr(mL))
sig_b <- vc$vcov[vc$grp == "bloque" & vc$var1 == "(Intercept)"]
sig_e <- vc$vcov[vc$grp == "Residual"]
ICC <- sig_b / (sig_b + sig_e); ICC

# 5) Diagnósticos
par(mfrow=c(1,2))
plot(fitted(mL), resid(mL), xlab="Fitted", ylab="Residuals"); abline(h=0, lty=2, col=2)
qqnorm(resid(mL)); qqline(resid(mL), col=2)

# 6) Levene (sobre un lm simple, a modo pedagógico)
car::leveneTest(rend ~ tratamiento, data = d)
```'
          )
        )
      ),

      # ====== PESTAÑA 5: Visualización & Notas ======
      bslib::nav_panel(
        title = "5) Visualización & Notas",
        tags$ul(
          tags$li("AIC: no comparar AIC de ", code("aov/lm"), " con ", code("lmer(REML=TRUE)"), ". Para comparar estructuras de efectos fijos en LMM, refit con ", code("REML=FALSE"), " si fuese estrictamente necesario."),
          tags$li("Satterthwaite/Kenward-Roger: aproximaciones para d.f. en mixtos; implementadas en ", code("lmerTest"), "."),
          tags$li("Pseudorreplicación: un ICC > 0 implica correlación intra-bloque; ignorarla (GLM simple) puede inflar falsos positivos.")
        ),
        tags$div(class="alert alert-warning",
          strong("Regla práctica:"),
          " si hay bloqueo/año/sitio y/o desbalance, prefiera ", code("lmer()"),
          " con especificación correcta de aleatorios; reporte VarCorr e ICC junto con la prueba de tratamientos."
        )
      ),

      # ====== PESTAÑA 6: Referencias ======
      bslib::nav_panel(
        title = "Referencias",
        tags$ul(
          tags$li(
            "Yang, R.-C. (2010). Towards understanding and use of mixed-model analysis of agricultural experiments. ",
            em("Canadian Journal of Plant Science"), " 90(5), 605–627. ",
            a(href="https://cdnsciencepub.com/doi/10.4141/CJPS10049", "doi:10.4141/CJPS10049", target="_blank")
          ),
          tags$li(
            "Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. ",
            em("Journal of Statistical Software, 67(1)"), ". ",
            a(href="https://www.jstatsoft.org/v67/i01/", "JSS", target="_blank")
          ),
          tags$li(
            "Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest Package: Tests in Linear Mixed Effects Models. ",
            em("Journal of Statistical Software, 82(13)"), ". ",
            a(href="https://www.jstatsoft.org/v82/i13/", "JSS", target="_blank")
          ),
          tags$li(
            "Gbur, E. E., Stroup, W. W., et al. (2012). ",
            em("Analysis of Generalized Linear Mixed Models in the Agricultural and Natural Resources Sciences"),
            ". ASA/SSSA/CSSA. ",
            a(href="https://www.ars.usda.gov/ARSUserFiles/3122/GburEtAl2012.pdf", "PDF", target="_blank")
          ),
          tags$li(
            "Hurlbert, S. (1984). Pseudoreplication and the Design of Ecological Field Experiments. ",
            em("Ecological Monographs, 54(2)"), " 187–211. ",
            a(href="https://www.sfu.ca/biology/faculty/M%27Gonigle/materials-qm/papers/hurlbert-1984-187.pdf", "PDF", target="_blank")
          ),
          tags$li(
            "Fox, J., & Weisberg, S. (Manual del paquete ", code("car"), "). ",
            a(href="https://cran.r-project.org/package=car/car.pdf", "car.pdf", target="_blank")
          )
        ),
        tags$hr(),
        tags$p(class="small text-muted",
               "Esta sesión enfatiza el uso de modelos mixtos para robustez ante desbalance y correcta cuantificación de varianza de bloqueo, acorde a la literatura citada.")
      )
    )
  )
}

# ----------------------------- SERVER -----------------------------------------
session4_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Utilidad: simulación RCBD con 1 réplica por (bloque x tratamiento)
  sim_rcbd <- function(n_trt, n_blk, mu, eff_lin, sd_blk, sd_res, perc_missing = 0) {
    stopifnot(n_trt >= 2, n_blk >= 2)
    # Tratamientos con efecto lineal (pedagógico); centrado para mantener media interpretable
    trt_levels <- factor(paste0("T", seq_len(n_trt)), levels = paste0("T", seq_len(n_trt)))
    blk_levels <- factor(paste0("B", seq_len(n_blk)), levels = paste0("B", seq_len(n_blk)))

    grid <- tidyr::expand_grid(tratamiento = trt_levels, bloque = blk_levels)
    # Efectos fijos de tratamiento: 0, eff, 2*eff, ...
    trt_idx <- as.integer(grid$tratamiento)
    eff_trt <- (trt_idx - 1) * eff_lin

    # Efectos aleatorios de bloque ~ N(0, sd_blk^2)
    set.seed(123)
    b_eff <- rnorm(n_blk, mean = 0, sd = sd_blk)
    blk_map <- setNames(b_eff, levels(blk_levels))
    # Residual
    e <- rnorm(nrow(grid), mean = 0, sd = sd_res)

    rend <- mu + eff_trt + blk_map[as.character(grid$bloque)] + e
    d <- dplyr::mutate(grid, rend = as.numeric(rend))

    # Introducir desbalance eliminando filas al azar
    if (perc_missing > 0) {
      n_drop <- floor(nrow(d) * perc_missing / 100)
      if (n_drop > 0 && n_drop < nrow(d)) {
        drop_idx <- sample(seq_len(nrow(d)), size = n_drop, replace = FALSE)
        d <- d[-drop_idx, , drop = FALSE]
      }
    }
    d
  }

  # Reactivo: datos simulados
  datos_rcbd <- eventReactive(input$run_sim, {
    sim_rcbd(
      n_trt = input$n_trt,
      n_blk = input$n_blk,
      mu    = input$mu_base,
      eff_lin = input$efecto_lineal,
      sd_blk  = input$sd_blk,
      sd_res  = input$sd_res,
      perc_missing = input$perc_missing
    )
  })

  output$tab_datos <- DT::renderDataTable({
    req(datos_rcbd())
    DT::datatable(datos_rcbd(), options = list(pageLength = 8, scrollX = TRUE), rownames = FALSE)
  })

  # ANOVA (Tipo I) con dos órdenes
  output$lbl_aov1 <- renderText({
    if (isTRUE(input$aov_trt_first)) "rend ~ tratamiento + bloque" else "rend ~ bloque + tratamiento"
  })
  output$lbl_aov2 <- renderText({
    if (isTRUE(input$aov_trt_first)) "rend ~ bloque + tratamiento" else "rend ~ tratamiento + bloque"
  })

  ajuste_aov1 <- reactive({
    req(datos_rcbd())
    f1 <- if (isTRUE(input$aov_trt_first)) as.formula(rend ~ tratamiento + bloque) else as.formula(rend ~ bloque + tratamiento)
    stats::aov(f1, data = datos_rcbd())
  })

  ajuste_aov2 <- reactive({
    req(datos_rcbd())
    f2 <- if (isTRUE(input$aov_trt_first)) as.formula(rend ~ bloque + tratamiento) else as.formula(rend ~ tratamiento + bloque)
    stats::aov(f2, data = datos_rcbd())
  })

  output$out_aov1 <- renderPrint({ req(ajuste_aov1()); summary(ajuste_aov1()) })
  output$out_aov2 <- renderPrint({ req(ajuste_aov2()); summary(ajuste_aov2()) })

  # LMM: lmer con bloque aleatorio; ANOVA tipo III con Satterthwaite
  ajuste_lmer <- reactive({
    req(datos_rcbd())
    lmerTest::lmer(rend ~ tratamiento + (1 | bloque), data = datos_rcbd(), REML = TRUE)
  })

  output$out_lmer_anova <- renderPrint({
    req(ajuste_lmer())
    lmerTest::anova(ajuste_lmer(), type = 3, ddf = "Satterthwaite")
  })

  # VarCorr & ICC
  output$out_varcorr <- renderPrint({
    req(ajuste_lmer())
    print(lme4::VarCorr(ajuste_lmer()), comp = c("Variance", "Std.Dev."))
  })

  output$out_icc <- renderPrint({
    req(ajuste_lmer())
    vc <- as.data.frame(lme4::VarCorr(ajuste_lmer()))
    sig_b <- vc$vcov[vc$grp == "bloque" & vc$var1 == "(Intercept)"]
    sig_e <- vc$vcov[vc$grp == "Residual"]
    ICC <- sig_b / (sig_b + sig_e)
    cat(sprintf("ICC = %.3f (%.1f%% de la varianza total atribuible a diferencias entre bloques)\n",
                ICC, 100*ICC))
  })

  # Diagnósticos (sobre el LMM)
  output$plot_resid_fitted <- renderPlot({
    req(ajuste_lmer())
    df <- data.frame(fitted = fitted(ajuste_lmer()),
                     resid  = resid(ajuste_lmer()))
    ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(x = "Fitted", y = "Residuals", title = "Residuals vs Fitted (LMM)") +
      ggplot2::theme_bw()
  })

  output$plot_qq <- renderPlot({
    req(ajuste_lmer())
    df <- data.frame(resid = resid(ajuste_lmer()))
    ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line(color = "red") +
      ggplot2::labs(title = "Normal Q-Q (residuales del LMM)") +
      ggplot2::theme_bw()
  })

  # BLUPs de bloque
  output$plot_ranef <- renderPlot({
    req(ajuste_lmer())
    re <- lme4::ranef(ajuste_lmer(), condVar = TRUE)
    rb <- as.data.frame(re$bloque)
    colnames(rb) <- c("ranef", "bloque")
    ggplot2::ggplot(rb, ggplot2::aes(x = reorder(bloque, ranef), y = ranef)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_point(size = 3) +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Bloque", y = "Intercepto aleatorio (BLUP)",
                    title = "BLUPs por bloque") +
      ggplot2::theme_bw()
  })

  # Descarga de datos simulados
  output$dl_csv <- downloadHandler(
    filename = function() {
      paste0("datos_rcbd_nT", input$n_trt, "_nB", input$n_blk,
             "_miss", input$perc_missing, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(datos_rcbd())
      utils::write.csv(datos_rcbd(), file, row.names = FALSE)
    }
  )
}
