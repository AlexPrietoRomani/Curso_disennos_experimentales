# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session5.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & Contexto
pestanna1_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & Contexto",
    h4(class = "section-header", "Objetivo y mapa de la sesión"),
    tags$ul(
      tags$li(strong("Contexto:"), " factores 'difíciles' → parcelas principales; factores 'fáciles' → subparcelas. Aleatorización restringida → dos (o más) errores."),
      tags$li(strong("Meta de aprendizaje:"), " construir el ", code("LMM"), " correcto y evitar inflar el Error Tipo I para el factor de parcela principal."),
      tags$li(strong("Entrega:"), " script reproducible con comparación ", code("aov()"), " vs ", code("lmer()"), ", componentes de varianza y post-hoc ", code("emmeans()"), ".")
    ),
    div(class = "alert alert-info",
        tags$p(
          "Diseños split-plot son ubicuos en campo y a menudo mal analizados como si fueran DBCA simples. Modelar explícitamente ",
          em("Bloque"), " y la ", em("Parcela Principal"), " como efectos aleatorios es la ruta robusta. (Yang, 2010; Bates et al., 2015)."
        )
    ),
    tags$hr(),
    h4("Ejemplo motivador (Riego × Nitrógeno)"),
    tags$ol(
      tags$li("Asignar ", strong("Riego (A)"), " a parcelas principales grandes por bloque."),
      tags$li("Subdividir cada parcela principal y asignar ", strong("Nitrógeno (B)"), " a subparcelas."),
      tags$li("Resultado: ", strong("Error A"), " (entre parcelas principales) y ", strong("Error B"), " (entre subparcelas).")
    ),
    tags$p(class = "text-muted small",
           "Notas de apoyo: ETHZ split-plot; transición a mixtos en R."), 
    tags$div(class = "text-muted small",
             HTML("Refs: Yang (2010), Bates et&nbsp;al. (2015), lmerTest JSS, emmeans CRAN."))
  )
}

# Pestaña 2: Conceptos clave
pestanna2_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave",
    h4(class = "section-header", "Dos unidades experimentales → dos errores"),
    tags$ul(
      tags$li(strong("UE A (Parcela principal):"), " unidad a la que se aplica el factor principal (p.ej., Riego). ",
              strong("Error A:"), " varianza entre parcelas principales dentro de bloque."),
      tags$li(strong("UE B (Subparcela):"), " unidad para el factor secundario (p.ej., Nitrógeno). ",
              strong("Error B:"), " varianza entre subparcelas dentro de parcela principal.")
    ),
    tags$div(class = "row",
             tags$div(class = "col-md-7",
                      h5("Matriz de denominadores F (pedagógica)"),
                      tableOutput(ns("tabla_denominadores_F"))),
             tags$div(class = "col-md-5",
                      div(class = "alert alert-warning",
                          tags$strong("Error GLM clásico:"),
                          p("Usar un único 'Residual' como denominador para todo → F de A (parcela principal) mal calculada (denominador demasiado pequeño).",
                            "Esto ", strong("infla el Tipo I"), ". LMM corrige al proveer ", em("Error A"), " y ",
                            em("Error B"), " separados. (Yang, 2010).")
                      ))
    ),
    hr(),
    h4("Espacio de inferencia y naturaleza del efecto"),
    tags$ul(
      tags$li(strong("Efectos fijos:"), " niveles específicos de interés (tratamientos)."),
      tags$li(strong("Efectos aleatorios:"), " niveles como muestra de una población (bloque, parcela principal).")
    ),
    p(class="text-muted small","Apoyo: Yang (2010); Stroup (GLMM, 2ª ed.).")
  )
}

# Pestaña 3: R: GLM ↔ LMM
pestanna3_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) R: GLM ↔ LMM",
    h4(class = "section-header", "Comparativo pedagógico de enfoques"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Parámetros de simulación"),
        selectInput(ns("n_bloques"), "Bloques (réplicas de A):", choices = c(3,4,5,6), selected = 4),
        selectInput(ns("niv_A"), "Niveles de Riego (A, parcela principal):", choices = c(2,3), selected = 3),
        selectInput(ns("niv_B"), "Niveles de Nitrógeno (B, subparcela):", choices = c(3,4), selected = 4),
        sliderInput(ns("eff_A"), "Efecto A (max – min):",  min = 0, max = 10, value = 4, step = 0.5),
        sliderInput(ns("eff_B"), "Efecto B (max – min):",  min = 0, max = 12, value = 6, step = 0.5),
        sliderInput(ns("eff_AB"), "Interacción A×B (rango):", min = -6, max = 6, value = 2, step = 0.5),
        hr(),
        h6("Componentes de error (desviaciones estándar)"),
        sliderInput(ns("sd_A"), "σ_A (Error parcela principal):", min = 0, max = 10, value = 4, step = 0.5),
        sliderInput(ns("sd_B"), "σ_B (Error subparcela / residual):", min = 0, max = 10, value = 2, step = 0.5),
        hr(),
        h6("Desbalance (faltantes)"),
        sliderInput(ns("prop_miss"), "Proporción de subparcelas faltantes (MCAR):", min = 0, max = 0.3, value = 0.0, step = 0.02),
        actionButton(ns("simular_fit"), "Simular & Ajustar", icon = icon("play"), class = "btn-primary w-100")
      ),
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          bslib::nav_panel(
            "3.1 Resultados",
            fluidRow(
              column(6,
                     h6("GLM simple (incorrecto)"),
                     verbatimTextOutput(ns("glm_bad_anova"))),
              column(6,
                     h6("ANOVA estratificado (puente): aov(Error(Bloque/ParcelaA))"),
                     verbatimTextOutput(ns("aov_strata")))
            ),
            hr(),
            h6("LMM correcto: lmer(y ~ A*B + (1|Bloque) + (1|Bloque:ParcelaA))"),
            verbatimTextOutput(ns("lmm_anova")),
            hr(),
            fluidRow(
              column(6,
                     h6("Componentes de varianza (VarCorr)"),
                     verbatimTextOutput(ns("lmm_varcorr"))),
              column(6,
                     h6("ICCs por nivel"),
                     tableOutput(ns("tabla_icc")))
            )
          ),
          bslib::nav_panel(
            "3.2 Visualización",
            plotOutput(ns("plot_means"), height = "340px"),
            plotOutput(ns("plot_layout"), height = "340px"),
            div(class="text-muted small",
                "El plano de layout es ilustrativo (bloques × parcelas principales × subparcelas).")
          ),
          bslib::nav_panel(
            "3.3 Post-hoc (emmeans)",
            p("Comparaciones de N (B) dentro de cada Riego (A): ", code("pairwise ~ B | A")),
            verbatimTextOutput(ns("emm_pairwise")),
            hr(),
            p("EMMs y gráfico de interacción estimado:"),
            plotOutput(ns("emm_plot"), height = "340px")
          )
        )
      )
    )
  )
}

# Pestaña 4: Extensión: Split-Split
pestanna4_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Extensión: Split-Split",
    h4("De split-plot a split-split-plot"),
    p("Estructura de anidación: ", code("Bloque / A (parcela) / A:B (subparcela) / A:B:C (sub-subparcela)")),
    pre(class="r-code",
        HTML(
          "# Ejemplo de fórmula (extensión):\n",
          "lmer(y ~ A*B*C + (1|Bloque) + (1|Bloque:A) + (1|Bloque:A:B), data = datos)\n"
        )
    ),
    p("El término ", code("(1|Bloque:A)"), " captura el ", strong("Error A"),
      "; ", code("(1|Bloque:A:B)"), " captura el ", strong("Error B"),
      "; el residuo modela el ", strong("Error C"), "."),
    div(class="alert alert-secondary",
        "Consejo: mantén fijo solo lo científico (tratamientos) y aleatorio lo logístico/jerárquico.",
        " Usa ", code("VarCorr()"), " para decidir dónde mejora la precisión si rediseñas el ensayo.")
  )
}

# Pestaña 5: Ejercicios prácticos
pestanna5_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Ejercicios prácticos",
    h4("Hands-on: construye y compara tu propio split-plot"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h6("Diseño"),
        numericInput(ns("ex_nb"), "Bloques:", 4, min = 2, max = 12, step = 1),
        numericInput(ns("ex_na"), "Niveles A (Riego):", 3, min = 2, max = 6, step = 1),
        numericInput(ns("ex_nbsub"), "Niveles B (N):", 4, min = 2, max = 8, step = 1),
        h6("Magnitudes de efecto (en unidades de respuesta)"),
        numericInput(ns("ex_effA"), "Rango efecto A:", 4, min = 0, max = 20, step = .5),
        numericInput(ns("ex_effB"), "Rango efecto B:", 6, min = 0, max = 20, step = .5),
        numericInput(ns("ex_effAB"), "Rango interacción:", 2, min = -20, max = 20, step = .5),
        h6("Errores (DE)"),
        numericInput(ns("ex_sdA"), "σ_A:", 4, min = 0, max = 20, step = .5),
        numericInput(ns("ex_sdB"), "σ_B:", 2, min = 0, max = 20, step = .5),
        h6("Faltantes"),
        sliderInput(ns("ex_miss"), "% faltantes:", min = 0, max = 30, value = 0, step = 2),
        hr(),
        actionButton(ns("ex_run"), "Generar & Ajustar", icon = icon("play"), class = "btn-success w-100"),
        br(), br(),
        uiOutput(ns("dl_ex_csv_ui"))
      ),
      mainPanel(
        width = 8,
        tabsetPanel(
          tabPanel("Datos",
                   DT::dataTableOutput(ns("ex_tabla")),
                   plotOutput(ns("ex_layout"), height = "280px")),
          tabPanel("Modelos",
                   fluidRow(
                     column(6, h6("GLM simple (incorrecto)"), verbatimTextOutput(ns("ex_glm_bad"))),
                     column(6, h6("LMM correcto"), verbatimTextOutput(ns("ex_lmm_ok")))
                   ),
                   hr(),
                   fluidRow(
                     column(6, h6("VarCorr (LMM)"), verbatimTextOutput(ns("ex_varcorr"))),
                     column(6, h6("ICC por nivel"), tableOutput(ns("ex_icc_tbl")))
                   )
          ),
          tabPanel("Post-hoc",
                   p("Comparaciones de B dentro de cada A: ", code("pairwise ~ B | A")),
                   verbatimTextOutput(ns("ex_emm")),
                   plotOutput(ns("ex_emm_plot"), height = "280px"))
        )
      )
    )
  )
}

# Pestaña 6: Referencias
pestanna6_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("Yang, R.-C. (2010). ",
              em("Towards understanding and use of mixed-model analysis of agricultural experiments."),
              " Canadian Journal of Plant Science, 90(5), 605–627. ",
              tags$a(href="https://doi.org/10.4141/CJPS10049", "https://doi.org/10.4141/CJPS10049")),
      tags$li("Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). ",
              em("Fitting linear mixed-effects models using lme4."),
              " Journal of Statistical Software, 67(1), 1–48. ",
              tags$a(href="https://www.jstatsoft.org/article/view/v067i01/946", "JSS link")),
      tags$li("Kuznetsova, A., Brockhoff, P.B., & Christensen, R.H.B. (2017). ",
              em("lmerTest package: Tests in linear mixed effects models."),
              " Journal of Statistical Software, 82(13). ",
              tags$a(href="https://backend.orbit.dtu.dk/ws/portalfiles/portal/140635100/lmerTestJStatSoft2017.pdf", "JSS link")),
      tags$li("Lenth, R. V. (2025). ",
              em("emmeans: Estimated marginal means."),
              " CRAN manual. ",
              tags$a(href="https://cran.r-project.org/package=emmeans", "CRAN: emmeans")),
      tags$li("ETH Zürich (s.f.). ",
              em("Split-Plot Designs – ANOVA and Mixed Models."),
              " ",
              tags$a(href="https://people.math.ethz.ch/~meier/teaching/anova/split-plot-designs.html", "course notes")),
      tags$li("Stats4SD (s.f.). ",
              em("Chapter 4: Split-plot designs (R)."),
              " ",
              tags$a(href="https://shiny.stats4sd.org/AgAnalysis/split-plot-designs.html", "resource"))
    ),
    p(class="text-muted small",
      "Estas referencias respaldan las decisiones de modelado, el uso de REML/Satterthwaite y las prácticas de post-hoc con EMMs.")
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session5_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 5: Diseños de Parcelas Divididas y Subdivididas con LMM")
    ),
    navset_tab(
      pestanna1_session5_v3UI(ns),
      pestanna2_session5_v3UI(ns),
      pestanna3_session5_v3UI(ns),
      pestanna4_session5_v3UI(ns),
      pestanna5_session5_v3UI(ns),
      pestanna6_session5_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

pestanna1_session5_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna2_session5_v3_server <- function(input, output, session) {
  output$tabla_denominadores_F <- renderTable({
    data.frame(
      `Fuente` = c("Bloque", "Parcela principal (A)", "Bloque×A (Error A)", "Subparcela (B)", "A×B", "Residual (Error B)"),
      `GLM simple (incorrecto)` = c("—", "CM(Residual)", "—", "CM(Residual)", "CM(Residual)", "CM(Residual)"),
      `aov(Error(Bloque/A))`    = c("—", "CM(Bloque:A)",  "—", "CM(Residual)", "CM(Residual)", "CM(Residual)"),
      `LMM (lmer)`              = c("aleatorio", "usa Var(Error A)", "aleatorio", "usa Var(Error B)", "usa Var(Error B)", "residual"),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, align = "c")
}

pestanna3_session5_v3_server <- function(input, output, session, sim_react, icc_from_varcorr) {
  output$glm_bad_anova <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_glm <- aov(y ~ Bloque + A*B, data = df)
    summary(mod_glm)
  })

  output$aov_strata <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_sp <- aov(y ~ A*B + Error(Bloque/ParcelaA), data = df)
    summary(mod_sp)
  })

  output$lmm_anova <- renderPrint({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("lmerTest", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    anova(mod_lmm, type = 3)
  })

  output$lmm_varcorr <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    print(lme4::VarCorr(mod_lmm), comp = c("Variance","Std.Dev."))
  })

  output$tabla_icc <- renderTable({
    df <- sim_react(); shiny::req(df)
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    icc_from_varcorr(lme4::VarCorr(mod_lmm))
  }, striped = TRUE, bordered = TRUE, align = "c")

  output$plot_means <- renderPlot({
    df <- sim_react(); shiny::req(df)
    library(ggplot2)
    ggplot(df, aes(B, y, color = A, group = A)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1) +
      stat_summary(fun = mean, geom = "point", size = 2) +
      labs(title = "Medias observadas por combinación A×B",
           x = "B (Nitrógeno)", y = "Respuesta media", color = "A (Riego)") +
      theme_bw()
  })

  output$plot_layout <- renderPlot({
    df <- sim_react(); shiny::req(df)
    library(ggplot2)
    df$Row <- as.numeric(df$Bloque)
    df$Col <- as.numeric(interaction(df$A, df$B, drop = TRUE))
    ggplot(df, aes(Col, Row, fill = A)) +
      geom_tile(color = "white") +
      geom_text(aes(label = B), size = 3) +
      scale_y_reverse(breaks = sort(unique(df$Row))) +
      labs(title = "Croquis ilustrativo (Bloque × ParcelaA × Subparcelas)",
           x = "A:B", y = "Bloque") +
      theme_minimal() +
      coord_fixed()
  })

  output$emm_pairwise <- renderPrint({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("emmeans", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    print(emmeans::emmeans(mod_lmm, pairwise ~ B | A))
  })

  output$emm_plot <- renderPlot({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("emmeans", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    em <- emmeans::emmeans(mod_lmm, ~ A*B)
    plot(em, comparisons = TRUE)
  })
}

pestanna4_session5_v3_server <- function(input, output, session) {
  # No server logic needed
}

pestanna5_session5_v3_server <- function(input, output, session, ex_df, icc_from_varcorr) {
  output$ex_tabla <- DT::renderDataTable({
    df <- ex_df(); shiny::req(df)
    DT::datatable(df, options = list(pageLength = 10), rownames = FALSE)
  })

  output$ex_layout <- renderPlot({
    df <- ex_df(); shiny::req(df)
    library(ggplot2)
    df$Row <- as.numeric(df$Bloque)
    df$Col <- as.numeric(interaction(df$A, df$B, drop = TRUE))
    ggplot(df, aes(Col, Row, fill = A)) +
      geom_tile(color = "white") +
      geom_text(aes(label = B), size = 3) +
      scale_y_reverse() +
      labs(title = "Croquis del ejercicio", x = "A:B", y = "Bloque") +
      theme_minimal() + coord_fixed()
  })

  output$ex_glm_bad <- renderPrint({
    df <- ex_df(); shiny::req(df)
    summary(aov(y ~ Bloque + A*B, data = df))
  })

  output$ex_lmm_ok <- renderPrint({
    df <- ex_df(); shiny::req(df)
    mod <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    anova(mod, type = 3)
  })

  output$ex_varcorr <- renderPrint({
    df <- ex_df(); shiny::req(df)
    mod <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    print(lme4::VarCorr(mod), comp = c("Variance","Std.Dev."))
  })

  output$ex_icc_tbl <- renderTable({
    df <- ex_df(); shiny::req(df)
    mod <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    icc_from_varcorr(lme4::VarCorr(mod))
  }, striped = TRUE, bordered = TRUE, align = "c")

  output$ex_emm <- renderPrint({
    df <- ex_df(); shiny::req(df)
    mod <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    print(emmeans::emmeans(mod, pairwise ~ B | A))
  })

  output$ex_emm_plot <- renderPlot({
    df <- ex_df(); shiny::req(df)
    mod <- lmerTest::lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = df, REML = TRUE)
    em <- emmeans::emmeans(mod, ~ A*B)
    plot(em, comparisons = TRUE)
  })

  output$dl_ex_csv_ui <- renderUI({
    shiny::req(ex_df())
    downloadButton(session$ns("dl_ex_csv"), "Descargar datos (CSV)", class = "btn-outline-secondary w-100")
  })
  output$dl_ex_csv <- downloadHandler(
    filename = function() {
      paste0("splitplot_ejercicio_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(ex_df(), file, row.names = FALSE)
    }
  )
}

pestanna6_session5_v3_server <- function(input, output, session) {
  # No server logic needed
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session5_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  sim_splitplot <- function(nB = 4, nA = 3, nBsub = 4,
                            effA = 4, effB = 6, effAB = 2,
                            sdA = 4, sdB = 2,
                            prop_miss = 0) {
    set.seed(1234)
    Bloque <- factor(paste0("B", seq_len(nB)))
    A <- factor(paste0("A", seq_len(nA)))
    B <- factor(paste0("N", seq_len(nBsub)))

    a_seq <- seq(0, 1, length.out = nA) - 0.5
    b_seq <- seq(0, 1, length.out = nBsub) - 0.5

    df <- expand.grid(Bloque = Bloque, A = A, B = B, KEEP.OUT.ATTRS = FALSE)
    df <- df[order(df$Bloque, df$A, df$B), , drop = FALSE]
    df$ParcelaA <- interaction(df$Bloque, df$A, drop = TRUE)

    mu <- 50
    a_map <- setNames(a_seq / max(1, abs(max(a_seq))), levels(A))
    b_map <- setNames(b_seq / max(1, abs(max(b_seq))), levels(B))

    df$fx_A  <- a_map[as.character(df$A)] * effA
    df$fx_B  <- b_map[as.character(df$B)] * effB
    df$fx_AB <- (a_map[as.character(df$A)] * b_map[as.character(df$B)]) * effAB

    u_Bloque   <- rnorm(nB, 0, sd = 0)
    names(u_Bloque) <- levels(Bloque)
    u_ParcelaA <- rnorm(length(levels(df$ParcelaA)), 0, sd = sdA)
    names(u_ParcelaA) <- levels(df$ParcelaA)

    df$y <- mu +
      df$fx_A + df$fx_B + df$fx_AB +
      u_Bloque[as.character(df$Bloque)] +
      u_ParcelaA[as.character(df$ParcelaA)] +
      rnorm(nrow(df), 0, sd = sdB)

    if (prop_miss > 0) {
      n_miss <- floor(nrow(df) * prop_miss)
      idx <- sample(seq_len(nrow(df)), n_miss)
      df$y[idx] <- NA
      df <- df[!is.na(df$y), , drop = FALSE]
    }
    rownames(df) <- NULL
    df
  }

  icc_from_varcorr <- function(vlist) {
    vc <- as.data.frame(vlist)
    get_var <- function(grp) {
      x <- vc[vc$grp == grp & vc$var1 == "(Intercept)", "vcov"]
      if (length(x) == 0) 0 else x[1]
    }
    v_blockA <- get_var("Bloque:A")
    v_block  <- get_var("Bloque")
    v_resid  <- get_var("Residual")
    v_tot_PA <- v_blockA + v_resid
    v_tot_all <- v_block + v_blockA + v_resid

    data.frame(
      Nivel = c("ICC ParcelaA (entre subparcelas misma parcelaA)",
                "ICC Bloque (entre parcelasA mismo bloque)"),
      Formula = c("σ²_ParcelaA / (σ²_ParcelaA + σ²_Residual)",
                  "(σ²_Bloque + σ²_ParcelaA) / (σ²_Bloque + σ²_ParcelaA + σ²_Residual)"),
      ICC = c(ifelse(v_tot_PA > 0, v_blockA / v_tot_PA, NA_real_),
              ifelse(v_tot_all > 0, (v_block + v_blockA) / v_tot_all, NA_real_))
    )
  }

  # Reactives
  sim_react <- eventReactive(input$simular_fit, {
    df <- sim_splitplot(
      nB     = as.integer(input$n_bloques),
      nA     = as.integer(input$niv_A),
      nBsub  = as.integer(input$niv_B),
      effA   = input$eff_A,
      effB   = input$eff_B,
      effAB  = input$eff_AB,
      sdA    = input$sd_A,
      sdB    = input$sd_B,
      prop_miss = input$prop_miss
    )
    validate(need(nrow(df) > 0, "No hay datos (demasiados faltantes). Reduce % faltantes."))
    df
  })

  ex_df <- eventReactive(input$ex_run, {
    sim_splitplot(
      nB = input$ex_nb,
      nA = input$ex_na,
      nBsub = input$ex_nbsub,
      effA = input$ex_effA,
      effB = input$ex_effB,
      effAB = input$ex_effAB,
      sdA = input$ex_sdA,
      sdB = input$ex_sdB,
      prop_miss = input$ex_miss / 100
    )
  })

  # Call tab servers
  pestanna1_session5_v3_server(input, output, session)
  pestanna2_session5_v3_server(input, output, session)
  pestanna3_session5_v3_server(input, output, session, sim_react, icc_from_varcorr)
  pestanna4_session5_v3_server(input, output, session)
  pestanna5_session5_v3_server(input, output, session, ex_df, icc_from_varcorr)
  pestanna6_session5_v3_server(input, output, session)
}
