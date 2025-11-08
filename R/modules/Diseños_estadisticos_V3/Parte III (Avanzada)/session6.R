# R/modules/session6.R
# Sesión 6: Diseños en Franjas (Strip-Plot)
# Requisitos de paquetes:
#   - agricolae (diseño y análisis strip-plot)
#   - ggplot2, dplyr, tidyr (visualización y manipulación)
#   - DT (tablas interactivas)
#   - stringr, purrr (utilidades)
#
# Nota: El análisis correcto en strip-plot requiere TRES errores:
#   * Error (a): franjas verticales -> prueba del Factor A
#   * Error (b): franjas horizontales -> prueba del Factor B
#   * Error (c): celdas (intersecciones) -> prueba de la Interacción A×B
# Y las comparaciones múltiples deben usar MS y gl del error correspondiente.
#
# UI --------------------------------------------------------------------------

session6_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 6: Diseños en Franjas (Strip-Plot)")
    ),

    # Conjunto de pestañas (bslib)
    navset_tab(

      # =========================
      # PESTAÑA 1: Fundamentos
      # =========================
      nav_panel(
        title = "1) Fundamentos",
        h4(class = "section-header", "¿Cuándo usar un diseño en franjas (strip-plot)?"),
        div(class = "alert alert-info",
            p(
              strong("Contexto:"), " Cuando ", em("ambos factores"),
              " son difíciles/costosos de aplicar a pequeña escala (p.ej. ",
              "labranza y riego), y es más factible aplicarlos en franjas ",
              "perpendiculares a lo largo del bloque."
            )
        ),
        fluidRow(
          column(
            width = 6,
            h5("Diferencia clave: Strip-Plot vs Split-Plot"),
            tags$ul(
              tags$li(
                strong("Split-Plot:"), " alta precisión en el factor de subparcela ",
                "y buena potencia para la interacción; menor precisión para el factor ",
                "de parcela principal."
              ),
              tags$li(
                strong("Strip-Plot:"), " precisión baja para efectos principales (A y B), ",
                strong("máxima precisión para la interacción A×B"),
                " (tres errores separados)."
              )
            ),
            div(class = "note-cloud",
                p(
                  strong("Idea rectora:"), " este diseño se elige si la ",
                  strong("interacción A×B"),
                  " es la pregunta científica prioritaria."
                )
            )
          ),
          column(
            width = 6,
            h5("Modelo y estructura de error"),
            withMathJax(
              helpText("Modelo (RCBD por bloques): $$Y_{ijk} = \\mu + R_k + \\alpha_i + \\gamma_{ik} + \\beta_j + \\Theta_{jk} + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"),
              tags$ul(
                tags$li(HTML("<b>Error (a) / &gamma;<sub>ik</sub></b>: franjas verticales &rarr; prueba de A")),
                tags$li(HTML("<b>Error (b) / &Theta;<sub>jk</sub></b>: franjas horizontales &rarr; prueba de B")),
                tags$li(HTML("<b>Error (c) / &epsilon;<sub>ijk</sub></b>: celdas &rarr; prueba de A&times;B"))
              )
            )
          )
        ),
        hr(),
        h5("Pequeño croquis mental"),
        p("En cada bloque: franjas verticales (A) × franjas horizontales (B) → celdas (A×B). ",
          "Las pruebas F usan: A→Error(a), B→Error(b), A×B→Error(c)."),
        br()
      ),

      # ==================================
      # PESTAÑA 2: Generar diseño (fieldbook)
      # ==================================
      nav_panel(
        title = "2) Generar diseño",
        p("Define niveles y bloques para crear el libro de campo aleatorizado ",
          "con ", code("agricolae::design.strip()"), "."),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            tags$h5("Factores y bloques"),
            textInput(ns("niveles_A_txt"), "Niveles Factor A (vertical), separados por coma:",
                      value = "Cincel,Disco,Directa"),
            textInput(ns("niveles_B_txt"), "Niveles Factor B (horizontal), separados por coma:",
                      value = "Goteo,Aspersion"),
            numericInput(ns("r_bloques"), "Número de bloques (r):", value = 4, min = 2, step = 1),
            checkboxInput(ns("usar_semilla"), "Usar semilla (reproducible)", value = TRUE),
            numericInput(ns("semilla"), "Semilla:", value = 123, min = 1, step = 1),
            actionButton(ns("btn_generar_diseno"), "Generar diseño", class = "btn btn-primary w-100 mt-2"),
            hr(),
            uiOutput(ns("ui_descargar_book"))
          ),
          mainPanel(
            width = 8,
            tags$h5("Resumen"),
            uiOutput(ns("resumen_diseno")),
            tags$h5("Libro de campo (fieldbook)"),
            DT::dataTableOutput(ns("tabla_book")),
            hr(),
            tags$h5("Croquis por bloque (orden de franjas A×B)"),
            plotOutput(ns("plot_croquis"), height = "420px")
          )
        )
      ),

      # ==================================
      # PESTAÑA 3: ANOVA de franjas
      # ==================================
      nav_panel(
        title = "3) ANOVA en franjas",
        p("Analiza con ", code("agricolae::strip.plot()"),
          ". Puedes usar el dataset de ejemplo de ", code("agricolae"),
          " o simular a partir del diseño de la pestaña 2."),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            radioButtons(ns("fuente_datos"), "Fuente de datos:",
                         choices = c("Ejemplo agricolae::plotted" = "ejemplo",
                                     "Simular a partir del diseño (pestaña 2)" = "sim_diseno")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'sim_diseno'", ns("fuente_datos")),
              tags$h5("Parámetros de simulación"),
              numericInput(ns("media_base"), "Media base (μ):", value = 100, step = 1),
              numericInput(ns("efecto_A_amp"), "Amplitud efecto A (±):", value = 8, step = 0.5),
              numericInput(ns("efecto_B_amp"), "Amplitud efecto B (±):", value = 6, step = 0.5),
              numericInput(ns("efecto_AB_amp"), "Amplitud interacción (±):", value = 4, step = 0.5),
              tags$hr(),
              numericInput(ns("sd_a"), HTML("Desv. Est. Error (a) &sigma;<sub>A</sub>:"), value = 6, step = 0.5),
              numericInput(ns("sd_b"), HTML("Desv. Est. Error (b) &sigma;<sub>B</sub>:"), value = 5, step = 0.5),
              numericInput(ns("sd_c"), HTML("Desv. Est. Error (c) &sigma;<sub>C</sub>:"), value = 4, step = 0.5)
            ),
            actionButton(ns("btn_correr_anova"), "Correr ANOVA de franjas", class = "btn btn-success w-100 mt-2")
          ),
          mainPanel(
            width = 8,
            tags$h5("Estructura de datos usada"),
            DT::dataTableOutput(ns("tabla_datos_analisis")),
            hr(),
            tags$h5("Resultado ANOVA (tres tablas)"),
            uiOutput(ns("anova_resumen")),
            verbatimTextOutput(ns("anova_print")),
            hr(),
            tags$h5("Mapa de calor de medias observadas (A×B)"),
            plotOutput(ns("heat_means"), height = "360px")
          )
        )
      ),

      # ==================================
      # PESTAÑA 4: Post-hoc correcto (LSD)
      # ==================================
      nav_panel(
        title = "4) Post-hoc (LSD correcto)",
        p("Aplicar ", code("LSD.test()"), " usando el ", strong("MS y gl"),
          " del error adecuado para cada efecto."),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            helpText("Ejecuta primero el ANOVA en la pestaña 3."),
            checkboxGroupInput(ns("posthoc_targets"),
                               "¿Qué comparar?",
                               choices = c("Factor A" = "A",
                                           "Factor B" = "B",
                                           "Interacción A×B" = "AB"),
                               selected = c("A","B","AB")),
            actionButton(ns("btn_correr_posthoc"), "Calcular comparaciones LSD", class = "btn btn-primary w-100")
          ),
          mainPanel(
            width = 8,
            conditionalPanel(
              condition = sprintf("input['%s'].includes('A')", ns("posthoc_targets")),
              tags$h5("LSD para Factor A (usa MS=Ea, gl=gl.a)"),
              verbatimTextOutput(ns("out_lsd_A"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'].includes('B')", ns("posthoc_targets")),
              hr(),
              tags$h5("LSD para Factor B (usa MS=Eb, gl=gl.b)"),
              verbatimTextOutput(ns("out_lsd_B"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'].includes('AB')", ns("posthoc_targets")),
              hr(),
              tags$h5("LSD para Interacción A×B (usa MS=Ec, gl=gl.c)"),
              verbatimTextOutput(ns("out_lsd_AB"))
            )
          )
        )
      ),

      # ==================================
      # PESTAÑA 5: Ejercicios interactivos
      # ==================================
      nav_panel(
        title = "5) Ejercicios interactivos",
        p("Explora cómo la estructura de error (a, b, c) determina la potencia para A, B e Interacción."),
        sidebarLayout(
          sidebarPanel(
            width = 4,
            tags$h5("Dimensiones y bloques"),
            numericInput(ns("ex_a_levels"), "Niveles de A:", value = 3, min = 2, max = 8, step = 1),
            numericInput(ns("ex_b_levels"), "Niveles de B:", value = 3, min = 2, max = 8, step = 1),
            numericInput(ns("ex_r_blocks"), "Bloques (r):", value = 4, min = 2, max = 20, step = 1),
            hr(),
            tags$h5("Efectos (medias)"),
            numericInput(ns("ex_mu"), "μ (media base):", value = 100),
            numericInput(ns("ex_amp_A"), "Amplitud efecto A (±):", value = 8, step = 0.5),
            numericInput(ns("ex_amp_B"), "Amplitud efecto B (±):", value = 6, step = 0.5),
            numericInput(ns("ex_amp_AB"), "Amplitud interacción (±):", value = 5, step = 0.5),
            hr(),
            tags$h5("Estructura de error"),
            numericInput(ns("ex_sd_a"), HTML("&sigma;<sub>A</sub> (Error a):"), value = 6, step = 0.5),
            numericInput(ns("ex_sd_b"), HTML("&sigma;<sub>B</sub> (Error b):"), value = 5, step = 0.5),
            numericInput(ns("ex_sd_c"), HTML("&sigma;<sub>C</sub> (Error c):"), value = 4, step = 0.5),
            actionButton(ns("btn_run_ex"), "Simular y analizar", class = "btn btn-success w-100 mt-2")
          ),
          mainPanel(
            width = 8,
            tags$h5("ANOVA (tres tablas)"),
            verbatimTextOutput(ns("ex_anova")),
            hr(),
            tags$h5("LSD (A, B y A×B con sus errores correctos)"),
            verbatimTextOutput(ns("ex_lsd")),
            hr(),
            tags$h5("Mapa de calor de medias simuladas"),
            plotOutput(ns("ex_heat"), height = "360px")
          )
        )
      ),

      # ==================================
      # PESTAÑA 6: Referencias
      # ==================================
      nav_panel(
        title = "6) Referencias",
        tags$ul(
          tags$li(
            "agricolae – paquete para diseño/análisis agrícolas. CRAN: ",
            tags$a(href="https://search.r-project.org/CRAN/refmans/agricolae/html/design.strip.html", target="_blank",
                   "design.strip()"), " y ",
            tags$a(href="https://rdrr.io/cran/agricolae/man/strip.plot.html", target="_blank", "strip.plot()")
          ),
          tags$li(
            "Capítulos y manuales con Strip-Plot y estructura de tres errores (ANOVA por partes): ",
            tags$a(href="https://krishikosh.egranth.ac.in/displaybitstream?handle=1/5812648225", target="_blank",
                   "IIRR/ICAR – diseños avanzados"),
            " y ",
            tags$a(href="https://tarwi.lamolina.edu.pe/~fmendiburu/index-filer/download/tutorial.pdf", target="_blank",
                   "Tutorial agricolae (De Mendiburu)")
          ),
          tags$li(
            "Ejemplos y discusión conceptual de strip-plot / split-block en recursos R y notas de cursos."
          )
        )
      )

    )
  )
}

# SERVER ----------------------------------------------------------------------

session6_v3Server <- function(input, output, session) {
  ns <- session$ns

  # ---- Utilidades y validaciones de paquetes ----
  has_pkg <- function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }
  need_pkgs <- c("agricolae", "ggplot2", "dplyr", "tidyr", "DT", "stringr", "purrr")
  miss <- need_pkgs[!vapply(need_pkgs, has_pkg, logical(1))]
  if (length(miss)) {
    showModal(modalDialog(
      title = "Paquetes faltantes",
      HTML(paste0(
        "Faltan paquetes: <b>", paste(miss, collapse = ", "), "</b>.",
        "<br>Instálalos con: <code>install.packages(c(\"",
        paste(miss, collapse="\",\""), "\"))</code>"
      )),
      easyClose = TRUE
    ))
  }

  `%||%` <- function(x, y) if (is.null(x)) y else x

  # ---- PESTAÑA 2: Generar diseño ----
  design_obj <- reactiveVal(NULL)

  observeEvent(input$btn_generar_diseno, {
    req(has_pkg("agricolae"))
    A <- strsplit(input$niveles_A_txt, "\\s*,\\s*")[[1]] |> unique()
    B <- strsplit(input$niveles_B_txt, "\\s*,\\s*")[[1]] |> unique()
    r <- input$r_bloques

    validate(
      need(length(A) >= 2, "Debe especificar al menos 2 niveles para A"),
      need(length(B) >= 2, "Debe especificar al menos 2 niveles para B"),
      need(r >= 2, "Se requieren al menos 2 bloques")
    )

    # reproducibilidad opcional
    if (isTRUE(input$usar_semilla)) {
      set.seed(input$semilla)
    }

    des <- agricolae::design.strip(
      trt1 = A, trt2 = B, r = r, design = "RCBD"
    )

    # Guardar
    design_obj(des)

    # Aviso sutil
    showNotification("Diseño generado (design.strip)", type = "message", duration = 3)
  })

  output$resumen_diseno <- renderUI({
    des <- design_obj(); req(des)
    A <- des$parameters$trt1
    B <- des$parameters$trt2
    r <- des$parameters$replications
    tagList(
      div(class="alert alert-success",
          HTML(paste0(
            "<b>Estructura:</b> ", length(A), " niveles de A × ",
            length(B), " niveles de B × ", r, " bloques (RCBD).<br>",
            "<b>Total de celdas:</b> ", length(A)*length(B)*r
          ))
      )
    )
  })

  output$tabla_book <- DT::renderDataTable({
    des <- design_obj(); req(des)
    DT::datatable(des$book, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })

  output$ui_descargar_book <- renderUI({
    req(design_obj())
    downloadButton(ns("dl_book"), "Descargar fieldbook (CSV)", class = "btn btn-secondary w-100")
  })

  output$dl_book <- downloadHandler(
    filename = function() {
      paste0("stripplot_fieldbook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      des <- design_obj(); write.csv(des$book, file, row.names = FALSE)
    }
  )

  # Croquis simple: por bloque, filas = niveles de A, columnas = niveles de B
  output$plot_croquis <- renderPlot({
    des <- design_obj(); req(des)
    book <- des$book
    # A en filas (vertical), B en columnas (horizontal), por bloque
    A_levels <- des$parameters$trt1
    B_levels <- des$parameters$trt2

    # Construir grilla por bloque y mapear combinaciones presentes en book
    grid <- expand.grid(
      block = sort(unique(book$block)),
      A = factor(A_levels, levels = A_levels),
      B = factor(B_levels, levels = B_levels)
    )
    grid <- grid |>
      dplyr::left_join(
        book |>
          dplyr::rename(A = !!names(book)[which(names(book) %in% c("trt1","labranza","col"))[1]],
                        B = !!names(book)[which(names(book) %in% c("trt2","riego","row"))[1]]) |>
          dplyr::mutate(A = factor(A, levels = A_levels),
                        B = factor(B, levels = B_levels)) |>
          dplyr::select(block, A, B, plots),
        by = c("block","A","B")
      )

    ggplot2::ggplot(grid, ggplot2::aes(x = B, y = A, fill = !is.na(plots))) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = ifelse(is.na(plots), "", plots)), size = 3) +
      ggplot2::facet_wrap(~ block, nrow = 1) +
      ggplot2::labs(x = "Factor B (horizontal)", y = "Factor A (vertical)",
                    fill = "Asignado",
                    title = "Croquis conceptual por bloque (A×B)") +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#9ecae1", "FALSE" = "#f0f0f0")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  })

  # ---- PESTAÑA 3: ANOVA de franjas ----

  # Construir datos para análisis según fuente
  datos_para_analisis <- eventReactive(input$btn_correr_anova, {
    src <- input$fuente_datos

    if (identical(src, "ejemplo")) {
      # Datos de ejemplo 'plotted' del paquete agricolae
      req(has_pkg("agricolae"))
      data("plotted", package = "agricolae")
      df <- agricolae::plotted
      # estandarizar nombres esperados
      df |>
        dplyr::rename(block = .data$block, col = .data$col, row = .data$row, Y = .data$Y) |>
        dplyr::mutate(
          block = factor(block),
          col = factor(col),
          row = factor(row)
        )
    } else {
      # Simular desde el diseño de la pestaña 2
      des <- design_obj(); req(des)
      book <- des$book
      # Determinar nombres de columnas de factores en el fieldbook:
      # (admite variaciones "trt1/labranza/col" y "trt2/riego/row")
      nm_A <- names(book)[which(names(book) %in% c("trt1","labranza","col"))[1]]
      nm_B <- names(book)[which(names(book) %in% c("trt2","riego","row"))[1]]

      # parámetros de simulación
      mu  <- input$media_base
      aamp <- input$efecto_A_amp
      bamp <- input$efecto_B_amp
      abamp <- input$efecto_AB_amp
      sda <- input$sd_a; sdb <- input$sd_b; sdc <- input$sd_c

      A_levels <- unique(book[[nm_A]])
      B_levels <- unique(book[[nm_B]])
      r_blocks <- sort(unique(book$block))

      # efectos deterministas (centrados) para A, B y A×B
      set.seed(1234)
      eff_A  <- stats::runif(length(A_levels), -aamp, aamp); names(eff_A) <- A_levels
      eff_B  <- stats::runif(length(B_levels), -bamp, bamp); names(eff_B) <- B_levels
      eff_AB <- matrix(stats::runif(length(A_levels)*length(B_levels), -abamp, abamp),
                       nrow = length(A_levels), dimnames = list(A_levels, B_levels))

      # componentes de error jerárquicos por bloque:
      #   gamma_ik ~ N(0, sda^2), theta_jk ~ N(0, sdb^2), epsilon_ijk ~ N(0, sdc^2)
      # Simulación por bloque y combinación A×B
      sim_list <- lapply(r_blocks, function(k) {
        ga <- stats::rnorm(length(A_levels), 0, sda); names(ga) <- A_levels
        tb <- stats::rnorm(length(B_levels), 0, sdb); names(tb) <- B_levels

        blk <- subset(book, block == k)
        with(blk, {
          y <- mu +
            eff_A[book[[nm_A]]] +
            eff_B[book[[nm_B]]] +
            mapply(function(a,b) eff_AB[a,b], book[[nm_A]], book[[nm_B]]) +
            ga[book[[nm_A]]] + tb[book[[nm_B]]] +
            stats::rnorm(nrow(blk), 0, sdc)

          data.frame(
            block = factor(k),
            col   = factor(book[[nm_A]], levels = A_levels),
            row   = factor(book[[nm_B]], levels = B_levels),
            Y     = y
          )
        })
      })
      dplyr::bind_rows(sim_list)
    }
  })

  output$tabla_datos_analisis <- DT::renderDataTable({
    df <- datos_para_analisis(); req(df)
    DT::datatable(head(df, 20), options = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE)
  })

  # Ejecutar strip.plot y mostrar ANOVA
  strip_fit <- reactiveVal(NULL)

  observeEvent(input$btn_correr_anova, {
    df <- datos_para_analisis(); req(df)
    validate(
      need(all(c("block","col","row","Y") %in% names(df)),
           "El data.frame debe contener columnas: block, col, row, Y.")
    )
    fit <- agricolae::strip.plot(
      block = df$block,
      col   = df$col,   # Factor A (vertical)
      row   = df$row,   # Factor B (horizontal)
      Y     = df$Y
    )
    strip_fit(fit)
    showNotification("ANOVA (strip.plot) completado", type = "message", duration = 3)
  })

  output$anova_resumen <- renderUI({
    fit <- strip_fit(); req(fit)
    tagList(
      div(class="alert alert-secondary",
          HTML(paste0(
            "<b>Errores estimados</b> — ",
            "Error(a) MS=Ea, gl=gl.a; ",
            "Error(b) MS=Eb, gl=gl.b; ",
            "Error(c) MS=Ec, gl=gl.c."
          ))
      )
    )
  })

  output$anova_print <- renderPrint({
    fit <- strip_fit(); req(fit)
    # fit$ANOVA es una lista con tres tablas (A, B, A×B)
    # Además, fit expone gl.a, gl.b, gl.c y Ea, Eb, Ec para usar en post-hoc.
    cat("== ANOVA A (vertical) ==\n")
    print(fit$ANOVA[[1]])
    cat("\n== ANOVA B (horizontal) ==\n")
    print(fit$ANOVA[[2]])
    cat("\n== ANOVA A × B (interacción) ==\n")
    print(fit$ANOVA[[3]])
    cat("\n--- MS y gl disponibles ---\n")
    cat("Ea:", fit$Ea, " | gl.a:", fit$gl.a, "\n")
    cat("Eb:", fit$Eb, " | gl.b:", fit$gl.b, "\n")
    cat("Ec:", fit$Ec, " | gl.c:", fit$gl.c, "\n")
  })

  output$heat_means <- renderPlot({
    df <- datos_para_analisis(); req(df)
    means <- df |>
      dplyr::group_by(col, row) |>
      dplyr::summarise(meanY = mean(Y), .groups = "drop")

    ggplot2::ggplot(means, ggplot2::aes(x = row, y = col, fill = meanY)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", meanY)), size = 3) +
      ggplot2::scale_fill_continuous(name = "Media") +
      ggplot2::labs(x = "Factor B (horizontal)", y = "Factor A (vertical)",
                    title = "Medias observadas por combinación A×B") +
      ggplot2::theme_minimal(base_size = 12)
  })

  # ---- PESTAÑA 4: Post-hoc (LSD) ----

  compute_LSDs <- eventReactive(input$btn_correr_posthoc, {
    req(strip_fit()); fit <- strip_fit()
    df <- datos_para_analisis(); req(df)

    res <- list()

    if ("A" %in% input$posthoc_targets) {
      res$A <- agricolae::LSD.test(
        y = df$Y, trt = df$col,
        DFerror = fit$gl.a, MSerror = fit$Ea,
        console = TRUE
      )
    }
    if ("B" %in% input$posthoc_targets) {
      res$B <- agricolae::LSD.test(
        y = df$Y, trt = df$row,
        DFerror = fit$gl.b, MSerror = fit$Eb,
        console = TRUE
      )
    }
    if ("AB" %in% input$posthoc_targets) {
      df$AB <- interaction(df$col, df$row, drop = TRUE)
      res$AB <- agricolae::LSD.test(
        y = df$Y, trt = df$AB,
        DFerror = fit$gl.c, MSerror = fit$Ec,
        console = TRUE
      )
    }
    res
  })

  output$out_lsd_A  <- renderPrint({ x <- compute_LSDs(); req(x$A);  print(x$A)  })
  output$out_lsd_B  <- renderPrint({ x <- compute_LSDs(); req(x$B);  print(x$B)  })
  output$out_lsd_AB <- renderPrint({ x <- compute_LSDs(); req(x$AB); print(x$AB) })

  # ---- PESTAÑA 5: Ejercicios interactivos ----

  ex_data <- eventReactive(input$btn_run_ex, {
    a <- input$ex_a_levels; b <- input$ex_b_levels; r <- input$ex_r_blocks
    mu <- input$ex_mu
    aamp <- input$ex_amp_A; bamp <- input$ex_amp_B; abamp <- input$ex_amp_AB
    sda <- input$ex_sd_a; sdb <- input$ex_sd_b; sdc <- input$ex_sd_c

    A_levels <- paste0("A", seq_len(a))
    B_levels <- paste0("B", seq_len(b))
    blocks   <- paste0("Bloque", seq_len(r))

    set.seed(777)
    eff_A  <- stats::runif(a, -aamp, aamp); names(eff_A) <- A_levels
    eff_B  <- stats::runif(b, -bamp, bamp); names(eff_B) <- B_levels
    eff_AB <- matrix(stats::runif(a*b, -abamp, abamp), nrow = a,
                     dimnames = list(A_levels, B_levels))

    sim <- lapply(blocks, function(k) {
      ga <- stats::rnorm(a, 0, sda); names(ga) <- A_levels
      tb <- stats::rnorm(b, 0, sdb); names(tb) <- B_levels
      expand.grid(col = A_levels, row = B_levels) |>
        dplyr::mutate(
          block = k,
          Y = mu + eff_A[col] + eff_B[row] + purrr::map2_dbl(col, row, ~ eff_AB[.x, .y]) +
            ga[col] + tb[row] + stats::rnorm(dplyr::n(), 0, sdc)
        )
    }) |> dplyr::bind_rows()
    sim$block <- factor(sim$block); sim$col <- factor(sim$col); sim$row <- factor(sim$row)
    sim
  })

  ex_fit <- reactive({
    df <- ex_data(); req(df)
    agricolae::strip.plot(block = df$block, col = df$col, row = df$row, Y = df$Y)
  })

  output$ex_anova <- renderPrint({
    fit <- ex_fit(); req(fit)
    cat("== ANOVA A (vertical) ==\n");   print(fit$ANOVA[[1]])
    cat("\n== ANOVA B (horizontal) ==\n"); print(fit$ANOVA[[2]])
    cat("\n== ANOVA A × B (interacción) ==\n"); print(fit$ANOVA[[3]])
    cat("\nMS/gl:  Ea=",fit$Ea," gl.a=",fit$gl.a," | Eb=",fit$Eb," gl.b=",fit$gl.b,
        " | Ec=",fit$Ec," gl.c=",fit$gl.c,"\n")
  })

  output$ex_lsd <- renderPrint({
    fit <- ex_fit(); df <- ex_data(); req(fit, df)
    df$AB <- interaction(df$col, df$row, drop = TRUE)
    cat("--- LSD A (MS=Ea, gl=gl.a) ---\n")
    print(agricolae::LSD.test(df$Y, df$col, DFerror = fit$gl.a, MSerror = fit$Ea, console = TRUE))
    cat("\n--- LSD B (MS=Eb, gl=gl.b) ---\n")
    print(agricolae::LSD.test(df$Y, df$row, DFerror = fit$gl.b, MSerror = fit$Eb, console = TRUE))
    cat("\n--- LSD A×B (MS=Ec, gl=gl.c) ---\n")
    print(agricolae::LSD.test(df$Y, df$AB,  DFerror = fit$gl.c, MSerror = fit$Ec, console = TRUE))
  })

  output$ex_heat <- renderPlot({
    df <- ex_data(); req(df)
    means <- df |>
      dplyr::group_by(col, row) |>
      dplyr::summarise(meanY = mean(Y), .groups = "drop")
    ggplot2::ggplot(means, ggplot2::aes(x = row, y = col, fill = meanY)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = sprintf("%.1f", meanY)), size = 3) +
      ggplot2::labs(x = "Factor B (horizontal)", y = "Factor A (vertical)",
                    fill = "Media", title = "Medias simuladas A×B") +
      ggplot2::theme_minimal(base_size = 12)
  })

}
