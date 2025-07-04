# R/modules/session9.R

# UI para la Sesión 9
session9UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3(class = "session-title", "Sesión 9: Modelando Relaciones - ANCOVA y Regresión"),
        
        navset_tab(
            # ===== PESTAÑA 1: ANCOVA =====
            nav_panel(
                title = "1. ANCOVA: Ajustando por Covariables",
                
                h4(class = "section-header", "1.1 El Problema del 'Ruido' Inicial"),
                p(
                    "El bloqueo controla la variación espacial, pero ¿qué pasa con la variación inherente a las propias unidades experimentales? Por ejemplo, una ligera diferencia en la población inicial de plantas o en el peso inicial de los animales. Si esta variación inicial (que podemos medir) está correlacionada con nuestra respuesta final, actúa como 'ruido', dificultando la detección de los efectos del tratamiento. La solución es el ", strong("Análisis de Covarianza (ANCOVA).")
                ),
                
                h4(class = "section-header", "1.2 La Lógica y Modelo del ANCOVA"),
                p("El ANCOVA combina ANOVA y regresión para 'limpiar' estadísticamente este ruido inicial. Ajusta la variable de respuesta por el efecto de una variable continua, llamada ", strong("covariable."), " La condición clave es que la covariable no debe ser afectada por los tratamientos."),
                p(strong("Modelo Matemático (ANCOVA sobre un DCA):")),
                withMathJax(helpText(
                    "$$Y_{ij} = \\mu + \\tau_i + \\beta(X_{ij} - \\bar{X}_{..}) + \\epsilon_{ij}$$"
                )),
                p("Donde \\(\\beta\\) es la pendiente que describe la relación entre la covariable \\(X\\) y la respuesta \\(Y\\)."),
                
                h4(class = "section-header", "1.3 Laboratorio Interactivo de ANCOVA"),
                p("Usa el deslizador para simular diferentes niveles de correlación entre una covariable (ej. 'Plantas por metro lineal al inicio') y la respuesta final (ej. 'Rendimiento'). Observa cómo cambia la eficiencia del ANCOVA."),
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Control de la Simulación"),
                        sliderInput(ns("ancova_cor"), "Correlación entre Covariable (X) y Respuesta (Y):",
                                    min = 0, max = 0.95, value = 0.7, step = 0.05),
                        sliderInput(ns("ancova_sd"), "Error Experimental Residual (σ):",
                                    min = 1, max = 10, value = 5, step = 0.5),
                        actionButton(ns("run_ancova_sim"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100")
                    ),
                    mainPanel(
                        width = 9,
                        plotOutput(ns("ancova_plot")),
                        fluidRow(
                            column(6,
                                h5("Resultados del ANOVA (sin covariable)"),
                                verbatimTextOutput(ns("anova_output_sim"))
                            ),
                            column(6,
                                h5("Resultados del ANCOVA (con covariable)"),
                                verbatimTextOutput(ns("ancova_output_sim"))
                            )
                        ),
                        uiOutput(ns("ancova_efficiency_ui"))
                    )
                )
            ),

            # ===== PESTAÑA 2: REGRESIÓN LINEAL SIMPLE =====
            nav_panel(
                title = "2. Regresión Lineal Simple",
                
                h4(class = "section-header", "2.1 Más Allá de Comparar Grupos: Modelando Relaciones Continuas"),
                p("Mientras que el ANOVA es excelente para comparar grupos categóricos (tratamientos), la ", strong("regresión lineal"), " se utiliza para modelar la relación entre dos o más variables continuas. Es una de las herramientas más fundamentales en el análisis de datos agronómicos."),
                p("La ", strong("Regresión Lineal Simple"), " busca describir la relación entre una variable predictora (X, o variable independiente) y una variable de respuesta (Y, o variable dependiente) a través de una línea recta."),
                
                h4(class = "section-header", "2.2 El Modelo de la Línea Recta"),
                withMathJax(helpText(
                    "$$Y_i = \\beta_0 + \\beta_1 X_i + \\epsilon_i$$"
                )),
                tags$ul(
                    tags$li(strong("\\(Y_i\\):"), " El valor observado de la variable de respuesta para la i-ésima observación."),
                    tags$li(strong("\\(\\beta_0\\) (Intercepto):"), " El valor predicho de Y cuando X es cero. Representa el punto de partida o la base de la respuesta."),
                    tags$li(strong("\\(\\beta_1\\) (Pendiente):"), " El coeficiente más importante. Nos dice cuánto cambia Y (en promedio) por cada unidad que aumenta X. Una pendiente positiva indica una relación directa; una negativa, una relación inversa."),
                    tags$li(strong("\\(X_i\\):"), " El valor de la variable predictora para la i-ésima observación."),
                    tags$li(strong("\\(\\epsilon_i\\):"), " El error o residuo aleatorio, la parte de Y que el modelo no puede explicar.")
                ),
                p("El objetivo es encontrar los valores de \\(\\beta_0\\) y \\(\\beta_1\\) que mejor se ajusten a los datos, lo cual se hace mediante el método de ", strong("mínimos cuadrados ordinarios (MCO)"), ", que minimiza la suma de los errores al cuadrado."),
                
                h4(class = "section-header", "2.3 Ejemplo Interactivo: Dosis de Fertilizante vs. Rendimiento"),
                p("Vamos a simular un experimento para encontrar la relación entre la dosis de nitrógeno aplicada y el rendimiento de un cultivo."),
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Parámetros del Modelo"),
                        sliderInput(ns("reg_intercept"), "Rendimiento Base (β₀):", min = 10, max = 50, value = 25, step = 1),
                        sliderInput(ns("reg_slope"), "Respuesta a N (β₁):", min = -0.5, max = 2, value = 0.8, step = 0.1),
                        sliderInput(ns("reg_error"), "Ruido Experimental (σ):", min = 1, max = 15, value = 5, step = 1),
                        numericInput(ns("reg_n"), "Número de Parcelas:", value=50, min=10, max=200)
                    ),
                    mainPanel(
                        width = 9,
                        plotOutput(ns("reg_plot")),
                        verbatimTextOutput(ns("reg_summary_output"))
                    )
                )
            ),
            
            # ===== PESTAÑA 3: REGRESIÓN LINEAL MÚLTIPLE =====
            nav_panel(
                title = "3. Regresión Lineal Múltiple",
                h4(class = "section-header", "3.1 La Realidad es Multifactorial"),
                p("En agronomía, es raro que una sola variable explique todo el comportamiento de la respuesta. El rendimiento no solo depende del nitrógeno, sino también del fósforo, del potasio, de la materia orgánica, del pH, etc. La ", strong("Regresión Lineal Múltiple (RLM)"), " extiende el modelo simple para incluir múltiples variables predictoras."),
                
                h4(class = "section-header", "3.2 El Modelo Extendido"),
                withMathJax(helpText(
                    "$$Y_i = \\beta_0 + \\beta_1 X_{1i} + \\beta_2 X_{2i} + \\dots + \\beta_p X_{pi} + \\epsilon_i$$"
                )),
                p(
                    "Cada predictor (\\(X_1, X_2, \\dots\\)) tiene su propio coeficiente de pendiente (\\(\\beta_1, \\beta_2, \\dots\\)). Cada \\(\\beta_j\\) ahora se interpreta como el cambio promedio en Y por un aumento de una unidad en \\(X_j\\), ", strong("manteniendo constantes todas las demás variables predictoras.")
                ),
                
                h4(class = "section-header", "3.3 Ejemplo: Prediciendo el Peso de un Fruto"),
                p("Analizaremos el dataset `iris` para modelar el ", strong("Ancho del Pétalo"), " en función del ", strong("Largo del Pétalo"), " y el ", strong("Largo del Sépalo.")),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Selección de Modelo"),
                        checkboxGroupInput(ns("rlm_predictors"), "Seleccionar Predictores:",
                            choices = c("Largo del Pétalo" = "Petal.Length", 
                                        "Ancho del Sépalo" = "Sepal.Width", 
                                        "Largo del Sépalo" = "Sepal.Length"),
                            selected = c("Petal.Length", "Sepal.Length")
                        ),
                        p("La variable de respuesta siempre será 'Ancho del Pétalo'.")
                    ),
                    mainPanel(
                        width = 9,
                        h5("Resultados del Modelo de Regresión Múltiple:"),
                        verbatimTextOutput(ns("rlm_summary_output")),
                        h5("Diagnóstico del Modelo:"),
                        plotOutput(ns("rlm_diagnostic_plot"))
                    )
                )
            )
        )
    )
}

# R/modules/session9.R

# Server para la Sesión 9
session9Server  <- function(input, output, session) {
    ns <- session$ns
    
    # --- LÓGICA PARA LA PESTAÑA 1: ANCOVA ---
    
    # Este código es el que ya tenías para el ANCOVA, renombrado a 's9' para el namespace
    ancova_sim_results_s9 <- eventReactive(input$run_ancova_sim, {
        set.seed(as.integer(Sys.time()))
        n_rep <- 15; n_trat <- 3
        correlacion <- input$ancova_cor; sd_error <- input$ancova_sd
        tratamientos <- factor(rep(paste0("T", 1:n_trat), each = n_rep))
        efecto_trat <- c(rep(0, n_rep), rep(5, n_rep), rep(-3, n_rep))
        covariable_x <- rnorm(n_rep * n_trat, mean = 20, sd = 3)
        error_y <- rnorm(n_rep * n_trat, mean = 0, sd = sd_error)
        respuesta_y <- efecto_trat + (covariable_x * correlacion) + (error_y * sqrt(1 - correlacion^2))
        df_sim <- data.frame(tratamiento = tratamientos, covariable_X = covariable_x, respuesta_Y = respuesta_y)
        modelo_anova <- aov(respuesta_Y ~ tratamiento, data = df_sim)
        modelo_ancova <- aov(respuesta_Y ~ covariable_X + tratamiento, data = df_sim)
        cme_anova <- summary(modelo_anova)[[1]]['Residuals', 'Mean Sq']
        cme_ancova <- summary(modelo_ancova)[[1]]['Residuals', 'Mean Sq']
        if(is.na(cme_ancova) || cme_ancova == 0) return(NULL)
        eficiencia_relativa <- cme_anova / cme_ancova
        list(datos = df_sim, modelo_anova = modelo_anova, modelo_ancova = modelo_ancova, cme_anova = cme_anova, cme_ancova = cme_ancova, er = eficiencia_relativa)
    }, ignoreNULL = FALSE)

    output$ancova_plot <- renderPlot({
        res <- ancova_sim_results_s9(); req(res)
        ggplot(res$datos, aes(x = covariable_X, y = respuesta_Y, color = tratamiento)) +
            geom_point(size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
            labs(title = "Relación entre Covariable (X) y Respuesta (Y)", x = "Valor de la Covariable", y = "Valor de la Respuesta Final") +
            theme_minimal(base_size = 14)
    })

    output$anova_output_sim <- renderPrint({
        res <- ancova_sim_results_s9(); req(res)
        cat("CME (Error) del ANOVA:", round(res$cme_anova, 4), "\n\n")
        summary(res$modelo_anova)
    })

    output$ancova_output_sim <- renderPrint({
        res <- ancova_sim_results_s9(); req(res)
        cat("CME (Error) del ANCOVA:", round(res$cme_ancova, 4), "\n\n")
        summary(res$modelo_ancova)
    })

    output$ancova_efficiency_ui <- renderUI({
        res <- ancova_sim_results_s9(); req(res)
        er_porc <- res$er * 100
        alert_class <- if (er_porc > 110) "alert alert-success" else "alert alert-warning"
        tagList(
            h5("Eficiencia Relativa (ER)"),
            tags$div(class = alert_class, style = "text-align: center; font-size: 1.2em;",
                strong(paste0("ER = ", round(er_porc, 1), "%")),
                p(class="small mt-2",
                    paste0("El ANCOVA fue un ", round(er_porc - 100, 1), "% más eficiente que el ANOVA.")
                )
            )
        )
    })
    
    # --- LÓGICA PARA LA PESTAÑA 2: REGRESIÓN LINEAL SIMPLE ---
    
    reg_data <- reactive({
        req(input$reg_intercept, input$reg_slope, input$reg_error, input$reg_n)
        set.seed(42) # Semilla fija para consistencia en este ejemplo
        
        # Simular la variable predictora (dosis de Nitrógeno)
        dosis_N <- runif(input$reg_n, min = 0, max = 150)
        
        # Simular la respuesta (Rendimiento) según el modelo lineal
        rendimiento <- input$reg_intercept + input$reg_slope * dosis_N + rnorm(input$reg_n, 0, input$reg_error)
        
        data.frame(Dosis_N = dosis_N, Rendimiento = rendimiento)
    })
    
    output$reg_plot <- renderPlot({
        df <- reg_data()
        ggplot(df, aes(x = Dosis_N, y = Rendimiento)) +
            geom_point(alpha = 0.6, color = "darkgreen") +
            geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +
            labs(
                title = "Regresión Lineal Simple: Rendimiento vs. Dosis de Nitrógeno",
                x = "Dosis de Nitrógeno Aplicada (kg/ha)",
                y = "Rendimiento del Cultivo (qq/ha)"
            ) +
            theme_bw(base_size = 14)
    })
    
    output$reg_summary_output <- renderPrint({
        df <- reg_data()
        modelo <- lm(Rendimiento ~ Dosis_N, data = df)
        
        cat("--- Resumen del Modelo de Regresión Lineal ---\n\n")
        print(summary(modelo))
        
        cat("\n--- Interpretación de los Coeficientes ---\n")
        coefs <- coef(summary(modelo))
        cat(paste0(" - Intercepto (β₀): ", round(coefs[1,1], 2), ". Es el rendimiento esperado con 0 kg/ha de N.\n"))
        cat(paste0(" - Pendiente (β₁ para Dosis_N): ", round(coefs[2,1], 4), ". Por cada kg adicional de N, el rendimiento aumenta en promedio ", round(coefs[2,1], 4), " qq/ha.\n"))
        cat(paste0(" - El p-valor para Dosis_N (Pr(>|t|)) es ", format.pval(coefs[2,4], digits=4), ", lo que indica si la relación es estadísticamente significativa.\n"))
        
        cat("\n--- Interpretación de la Calidad del Ajuste ---\n")
        r_sq <- summary(modelo)$r.squared
        cat(paste0(" - R-cuadrado (R-squared): ", round(r_sq*100, 1), "%. Este porcentaje de la variabilidad en el Rendimiento es explicado por la Dosis de Nitrógeno."))
    })
    
    # --- LÓGICA PARA LA PESTAÑA 3: REGRESIÓN LINEAL MÚLTIPLE ---
    
    rlm_modelo <- reactive({
        req(input$rlm_predictors)
        
        # Construir la fórmula dinámicamente
        formula_str <- paste("Petal.Width ~", paste(input$rlm_predictors, collapse = " + "))
        
        lm(as.formula(formula_str), data = iris)
    })
    
    output$rlm_summary_output <- renderPrint({
        modelo <- rlm_modelo()
        cat("--- Resumen del Modelo de Regresión Múltiple ---\n\n")
        print(summary(modelo))
        
        r_sq_adj <- summary(modelo)$adj.r.squared
        cat("\n--- Interpretación del Ajuste ---\n")
        cat(paste0(" - R-cuadrado Ajustado: ", round(r_sq_adj*100, 1), "%. El modelo explica este porcentaje de la variabilidad en el Ancho del Pétalo."))
    })
    
    output$rlm_diagnostic_plot <- renderPlot({
        modelo <- rlm_modelo()
        par(mfrow = c(2,2))
        plot(modelo)
        par(mfrow = c(1,1))
    })
}