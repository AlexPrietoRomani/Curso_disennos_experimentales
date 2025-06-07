# R/modules/session6.R

# UI para la Sesión 6
session6UI <- function(id) {
    ns <- NS(id)
    tagList(
        h3(class = "session-title", "Sesión 6: Diseños Factoriales y el Poder de la Interacción"),
        
        navset_tab(
            # ===== PESTAÑA 1: Introducción =====
            nav_panel(
                title = "1. ¿Por qué Factoriales?",
                h4(class = "section-header", "1.1 Limitaciones de Estudiar un Factor a la Vez"),
                p("Hasta ahora, hemos estudiado el efecto de un solo factor (ej. Tratamientos de fertilizante, Variedades). Si quisiéramos estudiar dos factores, como ", tags$b("Fertilización (N)"), " y ", tags$b("Riego (R)"), ", el enfoque de un factor a la vez requeriría dos experimentos separados:"),
                tags$ul(
                    tags$li("Experimento 1: Comparar niveles de Nitrógeno, manteniendo el Riego constante."),
                    tags$li("Experimento 2: Comparar niveles de Riego, manteniendo el Nitrógeno constante.")
                ),
                tags$div(class = "alert alert-danger", role = "alert",
                    tags$h5(class="alert-heading", "¡Ineficiente y Engañoso!"),
                    "Este enfoque es ineficiente (requiere más recursos) y, lo que es peor, puede ser completamente engañoso. Asume que el efecto de la fertilización es el mismo sin importar cómo riegues, y viceversa. ¿Pero qué pasa si un alto nivel de nitrógeno solo es efectivo con un alto nivel de riego? Esta ", tags$b("interdependencia"), " se pierde por completo."
                ),

                h4(class = "section-header", "1.2 La Solución: Diseños Factoriales"),
                p("Un ", tags$b("diseño factorial"), " es un tipo de experimento donde se investigan los efectos de dos o más factores simultáneamente. En un diseño factorial completo, se prueban todas las posibles combinaciones de los niveles de todos los factores."),
                p(strong("Ejemplo: Factorial 2x3")),
                tags$ul(
                    tags$li("Factor A (Nitrógeno): 2 niveles (N_bajo, N_alto)"),
                    tags$li("Factor B (Riego): 3 niveles (R_bajo, R_medio, R_alto)")
                ),
                p("Este diseño tendría 2 x 3 = 6 combinaciones de tratamiento: (N_bajo, R_bajo), (N_bajo, R_medio), (N_bajo, R_alto), (N_alto, R_bajo), (N_alto, R_medio), (N_alto, R_alto)."),
                tags$div(class = "note-cloud",
                    tags$strong("Ventajas Clave de los Factoriales:"),
                    tags$ul(
                        tags$li(strong("Eficiencia:"), " Se obtiene información sobre múltiples factores con el mismo (o menor) número de unidades experimentales."),
                        tags$li(strong("Interacciones:"), " Permiten detectar y cuantificar las interacciones entre factores, lo cual es imposible en experimentos de un solo factor. Este es su mayor poder."),
                        tags$li(strong("Validez Externa:"), " Las conclusiones son más generales, ya que se han probado en una variedad de condiciones (diferentes niveles del otro factor).")
                    )
                )
            ),

            # ===== PESTAÑA 2: Interacciones =====
            nav_panel(
                title = "2. El Corazón del Factorial: La Interacción",
                h4(class = "section-header", "2.1 ¿Qué es una Interacción?"),
                p("Una ", tags$b("interacción"), " entre dos factores (A y B) ocurre cuando el efecto de un factor (A) sobre la variable de respuesta depende del nivel en el que se encuentre el otro factor (B). En términos simples: ", strong("la respuesta no es aditiva.")),
                p("Si no hay interacción, los efectos son ", tags$b("aditivos."), " El efecto combinado de A y B es simplemente la suma de sus efectos individuales. Gráficamente, esto se ve como líneas paralelas en un gráfico de interacción."),
                p("Si hay interacción, el efecto combinado es diferente de la suma de sus partes. Puede ser:"),
                tags$ul(
                    tags$li(strong("Interacción Sinérgica (o Potenciadora):"), " El efecto combinado es mayor que la suma de los efectos individuales. Ej: un fungicida y un coadyuvante juntos controlan una enfermedad mucho mejor de lo que se esperaría sumando sus efectos por separado."),
                    tags$li(strong("Interacción Antagónica:"), " El efecto combinado es menor que la suma de los efectos individuales. Ej: la mezcla de dos herbicidas resulta ser menos efectiva porque uno interfiere con la acción del otro.")
                ),

                h4(class = "section-header", "2.2 Visualización de Interacciones"),
                plotOutput(ns("plot_interaccion_ejemplos")),
                p(class="text-center mt-2", strong("Figura 6.1:"), " (Izquierda) Sin interacción: el efecto de B es el mismo en A1 y A2 (líneas paralelas). (Centro) Interacción sinérgica: el efecto de B es mucho más pronunciado en A2. (Derecha) Interacción antagónica (crossover): el tratamiento B1 es mejor en A1, pero B2 es mejor en A2. ¡La recomendación depende de la combinación!")
            ),

            # ===== PESTAÑA 3: Análisis en R =====
            nav_panel(
                title = "3. Análisis en R",
                h4(class = "section-header", "3.1 El Modelo Factorial y su ANOVA"),
                p("El modelo para un diseño factorial con dos factores (A y B) incluye los efectos principales y el término de interacción:"),
                withMathJax(helpText(
                    "$$Y_{ijk} = \\mu + \\alpha_i + \\beta_j + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
                )),
                p("Donde \\((\\alpha\\beta)_{ij}\\) es el efecto de la interacción entre el nivel \\(i\\) del factor A y el nivel \\(j\\) del factor B."),
                p("En R, la fórmula se escribe de forma compacta como ", code("respuesta ~ A * B"), ", que R expande automáticamente a ", code("respuesta ~ A + B + A:B"), "."),
                pre(class = "r-code",
                    htmltools::HTML(
                        "# Asumiendo un data.frame 'datos_factorial' con columnas: rendimiento, nitrogeno, riego\n",
                        "modelo_factorial <- aov(rendimiento ~ nitrogeno * riego, data = datos_factorial)\n",
                        "summary(modelo_factorial)"
                    )
                ),
                h5("Interpretación de la Tabla ANOVA Factorial"),
                p("La tabla ANOVA ahora tendrá tres p-valores de interés: uno para el efecto principal de A, uno para el efecto principal de B, y uno para la interacción A:B."),
                tags$div(class="alert alert-info", role="alert",
                    strong("¡Regla de Oro de la Interpretación!"), " Siempre interpreta el término de la ", strong("interacción primero."),
                    tags$ul(
                        tags$li(strong("Si la interacción es significativa (p < 0.05):"), " Los efectos principales de A y B son de interés secundario o incluso engañosos. La historia principal está en la interacción. Debes desglosarla para entenderla (ver Pestaña 4)."),
                        tags$li(strong("Si la interacción NO es significativa (p ≥ 0.05):"), " Puedes proceder a interpretar los efectos principales de A y B de forma independiente, como si fueran dos experimentos de un solo factor.")
                    )
                ),
                h5("Tamaño del Efecto (η²p)"),
                p("Al igual que en DBCA, usamos el Eta-cuadrado parcial para medir la importancia práctica de cada efecto. ", code("effectsize::eta_squared(modelo_factorial, partial = TRUE)"), " nos dará un η²p para cada factor y para la interacción.")
            ),

            # ===== PESTAÑA 4: Desglosando Interacciones =====
            nav_panel(
                title = "4. Desglosando Interacciones",
                h4(class = "section-header", "4.1 El Poder de `emmeans` para Post-Hoc"),
                p("Cuando la interacción es significativa, una prueba de Tukey sobre los efectos principales es incorrecta. Necesitamos comparar los niveles de un factor ", strong("dentro"), " de cada nivel del otro factor. Esto se conoce como análisis de ", strong("efectos simples."), " El paquete ", code("emmeans"), " (Estimated Marginal Means) es la herramienta moderna y estándar para esto."),
                
                tags$h5("Escenario 1: La Interacción ES Significativa"),
                p("Nuestra pregunta es: ¿Cuál es el efecto del Nitrógeno en cada nivel de Riego? (o viceversa)."),
                pre(class="r-code",
                    htmltools::HTML(
                        "library(emmeans)\n\n",
                        "# Comparar niveles de 'nitrogeno' para cada nivel de 'riego'\n",
                        "posthoc_interaccion <- emmeans(modelo_factorial, pairwise ~ nitrogeno | riego)\n",
                        "print(posthoc_interaccion)\n\n",
                        "# El resultado muestra una prueba de comparación (ej. N_alto vs N_bajo) por separado\n",
                        "# para R_bajo, R_medio y R_alto. Las conclusiones pueden cambiar en cada nivel de riego."
                    )
                ),

                tags$h5("Escenario 2: La Interacción NO ES Significativa"),
                p("Podemos analizar los efectos principales. `emmeans` también puede hacer esto, promediando a través de los niveles del otro factor."),
                pre(class="r-code",
                    htmltools::HTML(
                        "# Si la interacción no es significativa, se analizan los efectos principales:\n\n",
                        "# Efecto principal del Nitrógeno (promediando sobre los niveles de Riego)\n",
                        "posthoc_nitrogeno <- emmeans(modelo_factorial, pairwise ~ nitrogeno)\n",
                        "print(posthoc_nitrogeno)\n\n",
                        "# Efecto principal del Riego (promediando sobre los niveles de Nitrógeno)\n",
                        "posthoc_riego <- emmeans(modelo_factorial, pairwise ~ riego)\n",
                        "print(posthoc_riego)"
                    )
                )
            ),
            
            # ===== PESTAÑA 5: Ejemplo Interactivo =====
            nav_panel(
                title = "5. Ejemplo Interactivo",
                h4(class = "section-header", "Simulación: Creando y Destruyendo Interacciones"),
                p("Manipula los efectos para ver cómo nace una interacción. El diseño es un factorial 2x2: Factor A (2 niveles) y Factor B (2 niveles)."),
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        sliderInput(ns("efecto_A"), "Efecto Principal de A (A2 vs A1):", min = -5, max = 10, value = 4, step = 0.5),
                        sliderInput(ns("efecto_B"), "Efecto Principal de B (B2 vs B1):", min = -5, max = 10, value = 2, step = 0.5),
                        sliderInput(ns("efecto_int"), "Fuerza de la Interacción (A2:B2):", min = -10, max = 10, value = 0, step = 0.5),
                        sliderInput(ns("sd_error_fact"), "Error Aleatorio (σ):", min = 0.5, max = 5, value = 1.5, step = 0.1),
                        actionButton(ns("run_sim_fact"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100")
                    ),
                    mainPanel(
                        width = 9,
                        plotOutput(ns("plot_interactivo_factorial")),
                        fluidRow(
                            column(6, 
                                h5("Tabla ANOVA"),
                                verbatimTextOutput(ns("anova_factorial"))
                            ),
                            column(6,
                                h5("Análisis Post-Hoc (emmeans)"),
                                verbatimTextOutput(ns("posthoc_factorial"))
                            )
                        )
                    )
                )
            )
        )
    )
}

# Server para la Sesión 6
session6Server<- function(input, output, session) {
        
    # --- Lógica para Pestaña 2: Gráficos de ejemplo de interacción ---
    output$plot_interaccion_ejemplos <- renderPlot({
        # Datos simulados para los ejemplos
        df_no_int <- data.frame(A = rep(c("A1","A2"), each=2), B = rep(c("B1","B2"), 2), Y = c(10, 15, 14, 19))
        df_sinergia <- data.frame(A = rep(c("A1","A2"), each=2), B = rep(c("B1","B2"), 2), Y = c(10, 12, 14, 25))
        df_antagon <- data.frame(A = rep(c("A1","A2"), each=2), B = rep(c("B1","B2"), 2), Y = c(10, 20, 18, 12))
        
        p1 <- ggplot(df_no_int, aes(x=B, y=Y, group=A, color=A)) + geom_line(size=1.5) + geom_point(size=4) + ggtitle("Sin Interacción (Aditivo)") + theme_bw()
        p2 <- ggplot(df_sinergia, aes(x=B, y=Y, group=A, color=A)) + geom_line(size=1.5) + geom_point(size=4) + ggtitle("Interacción Sinérgica") + theme_bw()
        p3 <- ggplot(df_antagon, aes(x=B, y=Y, group=A, color=A)) + geom_line(size=1.5) + geom_point(size=4) + ggtitle("Interacción Antagónica (Crossover)") + theme_bw()
        
        p1 + p2 + p3
    })
    
    # --- Lógica para Pestaña 5: Simulación Interactiva ---
    sim_results_fact <- eventReactive(input$run_sim_fact, {
        
        set.seed(as.integer(Sys.time()))
        n_rep <- 20 # Replicaciones por combinación
        
        # Crear el data.frame del diseño 2x2
        datos_fact <- expand.grid(
            A = factor(c("A1", "A2")),
            B = factor(c("B1", "B2")),
            rep = 1:n_rep
        )
        
        # Definir efectos
        media_base <- 20
        efecto_A <- input$efecto_A
        efecto_B <- input$efecto_B
        efecto_int <- input$efecto_int # Efecto extra solo cuando A=A2 y B=B2
        
        # Calcular la respuesta
        datos_fact <- datos_fact %>%
            mutate(
                respuesta = media_base + 
                            ifelse(A == "A2", efecto_A, 0) +
                            ifelse(B == "B2", efecto_B, 0) +
                            ifelse(A == "A2" & B == "B2", efecto_int, 0) +
                            rnorm(n(), mean = 0, sd = input$sd_error_fact)
            )
        
        # Ajustar el modelo
        modelo <- aov(respuesta ~ A * B, data = datos_fact)
        
        # Determinar si la interacción es significativa para el post-hoc
        p_val_int <- broom::tidy(modelo) %>% filter(term == "A:B") %>% pull(p.value)
        
        posthoc_results <- if (p_val_int < 0.05) {
            # Desglosar la interacción
            emmeans(modelo, pairwise ~ A | B)
        } else {
            # Analizar efectos principales
            list(
                main_A = emmeans(modelo, pairwise ~ A),
                main_B = emmeans(modelo, pairwise ~ B)
            )
        }
        
        list(
            datos = datos_fact,
            modelo = modelo,
            p_val_int = p_val_int,
            posthoc = posthoc_results
        )
    })
    
    output$plot_interactivo_factorial <- renderPlot({
        res <- sim_results_fact(); req(res)
        
        ggplot(res$datos, aes(x = B, y = respuesta, group = A, color = A)) +
            stat_summary(fun = mean, geom = "line", size = 1.5) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
            stat_summary(fun = mean, geom = "point", size = 4) +
            labs(
                title = "Gráfico de Interacción de la Simulación",
                subtitle = ifelse(res$p_val_int < 0.05, "¡Interacción Detectada!", "No hay evidencia de interacción"),
                y = "Respuesta Media"
            ) +
            theme_minimal(base_size = 14)
    })
    
    output$anova_factorial <- renderPrint({
        res <- sim_results_fact(); req(res)
        summary(res$modelo)
    })
    
    output$posthoc_factorial <- renderPrint({
        res <- sim_results_fact(); req(res)
        
        cat("--- ANÁLISIS POST-HOC ---\n")
        if (res$p_val_int < 0.05) {
            cat("\nLa interacción es SIGNIFICATIVA. Se analizan los efectos simples:\n")
            print(res$posthoc)
        } else {
            cat("\nLa interacción NO es significativa. Se analizan los efectos principales:\n\n")
            cat(">> Efecto Principal de A:\n")
            print(res$posthoc$main_A)
            cat("\n>> Efecto Principal de B:\n")
            print(res$posthoc$main_B)
        }
    })

}