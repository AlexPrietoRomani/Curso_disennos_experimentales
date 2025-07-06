# R/modules/session9.R

# UI para la Sesión 9
session9UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3(class = "session-title", "Sesión 9: Modelando Relaciones - ANCOVA y Regresión"),
        
        navset_tab(
            # ===== PESTAÑA 1: ANÁLISIS DE COVARIANZA (ANCOVA) =====
            nav_panel(
                title = "1. ANCOVA: Ajustando por Covariables",
                
                # ---------------------------------------------------------------
                # Subsección 1.1: ¿Qué es ANCOVA y cómo se diferencia?
                # ---------------------------------------------------------------
                h4(class = "section-header", "1.1 ANCOVA: El Puente entre ANOVA y Regresión"),
                p(
                    "El Análisis de Covarianza (ANCOVA) es una de las técnicas más potentes y prácticas en la experimentación agronómica. No es simplemente otro tipo de ANOVA; es una fusión inteligente que combina la ", strong("comparación de grupos (ANOVA)"), " con la ", strong("modelización de relaciones (Regresión Lineal).")
                ),

                # --- Visualización Conceptual ---
                tags$div(class="card border-primary mb-4",
                    tags$div(class="card-header bg-primary text-white", strong("La Idea Central: Limpiar el 'Ruido' para Ver la 'Señal'")),
                    tags$div(class="card-body",
                        h5(class="card-title text-center", "Descomponiendo la Variabilidad del Rendimiento"),
                        
                        # Diagrama de flujo visual
                        tags$div(class="d-flex justify-content-center align-items-center flex-wrap",
                            tags$div(class="alert alert-secondary m-2 text-center", 
                                strong("Rendimiento Observado (Y)"), br(),
                                em("La mezcla total")
                            ),
                            tags$i(class="bi bi-arrow-right-short fa-2x mx-2 d-none d-md-block"), # Flecha visible en escritorio
                            
                            # Usar un div para agrupar los componentes
                            tags$div(
                                tags$div(class="d-flex justify-content-center align-items-center flex-wrap",
                                    tags$div(class="alert alert-info m-2 text-center", strong("Efecto del Tratamiento"), br(), em("La 'Señal' que buscamos")),
                                    tags$i(class="bi bi-plus fa-2x mx-2"),
                                    tags$div(class="alert alert-warning m-2 text-center", strong("Efecto de la Covariable"), br(), em("El 'Ruido' que podemos medir y restar")),
                                    tags$i(class="bi bi-plus fa-2x mx-2"),
                                    tags$div(class="alert alert-danger m-2 text-center", strong("Error Aleatorio"), br(), em("El 'Ruido' impredecible"))
                                )
                            )
                        ),
                        p(class="text-center mt-3", "El ANCOVA es la herramienta que nos permite aislar y remover el 'Efecto de la Covariable', permitiéndonos hacer una prueba más limpia y potente sobre el 'Efecto del Tratamiento'.")
                    )
                ),

                # --- Comparación de Técnicas ---
                h4(class = "section-header", "Ubicando el ANCOVA en el Universo de los Modelos Lineales"),
                p("Entender la estructura de entrada y la pregunta que responde cada técnica es clave para no confundirlas."),

                fluidRow(
                    # Tarjeta para ANOVA
                    column(4,
                        tags$div(class="card h-100",
                            tags$div(class="card-header text-center", h5(strong("ANOVA"))),
                            tags$div(class="card-body",
                                p(strong("Respuesta (Y):"), " UNA (continua)"),
                                p(strong("Predictor (X):"), " Un factor CATEGÓRICO (Tratamiento)"),
                                hr(),
                                p(class="text-center", icon("question-circle", "fa-2x text-primary")),
                                p(class="text-center", em("¿Son diferentes las medias de los grupos?"))
                            )
                        )
                    ),
                    # Tarjeta para ANCOVA (destacada)
                    column(4,
                        tags$div(class="card h-100 border-primary shadow",
                            tags$div(class="card-header bg-primary text-white text-center", h5(strong("ANCOVA"))),
                            tags$div(class="card-body",
                                p(strong("Respuesta (Y):"), " UNA (continua)"),
                                p(strong("Predictor (X):"), tagList(
                                    "Un factor CATEGÓRICO (Tratamiento)", br(),
                                    strong("+"), " UNA o más COVARIABLES (continuas)")
                                ),
                                hr(),
                                p(class="text-center", icon("question-circle", "fa-2x text-primary")),
                                p(class="text-center", em("Después de ajustar por la covariable, ¿son diferentes las medias de los grupos?"))
                            )
                        )
                    ),
                    # Tarjeta para MANOVA
                    column(4,
                        tags$div(class="card h-100",
                            tags$div(class="card-header text-center", h5(strong("MANOVA"))),
                            tags$div(class="card-body",
                                p(strong("Respuesta (Y):"), strong("MÚLTIPLES (continuas)")),
                                p(strong("Predictor (X):"), " Un factor CATEGÓRICO (Tratamiento)"),
                                hr(),
                                p(class="text-center", icon("question-circle", "fa-2x text-primary")),
                                p(class="text-center", em("¿Son diferentes los vectores de medias (centroides) de los grupos?"))
                            )
                        )
                    )
                ),
                tags$hr(),
                # --------------------------------------------------------------------------------------
                # Subsección 1.2: El Problema del 'Ruido' Inicial y el Poder de la Covariable
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "1.2 Identificando el 'Ruido' Medible: El Rol de la Covariable"),
                p(
                    "El bloqueo es nuestra herramienta para controlar la variación ", strong("espacial"), " (gradientes). Pero, ¿qué pasa con la variación ", strong("inicial"), " que cada unidad experimental trae consigo? Esta variación preexistente, si está relacionada con nuestra variable de respuesta, actúa como un 'ruido de fondo' que dificulta escuchar la 'señal' de nuestros tratamientos."
                ),

                tags$div(class = "content-row",
                    # Columna para la explicación y ejemplos
                    tags$div(class = "main-content",
                        p("Una ", strong("covariable"), " (X) es precisamente eso: una variable continua que medimos, que no es afectada por nuestros tratamientos, y que creemos que tiene una relación lineal con nuestra variable de respuesta final (Y)."),
                        p("Al medirla (generalmente antes de aplicar los tratamientos), podemos usarla para ajustar matemáticamente nuestros resultados."),
                        
                        tags$h5("Ejemplos Clásicos de Covariables en Agronomía:"),
                        tags$ul(
                            tags$li(
                                strong("Ensayo de Rendimiento (Y):"), " La covariable (X) podría ser el ", em("número de plantas emergidas por parcela"), ". Es lógico pensar que parcelas que comienzan con más plantas tenderán a rendir más, independientemente del fertilizante aplicado."
                            ),
                            tags$li(
                                strong("Ensayo de Ganancia de Peso en Ganado (Y):"), " La covariable (X) es el ", em("peso inicial del animal."), " Un animal que ya es más pesado probablemente ganará peso de manera diferente a uno más liviano."
                            ),
                            tags$li(
                                strong("Ensayo de Control de Plagas (Y = % de daño final):"), " La covariable (X) sería la ", em("infestación basal de la plaga."), " Una planta que ya tiene muchos insectos sufrirá un daño final diferente a una que comienza casi limpia."
                            )
                        )
                    ),
                    
                    # Columna para el gráfico conceptual
                    tags$div(class = "note-cloud text-center",
                        tags$strong("Visualizando el Problema del Ruido"),
                        # plotOutput para el gráfico conceptual
                        plotOutput(ns("ancova_noise_plot"), height="250px"),
                        p(class="small text-muted mt-2",
                        "Sin ajuste, la gran superposición entre los grupos (debido al 'ruido' de la covariable) hace que parezca que no hay diferencia entre los tratamientos."
                        )
                    )
                ),
                tags$hr(),
                # --------------------------------------------------------------------------------------
                # Subsección 1.3: El Modelo ANCOVA y su 'Contrato' de Supuestos
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "1.3 El Modelo ANCOVA y su 'Contrato' de Supuestos"),
                p(
                    "Para 'restar' el efecto del ruido, el ANCOVA utiliza un modelo lineal que incorpora la covariable. Entender este modelo y sus 'cláusulas' (los supuestos) es esencial para usarlo correctamente."
                ),

                # --- El Modelo Matemático ---
                tags$div(class = "card mb-4",
                    tags$div(class = "card-header", strong("El Modelo Matemático del ANCOVA")),
                    tags$div(class = "card-body",
                        p("El modelo ANCOVA extiende el del ANOVA al añadir un término de regresión lineal para la covariable \\(X\\):"),
                        withMathJax(helpText(
                            "$$Y_{ij} = \\mu + \\tau_i + \\beta(X_{ij} - \\bar{X}_{..}) + \\epsilon_{ij}$$"
                        )),
                        p("Desglosando los componentes:"),
                        tags$ul(
                            tags$li("\\(\\mu + \\tau_i\\): Representa la ", strong("media ajustada"), " para el tratamiento i. Es la media que tendría el tratamiento si todas las unidades experimentales hubieran tenido el mismo valor promedio de la covariable (\\(\\bar{X}_{..}\\))."),
                            tags$li(strong("\\(\\beta(X_{ij} - \\bar{X}_{..})\\)"),": Este es el término de ajuste. Para cada observación, calcula cuánto se desvía su covariable \\(X_{ij}\\) de la media general y ajusta su valor de respuesta \\(Y_{ij}\\) en proporción a la pendiente \\(\\beta\\)."),
                            tags$li(strong("\\(\\beta\\)"), " es el coeficiente de regresión. Nos dice cuánto cambia Y por cada unidad de cambio en X.")
                        )
                    )
                ),

                # --- Los Supuestos del ANCOVA ---
                h4(class = "section-header", "Los Supuestos: Las Reglas del Juego"),
                p("Como todo modelo estadístico, el ANCOVA solo funciona si se cumplen ciertas condiciones. Violar estos supuestos puede llevar a conclusiones incorrectas."),

                # Usaremos una estructura de fila para mostrar los supuestos con gráficos
                fluidRow(
                    # Supuesto 1: Linealidad
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                h5(class="card-title", "1. Linealidad entre Covariable y Respuesta"),
                                p("El modelo asume que la relación entre la covariable (X) y la respuesta (Y) es una línea recta. Si la relación es curva (ej. cuadrática), el ajuste lineal será incorrecto y no eliminará el ruido eficazmente."),
                                plotOutput(ns("ancova_assumption_linearity"), height="200px")
                            )
                        )
                    ),
                    # Supuesto 2: Independencia de la Covariable
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                h5(class="card-title", "2. La Covariable es Independiente del Tratamiento"),
                                p("Este es un supuesto lógico, no estadístico. Los tratamientos NO deben afectar a la covariable. Si lo hacen, al ajustar por la covariable, estaríamos eliminando parte del efecto real del tratamiento."),
                                p(strong("Regla práctica:"), " Mide siempre la covariable ", em("antes"), " de asignar y aplicar los tratamientos.")
                            )
                        )
                    )
                ),
                fluidRow(class="mt-4",
                    # Supuesto 3: Homogeneidad de Pendientes
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                h5(class="card-title", "3. Homogeneidad de las Pendientes de Regresión"),
                                p(strong("¡El supuesto más importante del ANCOVA!"), " Asume que la relación (la pendiente) entre X e Y es la ", strong("misma"), " para todos los grupos de tratamiento. Las líneas de regresión deben ser paralelas."),
                                plotOutput(ns("ancova_assumption_slopes"), height="200px")
                            )
                        )
                    ),
                    # Supuesto 4: Supuestos del ANOVA
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                h5(class="card-title", "4. Supuestos Clásicos del ANOVA"),
                                p("Finalmente, los ", strong("residuos"), " del modelo ANCOVA (la parte que queda después del ajuste) deben cumplir los supuestos que ya conocemos:"),
                                tags$ul(
                                    tags$li("Independencia"),
                                    tags$li("Normalidad"),
                                    tags$li("Homogeneidad de Varianzas (Homocedasticidad)")
                                ),
                                p("Estos se verifican con los mismos gráficos de diagnóstico que usamos para el ANOVA.")
                            )
                        )
                    )
                ),

                tags$div(class="alert alert-danger mt-4",
                    icon("exclamation-triangle"),
                    strong("¿Qué pasa si las pendientes no son homogéneas?"),
                    p("Si las líneas de regresión no son paralelas, significa que existe una ", strong("interacción significativa entre la covariable y el tratamiento."), " Esto es un resultado interesante por sí mismo. Significa que el efecto de la condición inicial (ej. fósforo en el suelo) es diferente para cada tratamiento. En este caso, el ANCOVA tradicional no es el modelo adecuado, y se deben usar modelos más complejos que incluyan explícitamente este término de interacción.")
                ),
                tags$hr(),
                # --------------------------------------------------------------------------------------
                # Subsección 1.4: Caso de Estudio en R - Ajustando la Ganancia de Peso en Terneros
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "1.4 Caso de Estudio en R: Ajustando la Ganancia de Peso en Terneros"),
                p(
                    "Ahora, apliquemos todo lo que hemos aprendido a un conjunto de datos simulado que representa un escenario agronómico común. En este experimento, se evaluó el efecto de dos dietas nuevas (`Dieta Alta Proteína`, `Dieta Alta Energía`) en comparación con una `Dieta Control` sobre el peso final (`peso_final`) de un grupo de terneros. La altura inicial de cada ternero (`peso_inicial`) se midió antes de comenzar el ensayo y se usará como covariable."
                ),
                p(
                    strong("La pregunta de investigación es:"), " Después de controlar el efecto del peso inicial de los terneros, ¿existe una diferencia significativa en el peso final entre las dietas?"
                ),

                # Usaremos pestañas para guiar el análisis paso a paso
                navset_card_pill(
                    header = tags$h5("Flujo de Análisis del ANCOVA"),
                    
                    # Pestaña 1: Exploración y Verificación de Supuestos
                    nav_panel(
                        "Paso 1: Exploración y Supuestos",
                        h6("Visualizando la Relación Covariable-Respuesta"),
                        p("Primero, verificamos si existe una relación lineal entre la covariable (`peso_inicial`) y la respuesta (`peso_final`). También verificamos visualmente el supuesto de homogeneidad de pendientes."),
                        
                        plotOutput(ns("ancova_case_study_plot")),
                        
                        h6("Interpretación del Gráfico:"),
                        tags$ul(
                            tags$li("Se observa una fuerte tendencia positiva general: terneros que comenzaron más pesados tienden a terminar más pesados. Esto confirma que `peso_inicial` es una excelente candidata para ser una covariable."),
                            tags$li("Las líneas de regresión para cada dieta son ", strong("aproximadamente paralelas."), " Esto sugiere que el supuesto de homogeneidad de pendientes se cumple, lo que nos permite proceder con el ANCOVA.")
                        )
                    ),
                    
                    # Pestaña 2: ANVA sin ajuste
                    nav_panel(
                        "Paso 2: ANOVA Simple (Sin Ajuste)",
                        p("Primero, realizamos un ANOVA simple, ignorando la covariable. Este es nuestro análisis 'base' o de referencia."),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# Modelo ANOVA simple (ignora el peso inicial)\n",
                                "modelo_anova <- aov(peso_final ~ dieta, data = datos_simulados)"
                            )
                        ),
                        h6("Tabla ANOVA:"),
                        verbatimTextOutput(ns("ancova_case_study_anova_out")),
                        h6("Interpretación:"),
                        p("Con esta simulación, es probable que el p-valor para la `dieta` sea > 0.05. Concluiríamos (erróneamente) que no hay diferencias, porque el 'ruido' del peso inicial es muy grande.")
                    ),
                    
                    # Pestaña 3: ANCOVA con ajuste
                    nav_panel(
                        "Paso 3: ANCOVA (Con Ajuste)",
                        p("Ahora, realizamos el ANCOVA, añadiendo la covariable (`peso_inicial`) al modelo para 'limpiar' el error."),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# Modelo ANCOVA (ajusta por el peso inicial)\n",
                                "modelo_ancova <- aov(peso_final ~ peso_inicial + dieta, data = datos_simulados)"
                            )
                        ),
                        h6("Tabla ANCOVA:"),
                        verbatimTextOutput(ns("ancova_case_study_ancova_out")),
                        h6("Interpretación:"),
                        tags$ul(
                            tags$li(strong("Fila `peso_inicial`:"), " El p-valor muy bajo confirma que la covariable está significativamente relacionada con la respuesta. ¡Ajustar por ella fue útil!"),
                            tags$li(strong("Fila `dieta`:"), " Este es el resultado clave. Después de remover el efecto del peso inicial, el p-valor para `dieta` ahora debería ser significativo. ¡El ANCOVA ha revelado un efecto que el ANOVA no pudo ver!")
                        )
                    ),
                    
                    # Pestaña 4: Eficiencia y Conclusión
                    nav_panel(
                        "Paso 4: Eficiencia y Conclusión",
                        h6("Cálculo de la Eficiencia Relativa (ER)"),
                        p("Finalmente, cuantificamos la ganancia en precisión."),
                        verbatimTextOutput(ns("ancova_case_study_er_out")),
                        h6("Conclusión Final:"),
                        div(class="alert alert-success",
                            icon("trophy"),
                            " El ANCOVA fue ", strong(textOutput(ns("er_text_conclusion"), inline=TRUE)), " más eficiente que el ANOVA simple. Al medir y ajustar por el peso inicial de los terneros, pudimos reducir el error experimental lo suficiente como para detectar diferencias significativas entre las dietas que de otro modo habrían pasado desapercibidas."
                        )
                    )
                ),
                tags$hr(),
                # ---------------------------------------------------------------
                # Subsección 1.5: Laboratorio Interactivo
                # ---------------------------------------------------------------
                h4(class = "section-header", "1.5 Laboratorio Interactivo: El Poder del Ajuste Estadístico"),

                # --- Usaremos navset_card_pill para organizar la explicación y el laboratorio ---
                navset_card_pill(
                    header = tags$h5("Guía del Laboratorio y Simulación"),
                    
                    # Pestaña con la explicación del escenario (la misma que antes, está perfecta)
                    nav_panel(
                        "El Escenario Simulado",
                        tags$h5("Contexto del Experimento"),
                        p(
                            "Vamos a simular un ensayo para evaluar el efecto de ", strong("dos nuevas dietas (Dieta A, Dieta B)"), " en comparación con una ", strong("Dieta Control"), " sobre la ganancia de peso en terneros. La variable de respuesta principal es el ", strong("Peso Final (Y).")
                        ),
                        p(
                            "Sin embargo, los terneros no comienzan el ensayo con el mismo peso. Existe una variabilidad natural en su ", strong("Peso Inicial (X)."), " Es lógico suponer que los terneros más pesados al inicio también tenderán a ser más pesados al final, independientemente de la dieta. Este Peso Inicial es nuestra ", strong("covariable.")
                        ),
                        
                        tags$hr(),
                        
                        tags$h5("El Desafío: Aislar el Efecto de la Dieta"),
                        p(
                            "Si la variación en el Peso Inicial es muy grande, podría crear tanto 'ruido' en los datos del Peso Final que enmascare las diferencias reales (y quizás sutiles) entre las dietas. Un simple ANOVA sobre el Peso Final podría no detectar nada."
                        ),
                        p(
                            "El objetivo de este laboratorio es usar el ANCOVA para 'restar' estadísticamente el efecto del Peso Inicial y ver si, una vez hecho este ajuste, queda un efecto significativo de la dieta."
                        ),
                        
                        tags$h5("Guía de los Controles de la Simulación"),
                        tags$dl(
                            tags$dt("Correlación (Peso Inicial ↔ Peso Final):"),
                            tags$dd("Controla qué tan fuerte es la relación entre el peso de un ternero al inicio y al final. Un valor alto (ej. 0.9) significa que el peso inicial es un predictor muy fuerte del peso final, creando mucho 'ruido' que el ANCOVA puede limpiar."),
                            tags$dt("Magnitud del Efecto de la Dieta:"),
                            tags$dd("Controla qué tan 'buenas' son las dietas nuevas. Un valor bajo simula un efecto sutil y difícil de detectar para un ANOVA simple.")
                        ),
                        
                        tags$div(class="alert alert-info", icon("lightbulb"),
                            strong("Tu Misión:"), " Ajusta los controles para encontrar un escenario donde el ANOVA del Peso Final dé un p-valor > 0.05, pero el ANCOVA (ajustado por Peso Inicial) dé un p-valor < 0.05. Esto demuestra cómo el ANCOVA aumenta nuestro poder para detectar efectos reales."
                        )
                    ),
                    
                    # Pestaña con la herramienta interactiva
                    nav_panel(
                        "Laboratorio de Simulación",
                        sidebarLayout(
                            sidebarPanel(
                                width = 3,
                                tags$h5("Control de la Simulación"),
                                # Slider para controlar la fuerza de la covariable
                                sliderInput(ns("ancova_cor"), 
                                            tagList("Fuerza de la Covariable (Correlación X↔Y)", icon("link")),
                                            min = 0, max = 0.95, value = 0.8, step = 0.05),
                                # Slider para controlar la "señal" del tratamiento
                                sliderInput(ns("ancova_effect_size"), 
                                            tagList("Magnitud del Efecto de la Dieta", icon("chart-line")),
                                            min = 0, max = 20, value = 5, step = 1),
                                # Slider para controlar el "ruido" general
                                sliderInput(ns("ancova_sd_error"), 
                                            tagList("Error Aleatorio Residual (Ruido)", icon("wave-square")),
                                            min = 5, max = 30, value = 15, step = 1),
                                
                                actionButton(ns("run_ancova_sim"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100 mt-3")
                            ),
                            mainPanel(
                                width = 9,
                                # Fila para los gráficos comparativos
                                fluidRow(
                                    column(6,
                                        h6(strong("Visión del ANOVA: Datos Brutos")),
                                        plotOutput(ns("ancova_plot_unadjusted"))
                                    ),
                                    column(6, style="border-left: 1px solid #ddd; padding-left: 15px;",
                                        h6(strong("Visión del ANCOVA: Datos Ajustados")),
                                        plotOutput(ns("ancova_plot_adjusted"))
                                    )
                                ),
                                p(class="text-center small text-muted mt-2", "El gráfico de la derecha muestra cómo se verían los datos si todos los terneros hubieran comenzado con el mismo peso inicial. Observa cómo el ajuste 'alinea' los grupos, haciendo las diferencias más claras."),
                                hr(),
                                
                                # Fila para los resultados numéricos
                                fluidRow(
                                    column(6,
                                        h5(icon("balance-scale-left"), "Análisis SIN Ajuste (ANOVA)"),
                                        verbatimTextOutput(ns("anova_output_sim")),
                                        uiOutput(ns("anova_interpretation_ui"))
                                    ),
                                    column(6, style="border-left: 1px solid #ddd; padding-left: 15px;",
                                        h5(icon("balance-scale-right"), "Análisis CON Ajuste (ANCOVA)"),
                                        verbatimTextOutput(ns("ancova_output_sim")),
                                        uiOutput(ns("ancova_interpretation_ui"))
                                    )
                                ),
                                hr(),
                                
                                # Panel para la Eficiencia Relativa
                                uiOutput(ns("ancova_efficiency_ui"))
                            )
                        )
                    )
                ),
                tags$hr(),
            ),

            # ===== PESTAÑA 2: MANCOVA =====
            nav_panel(
                title = "2. MANCOVA: Ajustando por Covariables en Múltiples Respuestas",
                
                # --------------------------------------------------------------------------------------
                # Subpestaña 1: Definiendo el MANCOVA y su Lugar en la Estadística
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "MANCOVA: El Análisis Definitivo para Múltiples Respuestas con Covariables"),
                p(
                    "Hemos llegado al modelo más completo y potente de esta serie. El Análisis Multivariado de la Covarianza (MANCOVA) no es una técnica completamente nueva, sino la ", strong("fusión sinérgica de todo lo que hemos aprendido:"), " la comparación de grupos del ANOVA, el ajuste por 'ruido' del ANCOVA, y el manejo de múltiples respuestas del MANOVA."
                ),

                # --- Diagrama Visual de Componentes ---
                tags$div(class="card mb-4",
                    tags$div(class="card-header", strong("Construyendo el MANCOVA: Pieza por Pieza")),
                    tags$div(class="card-body text-center",
                        tags$div(class="d-flex justify-content-center align-items-center flex-wrap",
                            tags$div(class="alert alert-info m-2", strong("ANOVA"), br(), em("Compara grupos")),
                            tags$i(class="bi bi-plus fa-2x mx-2"),
                            tags$div(class="alert alert-warning m-2", strong("REGRESIÓN"), br(), em("Ajusta por covariables")),
                            tags$i(class="bi bi-plus fa-2x mx-2"),
                            tags$div(class="alert alert-success m-2", strong("ANÁLISIS MULTIVARIADO"), br(), em("Maneja múltiples Y")),
                            tags$i(class="bi bi-arrow-right-short fa-2x mx-2 d-none d-md-block"),
                            tags$div(class="alert alert-primary m-2", strong("MANCOVA"), br(), em("El modelo integrado"))
                        )
                    )
                ),

                h4(class="section-header", "La Jerarquía de los Modelos Lineales: Una Guía Visual"),
                p("La mejor forma de entender el MANCOVA es verlo como el tope de una pirámide, donde cada nivel añade una nueva capa de complejidad y poder analítico. Cada modelo responde a una pregunta de investigación cada vez más sofisticada."),

                # --- Comparación con Tarjetas Lado a Lado ---
                fluidRow(
                    # Tarjeta para ANOVA y ANCOVA (Modelos Univariados)
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-header text-center", h5(icon("ruler-vertical"), " Modelos Univariados (Una Variable de Respuesta)")),
                            tags$div(class="card-body",
                                h6(strong("ANOVA")),
                                p(em("Pregunta: ¿La media de rendimiento difiere entre las variedades?")),
                                hr(),
                                h6(strong("ANCOVA")),
                                p(em("Pregunta: Después de ajustar por la fertilidad inicial del suelo, ¿la media de rendimiento sigue difiriendo entre las variedades?")),
                                p(class="text-info", strong("Añade:"), " Una o más covariables continuas.")
                            )
                        )
                    ),
                    # Tarjeta para MANOVA y MANCOVA (Modelos Multivariados)
                    column(6,
                        tags$div(class="card h-100 border-primary shadow",
                            tags$div(class="card-header bg-primary text-white text-center", h5(icon("table-cells"), " Modelos Multivariados (Múltiples Respuestas)")),
                            tags$div(class="card-body",
                                h6(strong("MANOVA")),
                                p(em("Pregunta: ¿El perfil combinado de [Rendimiento Y Calidad] difiere entre las variedades?")),
                                hr(),
                                h6(strong("MANCOVA")),
                                p(em("Pregunta: Después de ajustar por la fertilidad inicial, ¿el perfil combinado de [Rendimiento Y Calidad] sigue difiriendo entre las variedades?")),
                                p(class="text-primary", strong("Añade:"), " Una o más covariables continuas.")
                            )
                        )
                    )
                ),

                # --- Resumen en una Tabla Detallada ---
                tags$h5(class="mt-4", "Tabla Resumen para Consulta Rápida"),
                tags$table(class="table table-bordered table-hover", style="vertical-align: middle;",
                    tags$thead(class="table-light",
                        tags$tr(
                            tags$th("Técnica"),
                            tags$th("Nº de Respuestas (Y)"),
                            tags$th("Naturaleza de los Predictores (X)"),
                            tags$th("Objetivo Analítico Principal")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td(strong("ANOVA")),
                            tags$td("UNA (continua)"),
                            tags$td("Categórico (Tratamiento)"),
                            tags$td("Comparar las medias de los grupos.")
                        ),
                        tags$tr(
                            tags$td(strong("ANCOVA")),
                            tags$td("UNA (continua)"),
                            tags$td(tagList("Categórico (Tratamiento)", br(), strong("+ Continua (Covariable)"))),
                            tags$td("Comparar las medias de los grupos, después de ajustar por el efecto de la covariable.")
                        ),
                        tags$tr(
                            tags$td(strong("MANOVA")),
                            tags$td(strong("MÚLTIPLES (continuas)")),
                            tags$td("Categórico (Tratamiento)"),
                            tags$td("Comparar los vectores de medias (centroides) de los grupos.")
                        ),
                        tags$tr(class="table-primary",
                            tags$td(strong("MANCOVA")),
                            tags$td(strong("MÚLTIPLES (continuas)")),
                            tags$td(tagList("Categórico (Tratamiento)", br(), strong("+ Continua (Covariable)"))),
                            tags$td("Comparar los vectores de medias (centroides) de los grupos, después de ajustar por el efecto de la covariable.")
                        )
                    )
                ),
                tags$hr(),

                # ---------------------------------------------------------------
                # Subsección 2: Modelo y Supuestos del MANCOVA
                # ---------------------------------------------------------------
                h4(class = "section-header", "2.2 Modelo y Supuestos del MANCOVA"),
                p("El modelo MANCOVA aplica un ajuste de regresión a cada una de las variables de respuesta simultáneamente."),
                withMathJax(helpText(
                    "$$\\text{Para cada respuesta } Y_k: Y_{kij} = \\mu_k + \\tau_{ki} + \\beta_k(X_{ij} - \\bar{X}_{..}) + \\epsilon_{kij}$$"
                )),
                p("El MANCOVA prueba si el conjunto de efectos del tratamiento (todos los \\(\\tau_{ki}\\)) es significativamente diferente de cero."),
                p(strong("Supuestos Clave:"), " El MANCOVA hereda los supuestos más estrictos de sus predecesores:"),
                tags$ul(
                    tags$li("Independencia de las observaciones."),
                    tags$li("Normalidad multivariada de los residuos."),
                    tags$li("Homogeneidad de las matrices de varianza-covarianza (Prueba M de Box)."),
                    tags$li(strong("Homogeneidad de las pendientes de regresión:"), " Se asume que la relación entre la covariable y el conjunto de respuestas es la misma en todos los grupos de tratamiento.")
                ),
                tags$hr(),

                # ---------------------------------------------------------------
                # Subsección 3: Ejemplo Práctico en R
                # ---------------------------------------------------------------
                h4(class = "section-header", "2.3 Ejemplo Práctico en R: `iris` con Covariable"),
                p("Usemos el dataset `iris`. Pregunta: ", em("Después de ajustar por el ", strong("Ancho del Sépalo"), " (nuestra covariable), ¿el perfil del pétalo (Largo y Ancho) sigue siendo diferente entre las Especies?")),
                tags$pre(class="r-code",
                    htmltools::HTML(
                        "# La sintaxis es una extensión natural de lo que ya conocemos:\n",
                        "modelo_mancova <- manova(cbind(Petal.Length, Petal.Width) ~ Sepal.Width + Species, data = iris)\n\n",
                        "# Ver el resumen de la prueba multivariada\n",
                        "summary(modelo_mancova, test = 'Pillai')\n\n",
                        "# Ver el efecto de cada término con summary.aov()\n",
                        "summary.aov(modelo_mancova)"
                    )
                ),
                
                tags$hr(),

                # ---------------------------------------------------------------
                # Subsección 4: Laboratorio Interactivo
                # ---------------------------------------------------------------
                h4(class = "section-header", "2.4 Laboratorio Interactivo: El Poder de Ajustar con MANCOVA"),
                # Se reutiliza la estructura de navset_card_pill que ya tenías, porque es excelente.
                navset_card_pill(
                    header = tags$h5("Guía del Laboratorio y Simulación"),
                    # Pestaña con la explicación del escenario
                    nav_panel(
                        "El Escenario Simulado",
                        tags$h5("Contexto del Experimento"),
                        p(
                            "Vamos a simular un ensayo agronómico para evaluar el efecto de ", strong("dos nuevos bioestimulantes (Trat_A, Trat_B)"), " en comparación con un ", strong("Control"), " sobre un cultivo de frutillas. En cada parcela, medimos dos variables de respuesta clave:"
                        ),
                        tags$ul(
                            tags$li(strong("Respuesta Y₁:"), " Rendimiento Total (kg/parcela)."),
                            tags$li(strong("Respuesta Y₂:"), " Contenido de Sólidos Solubles (°Brix), un indicador de dulzura.")
                        ),
                        p(
                            "Sin embargo, antes de aplicar los bioestimulantes, realizamos un análisis de suelo y medimos una variable que no podemos controlar: el ", strong("contenido de Fósforo (P) disponible en el suelo (ppm)."), " Sospechamos que un mayor nivel de Fósforo podría influir positivamente tanto en el rendimiento como en la dulzura, independientemente del tratamiento. Este Fósforo inicial es nuestra ", strong("covariable (X).")
                        ),
                        
                        tags$hr(),
                        
                        tags$h5("El Desafío: ¿Hay un efecto real del tratamiento?"),
                        p(
                            "El efecto de nuestra covariable (Fósforo) introduce 'ruido' en los datos. Si la variación debida al Fósforo es muy grande, podría enmascarar las diferencias reales (y quizás más sutiles) entre los bioestimulantes. Nuestro objetivo es usar MANCOVA para 'limpiar' estadísticamente el efecto del Fósforo y ver si, una vez ajustado, queda un efecto significativo del tratamiento."
                        ),
                        
                        tags$h5("Guía de los Controles de la Simulación"),
                        tags$dl(
                            tags$dt("Correlación (Covariable ↔ Respuestas):"),
                            tags$dd("Controla qué tan fuerte es la relación entre el Fósforo inicial (X) y las respuestas (Y₁, Y₂). Un valor alto (ej. 0.9) significa que el Fósforo tiene un gran impacto, creando mucho 'ruido'. Un valor bajo (ej. 0.1) significa que tiene poco impacto."),
                            tags$dt("Magnitud del Efecto del Tratamiento:"),
                            tags$dd("Controla qué tan 'buenos' son los bioestimulantes en la realidad. Un valor alto significa que Trat_A y Trat_B tienen un efecto grande sobre el rendimiento y los °Brix. Un valor bajo significa que su efecto es sutil y difícil de detectar.")
                        ),
                        
                        tags$div(class="alert alert-info", icon("lightbulb"),
                            strong("Tu Misión:"), " Intenta encontrar un escenario donde el MANOVA (sin covariable) dé un p-valor > 0.05 (no significativo), pero el MANCOVA (con covariable) dé un p-valor < 0.05 (significativo). Esto demostrará el poder del ajuste. ", em("Pista: necesitas un efecto de tratamiento sutil y una correlación fuerte.")
                        )
                    ),

                    # Pestaña con la herramienta interactiva
                    nav_panel("Laboratorio de Simulación",
                        sidebarLayout(
                            sidebarPanel(
                                width = 3,
                                tags$h5("Control de la Simulación"),
                                # Los sliders de tu UI están bien, solo aseguramos que los nombres coincidan
                                sliderInput(ns("mancova_cor"), "Correlación (Covariable ↔ Respuestas):",
                                            min = 0, max = 0.95, value = 0.8, step = 0.05),
                                sliderInput(ns("mancova_effect"), "Magnitud del Efecto del Tratamiento:", # ID ligeramente cambiado
                                            min = 0, max = 5, value = 1.5, step = 0.2),
                                actionButton(ns("run_mancova_sim"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100")
                            ),
                            mainPanel(
                                width = 9,
                                fluidRow(
                                    column(6,
                                        h6(strong("Respuesta 1: Rendimiento (kg/parcela)")),
                                        plotOutput(ns("mancova_plot_y1"))
                                    ),
                                    column(6,
                                        h6(strong("Respuesta 2: Dulzura (°Brix)")),
                                        plotOutput(ns("mancova_plot_y2"))
                                    )
                                ),
                                hr(),
                                fluidRow(
                                    column(6,
                                        h5("Resultados del MANOVA (sin ajustar)"),
                                        verbatimTextOutput(ns("manova_output_sim")),
                                        uiOutput(ns("manova_interpretation")) # UI para interpretación dinámica
                                    ),
                                    column(6,
                                        h5("Resultados del MANCOVA (ajustado por Fósforo)"),
                                        verbatimTextOutput(ns("mancova_output_sim")),
                                        uiOutput(ns("mancova_interpretation")) # UI para interpretación dinámica
                                    )
                                )
                            )
                        )
                    )
                ),

                tags$hr(),
            ),

            # ===== PESTAÑA 3: REGRESIÓN LINEAL SIMPLE =====
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
            
            # ===== PESTAÑA 4: REGRESIÓN LINEAL MÚLTIPLE =====
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
            ),

            # ===== PESTAÑA 5: PCA =====
            nav_panel(
                title = "4. PCA: Explorando la Complejidad",
                h4(class = "section-header", "6.3 PCA: El Arte de Simplificar la Complejidad"),
                    p(
                        "Mientras que MANOVA prueba una hipótesis formal, a menudo nuestro primer objetivo es simplemente ", strong("explorar"), " un conjunto de datos complejo. Si hemos medido 10, 20 o 50 variables en nuestras parcelas, ¿cómo podemos 'ver' la estructura general de los datos? ¿Qué tratamientos se parecen entre sí? ¿Qué variables se comportan de forma similar? Aquí es donde brilla el ", strong("Análisis de Componentes Principales (PCA).")
                    ),

                    # --- Analogía y Concepto Visual ---
                    tags$div(class="card mb-4",
                        tags$div(class="card-body",
                            h5(class="card-title text-center", "La Analogía de la Sombra: Reduciendo Dimensiones"),
                            p("Imagina que tienes una escultura 3D compleja (tus datos multivariados). Es difícil capturar su forma completa en una foto 2D. El PCA es como un fotógrafo experto que busca el ángulo perfecto para proyectar una sombra de esa escultura sobre una pared. Esa sombra (un plano 2D) pierde algunos detalles, pero si se elige el ángulo correcto, puede capturar la mayor parte de la forma y estructura esencial de la escultura original."),
                            
                            fluidRow(
                                column(5, class="text-center",
                                    p(strong("Datos Originales (Espacio Multidimensional)")),
                                    # Gráfico que simula datos 3D
                                    plotOutput(ns("pca_concept_3d"), height="300px")
                                ),
                                column(2, class="text-center align-self-center",
                                    p(icon("compress-arrows-alt", class="fa-4x text-primary")),
                                    p(strong("PCA 'Proyecta' la Sombra"))
                                ),
                                column(5, class="text-center",
                                    p(strong("Biplot 2D (Espacio Reducido)")),
                                    # Gráfico que simula la proyección 2D
                                    plotOutput(ns("pca_concept_2d"), height="300px")
                                )
                            )
                        )
                    ),

                    p(
                        "Técnicamente, el PCA es una técnica de ", strong("reducción de dimensionalidad."), " Toma un conjunto de datos con muchas variables (posiblemente correlacionadas) y las re-expresa como un nuevo conjunto de variables sintéticas, no correlacionadas, llamadas ", strong("Componentes Principales (PCs)."), " Estos PCs se construyen con dos objetivos:"
                    ),
                    tags$ul(
                        tags$li(strong("PC1 (Primera Componente Principal):"), " Es un nuevo eje (una combinación lineal de las variables originales) que se orienta en la dirección de la ", em("máxima variabilidad"), " de los datos. Captura la mayor cantidad posible de la 'dispersión' total."),
                        tags$li(strong("PC2 (Segunda Componente Principal):"), " Es un segundo eje, ", strong("perpendicular (no correlacionado)"), " al PC1, que captura la mayor cantidad posible de la variabilidad ", em("restante.")),
                        tags$li("...y así sucesivamente para PC3, PC4, etc.")
                    ),
                    p(
                        "El resultado es asombroso: a menudo, los dos primeros componentes (PC1 y PC2) pueden capturar el 70%, 80% o incluso más del 90% de la información total contenida en docenas de variables originales. Esto nos permite visualizar la estructura principal de los datos en un simple gráfico 2D llamado ", strong("biplot,"), " que muestra tanto las observaciones (ej. genotipos) como las variables originales (ej. rendimiento, altura) en este nuevo espacio reducido."
                    ),

                    tags$hr(),
                    
                    h4(class = "section-header", "6.4 Laboratorio Interactivo: Explorando el PCA con `iris`"),
                    p(
                        "Ahora, usemos el PCA de forma interactiva para explorar el clásico dataset `iris`. Tu objetivo es actuar como un científico de datos: selecciona diferentes combinaciones de variables y observa cómo cambia la visualización de la estructura de los datos."
                    ),

                    sidebarLayout(
                        sidebarPanel(
                            width = 3,
                            tags$h5("Control del Análisis PCA"),
                            checkboxGroupInput(ns("pca_vars"), "1. Seleccionar Variables para el PCA:",
                                choices = c("Largo Sépalo" = "Sepal.Length", 
                                            "Ancho Sépalo" = "Sepal.Width", 
                                            "Largo Pétalo" = "Petal.Length",
                                            "Ancho Pétalo" = "Petal.Width"),
                                selected = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
                            ),
                            checkboxInput(ns("pca_scale"), "Escalar Variables (Recomendado)", value = TRUE),
                            hr(),
                            tags$div(class="note-cloud",
                                strong("¿Por qué escalar?"),
                                p("Si las variables tienen escalas muy diferentes (ej. altura en cm vs. peso en kg), la variable con la mayor varianza dominará el PC1. Escalar (estandarizar) las variables asegura que todas contribuyan por igual al análisis, basándose en sus correlaciones, no en su escala.")
                            )
                        ),
                        mainPanel(
                            width = 9,
                            # Usaremos pestañas para organizar la salida
                            navset_card_pill(
                                # Pestaña para el Biplot
                                nav_panel(
                                    "Biplot",
                                    plotOutput(ns("pca_biplot")),
                                    tags$hr(),
                                    h6(strong("¿Cómo interpretar este Biplot?")),
                                    tags$ul(
                                        tags$li(strong("Puntos (Observaciones):"), " Cada punto es una flor. Puntos cercanos representan flores con características similares. Observa cómo las especies forman conglomerados (clusters) distintos."),
                                        tags$li(strong("Flechas (Variables):"), " Cada flecha representa una de las variables originales."),
                                        tags$ul(
                                            tags$li(strong("Dirección:"), " Flechas que apuntan en direcciones similares indican variables que están positivamente correlacionadas (ej. Largo y Ancho del Pétalo). Flechas en direcciones opuestas están negativamente correlacionadas."),
                                            tags$li(strong("Largo:"), " Flechas más largas indican variables que tienen una mayor contribución a la variación capturada en estos dos componentes.")
                                        )
                                    )
                                ),
                                # Pestaña para el Resumen Numérico
                                nav_panel(
                                    "Resumen Numérico",
                                    h6("Resumen de la Importancia de los Componentes"),
                                    verbatimTextOutput(ns("pca_summary")),
                                    hr(),
                                    h6("Loadings (Cargas de los Componentes)"),
                                    p("Esta tabla muestra cómo cada variable original 'carga' o contribuye a la construcción de cada Componente Principal. Valores grandes (cercanos a 1 o -1) indican una fuerte contribución."),
                                    verbatimTextOutput(ns("pca_loadings"))
                                )
                            )
                        )
                    ),

                    tags$hr()
            ),
        )
    )
}

# R/modules/session9.R

# Server para la Sesión 9
session9Server  <- function(input, output, session) {
    ns <- session$ns
    
    # --- LÓGICA PARA LA PESTAÑA 1: ANCOVA ---
    ### ---- Subsección 1.2 ----
    # Gráfico conceptual que muestra el problema del 'ruido' de la covariable
    output$ancova_noise_plot <- renderPlot({
        set.seed(3)
        n_per_group <- 30
        
        # Simular datos donde el efecto del tratamiento es pequeño y el de la covariable es grande
        df <- data.frame(
            Tratamiento = factor(rep(c("Control", "Tratado"), each = n_per_group)),
            # Efecto de la covariable (ruido grande)
            Efecto_Covariable = rep(rnorm(n_per_group, mean = 0, sd = 10), 2),
            # Efecto real del tratamiento (señal pequeña)
            Efecto_Tratamiento = rep(c(0, 3), each = n_per_group)
        )
        
        # La respuesta final es la suma de los efectos
        df$Respuesta_Y <- df$Efecto_Covariable + df$Efecto_Tratamiento + rnorm(n_per_group*2, 0, 2)
        
        ggplot(df, aes(x = Tratamiento, y = Respuesta_Y, fill = Tratamiento)) +
            geom_boxplot(alpha = 0.6) +
            geom_jitter(width = 0.2, alpha = 0.4) +
            theme_minimal(base_size = 12) +
            theme(legend.position = "none") +
            labs(
                y = "Respuesta Final (Y)",
                title = "Efecto del Tratamiento 'Oculto' por el Ruido"
            )
    })

    ### ---- Subsección 1.3 ----
    # Gráfico conceptual para el supuesto de Linealidad
    output$ancova_assumption_linearity <- renderPlot({
        set.seed(1)
        x <- 1:100
        y_linear <- 10 + 0.5*x + rnorm(100, 0, 10)
        y_nonlinear <- 10 + 0.01*(x-50)^2 + rnorm(100, 0, 10)
        
        df <- data.frame(
            X = rep(x, 2),
            Y = c(y_linear, y_nonlinear),
            Tipo = rep(c("Relación Lineal (Válido)", "Relación No Lineal (Inválido)"), each=100)
        )
        
        ggplot(df, aes(x=X, y=Y)) +
            geom_point(alpha=0.5, color="gray50") +
            geom_smooth(method="lm", se=FALSE, color="red", linetype="dashed") +
            geom_smooth(method="loess", se=FALSE, color="blue") +
            facet_wrap(~Tipo) +
            theme_minimal(base_size = 11) +
            labs(x="Covariable (X)", y="Respuesta (Y)",
                caption="La línea azul (tendencia real) debe coincidir con la roja (ajuste lineal).") +
            theme(strip.text = element_text(face="bold"))
    })

    # Gráfico conceptual para el supuesto de Homogeneidad de Pendientes
    output$ancova_assumption_slopes <- renderPlot({
        set.seed(2)
        x <- 1:50
        # Escenario válido: pendientes paralelas
        df_valid <- data.frame(
            X = rep(x, 2),
            Y = c(10 + 0.8*x + rnorm(50,0,5), 20 + 0.8*x + rnorm(50,0,5)),
            Grupo = rep(c("Trat A", "Trat B"), each=50),
            Escenario = "Pendientes Homogéneas (Válido)"
        )
        # Escenario inválido: pendientes diferentes
        df_invalid <- data.frame(
            X = rep(x, 2),
            Y = c(10 + 0.5*x + rnorm(50,0,5), 20 + 1.2*x + rnorm(50,0,5)),
            Grupo = rep(c("Trat A", "Trat B"), each=50),
            Escenario = "Pendientes Heterogéneas (Inválido)"
        )
        
        df <- rbind(df_valid, df_invalid)
        
        ggplot(df, aes(x=X, y=Y, color=Grupo)) +
            geom_point(alpha=0.4) +
            geom_smooth(method="lm", se=FALSE, linewidth=1.2) +
            facet_wrap(~Escenario) +
            theme_minimal(base_size = 11) +
            labs(x="Covariable (X)", y="Respuesta (Y)") +
            theme(strip.text = element_text(face="bold"), legend.position = "bottom")
    })

    ### ---- Subsección 1.4 ----
    # Pre-calcular los modelos para no repetirlos en cada output
    case_study_models <- reactive({
        
        # --- INICIO DE LA CORRECCIÓN DEFINITIVA ---
        # 1. Simular un dataset desde cero que se ajuste a nuestra historia.
        #    Escenario: Ensayo de ganancia de peso en terneros.
        set.seed(42) # Usamos una semilla fija para que el resultado sea siempre el mismo.
        n_por_grupo <- 15
        
        datos_simulados <- data.frame(
            # Creamos el factor de tratamiento
            dieta = factor(rep(c("Control", "Dieta Alta Proteína", "Dieta Alta Energía"), each = n_por_grupo))
        ) %>%
        mutate(
            # Creamos la covariable: Peso Inicial de los terneros
            peso_inicial = rnorm(n(), mean = 250, sd = 15),
            
            # Definimos el efecto real de cada dieta
            efecto_dieta = case_when(
                dieta == "Control" ~ 0,
                dieta == "Dieta Alta Proteína" ~ 8,  # Un efecto sutil
                dieta == "Dieta Alta Energía"  ~ 12  # Un efecto un poco mayor
            ),
            
            # La respuesta final (Peso Final) depende del peso inicial, el efecto de la dieta y un error
            peso_final = 300 + (peso_inicial - 250) * 1.1 + efecto_dieta + rnorm(n(), mean = 0, sd = 10)
        )
        # --- FIN DE LA CORRECCIÓN DEFINITIVA ---
        
        # Ajustar ambos modelos usando el nuevo dataframe simulado
        modelo_anova <- aov(peso_final ~ dieta, data = datos_simulados)
        modelo_ancova <- aov(peso_final ~ peso_inicial + dieta, data = datos_simulados)
        
        # Calcular ER
        cme_anova <- anova(modelo_anova)['Residuals', 'Mean Sq']
        cme_ancova <- anova(modelo_ancova)['Residuals', 'Mean Sq']
        
        validate(
            need(!is.na(cme_anova) && !is.na(cme_ancova) && cme_ancova != 0, 
                "No se pudieron calcular los errores para la Eficiencia Relativa.")
        )
        
        eficiencia_relativa <- cme_anova / cme_ancova
        
        list(
            datos = datos_simulados,
            anova_summary = summary(modelo_anova),
            ancova_summary = anova(modelo_ancova),
            cme_anova = cme_anova,
            cme_ancova = cme_ancova,
            er = eficiencia_relativa
        )
    })

    # Gráfico para la Pestaña 1 del caso de estudio
    output$ancova_case_study_plot <- renderPlot({
        res <- case_study_models(); req(res)
        ggplot(res$datos, aes(x = peso_inicial, y = peso_final, color = dieta)) +
            geom_point(alpha = 0.7, size = 3) +
            geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
            labs(
                title = "Relación entre Peso Inicial (Covariable) y Peso Final (Respuesta)",
                x = "Peso Inicial (kg)",
                y = "Peso Final (kg)",
                color = "Dieta"
            ) +
            theme_minimal(base_size = 14)
    })

    # Salida para la tabla ANOVA
    output$ancova_case_study_anova_out <- renderPrint({
        res <- case_study_models(); req(res)
        print(res$anova_summary)
    })

    # Salida para la tabla ANCOVA
    output$ancova_case_study_ancova_out <- renderPrint({
        res <- case_study_models(); req(res)
        print(res$ancova_summary)
    })

    # Salida para el cálculo de ER
    output$ancova_case_study_er_out <- renderPrint({
        res <- case_study_models(); req(res)
        cat(paste0("ER = CME_ANOVA / CME_ANCOVA\n"))
        cat(paste0("ER = ", round(res$cme_anova, 2), " / ", round(res$cme_ancova, 2), "\n"))
        cat(paste0("ER = ", round(res$er, 2)))
    })

    # Salida de texto para la conclusión
    output$er_text_conclusion <- renderText({
        res <- case_study_models(); req(res)
        paste0(round((res$er - 1) * 100, 1), "%")
    })

    ### ---- Subsección 1.5 ----
    ancova_sim_results <- eventReactive(input$run_ancova_sim, {
    
        set.seed(as.integer(Sys.time()))
        n_rep <- 20; n_trat <- 3
        
        # Parámetros de la simulación desde la UI
        correlacion <- input$ancova_cor
        efecto_trat_mag <- input$ancova_effect_size
        sd_error_residual <- input$ancova_sd_error # Nuevo slider
        
        # Crear vectores
        dieta <- factor(rep(c('Control', 'Dieta_A', 'Dieta_B'), each = n_rep))
        efecto_trat <- c(rep(0, n_rep), rep(efecto_trat_mag, n_rep), rep(efecto_trat_mag * 0.5, n_rep))
        peso_inicial_x <- rnorm(n_rep * n_trat, mean = 250, sd = 20)
        error_y <- rnorm(n_rep * n_trat, mean = 0, sd = sd_error_residual)
        
        # Generar la variable de respuesta (Peso Final)
        peso_final_y <- 300 + efecto_trat + ((peso_inicial_x - mean(peso_inicial_x)) * 1.1 * correlacion) + (error_y * sqrt(1 - correlacion^2))
        
        df_sim <- data.frame(
            Dieta = dieta,
            Peso_Inicial = peso_inicial_x,
            Peso_Final = peso_final_y
        )
        
        # Ajustar ambos modelos
        modelo_anova <- aov(Peso_Final ~ Dieta, data = df_sim)
        modelo_ancova <- aov(Peso_Final ~ Peso_Inicial + Dieta, data = df_sim)
        
        # Calcular CME y ER
        cme_anova <- anova(modelo_anova)['Residuals', 'Mean Sq']
        cme_ancova <- anova(modelo_ancova)['Residuals', 'Mean Sq']
        
        validate(need(!is.na(cme_ancova) && cme_ancova > 0, "Error en el cálculo del modelo."))
        eficiencia_relativa <- cme_anova / cme_ancova
        
        # --- INICIO DE LA MEJORA: Calcular Medias Ajustadas ---
        # Usamos emmeans para obtener las medias ajustadas por la covariable
        medias_ajustadas_obj <- emmeans(modelo_ancova, ~ Dieta)
        medias_ajustadas_df <- as.data.frame(medias_ajustadas_obj)
        
        # Creamos una columna de "datos ajustados" para el gráfico
        # El valor ajustado es el residuo + la media ajustada del grupo
        df_sim$Peso_Ajustado <- residuals(modelo_ancova) + medias_ajustadas_df$emmean[match(df_sim$Dieta, medias_ajustadas_df$Dieta)]
        # --- FIN DE LA MEJORA ---
        
        list(
            datos = df_sim,
            modelo_anova = modelo_anova,
            modelo_ancova = modelo_ancova,
            cme_anova = cme_anova,
            cme_ancova = cme_ancova,
            er = eficiencia_relativa
        )
    }, ignoreNULL = FALSE)

    # Gráfico 1: Datos Brutos (Visión del ANOVA)
    output$ancova_plot_unadjusted <- renderPlot({
        res <- ancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Dieta, y = Peso_Final, fill = Dieta)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            geom_jitter(width = 0.2, alpha = 0.4) +
            theme_minimal(base_size = 12) +
            labs(y = "Peso Final (kg)", x = "Dieta", 
                subtitle = "Alta superposición debido al 'ruido' del peso inicial")
    })

    # Gráfico 2: Datos Ajustados (Visión del ANCOVA)
    output$ancova_plot_adjusted <- renderPlot({
        res <- ancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Dieta, y = Peso_Ajustado, fill = Dieta)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            geom_jitter(width = 0.2, alpha = 0.4) +
            theme_minimal(base_size = 12) +
            labs(y = "Peso Final Ajustado (kg)", x = "Dieta",
                subtitle = "Menor superposición al eliminar el efecto del peso inicial")
    })

    # El resto del código del servidor (renderPrints y renderUIs para las tablas e interpretaciones)
    # puede permanecer como estaba, pero asegúrate de que los IDs de output sean únicos
    # si los tienes duplicados. Corrijo esto aquí:

    # Salidas de texto para los modelos
    output$anova_output_sim <- renderPrint({
        res <- ancova_sim_results(); req(res)
        cat("--- Tabla ANOVA ---\n")
        summary(res$modelo_anova)
    })

    output$ancova_output_sim <- renderPrint({
        res <- ancova_sim_results(); req(res)
        cat("--- Tabla ANCOVA ---\n")
        anova(res$modelo_ancova)
    })

    # Interpretaciones dinámicas
    output$anova_interpretation_ui <- renderUI({
        res <- ancova_sim_results(); req(res)
        p_val <- summary(res$modelo_anova)[[1]]["Dieta", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"), strong("Significativo."))
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"), strong("No Significativo."))
        }
    })

    output$ancova_interpretation_ui <- renderUI({
        res <- ancova_sim_results(); req(res)
        p_val <- anova(res$modelo_ancova)["Dieta", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"), strong("Significativo."))
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"), strong("No Significativo."))
        }
    })

    # Salida para la Eficiencia Relativa
    output$ancova_efficiency_ui <- renderUI({
        res <- ancova_sim_results(); req(res)
        er_porc <- res$er * 100
        alert_class <- if (er_porc > 110) "alert alert-success" else "alert alert-warning"
        
        tagList(
            h5(class="text-center", "Eficiencia Relativa (ER)"),
            tags$div(class = alert_class, style = "text-align: center; font-size: 1.2em;",
                strong(paste0("ER = ", round(er_porc, 1), "%")),
                p(class="small mt-2",
                paste0("El ANCOVA fue un ", round(er_porc - 100, 1), "% más eficiente, aumentando nuestro poder de detección.")
                )
            )
        )
    })
    
    # --- LÓGICA PARA LA PESTAÑA 2: MANCOVA ---
    ### ---- Subsección 2.4 ----
    mancova_sim_results <- eventReactive(input$run_mancova_sim, {
        
        set.seed(as.integer(Sys.time()))
        n_rep <- 20; n_trat <- 3
        
        # Parámetros de la simulación
        correlacion <- input$mancova_cor
        efecto_trat_mag <- input$mancova_effect # Usar el ID corregido
        
        # Crear vectores
        tratamiento_vec <- factor(rep(c('Control', 'Trat_A', 'Trat_B'), each = n_rep))
        efecto_y1_vec <- c(rep(0, n_rep), rep(efecto_trat_mag, n_rep), rep(efecto_trat_mag * 0.8, n_rep))
        efecto_y2_vec <- c(rep(0, n_rep), rep(efecto_trat_mag * -0.5, n_rep), rep(efecto_trat_mag * 0.2, n_rep))
        covariable_x_vec <- rnorm(n_rep * n_trat, mean = 50, sd = 10)
        error_y1_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 8)
        error_y2_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 5)
        
        respuesta_Y1_vec <- 20 + efecto_y1_vec + (covariable_x_vec * correlacion) + (error_y1_vec * sqrt(1 - correlacion^2))
        respuesta_Y2_vec <- 15 + efecto_y2_vec + (covariable_x_vec * correlacion * 0.5) + (error_y2_vec * sqrt(1 - correlacion^2))
        
        # Construir el data.frame
        df_sim <- data.frame(
            tratamiento = tratamiento_vec,
            covariable_X = covariable_x_vec,
            respuesta_Y1 = respuesta_Y1_vec,
            respuesta_Y2 = respuesta_Y2_vec
        )
        
        # Ajustar ambos modelos
        modelo_manova <- manova(cbind(respuesta_Y1, respuesta_Y2) ~ tratamiento, data = df_sim)
        modelo_mancova <- manova(cbind(respuesta_Y1, respuesta_Y2) ~ covariable_X + tratamiento, data = df_sim)
        
        list(
            datos = df_sim,
            manova_summary = summary(modelo_manova, test = "Pillai"),
            mancova_summary = summary(modelo_mancova, test = "Pillai")
        )
    }, ignoreNULL = FALSE)

    # Gráfico para la Respuesta 1
    output$mancova_plot_y1 <- renderPlot({
        res <- mancova_sim_results(); req(res)
        # Renombrar variables para los títulos del gráfico
        ggplot(res$datos, aes(x = tratamiento, y = respuesta_Y1, fill = tratamiento)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            theme_minimal(base_size = 12) +
            labs(title = "Respuesta 1 (ej. Rendimiento) sin ajustar", y = "Rendimiento (kg/parcela)")
    })

    # Gráfico para la Respuesta 2
    output$mancova_plot_y2 <- renderPlot({
        res <- mancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = tratamiento, y = respuesta_Y2, fill = tratamiento)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            theme_minimal(base_size = 12) +
            labs(title = "Respuesta 2 (ej. Dulzura) sin ajustar", y = "°Brix")
    })

    # Salidas de texto para los modelos - CORRECCIÓN DE IDs
    output$manova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        cat("--- Prueba Global MANOVA (sin ajustar) ---\n")
        print(res$manova_summary)
    })

    output$mancova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        cat("--- Prueba Global MANCOVA (ajustada por covariable) ---\n")
        print(res$mancova_summary)
    })

    # Interpretaciones dinámicas - CORRECCIÓN DE IDs
    output$manova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        # Asegurarse de extraer el p-valor correcto
        p_val <- res$manova_summary$stats["tratamiento", "Pr(>F)"]
        
        if (is.na(p_val)) return() # Evitar errores si no se encuentra
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"), strong("Conclusión: Significativo."))
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"), strong("Conclusión: No Significativo."))
        }
    })

    output$mancova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        # En la salida de MANCOVA, el efecto del tratamiento es el segundo término
        p_val <- res$mancova_summary$stats["tratamiento", "Pr(>F)"]
        
        if (is.na(p_val)) return() # Evitar errores si no se encuentra
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"), strong("Conclusión: Significativo."))
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"), strong("Conclusión: No Significativo."))
        }
    })

    # --- LÓGICA PARA LA PESTAÑA 3: REGRESIÓN LINEAL SIMPLE ---
    
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
    
    # --- LÓGICA PARA LA PESTAÑA 4: REGRESIÓN LINEAL MÚLTIPLE ---
    
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

    # --- LÓGICA PARA LA PESTAÑA 5: PCA ---

    ### ---- Subsección 3 ----
    # Gráfico conceptual de datos 3D para la analogía del PCA
    output$pca_concept_3d <- renderPlot({
        # install.packages("scatterplot3d") si no está instalado
        req(scatterplot3d)
        set.seed(123)
        
        # Simular una nube de puntos 3D con estructura
        nube_3d <- data.frame(
            x = rnorm(100, 0, 2),
            y = rnorm(100, 0, 2),
            z = rnorm(100, 0, 0.5)
        )
        nube_3d$y <- nube_3d$y + nube_3d$x * 0.5
        nube_3d$z <- nube_3d$z + nube_3d$x * 0.2

        # Crear el gráfico 3D
        scatterplot3d::scatterplot3d(
            x = nube_3d$x, y = nube_3d$y, z = nube_3d$z,
            pch = 19, color = "steelblue",
            angle = 40,
            grid = TRUE, box = FALSE,
            xlab = "Variable 1", ylab = "Variable 2", zlab = "Variable 3"
        )
    })

    # Gráfico conceptual de la proyección 2D (la 'sombra')
    output$pca_concept_2d <- renderPlot({
        set.seed(123) # Usar la misma semilla para consistencia
        
        # Simular los mismos datos
        nube_3d <- data.frame(
            x = rnorm(100, 0, 2),
            y = rnorm(100, 0, 2),
            z = rnorm(100, 0, 0.5)
        )
        nube_3d$y <- nube_3d$y + nube_3d$x * 0.5
        nube_3d$z <- nube_3d$z + nube_3d$x * 0.2
        
        # Realizar el PCA sobre los datos 3D
        pca_result <- prcomp(nube_3d, scale. = TRUE)
        df_pca <- as.data.frame(pca_result$x)
        
        # Graficar los dos primeros componentes
        ggplot(df_pca, aes(x = PC1, y = PC2)) +
            geom_point(color = "steelblue", alpha = 0.7, size = 3) +
            labs(
                x = "Componente Principal 1 (Máxima Variación)",
                y = "Componente Principal 2 (Segunda Variación)"
            ) +
            theme_minimal(base_size = 12) +
            coord_fixed()
    })

    ### ---- Subsección 4 ----
    # Reactive para realizar el Análisis de Componentes Principales (PCA)
    pca_results <- reactive({
        # Requerir al menos 2 variables para el PCA
        validate(
            need(length(input$pca_vars) >= 2, "Por favor, seleccione al menos dos variables para realizar el PCA.")
        )
        
        iris_subset <- iris %>% dplyr::select(all_of(input$pca_vars))
        
        # Realizar el PCA
        prcomp(iris_subset, scale. = input$pca_scale)
    })

    # Gráfico Biplot del PCA (CORREGIDO)
    output$pca_biplot <- renderPlot({
        pca_model <- pca_results()
        req(pca_model)
        
        # La llamada a autoplot() debe hacerse después de cargar la librería, sin el prefijo.
        # El paquete ggfortify extiende la función genérica autoplot.
        autoplot(
            pca_model, 
            data = iris,
            colour = 'Species',
            shape = 'Species', # Añadir forma para mayor claridad
            size = 3,
            
            # Estética de los loadings (flechas)
            loadings = TRUE,
            loadings.colour = 'blue',
            loadings.label = TRUE,
            loadings.label.colour = 'blue',
            loadings.label.size = 5,
            loadings.label.vjust = 1.2, # Ajustar posición vertical de la etiqueta
            loadings.arrow.size = 1.5   # Hacer las flechas un poco más gruesas
        ) +
        labs(
            title = "Biplot del Análisis de Componentes Principales (PCA)",
            subtitle = "Visualización de la relación entre especies y variables medidas"
        ) +
        theme_bw(base_size = 14) + # Usar theme_bw para un look más clásico
        theme(legend.position = "bottom")
    })

    # Resumen del PCA
    output$pca_summary <- renderPrint({
        pca_model <- pca_results()
        req(pca_model)
        
        cat("--- Importancia de los Componentes ---\n")
        summary_pca <- summary(pca_model)
        print(summary_pca$importance)
        
        cat("\n--- Interpretación del Resumen ---\n")
        prop_var_pc1 <- round(summary_pca$importance['Proportion of Variance', 'PC1'] * 100, 1)
        prop_var_pc2 <- round(summary_pca$importance['Proportion of Variance', 'PC2'] * 100, 1)
        
        cat(paste0(" - PC1 explica el ", prop_var_pc1, "% de la variabilidad total.\n"))
        cat(paste0(" - PC2 explica el ", prop_var_pc2, "% de la variabilidad total.\n"))
        cat(paste0(" - Juntos, los dos primeros componentes capturan el ", 
                round(summary_pca$importance['Cumulative Proportion', 'PC2'] * 100, 1), 
                "% de toda la información de las variables originales.\n"))
        cat("   Esto justifica el uso del biplot 2D para explorar los datos.\n")
    })

    # Salida para los Loadings
    output$pca_loadings <- renderPrint({
        pca_model <- pca_results()
        req(pca_model)
        
        cat("--- Loadings (Coeficientes de las Variables) ---\n")
        # Los loadings están en el componente 'rotation' del objeto prcomp
        print(pca_model$rotation)
    })
}