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
                h4(class = "section-header", "2.1 MANCOVA: El Análisis Definitivo para Múltiples Respuestas con Covariables"),
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
                # Subsección 2: El Modelo y su 'Contrato' de Supuestos
                # ---------------------------------------------------------------
                h4(class = "section-header", "2.2 Modelo y Supuestos del MANCOVA: Las Reglas del Juego"),
                p(
                    "El MANCOVA es una herramienta poderosa porque su modelo estadístico es capaz de manejar una complejidad que se acerca a la realidad agronómica. Vamos a desglosarlo para entender cómo funciona y qué condiciones ('supuestos') deben cumplirse para que sus resultados sean válidos."
                ),

                # --- El Modelo Matemático Desglosado ---
                tags$div(class="card mb-4",
                    tags$div(class="card-header", strong("Anatomía del Modelo MANCOVA")),
                    tags$div(class="card-body",
                        p("El modelo MANCOVA esencialmente ajusta un modelo ANCOVA por separado para cada variable de respuesta, pero los analiza de forma conjunta. Para una respuesta \\(Y_k\\) cualquiera, el modelo se ve así:"),
                        withMathJax(helpText(
                            "$$Y_{kij} = \\underbrace{\\mu_k + \\tau_{ki}}_{\\text{Media Ajustada del Tratamiento}} + \\underbrace{\\beta_k(X_{ij} - \\bar{X}_{..})}_{\\text{Ajuste por Covariable}} + \\underbrace{\\epsilon_{kij}}_{\\text{Error Aleatorio}}$$")),
                        p(strong("La prueba multivariada del MANCOVA evalúa la hipótesis nula de que todos los efectos del tratamiento (todos los \\(\\tau_{ki}\\) para todas las respuestas) son conjuntamente cero.")),
                        tags$ul(
                            tags$li(strong("Media Ajustada:"), " Es el corazón de la comparación. Representa cuál sería la media del tratamiento 'i' si todas las parcelas hubieran tenido un valor promedio de la covariable \\(X\\)."),
                            tags$li(strong("Ajuste por Covariable:"), " Para cada observación, este término 'corrige' el valor de Y hacia arriba o hacia abajo, dependiendo de si su valor de la covariable \\(X_{ij}\\) estaba por encima o por debajo de la media general. La fuerza de esta corrección depende de la pendiente \\(\\beta_k\\).")
                        )
                    )
                ),

                # --- Los Supuestos del MANCOVA ---
                h4(class = "section-header", "Lista de Chequeo de Supuestos del MANCOVA"),
                p("El MANCOVA es el modelo más exigente que hemos visto y hereda los supuestos de todos sus predecesores. Verificar estos 'puntos de control' es fundamental."),

                fluidRow(
                    # Supuestos Clásicos del ANOVA
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-header", h6(strong("1. Supuestos Básicos (Heredados del ANOVA)"))),
                            tags$div(class="card-body",
                                tags$ul(
                                    tags$li(strong("Independencia de las Observaciones:"), " El más importante. Garantizado por un buen diseño y aleatorización."),
                                    tags$li(strong("Normalidad Multivariada de los Residuos:"), " Los residuos de cada variable de respuesta deben ser normales. MANOVA/MANCOVA son robustos a desviaciones leves si n es grande."),
                                    tags$li(strong("Linealidad:"), " Debe existir una relación lineal entre la covariable y cada una de las variables de respuesta.")
                                )
                            )
                        )
                    ),
                    # Supuestos Específicos Multivariados
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-header", h6(strong("2. Supuestos Específicamente Multivariados"))),
                            tags$div(class="card-body",
                                tags$ul(
                                    tags$li(strong("Homogeneidad de Matrices de Covarianza:"), " Se asume que la estructura de varianzas y covarianzas de las respuestas es la misma en todos los grupos. Se verifica con la ", code("Prueba M de Box."), " (Sensible a la falta de normalidad)."),
                                    tags$li(strong("Ausencia de Multicolinealidad:"), " Las variables de respuesta no deben estar perfectamente o muy altamente correlacionadas entre sí. Si una Y es una combinación casi perfecta de otras, es redundante y puede causar problemas matemáticos.")
                                )
                            )
                        )
                    )
                ),
                fluidRow(class="mt-4",
                    # El supuesto más importante del ANCOVA
                    column(12,
                        tags$div(class="card border-danger",
                            tags$div(class="card-header bg-danger text-white", h6(strong("3. El Supuesto CRÍTICO del ANCOVA/MANCOVA: Homogeneidad de las Pendientes de Regresión"))),
                            tags$div(class="card-body",
                                p("Este supuesto es tan importante que tiene su propia sección. Asume que la relación lineal (la pendiente) entre la covariable (X) y las respuestas (Y) es la ", strong("misma para todos los grupos de tratamiento."), " En otras palabras, las líneas de regresión para cada tratamiento deben ser paralelas."),
                                # Gráfico conceptual que ya tenías
                                plotOutput(ns("ancova_assumption_slopes"), height="200px"),
                                p(strong("¿Cómo verificarlo?"), " Se ajusta un modelo que incluya el término de interacción entre la covariable y el tratamiento (ej. ", code("aov(Y ~ X * Tratamiento)"), "). Si este término de interacción es ", strong("NO significativo (p > 0.05)"), ", entonces el supuesto se cumple y podemos proceder con el MANCOVA. Si es significativo, el MANCOVA no es el modelo adecuado.")
                            )
                        )
                    )
                ),
                tags$hr(),

                # --------------------------------------------------------------------------------------
                # Subsección 3: Caso de Estudio en R - Análisis de un Ensayo de Biofertilizantes
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "2.3 Caso de Estudio en R: Análisis de un Ensayo de Biofertilizantes"),
                p(
                    "Vamos a aplicar el MANCOVA a un caso práctico. Simularemos los datos de un experimento agronómico y seguiremos el flujo de análisis completo en R."
                ),

                # --- Contexto y Datos del Caso ---
                tags$div(class="card mb-3",
                    tags$div(class="card-body",
                        h5(class="card-title", "Contexto del Experimento"),
                        p(
                            "Se evaluaron ", strong("dos nuevos biofertilizantes (BioF_A, BioF_B)"), " contra un ", strong("Control"), " en un cultivo de pimientos. Se midieron dos variables de respuesta importantes: el ", strong("Número de Frutos"), " por planta y el ", strong("Contenido de Vitamina C"), " (mg/100g) en los frutos. Adicionalmente, se midió el ", strong("pH inicial del suelo"), " en cada parcela, ya que se sospecha que puede influir en la disponibilidad de nutrientes y, por tanto, en ambas respuestas."
                        ),
                        p(strong("Pregunta de Investigación:"), em(" Después de ajustar por las diferencias iniciales en el pH del suelo, ¿los biofertilizantes afectan el perfil combinado de [Número de Frutos y Vitamina C]?"))
                    )
                ),

                # --- Descarga de Datos ---
                tags$div(class="text-center mb-4",
                    p("Para seguir este ejemplo en tu propia consola de R, primero descarga el conjunto de datos simulado:"),
                    downloadButton(ns("download_mancova_data"), "Descargar Datos de Ejemplo (CSV)", icon=icon("download"), class="btn-success")
                ),

                # --- Análisis Paso a Paso ---
                h5("Flujo de Análisis en R"),

                # Paso 1: Verificación de Supuestos Clave
                tags$details(
                    class="card mb-2",
                    tags$summary(class="card-header", strong("Paso 1: Verificar el Supuesto de Homogeneidad de Pendientes")),
                    tags$div(class="card-body",
                        p("Antes de correr el MANCOVA, debemos verificar su supuesto más importante. Ajustamos un modelo que incluye la interacción entre la covariable y el tratamiento."),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# Cargar los datos (asumiendo que los descargaste)\n",
                                "datos_biof <- read.csv('datos_biofertilizantes.csv')\n\n",
                                "# Ajustar un modelo MANOVA con interacción para probar el supuesto\n",
                                "modelo_supuesto <- manova(cbind(Num_Frutos, Vit_C) ~ pH_Inicial * Tratamiento, data = datos_biof)\n\n",
                                "# Ver el resumen. Nos interesa el p-valor de la interacción 'pH_Inicial:Tratamiento'\n",
                                "summary(modelo_supuesto, test='Pillai')"
                            )
                        ),
                        p(strong("Interpretación del Resultado (Salida no mostrada):"), " Si el p-valor para el término de interacción es > 0.05, el supuesto se cumple y podemos proceder con el MANCOVA. Asumiremos que se cumple para este ejemplo.")
                    )
                ),

                # Paso 2: Ejecución del MANCOVA
                tags$details(
                    class="card mb-2",
                    tags$summary(class="card-header", strong("Paso 2: Ejecutar y Interpretar el MANCOVA")),
                    tags$div(class="card-body",
                        p("Ahora que el supuesto principal está verificado, corremos el modelo MANCOVA aditivo (sin la interacción)."),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# Modelo MANCOVA aditivo\n",
                                "modelo_mancova <- manova(cbind(Num_Frutos, Vit_C) ~ pH_Inicial + Tratamiento, data = datos_biof)\n\n",
                                "# Ver la prueba multivariada para el efecto del tratamiento\n",
                                "summary(modelo_mancova, test='Pillai')"
                            )
                        ),
                        p(strong("Salida de R y su Interpretación:")),
                        verbatimTextOutput(ns("mancova_case_study_output")),
                        p("El p-valor para `Tratamiento` es significativo. Esto nos da 'luz verde' para investigar qué variables de respuesta individuales son responsables de esta diferencia global.")
                    )
                ),

                # Paso 3: Análisis de Seguimiento
                tags$details(
                    class="card mb-2",
                    tags$summary(class="card-header", strong("Paso 3: Análisis de Seguimiento (ANCOVAs Univariados)")),
                    tags$div(class="card-body",
                        p("Para ver el efecto del tratamiento en cada respuesta ajustada, usamos ", code("summary.aov()"), " sobre nuestro modelo MANCOVA."),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# Obtener los ANCOVAs univariados para cada respuesta\n",
                                "summary.aov(modelo_mancova)"
                            )
                        ),
                        p(strong("Salida de R y su Interpretación:")),
                        verbatimTextOutput(ns("ancova_followup_output")),
                        p("Vemos que, después de ajustar por el pH, el `Tratamiento` tiene un efecto significativo tanto en el `Num_Frutos` como en la `Vit_C`. Ahora podemos proceder con pruebas post-hoc para cada una.")
                    )
                ),
                
                tags$hr(),

                # ---------------------------------------------------------------
                # Subsección 4: Laboratorio Interactivo
                # ---------------------------------------------------------------
                h4(class = "section-header", "2.4 Laboratorio Interactivo: El Poder del Ajuste Multivariado"),
                p("En este laboratorio, simularemos un experimento para demostrar visual e interactivamente cómo el MANCOVA puede revelar efectos de tratamiento que un MANOVA simple podría pasar por alto debido al 'ruido' de una covariable."),

                # El layout principal ahora es un sidebarLayout que contiene pestañas en el panel principal
                sidebarLayout(
                    # --- PANEL DE CONTROL (IZQUIERDA) ---
                    sidebarPanel(
                        width = 3,
                        tags$h5("1. Control de la Simulación"),
                        sliderInput(ns("mancova_cor"), 
                                    tagList("Fuerza de la Covariable", br(), em("(Correlación X↔Y)")),
                                    min = 0, max = 0.95, value = 0.8, step = 0.05),
                        sliderInput(ns("mancova_effect_size"), 
                                    tagList("Magnitud del Efecto de la Dieta", br(), em("(Señal del Tratamiento)")),
                                    min = 0, max = 20, value = 5, step = 1),
                        actionButton(ns("run_ancova_sim"), "Generar/Correr Simulación", icon=icon("play"), class="btn-primary w-100 mt-3"),
                        
                        tags$hr(),
                        
                        tags$h5("2. Descargar Datos"),
                        p(class="small", "Usa este botón para descargar el dataset que acabas de simular y poder replicar el análisis en tu propia consola de R."),
                        downloadButton(ns("download_mancova_sim_data"), "Descargar Datos (CSV)", class="btn-success btn-sm w-100")
                    ),
                    
                    # --- PANEL DE RESULTADOS (DERECHA) ---
                    mainPanel(
                        width = 9,
                        navset_tab(
                            # --- Pestaña 1: Escenario y Datos ---
                            nav_panel(
                                "Escenario y Datos",
                                icon = icon("book-open"),
                                h5("Contexto del Experimento Simulado"),
                                p(
                                    "Evaluamos el efecto de ", strong("dos nuevas dietas (Dieta_A, Dieta_B)"), " contra un ", strong("Control"), " en la ganancia de peso de terneros. Medimos:"
                                ),
                                tags$ul(
                                    tags$li(strong("Respuesta Y₁:"), " Peso Final (kg)."),
                                    tags$li(strong("Respuesta Y₂:"), " Medida de Condición Corporal (escala 1-9).")
                                ),
                                p("Sabemos que el ", strong("Peso Inicial (X)"), " de los terneros varía y afecta ambas respuestas. Este será nuestra ", strong("covariable.")),
                                
                                h6("Datos Simulados:"),
                                DT::dataTableOutput(ns("mancova_sim_table"))
                            ),
                            
                            # --- Pestaña 2: Exploración Visual ---
                            nav_panel(
                                "Exploración Visual",
                                icon = icon("chart-pie"),
                                fluidRow(
                                    column(6, 
                                        h6("Datos Brutos (Visión del MANOVA)"),
                                        plotOutput(ns("mancova_plot_unadjusted_y1"))
                                    ),
                                    column(6, 
                                        plotOutput(ns("mancova_plot_unadjusted_y2"))
                                    )
                                ),
                                hr(),
                                h6("Relación con la Covariable"),
                                plotOutput(ns("mancova_plot_correlation"))
                            ),

                            # --- Pestaña 3: Verificación de Supuestos ---
                            nav_panel(
                                "Verificación de Supuestos",
                                icon = icon("check-double"),
                                p("El supuesto más importante del MANCOVA es la ", strong("homogeneidad de las pendientes de regresión."), " Es decir, la relación entre la covariable y las respuestas debe ser la misma para todos los tratamientos. Lo verificamos probando la significancia de la interacción `Covariable * Tratamiento`."),
                                h6("Prueba del Supuesto de Homogeneidad de Pendientes:"),
                                verbatimTextOutput(ns("mancova_assumption_test")),
                                uiOutput(ns("mancova_assumption_interpretation"))
                            ),
                            
                            # --- Pestaña 4: Comparación de Modelos ---
                            nav_panel(
                                "Resultados: MANOVA vs. MANCOVA",
                                icon = icon("balance-scale"),
                                p(strong("Tu Misión:"), " Ajusta los controles para encontrar un escenario donde el MANOVA no sea significativo, pero el MANCOVA sí lo sea. Esto demuestra cómo el ajuste por la covariable 'rescata' la señal del tratamiento."),
                                fluidRow(
                                    column(6,
                                        h5("Resultados del MANOVA (sin ajustar)"),
                                        verbatimTextOutput(ns("manova_output_sim")),
                                        uiOutput(ns("manova_interpretation"))
                                    ),
                                    column(6, style="border-left: 1px solid #ddd; padding-left: 15px;",
                                        h5("Resultados del MANCOVA (ajustado)"),
                                        verbatimTextOutput(ns("mancova_output_sim")),
                                        uiOutput(ns("mancova_interpretation"))
                                    )
                                )
                            ),

                            # --- Pestaña 5: Código R ---
                            nav_panel(
                                "Código R de Ejemplo",
                                icon = icon("code"),
                                p("Este es el código R que puedes usar en tu propia consola para replicar el análisis completo sobre los datos que descargaste."),
                                tags$pre(class="r-code",
                                    htmltools::HTML(
                                        "# 1. Cargar datos y librerías\n",
                                        "library(dplyr)\n",
                                        "datos <- read.csv('nombre_descargado.csv')\n",
                                        "datos$tratamiento <- as.factor(datos$tratamiento)\n\n",
                                        
                                        "# 2. Verificar supuesto de homogeneidad de pendientes\n",
                                        "modelo_interaccion <- manova(cbind(Respuesta_Y1, Respuesta_Y2) ~ covariable_X * tratamiento, data = datos)\n",
                                        "summary(modelo_interaccion) # Buscamos que 'covariable_X:tratamiento' NO sea significativo\n\n",

                                        "# 3. Correr y comparar MANOVA vs. MANCOVA\n",
                                        "modelo_manova <- manova(cbind(Respuesta_Y1, Respuesta_Y2) ~ tratamiento, data = datos)\n",
                                        "modelo_mancova <- manova(cbind(Respuesta_Y1, Respuesta_Y2) ~ covariable_X + tratamiento, data = datos)\n\n",

                                        "cat('--- RESULTADOS MANOVA ---\\n')\n",
                                        "summary(modelo_manova)\n\n",

                                        "cat('--- RESULTADOS MANCOVA ---\\n')\n",
                                        "summary(modelo_mancova)"
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
                
                # --------------------------------------------------------------------------------------
                # Subsección 3.1: De Comparar Grupos a Modelar Relaciones
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "3.1 ¿Por qué Regresión y no ANOVA? De Grupos a Gradientes"),
                p(
                    "Hasta ahora, hemos usado ANOVA para comparar el efecto de tratamientos ", strong("categóricos"), " (ej. Variedad A vs. Variedad B vs. Variedad C). Pero, ¿qué pasa si nuestro 'tratamiento' no son categorías discretas, sino un ", strong("factor cuantitativo"), " que aplicamos en diferentes niveles, como una dosis de fertilizante (0, 50, 100, 150 kg/ha)?"
                ),

                # --- Comparación Visual de Enfoques ---
                tags$div(class="card mb-4",
                    tags$div(class="card-body",
                        h5(class="card-title text-center", "El Mismo Experimento, Dos Perspectivas Analíticas"),
                        p(class="text-center", "Imaginemos un ensayo de respuesta a la dosis de Nitrógeno. Podríamos analizarlo de dos maneras:"),
                        
                        fluidRow(
                            # Columna para la Visión del ANOVA
                            column(6, style="border-right: 1px solid #ddd;",
                                h6(strong("Visión del ANOVA: 'Cajas Separadas'")),
                                p(class="text-muted", "El ANOVA trata cada dosis como un grupo categórico e independiente. Compara las medias entre estos grupos."),
                                plotOutput(ns("intro_reg_anova_plot"), height = "300px")
                            ),
                            # Columna para la Visión de la Regresión
                            column(6,
                                h6(strong("Visión de la Regresión: 'Buscando la Tendencia'")),
                                p(class="text-muted", "La Regresión aprovecha que las dosis son un continuo numérico. Busca modelar la relación y la tendencia a lo largo de todo el gradiente de dosis."),
                                plotOutput(ns("intro_reg_regression_plot"), height = "300px")
                            )
                        )
                    )
                ),

                h4(class="section-header", "Las Limitaciones del Enfoque ANOVA para Variables Cuantitativas"),
                p(
                    "Aunque técnicamente se puede hacer un ANOVA, este enfoque tiene dos grandes desventajas cuando el factor es cuantitativo:"
                ),
                tags$div(class="row",
                    tags$div(class="col-md-6",
                        tags$div(class="card h-100 border-danger",
                            tags$div(class="card-header bg-danger text-white", strong("1. Pérdida de Información y Poder")),
                            tags$div(class="card-body",
                                p("El ANOVA ignora por completo el orden y la magnitud de las dosis. Para él, los grupos '50 kg/ha' y '100 kg/ha' son tan diferentes como 'Variedad A' y 'Variedad B'. Al no usar la información de que 100 es mayor que 50, pierde poder estadístico para detectar una tendencia sistemática.")
                            )
                        )
                    ),
                    tags$div(class="col-md-6",
                        tags$div(class="card h-100 border-warning",
                            tags$div(class="card-header bg-warning text-dark", strong("2. Incapacidad de Predecir")),
                            p("La pregunta más importante de un ensayo de dosis es: ", em("¿Cuál sería el rendimiento si aplico 75 kg/ha o 120 kg/ha?"), " El ANOVA no puede responder a esto. Solo puede hablar de las medias de los niveles que probaste. La regresión, al generar una ecuación (una línea), nos permite ", strong("interpolar y predecir"), " resultados para dosis que no estaban en el experimento original.")
                        )
                    )
                ),
                p(class="mt-3", strong("Conclusión:"), " Usamos la ", strong("Regresión Lineal"), " cuando nuestro objetivo no es solo comparar, sino ", strong("entender y modelar la relación funcional"), " entre una variable predictora continua y una respuesta continua."),
                
                tags$hr(),

                # --------------------------------------------------------------------------------------
                # Subsección 3.2: Anatomía de la Línea Recta - El Modelo de Regresión
                # --------------------------------------------------------------------------------------

                h4(class = "section-header", "3.2 Anatomía de la Línea Recta: El Modelo de Regresión"),
                p(
                    "La Regresión Lineal Simple busca describir la relación entre una variable predictora continua (X) y una de respuesta (Y) a través de una línea recta. Para entender el modelo, primero veamos el gráfico que intenta generar."
                ),

                # --- Diagrama Visual del Modelo ---
                tags$div(class="card mb-4",
                    tags$div(class="card-header", strong("Visualizando los Componentes de la Regresión")),
                    tags$div(class="card-body",
                        plotOutput(ns("reg_concept_plot"), height="400px")
                    )
                ),

                # --- Explicación de los Componentes ---
                fluidRow(
                    # Columna para el Modelo Matemático
                    column(6, style="border-right: 1px solid #ddd;",
                        h5("El Modelo Matemático"),
                        p("La línea del gráfico se describe con esta famosa ecuación:"),
                        withMathJax(helpText("$$Y_i = \\beta_0 + \\beta_1 X_i + \\epsilon_i$$")),
                        tags$dl(
                            tags$dt("\\(Y_i\\) - La Respuesta Observada"),
                            tags$dd("El valor real que medimos en el campo (ej. el rendimiento de la parcela 'i'). Corresponde a la altura de cada punto azul en el gráfico."),
                            
                            tags$dt("\\(\\beta_0\\) - El Intercepto"),
                            tags$dd("El punto de partida. Es el valor predicho de Y cuando X es cero. Agronómicamente, es el rendimiento base que esperaríamos ", em("sin aplicar nada del predictor.")),

                            tags$dt("\\(\\beta_1\\) - La Pendiente"),
                            tags$dd("El coeficiente más importante. Nos dice cuánto cambia Y (en promedio) por cada unidad que aumenta X. Es la ", strong("tasa de cambio o eficiencia."), " Por ejemplo, cuántos kg de rendimiento ganamos por cada kg de N aplicado."),

                            tags$dt("\\(\\epsilon_i\\) - El Error (o Residuo)"),
                            tags$dd("La distancia vertical entre un punto observado y la línea de regresión (las líneas naranjas discontinuas). Representa la variabilidad que el modelo no puede explicar (el 'ruido').")
                        )
                    ),
                    # Columna para Mínimos Cuadrados
                    column(6,
                        h5("¿Cómo se Encuentra la 'Mejor' Línea?"),
                        p("Podríamos dibujar infinitas líneas a través de esa nube de puntos. R encuentra la línea 'óptima' utilizando el ", strong("Método de Mínimos Cuadrados Ordinarios (MCO).")),
                        
                        tags$div(class="text-center",
                            p(icon("arrows-down-to-line", "fa-3x text-warning"))
                        ),

                        p(strong("La Intuición del MCO:")),
                        p("Imagina que cada residuo (\\(\\epsilon_i\\)) es un resorte elástico que conecta el punto de dato con la línea de regresión. El MCO es un procedimiento matemático que encuentra la única posición e inclinación de la línea que ", strong("minimiza la suma total de la energía (o tensión) de todos esos resortes al cuadrado."), " Es la línea que, en conjunto, pasa 'lo más cerca posible' de todos los puntos de datos simultáneamente."),
                        withMathJax(helpText("$$\\text{MCO minimiza: } \\sum_{i=1}^{n} \\epsilon_i^2 = \\sum_{i=1}^{n} (Y_i - (\\beta_0 + \\beta_1 X_i))^2$$"))
                    )
                ),

                tags$hr(),

                # --------------------------------------------------------------------------------------
                # Subsección 3.3: Encontrando la 'Mejor' Línea: Mínimos Cuadrados Ordinarios (MCO)
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "3.3 Encontrando la 'Mejor' Línea: Mínimos Cuadrados Ordinarios (MCO)"),
                p(
                    "A través de una nube de puntos, podríamos dibujar infinitas líneas rectas. ¿Cómo elige R la 'mejor' línea de ajuste? La respuesta es un procedimiento matemático elegante y fundamental llamado ", strong("Mínimos Cuadrados Ordinarios (MCO)"), " o OLS por sus siglas en inglés (Ordinary Least Squares)."
                ),

                # --- Comparación Visual de Ajustes ---
                tags$div(class="card mb-4",
                    tags$div(class="card-body",
                        h5(class="card-title text-center", "Visualizando el Concepto de 'Mejor Ajuste'"),
                        fluidRow(
                            # Columna para una línea de mal ajuste
                            column(6, style="border-right: 1px solid #ddd;",
                                h6(strong("Intento 1: Una Línea 'Cualquiera'")),
                                p(class="text-muted", "Esta línea roja claramente no representa bien la tendencia de los datos. Observa los grandes errores (distancias verticales)."),
                                plotOutput(ns("mco_bad_fit_plot"), height = "300px")
                            ),
                            # Columna para la línea de buen ajuste (MCO)
                            column(6,
                                h6(strong("Intento 2: La Línea de Mínimos Cuadrados")),
                                p(class="text-muted", "Esta es la línea óptima encontrada por MCO. La suma de las áreas de los cuadrados de los errores es la más pequeña posible."),
                                plotOutput(ns("mco_best_fit_plot"), height = "300px")
                            )
                        )
                    )
                ),

                # --- Explicación del Proceso ---
                h4(class="section-header", "La Lógica del MCO: Minimizando los Errores al Cuadrado"),
                tags$div(class="content-row",
                    tags$div(class="main-content",
                        p(
                            "El principio del MCO es simple:"
                        ),
                        tags$ol(
                            tags$li("Para cualquier línea candidata, se calcula la distancia vertical (el residuo, \\(\\epsilon_i\\)) desde cada punto de dato hasta la línea."),
                            tags$li("Cada uno de estos residuos se eleva al cuadrado. ¿Por qué al cuadrado? Esto tiene dos beneficios: primero, asegura que todos los valores sean positivos (no importa si el punto está por encima o por debajo); segundo, penaliza más fuertemente los errores grandes."),
                            tags$li("Se suman todas estas áreas cuadradas. El resultado es la ", strong("Suma de los Cuadrados de los Errores (SCE).")),
                            tags$li("El MCO es el algoritmo que prueba todas las posibles líneas y encuentra la única combinación de intercepto (\\(\\beta_0\\)) y pendiente (\\(\\beta_1\\)) que produce la ", strong("SCE más pequeña posible.")),
                        ),
                        withMathJax(helpText("$$\\text{MCO busca minimizar: } \\quad SCE = \\sum_{i=1}^{n} \\epsilon_i^2 = \\sum_{i=1}^{n} (Y_i - \\hat{Y}_i)^2$$"))
                    ),
                    tags$div(class="note-cloud",
                        tags$strong("Analogía de los Resortes"),
                        p("Imagina que cada residuo es un resorte que conecta el punto de dato con la línea. La 'energía potencial' de un resorte es proporcional a su longitud al cuadrado."),
                        p("El MCO encuentra la posición y el ángulo de la línea que minimiza la energía total almacenada en todos los resortes. Es la posición de 'mínima tensión' y, por lo tanto, el mejor equilibrio y ajuste general.")
                    )
                ),

                tags$hr(),

                # --------------------------------------------------------------------------------------
                # Subsección 3.4: Laboratorio Interactivo de Regresión
                # --------------------------------------------------------------------------------------
                h4(class = "section-header", "3.4 Laboratorio Interactivo: Construyendo un Modelo de Regresión"),

                # --- Guía y Misión ---
                tags$div(class="card bg-light mb-4",
                    tags$div(class="card-body",
                        h5(icon("compass"), " Guía del Laboratorio y Tu Misión"),
                        p(
                            "En esta simulación, tú eres el arquitecto de un sistema biológico. Usarás los controles de la izquierda para definir la ", strong("relación verdadera"), " entre la dosis de nitrógeno y el rendimiento de un cultivo. Luego, generarás datos de un experimento con 'ruido' aleatorio y observarás cómo el modelo de regresión lineal intenta 'redescubrir' los parámetros que tú estableciste."
                        ),
                        p(strong("Tu Misión:"), " Juega con los controles para responder a estas preguntas:"),
                        tags$ul(
                            tags$li("¿Cómo afecta el 'Ruido Experimental' (σ) a la precisión de las estimaciones y al R²?"),
                            tags$li("¿Cómo afecta el 'Número de Parcelas' (n) a la significancia de la pendiente (su p-valor)?"),
                            tags$li("¿Qué tan cerca están los coeficientes estimados por el modelo de los valores 'reales' que tú definiste?")
                        )
                    )
                ),

                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Parámetros del Sistema Biológico"),
                        sliderInput(ns("reg_intercept"), 
                                    tagList(icon("leaf"), "Rendimiento Base (β₀ Verdadero)"),
                                    min = 10, max = 50, value = 25, step = 1),
                        sliderInput(ns("reg_slope"), 
                                    tagList(icon("chart-line"), "Respuesta a N (β₁ Verdadero)"),
                                    min = -0.5, max = 2, value = 0.8, step = 0.1),
                        sliderInput(ns("reg_error"), 
                                    tagList(icon("wave-square"), "Ruido Experimental (σ Verdadero)"),
                                    min = 1, max = 20, value = 8, step = 1),
                        numericInput(ns("reg_n"), "Número de Parcelas (n):", value=50, min=10, max=200),
                        actionButton(ns("run_reg_sim"), "Generar Nueva Simulación", icon=icon("sync"), class="btn-primary w-100 mt-2")
                    ),
                    mainPanel(
                        width = 9,
                        # Usaremos pestañas para la salida
                        navset_card_pill(
                            nav_panel(
                                "Gráfico de Regresión",
                                plotOutput(ns("reg_plot"))
                            ),
                            nav_panel(
                                "Resumen e Interpretación",
                                verbatimTextOutput(ns("reg_summary_output")),
                                uiOutput(ns("reg_interpretation_ui"))
                            ),
                            nav_panel(
                                "Diagnóstico del Modelo",
                                p("Incluso en una simulación, es una buena práctica verificar los supuestos del modelo sobre los residuos. ¿Se distribuyen normalmente? ¿Tienen varianza constante?"),
                                plotOutput(ns("reg_diagnostic_plot"))
                            )
                        )
                    )
                ),
                tags$hr(),
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
    ### ---- Subsección 2.3 ---
    # Reactive para generar los datos del caso de estudio
    case_study_mancova_data <- reactive({
        set.seed(123) # Semilla fija para consistencia
        n_rep <- 25
        
        # Crear datos base
        df <- data.frame(
            Tratamiento = factor(rep(c('Control', 'BioF_A', 'BioF_B'), each = n_rep))
        ) %>%
        mutate(
            # Covariable: pH inicial del suelo
            pH_Inicial = rnorm(n(), mean = 6.5, sd = 0.4),
            
            # Efectos del tratamiento
            efecto_frutos = case_when(
                Tratamiento == 'Control' ~ 0,
                Tratamiento == 'BioF_A'  ~ 5,
                Tratamiento == 'BioF_B'  ~ 2
            ),
            efecto_vitc = case_when(
                Tratamiento == 'Control' ~ 0,
                Tratamiento == 'BioF_A'  ~ -1.5,
                Tratamiento == 'BioF_B'  ~ 3
            ),
            
            # Respuestas finales, influenciadas por tratamiento, covariable y error
            Num_Frutos = 20 + efecto_frutos + (pH_Inicial - 6.5) * 5 + rnorm(n(), 0, 4),
            Vit_C = 40 + efecto_vitc - (pH_Inicial - 6.5) * 3 + rnorm(n(), 0, 5)
        )
        
        return(df)
    })

    # Lógica para el botón de descarga
    output$download_mancova_data <- downloadHandler(
        filename = function() {
            "datos_biofertilizantes_ejemplo.csv"
        },
        content = function(file) {
            write.csv(case_study_mancova_data(), file, row.names = FALSE)
        }
    )

    # Salida para el resultado del MANCOVA principal
    output$mancova_case_study_output <- renderPrint({
        datos <- case_study_mancova_data()
        modelo_mancova <- manova(cbind(Num_Frutos, Vit_C) ~ pH_Inicial + Tratamiento, data = datos)
        summary(modelo_mancova, test = 'Pillai')
    })

    # Salida para los ANCOVAs de seguimiento
    output$ancova_followup_output <- renderPrint({
        datos <- case_study_mancova_data()
        modelo_mancova <- manova(cbind(Num_Frutos, Vit_C) ~ pH_Inicial + Tratamiento, data = datos)
        summary.aov(modelo_mancova)
    })

    ### ---- Subsección 2.4 ----
    mancova_sim_results <- eventReactive(input$run_ancova_sim, { # Usamos el ID del botón de la UI
        
        set.seed(as.integer(Sys.time()))
        n_rep <- 25; n_trat <- 3
        
        correlacion <- input$mancova_cor
        efecto_trat_mag <- input$mancova_effect_size
        
        # Crear vectores
        tratamiento_vec <- factor(rep(c('Control', 'Dieta_A', 'Dieta_B'), each = n_rep))
        efecto_y1_vec <- c(rep(0, n_rep), rep(efecto_trat_mag, n_rep), rep(efecto_trat_mag * 0.8, n_rep))
        efecto_y2_vec <- c(rep(0, n_rep), rep(efecto_trat_mag * -0.5, n_rep), rep(efecto_trat_mag * 0.2, n_rep))
        covariable_x_vec <- rnorm(n_rep * n_trat, mean = 250, sd = 25)
        error_y1_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 15)
        error_y2_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 1)
        
        # Generar respuestas
        respuesta_Y1_vec <- 300 + efecto_y1_vec + ((covariable_x_vec - mean(covariable_x_vec)) * correlacion * 2) + (error_y1_vec * sqrt(1 - correlacion^2))
        respuesta_Y2_vec <- 5 + efecto_y2_vec + ((covariable_x_vec - mean(covariable_x_vec)) * correlacion * 0.05) + (error_y2_vec * sqrt(1 - correlacion^2))
        
        df_sim <- data.frame(
            Tratamiento = tratamiento_vec,
            Peso_Inicial = covariable_x_vec,
            Peso_Final = respuesta_Y1_vec,
            Condicion_Corporal = respuesta_Y2_vec
        )
        
        # Ajustar los modelos necesarios
        modelo_manova <- manova(cbind(Peso_Final, Condicion_Corporal) ~ Tratamiento, data = df_sim)
        modelo_mancova <- manova(cbind(Peso_Final, Condicion_Corporal) ~ Peso_Inicial + Tratamiento, data = df_sim)
        modelo_supuesto <- manova(cbind(Peso_Final, Condicion_Corporal) ~ Peso_Inicial * Tratamiento, data = df_sim)
        
        list(
            datos = df_sim,
            manova_summary = summary(modelo_manova, test = "Pillai"),
            mancova_summary = summary(modelo_mancova, test = "Pillai"),
            assumption_summary = summary(modelo_supuesto, test = "Pillai")
        )
    }, ignoreNULL = FALSE)

    # --- Salidas para la Pestaña "Escenario y Datos" ---
    output$mancova_sim_table <- DT::renderDataTable({
        res <- mancova_sim_results(); req(res)
        DT::datatable(res$datos, options=list(pageLength=5, scrollX=TRUE), rownames=FALSE)
    })

    # --- Salidas para la Pestaña "Exploración Visual" ---
    output$mancova_plot_unadjusted_y1 <- renderPlot({
        res <- mancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Tratamiento, y = Peso_Final, fill = Tratamiento)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            theme_minimal(base_size = 12) +
            labs(title = "Respuesta 1: Peso Final", y = "Peso Final (kg)")
    })
    output$mancova_plot_unadjusted_y2 <- renderPlot({
        res <- mancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Tratamiento, y = Condicion_Corporal, fill = Tratamiento)) +
            geom_boxplot(alpha = 0.7, show.legend = FALSE) +
            theme_minimal(base_size = 12) +
            labs(title = "Respuesta 2: Condición Corporal", y = "Escala de Condición")
    })
    output$mancova_plot_correlation <- renderPlot({
        res <- mancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Peso_Inicial, y = Peso_Final, color = Tratamiento)) +
            geom_point(alpha = 0.6) +
            geom_smooth(method = "lm", se = FALSE, linetype="dashed") +
            theme_minimal(base_size = 12) +
            labs(title = "Relación entre Covariable y Respuesta Principal", x="Peso Inicial (kg)", y="Peso Final (kg)")
    })

    # --- Salidas para la Pestaña "Verificación de Supuestos" ---
    output$mancova_assumption_test <- renderPrint({
        res <- mancova_sim_results(); req(res)
        print(res$assumption_summary)
    })
    output$mancova_assumption_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        p_val_int <- res$assumption_summary$stats["Peso_Inicial:Tratamiento", "Pr(>F)"]
        
        if(is.na(p_val_int)) return()
        
        if (p_val_int > 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"), 
                strong("Supuesto Cumplido."), " El p-valor de la interacción (", round(p_val_int, 3), ") es > 0.05. No hay evidencia de que las pendientes de regresión sean diferentes. Podemos proceder con el MANCOVA.")
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"),
                strong("Supuesto Violado."), " El p-valor de la interacción (", round(p_val_int, 3), ") es <= 0.05. Las pendientes no son homogéneas. El MANCOVA no es el modelo apropiado.")
        }
    })

    # --- Salidas para la Pestaña "Resultados: MANOVA vs. MANCOVA" ---
    output$manova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        print(res$manova_summary)
    })
    output$mancova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        print(res$mancova_summary)
    })

    # Interpretaciones dinámicas
    output$manova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        p_val <- res$manova_summary$stats["Tratamiento", "Pr(>F)"]
        if(is.na(p_val)) return()
        # (El resto de la lógica de if/else que ya tenías)
    })
    output$mancova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        p_val <- res$mancova_summary$stats["Tratamiento", "Pr(>F)"]
        if(is.na(p_val)) return()
        # (El resto de la lógica de if/else que ya tenías)
    })

    # --- Lógica para el botón de descarga ---
    output$download_mancova_sim_data <- downloadHandler(
        filename = function() { "datos_simulados_mancova.csv" },
        content = function(file) {
            res <- mancova_sim_results()
            if (!is.null(res)) {
                write.csv(res$datos, file, row.names = FALSE)
            }
        }
    )

    # --- LÓGICA PARA LA PESTAÑA 3: REGRESIÓN LINEAL SIMPLE ---

    ### ---- Subsección 3.1 ----
    # Crear un dataset conceptual una sola vez para ambos gráficos
    conceptual_data <- reactive({
        set.seed(123)
        dosis <- rep(c(0, 50, 100, 150), each = 15)
        rendimiento <- 30 + 0.15 * dosis + rnorm(length(dosis), 0, 8)
        data.frame(
            Dosis_N = dosis,
            Rendimiento = rendimiento,
            Dosis_Factor = as.factor(dosis) # Versión categórica para el ANOVA
        )
    })

    # Gráfico que muestra la visión del ANOVA
    output$intro_reg_anova_plot <- renderPlot({
        df <- conceptual_data()
        ggplot(df, aes(x = Dosis_Factor, y = Rendimiento, fill = Dosis_Factor)) +
            geom_boxplot(show.legend = FALSE) +
            labs(
                subtitle = "Pregunta: ¿Son diferentes las medias?",
                x = "Dosis de N (tratada como categoría)",
                y = "Rendimiento"
            ) +
            theme_minimal(base_size = 12)
    })

    # Gráfico que muestra la visión de la Regresión
    output$intro_reg_regression_plot <- renderPlot({
        df <- conceptual_data()
        ggplot(df, aes(x = Dosis_N, y = Rendimiento)) +
            geom_point(alpha = 0.6, color = "darkblue", size = 2.5) +
            # La línea de regresión es la protagonista
            geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 1.5) +
            labs(
                subtitle = "Pregunta: ¿Cuál es la tendencia?",
                x = "Dosis de N (tratada como continuo)",
                y = "Rendimiento"
            ) +
            theme_minimal(base_size = 12)
    })

    ### ---- Subsección 3.2 ----
    # Gráfico conceptual de la anatomía de la regresión
    output$reg_concept_plot <- renderPlot({
        set.seed(101)
        x <- seq(5, 95, length.out = 15)
        y <- 20 + 0.7*x + rnorm(length(x), 0, 8)
        df <- data.frame(X=x, Y=y)
        model <- lm(Y ~ X, data=df)
        
        df$pred <- predict(model)
        df$resid_start <- pmin(df$Y, df$pred) # Para dibujar desde el punto más bajo
        df$resid_end <- pmax(df$Y, df$pred)   # hasta el más alto
        
        # Encontrar los coeficientes
        b0 <- coef(model)[1]
        b1 <- coef(model)[2]
        
        ggplot(df, aes(x = X, y = Y)) +
            # Segmentos de los residuos
            geom_segment(aes(xend = X, yend = pred), color = "orange", linetype = "dashed", linewidth=1) +
            # Línea de regresión
            geom_smooth(method = "lm", formula = y~x, se = FALSE, color = "red", linewidth=1.5, fullrange = TRUE) +
            # Puntos de datos
            geom_point(size = 4, color = "darkblue", shape=21, fill="lightblue", stroke=1.2) +
            
            # Anotaciones claras
            annotate("text", x=8, y=b0, label=paste0("β₀ ≈ ", round(b0,1)), color="darkred", size=5, hjust=0) +
            geom_point(aes(x=0, y=b0), color="darkred", size=5) +
            
            annotate("text", x=70, y=75, label=paste0("Pendiente (β₁) ≈ ", round(b1,2)), color="darkred", size=5, angle=atan(b1)*(180/pi)) +
            
            annotate("segment", x=30, y=45, xend=40, yend=40, arrow=arrow(length=unit(0.2,"cm")), color="darkorange") +
            annotate("text", x=28, y=45, label="εᵢ (Residuo)", color="darkorange", size=5, hjust=1) +
            
            theme_bw(base_size = 14) +
            labs(x="Variable Predictora (X) - ej. Dosis de N", 
                y="Variable de Respuesta (Y) - ej. Rendimiento",
                title="Anatomía de un Modelo de Regresión Lineal Simple") +
            coord_cartesian(xlim=c(0, 100), ylim=c(0, 100)) # Fijar los ejes para consistencia
    })
    
    ### ---- Subsección 3.3 ----
    # Crear un dataset conceptual una sola vez para ambos gráficos de MCO
    mco_conceptual_data <- reactive({
        set.seed(50)
        x <- seq(10, 90, length.out = 10)
        y <- 15 + 0.8*x + rnorm(length(x), 0, 12)
        df <- data.frame(X=x, Y=y)
        return(df)
    })

    # Gráfico que muestra un mal ajuste
    output$mco_bad_fit_plot <- renderPlot({
        df <- mco_conceptual_data()
        # Una línea "mala" con intercepto y pendiente incorrectos
        df$pred_mala <- 30 + 0.4 * df$X 
        
        ggplot(df, aes(x = X, y = Y)) +
            # Cuadrados de los errores (grandes)
            geom_rect(aes(xmin=X-2, xmax=X+2, ymin=pmin(Y, pred_mala), ymax=pmax(Y, pred_mala)), 
                    fill="orange", alpha=0.3) +
            geom_segment(aes(xend = X, yend = pred_mala), color = "orange", linetype = "dashed") +
            geom_line(aes(y=pred_mala), color="red", linewidth=1.2) +
            geom_point(size = 3, color = "darkblue") +
            theme_minimal(base_size = 12) + labs(x="X", y="Y")
    })

    # Gráfico que muestra el mejor ajuste (MCO)
    output$mco_best_fit_plot <- renderPlot({
        df <- mco_conceptual_data()
        model <- lm(Y ~ X, data=df)
        df$pred_buena <- predict(model)
        
        ggplot(df, aes(x = X, y = Y)) +
            # Cuadrados de los errores (minimizados)
            geom_rect(aes(xmin=X-2, xmax=X+2, ymin=pmin(Y, pred_buena), ymax=pmax(Y, pred_buena)), 
                    fill="skyblue", alpha=0.3) +
            geom_segment(aes(xend = X, yend = pred_buena), color = "blue", linetype = "dashed") +
            geom_smooth(method="lm", se=FALSE, color="red", linewidth=1.2) +
            geom_point(size = 3, color = "darkblue") +
            theme_minimal(base_size = 12) + labs(x="X", y="Y")
    })

    ### ---- Subsección 3.4 ----
    # Reactive para los datos y el modelo de la simulación
    # Usamos eventReactive para que solo se actualice al presionar el botón
    reg_sim_results <- eventReactive(input$run_reg_sim, {
        req(input$reg_intercept, input$reg_slope, input$reg_error, input$reg_n)
        set.seed(as.integer(Sys.time())) # Nueva simulación cada vez
        
        # Parámetros "verdaderos" definidos por el usuario
        beta_0_real <- input$reg_intercept
        beta_1_real <- input$reg_slope
        sigma_real <- input$reg_error
        n_real <- input$reg_n
        
        # Simular los datos
        dosis_N <- runif(n_real, min = 0, max = 150)
        rendimiento <- beta_0_real + beta_1_real * dosis_N + rnorm(n_real, 0, sigma_real)
        
        df <- data.frame(Dosis_N = dosis_N, Rendimiento = rendimiento)
        
        # Ajustar el modelo lineal
        modelo <- lm(Rendimiento ~ Dosis_N, data = df)
        
        # Devolver todo en una lista
        list(
            datos = df,
            modelo = modelo,
            parametros_reales = list(b0 = beta_0_real, b1 = beta_1_real, sigma = sigma_real)
        )
    }, ignoreNULL = FALSE)

    # Gráfico interactivo mejorado
    output$reg_plot <- renderPlot({
        res <- reg_sim_results(); req(res)
        
        ggplot(res$datos, aes(x = Dosis_N, y = Rendimiento)) +
            # Línea de la "Verdad" (definida por el usuario)
            geom_abline(
                intercept = res$parametros_reales$b0, 
                slope = res$parametros_reales$b1,
                color = "black", linetype = "dashed", linewidth = 1.2
            ) +
            # Línea del modelo ajustado por R
            geom_smooth(method = "lm", se = TRUE, color = "red", fill="red", alpha=0.1) +
            # Puntos de datos
            geom_point(alpha = 0.6, color = "darkgreen") +
            
            labs(
                title = "Resultado de la Simulación: Verdad vs. Estimación del Modelo",
                subtitle = "Línea discontinua = Relación 'Verdadera' | Línea roja = Modelo ajustado por R",
                x = "Dosis de Nitrógeno Aplicada (kg/ha)",
                y = "Rendimiento del Cultivo (qq/ha)"
            ) +
            theme_bw(base_size = 14)
    })

    # Salida del resumen del modelo
    output$reg_summary_output <- renderPrint({
        res <- reg_sim_results(); req(res)
        summary(res$modelo)
    })

    # Interpretación dinámica mejorada
    output$reg_interpretation_ui <- renderUI({
        res <- reg_sim_results(); req(res)
        
        # Coeficientes estimados por el modelo
        coefs_estimados <- coef(summary(res$modelo))
        b0_estimado <- coefs_estimados[1, 1]
        b1_estimado <- coefs_estimados[2, 1]
        
        # Parámetros reales de la simulación
        b0_real <- res$parametros_reales$b0
        b1_real <- res$parametros_reales$b1
        
        # Resultados de la prueba
        p_val_slope <- coefs_estimados[2, 4]
        r_sq_adj <- summary(res$modelo)$adj.r.squared

        tagList(
            h5("Comparación: Parámetros Verdaderos vs. Estimados"),
            tags$table(class="table table-sm table-bordered",
                tags$thead(tags$tr(tags$th("Parámetro"), tags$th("Valor Verdadero (Simulación)"), tags$th("Valor Estimado (Modelo)"))),
                tags$tbody(
                    tags$tr(tags$td(strong("Intercepto (β₀)")), tags$td(b0_real), tags$td(round(b0_estimado, 2))),
                    tags$tr(tags$td(strong("Pendiente (β₁)")), tags$td(b1_real), tags$td(round(b1_estimado, 4)))
                )
            ),
            h5("Interpretación del Ajuste del Modelo"),
            tags$ul(
                tags$li(HTML(paste0("<b>Significancia de la Pendiente:</b> El p-valor para la Dosis de N es ", strong(format.pval(p_val_slope, digits=3)), ". ", 
                                    ifelse(p_val_slope < 0.05, "Esto indica que el modelo detectó exitosamente la relación lineal.", "El modelo no pudo detectar una relación significativa (probablemente por un efecto pequeño, pocas repeticiones o mucho ruido).")))),
                tags$li(HTML(paste0("<b>Calidad del Ajuste (R² Adj.):</b> El modelo explica aproximadamente el ", strong(round(r_sq_adj*100, 1)), "% de la variabilidad en el Rendimiento.")))
            )
        )
    })

    # Gráfico de diagnóstico del modelo
    output$reg_diagnostic_plot <- renderPlot({
        res <- reg_sim_results(); req(res)
        par(mfrow = c(2,2))
        plot(res$modelo)
        par(mfrow = c(1,1))
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