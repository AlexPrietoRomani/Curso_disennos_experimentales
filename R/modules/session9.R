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
                # ---------------------------------------------------------------
                # Subsección 1.4: Ejemplo Práctico en R
                # ---------------------------------------------------------------
                h4(class = "section-header", "1.4 Ejemplo Práctico: Ajustando el Crecimiento de Plantas"),
                p("Usaremos el dataset `weight` del paquete `agricolae`, que contiene datos del peso de plantas bajo diferentes tratamientos. Usaremos la altura inicial (`cov1`) como covariable para ajustar el peso final (`yield`)."),
                tags$pre(class="r-code",
                    htmltools::HTML(
                        "library(agricolae)\n",
                        "data(growth)\n\n",
                        "# Modelo ANOVA simple (sin covariable)\n",
                        "modelo_anova <- aov(yield ~ treat, data = growth)\n",
                        "cat('--- Tabla ANOVA ---\\n')\n",
                        "print(summary(modelo_anova))\n\n",
                        
                        "# Modelo ANCOVA (ajustando por altura inicial 'cov1')\n",
                        "modelo_ancova <- aov(yield ~ cov1 + treat, data = growth)\n",
                        "cat('--- Tabla ANCOVA ---\\n')\n",
                        "print(summary(modelo_ancova))\n\n",
                        
                        "# Cálculo de la Eficiencia Relativa\n",
                        "cme_anova <- anova(modelo_anova)['Residuals', 'Mean Sq']\n",
                        "cme_ancova <- anova(modelo_ancova)['Residuals', 'Mean Sq']\n",
                        "eficiencia_relativa <- cme_anova / cme_ancova\n",
                        "cat(paste0('\\nEficiencia Relativa (ER): ', round(eficiencia_relativa, 2)))"
                    )
                ),
                tags$hr(),
                # ---------------------------------------------------------------
                # Subsección 1.5: Laboratorio Interactivo
                # ---------------------------------------------------------------
                h4(class = "section-header", "1.5 Laboratorio Interactivo: El Poder del Ajuste"),

                # --- Usaremos navset_card_pill para organizar la explicación y el laboratorio ---
                navset_card_pill(
                    header = tags$h5("Guía del Laboratorio y Simulación"),
                    
                    # Pestaña con la explicación del escenario
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
                                sliderInput(ns("ancova_cor"), "Correlación (Peso Inicial ↔ Final):",
                                            min = 0, max = 0.95, value = 0.8, step = 0.05),
                                sliderInput(ns("ancova_effect_size"), "Magnitud del Efecto de la Dieta:", # Nuevo ID para el efecto
                                            min = 0, max = 20, value = 5, step = 1),
                                actionButton(ns("run_ancova_sim"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100")
                            ),
                            mainPanel(
                                width = 9,
                                # Gráfico principal que muestra la relación
                                plotOutput(ns("ancova_plot")),
                                hr(),
                                
                                # Fila con los resultados comparativos
                                fluidRow(
                                    column(6,
                                        h5(icon("balance-scale-left"), "Análisis SIN Ajuste (ANOVA)"),
                                        verbatimTextOutput(ns("anova_output_sim")),
                                        uiOutput(ns("anova_interpretation_ui")) # UI para interpretación
                                    ),
                                    column(6, style="border-left: 1px solid #ddd; padding-left: 20px;",
                                        h5(icon("balance-scale-right"), "Análisis CON Ajuste (ANCOVA)"),
                                        verbatimTextOutput(ns("ancova_output_sim")),
                                        uiOutput(ns("ancova_interpretation_ui")) # UI para interpretación
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
                h4(class = "section-header", "2.3 MANCOVA: Combinando Mundos - Múltiples Respuestas y Covariables"),
                    p(
                        "Hemos visto que el MANOVA maneja múltiples variables de respuesta y que el ANCOVA ajusta una respuesta por una covariable. El siguiente paso lógico es combinar ambos conceptos: ¿qué hacemos cuando tenemos ", strong("múltiples variables de respuesta"), " Y también una ", strong("covariable"), " que podría estar influyendo en todas ellas?"
                    ),
                    p(
                        "La respuesta es el ", strong("Análisis Multivariado de la Covarianza (MANCOVA)."), " Es la herramienta más completa que hemos visto hasta ahora, ya que nos permite probar las diferencias entre los centroides de los tratamientos DESPUÉS de haber ajustado estadísticamente todas las variables de respuesta por el efecto de la covariable."
                    ),
                    tags$div(class = "note-cloud",
                        tags$strong("Ejemplo Agronómico Concreto:"),
                        p("Queremos comparar 3 nuevos regímenes de fertilización (Tratamiento). Medimos dos respuestas: ", strong("Rendimiento (Y₁)"), " y ", strong("Contenido de Proteína (Y₂)."), " Sin embargo, sabemos que el contenido de ", strong("materia orgánica inicial (X)"), " de cada parcela era variable y podría afectar tanto al rendimiento como a la proteína. El MANCOVA nos permite responder: 'Controlando el efecto de la materia orgánica inicial, ¿los regímenes de fertilización producen perfiles diferentes de rendimiento y proteína?'")
                    ),

                    h4(class = "section-header", "Modelo y Supuestos del MANCOVA"),
                    p("El modelo MANCOVA es una extensión directa del ANCOVA al caso multivariado. La clave es que la covariable (X) se usa para ajustar cada una de las variables de respuesta (Y₁, Y₂, etc.)."),
                    withMathJax(helpText(
                        "$$\\text{Modelo para } Y_1: Y_{1ij} = \\mu_1 + \\tau_{1i} + \\beta_1(X_{ij} - \\bar{X}_{..}) + \\epsilon_{1ij}$$"
                    )),
                    withMathJax(helpText(
                        "$$\\text{Modelo para } Y_2: Y_{2ij} = \\mu_2 + \\tau_{2i} + \\beta_2(X_{ij} - \\bar{X}_{..}) + \\epsilon_{2ij}$$"
                    )),
                    p("El MANCOVA prueba si los efectos del tratamiento (\\(\\tau_{1i}, \\tau_{2i}, ...\\)) son significativamente diferentes de cero de forma conjunta."),
                    p(strong("Supuesto Adicional Importante:"), " Además de los supuestos del MANOVA, el MANCOVA asume que la relación (la pendiente de la regresión, \\(\\beta\\)) entre la covariable y cada una de las variables de respuesta es la misma para todos los grupos de tratamiento. Esto se conoce como el supuesto de ", strong("homogeneidad de las pendientes de regresión.")),

                    h4(class = "section-header", "Implementación en R"),
                    p("La sintaxis en R es una combinación de las que ya hemos visto. Usamos `cbind()` para las respuestas y simplemente añadimos la covariable al modelo."),
                    tags$pre(class="r-code",
                        htmltools::HTML(
                            "# Ejemplo de MANCOVA en R usando el dataset 'iris'\n",
                            "# Pregunta: Controlando el efecto del Ancho del Sépalo (covariable), ¿difiere el\n",
                            "# 'perfil del pétalo' (Largo y Ancho) entre las especies?\n\n",
                            "modelo_mancova <- manova(cbind(Petal.Length, Petal.Width) ~ Sepal.Width + Species, data = iris)\n\n",
                            "# Ver el resumen de la prueba multivariada\n",
                            "summary(modelo_mancova, test = 'Pillai')"
                        )
                    ),

                    tags$hr(),

                    h4(class = "section-header", "2.4 Laboratorio Interactivo: El Poder de Ajustar con MANCOVA"),

                    # --- Usaremos navset_card_pill para organizar la explicación y el laboratorio ---
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
                        nav_panel(
                            "Laboratorio de Simulación",
                            sidebarLayout(
                                sidebarPanel(
                                    width = 3,
                                    tags$h5("Control de la Simulación"),
                                    sliderInput(ns("mancova_cor"), "Correlación (Fósforo ↔ Respuestas):",
                                                min = 0, max = 0.95, value = 0.8, step = 0.05),
                                    sliderInput(ns("mancova_trat_effect"), "Magnitud del Efecto del Bioestimulante:",
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

    ### ---- Subsección 1.5 ----
    ancova_sim_results <- eventReactive(input$run_ancova_sim, {
    
        set.seed(as.integer(Sys.time()))
        n_rep <- 20; n_trat <- 3
        
        # Parámetros de la simulación
        correlacion <- input$ancova_cor
        efecto_trat_mag <- input$ancova_effect_size # Usar el nuevo ID
        sd_error_residual <- 15 # Fijamos un error residual para simplificar
        
        # Crear datos base
        dieta <- factor(rep(c('Control', 'Dieta_A', 'Dieta_B'), each = n_rep))
        
        # Simular el efecto del tratamiento
        efecto_trat <- c(rep(0, n_rep), rep(efecto_trat_mag, n_rep), rep(efecto_trat_mag * 0.5, n_rep))
        
        # Simular la covariable (Peso Inicial)
        peso_inicial_x <- rnorm(n_rep * n_trat, mean = 250, sd = 20)
        
        # Simular el error aleatorio puro
        error_y <- rnorm(n_rep * n_trat, mean = 0, sd = sd_error_residual)
        
        # Generar la variable de respuesta (Peso Final)
        # Y = Peso_Base + Efecto_Dieta + Efecto_Peso_Inicial + Error
        peso_final_y <- 300 + efecto_trat + ((peso_inicial_x - mean(peso_inicial_x)) * correlacion * 2) + (error_y * sqrt(1 - correlacion^2))
        
        df_sim <- data.frame(
            Dieta = dieta,
            Peso_Inicial = peso_inicial_x,
            Peso_Final = peso_final_y
        )
        
        # Ajustar ambos modelos
        modelo_anova <- aov(Peso_Final ~ Dieta, data = df_sim)
        modelo_ancova <- aov(Peso_Final ~ Peso_Inicial + Dieta, data = df_sim)
        
        # Extraer CME y calcular ER
        cme_anova <- anova(modelo_anova)['Residuals', 'Mean Sq']
        cme_ancova <- anova(modelo_ancova)['Residuals', 'Mean Sq']
        
        if (is.na(cme_ancova) || cme_ancova == 0) return(NULL)
        eficiencia_relativa <- cme_anova / cme_ancova
        
        list(
            datos = df_sim,
            modelo_anova = modelo_anova,
            modelo_ancova = modelo_ancova,
            cme_anova = cme_anova,
            cme_ancova = cme_ancova,
            er = eficiencia_relativa
        )
    }, ignoreNULL = FALSE)

    # Gráfico principal
    output$ancova_plot <- renderPlot({
        res <- ancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = Peso_Inicial, y = Peso_Final, color = Dieta)) +
            geom_point(size = 3, alpha = 0.7) +
            geom_smooth(method = "lm", se = FALSE, linewidth = 1, linetype = "dashed") +
            labs(
                title = "Relación entre Peso Inicial (Covariable) y Peso Final (Respuesta)",
                subtitle = "Las líneas discontinuas muestran la tendencia para cada dieta",
                x = "Peso Inicial (kg)",
                y = "Peso Final (kg)"
            ) +
            theme_minimal(base_size = 14)
    })

    # Salidas de texto para los modelos
    output$anova_output_sim <- renderPrint({
        res <- ancova_sim_results(); req(res)
        cat("--- Tabla ANOVA ---\n")
        summary(res$modelo_anova)
    })

    output$ancova_output_sim <- renderPrint({
        res <- ancova_sim_results(); req(res)
        cat("--- Tabla ANCOVA ---\n")
        # Usamos anova() en lugar de summary() para ver la tabla Tipo I secuencial, que es más clara aquí
        anova(res$modelo_ancova)
    })

    # Interpretaciones dinámicas
    output$anova_interpretation_ui <- renderUI({
        res <- ancova_sim_results(); req(res)
        p_val <- summary(res$modelo_anova)[[1]]["Dieta", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2",
                icon("check-circle"),
                strong("Conclusión: Significativo."), " El ANOVA simple fue capaz de detectar una diferencia entre las dietas.")
        } else {
            div(class="alert alert-danger mt-2",
                icon("times-circle"),
                strong("Conclusión: No Significativo."), " El 'ruido' causado por la variación en el Peso Inicial es tan grande que oculta el efecto de las dietas.")
        }
    })

    output$ancova_interpretation_ui <- renderUI({
        res <- ancova_sim_results(); req(res)
        p_val <- anova(res$modelo_ancova)["Dieta", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2",
                icon("check-circle"),
                strong("Conclusión: Significativo."), " Después de 'limpiar' el efecto del Peso Inicial, el ANCOVA revela que sí existe un efecto real de las dietas. ¡El ajuste funcionó!")
        } else {
            div(class="alert alert-danger mt-2",
                icon("times-circle"),
                strong("Conclusión: No Significativo."), " Incluso después de ajustar por el Peso Inicial, no hay evidencia de un efecto de las dietas. Probablemente no son efectivas.")
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
    ### ---- Subsección 4 ----
    mancova_sim_results <- eventReactive(input$run_mancova_sim, {
        
        set.seed(as.integer(Sys.time()))
        n_rep <- 20
        n_trat <- 3
        
        # Parámetros de la simulación
        correlacion <- input$mancova_cor
        efecto_trat_mag <- input$mancova_trat_effect
        
        # Crear datos base
        tratamiento_vec <- factor(rep(c('Control', 'Trat_A', 'Trat_B'), each = n_rep))
        
        # Efecto del tratamiento sobre las respuestas
        efecto_y1_vec <- c(rep(0, n_rep), rep(efecto_trat_mag, n_rep), rep(efecto_trat_mag * 0.8, n_rep))
        efecto_y2_vec <- c(rep(0, n_rep), rep(efecto_trat_mag * -0.5, n_rep), rep(efecto_trat_mag * 0.2, n_rep))
        
        # Covariable (X) que afecta a ambas respuestas
        covariable_x_vec <- rnorm(n_rep * n_trat, mean = 50, sd = 10)
        
        # Errores aleatorios para cada respuesta
        error_y1_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 8)
        error_y2_vec <- rnorm(n_rep * n_trat, mean = 0, sd = 5)
        
        # Generar las variables de respuesta
        # Y = efecto_trat + efecto_covariable + error
        respuesta_Y1_vec <- 20 + efecto_y1_vec + (covariable_x_vec * correlacion) + (error_y1_vec * sqrt(1 - correlacion^2))
        respuesta_Y2_vec <- 15 + efecto_y2_vec + (covariable_x_vec * correlacion * 0.5) + (error_y2_vec * sqrt(1 - correlacion^2)) # La covariable afecta a Y2 con la mitad de fuerza
        
        # Construimos el data.frame asignando explícitamente los vectores a nombres de columna
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
        ggplot(res$datos, aes(x = tratamiento, y = respuesta_Y1, fill = tratamiento)) +
            geom_boxplot(alpha = 0.7) +
            theme_minimal() +
            labs(title = "Respuesta Y1 (ej. Rendimiento) sin ajustar", y = "Respuesta 1")
    })

    # Gráfico para la Respuesta 2
    output$mancova_plot_y2 <- renderPlot({
        res <- mancova_sim_results(); req(res)
        ggplot(res$datos, aes(x = tratamiento, y = respuesta_Y2, fill = tratamiento)) +
            geom_boxplot(alpha = 0.7) +
            theme_minimal() +
            labs(title = "Respuesta Y2 (ej. Proteína) sin ajustar", y = "Respuesta 2")
    })

    # Salida para el MANOVA
    output$manova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        cat("--- Prueba Global MANOVA (sin covariable) ---\n")
        print(res$manova_summary)
    })

    # Salida para el MANCOVA
    output$mancova_output_sim <- renderPrint({
        res <- mancova_sim_results(); req(res)
        cat("--- Prueba Global MANCOVA (ajustada por covariable) ---\n")
        print(res$mancova_summary)
    })

    # Interpretación para el MANOVA
    output$manova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        p_val <- res$manova_summary$stats["tratamiento", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2",
                icon("check-circle"),
                strong("Conclusión: Significativo."), " Basado en los datos brutos, hay evidencia de que al menos un bioestimulante tiene un perfil de Rendimiento/Dulzura diferente al de los otros."
            )
        } else {
            div(class="alert alert-danger mt-2",
                icon("times-circle"),
                strong("Conclusión: No Significativo."), " Basado en los datos brutos, no hay evidencia suficiente para decir que los tratamientos difieren. El 'ruido' podría estar ocultando el efecto."
            )
        }
    })

    # Interpretación para el MANCOVA
    output$mancova_interpretation <- renderUI({
        res <- mancova_sim_results(); req(res)
        p_val <- res$mancova_summary$stats["tratamiento", "Pr(>F)"]
        
        if (p_val < 0.05) {
            div(class="alert alert-success mt-2",
                icon("check-circle"),
                strong("Conclusión: Significativo."), " Después de ajustar por las diferencias en Fósforo inicial, se revela un efecto real del tratamiento. ¡El ANCOVA funcionó!"
            )
        } else {
            div(class="alert alert-danger mt-2",
                icon("times-circle"),
                strong("Conclusión: No Significativo."), " Incluso después de ajustar por el Fósforo, no hay evidencia de un efecto del tratamiento. Es probable que los bioestimulantes no sean efectivos."
            )
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