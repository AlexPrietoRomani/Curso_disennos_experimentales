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
                
                h4(class = "section-header", "1.1 La Trampa del Enfoque 'Un Factor a la Vez' (OFAT)"),
                p(
                    "Históricamente, una aproximación intuitiva a la experimentación ha sido el método de 'Un Factor a la Vez' (OFAT, por sus siglas en inglés). Si un agrónomo desea optimizar el rendimiento estudiando la fertilización con nitrógeno (N) y la densidad de siembra (D), el enfoque OFAT implicaría:"
                ),
                tags$ol(
                    tags$li("Fijar una densidad de siembra estándar y variar los niveles de N para encontrar el óptimo."),
                    tags$li("Usando ese nivel óptimo de N, variar la densidad de siembra para encontrar la densidad óptima.")
                ),
                p(
                    "Este método, aunque simple, tiene dos fallas fundamentales que lo hacen inadecuado para sistemas complejos como los agronómicos (Czarnota & Thomas, 2018)."
                ),
                tags$div(class = "row",
                    tags$div(class="col-md-6",
                        tags$div(class="alert alert-danger", role="alert", style="height: 100%;",
                            tags$h5(class="alert-heading", "Falla 1: Ineficiencia"),
                            "El enfoque OFAT es estadísticamente ineficiente. Para obtener la misma precisión en la estimación de los efectos, un diseño factorial requiere significativamente menos unidades experimentales (parcelas, plantas, etc.). Como señaló R.A. Fisher, el padre de la estadística moderna, en un diseño factorial cada observación proporciona información sobre todos los factores estudiados, maximizando el retorno de la inversión experimental (Fisher, 1926)."
                        )
                    ),
                    tags$div(class="col-md-6",
                        tags$div(class="alert alert-danger", role="alert", style="height: 100%;",
                            tags$h5(class="alert-heading", "Falla 2: Incapacidad de Detectar Interacciones"),
                            "Esta es la falla más crítica. El OFAT asume implícitamente que no existen interacciones; es decir, que el efecto de un factor es independiente del nivel del otro. Esta suposición es raramente cierta en biología. Por ejemplo, la respuesta del cultivo al nitrógeno casi siempre depende de la disponibilidad de agua (riego), y la densidad de siembra óptima puede cambiar según la variedad utilizada. Ignorar estas interdependencias puede llevar a conclusiones erróneas y recomendaciones subóptimas (Montgomery, 2017)."
                        )
                    )
                ),

                # --- Parte 2: La Solución Factorial (con el ejemplo integrado) ---
                h4(class = "section-header", "1.2 La Solución Moderna: Diseños Factoriales"),
                p(
                    "Para superar estas limitaciones, la estadística moderna propone los ", tags$b("diseños factoriales,"), " un enfoque donde los factores se varían conjuntamente. En un ", tags$em("diseño factorial completo,"), " se investigan todas las posibles combinaciones de los niveles de los factores de interés (Kuehl, 2000)."
                ),
                p(
                    "Por ejemplo, imaginemos que un agrónomo quiere estudiar el efecto de un ", strong("bioestimulante (Factor A)"), " y la ", strong("época de aplicación (Factor B)"), " sobre el rendimiento. Si el bioestimulante tiene 2 niveles (Control, Aplicación) y la época tiene 3 niveles (Siembra, Crecimiento Vegetativo, Floración), estamos ante un ", strong("diseño factorial 2x3."), " Este diseño requerirá probar 2 x 3 = 6 tratamientos combinados, como se muestra a continuación:"
                ),
                # La tabla ahora está en el flujo principal, no en una note-cloud
                tags$div(class="row justify-content-center",
                    tags$div(class="col-md-8",
                        tags$table(class="table table-sm table-bordered table-striped text-center",
                            tags$thead(tags$tr(tags$th("Tratamiento Combinado"), tags$th("Factor A: Bioestimulante"), tags$th("Factor B: Época"))),
                            tags$tbody(
                                tags$tr(tags$td(strong("1")), tags$td("Control"), tags$td("Siembra")),
                                tags$tr(tags$td(strong("2")), tags$td("Control"), tags$td("Crec. Vegetativo")),
                                tags$tr(tags$td(strong("3")), tags$td("Control"), tags$td("Floración")),
                                tags$tr(tags$td(strong("4")), tags$td("Aplicación"), tags$td("Siembra")),
                                tags$tr(tags$td(strong("5")), tags$td("Aplicación"), tags$td("Crec. Vegetativo")),
                                tags$tr(tags$td(strong("6")), tags$td("Aplicación"), tags$td("Floración"))
                            )
                        ),
                        tags$p(class="small-diagram text-center", "Figura 6.1: Las 6 combinaciones de tratamiento de un factorial 2x3.")
                    )
                ),
                
                # --- Parte 3: Constructor Interactivo de Diseños Factoriales ---
                h4(class = "section-header", "1.3 Constructor de Diseños Factoriales"),
                p("Usa las siguientes herramientas para diseñar tu propio experimento factorial y ver todas las combinaciones de tratamiento que necesitarías probar. Esto te ayudará a entender la escala de estos diseños."),
                sidebarLayout(
                    sidebarPanel(
                        width = 4,
                        tags$h5("Define tus Factores"),
                        numericInput(ns("num_niveles_A"), "Niveles del Factor A (ej. Fertilizante):", value = 2, min = 2, max = 10),
                        numericInput(ns("num_niveles_B"), "Niveles del Factor B (ej. Variedad):", value = 3, min = 2, max = 10),
                        actionButton(ns("generar_diseno_factorial"), "Generar Combinaciones", icon = icon("cogs"), class = "btn-info w-100 mt-3")
                    ),
                    mainPanel(
                        width = 8,
                        # uiOutput para la salida dinámica
                        uiOutput(ns("salida_diseno_factorial"))
                    )
                ),
                
                hr(),

                # --- Parte 4: Ventajas Estratégicas ---
                h4(class = "section-header", "1.4 Ventajas Estratégicas de los Diseños Factoriales"),
                p(
                    "La adopción de diseños factoriales representa un cambio de paradigma hacia una experimentación más holística y eficiente. Sus beneficios van más allá de la simple corrección de las fallas del OFAT."
                ),
                tags$ul(
                    tags$li(
                        tags$strong("Maximización de la Información:"), " Los factoriales permiten el estudio de los ", tags$em("efectos principales"), " (el efecto promedio de cada factor a través de los niveles de los otros factores) y, crucialmente, de los ", tags$em("efectos de interacción."), " Descubrir una interacción es a menudo el hallazgo más valioso de un experimento, ya que revela la complejidad real del sistema biológico y puede llevar a recomendaciones agronómicas específicas y personalizadas (Box et al., 2005). Por ejemplo, recomendar un fertilizante solo si se usa una variedad específica."
                    ),
                    tags$li(
                        tags$strong("Eficiencia de Recursos:"), " Proporcionan estimaciones más precisas de los efectos con el mismo número de observaciones, o la misma precisión con menos observaciones. Este concepto, a veces llamado 'ocultamiento de factores' (factor hiding), se debe a que para estimar el efecto principal de, por ejemplo, el Factor A, se utilizan todas las observaciones del experimento, promediando sobre los niveles del Factor B. Esto reduce el impacto del 'ruido' aleatorio y aumenta la potencia estadística del diseño (Mead, 1988)."
                    ),
                    tags$li(
                        tags$strong("Ampliación de la Base de Inferencia (Validez Externa):"), " Las conclusiones sobre el efecto de un factor son más robustas y generales porque han sido evaluadas a través de un rango de condiciones (los diferentes niveles de los otros factores). Si el efecto del Nitrógeno es consistente a través de varias densidades de siembra, podemos estar más seguros de que nuestra recomendación será válida en diferentes escenarios. Esto aumenta la ", tags$em("validez externa"), " de los resultados, haciéndolos más aplicables a las condiciones variables del mundo real que enfrenta un agricultor (Montgomery, 2017)."
                    ),
                    tags$li(
                        tags$strong("Base para la Optimización de Procesos:"), " Los diseños factoriales son el fundamento de metodologías más avanzadas como la Metodología de Superficie de Respuesta (RSM), que se utiliza para encontrar las condiciones óptimas (ej. la combinación exacta de fertilizante y agua que maximiza el rendimiento) y para modelar la respuesta del sistema en su totalidad."
                    )
                )
            ),

            # ===== PESTAÑA 2: Interacciones =====
            nav_panel(
                title = "2. El Corazón del Factorial: La Interacción",
                
                h4(class = "section-header", "2.1 Una Analogía Simple: Café y Azúcar"),
                p(
                    "Imagina que mides tu nivel de disfrute de una bebida. Los factores son 'Café' (Sí/No) y 'Azúcar' (Sí/No).",
                ),
                tags$ul(
                    tags$li(strong("Efecto del Café:"), "Añadir café a un vaso de agua aumenta tu disfrute."),
                    tags$li(strong("Efecto del Azúcar:"), "Añadir azúcar a un vaso de agua también aumenta tu disfrute."),
                    tags$li(strong("Efecto Aditivo (Sin Interacción):"), "Si los efectos son aditivos, el disfrute de 'Café con Azúcar' sería simplemente la suma del disfrute del 'Café solo' más el del 'Azúcar sola'."),
                    tags$li(strong("Interacción Sinérgica:"), "¡Pero esto no es real! El café y el azúcar juntos son mucho más deliciosos que la suma de sus partes. El azúcar ", strong("potencia"), " el efecto del café. Esto es una interacción."),
                    tags$li(strong("Interacción Antagónica:"), "Imagina ahora que el 'Azúcar' es en realidad 'Sal'. Añadir sal al café probablemente disminuiría tu disfrute mucho más de lo esperado. La sal interfiere negativamente con el café.")
                ),
                
                h4(class = "section-header", "2.2 Definición Formal y Visualización"),
                p(
                    "En términos estadísticos, una ", tags$b("interacción entre el Factor A y el Factor B"), " significa que el efecto de un nivel del Factor A no es el mismo en todos los niveles del Factor B. La clave visual para detectar una interacción es la ", tags$b("falta de paralelismo"), " en los gráficos de interacción (Mead et al., 2012)."
                ),
                
                hr(),

                # --- Escenario 1: Sin Interacción (Aditividad) ---
                tags$h5(tags$strong("Escenario 1: Sin Interacción (Efectos Aditivos)")),
                tags$p("Las líneas son paralelas (o muy cercanas a serlo). La diferencia entre B1 y B2 es la misma tanto en el nivel A1 como en el A2."),
                fluidRow(
                    column(7, plotOutput(ns("plot_no_int"), height="300px")),
                    column(5, 
                        tags$h6("Tabla de Medias:", class="mt-3"),
                        tableOutput(ns("tabla_no_int")),
                        tags$h6("Interpretación Agronómica:"),
                        p("El fertilizante A2 siempre aumenta el rendimiento en 4 unidades respecto a A1, sin importar la variedad. La variedad B2 siempre aumenta el rendimiento en 5 unidades respecto a B1, sin importar el fertilizante. Las recomendaciones son independientes: ", strong("Use siempre A2 y B2."))
                    )
                ),

                hr(),

                # --- Escenario 2: Interacción Sinérgica ---
                tags$h5(tags$strong("Escenario 2: Interacción Sinérgica (u Ordinal)")),
                p("Las líneas no son paralelas pero no se cruzan. El efecto de un factor se potencia en un nivel del otro. A2 sigue siendo mejor que A1 en todos los casos, pero la ", em("magnitud"), " de su superioridad cambia."),
                fluidRow(
                    column(7, plotOutput(ns("plot_sinergia"), height="300px")),
                    column(5,
                        tags$h6("Tabla de Medias:", class="mt-3"),
                        tableOutput(ns("tabla_sinergia")),
                        tags$h6("Interpretación Agronómica:"),
                        p("El fertilizante A2 solo da un pequeño beneficio con la variedad B1 (+2 unidades), pero tiene un efecto ", strong("explosivo"), " con la variedad B2 (+13 unidades). La recomendación principal sigue siendo 'Use A2 y B2', pero ahora entendemos que el verdadero potencial se desbloquea al combinarlos.")
                    )
                ),
                
                hr(),

                # --- Escenario 3: Interacción Antagónica (Crossover) ---
                tags$h5(tags$strong("Escenario 3: Interacción Antagónica (de Cruce o Crossover)")),
                p("Este es el tipo de interacción más fuerte y dramático. Las líneas se cruzan, lo que significa que la recomendación de un factor se invierte dependiendo del nivel del otro."),
                fluidRow(
                    column(7, plotOutput(ns("plot_antagon"), height="300px")),
                    column(5,
                        tags$h6("Tabla de Medias:", class="mt-3"),
                        tableOutput(ns("tabla_antagon")),
                        tags$h6("Interpretación Agronómica:"),
                        p(strong("¡No hay una única recomendación!"), " Si un agricultor usa la variedad B1, DEBE usar el fertilizante A2. Pero si usa la variedad B2, DEBE usar el fertilizante A1 para obtener el mejor rendimiento. Una recomendación general como 'Use el fertilizante A2' sería desastrosa para los agricultores que usan la variedad B2.")
                    )
                ),
                
                hr(),
                
                tags$div(class="alert alert-success text-center", role="alert",
                    tags$h4("La Gran Lección"),
                    p("Cuando la interacción es significativa, la pregunta ya no es '¿Qué factor es mejor?', sino '", strong("¿Qué combinación de niveles de factores es la óptima?'"), " La interacción es el descubrimiento, y los efectos principales pierden su significado independiente.")
                )
            ),

            # ===== PESTAÑA 3: Análisis en R =====
            nav_panel(
                title = "3. Análisis en R",
                
                h4(class = "section-header", "3.1 El Diseño Base: Factoriales sobre DCA o DBCA"),
                p(
                    "Un diseño factorial no existe en el vacío; debe ser implementado sobre una estructura de aleatorización. Las dos más comunes son:"
                ),
                fluidRow(
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                tags$h5(class="card-title", "Factorial sobre un DCA"),
                                p(class="card-text", "Se usa cuando las unidades experimentales son homogéneas. Todas las combinaciones de tratamiento se asignan de forma completamente aleatoria."),
                                p(strong("Modelo Matemático:")),
                                withMathJax(helpText(
                                    "$$Y_{ijk} = \\mu + \\alpha_i + \\beta_j + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
                                )),
                                p(strong("Fórmula en R:")),
                                code("aov(respuesta ~ factorA * factorB, data = datos)")
                            )
                        )
                    ),
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                tags$h5(class="card-title", "Factorial sobre un DBCA"),
                                p(class="card-text", "Es el enfoque más común en campo. Las combinaciones de tratamiento se aleatorizan dentro de cada bloque para controlar una fuente de heterogeneidad."),
                                p(strong("Modelo Matemático:")),
                                withMathJax(helpText(
                                    "$$Y_{ijk} = \\mu + \\rho_k + \\alpha_i + \\beta_j + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
                                )),
                                p(class="card-text", em("Donde \\(\\rho_k\\) es el efecto del k-ésimo bloque.")),
                                p(strong("Fórmula en R:")),
                                code("aov(respuesta ~ bloque + factorA * factorB, data = datos)")
                            )
                        )
                    )
                ),

                hr(),
                
                h4(class = "section-header", "3.2 Análisis Interactivo: Paso a Paso"),
                p("Vamos a analizar un conjunto de datos real de un experimento factorial. Usa los botones para avanzar por cada paso del análisis, desde la exploración hasta la interpretación final."),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Control del Análisis"),
                        p("Datos de ejemplo: Efecto de la variedad de avena (", strong("V"), ") y la fertilización nitrogenada (", strong("N"), ") sobre el rendimiento, en un diseño en bloques."),
                        radioButtons(ns("paso_analisis"), "Selecciona el Paso del Análisis:",
                            # La clave es que cada "choiceName" sea un único elemento de la lista.
                            # Usamos tagList para agrupar el icono y el texto para cada opción.
                            choiceNames = list(
                                tags$span(icon("table"), "1. Ver los Datos"),
                                tags$span(icon("flask"), "2. Ajustar Modelo y Verif. Supuestos"),
                                tags$span(icon("clipboard-list"), "3. Interpretar ANOVA y Efectos"),
                                tags$span(icon("sitemap"), "4. Realizar Análisis Post-Hoc")
                            ),
                            choiceValues = c("datos", "supuestos", "anova", "posthoc")
                        ),
                        hr(),
                        tags$div(class="note-cloud",
                            tags$strong("Contexto del Dataset:"),
                            "Este es un subconjunto del dataset clásico 'oats' del paquete MASS, que ilustra un diseño factorial en parcelas divididas, pero para este ejemplo lo trataremos como un factorial en DBCA simple."
                        )
                    ),
                    mainPanel(
                        width = 9,
                        # La salida cambiará dinámicamente según el paso seleccionado
                        uiOutput(ns("salida_analisis_pasos"))
                    )
                )
            ),

            # ===== PESTAÑA 4: Desglosando Interacciones =====
            nav_panel(
                title = "4. Desglosando Interacciones",
                
                h4(class = "section-header", "4.1 Post-Hoc en Factoriales: Una Estrategia de Dos Caminos"),
                p(
                    "El análisis post-hoc en un diseño factorial no es un procedimiento único; es una estrategia que depende completamente del resultado de la prueba de interacción en la tabla ANOVA. La significancia de la interacción actúa como una bifurcación en el camino del análisis."
                ),
                p(
                    "La herramienta moderna y más robusta para este trabajo es el paquete ", code("emmeans"), ". Su nombre viene de ", strong("Estimated Marginal Means"), " (Medias Marginales Estimadas). A diferencia de las medias simples, las 'emmeans' son las medias predichas por el modelo para cada nivel, ajustadas por cualquier desbalance en el diseño y promediadas sobre los otros factores, lo que las hace más fiables para las comparaciones (Lenth, 2023)."
                ),
                
                hr(),
                
                # --- Usamos navset_card_pill para separar los dos escenarios ---
                navset_card_pill(
                    header = tags$h5("Elige el camino según tu resultado de interacción:"),
                    
                    # --- CAMINO A: INTERACCIÓN SIGNIFICATIVA ---
                    nav_panel(
                        "Camino A: Interacción SIGNIFICATIVA",
                        tags$div(class="alert alert-warning",
                            p(strong("La Regla:"), " Si la interacción A:B es significativa, los efectos principales de A y B pierden su significado. Debes analizar los ", strong("efectos simples."), " Esto significa comparar los niveles de un factor DENTRO de cada nivel del otro factor.")
                        ),
                        h6("Pregunta de Investigación: ¿El efecto del Factor A (Variedad) es el mismo en todos los niveles del Factor B (Nitrógeno)?"),
                        p(
                            "La sintaxis en `emmeans` es muy intuitiva. La barra vertical `|` se lee como 'dado' o 'dentro de'. La fórmula ",
                            code("pairwise ~ FactorA | FactorB"), " se traduce como: 'Realiza comparaciones por pares (pairwise) de los niveles del Factor A para cada nivel del Factor B'."
                        ),
                        pre(class="r-code",
                            htmltools::HTML(
                                "# Asumiendo que 'modelo_analisis' es nuestro modelo aov()\n",
                                "library(emmeans)\n",
                                "\n",
                                "# Analizar el efecto simple de la Variedad para cada nivel de Nitrógeno\n",
                                "emm_efectos_simples <- emmeans(modelo_analisis, pairwise ~ variedad | nitrogeno)\n",
                                "print(emm_efectos_simples)"
                            )
                        ),
                        h6("Interpretación de la Salida y su Gráfico"),
                        p(
                            "La salida de `emmeans` nos dará una tabla de comparaciones por pares (ej. 'Golden Rain - Marvellous') para cada nivel de nitrógeno por separado. Un p-valor significativo en una de estas sub-tablas, pero no en otra, es la prueba numérica de la interacción. El gráfico asociado muestra visualmente estas comparaciones."
                        ),
                        pre(class="r-code",
                            htmltools::HTML(
                                "# Visualizar las comparaciones de los efectos simples\n",
                                "plot(emm_efectos_simples, comparisons = TRUE)"
                            )
                        ),
                        p(strong("Conclusión Agronómica Ejemplo:"), " 'La variedad Golden Rain solo supera significativamente a Marvellous cuando se aplican 0.2 o 0.4 cwt/acre de nitrógeno; sin nitrógeno, no hay diferencia entre ellas'. Esta es una recomendación mucho más precisa que decir simplemente que una variedad es 'mejor'."
                        )
                    ),
                    
                    # --- CAMINO B: INTERACCIÓN NO SIGNIFICATIVA ---
                    nav_panel(
                        "Camino B: Interacción NO Significativa",
                        tags$div(class="alert alert-success",
                            p(strong("La Regla:"), " Si la interacción A:B NO es significativa, puedes concluir que los efectos de los factores son ", strong("aditivos e independientes."), " Esto te autoriza a examinar los ", strong("efectos principales"), " de cada factor por separado.")
                        ),
                        h6("Pregunta de Investigación: ¿Cuál es el efecto promedio del Factor A (Variedad) a través de todos los niveles del Factor B (Nitrógeno)?"),
                        p(
                            "Para analizar el efecto principal, simplemente omitimos la barra vertical. La fórmula ",
                            code("pairwise ~ FactorA"), " le pide a `emmeans` que calcule la media de cada nivel de A (promediando sobre todos los niveles de B) y luego las compare."
                        ),
                        pre(class="r-code",
                            htmltools::HTML(
                                "# Analizar el efecto principal de la Variedad\n",
                                "emm_main_variedad <- emmeans(modelo_analisis, pairwise ~ variedad)\n",
                                "print(emm_main_variedad)\n",
                                "\n",
                                "# Analizar el efecto principal del Nitrógeno\n",
                                "emm_main_nitrogeno <- emmeans(modelo_analisis, pairwise ~ nitrogeno)\n",
                                "print(emm_main_nitrogeno)"
                            )
                        ),
                        h6("Interpretación de la Salida y su Gráfico"),
                        p(
                            "Ahora obtendremos una única tabla de comparaciones para cada factor. El gráfico mostrará las medias marginales estimadas para cada nivel del factor, con sus intervalos de confianza."
                        ),
                        pre(class="r-code",
                            htmltools::HTML(
                                "# Visualizar las comparaciones del efecto principal\n",
                                "plot(emm_main_variedad, comparisons = TRUE)"
                            )
                        ),
                        p(strong("Conclusión Agronómica Ejemplo:"), " 'En promedio, a través de todos los niveles de nitrógeno probados, la variedad Golden Rain rinde significativamente más que Marvellous'. Y, por separado, 'Cada incremento en la dosis de nitrógeno produce un aumento significativo en el rendimiento, independientemente de la variedad utilizada'."
                        )
                    )
                ),
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
            ),

            # ===== PESTAÑA 6: Referencias =====
            nav_panel(
                title = "Referencias",
                tags$ul(
                    tags$li("Box, G. E. P., Hunter, J. S., & Hunter, W. G. (2005). ", tags$em("Statistics for experimenters: Design, innovation, and discovery"), " (2nd ed.). Wiley-Interscience."),
                    tags$li("Czarnota, M. A., & Thomas, P. A. (2018). The Case for Factorial Experimental Designs in Horticultural Research. ", tags$em("HortScience, 53"),"(1), 4-9. ", tags$a(href="https://doi.org/10.21273/HORTSCI12403-17", "https://doi.org/10.21273/HORTSCI12403-17")),
                    tags$li("Fisher, R. A. (1926). The Arrangement of Field Experiments. ", tags$em("Journal of the Ministry of Agriculture of Great Britain, 33"),", 503-513."),
                    tags$li("Kuehl, R. O. (2000). ", tags$em("Design of experiments: Statistical principles of research design and analysis"), " (2nd ed.). Duxbury Press."),
                    tags$li("Mead, R. (1988). ", tags$em("The design of experiments: Statistical principles for practical application."), " Cambridge university press."),
                    tags$li("Montgomery, D. C. (2017). ", tags$em("Design and analysis of experiments"), " (9th ed.). John Wiley & Sons."),
                    tags$li("Mead, R., Curnow, R. N., & Hasted, A. M. (2012). ", tags$em("Statistical methods in agriculture and experimental biology"), " (3rd ed.). CRC press."),
                    tags$li("Lenth, R. V. (2023). ", tags$em("emmeans: Estimated Marginal Means, aka Least-Squares Means."), " R package version 1.8.8. ", tags$a(href="https://CRAN.R-project.org/package=emmeans", "https://CRAN.R-project.org/package=emmeans")),
                    tags$li("Searle, S. R., Speed, F. M., & Milliken, G. A. (1980). Population marginal mean, in the linear model: An alternative to least squares means. ", tags$em("The American Statistician, 34"),"(4), 216-221.")
                )
            )
        )
    )
}

# Server para la Sesión 6
session6Server <- function(input, output, session) {
    # Definir el namespace para usarlo en el server
    ns <- session$ns 

    # --- LÓGICA PARA LA PESTAÑA 1: CONSTRUCTOR DE DISEÑOS FACTORIALES ---
        
    # eventReactive para el constructor de diseños
    diseno_factorial_generado <- eventReactive(input$generar_diseno_factorial, {
        
        req(input$num_niveles_A, input$num_niveles_B)
        
        # Crear los niveles para cada factor
        niveles_A <- paste0("A", 1:input$num_niveles_A)
        niveles_B <- paste0("B", 1:input$num_niveles_B)
        
        # Usar expand.grid para obtener todas las combinaciones
        combinaciones <- expand.grid(
            FactorA = factor(niveles_A),
            FactorB = factor(niveles_B)
        )
        
        # Añadir un ID para cada tratamiento combinado y reordenar
        combinaciones <- combinaciones %>%
            mutate(TratamientoID = row_number()) %>%
            # ===== CORRECCIÓN APLICADA AQUÍ =====
            dplyr::select(TratamientoID, everything())

        list(
            combinaciones = combinaciones,
            num_A = input$num_niveles_A,
            num_B = input$num_niveles_B
        )
        
    }, ignoreNULL = FALSE)

    # DEFINIR EL OUTPUT PARA LA TABLA DT POR SEPARADO
    output$tabla_combinaciones_dt <- DT::renderDataTable({
        diseno <- diseno_factorial_generado()
        req(diseno)
        
        DT::datatable(
            diseno$combinaciones,
            options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE, info = FALSE),
            rownames = FALSE,
            class = 'table table-hover table-striped'
        )
    })

    # renderUI para mostrar los resultados del constructor
    output$salida_diseno_factorial <- renderUI({
        
        diseno <- diseno_factorial_generado()
        req(diseno)
        
        num_combinaciones <- nrow(diseno$combinaciones)
        
        tagList(
            # Resumen del diseño
            tags$div(class = "alert alert-success",
                tags$h5("Diseño Generado: Factorial ", strong(paste0(diseno$num_A, "x", diseno$num_B))),
                p("Este diseño tiene ", strong(diseno$num_A), " niveles para el Factor A y ", strong(diseno$num_B), 
                  " niveles para el Factor B, resultando en un total de ", strong(num_combinaciones), 
                  " combinaciones de tratamiento únicas que deben ser probadas.")
            ),
            
            # Tabla con las combinaciones
            tags$h6("Tabla de Combinaciones de Tratamiento:"),
            tags$div(style="max-height: 300px; overflow-y: auto;",
                # AHORA LLAMAMOS AL *Output CORRESPONDIENTE
                DT::dataTableOutput("tabla_combinaciones_dt")
            )
        )
    })

    # --- LÓGICA PARA LA PESTAÑA 2: INTERACCIONES ---

    # Crear los data frames de ejemplo una sola vez
    df_no_int <- data.frame(FactorA = rep(c("Fertilizante A1","Fertilizante A2"), each=2), 
                            FactorB = rep(c("Variedad B1","Variedad B2"), 2), 
                            Rendimiento = c(10, 15, 14, 19))
    df_sinergia <- data.frame(FactorA = rep(c("Fertilizante A1","Fertilizante A2"), each=2), 
                              FactorB = rep(c("Variedad B1","Variedad B2"), 2), 
                              Rendimiento = c(10, 12, 14, 25))
    df_antagon <- data.frame(FactorA = rep(c("Fertilizante A1","Fertilizante A2"), each=2), 
                             FactorB = rep(c("Variedad B1","Variedad B2"), 2), 
                             Rendimiento = c(10, 20, 18, 12))
    
    # Función para crear el gráfico base
    crear_grafico_int <- function(data, titulo) {
        ggplot(data, aes(x = FactorB, y = Rendimiento, group = FactorA, color = FactorA)) +
            geom_line(linewidth = 1.5) +
            geom_point(size = 4, shape = 21, fill = "white", stroke = 1.5) +
            labs(title = titulo, y = "Rendimiento Medio", x = "Factor B (Variedad)", color = "Factor A (Fertilizante)") +
            theme_minimal(base_size = 14) +
            theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5, face="bold")) +
            ylim(min(c(df_no_int$Rendimiento, df_sinergia$Rendimiento, df_antagon$Rendimiento)) - 1,
                 max(c(df_no_int$Rendimiento, df_sinergia$Rendimiento, df_antagon$Rendimiento)) + 1) # Eje Y consistente
    }

    # Renderizar cada gráfico por separado
    output$plot_no_int <- renderPlot({
        crear_grafico_int(df_no_int, "Escenario 1: Sin Interacción (Aditivo)")
    })
    output$plot_sinergia <- renderPlot({
        crear_grafico_int(df_sinergia, "Escenario 2: Interacción Sinérgica")
    })
    output$plot_antagon <- renderPlot({
        crear_grafico_int(df_antagon, "Escenario 3: Interacción Antagónica (Crossover)")
    })

    # Función para crear la tabla de medias
    crear_tabla_medias <- function(data) {
        tidyr::pivot_wider(data, names_from = FactorB, values_from = Rendimiento)
    }

    # Renderizar cada tabla por separado
    output$tabla_no_int <- renderTable({
        crear_tabla_medias(df_no_int)
    }, bordered = TRUE, hover = TRUE, striped = TRUE, align = 'c')
    output$tabla_sinergia <- renderTable({
        crear_tabla_medias(df_sinergia)
    }, bordered = TRUE, hover = TRUE, striped = TRUE, align = 'c')
    output$tabla_antagon <- renderTable({
        crear_tabla_medias(df_antagon)
    }, bordered = TRUE, hover = TRUE, striped = TRUE, align = 'c')
    
    # --- LÓGICA PARA LA PESTAÑA 3: ANÁLISIS INTERACTIVO ---

    # Cargar los datos y ajustar el modelo una sola vez (esto ya está bien)
    datos_analisis <- reactive({
        # Asegurarse de que los paquetes necesarios están disponibles
        req(MASS::oats, dplyr::mutate, dplyr::rename, dplyr::select)
        MASS::oats %>%
            mutate(
                bloque = B,
                variedad = factor(V),
                nitrogeno = factor(N, labels = paste0(levels(N), "cwt/acre"))
            ) %>%
            rename(rendimiento = Y) %>%
            dplyr::select(bloque, variedad, nitrogeno, rendimiento)
    })
    
    modelo_analisis <- reactive({
        req(datos_analisis())
        aov(rendimiento ~ bloque + variedad * nitrogeno, data = datos_analisis())
    })

    # ===== PASO 1: DEFINIR TODOS LOS OUTPUTS POR SEPARADO =====

    # --- Salidas para el bloque "VER DATOS" ---
    output$tabla_datos_exploratorios <- DT::renderDataTable({
        req(datos_analisis())
        DT::datatable(datos_analisis(), options = list(pageLength=5, searching=FALSE, lengthChange=FALSE, info=FALSE), rownames=FALSE)
    })
    
    output$grafico_interaccion_observada <- renderPlot({
        req(datos_analisis())
        ggplot(datos_analisis(), aes(x = nitrogeno, y = rendimiento, group = variedad, color = variedad)) +
            stat_summary(fun = mean, geom = "line", size = 1) +
            stat_summary(fun = mean, geom = "point", size = 3) +
            labs(title="Rendimiento promedio por combinación de Nitrógeno y Variedad",
                 y = "Rendimiento (bushels/acre)", x = "Nivel de Nitrógeno") +
            theme_minimal(base_size = 12)
    })

    # --- Salidas para el bloque "VERIFICAR SUPUESTOS" ---
    output$grafico_diagnostico_supuestos <- renderPlot({
        req(modelo_analisis())
        modelo <- modelo_analisis()
        # Usamos patchwork para combinar los gráficos
        p1 <- ggplot(modelo, aes(.fitted, .resid)) + geom_point(alpha=0.6) + 
              geom_hline(yintercept=0, linetype="dashed", color="red") +
              labs(x="Valores Ajustados", y="Residuales", title="A) Residuales vs. Ajustados") + theme_bw()
              
        p2 <- ggplot(modelo, aes(sample = .resid)) + stat_qq() + stat_qq_line(color="red") +
              labs(title="B) Normal Q-Q Plot de Residuales") + theme_bw()
              
        p1 + p2
    })
    
    output$prueba_levene <- renderPrint({
        req(datos_analisis())
        car::leveneTest(rendimiento ~ variedad * nitrogeno, data = datos_analisis())
    })

    # --- Salidas para el bloque "INTERPRETAR ANOVA" ---
    output$tabla_anova_analisis <- renderPrint({
        req(modelo_analisis())
        summary(modelo_analisis())
    })
    
    output$tabla_eta_squared <- renderPrint({
        req(modelo_analisis())
        effectsize::eta_squared(modelo_analisis(), partial = TRUE)
    })
    
    # --- Salidas para el bloque "POST-HOC" ---
    # Este es especial porque la salida en sí es condicional.
    # El renderPrint se define una sola vez y la lógica if/else está dentro.
    output$posthoc_output_condicional <- renderPrint({
        req(modelo_analisis())
        modelo <- modelo_analisis()
        p_val_int <- broom::tidy(modelo) %>% 
                        filter(term == "variedad:nitrogeno") %>% 
                        pull(p.value)

        if (p_val_int < 0.05) {
            cat("--- DESGLOSE DE INTERACCIÓN SIGNIFICATIVA ---\n\n")
            cat("Comparación de Variedades DENTRO de cada Nivel de Nitrógeno:\n")
            print(emmeans::emmeans(modelo, pairwise ~ variedad | nitrogeno))
            cat("\nComparación de Niveles de Nitrógeno DENTRO de cada Variedad:\n")
            print(emmeans::emmeans(modelo, pairwise ~ nitrogeno | variedad))
        } else {
            cat("--- ANÁLISIS DE EFECTOS PRINCIPALES (INTERACCIÓN NO SIGNIFICATIVA) ---\n\n")
            cat("Efecto Principal de la Variedad (promediando sobre nitrógeno):\n")
            print(emmeans::emmeans(modelo, pairwise ~ variedad))
            cat("\nEfecto Principal del Nitrógeno (promediando sobre variedad):\n")
            print(emmeans::emmeans(modelo, pairwise ~ nitrogeno))
        }
    })


    # ===== PASO 2: UI DINÁMICO QUE AHORA USA LAS FUNCIONES *Output =====
    output$salida_analisis_pasos <- renderUI({
        
        paso <- input$paso_analisis
        req(paso)
        
        # --- PASO 1: VER LOS DATOS ---
        if (paso == "datos") {
            tagList(
                h5("1. Exploración de Datos"),
                p("Primero, examinamos la estructura de los datos y visualizamos las tendencias principales."),
                h6("Primeras filas del dataset:"),
                # CORRECCIÓN: Volvemos a añadir ns()
                DT::dataTableOutput(ns("tabla_datos_exploratorios")),
                h6("Gráfico de Interacción (Medias Observadas):"),
                # CORRECCIÓN: Volvemos a añadir ns()
                plotOutput(ns("grafico_interaccion_observada"))
            )
        
        # --- PASO 2: VERIFICAR SUPUESTOS ---
        } else if (paso == "supuestos") {
            tagList(
                h5("2. Verificación de Supuestos del Modelo"),
                p("Antes de confiar en la tabla ANOVA, debemos verificar los supuestos sobre los residuales del modelo."),
                # CORRECCIÓN: Volvemos a añadir ns()
                plotOutput(ns("grafico_diagnostico_supuestos")),
                h6("Prueba formal de Homocedasticidad (Levene):"),
                # CORRECCIÓN: Volvemos a añadir ns()
                verbatimTextOutput(ns("prueba_levene"))
            )

        # --- PASO 3: INTERPRETAR ANOVA Y EFECTOS ---
        } else if (paso == "anova") {
            tagList(
                h5("3. Interpretación de la Tabla ANOVA y Tamaño del Efecto"),
                p("Con los supuestos razonablemente cumplidos, interpretamos la tabla ANOVA. Recuerda: ¡la interacción primero!"),
                h6("Tabla ANOVA del Modelo Factorial en Bloques:"),
                # CORRECCIÓN: Volvemos a añadir ns()
                verbatimTextOutput(ns("tabla_anova_analisis")),
                h6("Tamaño del Efecto (Eta-cuadrado Parcial, η²p):"),
                p("Esta tabla nos dice qué proporción de la varianza explica cada término, controlando los demás."),
                # CORRECCIÓN: Volvemos a añadir ns()
                verbatimTextOutput(ns("tabla_eta_squared"))
            )

        # --- PASO 4: ANÁLISIS POST-HOC ---
        } else if (paso == "posthoc") {
            p_val_int <- broom::tidy(modelo_analisis()) %>% 
                            filter(term == "variedad:nitrogeno") %>% 
                            pull(p.value)
            
            tagList(
                h5("4. Análisis Post-Hoc"),
                if (p_val_int < 0.05) {
                    div(class="alert alert-warning", 
                        p(strong("¡Interacción Significativa!"), " (p =", round(p_val_int, 4), "). Debemos analizar los efectos simples.")
                    )
                } else {
                    div(class="alert alert-success",
                        p(strong("Interacción No Significativa."), " (p =", round(p_val_int, 4), "). Podemos interpretar los efectos principales.")
                    )
                },
                # CORRECCIÓN: Volvemos a añadir ns()
                verbatimTextOutput(ns("posthoc_output_condicional"))
            )
        }
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