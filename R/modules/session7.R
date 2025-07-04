# R/modules/session7.R

# UI para la Sesión 7
session7UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3(class = "session-title", "Sesión 7: Diseño en Parcelas Divididas (Split-Plot)"),
        
        navset_tab(
            # ===== PESTAÑA 1: Introducción al Split-Plot =====
            nav_panel(
                title = "1. ¿Por qué Parcelas Divididas?",
                
                tags$h4(class = "section-header", "1.1 El Dilema del Tamaño de la Parcela"),
                tags$p(
                    "Los diseños factoriales son poderosos, pero asumen que podemos aleatorizar todas las combinaciones de tratamiento con la misma facilidad. En la práctica agronómica, esto no siempre es cierto. Nos enfrentamos a un dilema cuando los factores a estudiar requieren escalas físicas muy diferentes."
                ),
                tags$div(class = "row",
                    tags$div(class="col-md-6",
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                tags$h5(class="card-title", "Factores 'Difíciles de Cambiar'"),
                                tags$p(class="card-text", "Estos factores son inherentemente difíciles, costosos o poco prácticos de aplicar en áreas pequeñas. Requieren ", strong("parcelas grandes."), " Su aleatorización completa en un diseño factorial estándar sería una pesadilla logística."),
                                tags$ul(
                                    tags$li("Sistemas de labranza (arado, siembra directa)."),
                                    tags$li("Métodos de riego (goteo, aspersión, surcos)."),
                                    tags$li("Fechas de siembra a gran escala.")
                                )
                            )
                        )
                    ),
                    tags$div(class="col-md-6",
                        tags$div(class="card h-100",
                            tags$div(class="card-body",
                                tags$h5(class="card-title", "Factores 'Fáciles de Cambiar'"),
                                tags$p(class="card-text", "Estos factores pueden ser aplicados con precisión en áreas pequeñas sin interferir con las parcelas vecinas. Se adaptan bien a ", strong("parcelas pequeñas.")),
                                tags$ul(
                                    tags$li("Variedades o genotipos."),
                                    tags$li("Dosis de fertilizantes granulados."),
                                    tags$li("Aplicaciones de fungicidas o herbicidas con mochila.")
                                )
                            )
                        )
                    )
                ),
                tags$p(class="mt-3",
                    "Cuando un experimento involucra la combinación de un factor 'difícil' y uno 'fácil', el diseño en parcelas divididas (Split-Plot) emerge como la solución más eficiente y práctica (Gomez & Gomez, 1984)."
                ),

                tags$hr(),

                tags$h4(class = "section-header", "1.2 Estructura y Constructor del Diseño Split-Plot"),
                tags$p("El diseño Split-Plot tiene una estructura jerárquica. La forma en que se aleatorizan las parcelas principales depende de si el campo es homogéneo (DCA) o si necesita controlar un gradiente (DBCA)."),

                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Define tus Factores"),
                        
                        # 1. Elegir el diseño base
                        radioButtons(ns("sp_diseno_base"), "Diseño Base de Aleatorización:",
                                    choices = c("DCA (Campo Homogéneo)" = "DCA",
                                                "DBCA (Campo con Gradiente)" = "DBCA"),
                                    selected = "DBCA"),
                        
                        # 2. El input de bloques ahora es condicional
                        conditionalPanel(
                            condition = "input.sp_diseno_base == 'DBCA'",
                            ns = ns, # Necesario para que la condición funcione en módulos
                            numericInput(ns("sp_bloques"), "Número de Bloques:", value = 3, min = 2, max = 8)
                        ),
                        # Si es DCA, el número de "bloques" es en realidad el número de replicaciones
                        conditionalPanel(
                            condition = "input.sp_diseno_base == 'DCA'",
                            ns = ns,
                            numericInput(ns("sp_reps_dca"), "Número de Replicaciones:", value = 3, min = 2, max = 8)
                        ),
                        
                        # 3. Niveles de los factores
                        numericInput(ns("sp_niveles_A"), "Niveles del Factor Principal (Parcela Grande):", value = 3, min = 2, max = 5),
                        numericInput(ns("sp_niveles_B"), "Niveles del Factor Secundario (Sub-Parcela):", value = 4, min = 2, max = 6),
                        
                        actionButton(ns("generar_diseno_sp"), "Generar Diseño", icon = icon("th-large"), class = "btn-success w-100 mt-3")
                    ),
                    mainPanel(
                        width = 9,
                        tags$h5(textOutput(ns("layout_titulo"))), # Título dinámico
                        plotOutput(ns("plot_layout_sp"), height = "500px")
                    )
                ),
            ),

            # ===== PESTAÑA 2: El Modelo y los Dos Errores =====
            nav_panel(
                title = "2. El Modelo y los Dos Errores",
                
                tags$h4(class = "section-header", "2.1 El Concepto Clave: Dos Tamaños, Dos Errores"),
                tags$p(
                    "Para entender el análisis de un Split-Plot, debemos abandonar la idea de un único 'error experimental'. Debido a su estructura jerárquica, este diseño tiene ", strong("dos unidades experimentales de diferente tamaño"), " y, por lo tanto, ", strong("dos varianzas de error distintas"), "."
                ),
                
                # --- Analogía visual y desglose ---
                tags$div(class = "card",
                    tags$div(class = "card-header", strong("Analogía Práctica: Labranza y Variedades")),
                    tags$div(class = "card-body",
                        p("Imaginemos un experimento con:", tags$br(),
                        strong("Factor A (Principal):"), " 2 métodos de labranza (Arado, Siembra Directa).", tags$br(),
                        strong("Factor B (Secundario):"), " 3 variedades de maíz (V1, V2, V3)."),
                        
                        fluidRow(
                            # Columna para la Parcela Principal
                            column(6, style="border-right: 1px solid #ddd;",
                                tags$h5("La Parcela Principal (Whole Plot)"),
                                tags$img(src="images/tractor.png", width="180px", style="float: right; margin-left: 10px;"), # Asume que tienes una imagen de un tractor
                                p("La unidad experimental para el factor labranza es una parcela grande, ya que se necesita un tractor para arar. Dentro de un bloque, comparamos las parcelas grandes entre sí."),
                                p(strong("Error (a) - Error de la Parcela Principal:")),
                                p(class="text-muted", "Es la variabilidad natural que existe ", em("entre dos parcelas grandes que recibieron el mismo tipo de labranza."), " Es relativamente grande porque las parcelas son grandes y están más separadas. Este error se usa para probar el efecto de la labranza.")
                            ),
                            # Columna para la Sub-Parcela
                            column(6,
                                tags$h5("La Sub-Parcela (Subplot)"),
                                tags$img(src="images/seeds.png", width="180px", style="float: right; margin-left: 10px;"), # Asume que tienes una imagen de semillas
                                p("La unidad experimental para el factor variedad es una pequeña sub-parcela dentro de una parcela grande. Las comparaciones entre variedades se hacen ", em("dentro"), " de la misma parcela de labranza."),
                                p(strong("Error (b) - Error de la Sub-Parcela:")),
                                p(class="text-muted", "Es la variabilidad natural que existe ", em("entre dos sub-parcelas vecinas que recibieron la misma variedad."), " Es más pequeño que el Error (a) porque las sub-parcelas son pequeñas y están juntas, en condiciones más homogéneas.")
                            )
                        )
                    )
                ),
                
                tags$h4(class = "section-header", "2.2 La Consecuencia: Precisión Diferencial"),
                tags$p(
                    "Esta estructura de dos errores tiene una consecuencia fundamental que guía el diseño de cualquier experimento Split-Plot (Yates, 1937):"
                ),
                tags$div(class="alert alert-info text-center",
                    strong("Mayor Precisión para el Factor Secundario y la Interacción."),
                    br(),
                    strong("Menor Precisión para el Factor Principal.")
                ),
                tags$p(
                    "¿Por qué? Porque el Factor Principal (labranza) se replica menos veces (solo hay una parcela de 'Arado' por bloque) y se prueba contra un error más grande (Error a). En cambio, el Factor Secundario (variedades) se replica más veces (hay una subparcela de 'V1' en cada parcela principal) y se prueba contra un error más pequeño (Error b).",
                    strong(" Regla de oro: Asigna a la sub-parcela el factor cuyos efectos te interesan más o crees que son más sutiles y difíciles de detectar.")
                ),
                
                tags$hr(),

                tags$h4(class = "section-header", "2.3 El Modelo Lineal Desglosado"),
                tags$p("El modelo matemático formaliza esta estructura. Para un Split-Plot en DBCA, el modelo es:"),
                withMathJax(helpText(
                    "$$Y_{ijk} = \\mu + \\rho_k + \\alpha_i + \\delta_{ik} + \\beta_j + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
                )),
                tags$p("Traducido a nuestro ejemplo de Labranza (\\(\\alpha\\)) y Variedad (\\(\\beta\\)) en Bloques (\\(\\rho\\)):"),
                tags$ul(
                    tags$li("\\(Y_{ijk}\\): El rendimiento de la variedad \\(j\\) en el método de labranza \\(i\\) del bloque \\(k\\)."),
                    tags$li("\\(\\mu\\): El rendimiento promedio general de todo el ensayo."),
                    tags$li("\\(\\rho_k\\): El efecto del Bloque \\(k\\) (ej. la diferencia de fertilidad entre el Bloque 1 y el Bloque 2)."),
                    tags$li("\\(\\alpha_i\\): El efecto principal de la Labranza \\(i\\) (ej. la diferencia promedio entre Arado y Siembra Directa)."),
                    tags$li(strong("\\(\\delta_{ik}\\)"), " (Error a): La variación aleatoria entre las parcelas grandes. ¡Este es el error para probar la Labranza!"),
                    tags$li("\\(\\beta_j\\): El efecto principal de la Variedad \\(j\\) (ej. la diferencia promedio entre V1, V2 y V3)."),
                    tags$li("\\((\\alpha\\beta)_{ij}\\): El efecto de la Interacción Labranza x Variedad (ej. si la ventaja de V1 sobre V2 es diferente en Arado que en Siembra Directa)."),
                    tags$li(strong("\\(\\epsilon_{ijk}\\)"), " (Error b): La variación aleatoria entre las sub-parcelas. ¡Este es el error para probar la Variedad y la Interacción!")
                ),

                tags$h4(class = "section-header", "2.4 La Tabla ANOVA del Split-Plot: Anatomía del Análisis"),
                tags$p(
                    "La estructura de dos errores del diseño Split-Plot se refleja directamente en la tabla del Análisis de Varianza (ANOVA). A diferencia de los diseños anteriores, la salida de R se presentará en ", strong("dos estratos o secciones separadas."), " Cada estrato utiliza un término de error diferente como denominador para su prueba F, reflejando las dos precisiones distintas del experimento."
                ),

                # --- Estrato 1: Parcelas Principales ---
                tags$h5(tags$strong("Estrato 1: El Análisis de las Parcelas Principales (Whole-Plot)")),
                tags$p(
                    "Esta primera parte de la tabla se enfoca en las comparaciones realizadas entre las parcelas grandes. Utiliza el ", strong("Error (a)"), " como su medida de 'ruido' o variabilidad aleatoria."
                ),
                tags$table(class="table table-bordered", style="text-align: center;",
                    tags$thead(
                        tags$tr(
                            tags$th("Fuente de Variación"), tags$th("Suma de Cuadrados (SC)"), tags$th("Grados de Libertad (gl)"), tags$th("Cuadrado Medio (CM)"), tags$th("Estadístico F")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td(style="text-align: left;", "Bloque"), tags$td("SC(Bloque)"), tags$td("b-1"), tags$td("CM(Bloque)"), tags$td(tags$code("CM(Bloque) / CM(Error a)"))
                        ),
                        tags$tr(
                            tags$td(style="text-align: left;", "Factor A (Labranza)"), tags$td("SC(A)"), tags$td("a-1"), tags$td("CM(A)"), tags$td(strong(tags$code("CM(A) / CM(Error a)")))
                        ),
                        tags$tr(class="table-warning",
                            tags$td(style="text-align: left;", strong("Error (a) = Bloque x Factor A")), tags$td("SC(Error a)"), tags$td("(b-1)(a-1)"), tags$td(strong("CM(Error a)")), tags$td()
                        )
                    )
                ),
                tags$ul(
                    tags$li(
                        strong("Factor A (Labranza):"), " Esta es la prueba para nuestro factor principal. Un p-valor significativo aquí indica que, en promedio, hay una diferencia en el rendimiento entre los métodos de labranza. Sin embargo, esta prueba es ", em("poco potente"), " porque el CM(A) se divide por el CM(Error a), que suele ser grande."
                    ),
                    tags$li(
                        strong("Error (a):"), " Este término, también conocido como ", strong("Error de la Parcela Principal"), ", representa la variabilidad aleatoria entre las parcelas grandes dentro de los bloques. En el modelo, es el término \\(\\delta_{ik}\\). Se calcula como la interacción entre el Bloque y el Factor Principal, y es la medida de 'ruido' para las comparaciones a gran escala. Si este valor es grande, indica una alta variabilidad entre las parcelas principales."
                    )
                ),

                # --- Estrato 2: Sub-Parcelas ---
                tags$h5(tags$strong("Estrato 2: El Análisis de las Sub-Parcelas (Subplot)")),
                tags$p(
                    "Esta segunda parte de la tabla se enfoca en las comparaciones realizadas dentro de las parcelas principales, donde la precisión es mayor. Utiliza el ", strong("Error (b)"), " como su medida de 'ruido'."
                ),
                tags$table(class="table table-bordered", style="text-align: center;",
                    tags$thead(
                        tags$tr(
                            tags$th("Fuente de Variación"), tags$th("Suma de Cuadrados (SC)"), tags$th("Grados de Libertad (gl)"), tags$th("Cuadrado Medio (CM)"), tags$th("Estadístico F")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td(style="text-align: left;", "Factor B (Variedad)"), tags$td("SC(B)"), tags$td("b-1"), tags$td("CM(B)"), tags$td(strong(tags$code("CM(B) / CM(Error b)")))
                        ),
                        tags$tr(
                            tags$td(style="text-align: left;", "Interacción A x B"), tags$td("SC(AxB)"), tags$td("(a-1)(b-1)"), tags$td("CM(AxB)"), tags$td(strong(tags$code("CM(AxB) / CM(Error b)")))
                        ),
                        tags$tr(class="table-success",
                            tags$td(style="text-align: left;", strong("Error (b) = Error Residual")), tags$td("SC(Error b)"), tags$td("a(b-1)(r-1)"), tags$td(strong("CM(Error b)")), tags$td()
                        )
                    )
                ),
                tags$ul(
                    tags$li(
                        strong("Factor B (Variedad) e Interacción A x B:"), " Estas son las pruebas para el factor secundario y, lo más importante, para la interacción. Son pruebas ", em("más potentes y precisas"), " porque sus Cuadrados Medios se dividen por el CM(Error b), que suele ser más pequeño."
                    ),
                    tags$li(
                        strong("Error (b):"), " Este término, conocido como ", strong("Error de la Sub-Parcela"), " o Error Residual, representa la variabilidad aleatoria entre las sub-parcelas dentro de la misma parcela principal. En el modelo, es el término \\(\\epsilon_{ijk}\\). Es la medida de 'ruido' para las comparaciones finas y detalladas. Un valor pequeño aquí indica que las mediciones dentro de una misma parcela grande fueron muy consistentes."
                    )
                ),

                tags$div(class="card",
                    tags$strong("En Resumen: ¿Qué Significa Cada Error?"),
                    tags$ul(
                        tags$li(tags$b("Error (a)"), " te dice: '¿Cuán diferentes son dos parcelas grandes tratadas de la misma forma?' Mide la variabilidad a gran escala."),
                        tags$li(tags$b("Error (b)"), " te dice: '¿Cuán diferentes son dos sub-parcelas vecinas tratadas de la misma forma?' Mide la variabilidad a pequeña escala.")
                    ),
                    tags$p("El éxito de un diseño Split-Plot radica en que la mayor parte de la variabilidad aleatoria esté contenida en el Error (a), dejando al Error (b) 'limpio' y pequeño para detectar con alta precisión las diferencias sutiles en las sub-parcelas y en la interacción.")
                )
            ),

            # ===== PESTAÑA 3: Análisis en R =====
            nav_panel(
                title = "3. Análisis en R",
                
                tags$h4(class = "section-header", "3.1 Análisis Interactivo de un Split-Plot"),
                tags$p(
                    "Ahora pondremos en práctica la teoría. Analizaremos el dataset clásico `oats` del paquete `MASS`. En este experimento, se estudió el efecto de la ", strong("fertilización nitrogenada (N)"), " y 3 ", strong("variedades de avena (V)"), " sobre el rendimiento, en un diseño en bloques. El Nitrógeno se aplicó a las parcelas principales y las Variedades a las sub-parcelas."
                ),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Control del Análisis"),
                        tags$p("Avanza por cada paso para realizar un análisis completo del diseño Split-Plot."),
                        radioButtons(ns("sp_paso_analisis"), "Paso del Análisis:",
                                    choiceNames = list(
                                        tags$span(icon("table"), "1. Exploración de Datos"),
                                        tags$span(icon("flask"), "2. Modelo y Supuestos"),
                                        tags$span(icon("clipboard-list"), "3. Interpretar ANOVA"),
                                        tags$span(icon("sitemap"), "4. Análisis Post-Hoc")
                                    ),
                                    choiceValues = c("datos", "supuestos", "anova", "posthoc")),
                        tags$hr(),
                        tags$div(class="note-cloud",
                            tags$strong("Factores del Ensayo `oats`"),
                            tags$ul(
                                tags$li(strong("Bloques:"), " 6 bloques (Factor `B`)."),
                                tags$li(strong("Factor Principal:"), " 4 niveles de Nitrógeno (Factor `N`)."),
                                tags$li(strong("Factor Secundario:"), " 3 variedades de avena (Factor `V`).")
                            )
                        )
                    ),
                    mainPanel(
                        width = 9,
                        # La salida cambiará dinámicamente según el paso seleccionado
                        uiOutput(ns("sp_salida_analisis"))
                    )
                )
            ),
            
            # ===== PESTAÑA 4: Post-Hoc y Resumen de Diseños (VERSIÓN MEJORADA) =====
            nav_panel(
                title = "4. Post-Hoc y Resumen de Diseños",
                
                tags$h4(class = "section-header", "4.1 Estrategias Avanzadas para Post-Hoc en Split-Plots"),
                tags$p(
                    "Como vimos en el análisis interactivo, realizar comparaciones múltiples en un Split-Plot requiere cuidado para usar el término de error correcto. Si bien ", code("agricolae"),
                    " es una herramienta clásica, el paquete ", code("emmeans"), " ofrece un enfoque más moderno y a menudo más seguro, ya que intenta detectar automáticamente la estructura de error del modelo."
                ),
                
                tags$h5(tags$strong("Enfoque recomendado con `emmeans`:")),
                tags$p(
                    "`emmeans` puede manejar la estructura `aovlist` directamente, lo que simplifica el código y reduce el riesgo de errores."
                ),
                tags$pre(class="r-code",
                    htmltools::HTML(
                        "# Asumiendo que 'modelo_sp' es el resultado de aov() con Error()\n",
                        "library(emmeans)\n\n",
                        "# 1. Verificar primero la interacción\n",
                        "emm_int <- emmeans(modelo_sp, ~ nitrogeno * variedad)\n",
                        "joint_tests(emm_int) # Esto da una prueba F para la interacción\n\n",
                        "# 2. Si la interacción es significativa, analizar efectos simples:\n",
                        "comparaciones_simples <- emmeans(modelo_sp, pairwise ~ nitrogeno | variedad)\n",
                        "print(comparaciones_simples)\n\n",
                        "# 3. Si la interacción NO es significativa, analizar efectos principales:\n",
                        "emm_nitrogeno <- emmeans(modelo_sp, ~ nitrogeno)\n",
                        "pairs(emm_nitrogeno) # Compara los niveles de nitrógeno\n\n",
                        "emm_variedad <- emmeans(modelo_sp, ~ variedad)\n",
                        "pairs(emm_variedad) # Compara los niveles de variedad"
                    )
                ),
                tags$div(class="note-cloud",
                    strong("¿Por qué `emmeans` es a menudo preferible?"),
                    " `emmeans` es consciente de la estructura del modelo. Al analizar el efecto principal del nitrógeno, sabe que debe promediar sobre las variedades y usar la estructura de error correcta (derivada del Error a) para las comparaciones, haciendo el análisis menos propenso a errores manuales."
                ),
                
                tags$hr(),

                tags$h4(class = "section-header", "4.2 Tabla Comparativa: Guía para la Elección del Diseño Experimental"),
                tags$p(
                    "La elección del diseño correcto es una de las decisiones más críticas en la investigación. No existe un 'mejor' diseño; existe el diseño ", em("más apropiado"),
                    " para tu pregunta de investigación, tus recursos y las condiciones de tu campo. Esta tabla resume los diseños que hemos visto y otros comunes para ayudarte a decidir."
                ),
                
                tags$div(class = "table-responsive",
                    tags$table(class = "table table-bordered table-hover",
                        tags$thead(class="table-light",
                            tags$tr(
                                tags$th("Diseño Experimental"),
                                tags$th("Objetivo Principal"),
                                tags$th("Estructura Clave"),
                                tags$th("Fortaleza Principal"),
                                tags$th("Limitación Principal")
                            )
                        ),
                        tags$tbody(
                            tags$tr(
                                tags$td(strong("DCA")),
                                tags$td("Comparar tratamientos en condiciones totalmente homogéneas."),
                                tags$td("Aleatorización completa de todos los tratamientos."),
                                tags$td("Máxima simplicidad y flexibilidad. Mayor número de g.l. para el error."),
                                tags$td("Muy sensible a la heterogeneidad del campo. Rara vez aplicable fuera del laboratorio/invernadero.")
                            ),
                            tags$tr(
                                tags$td(strong("DBCA")),
                                tags$td("Comparar tratamientos controlando UNA fuente de variación (gradiente)."),
                                tags$td("Todos los tratamientos están en cada bloque. Aleatorización dentro de bloques."),
                                tags$td("Aumenta drásticamente la precisión si el bloqueo es efectivo."),
                                tags$td("Ineficaz si el gradiente es irregular o si hay más de una fuente de variación.")
                            ),
                            tags$tr(
                                tags$td(strong("Cuadrado Latino (DCL)")),
                                tags$td("Comparar tratamientos controlando DOS fuentes de variación perpendiculares."),
                                tags$td("Nº Tratamientos = Nº Filas = Nº Columnas. Cada tratamiento aparece una vez por fila/columna."),
                                tags$td("Extremadamente eficiente para controlar doble gradiente (ej. pendiente y exposición solar)."),
                                tags$td("Muy rígido. El número de tratamientos está restringido y no es práctico para > 8 tratamientos.")
                            ),
                            tags$tr(
                                tags$td(strong("Factorial")),
                                tags$td("Estudiar los efectos de dos o más factores y, crucialmente, sus INTERACCIONES."),
                                tags$td("Todas las combinaciones de los niveles de los factores son probadas."),
                                tags$td("Es el único diseño que permite descubrir sinergias o antagonismos entre factores."),
                                tags$td("El número de tratamientos combinados crece exponencialmente, puede volverse muy grande.")
                            ),
                            tags$tr(
                                tags$td(strong("Parcelas Divididas (Split-Plot)")),
                                tags$td("Implementar un diseño factorial cuando un factor requiere parcelas mucho más grandes que el otro."),
                                tags$td("Estructura jerárquica: Factor A en parcelas grandes, Factor B en sub-parcelas dentro de A."),
                                tags$td("Logísticamente práctico para factores como labranza o riego. Alta precisión para el factor secundario y la interacción."),
                                tags$td("Baja precisión para el factor principal. Análisis y post-hoc más complejos (dos errores).")
                            ),
                            tags$tr(
                                tags$td(strong("Parcelas en Franjas (Strip-Plot)")),
                                tags$td("Implementar un factorial donde AMBOS factores requieren parcelas grandes."),
                                tags$td("Factor A aplicado en franjas en una dirección, Factor B en franjas perpendiculares."),
                                tags$td("Soluciona el problema logístico de dos factores 'difíciles de cambiar' (ej. labranza y método de siembra)."),
                                tags$td("Análisis muy complejo (tres errores). La precisión para la interacción es alta, pero para ambos efectos principales es baja.")
                            ),
                            tags$tr(
                                tags$td(strong("Bloques Incompletos (BIBD)")),
                                tags$td("Comparar muchos tratamientos cuando el tamaño del bloque es demasiado pequeño para contenerlos a todos."),
                                tags$td("Cada bloque contiene solo un subconjunto de los tratamientos, pero el diseño está balanceado."),
                                tags$td("Permite controlar la variabilidad en ensayos con muchísimos tratamientos (ej. mejoramiento genético)."),
                                tags$td("Requiere que se cumplan condiciones combinatorias específicas. El análisis es más complejo (ajuste inter-bloque).")
                            ),
                            tags$tr(
                                tags$td(strong("Diseños Aumentados")),
                                tags$td("Evaluar un gran número de tratamientos nuevos (no replicados) junto a unos pocos controles (replicados)."),
                                tags$td("Los controles se repiten en cada bloque para estimar la variabilidad, mientras que los tratamientos nuevos aparecen solo una vez."),
                                tags$td("Extremadamente eficiente en recursos para fases tempranas de mejoramiento genético."),
                                tags$td("Baja precisión para los tratamientos nuevos. Las comparaciones dependen fuertemente de los controles.")
                            )
                        )
                    )
                ),
                tags$div(class="question mt-4",
                    strong("Pregunta Final: ¿Cómo decidir?"),
                    p("La elección del diseño es un balance entre el ideal estadístico y la realidad logística. Comienza con la pregunta más simple: '¿Cuántos factores estoy estudiando?' Si es uno, piensa en DCA o DBCA. Si son más de uno, piensa en Factorial. Luego, haz la pregunta logística: '¿Puedo aleatorizar todas las combinaciones fácilmente?' Si la respuesta es no, piensa en Split-Plot. Finalmente, considera la escala: '¿Tengo demasiados tratamientos para un bloque normal?' Si es así, explora diseños en bloques incompletos o aumentados.")
                )
            ),

            # ===== PESTAÑA 5: BLOQUES INCOMPLETOS Y ANVA AUXILIAR =====
            nav_panel(
                title = "5. Eficiencia de Diseños",
                
                h4(class = "section-header", "5.1 La Pregunta del Experimentador Eficiente: ¿Mi Diseño Valió la Pena?"),
                p(
                    "A lo largo de este curso, hemos ascendido en la escala de complejidad de los diseños experimentales. Cada paso, desde un DCA a un DBCA, y de un DBCA a un Alfa-Látice o un Split-Plot, se justifica por un intento de controlar una fuente específica de 'ruido' o variabilidad. El bloqueo controla gradientes espaciales, las parcelas divididas manejan factores logísticamente difíciles, y los bloques incompletos manejan un gran número de tratamientos."
                ),
                p(
                    "Sin embargo, esta complejidad tiene un costo: en la planificación, en la ejecución y en el análisis. La pregunta fundamental que todo buen experimentador debe hacerse es: ", strong("¿La ganancia en precisión justificó la complejidad añadida?"), " Para responder a esta pregunta de manera cuantitativa, utilizamos la técnica del ", strong("Análisis de Varianza Auxiliar (ANVA Auxiliar).")
                ),

                tags$hr(),

                h4(class = "section-header", "5.2 Desmitificando el ANVA Auxiliar: El Principio del '¿Qué Hubiera Pasado?'"),

                # --- Sección de Aclaración: ANVA vs. ANOVA ---
                tags$div(class = "card bg-light mb-4", # 'mb-4' para un margen inferior
                    tags$div(class = "card-body",
                        tags$div(class = "row align-items-center",
                            tags$div(class = "col-md-2 text-center",
                                p(icon("language", class="fa-4x text-info"))
                            ),
                            tags$div(class = "col-md-10",
                                tags$h5(class = "card-title", "Aclaración Terminológica: ¿ANVA o ANOVA?"),
                                p(
                                    "A lo largo de la literatura estadística y en diferentes regiones, te encontrarás con dos acrónimos que parecen distintos pero significan exactamente lo mismo: ", strong("ANVA"), " y ", strong("ANOVA.")
                                ),
                                tags$ul(
                                    tags$li(strong("ANOVA:"), " Es el acrónimo en inglés para ", tags$em(strong("ANalysis Of VAriance.")), " Es el término más utilizado a nivel internacional, en software estadístico (como R) y en la mayoría de las publicaciones científicas."),
                                    tags$li(strong("ANVA:"), " Es el acrónimo en español para ", tags$em(strong("ANálisis de la VArianza.")), " Es la traducción directa y correcta, comúnmente utilizada en textos académicos y enseñanza en español.")
                                )
                            )
                        )
                    ),
                    tags$div(class="card-footer text-center bg-info text-white",
                        strong("Conclusión: ANVA = ANOVA. Son sinónimos. En este curso, usaremos ambos términos indistintamente, pero recuerda que al escribir código en R, las funciones siempre usarán la terminología en inglés (ej. `aov`, `anova`).")
                    )
                ),

                tags$hr(),
                
                p(
                    "Para entender el valor de un diseño complejo, debemos compararlo con una alternativa más simple. El ANVA Auxiliar es nuestra 'máquina del tiempo' estadística: nos permite tomar los resultados de nuestro experimento real y responder a la pregunta: ", strong("¿Qué nivel de 'ruido' o error experimental habríamos tenido si hubiéramos optado por un diseño más básico?")
                ),

                tags$div(class = "row align-items-center", # 'align-items-center' para centrar verticalmente el contenido
                    # Columna para el ANVA Real
                    tags$div(class = "col-md-5 text-center",
                        tags$div(class="card border-success shadow-sm",
                            tags$div(class="card-header bg-success text-white", strong("ANVA del Diseño Real (Complejo)")),
                            tags$div(class="card-body",
                                p(icon("microscope", class="fa-3x text-success")),
                                p("Se analiza el experimento usando el ", strong("modelo matemático completo"), " que describe con precisión cómo se realizó el ensayo en campo (ej. reconociendo bloques incompletos, parcelas divididas, etc.)."),
                                p("El resultado es el ", code("CME_Real"), ", que representa la varianza residual o el 'ruido' después de haber controlado todas las fuentes de variación posibles con nuestro diseño sofisticado. Es nuestra medida de la ", strong("precisión alcanzada.")),
                                tags$p(class="card-text small text-muted", "Modelo: Y ~ Bloques + Tratamientos + (Términos del Diseño Complejo)")
                            )
                        )
                    ),
                    
                    # Columna central para la flecha de comparación
                    tags$div(class = "col-md-2 text-center",
                        p(icon("exchange-alt", class="fa-4x text-primary")),
                        tags$p(strong("VS"))
                    ),
                    
                    # Columna para el ANVA Auxiliar
                    tags$div(class = "col-md-5 text-center",
                        tags$div(class="card border-info shadow-sm",
                            tags$div(class="card-header bg-info text-white", strong("ANVA Auxiliar (Simple)")),
                            tags$div(class="card-body",
                                p(icon("ruler-combined", class="fa-3x text-info")),
                                p("Se analizan los ", strong("mismos datos"), ", pero se ", strong("ignora deliberadamente"), " la estructura compleja. Se utiliza un modelo más simple como punto de referencia (generalmente, el siguiente diseño más simple en la jerarquía)."),
                                p("El resultado es el ", code("CME_Auxiliar"), ", que representa la varianza residual que habríamos obtenido. Este CME inevitablemente contendrá el 'ruido' que nuestro diseño complejo logró aislar. Es nuestra medida de la ", strong("precisión base.")),
                                tags$p(class="card-text small text-muted", "Modelo: Y ~ Bloques + Tratamientos")
                            )
                        )
                    )
                ),

                tags$div(class = "card mt-4",
                    tags$div(class = "card-header", strong("Una Analogía Agronómica más Profunda: El Sistema de Riego de Precisión")),
                    tags$div(class = "card-body",
                        tags$p(
                            "Imagina que instalas un costoso sistema de ", strong("riego por goteo de precisión (Diseño Complejo)"), " en tu campo, donde cada planta recibe la cantidad exacta de agua que necesita. Al final de la temporada, mides la variabilidad en el crecimiento de las plantas y obtienes un 'error' muy bajo (", code("CME_Real"), ")."
                        ),
                        p(
                            "Ahora, para justificar la inversión, realizas un análisis 'auxiliar'. Tomas los mismos datos de crecimiento, pero en tu modelo asumes que regaste todo el campo con un ", strong("aspersor gigante (Diseño Simple)."), " Sabes que el aspersor no riega de manera uniforme; algunas áreas reciben más agua que otras. Al analizarlo de esta forma, el 'error' que calculas (", code("CME_Auxiliar"), ") será mucho mayor, porque ahora incluye la variabilidad debida al riego desigual."
                        ),
                        tags$blockquote(class="blockquote text-center",
                            p(class="mb-0", "La ", strong("Eficiencia Relativa"), " es simplemente la razón entre el 'error' del aspersor gigante y el 'error' del riego por goteo. Te dice exactamente cuánto mejor fue tu sistema de precisión.")
                        )
                    )
                ),

                tags$hr(),

                h4(class = "section-header", "5.3 La Matemática de la Precisión: La Eficiencia Relativa (ER)"),
                p(
                    "La comparación cuantitativa se realiza mediante la ", strong("Eficiencia Relativa (ER)."), " La fórmula es una simple razón de los Cuadrados Medios del Error (CME), donde un CME más bajo indica un experimento más preciso."
                ),
                withMathJax(helpText(
                    "$$ER = \\frac{CME_{Auxiliar \\; (Simple)}}{CME_{Real \\; (Complejo)}}$$"
                )),
                p("La ER tiene una interpretación muy práctica y poderosa:"),
                tags$ul(
                    tags$li(
                        "Un ER de 1.25 significa que el diseño complejo fue un 25% más preciso que el diseño simple. Para decirlo de otra manera, ", strong("por cada 100 repeticiones que necesitarías en el diseño simple para alcanzar una cierta precisión, el diseño complejo solo necesita 80 repeticiones"), " (100 / 1.25 = 80). Esto representa un ahorro masivo de recursos."
                    ),
                    tags$li(
                        "La ER cuantifica directamente el retorno de la inversión de haber elegido un diseño más sofisticado. Es la justificación numérica de tu pericia como experimentador."
                    )
                ),
                p(strong("Ajuste por Grados de Libertad:"), " Una fórmula más precisa de la ER, especialmente para muestras pequeñas, también considera los grados de libertad (gl) del error de cada modelo, ya que el diseño complejo 'gasta' más gl. Sin embargo, para fines prácticos, la razón de los CME es la más utilizada e intuitiva."),
                withMathJax(helpText(
                    "$$ER_{ajustada} = \\frac{(gl_{Real} + 1)(gl_{Auxiliar} + 3)}{(gl_{Real} + 3)(gl_{Auxiliar} + 1)} \\times \\frac{CME_{Auxiliar}}{CME_{Real}}$$"
                )),
                
                tags$hr(),

                h4(class = "section-header", "5.4 Guía de Aplicación: Escenarios Comunes del ANVA Auxiliar"),
                p(
                    "La evaluación de la eficiencia es una práctica que valida nuestras decisiones de diseño y guía la planificación futura. A continuación, se desglosan los escenarios más comunes en fichas técnicas para entender cuándo y por qué se aplica esta comparación."
                ),

                # --- Escenario 1: Bloques Incompletos ---
                tags$div(class="card mb-4", # Aumentar el margen inferior
                    tags$div(class="card-header bg-primary text-white",
                        strong("Escenario 1: Justificando Diseños en Bloques Incompletos (ej. Alfa-Látice)")
                    ),
                    tags$div(class="card-body",
                        tags$h5(class="card-title", "Contexto Agronómico Típico:"),
                        p(class="card-text", em("Un programa de mejoramiento genético necesita evaluar 64 líneas nuevas de trigo. Un bloque completo de 64 parcelas sería demasiado grande y estaría afectado por la variabilidad del suelo (heterogeneidad). Se opta por un diseño Alfa-Látice con 8 bloques incompletos de 8 parcelas cada uno, dentro de cada replicación.")),
                        tags$hr(),
                        
                        tags$h6(strong("La Comparación Analítica:")),
                        tags$table(class="table table-sm",
                            tags$tbody(
                                tags$tr(
                                    tags$td(icon("check-circle", class="text-success"), strong("ANVA Real:")),
                                    tags$td("Se usa el modelo Alfa-Látice que reconoce las replicaciones y los bloques incompletos anidados: ", code("aov(Y ~ Trat + Rep + Rep:BloqueIncompleto)"))
                                ),
                                tags$tr(
                                    tags$td(icon("balance-scale", class="text-info"), strong("ANVA Auxiliar:")),
                                    tags$td("Se usa un modelo DBCA simple que ignora los bloques incompletos: ", code("aov(Y ~ Trat + Rep)"))
                                )
                            )
                        ),

                        tags$div(class="alert alert-light",
                            strong("Pregunta que Responde la Eficiencia Relativa (ER):"), br(),
                            em("¿Valió la pena el esfuerzo de crear y analizar los bloques pequeños? ¿La heterogeneidad a micro-escala era lo suficientemente grande como para justificar el diseño Látice sobre un simple DBCA?")
                        ),

                        tags$h6(strong("Conclusión Guiada por la ER:")),
                        tags$ul(
                            tags$li(strong("Si ER > 1.10:"), " ¡Decisión Acertada! El Látice fue significativamente más preciso. El suelo tenía variabilidad a pequeña escala que fue controlada con éxito, limpiando el error y aumentando la potencia para detectar diferencias reales entre los genotipos."),
                            tags$li(strong("Si ER ≈ 1.00:"), " Decisión Neutral. Los bloques incompletos no aportaron un beneficio claro. El campo era probablemente más homogéneo de lo esperado. Para futuros ensayos, un DBCA sería suficiente y más simple.")
                        )
                    )
                ),

                # --- Escenario 2: Parcelas Divididas ---
                tags$div(class="card mb-4",
                    tags$div(class="card-header bg-info text-white",
                        strong("Escenario 2: Justificando un Diseño en Parcelas Divididas (Split-Plot)")
                    ),
                    tags$div(class="card-body",
                        tags$h5(class="card-title", "Contexto Agronómico Típico:"),
                        p(class="card-text", em("Se quiere estudiar el efecto de 3 métodos de riego (Factor A, difícil de cambiar) y 4 dosis de un fungicida (Factor B, fácil de cambiar). Aleatorizar las 12 combinaciones es impráctico. Se implementa un Split-Plot con el riego en las parcelas grandes y el fungicida en las sub-parcelas.")),
                        tags$hr(),
                        
                        tags$h6(strong("La Comparación Analítica:")),
                        tags$table(class="table table-sm",
                            tags$tbody(
                                tags$tr(
                                    tags$td(icon("check-circle", class="text-success"), strong("ANVA Real:")),
                                    tags$td("Se usa el modelo Split-Plot con dos errores: ", code("aov(Y ~ Bloque + Riego + Error(Bloque:Riego) + Fungicida + Riego:Fungicida)"))
                                ),
                                tags$tr(
                                    tags$td(icon("balance-scale", class="text-info"), strong("ANVA Auxiliar:")),
                                    tags$td("Se analiza como un factorial convencional en DBCA: ", code("aov(Y ~ Bloque + Riego * Fungicida)"))
                                )
                            )
                        ),
                        
                        tags$div(class="alert alert-light",
                            strong("Pregunta que Responde la Eficiencia Relativa (ER):"), br(),
                            em("Considerando la precisión para el factor secundario y la interacción (usando el CMEb), ¿la estructura Split-Plot fue una estrategia eficiente? ¿La ganancia en viabilidad logística y precisión en las sub-parcelas compensó la pérdida de precisión en el factor principal?")
                        ),

                        tags$h6(strong("Conclusión Guiada por la ER:")),
                        tags$ul(
                            tags$li(strong("Si ER alta:"), " Justificado. El diseño fue logísticamente viable y proporcionó alta precisión para las preguntas más importantes (efecto del fungicida y su interacción con el riego). La homogeneidad dentro de las parcelas de riego era alta."),
                            tags$li(strong("Si ER baja:"), " Cuestionable. Si la variabilidad dentro de las parcelas grandes era casi tan alta como entre ellas, la ventaja del Split-Plot se pierde y las conclusiones sobre el factor principal son muy poco fiables.")
                        )
                    )
                ),

                # --- Escenario 3: DBCA ---
                tags$div(class="card mb-4",
                    tags$div(class="card-header bg-secondary text-white",
                        strong("Escenario 3: Evaluando la Efectividad del Bloqueo (DBCA vs. DCA)")
                    ),
                    tags$div(class="card-body",
                        tags$h5(class="card-title", "Contexto Agronómico Típico:"),
                        p(class="card-text", em("Se evalúan 5 fungicidas. El campo tiene una ligera pendiente de norte a sur, por lo que se establecen bloques perpendiculares a esa pendiente. Quiero cuantificar si el bloqueo realmente funcionó.")),
                        tags$hr(),
                        
                        tags$h6(strong("La Comparación Analítica:")),
                        tags$table(class="table table-sm",
                            tags$tbody(
                                tags$tr(
                                    tags$td(icon("check-circle", class="text-success"), strong("ANVA Real:")),
                                    tags$td("Se usa el modelo DBCA, que extrae la variación debida a los bloques: ", code("aov(Y ~ Bloque + Tratamiento)"))
                                ),
                                tags$tr(
                                    tags$td(icon("balance-scale", class="text-info"), strong("ANVA Auxiliar:")),
                                    tags$td("Se usa un modelo DCA, que agrupa toda la variación no explicada por el tratamiento en el error: ", code("aov(Y ~ Tratamiento)"))
                                )
                            )
                        ),

                        tags$div(class="alert alert-light",
                            strong("Pregunta que Responde la Eficiencia Relativa (ER):"), br(),
                            em("¿El gradiente de campo que yo sospechaba era real y lo suficientemente fuerte? ¿Cuánto 'ruido' (varianza) logré sacar del error experimental y atribuirlo correctamente a los bloques?")
                        ),

                        tags$h6(strong("Conclusión Guiada por la ER:")),
                        p(
                            "En este caso, aunque se puede calcular la ER, la forma más directa de evaluar la efectividad del bloqueo es simplemente mirar el ", strong("p-valor del factor 'Bloque' en la tabla ANOVA del DBCA."), " Si ese p-valor es significativo (ej. < 0.10 o < 0.05), significa que el bloqueo fue efectivo y no es necesario calcular la ER. Si no es significativo, significa que el campo era homogéneo y un DCA habría sido suficiente."
                        )
                    )
                )
            )
        )
    )
}

# Server para la Sesión 7
session7Server <- function(input, output, session) {
        
    # --- LÓGICA PARA LA PESTAÑA 1: CONSTRUCTOR DE DISEÑO SPLIT-PLOT ---
        
    # eventReactive para generar el diseño
    diseno_sp_generado <- eventReactive(input$generar_diseno_sp, {
        
        req(input$sp_diseno_base, input$sp_niveles_A, input$sp_niveles_B)
        
        # Definir parámetros
        factor_A_niveles <- paste0("A", 1:input$sp_niveles_A)
        factor_B_niveles <- paste0("B", 1:input$sp_niveles_B)
        
        # Determinar el número de replicaciones/bloques
        if (input$sp_diseno_base == "DBCA") {
            req(input$sp_bloques)
            n_bloques <- input$sp_bloques
            tipo <- "DBCA"
        } else {
            req(input$sp_reps_dca)
            n_bloques <- input$sp_reps_dca
            tipo <- "DCA"
        }
        
        # --- Generación Manual del Data Frame ---
        
        lista_bloques <- list()
        
        for (b in 1:n_bloques) {
            # 1. Aleatorizar el Factor Principal (A)
            parcelas_principales <- sample(factor_A_niveles)
            
            # 2. Para cada Parcela Principal, aleatorizar el Factor Secundario (B)
            df_bloque <- lapply(parcelas_principales, function(parcela_A) {
                sub_parcelas <- sample(factor_B_niveles)
                data.frame(
                    bloque = b,
                    parcela_principal = parcela_A,
                    sub_parcela = sub_parcelas
                )
            }) %>% bind_rows()
            
            lista_bloques[[b]] <- df_bloque
        }
        
        # Combinar todos los bloques en un solo data frame
        plan_completo <- bind_rows(lista_bloques)
        
        # Crear coordenadas para ggplot
        plan_final <- plan_completo %>%
            mutate(
                # Asegurar que sean factores para el gráfico
                parcela_principal = factor(parcela_principal, levels = factor_A_niveles),
                sub_parcela = factor(sub_parcela, levels = factor_B_niveles)
            ) %>%
            # Las coordenadas X son las parcelas principales, las Y son las subparcelas
            mutate(
                x_coord = as.numeric(parcela_principal),
                y_coord = as.numeric(sub_parcela)
            )
            
        return(list(plan = plan_final, tipo = tipo))
        
    }, ignoreNULL = FALSE)

    # Título dinámico para el gráfico
    output$layout_titulo <- renderText({
        diseno <- diseno_sp_generado()
        req(diseno)
        if (diseno$tipo == "DBCA") {
            "Layout del Campo: Split-Plot en DBCA"
        } else {
            "Layout del Campo: Split-Plot en DCA (Replicaciones Aleatorizadas)"
        }
    })

    # Gráfico del layout
    output$plot_layout_sp <- renderPlot({
        
        diseno <- diseno_sp_generado()
        req(diseno)
        
        plan_df <- diseno$plan
        
        main_plots_df <- plan_df %>%
            group_by(bloque, parcela_principal, x_coord) %>%
            summarize(
                y_center = (max(y_coord) + min(y_coord)) / 2,
                y_height = n_distinct(y_coord),
                .groups = 'drop'
            )
        
        colores_A <- RColorBrewer::brewer.pal(max(3, nlevels(plan_df$parcela_principal)), "Pastel1")
        colores_B <- RColorBrewer::brewer.pal(max(3, nlevels(plan_df$sub_parcela)), "Set2")
        
        p <- ggplot() +
            geom_tile(
                data = main_plots_df,
                aes(x = x_coord, y = y_center, height = y_height, fill = parcela_principal),
                color = "black", linewidth = 1.2, alpha = 0.5
            ) +
            geom_text(
                data = plan_df,
                aes(x = x_coord, y = y_coord, label = sub_parcela),
                color = "black", fontface = "bold", size = 6
            ) +
            scale_fill_manual(values = colores_A, name = "Factor Principal (A)") +
            scale_x_continuous(breaks = 1:nlevels(plan_df$parcela_principal), labels = levels(plan_df$parcela_principal)) +
            scale_y_continuous(breaks = 1:nlevels(plan_df$sub_parcela), labels = levels(plan_df$sub_parcela)) +
            labs(
                subtitle = "Las áreas de color son las Parcelas Principales (A). Las etiquetas son las Sub-Parcelas (B).",
                x = "Factor Principal", y = "Factor Secundario"
            ) +
            theme_bw(base_size = 14) +
            theme(legend.position = "bottom")

        # El facetado depende del tipo de diseño
        if (diseno$tipo == "DBCA") {
            p + facet_wrap(~ bloque, labeller = labeller(bloque = function(x) paste("Bloque", x)))
        } else {
            # Para un DCA, el "bloque" es solo una replicación y no se muestra como faceta
            # sino que se distribuye en un solo plano.
            p + facet_wrap(~ bloque, labeller = labeller(bloque = function(x) paste("Rep.", x)), ncol = 4) +
            labs(caption = "Nota: En DCA, los 'bloques' son replicaciones distribuidas al azar, no para controlar un gradiente.")
        }
    })

    # --- LÓGICA PARA LA PESTAÑA 3: ANÁLISIS INTERACTIVO DE UN SPLIT-PLOT ---

    # Cargar y preparar los datos una sola vez
    datos_sp_analisis <- reactive({
        req(MASS::oats, dplyr::mutate, dplyr::rename)
        MASS::oats %>%
            mutate(
                bloque = B,
                nitrogeno = N, # Factor Principal
                variedad = V   # Factor Secundario
            ) %>%
            rename(rendimiento = Y)
    })
    
    # Ajustar el modelo Split-Plot una sola vez
    modelo_sp_analisis <- reactive({
        req(datos_sp_analisis())
        # Sintaxis clave para Split-Plot en DBCA
        aov(rendimiento ~ bloque + nitrogeno + Error(bloque/nitrogeno) + variedad + nitrogeno:variedad,
            data = datos_sp_analisis())
    })
    
    # --- Salidas individuales para cada paso ---

    # PASO 1: Datos
    output$sp_tabla_datos <- DT::renderDataTable({
        DT::datatable(datos_sp_analisis(), options=list(pageLength=5, searching=F), rownames=F)
    })
    output$sp_plot_exploratorio <- renderPlot({
        ggplot(datos_sp_analisis(), aes(x = nitrogeno, y = rendimiento, group = variedad, color = variedad)) +
            stat_summary(fun = mean, geom = "line", size = 1) +
            stat_summary(fun = mean, geom = "point", size = 3) +
            facet_wrap(~ bloque) +
            labs(title="Rendimiento Medio por Bloque", y = "Rendimiento", x="Nivel de Nitrógeno") +
            theme_bw()
    })

    # PASO 2: Supuestos
    output$sp_plot_supuestos <- renderPlot({
        req(modelo_sp_analisis())
        
        modelo_lista <- modelo_sp_analisis()
        
        # Extraer los componentes del estrato "Within" (Error b)
        # Este estrato es el que contiene los residuales de nivel de subparcela
        within_stratum <- modelo_lista$`Error: Within`
        
        # A veces, el objeto no tiene nombres. El último elemento es usualmente el modelo de los residuales.
        if(is.null(within_stratum)) {
             # Buscarlo por el índice si el nombre no funciona
             within_stratum <- modelo_lista[[length(modelo_lista)]]
        }
        
        req(within_stratum) # Asegurarse de que lo encontramos
        
        # Crear un data.frame para ggplot
        df_res <- data.frame(
            residuos = residuals(within_stratum),
            ajustados = fitted(within_stratum)
        )
        
        # Eliminar NAs si los hubiera
        df_res <- na.omit(df_res)
        
        if (nrow(df_res) == 0) {
            # Si no hay datos, mostrar un mensaje
            plot(1, type="n", axes=F, xlab="", ylab="")
            text(1, 1, "No se pudieron generar los gráficos de diagnóstico.", col = "red")
            return()
        }
        
        p1 <- ggplot(df_res, aes(x = ajustados, y = residuos)) + 
              geom_point(alpha = 0.6) + 
              geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
              labs(title = "A) Residuales (Error b) vs. Ajustados") + theme_bw()
              
        p2 <- ggplot(df_res, aes(sample = residuos)) + 
              stat_qq() + stat_qq_line(color = "red") +
              labs(title = "B) Normal Q-Q Plot de Residuales (Error b)") + theme_bw()
              
        p1 + p2
    })

    # PASO 3: ANOVA
    output$sp_tabla_anova <- renderPrint({
        summary(modelo_sp_analisis())
    })
    
    # PASO 4: Post-Hoc
    output$sp_posthoc <- renderPrint({
        modelo_sp <- modelo_sp_analisis()
        datos <- datos_sp_analisis()
        req(modelo_sp, datos)
        
        # --- Obtener los errores del modelo Split-Plot correcto ---
        sum_modelo_sp <- summary(modelo_sp)
        
        # Error (a) para el factor principal
        error_a_summary <- sum_modelo_sp$`Error: bloque:nitrogeno`[[1]]
        error_a_df <- error_a_summary['Residuals', 'Df']
        error_a_ms <- error_a_summary['Residuals', 'Mean Sq']
        
        # Error (b) para el factor secundario y la interacción
        error_b_summary <- sum_modelo_sp$`Error: Within`[[1]]
        error_b_df <- error_b_summary['Residuals', 'Df']
        error_b_ms <- error_b_summary['Residuals', 'Mean Sq']

        # --- Crear un modelo ANOVA factorial estándar para que agricolae lo lea ---
        # Este modelo NO se usa para las pruebas F, solo para las medias.
        modelo_anova_completo <- aov(rendimiento ~ bloque + nitrogeno * variedad, data = datos)
        
        # --- Lógica condicional del Post-Hoc ---
        p_val_int <- error_b_summary['nitrogeno:variedad', 'Pr(>F)']
        
        cat("--- ANÁLISIS POST-HOC ---\n\n")
        
        if (p_val_int < 0.05) {
            cat("Interacción Nitrógeno:Variedad es SIGNIFICATIVA (p < 0.05).\n")
            cat("Se procede a analizar los efectos simples usando el Error (b).\n\n")
            
            # Para desglosar la interacción, usamos el modelo completo pero le damos el Error(b)
            out_int <- agricolae::LSD.test(
                y = modelo_anova_completo,
                trt = c("nitrogeno", "variedad"),
                DFerror = error_b_df,
                MSerror = error_b_ms,
                console = TRUE
            )

        } else {
            cat("Interacción Nitrógeno:Variedad NO es significativa (p >= 0.05).\n")
            cat("Se procede a analizar los efectos principales.\n\n")
            
            cat(">> Análisis del Efecto Principal del Nitrógeno (usando Error a):\n")
            out_n <- with(datos, agricolae::LSD.test(
                y = rendimiento, 
                trt = nitrogeno, 
                DFerror = error_a_df, 
                MSerror = error_a_ms,
                group = TRUE,
                console = TRUE
            ))
            
            cat("\n>> Análisis del Efecto Principal de la Variedad (usando Error b):\n")
            out_v <- with(datos, agricolae::LSD.test(
                y = rendimiento,
                trt = variedad,
                DFerror = error_b_df,
                MSerror = error_b_ms,
                group = TRUE,
                console = TRUE
            ))
        }
    })


    # --- UI Dinámico para mostrar los pasos ---
    output$sp_salida_analisis <- renderUI({
        ns <- session$ns
        paso <- input$sp_paso_analisis
        req(paso)
        
        if (paso == "datos") {
            tagList(
                h5("1. Exploración de Datos"),
                p("Visualizamos la estructura y las tendencias generales. El gráfico muestra las medias por bloque, permitiendo ver si hay un efecto de bloque evidente."),
                DT::dataTableOutput(ns("sp_tabla_datos")),
                plotOutput(ns("sp_plot_exploratorio"))
            )
        } else if (paso == "supuestos") {
            tagList(
                h5("2. Verificación de Supuestos"),
                p("Los diagnósticos se realizan sobre los residuales del error más pequeño y numeroso, el Error (b) o error de las sub-parcelas, ya que es el que afecta a más pruebas (Factor Secundario e Interacción)."),
                plotOutput(ns("sp_plot_supuestos"))
            )
        } else if (paso == "anova") {
            tagList(
                h5("3. Interpretación de la Tabla ANOVA"),
                p("La salida se divide en estratos. La sección `Error: bloque:nitrogeno` contiene el análisis del factor principal (Nitrógeno) usando el Error (a). La sección `Error: Within` contiene el análisis del factor secundario (Variedad) y la Interacción, usando el Error (b)."),
                verbatimTextOutput(ns("sp_tabla_anova"))
            )
        } else if (paso == "posthoc") {
            tagList(
                h5("4. Comparaciones Múltiples (Post-Hoc)"),
                p("El análisis post-hoc es guiado por la significancia de la interacción. Se deben usar los errores correctos para cada comparación."),
                verbatimTextOutput(ns("sp_posthoc"))
            )
        }
    })
}