# R/modules/session3.R
library(dplyr)
library(ggplot2)
library(moments)
library(patchwork)
library(grid)

session3UI <- function(id) {
    ns <- NS(id)
    tagList(
        div(class = "session-title",
            h3("Sesión 3: Probabilidad y Distribuciones Esenciales")
        ),

        # Usar navset_tab con nav_panel de bslib
        navset_tab(
            # ——————————————
            # PESTAÑA: TEMARIO
            # ——————————————
            nav_panel(title = "Temario",
                h4(class = "section-header", "Temario"),

                # Tabla de actividades
                tags$table(class = "table activity-table",
                    tags$thead(tags$tr(
                        tags$th("Segmento"),
                        tags$th("Tiempo"),
                        tags$th("Actividad")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td("1 Teoría de probabilidad"),
                            tags$td("0–15 min"),
                            tags$td("Revisar eventos simples y condicionales")
                        ),
                        tags$tr(
                            tags$td("2 Definición y simulación"),
                            tags$td("15–45 min"),
                            tags$td(
                                HTML(
                                "Binomial (infestación de plagas, <code>rbinom()</code>)<br/>",
                                "Poisson (lesiones foliares, <code>rpois()</code>)<br/>",
                                "Normal (variabilidad de rendimiento, <code>rnorm()</code>)"
                                )
                            )
                        ),
                        tags$tr(
                            tags$td("3 Graficar distribuciones"),
                            tags$td("45–75 min"),
                            tags$td("Densidades y barras de frecuencia con <code>ggplot2</code>")
                        ),
                        tags$tr(
                            tags$td("4 Discusión de elección"),
                            tags$td("75–95 min"),
                            tags$td("Comparar ajustes de distribuciones con datos reales")
                        ),
                        tags$tr(
                            tags$td("5 Resultado esperado"),
                            tags$td("95–120 min"),
                            tags$td("Scripts de simulación y gráficos interpretados")
                        )
                    )
                )
            ),
            # ——————————————
            # PESTAÑA: 1. TEORÍA
            # ——————————————
            nav_panel(
                title = "1 Teoría de probabilidad",
                h4(class = "section-header", "1.1 Conceptos básicos de probabilidad"),

                # 1. Experimento aleatorio & Espacio muestrasl (Ω)
                tags$h6(tags$b("1.1.1 Experimento aleatorio y Espacio Muestral")),
                tags$div(class = "row",
                    # Columna principal
                    tags$div(class = "col-md-8",
                    tags$p(
                        "Un ", tags$b("experimento aleatorio"), " es un proceso cuyo resultado no se puede predecir con certeza antes de llevarlo a cabo",
                        " (Montgomery & Runger, 2018). Por ejemplo, la germinación de una semilla puede resultar en “germina” o “no germina”."
                    ),
                    tags$p(
                        "El ", tags$b("espacio muestral"), " ",
                        tags$code("Ω"),
                        " es el conjunto de todos los resultados posibles de ese experimento",
                        " (Suza & Lamkey, 2024)."
                    ),
                    tags$ul(
                        tags$li("Ejemplo agrícola: Ω = {germina, no germina}"),
                        tags$li("Cada resultado recibe una probabilidad entre 0 y 1, con P(Ω) = 1.")
                    )
                    ),
                    # Nube lateral
                    tags$div(class = "note-cloud",
                    tags$strong("Nota didáctica:"),
                    "Imagina lanzar un dado justo: Ω = {1,2,3,4,5,6}, cada cara con P = 1/6."  
                    )
                ),


                # 2. Axiomas y Reglas
                tags$h6(tags$b("1.1.2. Axiomas de Kolmogórov y Reglas")),

                tags$div(class = "row mb-3",
                    # Explicación de axiomas
                    tags$div(class = "col-md-8",
                        tags$p(
                        "Andrey Kolmogórov estableció en 1933 los tres axiomas fundamentales que definen la probabilidad moderna:",
                        tags$ul(
                            tags$li(tags$b("1. No negatividad:"), " para todo evento A, ", withMathJax(helpText("$$P(A) \\ge 0$$"))),
                            tags$li(tags$b("2. Normalización:"), " la probabilidad del espacio muestral Ω es 1, ", withMathJax(helpText("$$P(\\Omega)=1$$"))),
                            tags$li(tags$b("3. Aditividad finita:"), " si A y B son disjuntos (mutuamente excluyentes), entonces", 
                                    withMathJax(helpText("$$P(A \\cup B)=P(A)+P(B)$$")))
                        )
                        ),  
                        tags$p("Estos axiomas permiten derivar todas las reglas prácticas de combinación de eventos.")
                    ),
                    # Nota en nube
                    tags$div(class = "note-cloud",
                        tags$strong("Nota agronómica:"),
                        "Si A = 'plaga presente' y B = 'daño en cosecha', y ambos no pueden ocurrir juntos, entonces",
                        "P(A ∪ B) = P(A) + P(B)."
                    )
                ),

                # Diagrama de Venn para unión
                tags$div(class = "venn-diagram",
                    tags$svg(xmlns="http://www.w3.org/2000/svg", width="80", height="60", viewBox="0 0 100 80",
                        # Dos círculos sin solape (disjuntos)
                        tags$circle(cx="24", cy="40", r="25", fill="#90caf9", `fill-opacity`="0.5"),
                        tags$circle(cx="76", cy="40", r="25", fill="#a5d6a7", `fill-opacity`="0.5")
                    ),
                    tags$p(tags$em("Figura: A y B disjuntos → unión simplemente suma P(A) + P(B)."))
                ),

                # Explicación de probabilidad condicional
                tags$div(class = "row mb-3",
                    tags$div(class = "col-md-8",
                        tags$p(
                        tags$b("Probabilidad condicional:"), " mide la probabilidad de A dado que B ya ocurrió,",
                        withMathJax(helpText("$$P(A\\mid B)=\\frac{P(A\\cap B)}{P(B)}$$")), "."
                        ),
                        tags$p("Es fundamental para modelar situaciones donde la ocurrencia de un evento altera la probabilidad de otro.")
                    ),
                    tags$div(class = "note-cloud",
                        tags$strong("Ejemplo práctico:"),
                        "La probabilidad de infección dado riego excesivo puede modelarse como P(infección|riego)."
                    )
                ),

                # Diagrama de Venn para intersección
                tags$div(class = "venn-diagram",
                    tags$svg(xmlns="http://www.w3.org/2000/svg", width="80", height="60", viewBox="0 0 100 80",
                        # Dos círculos superpuestos
                        tags$circle(cx="35", cy="40", r="25", fill="#90caf9", `fill-opacity`="0.5"),
                        tags$circle(cx="65", cy="40", r="25", fill="#a5d6a7", `fill-opacity`="0.5"),
                        # Sombrear la intersección
                        tags$path(d="M45,40 a25,25 0 0,1 20,0 a25,25 0 0,1 -20,0", fill="#ffcc80", `fill-opacity`="0.7")
                    ),
                    tags$p(tags$em("Figura: P(A ∩ B) → área sombreada de solape."))
                ),

                # 3. Eventos
                tags$h6(tags$b("1.1.3 Eventos")),
                tags$p("Un evento es un subconjunto del espacio muestral Ω"),
                tags$p("Ejemplos de eventos:"),
                tags$div(class="row mb-3",
                    tags$div(class="col-md-8",
                        tags$ul(
                            tags$li(
                                tags$b("Evento simple:"), 
                                "un único resultado posible de Ω (un solo punto del espacio muestral), por ejemplo “germina”.",
                                "En probabilidad, un evento simple es aquel que consta de un solo punto del espacio muestral."
                            ),
                            tags$li(
                                tags$b("Mutuamente excluyentes:"), 
                                "dos eventos que no pueden ocurrir simultáneamente; P(A ∩ B) = 0. Un ejemplo clásico es “cara” y “cruz” al lanzar una moneda."
                            ),
                            # Diagrama de Venn para mutuamente excluyentes
                            tags$div(class="venn-diagram",
                                tags$svg(xmlns="http://www.w3.org/2000/svg", width="80", height="60", viewBox="0 0 100 80",
                                    tags$circle(cx="35", cy="40", r="25", fill="#ff8a80", `fill-opacity`="0.5"),
                                    tags$circle(cx="65", cy="40", r="25", fill="#ff8a80", `fill-opacity`="0.5"),
                                    tags$text(x="50", y="45", "∅", `font-size`="16", `text-anchor`="middle", fill="#d32f2f")
                                ),
                                tags$p(tags$em("Figura: Eventos mutuamente excluyentes, sin intersección."))
                            ),
                            tags$li(
                                tags$b("Independientes:"), 
                                "la ocurrencia de un evento no altera la probabilidad del otro, de modo que P(A ∩ B) = P(A)·P(B).",
                                "Por ejemplo, lanzamientos de dos monedas distintas son independientes."
                            ),
                            # Diagrama de Venn para independientes
                            tags$div(class="venn-diagram",
                                tags$svg(xmlns="http://www.w3.org/2000/svg", width="80", height="60", viewBox="0 0 100 80",
                                    tags$circle(cx="35", cy="40", r="25", fill="#80d8ff", `fill-opacity`="0.5"),
                                    tags$circle(cx="65", cy="40", r="25", fill="#80d8ff", `fill-opacity`="0.5")
                                ),
                                tags$p(tags$em("Figura: Eventos independientes; su intersección refleja P(A)·P(B)."))
                            ),
                            tags$div(class="note-cloud",
                                tags$strong("Agronómico:"),
                                "La infestación de plagas en dos parcelas separadas, bajo condiciones controladas y sin propagación cruzada, puede considerarse independiente."
                            )
                        )
                    ),
                ),

                # 4. Variable Aleatoria, Esperanza y Varianza
                tags$h6(tags$b("1.1.4 Variable Aleatoria, Esperanza y Varianza")),

                tags$div(class = "row mb-3",

                    # Columna principal: definiciones y fórmulas
                    tags$div(class = "col-md-8",
                        tags$p(
                        "Una ", tags$b("variable aleatoria"), " ",
                        "es una función que asigna valores numéricos a los resultados de Ω, formalizando fenómenos aleatorios",
                        "."
                        ),
                        tags$p(
                        "Cada variable aleatoria tiene una ",
                        tags$b("distribución de probabilidad"),
                        " asociada (función masa o densidad)."
                        ),
                        tags$ul(
                        tags$li(
                            tags$b("Discreta:"), " toma valores contables (p.ej. número de semillas germinadas)."
                        ),
                        tags$li(
                            tags$b("Continua:"), " puede asumir cualquier valor en un intervalo (p.ej. altura de planta)."
                        )
                        ),

                        tags$h6(tags$b("Esperanza Matemática")),
                        tags$p(
                        "El ", tags$b("valor esperado"), " o esperanza E[X] es el promedio teórico de X si repitiéramos el experimento infinitas veces."
                        ),
                        withMathJax(helpText("$$E[X]=\\sum_x x\\,P(X=x)$$")),
                        tags$p("y, para variable continua:"),
                        withMathJax(helpText("$$E[X]=\\int_{-\\infty}^{\\infty} x\\,f(x)\\,dx$$")),

                        tags$h6(tags$b("Varianza")),
                        tags$p(
                        "La ", tags$b("varianza"), " Var(X) cuantifica la dispersión alrededor de E[X]."
                        ),
                        withMathJax(helpText("$$\\mathrm{Var}(X)=E\\bigl[(X - E[X])^2\\bigr]$$")),
                        tags$p("expresada en forma discreta como:"),
                        withMathJax(helpText("$$\\mathrm{Var}(X)=\\sum_x (x - E[X])^2\\,P(X=x)$$")),
                        tags$p("y en forma continua como:"),
                        withMathJax(helpText("$$\\mathrm{Var}(X)=\\int_{-\\infty}^{\\infty}(x - E[X])^2\\,f(x)\\,dx$$"))
                    ),

                    # Nube lateral con recordatorio
                    tags$div(class = "note-cloud",
                        tags$strong("Recuerda:"),
                        "E[X] equivale a la media poblacional y Var(X) al promedio de cuadrados de las desviaciones."
                    )
                ),

                # Separador de sección
                h4(class = "section-header", "1.2 Ejemplos básicos de probabilidad"),

                # — Ejemplo 1: Distribución Binomial — 
                tags$div(class = "practice-text",
                    tags$h6("Ejemplo práctico 1: Distribución binomial"),

                    # Paso a paso
                    tags$ol(
                    tags$li(
                        tags$b("Definir la variable aleatoria:"), 
                        "Sea ",
                        tags$code("X"),
                        " = número de semillas germinadas en ",
                        tags$code("n = 10"),
                        " ensayos; probabilidad de éxito ",
                        tags$code("p = 0.8"),
                        "."
                    ),
                    tags$li(
                        tags$b("Escribir la función de masa de probabilidad:"),
                        withMathJax(helpText("$$P(X=k) = \\binom{n}{k} p^k (1-p)^{n-k}, \\quad k = 0,1,\\dots,n.$$"))
                    ),
                    tags$li(
                        tags$b("Calcular un caso particular:"), 
                        "Por ejemplo, probabilidad de que germinen todas (",
                        tags$code("k = 10"),
                        "):",
                        withMathJax(helpText("$$P(X=10) = 0.8^{10} \\approx 0.107$$"))
                    ),
                    tags$li(
                        tags$b("Obtener esperanza y varianza teóricas:"),
                        withMathJax(helpText("$$E[X] = n p = 10 \\times 0.8 = 8$$")),
                        withMathJax(helpText("$$\\mathrm{Var}(X) = n p (1-p) = 10 \\times 0.8 \\times 0.2 = 1.6$$"))
                    ),
                    tags$li(
                        tags$b("Implementar en R:"),
                        tags$pre(class = "r-code", 
                            htmltools::HTML(
                                "# P(X = 10)\n",
                                "dbinom(10, size = 10, prob = 0.8)\n",
                                "\n",
                                "# P(X <= 10)\n",
                                "pbinom(10, size = 10, prob = 0.8)\n",
                                "\n",
                                "# Valor esperado y varianza\n",
                                "n <- 10; p <- 0.8\n",
                                "E <- n * p\n",
                                "Var <- n * p * (1 - p)\n",
                                "\n"))
                    ),
                    tags$li(
                        tags$b("Interpretación:"),
                        "En promedio germinan ",
                        tags$code("E = 8"),
                        " semillas, con dispersión ",
                        tags$code("sqrt(Var) ≈ 1.264"),
                        "; la probabilidad de éxito completo es baja (~10.7%)."
                    )
                    )
                ),

                tags$hr(),

                # — Ejemplo 2: Distribución Normal (continua) —
                tags$div(class = "practice-text",
                    tags$h6("Ejemplo práctico 2: Variable aleatoria continua"),

                    tags$ol(
                    tags$li(
                        tags$b("Definir la variable aleatoria:"), 
                        "Sea ",
                        tags$code("Y"),
                        " = altura de planta (cm) tras 4 meses, modelada como ",
                        withMathJax(helpText("$$Y \\sim N(\\mu = 150,\\;\\sigma^2 = 10^2)$$"))
                    ),
                    tags$li(
                        tags$b("Calcular la probabilidad de exceder un umbral:"), 
                        "Queremos ",
                        withMathJax(helpText("$$P(Y > 160) = 1 - \\Phi\\bigl(\\tfrac{160-150}{10}\\bigr)$$")),
                        " donde ",
                        tags$code(withMathJax("\\Phi")),
                        " es la función de distribución normal estándar."
                    ),
                    tags$li(
                        tags$b("Encontrar cuantiles:"), 
                        "Altura por debajo de la cual caen el 95% de plantas:",
                        withMathJax(helpText("$$y_{0.95} = \\Phi^{-1}(0.95; 150,10) \\approx 166.5\\text{ cm}$$"))
                    ),
                    tags$li(
                        tags$b("Implementar en R:"),
                        tags$pre(class = "r-code", 
                            htmltools::HTML(
                                "# P(Y > 160)\n",
                                "1 - pnorm(160, mean = 150, sd = 10)\n",
                                "\n",
                                "# Cuantil 95%\n",
                                "qnorm(0.95, mean = 150, sd = 10)\n",
                                "\n",))
                    ),
                    tags$li(
                        tags$b("Interpretación:"),
                        "Solo ~16% de plantas superan los 160 cm. El 95% de alturas está por debajo de ~166.5 cm."
                    )
                    )
                ),

                # Tabla de conceptos
                tags$br(),
                tags$h5("Tabla: Conceptos Fundamentales de Probabilidad"),
                tags$table(class = "table table-bordered",
                    tags$thead(
                    tags$tr(
                        tags$th("Concepto"),
                        tags$th("Definición"),
                        tags$th("Ejemplo Agronómico"),
                        tags$th("Fórmula")
                    )
                    ),
                    tags$tbody(
                    tags$tr(
                        tags$td("Evento Simple"),
                        tags$td("Un único resultado de Ω"),
                        tags$td("Que una planta esté infestada o no"),
                        tags$td("Resultados favorables / totales")
                    ),
                    tags$tr(
                        tags$td("Probabilidad Condicional"),
                        tags$td("P(A|B) = P(A ∩ B)/P(B)"),
                        tags$td("Enfermedad dada alta humedad"),
                        tags$td(withMathJax(helpText("$$P(A|B)=\\frac{P(A\\cap B)}{P(B)}$$")))
                    ),
                    tags$tr(
                        tags$td("Mutuamente Excluyentes"),
                        tags$td("No pueden ocurrir simultáneamente"),
                        tags$td("Semilla germina o no germina"),
                        tags$td(withMathJax(helpText("$$P(A\\cup B)=P(A)+P(B)$$")))
                    ),
                    tags$tr(
                        tags$td("Independientes"),
                        tags$td("Evento A no afecta a B"),
                        tags$td("Lluvia en dos días distintos"),
                        tags$td(withMathJax(helpText("$$P(A\\cap B)=P(A)P(B)$$")))
                    )
                    )
                )
            ),
            # ——————————————
            # PESTAÑA: 2. Definición y simulación
            # ——————————————
            nav_panel(
                title = "2 Definición y simulación",
                
                # 2.1 Binomial
                tags$h4(
                    class = "section-header",
                    "2.1 Binomial para infestación de plagas (rbinom())"
                ),
                tags$p(
                    "La distribución binomial es un modelo de probabilidad discreto que describe el número de éxitos en un número fijo de ensayos de Bernoulli independientes, donde cada ensayo presenta solo dos resultados posibles: éxito o fracaso.",
                    tags$ul(
                        tags$li("Número fijo de ensayos \\(n\\), cada uno con dos posibles resultados."),
                        tags$li("Ensayos independientes entre sí."),
                        tags$li("Probabilidad de éxito \\(p\\) constante en cada ensayo."),
                        tags$li("Función de masa: media \\(E[X] = np\\) y varianza \\(Var(X) = np(1-p)\\)."),
                        tags$li("Aplicaciones: conteo de plantas infestadas, defectos en semillas, etc.")
                    ),
                    "La función de masa de probabilidad viene dada por:",
                    withMathJax(helpText(
                    "$$P(X = k) = \\binom{n}{k} p^k (1 - p)^{n - k}, \\quad k = 0,1,\\dots,n$$"
                    )),
                    "En el contexto de la infestación de plagas, un éxito se define como una planta infestada. Los parámetros son \\(n\\) (número de plantas muestreadas) y \\(p\\) (probabilidad de infestación por planta).",
                    tags$small("(Montgomery & Runger, 2018; Dalgaard, 2008)")
                ),
                tags$pre(class = "r-code",
                    htmltools::HTML(
                        "# Simulación binomial: infestación de plagas\n",
                        "set.seed(42)                    # Reproducibilidad\n",
                        "n       <- 50                  # número de plantas muestreadas\n",
                        "p       <- 0.2                 # probabilidad de infestación por planta\n",
                        "R       <- 1000               # réplicas de simulación\n",
                        "X       <- rbinom(R, size = n, prob = p)\n",
                        "# Estadísticos de la muestra simulada:\n",
                        "mean(X)    # Aprox. E[X] = np    # 50 × 0.2 = 10\n",
                        "var(X)     # Aprox. Var(X) = np(1-p)\n",
                        "summary(X)  # Cinco números resumo\n",
                        "# Probabilidades teóricas (ej.): P(X ≤ 5)\n",
                        "pbinom(5, size = n, prob = p)\n"
                    )
                ),
                plotOutput(ns("histogramaBinomial"), height = "400px"),
                tags$table(class = "table-bordered",
                    tags$thead(
                    tags$tr(
                        tags$th("Parámetro"),
                        tags$th("Símbolo"),
                        tags$th("Descripción en Infestación de Plagas"),
                        tags$th("Función en R")
                    )
                    ),
                    tags$tbody(
                    tags$tr(
                        tags$td("Número de Ensayos"),
                        tags$td("n"),
                        tags$td("Número de plantas muestreadas"),
                        tags$td("size de rbinom()")
                    ),
                    tags$tr(
                        tags$td("Probabilidad de Éxito"),
                        tags$td("p"),
                        tags$td("Probabilidad de infestación por planta"),
                        tags$td("prob de rbinom()")
                    ),
                    tags$tr(
                        tags$td("Media"),
                        tags$td("E[X]"),
                        tags$td("Número esperado de plantas infestadas: np"),
                        tags$td("mean() de vectores simulados")
                    ),
                    tags$tr(
                        tags$td("Varianza"),
                        tags$td("Var(X)"),
                        tags$td("Dispersión del número de infestaciones: np(1-p)"),
                        tags$td("var() de vectores simulados")
                    )
                    )
                ),
                
                # 2.2 Poisson
                tags$h4(
                    class = "section-header",
                    "2.2 Poisson para conteo de lesiones foliares (rpois())"
                ),
                tags$p(
                    "La distribución de Poisson es un modelo de probabilidad discreto que describe el número de eventos raros que ocurren de manera independiente en un intervalo fijo de tiempo o espacio. Se basa en los siguientes supuestos:",
                    tags$ul(
                    tags$li("Intervalo de observación fijo de tiempo o espacio."),
                    tags$li("Eventos independientes entre sí."),
                    tags$li("Tasa media de ocurrencia \\(\\lambda\\) constante."),
                    tags$li("Probabilidad de dos o más eventos simultáneos prácticamente cero."),
                    tags$li("Propiedad de dispersión equidispersa:  E[X] = Var(X) = \\(\\lambda\\).")
                    ),
                    "Formalmente, la función de masa de probabilidad es:",
                    withMathJax(helpText(
                    "$$P(X = k) = \\frac{\\lambda^k e^{-\\lambda}}{k!}, \\quad k = 0,1,2,\\dots$$"
                    )),
                    "En agronomía, para el conteo de lesiones foliares en hojas, \\(\\lambda\\) representa la tasa media de lesiones por hoja en un área muestreada. Cuando el número de ensayos \\(n\\) es grande y la probabilidad de evento \\(p\\) pequeña, la distribución binomial converge a Poisson con \\(\\lambda = n p\\).",
                    tags$small("(Dalgaard, 2008; Montgomery & Runger, 2018)")
                ),
                tags$pre(class = "r-code",
                    htmltools::HTML(
                    "# Simulación Poisson: lesiones foliares\n",
                    "num_hojas               <- 20      # hojas muestreadas por réplica\n",
                    "tasa_promedio_lesiones  <- 3       # lesiones promedio por hoja (\u03BB)\n",
                    "num_simulaciones       <- 1000    # réplicas totales\n",
                    "lesiones_simuladas     <- rpois(n = num_simulaciones * num_hojas, lambda = tasa_promedio_lesiones)\n",
                    "# Visualización: histograma de lesiones simuladas\n",
                    "hist(lesiones_simuladas, main = 'Distribución Simulada de Lesiones Foliares', xlab = 'Número de Lesiones por Hoja')  # Dalgaard (2008)\n"
                    )
                ),
                plotOutput(ns("histogramaPoisson"), height = "400px"),
                tags$table(class = "table-bordered",
                    tags$thead(
                    tags$tr(
                        tags$th("Parámetro"),
                        tags$th("Símbolo"),
                        tags$th("Contexto de Lesiones Foliares"),
                        tags$th("Función en R")
                    )
                    ),
                    tags$tbody(
                    tags$tr(
                        tags$td("Tasa de Ocurrencia"),
                        tags$td("\u03BB"),
                        tags$td("Lesiones promedio por hoja"),
                        tags$td("lambda de rpois()")
                    ),
                    tags$tr(
                        tags$td("Repeticiones"),
                        tags$td("n"),
                        tags$td("Número de hojas * simulaciones"),
                        tags$td("n de rpois()")
                    )
                    )
                ),
                
                # 2.3 Normal (sin cambios)
                tags$h4(
                    class = "section-header",
                    "2.3 Normal para variabilidad de rendimiento (rnorm())"
                ),
                tags$p(
                    "La distribución normal, o gaussiana, es continua y se define por su media \\(\\mu\\) y desviación estándar \\(\\sigma\\).",
                    withMathJax(helpText(
                    "$$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\tfrac{(x-\\mu)^2}{2\\sigma^2}}$$"
                    )),
                    "Esta distribución es fundamental en agronomía para modelar rendimientos de cultivo, debido a la agregación de múltiples factores aleatorios que siguen el Teorema Central del Límite.",
                    tags$ul(
                    tags$li("Es simétrica alrededor de la media, con asimetría cero."),
                    tags$li("Curtosis igual a 3 (colas moderadas)."),
                    tags$li("Regla empírica: ~68% dentro de 1\\(\\sigma\\), 95% dentro de 2\\(\\sigma\\), 99.7% dentro de 3\\(\\sigma\\).")
                    ),
                    "Antes de aplicar, verificar normalidad mediante Q-Q plot o pruebas (Shapiro-Wilk, Kolmogorov-Smirnov).",
                    tags$small("(Dalgaard, 2008; Montgomery & Runger, 2018)")
                ),
                tags$pre(class = "r-code",
                    htmltools::HTML(
                    "# Simulación Normal: variabilidad de rendimiento\n",
                    "num_rendimientos   <- 100   # cantidad de valores simulados\n",
                    "media_rendimiento  <- 70    # rendimiento promedio (bushel/acre)\n",
                    "sd_rendimiento     <- 10    # variabilidad alrededor de la media\n",
                    "rendimientos_sim   <- rnorm(n = num_rendimientos, mean = media_rendimiento, sd = sd_rendimiento)\n",
                    "# Visualización: histograma y Q-Q plot\n",
                    "hist(rendimientos_sim, main = 'Rendimientos Simulados (Normal)', xlab = 'Bushels/acre')\n",
                    "qqnorm(rendimientos_sim); qqline(rendimientos_sim, col = 'red')  # Dalgaard (2008)\n"
                    )
                ),
                plotOutput(ns("histogramaNormal"), height = "400px"),
                tags$table(class = "table-bordered",
                    tags$thead(
                    tags$tr(
                        tags$th("Parámetro"),
                        tags$th("Símbolo"),
                        tags$th("Contexto de Rendimiento de Cultivo"),
                        tags$th("Función en R")
                    )
                    ),
                    tags$tbody(
                    tags$tr(
                        tags$td("Media"),
                        tags$td("\u03BC"),
                        tags$td("Rendimiento promedio esperado"),
                        tags$td("mean de rnorm()")
                    ),
                    tags$tr(
                        tags$td("Desviación Estándar"),
                        tags$td("\u03C3"),
                        tags$td("Variabilidad de rendimientos"),
                        tags$td("sd de rnorm()")
                    ),
                    tags$tr(
                        tags$td("Observaciones"),
                        tags$td("n"),
                        tags$td("Número de valores simulados"),
                        tags$td("n de rnorm()")
                    )
                    )
                ),
                tags$br(),
                tags$p(
                    "La distribución normal es fundamental en estadística y agronomía, ya que muchos fenómenos naturales tienden a seguirla. Su uso es esencial para la inferencia estadística y el análisis de datos."
                ),
                # 2.4 Comparativa de distribuciones
                tags$h4(class = "section-header", "2.4 Tabla comparativa de distribuciones"),
                tags$table(class = "table-bordered",
                    tags$thead(
                    tags$tr(
                        tags$th("Distribución"),
                        tags$th("Teoría"),
                        tags$th("Ejemplo Agronómico"),
                        tags$th("Beneficio"),
                        tags$th("Desventaja")
                    )
                    ),
                    tags$tbody(
                    tags$tr(
                        tags$td("Binomial"),
                        tags$td("Número de éxitos en ensayos Bernoulli."),
                        tags$td("Plantas infestadas en muestra fija."),
                        tags$td("Fácil interpretación; momentos simples."),
                        tags$td("Requiere ensayos independientes; p constante.")
                    ),
                    tags$tr(
                        tags$td("Poisson"),
                        tags$td("Conteo de eventos raros en intervalo."),
                        tags$td("Lesiones foliares por hoja."),
                        tags$td("Convergencia desde binomial; manejo de rareza."),
                        tags$td("Supuestos independientes; equidispersión.")
                    ),
                    tags$tr(
                        tags$td("Normal"),
                        tags$td("Distribución continua en campana."),
                        tags$td("Variabilidad de rendimiento."),
                        tags$td("Aplicable por TCL; descontamina outliers."),
                        tags$td("No maneja bien datos sesgados/extremos.")
                    )
                    )
                ),
                tags$br(),
                tags$p(
                    "La elección de la distribución adecuada es crucial para un análisis estadístico efectivo. La comprensión de sus supuestos y limitaciones permite una mejor interpretación de los resultados."
                ),
                tags$h4(class = "section-header", "2.5 Gráfico comparativo"),
                plotOutput(ns("distComparison"), height = "400px")
            ),
            # ——————————————
            # PESTAÑA 3: Graficar distribuciones
            # ——————————————
            
            nav_panel(
                title = "3 Graficar distribuciones",
                h4(class = "section-header", "3.1 Visualización de distribuciones con ggplot2"),

                tags$h6(tags$b("3.1.1 Histogramas y curvas de densidad")),
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col-md-8",
                        tags$p(
                            "Los histogramas permiten visualizar la distribución de una variable continua, mostrando la frecuencia de observaciones en intervalos específicos. ",
                            "Superponer una curva de densidad sobre el histograma proporciona una estimación suave de la distribución subyacente."
                        ),
                        tags$p(
                            "En ggplot2, esto se logra utilizando ",
                            tags$code("geom_histogram()"),
                            " y ",
                            tags$code("geom_density()"),
                            "."
                        ),
                        tags$pre(
                            class = "r-code",
                            htmltools::HTML(
                            "# Simulación de datos\n",
                            "set.seed(123)\n",
                            "datos <- data.frame(valor = rnorm(1000, mean = 0, sd = 1))\n",
                            "\n",
                            "# Gráfico con histograma y curva de densidad\n",
                            "ggplot(datos, aes(x = valor)) +\n",
                            "  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = 'skyblue', color = 'black') +\n",
                            "  geom_density(color = 'red', size = 1) +\n",
                            "  labs(title = 'Histograma con curva de densidad', x = 'Valor', y = 'Densidad')"
                            )
                        ),
                        tags$p(
                            "En este ejemplo, se simulan 1000 observaciones de una distribución normal estándar. ",
                            "El histograma muestra la frecuencia relativa de los datos, mientras que la curva roja representa la estimación de densidad."
                        )
                    ),
                    tags$div(
                        class = "note-cloud",
                        tags$strong("Nota didáctica:"),
                        "Ajustar el parámetro ",
                        tags$code("binwidth"),
                        " en ",
                        tags$code("geom_histogram()"),
                        " permite controlar el número de barras y, por ende, la granularidad del histograma."
                    ),
                    plotlyOutput(ns("histogramaDensidad"), height = "400px"),
                ),

                tags$h6(tags$b("3.1.2 Gráficos de barras de frecuencia")),
                tags$div(
                    class = "row",
                    tags$div(
                        class = "col-md-8",
                        tags$p(
                            "Para variables categóricas, los gráficos de barras son útiles para mostrar la frecuencia de cada categoría. ",
                            "En ggplot2, se utiliza ",
                            tags$code("geom_bar()"),
                            " para este propósito."
                        ),
                        tags$pre(
                            class = "r-code",
                            htmltools::HTML(
                            "# Simulación de datos categóricos\n",
                            "set.seed(456)\n",
                            "categorias <- data.frame(grupo = sample(c('A', 'B', 'C'), size = 500, replace = TRUE))\n",
                            "\n",
                            "# Gráfico de barras de frecuencia\n",
                            "ggplot(categorias, aes(x = grupo)) +\n",
                            "  geom_bar(fill = 'lightgreen', color = 'black') +\n",
                            "  labs(title = 'Frecuencia por grupo', x = 'Grupo', y = 'Frecuencia')"
                            )
                        ),
                        tags$p(
                            "Este gráfico muestra la cantidad de observaciones en cada grupo categórico (A, B y C)."
                        )
                    ),
                    tags$div(
                        class = "note-cloud",
                        tags$strong("Nota didáctica:"),
                        "Para ordenar las barras de mayor a menor frecuencia, se puede utilizar la función ",
                        tags$code("reorder()"),
                        " en el mapeo estético de ",
                        tags$code("aes()"),
                        "."
                    ),
                    plotlyOutput(ns("graficoBarras"), height = "400px"),
                ),

                tags$h6(tags$b("3.1.3 Comparación de distribuciones por grupo")),
                tags$div(class = "row",
                    tags$div(
                        class = "col-md-8",
                        tags$p(
                            "Cuando se desea comparar la distribución de una variable continua entre diferentes grupos, se pueden utilizar histogramas o curvas de densidad diferenciadas por color."
                        ),
                        tags$pre(class = "r-code",
                            htmltools::HTML(
                            "# Simulación de datos con grupos\n",
                            "set.seed(789)\n",
                            "datos_grupo <- data.frame(\n",
                            "  valor = c(rnorm(500, mean = 0), rnorm(500, mean = 3)),\n",
                            "  grupo = rep(c('Grupo 1', 'Grupo 2'), each = 500)\n",
                            ")\n",
                            "\n",
                            "# Histograma con grupos\n",
                            "ggplot(datos_grupo, aes(x = valor, fill = grupo)) +\n",
                            "  geom_histogram(position = 'identity', alpha = 0.6, binwidth = 0.5) +\n",
                            "  labs(title = 'Comparación de distribuciones por grupo', x = 'Valor', y = 'Frecuencia')"
                            )
                        ),
                        tags$p(
                            "En este ejemplo, se comparan dos grupos con medias diferentes. ",
                            "El uso de transparencia (",
                            tags$code("alpha"),
                            ") permite visualizar la superposición de las distribuciones."
                        )
                    ),
                    tags$div(
                        class = "note-cloud",
                        tags$strong("Nota didáctica:"),
                        "Para una comparación más clara, también se pueden utilizar gráficos de densidad con ",
                        tags$code("geom_density()"),
                        ", diferenciando los grupos por color."
                    ),
                    plotlyOutput(ns("comparacionDistribuciones"), height = "400px"),
                )
            ),

            # ——————————————
            # PESTAÑA 4: Discusión de elección
            # ——————————————

            nav_panel(
                title = "4 Discusión de elección",
                h4(class = "section-header", "4.1 Comparación de ajustes de distribuciones con datos reales"),

                tags$h6(tags$b("4.1.1 Ajuste de distribuciones a datos reales")),
                tags$div(class = "row",
                    tags$div(
                        class = "col-md-8",
                        tags$p(
                            "En esta sección, se ilustra cómo ajustar diferentes distribuciones teóricas a un conjunto de datos reales y comparar su adecuación utilizando gráficos y medidas estadísticas."
                        ),
                        tags$p(
                            "Se simulan datos que representan tiempos de espera, los cuales suelen seguir distribuciones como la exponencial o la gamma."
                        ),
                        tags$pre(class = "r-code",
                            htmltools::HTML(
                            "# Simulación de datos de tiempos de espera\n",
                            "set.seed(123)\n",
                            "datos_espera <- rexp(1000, rate = 1/5)  # media de 5 minutos\n",
                            "\n",
                            "# Ajuste de distribuciones\n",
                            "ajuste_normal <- fitdistr(datos_espera, 'normal')\n",
                            "ajuste_exponencial <- fitdistr(datos_espera, 'exponential')\n",
                            "ajuste_gamma <- fitdistr(datos_espera, 'gamma')"
                            )
                        ),
                        tags$p(
                            "Se ajustan tres distribuciones: normal, exponencial y gamma. A continuación, se comparan visualmente mediante histogramas y curvas de densidad."
                        )
                    ),
                    tags$div(
                        class = "note-cloud",
                        tags$strong("Nota didáctica:"),
                        "El ajuste de distribuciones permite identificar el modelo probabilístico que mejor representa los datos observados, lo cual es fundamental para realizar inferencias estadísticas adecuadas."
                    ),
                ),

                tags$h6(tags$b("4.1.2 Evaluación de la bondad de ajuste")),
                tags$div(
                    class = "row",
                    tags$div(class = "col-md-8",
                    tags$p(
                        "Para evaluar la calidad del ajuste de cada distribución, se utilizan pruebas estadísticas como la de Kolmogorov-Smirnov y gráficos Q-Q."
                    ),
                    tags$pre(class = "r-code",
                        htmltools::HTML(
                        "# Pruebas de Kolmogorov-Smirnov\n",
                        "ks.test(datos_espera, 'pnorm', mean = ajuste_normal$estimate['mean'], sd = ajuste_normal$estimate['sd'])\n",
                        "ks.test(datos_espera, 'pexp', rate = ajuste_exponencial$estimate['rate'])\n",
                        "ks.test(datos_espera, 'pgamma', shape = ajuste_gamma$estimate['shape'], rate = ajuste_gamma$estimate['rate'])"
                        )
                    ),
                    tags$p(
                        "Los valores p obtenidos en las pruebas de Kolmogorov-Smirnov indican la plausibilidad de que los datos provengan de cada distribución teórica."
                    )
                    ),
                    tags$div(class = "note-cloud",
                    tags$strong("Nota didáctica:"),
                    "Un valor p alto en la prueba de Kolmogorov-Smirnov sugiere que no hay evidencia suficiente para rechazar la hipótesis de que los datos siguen la distribución teórica considerada."
                    )
                ),

                tags$h6(
                    
                    tags$b("4.1.3 Selección de distribución y visualización"),

                    tabPanel(
                        title = "4. Discusión de elección",
                        sidebarLayout(
                            sidebarPanel(
                            selectInput(ns("dist_seleccionada"), "Seleccione la distribución:",
                                        choices = c("Normal", "Exponencial", "Gamma")),
                            selectInput(ns("tipo_grafico"), "Seleccione el tipo de gráfico:",
                                        choices = c("Histograma con curva de densidad", "Gráfico Q-Q"))
                            ),
                            mainPanel(
                            plotlyOutput(ns("grafico_ajuste"), height = "500px")
                            )
                        )
                    )                   
                ),
            ),

            # ——————————————
            # PESTAÑA: 5. Resultado esperado
            # ——————————————

            

            # ——————————————
            # PESTAÑA: Referencias 
            # ——————————————
            nav_panel(
                title = "Referencias",
                tags$ul(
                tags$li("Dalgaard, P. (2008). Introductory Statistics with R. Springer."),
                tags$li("Investopedia. (2005). Descriptive Statistics: Definition, Overview, Types, and Examples. Retrieved from https://www.investopedia.com/terms/d/descriptive_statistics.asp"),
                tags$li("Montgomery, D. C., & Runger, G. C. (2018). Applied Statistics and Probability for Engineers (7th ed.). Wiley."),
                tags$li("Suza, W. P., & Lamkey, K. R. (2024). Quantitative Methods for Plant Breeding. LibreTexts. Retrieved from https://bio.libretexts.org/Bookshelves/Agriculture_and_Horticulture/Quantitative_Methods_for_Plant_Breeding"),
                tags$li("BYJU’S. (n.d.). Types of Events in Probability. Retrieved from https://byjus.com/maths/types-of-events-in-probability/"),
                tags$li("Cuemath. (n.d.). Events in Probability: Definition, Types, Examples. Retrieved from https://www.cuemath.com/data/events-in-probability/"),
                tags$li("Homework.Study.com. (n.d.). Explain the concepts of an experiment, outcome, sample space and event. Retrieved from https://homework.study.com/explanation/explain-the-concepts-of-an-experiment-outcome-sample-space-and-event.html"),
                tags$li("Eagri. (n.d.). STAM101 :: Lecture 06 :: Probability – Basic concepts. Retrieved from https://eagri.org/eagri50/STAM101/lec06.html"),
                tags$li("Statistics Teacher. (2022). Experiments of Two Identical Coin Tosses. Retrieved from https://www.statisticsteacher.org/2022/10/24/cointosses/"),
                tags$li("Investopedia. (2008). Random Variable. Retrieved from https://www.investopedia.com/terms/r/random-variable.asp"),
                tags$li("Kolmogórov, A. (1933). \"Foundations of the Theory of Probability\". Chelsea Publishing."),
                tags$li("Montgomery, D. C., & Runger, G. C. (2018). _Applied Statistics and Probability for Engineers_ (7th ed.). Wiley."),
                tags$li("Agro UBA. (n.d.). _Probabilidad Condicional_. Retrieved from https://www.agro.uba.ar/users/batista/EG/pcind.pdf"),
                tags$li("LibreTexts. (n.d.). Intersection and Union of Events and Venn Diagrams. Retrieved from https://stats.libretexts.org/Courses/City_University_of_New_York/Introductory_Statistics_with_Probability_%28CUNY%29/04%3A_Probability_Theory/4.05%3A_Intersection_and_Union_of_Events_and_Venn_Diagrams"),
                tags$li("Dalgaard, P. (2008). _Introductory Statistics with R_. Springer."),
                tags$li("Investopedia. (2005). Descriptive Statistics: Definition, Overview, Types, and Examples. Retrieved from https://www.investopedia.com/terms/d/descriptive_statistics.asp"),
                tags$li("Suza, W. P., & Lamkey, K. R. (2024). _Quantitative Methods for Plant Breeding_. LibreTexts."),
                tags$li("Posit. (2024). _downloadHandler – Shiny_. Retrieved from https://shiny.posit.co/r/reference/shiny/1.7.2/downloadhandler.html"),
                tags$li("Tidyverse Blog. (2023). \"Base vs magrittr pipe\". Retrieved from https://www.tidyverse.org/blog/2023/04/base-vs-magrittr-pipe/"),
                tags$li("W3Schools. (n.d.). _R Operators_. Retrieved from https://www.w3schools.com/r/r_operators.asp"),
                tags$li("BYJU’S. (2020). What are Simple and Compound Events in Probability [Artículo web]. Recuperado de https://byjus.com/jee/what-are-simple-and-compound-events-in-probability/"),
                tags$li("Math is Fun. (s. f.). Mutually Exclusive Events. Recuperado de https://www.mathsisfun.com/data/probability-events-mutually-exclusive.html"),
                tags$li("BYJU’S. (2016). Independent Events And Probability. Recuperado de https://byjus.com/maths/independent-events/"),
                tags$li(""),
                )
            )
        )
    )
}

session3Server <- function(input, output, session) {
    #----- Pestana 2: Simulación -----
    # Histograma Binomial
    output$histogramaBinomial <- renderPlot({
        num_plantas_muestra <- 50
        prob_infestacion   <- 0.2
        num_simulaciones   <- 1000
        infestaciones_simuladas <- rbinom(n = num_simulaciones, size = num_plantas_muestra, prob = prob_infestacion)
        hist(infestaciones_simuladas,
            main = "Distribución Simulada de Infestación de Plagas",
            xlab = "Número de Plantas Infestadas",
            border = "white",
            col = "steelblue",
            breaks = seq(0, num_plantas_muestra, by = 1))
    })

    # Histograma Poisson
    output$histogramaPoisson <- renderPlot({
        num_hojas               <- 20
        tasa_promedio_lesiones  <- 3
        num_simulaciones        <- 1000
        lesiones_simuladas      <- rpois(n = num_simulaciones * num_hojas, lambda = tasa_promedio_lesiones)
        hist(lesiones_simuladas,
            main = "Distribución Simulada de Lesiones Foliares",
            xlab = "Número de Lesiones por Hoja",
            border = "white",
            col = "steelblue",
            breaks = 12)
    })

    # Histograma Normal
    output$histogramaNormal <- renderPlot({
        num_rendimientos  <- 100
        media_rendimiento <- 70
        sd_rendimiento    <- 10
        rendimientos_sim  <- rnorm(n = num_rendimientos, mean = media_rendimiento, sd = sd_rendimiento)
        hist(rendimientos_sim,
            main = "Rendimientos Simulados (Normal)",
            xlab = "Bushels/acre",
            border = "white",
            col = "steelblue",
            breaks = 12)
    })

    # Comparativa de distribuciones
    output$distComparison <- renderPlot({
        set.seed(123)
        b <- rbinom(1000, size = 50, prob = 0.2)/50
        p <- rpois(1000, lambda = 3)/10
        n <- rnorm(1000, mean = 70, sd = 10)/100
        plot(density(b), main = "Comparación de Distribuciones", xlim = c(0,1), ylim = c(0,5))
        lines(density(p), col = "darkgreen")
        lines(density(n), col = "blue")
        legend("topright", legend = c("Binomial","Poisson","Normal"), 
            col = c("black","darkgreen","blue"), lwd = 2)
    })

    #----- Pestana 3: Graficar distribuciones -----
    # 3.1.1 Histograma con curva de densidad interactivo
    output$histogramaDensidad <- renderPlotly({
        set.seed(123)
        datos <- rnorm(1000, mean = 0, sd = 1)
        densidad <- density(datos)
        counts <- hist(datos, plot = FALSE, breaks = 30)
        
        fig <- plot_ly()
        # Histograma (frecuencia absoluta, eje Y izquierdo)
        fig <- fig %>% add_bars(
            x = counts$mids,
            y = counts$counts,
            name = "Frecuencia",
            marker = list(color = 'skyblue'),
            opacity = 0.6,
            yaxis = "y1"
        )
        # Densidad (eje Y derecho)
        fig <- fig %>% add_lines(
            x = densidad$x,
            y = densidad$y,
            name = "Densidad",
            line = list(color = 'red', width = 3),
            yaxis = "y2"
        )
        fig <- fig %>% layout(
            title = "Histograma (frecuencia) y Curva de Densidad",
            xaxis = list(title = "Valor"),
            yaxis = list(title = "Frecuencia", side = "left"),
            yaxis2 = list(title = "Densidad", overlaying = "y", side = "right", showgrid = FALSE),
            legend = list(x = 0.8, y = 0.95)
        )
        fig
    })
    
    # 3.1.2 Gráfico de barras de frecuencia interactivo
    output$graficoBarras <- renderPlotly({
        set.seed(456)
        categorias <- sample(c("A", "B", "C"), size = 500, replace = TRUE)
        df_categorias <- as.data.frame(table(categorias))
        
        fig <- plot_ly(data = df_categorias,
                    x = ~categorias,
                    y = ~Freq,
                    type = 'bar',
                    marker = list(color = 'lightgreen'))
        fig <- fig %>% layout(title = "Frecuencia por grupo",
                            xaxis = list(title = "Grupo"),
                            yaxis = list(title = "Frecuencia"))
        fig
    })
    
    # 3.1.3 Comparación de distribuciones por grupo interactivo
    output$comparacionDistribuciones <- renderPlotly({
        set.seed(789)
        grupo1 <- rnorm(500, mean = 0)
        grupo2 <- rnorm(500, mean = 3)
        
        fig <- plot_ly()
        fig <- fig %>% add_histogram(x = grupo1,
                                    name = "Grupo 1",
                                    opacity = 0.6,
                                    marker = list(color = 'blue'))
        fig <- fig %>% add_histogram(x = grupo2,
                                    name = "Grupo 2",
                                    opacity = 0.6,
                                    marker = list(color = 'orange'))
        fig <- fig %>% layout(title = "Comparación de distribuciones por grupo",
                            xaxis = list(title = "Valor"),
                            yaxis = list(title = "Frecuencia"),
                            barmode = "overlay")
        fig
    })

    #----- Pestana 4: Discusión de elección -----
    # Cargar los datos de ejemplo
    datos_espera <- faithful$waiting

    # Función para ajustar la distribución seleccionada
    ajustar_distribucion <- function(distribucion, datos) {
        switch(distribucion,
            "Normal" = fitdistr(datos, "normal"),
            "Exponencial" = fitdistr(datos, "exponential"),
            "Gamma" = fitdistr(datos, "gamma"))
    }

    # Renderizar el gráfico interactivo según las selecciones del usuario
    output$grafico_ajuste <- renderPlotly({
        req(input$dist_seleccionada, input$tipo_grafico)

        ajuste <- ajustar_distribucion(input$dist_seleccionada, datos_espera)
        x_vals <- seq(min(datos_espera), max(datos_espera), length.out = 1000)

        if (input$tipo_grafico == "Histograma con curva de densidad") {
            densidad_teorica <- switch(input$dist_seleccionada,
            "Normal" = dnorm(x_vals, mean = ajuste$estimate["mean"], sd = ajuste$estimate["sd"]),
            "Exponencial" = dexp(x_vals, rate = ajuste$estimate["rate"]),
            "Gamma" = dgamma(x_vals, shape = ajuste$estimate["shape"], rate = ajuste$estimate["rate"])
            )

            fig <- plot_ly()
            fig <- fig %>% add_histogram(
            x = datos_espera,
            name = "Datos observados",
            marker = list(color = 'lightgray'),
            opacity = 0.6,
            nbinsx = 30,
            yaxis = "y1"
            )
            fig <- fig %>% add_lines(
            x = x_vals,
            y = densidad_teorica,
            name = paste("Ajuste", input$dist_seleccionada),
            line = list(color = 'red'),
            yaxis = "y2"
            )
            fig <- fig %>% layout(
            title = paste("Histograma con ajuste", input$dist_seleccionada),
            xaxis = list(title = "Tiempo de espera"),
            yaxis = list(title = "Frecuencia", side = "left"),
            yaxis2 = list(title = "Densidad", overlaying = "y", side = "right")
            )
            fig
        } else if (input$tipo_grafico == "Gráfico Q-Q") {
            n <- length(datos_espera)
            observados <- sort(datos_espera)

            # Calcular los cuantiles teóricos según la distribución seleccionada
            teoricos <- switch(input$dist_seleccionada,
            "Normal" = qnorm(ppoints(n), mean = ajuste$estimate["mean"], sd = ajuste$estimate["sd"]),
            "Exponencial" = qexp(ppoints(n), rate = ajuste$estimate["rate"]),
            "Gamma" = qgamma(ppoints(n), shape = ajuste$estimate["shape"], rate = ajuste$estimate["rate"])
            )

            # Crear el gráfico Q-Q
            fig <- plot_ly()
            fig <- fig %>% add_markers(
            x = teoricos,
            y = observados,
            name = "Datos",
            marker = list(color = 'blue')
            )
            fig <- fig %>% add_lines(
            x = teoricos,
            y = teoricos,
            name = "Línea teórica",
            line = list(color = 'red', dash = 'dash')
            )
            fig <- fig %>% layout(
            title = paste("Gráfico Q-Q: ajuste", input$dist_seleccionada),
            xaxis = list(title = "Cuantiles teóricos"),
            yaxis = list(title = "Cuantiles de los datos")
            )
            fig
        }
    })
}
