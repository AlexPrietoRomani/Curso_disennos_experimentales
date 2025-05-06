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
                )),
                tags$tbody(
                    tags$tr(
                    tags$td("1 Teoría de probabilidad"),
                    tags$td("0–15 min"),
                    tags$td("Revisar eventos simples y condicionales")
                    ),
                    tags$tr(
                    tags$td("2 Definición y simulación"),
                    tags$td("15–45 min"),
                    tags$td(HTML(
                        "Binomial (infestación de plagas, <code>rbinom()</code>)<br/>",
                        "Poisson (lesiones foliares, <code>rpois()</code>)<br/>",
                        "Normal (variabilidad de rendimiento, <code>rnorm()</code>)"
                    ))
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
        
            # — aquí podrían agregarse más nav_panel() para ejercicios, teoría, etc.
        )
    )
}

session3Server <- function(input, output, session) {
  # Lógica de simulación y gráficos se implementará aquí
}
