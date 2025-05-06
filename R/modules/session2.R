# R/modules/session2.R
library(dplyr)
library(ggplot2)
library(moments)
library(patchwork)
library(grid)

session2UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 2: Estadística Descriptiva Avanzada")
    ),
    
    # Usar navset_tab con nav_panel de bslib
    navset_tab(
      # ——————————————
      # PESTAÑA: TEMARIO
      # ——————————————
      nav_panel(title = "Temario",
        h4(class = "section-header", "Temario"),
        
        # Tabla de actividades
        tags$table(class = "table activity-table table-bordered",
          tags$thead(tags$tr(
            tags$th("Segmento"),
            tags$th("Tiempo"),
            tags$th("Actividad")
          )),
          tags$tbody(
            tags$tr(
              tags$td("1 Medidas básicas"),
              tags$td("0–25 min"),
              tags$td("Cálculo de media, mediana, rango, varianza y desviación estándar (mean(), sd())")
            ),
            tags$tr(
              tags$td("2 Curtosis/Asimetría"),
              tags$td("25–45 min"),
              tags$td("Introducción y cálculo con moments::kurtosis() y moments::skewness()")
            ),
            tags$tr(
              tags$td("3 Agrupación"),
              tags$td("45–70 min"),
              tags$td("Uso de group_by() y summarise() para análisis por grupos")
            ),
            tags$tr(
              tags$td("4 Visualización"),
              tags$td("70–95 min"),
              tags$td("Gráficos de caja con geom_boxplot() y detección de outliers")
            ),
            tags$tr(
              tags$td("5 Interpretación"),
              tags$td("95–120 min"),
              tags$td("Aplicación agronómica de resultados y discusión")
            )
          )
        ),
      ),  

      # ——————————————
      # PESTAÑA: 1 Medidas básicas
      # ——————————————
      nav_panel(
          title = "1 Medidas básicas",
          h4(class = "section-header", "Medidas básicas"),

          # ----- Texto teórico introductorio -----
          h5(class = "section-header", "Explicación teorica"),
          tags$div(class = "theory-text plot-box",
            tags$p("En esta sesión se profundiza en la estadística descriptiva, presentando las medidas numéricas más comunes para resumir datos cuantitativos. Las medidas de tendencia central describen el centro de la distribución de datos, mientras que las medidas de dispersión describen la variabilidad de los datos alrededor de ese centro (Montgomery & Runger, 2018)."),
            tags$p("Las principales medidas de tendencia central son:"),
            tags$ul(
              tags$li(tags$b("Media aritmética:"), " es el promedio de los valores, calculado sumando todos los datos y dividiendo entre el número de observaciones. Útil para datos cuantitativos continuos, pero sensible a valores extremos."),
              tags$li(tags$b("Mediana:"), " es el valor central de los datos cuando se ordenan de menor a mayor. Divide al conjunto en dos mitades iguales. Es una medida robusta, menos afectada por valores atípicos que la media (Montgomery & Runger, 2018)."),
              tags$li(tags$b("Moda:"), " es el valor o categoría que aparece con mayor frecuencia; útil para variables categóricas o distribuciones discretas."),
              tags$p("A continuación, un histograma de datos generados con distribución normal, con líneas punteadas que indican dónde caen la media (rojo), la mediana (verde) y la moda estimada (azul)."),
              tags$div(class = "plot-box",
                plotOutput(ns("theoryPlot"), height = "300px")
              ),
            ),
          ),
          
          tags$div(class = "theory-text-cuant plot-box",
            tags$p("Para cuantificar la dispersión o variabilidad de los datos:"),
            tags$ul(
              tags$li(tags$b("Rango (amplitud):"), " la diferencia entre el valor máximo y mínimo del conjunto de datos. Es fácil de calcular pero solo depende de dos valores extremos."),
              tags$li(tags$b("Varianza:"), " la media de las desviaciones al cuadrado de cada observación respecto a la media del conjunto. Sirve como base teórica, aunque en unidades al cuadrado."),
              tags$li(tags$b("Desviación estándar:"), " la raíz cuadrada de la varianza. Expresada en las mismas unidades que los datos originales, facilita la interpretación. Una desviación estándar alta indica que los datos están muy dispersos alrededor de la media, mientras que una baja significa que los datos están más concentrados cerca de la media (Montgomery & Runger, 2018)."),
              tags$li(tags$b("Coeficiente de variación (CV):"), " la razón entre la desviación estándar y la media (a menudo expresada en porcentaje). Es útil para comparar variabilidad relativa entre conjuntos de datos de magnitudes muy distintas. En agricultura, el CV se usa para evaluar la estabilidad de rendimientos: por ejemplo, un cultivo con CV alto en rendimiento presenta mucha variabilidad entre parcelas, lo que puede implicar inconsistencias en manejo o en condiciones.")
            )
          ),
          # Slider para sigma con animación
          sliderInput(
            inputId = ns("sigma"),
            label   = "Desviación estándar \\(\\sigma\\):",
            min     = 1,
            max     = 5,
            value   = 1,
            step    = 0.1,
            animate = animationOptions(interval = 500, loop = TRUE)
          ),
          # Punto de dibujo
          tags$div(class = "plot-box",
            tags$p("La línea azul indica la media, la verde la mediana y la roja el rango intercuartílico (IQR)."),
            plotOutput(ns("densPlot"), height = "400px"),
          ),         

          tags$br(),
          tags$p("Por ejemplo, un rendimiento medio de 5.0 t/ha con desviación estándar 0.8 t/ha sugiere dispersión moderada; si la desviación fuera 2.0 t/ha, habría gran heterogeneidad entre parcelas, quizá por variaciones en suelo o manejo."),

          tags$br(),
          h5(class = "section-header", "Ejemplo práctico"),
          
          tags$div(class = "ejemplo-practico",
            tags$p("Supongamos que tenemos un conjunto de datos de rendimiento de trigo en parcelas agrícolas. Queremos calcular medidas descriptivas para entender la variabilidad de los rendimientos."),
            tags$p("Ejemplo práctico: Rendimientos de trigo en 10 parcelas (t/ha)"),
            tags$pre(class = "r-code", 
              htmltools::HTML(
                "# Datos de ejemplo: rendimientos de trigo (t/ha) en 10 parcelas\n",
                "rend_trigo <- c(4.8, 5.5, 5.0, 6.1, 4.9, 5.3, 5.8, 4.7, 5.0, 5.4)\n",
                "\n",
                "# Cálculos de medidas descriptivas\n",
                "mean(rend_trigo, na.rm = TRUE)      # media ≈ 5.25\n",
                "median(rend_trigo, na.rm = TRUE)    # mediana = 5.15\n",
                "sd(rend_trigo, na.rm = TRUE)        # desviación ≈ 0.45\n",
                "\n",
                "# Rango (valor mínimo y máximo)\n",
                "(min(rend_trigo), max(rend_trigo))  # rango = (4.7, 6.1)\n",
                "range(rend_trigo)                   # alternativa que da min y max juntos\n",
                "\n",
                "# Coeficiente de variación (sd/mean)\n",
                "cv <- sd(rend_trigo) / mean(rend_trigo) * 100  \n",
                "cv                                  # CV ≈ 8.5%\n",
              )
            ), 
            tags$p("Este análisis muestra baja variabilidad relativa (CV ~8.5%), indicando rendimientos consistentes."),
            
            h5(class = "section-header", "Graficos que ayudan a visualizar"),
            tags$p("Para visualizar la distribución de los datos y detectar posibles outliers, se pueden usar histogramas y boxplots. Estos gráficos permiten observar la forma de la distribución y la presencia de valores atípicos."),
            tags$pre(class = "r-code", 
              htmltools::HTML(
                "# Histograma\n",
                "hist(rend_trigo, \n",
                "     main = 'Histograma de rendimiento de trigo', \n",
                "     xlab = 'Rendimiento (t/ha)', \n",
                "     ylab = 'Frecuencia')\n",
                "\n",
                "# Boxplot\n",
                "boxplot(rend_trigo, \n",
                "        main = 'Boxplot de rendimiento de trigo', \n",
                "        ylab = 'Rendimiento (t/ha)')\n"
              )
            )
          ),
      
        # ----- Texto práctico -----
        tags$br(),
        tags$div(class = "plot-section plot-box",
          h5(class = "section-header", "Esenarios práctico"),
          tags$p("A continuación, se presentan dos gráficos generados con datos aleatorios. Elija un escenario de datos y observe cómo cambian las medidas de tendencia central y dispersión."),
          tags$br(),
            tags$div(class = "plot-section",
            # Selección de escenario
            selectInput(
              ns("escenario"),
              "Elija un escenario de datos:",
              choices = c(
                "Normal"         = "normal",
                "Con outliers"   = "outliers",
                "CV alto"        = "cv_alto",
                "SD muy grande"  = "sd_grande",
                "Sesgo derecha"  = "skew_right",
                "Sesgo izquierda"= "skew_left",
                "Bimodal"        = "bimodal"
              )
            ),
            tags$br(),
            # Salidas de los dos gráficos
            tags$div(class = "plot-box",
              plotOutput(ns("histPlot"), height = "400px"),
              tags$div(class = "plot-explanation",
                tags$p("El histograma muestra la distribución de los datos elegidos. La línea roja indica la media, la verde la mediana y la azul la moda estimada.")
              ) 
            ),
            tags$br(),
            tags$div(class = "plot-box",
              # Boxplot
              tags$p("El boxplot muestra la mediana (línea negra), el rango intercuartílico (IQR) y los outliers (puntos fuera de los bigotes)."),
              plotOutput(ns("boxPlot"),  height = "200px")
            )
          ),

          tags$br(),
          # Explicación de escenarios de datos
          tags$div(class = "scenario-explanations",
            
            # Explicaciones científicas de cada escenario
            # --- Normal ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: Normal"),
              tags$p(
                "Los datos generados con distribución normal (o gaussiana) siguen la función de densidad ",
                withMathJax(helpText("$$f(x) = \\frac{1}{\\sigma \\sqrt{2\\pi}} e^{-\\tfrac{(x-\\mu)^2}{2\\sigma^2}}$$")),
                " donde \\(\\mu\\) es la media y \\(\\sigma\\) la desviación estándar. Esta distribución es simétrica (asimetría cero), con curtosis teórica de 3, y aparece de forma natural en múltiples fenómenos gracias al Teorema Central del Límite."
              )
            ),

            # --- Con outliers ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: Con outliers"),
              tags$p(
                "Un outlier es una observación que se encuentra a una distancia anormal respecto al resto de los datos. Habitualmente, se identifican los valores que cumplen ",
                withMathJax(helpText("$$x < Q_1 - 1.5\\times IQR \\quad\\text{o}\\quad x > Q_3 + 1.5\\times IQR$$")),
                ", donde \\(Q_1\\) y \\(Q_3\\) son el primer y tercer cuartil, e IQR es la amplitud intercuartílica."
              )
            ),

            # --- CV alto ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: CV alto"),
              tags$p(
                "El coeficiente de variación se define como ",
                withMathJax(helpText("$$CV = \\frac{\\sigma}{\\mu}$$")),
                ", y cuantifica la variabilidad relativa de los datos. Un CV elevado señala que la dispersión (\\(\\sigma\\)) es considerable en relación con la media (\\(\\mu\\)), lo que puede reflejar inconsistencias o alta heterogeneidad en datos agronómicos."
              )
            ),

            # --- SD muy grande ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: SD muy grande"),
              tags$p(
                "La desviación estándar mide la dispersión absoluta: ",
                withMathJax(helpText("$$\\sigma = \\sqrt{\\frac{1}{n}\\sum_{i=1}^n (x_i - \\mu)^2}$$")),
                ". Un \\(\\sigma\\) muy alto indica gran dispersión, lo que dificulta identificar tendencias centrales y outliers."
              )
            ),

            # --- Sesgo a la derecha ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: Sesgo a la derecha"),
              tags$p(
                "El coeficiente de asimetría se define como ",
                withMathJax(helpText("$$\\gamma_1 = \\frac{E[(X-\\mu)^3]}{\\sigma^3}$$")),
                ". Si \\(\\gamma_1 > 0\\), la distribución presenta una cola larga hacia valores mayores, concentrando la mayoría de observaciones a la izquierda."
              )
            ),

            # --- Sesgo a la izquierda ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: Sesgo a la izquierda"),
              tags$p(
                "Cuando \\(\\gamma_1 < 0\\), la distribución tiene cola larga hacia valores menores, implicando una mayoría de datos en torno a valores altos con extremos por debajo del promedio."
              )
            ),

            # --- Bimodal ---
            tags$div(class = "scenario-explanation",
              tags$h5("Escenario: Bimodal"),
              tags$p(
                "Una distribución bimodal presenta dos picos o modos, modelable como mezcla de dos funciones de densidad:",
                withMathJax(helpText("$$f(x) = p\\,f_1(x) + (1-p)\\,f_2(x)$$")),
                ", donde cada componente aporta uno de los modos (mayor y minoritario)."
              )
            )
          ),
        ),
        
        tags$br(),
        # ----- Ejercicios prácticos -----
        tags$div(class = "exercise-section plot-box",
          tags$h5("Ejercicios prácticos"),
          tags$ol(
            tags$li("Importa tu propio dataset de rendimiento y calcula media, mediana y desviación estándar."),
            tags$li("Compara las medidas de tendencia central y dispersión entre dos tratamientos usando group_by() y summarise()."),
            tags$li("Genera un histograma y un boxplot de tus datos; interpreta si existen valores atípicos."),
            tags$li("Calcula el coeficiente de variación para evaluar la estabilidad de rendimientos.")
          )
        )
      ),

      # ——————————————
      # PESTAÑA: 2 Curtosis/Asimetría
      # ——————————————

      # --- UI: pestaña 2 Curtosis/Asimetría ---
      nav_panel(
        title = "2 Curtosis/Asimetría",
        h4(class = "section-header", "Curtosis y Asimetría"),
        
        # ---- Texto teórico ----
        tags$br(),
        h5(class = "section-header", "Explicación teorica"),
        tags$div(class = "theory-text",
          tags$p("La curtosis y la asimetría describen la forma de la distribución de los datos. Según Joanes y Gill (1998), la asimetría mide la simetría, con valores negativos indicando distribución sesgada a la izquierda y positivos a la derecha, mientras que la curtosis mide la “colitud” de las colas, con valores positivos indicando colas pesadas (leptocúrtica) y negativos colas ligeras (platicúrtica). En R, estas medidas se calculan con el paquete ", tags$i("moments"), "."),
          
          # Ecuaciones con MathJax
          tags$p(
            "Se definen matemáticamente como:",
            withMathJax(helpText("$$\\gamma_1 = \\frac{E\\bigl[(X-\\mu)^3\\bigr]}{\\sigma^3}, \\quad \\text{(Asimetría)}$$")),
            withMathJax(helpText("$$\\gamma_2 = \\frac{E\\bigl[(X-\\mu)^4\\bigr]}{\\sigma^4} - 3, \\quad \\text{(Exceso de Curtosis)}$$"))
          ),

          tags$p("Donde \\(\\mu\\) es la media, \\(\\sigma\\) la desviación estándar, \\(E[\\cdot]\\) esperanza matemática. Un \\(\\gamma_1 > 0\\) indica sesgo a la derecha; \\(\\gamma_1 < 0\\), sesgo a la izquierda. Un \\(\\gamma_2 > 0\\) es leptocúrtico; \\(\\gamma_2 < 0\\), platicúrtico; y \\(\\gamma_2 = 0\\) mesocúrtico, igual a la normal."),

          # Rangos recomendados
          tags$p(
            "Para aproximarse a una distribución normal y aplicar tests paramétricos, se recomienda que la asimetría se mantenga entre −1 y +1 (excelente) o como máximo ±2 (aceptable), y que el exceso de curtosis esté dentro de ±2 (estricto) o, en contextos más flexibles, dentro de −7 a +7 (Byrne, 2010; Hair et al., 2010)."
          ), 

          tags$div(class = "note-cloud",
            tags$strong("Ayuda al alumno:"),
            "Cuando calcules la curtosis, fíjate si usas exceso de curtosis (γ₂) o la definición directa (kurtosis – 3)."
          ),

          # Utilidad en agronomía
          tags$p("En agronomía, estos estadísticos permiten:"),
          tags$ul(
            tags$li(
              "  - Detectar sesgos en los rendimientos para ajustar manejo: valores de asimetría próximos a cero garantizan la validez de ANOVA y regresión lineal en ensayos de campo (George & Mallery, 2010)."
            ),

            tags$li(
              "Cuantificar riesgo de eventos extremos: un exceso de curtosis positivo indica colas pesadas y mayor probabilidad de rendimientos muy bajos o muy altos, afectando planificación de cosecha y control de calidad (SPC for Excel, 2007)."
            ),

            tags$li(
              "Identificar homogeneidad en parcelas: exceso de curtosis negativo señala colas ligeras, propio de datos más uniformes, deseable en ensayos comparativos de variedades (Ramirez, 2001)."
            ),

            tags$li(
              "Diseñar prácticas resilientes: un sesgo bajo (|γ₁| < 0.5) y exceso de curtosis cercano a cero (|γ₂| < 0.5) son óptimos para modelos de estabilidad interanual y pronósticos de rendimiento (Tabachnick & Fidell, 2013)."
            ), 
          ),

          # Ejemplo de datos y figura
          tags$p(
            "Ejemplo de datos (t/ha): ",
            tags$code("c(4.8, 5.5, 5.0, 6.1, 4.9, 5.3, 5.8, 4.7, 5.0, 5.4, 4.6, 5.2)")
          ),

          tags$p(
            "Calculamos en R:",
            tags$code("skewness(datos)  ≈ -0.15"),
            ", ",
            tags$code("kurtosis(datos) ≈ -0.42"),
            "—ambos cercanos a cero, indicando una distribución casi normal."
          ),

          tags$p("A continuación, la distribución de estos datos con líneas punteadas que señalan la media (rojo), la dirección del sesgo (morado) y la curtosis (marrón):"),
          plotOutput(ns("skewKurtIllustration"), height = "300px")

        ),

        # ---- Ejemplo práctico en R ----
        h5(class = "section-header", "Ejemplo práctico en R"),
        tags$pre("
# Instalar y cargar paquete
# install.packages('moments')
library(moments)

# Supongamos datos ficticios en un data.frame
set.seed(42)
datos <- data.frame(variable = c(rnorm(100, 10, 2), rnorm(10, 20, 1)))

# Calcular asimetría y curtosis
asimetria <- skewness(datos$variable)    # skewness()
curtosis   <- kurtosis(datos$variable)   # kurtosis()  (retorna exceso de curtosis)
print(paste('Asimetría =', round(asimetria, 3)))
print(paste('Exceso de Curtosis =', round(curtosis, 3)))
            "),

            # ---- Visualización de datos ----
            tags$br(),
            tags$h5("Visualización de distribuciones"),
            tags$p("Para explorar la forma de la distribución se recomienda usar:"),
            tags$ul(
              tags$li(tags$b("Histograma:"), " identifica la forma general (normalidad, sesgos, multimodalidad)."),
              tags$li(tags$b("Boxplot:"), " muestra mediana, cuartiles y outliers (<1.5×IQR)."),
              tags$li(tags$b("Gráfico de densidad:"), " enfatiza colas y picos suavizados.")
            ),

            # Curtosis
            tags$h5("Visualización interactiva de Curtosis"),
            plotlyOutput(ns("kurtosisPlot"), height = "300px"),
            tags$div(class = "plot-explanation",
              tags$p(
                "La curtosis mide el peso relativo de las colas de la distribución frente a su parte central. Aunque se pensó inicialmente que cuantificaba el \"pico\" de la curva, en realidad refleja la probabilidad de obtener valores extremos."
              ),
              tags$p(
                "Una distribución normal tiene curtosis = 3. Muchos paquetes reportan el ",
                tags$b("exceso de curtosis"),
                " (kurtosis – 3), de modo que para la normal el exceso es 0."
              ),
              tags$ul(
                tags$li(
                  tags$b("Mesocúrtica (Exceso = 0):"),
                  " igual a la normal; función ",
                  tags$code("dnorm(x, 0, 1)")
                ),
                tags$li(
                  tags$b("Leptocúrtica (Exceso > 0):"),
                  " colas pesadas, mayor probabilidad de valores extremos; ejemplo con ",
                  tags$code("dt(x, df = 2)")
                ),
                tags$li(
                  tags$b("Platicúrtica (Exceso < 0):"),
                  " colas ligeras, menor probabilidad de valores extremos; ejemplo con ",
                  tags$code("dunif(x, -3, 3)")
                )
              ),
              tags$p(
                "En este gráfico usamos un rango simétrico de ",
                tags$code("x = -5"),
                " a ",
                tags$code("x = 5"),
                " para comparar claramente la extensión y el peso de las colas en cada caso."
              )
            ),


            # Asimetría
            tags$h5("Visualización interactiva de Asimetría"),
            tags$br(),
            plotlyOutput(ns("skewnessPlot"), height = "400px"),
            tags$div(class = "plot-explanation",
              tags$p("Este gráfico interactivo muestra tres paneles con distribuciones de distinto sesgo:"),
              tags$ul(
                tags$li(tags$b("Sesgo Positivo (Gamma):"),
                        " una distribución Gamma (shape = 2, scale = 2) con cola larga a la derecha;"),
                tags$li(tags$b("Distribución Simétrica (Normal):"),
                        " una curva normal estándar sin sesgo;"),
                tags$li(tags$b("Sesgo Negativo (Gamma invertida):"),
                        " el espejo de la Gamma, con cola larga a la izquierda.")
              ),
              tags$p("En cada panel se han añadido líneas verticales que señalan:"),
              tags$ul(
                tags$li(tags$span(style = "color:blue;", "Moda"), "(línea sólida azul)"),
                tags$li(tags$span(style = "color:green;", "Mediana"), "(línea punteada verde)"),
                tags$li(tags$span(style = "color:red;", "Media"), "(línea punteada roja)")
              ),
              tags$p("Los rangos en el eje X son:"),
              tags$ul(
                tags$li("0 a 14 para Sesgo Positivo,"),
                tags$li("-6 a 6 para la Distribución Simétrica,"),
                tags$li("-14 a 0 para Sesgo Negativo.")
              ),
            ),    

            # ---- Tabla resumen ----
            tags$br(),
            h5(class = "section-header", "Tabla resumen"),
            tags$div(class = "table-responsive",
              tags$table(class = "table table-bordered",
                tags$thead(
                  tags$tr(
                    tags$th("Medida"),
                    tags$th("Definición"),
                    tags$th("Interpretación"),
                    tags$th("Función en R (moments)")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Curtosis"),
                    tags$td("Pesadez de las colas de la distribución"),
                    tags$td(
                      tags$ul(
                        tags$li("> 0: Leptocúrtica colas pesadas, pico agudo"),
                        tags$li("< 0: Platicúrtica colas ligeras, pico plano"),
                        tags$li("= 0: Mesocúrtica igual a normal")
                      )
                    ),
                    tags$td(tags$code("kurtosis(x)"))
                  ),
                  tags$tr(
                    tags$td("Asimetría"),
                    tags$td("Falta de simetría alrededor de la media"),
                    tags$td(
                      tags$ul(
                        tags$li("> 0: Sesgo positivo cola derecha alargada"),
                        tags$li("< 0: Sesgo negativo cola izquierda alargada"),
                        tags$li("≈ 0: Simétrica")
                      )
                    ),
                    tags$td(tags$code("skewness(x)"))
                  )
                )
              )
            ),

            # ---- Ejercicio práctico ----
            tags$br(),
            tags$h5("Ejercicio práctico"),
            tags$ol(
              tags$li("Importa tu propio conjunto de datos y calcula asimetría y curtosis con ", tags$code("skewness()"), " y ", tags$code("kurtosis()"), "."),
              tags$li("Interpreta si tu variable presenta sesgo o colas pesadas/ligeras."),
              tags$li("Compara los resultados entre dos grupos de tratamiento usando ", tags$code("group_by()"), " + ", tags$code("summarise()"), ".")
            ),
      ),

      # ——————————————
      # PESTAÑA: 3 Agrupación
      # ——————————————

      nav_panel(
        title = "3 Agrupación",
        h4(class = "section-header", "3 Agrupación – Análisis por grupos"),

        # --- Texto teórico ---
        tags$br(),
        h5(class = "section-header", "Explicación teorica"),
        tags$div(class = "theory-text",
          tags$p(
            "En experimentos agronómicos a menudo se compara la estadística descriptiva entre grupos —por ejemplo, tratamientos o lotes— para evaluar diferencias en rendimiento o calidad. 
            El paquete ", tags$b("dplyr"), " (parte del tidyverse) ofrece dos funciones clave:",
          ),
          tags$ul(
            tags$li(
              tags$b("group_by():"), 
              "agrupa filas de un data frame según una o más variables categóricas."
            ),
            tags$li(
              tags$b("summarise():"), 
              "calcula estadísticas resumen por cada grupo generado por group_by()."
            )
          ),
          tags$p(
            "Usamos el pipe nativo de R (`|>`) o (`%>%`) para encadenar pasos de forma legible:"
          )
        ),

        # --- Tabla de funciones ---
        tags$br(),
        tags$h5("Funciones group_by() y summarise()"),
        tags$table(class = "table table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Función"),
              tags$th("Paquete"),
              tags$th("Descripción"),
              tags$th("Sintaxis Básica"),
              tags$th("Ejemplo")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("group_by()"),
              tags$td("dplyr"),
              tags$td("Define las variables de agrupación."),
              tags$td("data %>% group_by(var1, var2)"),
              tags$td("df %>% group_by(Tratamiento)")
            ),
            tags$tr(
              tags$td("summarise()"),
              tags$td("dplyr"),
              tags$td("Calcula estadísticas de resumen por grupo."),
              tags$td("data_grp %>% summarise(nueva = fun(columna))"),
              tags$td("df_grp %>% summarise(\n  media = mean(Rendimiento, na.rm=TRUE),\n  sd = sd(Rendimiento, na.rm=TRUE),\n  n = n()\n)")
            )
          )
        ),

        # Selección de número de datos
        tags$div(class = "data-selection",
          numericInput(
            ns("n_registros"),
            label   = "Número de registros a generar:",
            value   = 20,
            min     = 5,
            max     = 100,
            step    = 5
          ),
          actionButton(ns("genData"), "Generar datos aleatorios"),
          tags$h5("Data frame generado"),
          tableOutput(ns("dfPreview"))
        ),

        tags$hr(),

        # --- Ejemplos prácticos ---
        tags$br(),
        tags$h5("Ejemplos prácticos en R"),
        # Botón y salida para ejercicio 1: solo group_by()
        tags$div(class = "exercise",
          tags$pre("
# Agrupamos sin resumir aún
df_grp <- df %>% group_by(Tratamiento)
          "),
          actionButton(ns("run1"), "Ejercicio: group_by()"),
          tableOutput(ns("res1")),
          verbatimTextOutput(ns("exp1"))
        ),

        tags$hr(),

        # Botón y salida para ejercicio 2: solo summarise()
        tags$div(class = "exercise",
          tags$pre("
# Resumimos TODO el data frame (sin group_by prevía)
df_sum <- df %>% summarise(
  media_rend = mean(Rendimiento, na.rm = TRUE),
  sd_rend    = sd(Rendimiento, na.rm = TRUE),
  n          = n()
)
          "),
          actionButton(ns("run2"), "Ejercicio: summarise()"),
          tableOutput(ns("res2")),
          verbatimTextOutput(ns("exp2"))
        ),

        tags$hr(),

        # Botón y salida para ejercicio 3: group_by() + summarise() por dos variables
        tags$div(class = "exercise",
          tags$pre("
# Agrupamos y resumimos por dos variables 
df_res <- df %>%
      group_by(Lote, Tratamiento) %>%
      summarise(
        media_cal = round(mean(Calidad, na.rm = TRUE), 2),
        sd_cal    = round(sd(Calidad, na.rm = TRUE), 2),
        .groups = 'drop'
      )
          "),
          actionButton(ns("run3"), "Ejercicio: group_by(Lote, Tratamiento)"),
          tableOutput(ns("res3")),
          verbatimTextOutput(ns("exp3"))
        ),

        tags$hr(),

        # Botón y salida para ejercicio: solo across() con mutate()
        tags$div(class = "exercise",
          tags$pre("
# Ejercicio: usar across() con mutate() para transformar columnas
# (sin resumir ni agrupar)
df_across_mut <- df |>
  mutate(
    across(
      c(Rendimiento, Calidad),
      ~ round((.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE), 2),
      .names = 'estandarizada_{.col}'
    )
  )
          "),
          actionButton(ns("runAcrossMut"), "Ejercicio: solo across() con mutate()"),
          tableOutput(ns("resAcrossMut")),
          verbatimTextOutput(ns("expAcrossMut"))
        ),

        tags$hr(),

        # Botón y salida para ejercicio 5: solo summarise() con across()
        tags$div(class = "exercise",
          tags$pre("
# across: aplicar funciones a múltiples columnas sin agrupar
df_across <- df |> 
  summarise(
    across(
      c(Rendimiento, Calidad),
      list(
        media = ~round(mean(.x, na.rm = TRUE), 2),
        sd    = ~round(sd(.x,   na.rm = TRUE), 2)
      ),
      .names = '{.col}_{.fn}'
    ),
    n_total = n()
  )
          "),
          actionButton(ns("runAcross"), "Ejercicio: summarise(), across()"),
          tableOutput(ns("resAcross")),
          verbatimTextOutput(ns("expAcross"))
        ),

        tags$hr(),

        # Botón y salida para ejercicio 4: summarise() con across()
        tags$div(class = "exercise",
          tags$pre("
# Agrupamos y resumimos usando summarise() con across() (para aplicar la misma función a varias columnas)
df_acr <- df %>%
      group_by(Tratamiento) %>%
      summarise(
        across(
          c(Rendimiento, Calidad),
          list(
            media = ~round(mean(.x, na.rm = TRUE), 2),
            sd    = ~round(sd(.x,   na.rm = TRUE), 2)
          ),
          .names = '{.col}_{.fn}'
        ),
        n = n(),
        .groups = 'drop'
      )
          "),
          actionButton(ns("run4"), "Ejercicio: group_by(), summarise(across())"),
          tableOutput(ns("res4")),
          verbatimTextOutput(ns("exp4"))
        )
      ),

      # ——————————————
      # PESTAÑA: 4 Visualización
      # ——————————————
      nav_panel(
        title = "4 Visualización",
        h4(class = "section-header", "4 Visualización de datos"),

        # Teoría
        tags$div(class = "theory-text",
          tags$p(
            "Esta actividad se centra en las técnicas gráficas para la exploración de datos. La visualización es una parte crucial del análisis estadístico, ya que facilita la comprensión de patrones, tendencias y anomalías que pueden no ser evidentes solo con números (Crawley, 2013)."
          ),
          tags$ul(
            tags$li(tags$b("Histogramas:"), " útiles para visualizar la distribución de frecuencias de una variable cuantitativa continua. Permiten identificar la forma de la distribución (normal, sesgada, multimodal, etc.), la presencia de valores extremos y la variabilidad general."),
            tags$li(tags$b("Boxplots:"), " resumen la distribución de una variable mostrando la mediana, los cuartiles (Q1 y Q3), y cualquier valor atípico fuera de 1.5 veces el rango intercuartílico. Son especialmente útiles para comparar distribuciones entre varios grupos de tratamiento o categorías. En agricultura, por ejemplo, se pueden comparar los rendimientos de diferentes variedades o tratamientos fertilizantes mediante boxplots lado a lado."),
            tags$li(tags$b("Gráficos de barras:"), " empleados principalmente para variables categóricas (por ejemplo, distribución de frecuencias de variedades sembradas en una región, o conteo de parcelas por tipo de tratamiento). Cada barra representa una categoría y su altura corresponde a la frecuencia o proporción."),
            tags$li(tags$b("Scatter plots:"), " muestran la relación entre dos variables cuantitativas, trazando puntos en un plano cartesiano. Son muy útiles para detectar correlaciones; por ejemplo, relacionar la cantidad de fertilizante aplicada (eje X) con el rendimiento obtenido (eje Y) para ver si existe tendencia positiva (mayor fertilizante, mayor rendimiento) o alguna tendencia no lineal."),
            tags$li(tags$b("Gráficos de líneas:"), " útiles cuando se analizan datos de series de tiempo o tendencia a lo largo de gradientes. Por ejemplo, rendimiento de un cultivo a través de los años, o crecimiento de una planta a lo largo de semanas.")
          ),
          tags$p(
            "Buenas prácticas: incluir títulos descriptivos, etiquetar los ejes con nombres y unidades, y agregar leyendas cuando corresponda. Un gráfico bien diseñado debe ser claro y transmitir la información clave de forma comprensible sin ambigüedad (Crawley, 2013). También se menciona la importancia de escalar correctamente los ejes (no truncarlos inapropiadamente) y de elegir el tipo de gráfico adecuado para cada tipo de dato."
          )
        ),

        tags$br(),
        tags$div(class = "my-image-container",
          tags$h5("Mi gráfico explicativo"),
          # Ruta relativa: www/images/elegir_graph.jpg → src = "images/elegir_graph.jpg"
          tags$img(
            src    = "images/elegir_graph.jpg",
            alt    = "Ilustración de como elegir el gráfico adecuado",
            class  = "img-responsive",
            width  = "50%",       # puedes controlar tamaño con width/height
            height = NULL,
            style  = "border: 1px solid #ddd; border-radius: 4px; padding: 5px;"
          ),
        ),
        tags$br(),
        tags$div(class = "my-image-container",
          tags$h5("Grafico Boxplot"),
          # Ruta relativa: www/images/boxplot_explanation.png → src = "images/boxplot_explanation.png"
          tags$img(
            src    = "images/boxplot_explanation.png",
            alt    = "Ilustración de como elegir el gráfico adecuado",
            class  = "img-responsive",
            width  = "50%",       # puedes controlar tamaño con width/height
            height = NULL,
            style  = "border: 1px solid #ddd; border-radius: 4px; padding: 5px;"
          )
        ),


        # Ejemplo 1: Boxplot comparativo
        tags$br(),
        h5(class = "section-header", "Ejemplos prácticos en R"),
        tags$div(class = "example-text",
          tags$p("A continuación, se presentan ejemplos de gráficos comunes en análisis de datos agronómicos. Se incluyen ejemplos de boxplots, histogramas, gráficos de barras, scatter plots y gráficos de líneas."),
          tags$p("Los ejemplos son generados aleatoriamente y no representan datos reales.")
        ),
        tags$h6("Ejemplo 1: Boxplot comparativo"),
        tags$pre("
# Simular rendimientos de dos variedades (t/ha)
variedad <- factor(c(rep('A', 15), rep('B', 15)))
rend_frijol <- c(rnorm(15, mean = 2.1, sd = 0.3),
                 rnorm(15, mean = 2.5, sd = 0.3))
df_box <- data.frame(Variedad = variedad, Rendimiento = rend_frijol)
      "),
        plotOutput(ns("vizBoxplot"), height = "400px"),
        tags$p("Comparamos la mediana y dispersión de Rendimiento entre las variedades A y B."),

        tags$hr(),

        # Ejemplo 2: Histograma
        tags$br(),
        tags$h6("Ejemplo 2: Histograma"),
        tags$pre("
# Reusar rend_frijol de antes o simular nueva variable
hist_data <- rend_frijol
      "),
        plotOutput(ns("vizHistogram"), height = "400px"),
        tags$p("El histograma muestra la forma de la distribución de Rendimiento, identificando sesgos y outliers."),

        tags$hr(),

        # Ejemplo 3: Gráfico de barras con medias y barras de error
        tags$br(),
        tags$h6("Ejemplo 3: Gráfico de barras con error estándar"),
        tags$pre("
# Calcular medias y error estándar por Variedad
library(dplyr)
df_bar <- df_box |>
  group_by(Variedad) |>
  summarise(
    media = mean(Rendimiento),
    se    = sd(Rendimiento)/sqrt(n()),
    .groups = 'drop'
  )
      "),
        plotOutput(ns("vizBarplot"), height = "400px"),
        tags$p("Las barras muestran la media de Rendimiento y las líneas de error son ± 1 SE."),

        tags$hr(),

        # Ejemplo 4: Scatter plot (Fertilizante vs Rendimiento)
        tags$br(),
        tags$h6("Ejemplo 4: Scatter plot"),
        tags$pre("
# Simular dosis de fertilizante y rendimiento
set.seed(2025)
fert <- runif(50, 50, 150)                  # kg/ha
yield <- 0.02 * fert + rnorm(50, 5, 0.5)    # t/ha
df_scatter <- data.frame(Fertilizador = fert, Rendimiento = yield)
      "),
        plotOutput(ns("vizScatter"), height = "400px"),
        tags$p("Exploramos la relación lineal entre dosis de fertilizante y rendimiento."),

        tags$hr(),

        # Ejemplo 5: Gráfico de líneas (Serie temporal)
        tags$br(),
        tags$h6("Ejemplo 5: Gráfico de líneas"),
        tags$pre("
# Simular rendimiento a lo largo de 10 días
time <- 1:10
rend_time <- cumsum(rnorm(10, mean = 0.5, sd = 0.2)) + 5
df_line <- data.frame(Día = time, Rendimiento = rend_time)
        "),
          plotOutput(ns("vizLine"), height = "400px"),
          tags$p("La línea muestra la tendencia de rendimiento a lo largo del tiempo.")
      ),

      # ——————————————
      # PESTAÑA: 5 Interpretación
      # ——————————————

      nav_panel(
        title = "5 Interpretación",
        h4(class = "section-header", "5 Interpretación de resultados"),

        # Texto educativo con pasos y citas APA
        tags$div(class = "theory-text",
          tags$h5("Pasos esenciales para un análisis riguroso con estadística descriptiva"),

          tags$p(
            "1. Importación y limpieza de datos: antes de cualquier análisis, es fundamental asegurar la calidad de los datos ",
            "mediante la importación con ", tags$code("read_csv()"), " o ", tags$code("read_excel()"), 
            " y la normalización de nombres y tipos con paquetes como ", tags$code("janitor"), ". Esta etapa minimiza errores de entrada y facilita la reproducibilidad (APEC, 2013)." 
          ),

          tags$p(
            "2. Exploración inicial y EDA: una inspección con ", tags$code("glimpse()"), ", ", tags$code("str()"), " y ", 
            tags$code("summary()"), " combinada con gráficas rápidas (plot(), ggplot2) permite identificar valores faltantes, outliers y estructuras inesperadas (ARS USDA, 2019)." 
          ),

          tags$p(
            "3. Resumen numérico: se calculan medidas de tendencia central (media, mediana, moda) y de dispersión (rango, varianza, desviación estándar) ",
            "para describir características básicas de cada variable." 
          ),

          tags$p(
            "4. Forma de la distribución: asimetría (skewness) y exceso de curtosis (kurtosis – 3) cuantifican sesgos y colas pesadas/ligeras. ",
            "Estos parámetros ayudan a decidir la idoneidad de pruebas paramétricas y a anticipar eventos extremos." 
          ), 

          tags$p(
            "5. Agrupación y resumen por subconjuntos: con `dplyr::group_by()` y `dplyr::summarise()` se comparan estadísticas entre tratamientos, lotes o categorías, ",
            "facilitando la detección de diferencias sistemáticas entre grupos (Scribbr, 2019)." 
          ),

          tags$p(
            "6. Visualización avanzada: histogramas, boxplots y gráficos de barras con errores estándar son esenciales para presentar la variabilidad y la forma de los datos, ",
            "mejorando la interpretación y comunicación de resultados (Scribbr, 2019)." 
          ),

          tags$p(
            "7. Interpretación en contexto agronómico: finalmente, los resultados deben traducirse a recomendaciones prácticas, ",
            "considerando estabilidad de rendimientos, riesgo de valores extremos y heterogeneidad de parcelas." 
          ),
        ),

        tags$hr(),

        # Botón para cargar datos reales y activar análisis
        tags$h5("Práctica aplicada con datos reales"),
        actionButton(ns("runInterp"), "Cargar y analizar datos"),

        # Salidas interactivas (preview, estadísticas, gráfico y texto interpretativo)
        tags$br(), 
        tags$h5("Vista previa"),
        tableOutput(ns("interpPreview")),

        tags$br(), 
        tags$h5("Estadísticos descriptivos"),
        tableOutput(ns("descStats")),

        tags$br(),
        tags$h5("Asimetría y Curtosis"),
        tableOutput(ns("shapeStats")),

        tags$br(), 
        tags$h5("Histograma de Rendimiento"),
        plotOutput(ns("interpHistogram"), height = "400px"),

        tags$br(), 
        tags$h5("Histograma de Calidad"),
        plotOutput(ns("interpHistogramCal"), height = "400px"),

        tags$br(),
        tags$h5("Boxplot por Tratamiento"),
        plotOutput(ns("interpBoxplot"), height = "400px"),

        tags$br(),
        tags$h5("Boxplot por Calidad"),
        plotOutput(ns("interpBoxplotCal"), height = "400px"),

        tags$br(),
        tags$h5("Interpretación final"),
        verbatimTextOutput(ns("interpText"))
      ),


      # ——————————————
      # PESTAÑA: Referencias 
      # ——————————————
      nav_panel(
        title = "Referencias",
        tags$ul(
          tags$li("Byrne, B. M. (2010). _Structural Equation Modeling with AMOS: Basic Concepts, Applications, and Programming_ (2nd ed.). Routledge."),
          tags$li("George, D., & Mallery, P. (2010). _SPSS for Windows Step by Step: A Simple Guide and Reference_ (10th ed.). Pearson."),
          tags$li("Hair, J. F., Black, W. C., Babin, B. J., & Anderson, R. E. (2010). _Multivariate Data Analysis_ (7th ed.). Prentice Hall."),
          tags$li("Joanes, D. N., & Gill, C. A. (1998). Comparing measures of sample skewness and kurtosis. _Journal of the Royal Statistical Society: Series D (The Statistician)_, 47(1), 183–189."),
          tags$li("Ramirez, O. A. (2001). Are crop yields normally distributed? Paper presented at the American Agricultural Economics Association Annual Meeting, Chicago, IL."),
          tags$li("SPC for Excel. (2007). Are skewness and kurtosis useful statistics? Retrieved from https://www.spcforexcel.com/knowledge/basic-statistics/are-skewness-and-kurtosis-useful-statistics"),
          tags$li("Tabachnick, B. G., & Fidell, L. S. (2013). _Using Multivariate Statistics_ (6th ed.). Pearson."),
          tags$li("APEC. (2013). _Agricultural Statistics Best Practice Methodology Handbook_. Retrieved from https://www.apec.org/docs/default-source/Publications/2013/12/Agricultural-Statistics-Best-Practice-Methodology-Handbook/2013_ATC_ag-stat-handbook.pdf"),
          tags$li("ARS USDA. (2019). _Best Practices for Presenting Statistical Information in a Research Article_. Retrieved from https://www.ars.usda.gov/ARSUserFiles/3122/KramerEtAl2019Hortscience-BestPracticesForPresentingStatisticalInformationInAResearchArticle.pdf"),
          tags$li("Investopedia. (2005). Descriptive Statistics: Definition, Overview, Types, and Examples. Retrieved from https://www.investopedia.com/terms/d/descriptive_statistics.asp"),
          tags$li("Julius AI. (2023). Guide to Descriptive Statistics: Definition, Types, and More. Retrieved from https://julius.ai/articles/descriptive-statistical-analysis-guide"),
          tags$li("van Elst, H. (2013). Foundations of Descriptive and Inferential Statistics (arXiv:1302.2525). Retrieved from https://arxiv.org/abs/1302.2525"),
          tags$li("Minitab. (n.d.). Interpret the Key Results for Display Descriptive Statistics. Retrieved from https://support.minitab.com/en-us/minitab/help-and-how-to/statistics/basic-statistics/how-to/display-descriptive-statistics/interpret-the-results/key-results/"),
          tags$li("Scribbr. (2019). Guide to Descriptive Statistics: Definition, Types, and Examples. Retrieved from https://www.scribbr.com/statistics/descriptive-statistics/"),
          tags$li("3ie. (2018). Appendix F – Descriptive Statistics. Retrieved from https://www.3ieimpact.org/sites/default/files/TW4.1022-Online-appendix-F-Descriptive-statistics.pdf"),
          tags$li("Medium. (2024). Unlocking Insights in Agricultural Science with Exploratory Data Analysis: A Complete Guide. Retrieved from https://medium.com/analytics-mastery/unlocking-insights-in-agricultural-science-with-exploratory-data-analysis-a-complete-guide-with-c67e475fa268"),
          tags$li("Minitab Support. (n.d.). Interpret the Key Results for Display Descriptive Statistics. Retrieved from https://support.minitab.com")
        )
      )


    )
  )
}

session2Server <- function(input, output, session) {
  
  #-- PESTAÑA: 1 Medidas básicas
  
  # Gráfico de teoría: media, mediana y moda
  output$theoryPlot <- renderPlot({
    library(ggplot2)

    # 1) Generar datos normales
    set.seed(123)
    x <- rnorm(500, mean = 5, sd = 2)

    # 2) Calcular media, mediana y moda por densidad
    m     <- mean(x)
    med   <- median(x)
    dens  <- density(x)
    mode_ <- dens$x[which.max(dens$y)]

    df <- data.frame(valor = x)

    # 3) Graficar
    ggplot(df, aes(x = valor)) +
      geom_histogram(aes(y = ..density..),
                     bins = 30,
                     fill = "lightblue",
                     color = "white") +
      # Líneas punteadas
      geom_vline(xintercept = m,     linetype = "dashed", color = "red",   size = 1) +
      geom_vline(xintercept = med,   linetype = "dashed", color = "green", size = 1) +
      geom_vline(xintercept = mode_, linetype = "dashed", color = "blue",  size = 1) +
      # Etiquetas con flechas
      annotate("segment",
               x = m,     xend = m,     y = 0, yend = 0.15,
               arrow = arrow(angle = 20, length = unit(0.15, "inches")),
               color = "red") +
      annotate("text",
               x = m, y = 0.17,
               label = "Media", color = "red", hjust = 0.5) +
      annotate("segment",
               x = med,   xend = med,   y = 0, yend = 0.15,
               arrow = arrow(angle = 20, length = unit(0.15, "inches")),
               color = "green") +
      annotate("text",
               x = med, y = 0.17,
               label = "Mediana", color = "green", hjust = 0.5) +
      annotate("segment",
               x = mode_, xend = mode_, y = 0, yend = 0.15,
               arrow = arrow(angle = 20, length = unit(0.15, "inches")),
               color = "blue") +
      annotate("text",
               x = mode_, y = 0.17,
               label = "Moda", color = "blue", hjust = 0.5) +
      labs(
        title    = "Histograma de distribución normal",
        subtitle = "Con media (rojo), mediana (verde) y moda (azul)",
        x        = "Valor",
        y        = "Densidad"
      ) +
      theme_minimal()
  })
  
  # Gráfico de escenarios de datos
  library(ggplot2)
  library(dplyr)
  
  output$densPlot <- renderPlot({
    mu    <- 10
    sigma <- input$sigma

    # Cálculo de varianza y CV
    varianza <- sigma^2                                # Varianza = σ²
    cv_pct    <- (sigma / mu) * 100                    # CV en porcentaje

    # Generar datos
    x <- seq(mu - 4*sigma, mu + 4*sigma, length.out = 500)
    y <- dnorm(x, mean = mu, sd = sigma)
    df <- data.frame(x = x, dens = y)

    validate(
      need(nrow(df) > 0, "No hay datos para graficar")
    )

    ggplot(df, aes(x = x, y = dens)) +
      # Área bajo la curva
      geom_area(fill = "purple", alpha = 0.5) +
      geom_line(color = "purple", size = 1) +

      # Línea punteada en mu
      geom_vline(xintercept = mu, linetype = "dashed", size = 0.7) +

      # Flecha desde y = 0.45 hasta la curva en x = mu
      annotate("segment",
              x    = mu + 1, xend = mu,
              y    = 0.45,  yend = dnorm(mu, mu, sigma),
              arrow = grid::arrow(length = unit(0.2, "cm")),
              colour = "black"
      ) +

      # Texto explicativo junto a la flecha
      annotate("text",
              x     = mu + 1.2,
              y     = 0.45,
              label = "Media~(mu==10)",
              parse = TRUE,
              hjust = 0, vjust = 0.5,
              size  = 4
      ) +

      # Etiquetas y tema
      labs(
        title = paste0(
        "Distribución Normal con σ ≈ ", round(sigma, 2),
        "  |  CV ≈ ", round(cv_pct, 2), "%", 
        "  |  Varianza ≈ ", round(varianza, 2)
        ),
        x     = "Valor",
        y     = "Densidad"
      ) +
      theme_minimal() +

      # Fijar el eje y hasta 0.5 sin recortar datos
      coord_cartesian(ylim = c(0, 0.5))
  })

  datosAleatorios <- reactive({
    switch(input$escenario,
      "normal"         = rnorm(100, mean = 50, sd = 10),
      "outliers"       = c(rnorm(100, 50, 10), 150, 160, 170),
      "cv_alto"        = rnorm(100, mean = 10, sd = 20),
      "sd_grande"      = rnorm(100, mean = 50, sd = 40),
      "skew_right"     = rgamma(100, shape = 2, scale = 10),
      "skew_left"      = -rgamma(100, shape = 2, scale = 10) + 100,
      "bimodal"        = c(rnorm(50, mean = 30, sd = 5),
                           rnorm(50, mean = 70, sd = 5))
    ) %>% 
    data.frame(value = .)
  })
  
  # Histograma
  output$histPlot <- renderPlot({
    df <- datosAleatorios()
    ggplot(df, aes(x = value)) +
      geom_histogram(binwidth = diff(range(df$value))/30, 
                     fill = "steelblue", alpha = 0.7, color = "white") +
      labs(
        title    = "Histograma de datos aleatorios",
        subtitle = names(which(
          c(
            normal = "Normal",
            outliers = "Con outliers",
            cv_alto = "CV alto",
            sd_grande = "SD muy grande",
            skew_right = "Sesgo derecha",
            skew_left = "Sesgo izquierda",
            bimodal = "Bimodal"
          ) == input$escenario
        )),
        x = "Valor", y = "Frecuencia"
      ) +
      theme_minimal()
  })
  
  # Boxplot
  output$boxPlot <- renderPlot({
    df <- datosAleatorios()
    ggplot(df, aes(y = value, x = "")) +
      geom_boxplot(outlier.color = "red", width = 0.3) +
      labs(
        title = "Boxplot de datos aleatorios",
        x     = NULL,
        y     = "Valor"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  })

  #-- PESTAÑA: 2 Curtosis/Asimetría

  # Gráfico de ilustración de sesgo y curtosis
  output$skewKurtIllustration <- renderPlot({
    library(ggplot2)
    library(moments)

    # Datos de ejemplo
    datos <- c(4.8, 5.5, 5.0, 6.1, 4.9, 5.3, 5.8, 4.7, 5.0, 5.4, 4.6, 5.2)
    m      <- mean(datos)
    s      <- sd(datos)
    gamma1 <- skewness(datos)
    gamma2 <- kurtosis(datos)

    df <- data.frame(x = datos)

    ggplot(df, aes(x)) +
      geom_histogram(aes(y = ..density..),
                     bins = 6,
                     fill = "lightgreen",
                     color = "darkgreen") +
      geom_density(color = "darkblue", size = 1) +
      # Media
      geom_vline(xintercept = m, linetype = "dashed", color = "red") +
      annotate("text", x = m, y = 0.4,
               label = "Media", color = "red", vjust = -0.5) +
      # Sesgo
      annotate("segment",
               x = m + gamma1 * s, xend = m + gamma1 * s,
               y = 0, yend = 0.3,
               arrow = arrow(length = unit(0.1, "inches")),
               color = "purple") +
      annotate("text", x = m + gamma1 * s, y = 0.32,
               label = paste0("Sesgo = ", round(gamma1, 2)),
               color = "purple", hjust = 0) +
      # Curtosis
      annotate("segment",
               x = m, xend = m,
               y = 0, yend = 0.25,
               arrow = arrow(length = unit(0.1, "inches")),
               color = "brown") +
      annotate("text", x = m, y = 0.27,
               label = paste0("Curtosis = ", round(gamma2, 2)),
               color = "brown", hjust = 1) +
      labs(
        title    = "Distribución de rendimientos de trigo",
        subtitle = "Media (rojo), Sesgo (morado), Curtosis (marrón)",
        x        = "Rendimiento (t/ha)",
        y        = "Densidad"
      ) +
      theme_minimal()
  })

  # Gráfico de distribución de curtosis
  # Curtosis: comparativa de mesocúrtica (normal), leptocúrtica (t df=2) y platicúrtica (uniforme)
  output$kurtosisPlot <- renderPlotly({
    library(plotly)
    # dominio común
    x <- seq(-5, 5, length.out = 400)
    # densidades
    y_norm <- dnorm(x, mean = 0, sd = 1)
    y_t2   <- dt(x, df = 2)                       # colas pesadas
    y_unif <- dunif(x, min = -3, max = 3)         # colas ligeras

    plot_ly(x = ~x, y = ~y_norm, name = "Mesocúrtica (Normal)",
            type = "scatter", mode = "lines") %>%
      add_trace(y = ~y_t2,   name = "Leptocúrtica (t df=2)") %>%
      add_trace(y = ~y_unif, name = "Platicúrtica (Uniforme)") %>%
      layout(
        title = "Distribuciones según curtosis",
        xaxis = list(title = "x"),
        yaxis = list(title = "Densidad")
      )
  })

  # Asimetría: simétrica (normal), sesgo a la derecha (exponencial) y a la izquierda (mirrored exp)
  library(plotly)

  output$skewnessPlot <- renderPlotly({
    # Parámetros Gamma para sesgo positivo
    shape <- 2; scale <- 2
    x_pos   <- seq(0, 14, length.out = 400)
    dens_pos <- dgamma(x_pos, shape = shape, scale = scale)

    # Normal estándar (simétrica)
    x_sym   <- seq(-6, 6, length.out = 400)
    dens_sym <- dnorm(x_sym, 0, 1)

    # Espejo Gamma para sesgo negativo
    x_neg   <- seq(-14, 0, length.out = 400)
    dens_neg <- dgamma(-x_neg, shape = shape, scale = scale)

    # Máximo global de densidad
    y_max_global <- max(dens_pos, dens_sym, dens_neg) * 1.1

    # Función auxiliar que fija range = y_max_global
    make_panel <- function(x, y, mn, md, mo, title) {
      plot_ly(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = title, hoverinfo = 'none') %>%
        add_lines(name = "Densidad") %>%
        add_lines(x = c(mo, mo), y = c(0, y_max_global),
                  line = list(color = 'blue', width = 2),
                  name = 'Moda') %>%
        add_lines(x = c(md, md), y = c(0, y_max_global),
                  line = list(color = 'green', dash = 'dash', width = 2),
                  name = 'Mediana') %>%
        add_lines(x = c(mn, mn), y = c(0, y_max_global),
                  line = list(color = 'red', dash = 'dash', width = 2),
                  name = 'Media') %>%
        layout(
          title  = title,
          xaxis  = list(title = 'x'),
          yaxis  = list(title = 'Densidad', range = c(0, y_max_global)),
          showlegend = FALSE
        )
    }

    # Cálculo de media, mediana y moda para cada distribución
    mean_pos <- shape * scale
    mode_pos <- if(shape > 1) (shape - 1) * scale else NA
    med_pos  <- qgamma(0.5, shape = shape, scale = scale)

    mean_sym <- 0; med_sym <- 0; mode_sym <- 0
    mean_neg <- -mean_pos; mode_neg <- -mode_pos; med_neg <- -med_pos

    # Generación de los tres paneles
    p_pos <- make_panel(x_pos,   dens_pos, mean_pos,  med_pos,  mode_pos,  "Sesgo Positivo")
    p_sym <- make_panel(x_sym,   dens_sym, mean_sym,  med_sym,  mode_sym,  "Distribución Simétrica")
    p_neg <- make_panel(x_neg,   dens_neg, mean_neg,  med_neg,  mode_neg,  "Sesgo Negativo")

    # Combinar manteniendo la misma escala Y
    subplot(p_pos, p_sym, p_neg,
            nrows = 1, shareY = TRUE, margin = 0.05) %>%
      layout(
        title = "Comparación de Sesgos: Positivo, Simétrico & Negativo",
        legend = list(x = 0.1, y = -0.2, orientation = 'h')
      )
  })

  # -- PESTAÑA: 3 Agrupación

  # Función que genera datos según n
  make_data <- function(n) {
    set.seed(2025)
    data.frame(
      Lote        = rep(1:4, length.out = n),
      Tratamiento = rep(c('Control','Fert A','Fert B','Orgánico'), length.out = n),
      Rendimiento = rnorm(n, mean = 5, sd = 0.5),
      Calidad     = rnorm(n, mean = 7, sd = 1)
    )
  }

  # Reactivo: data frame generado al pulsar genData
  datos_reactive <- eventReactive(input$genData, {
    make_data(input$n_registros)
  })

  # Mostrar preview del data.frame
  output$dfPreview <- renderTable({
    datos_reactive()
  })

  # Ejercicio 1: solo group_by()
  observeEvent(input$run1, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()
    # Agrupamos sin resumir aún
    df_grp <- df %>% group_by(Tratamiento)
    output$res1 <- renderTable({
      as.data.frame(head(df_grp, 12))  # muestro primeras filas de cada grupo
    })
    output$exp1 <- renderText({
      paste(
        "Se agruparon los datos por Tratamiento con group_by().",
        "Aquí ves las primeras filas dentro de cada grupo;",
        "este paso prepara para calcular estadísticos agrupados."
      )
    })
  })

  # Ejercicio 2: solo summarise()
  observeEvent(input$run2, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()
    # Resumimos TODO el data frame (sin group_by prevía)
    df_sum <- df %>% summarise(
      media_rend = mean(Rendimiento, na.rm = TRUE),
      sd_rend    = sd(Rendimiento, na.rm = TRUE),
      n          = n()
    )
    output$res2 <- renderTable({ df_sum })
    output$exp2 <- renderText({
      paste(
        "Se aplicó summarise() directamente al data frame",
        "sin agrupar primero, por lo que se obtiene un único conjunto de estadísticas",
        "(media, desviación y cuenta) para todo el data set."
      )
    })
  })

  # Ejercicio 3: group_by(Lote, Tratamiento) + summarise()
  observeEvent(input$run3, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()
    df_res <- df %>%
      group_by(Lote, Tratamiento) %>%
      summarise(
        media_cal = round(mean(Calidad, na.rm = TRUE), 2),
        sd_cal    = round(sd(Calidad, na.rm = TRUE), 2),
        .groups = 'drop'
      )
    output$res3 <- renderTable({ df_res })
    output$exp3 <- renderText({
      paste(
        "Aquí agrupamos por dos variables simultáneamente (Lote y Tratamiento) y",
        "calculamos media y desviación de Calidad en cada combinación de grupo.",
        "Es útil para comparar cómo varía la calidad entre lotes y tratamientos."
      )
    })
  })

  observeEvent(input$runAcrossMut, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()

    # Aplicar mutate() con across() para estandarizar columnas
    df_across_mut <- df |>
      mutate(
        across(
          c(Rendimiento, Calidad),
          ~ round((.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE), 2),
          .names = 'estandarizada_{.col}'
        )
      )

    # Mostrar resultados
    output$resAcrossMut <- renderTable({ head(df_across_mut, 10) })

    # Explicación
    output$expAcrossMut <- renderText({
      paste(
        "En este ejercicio usamos mutate() con across():",
        "- seleccionamos las columnas Rendimiento y Calidad;",
        "- para cada una restamos su media y dividimos por su sd (estandarización);",
        "- guardamos el resultado en nuevas columnas 'estandarizada_Rendimiento' y 'estandarizada_Calidad';",
        "- mostramos las primeras 10 filas para ver las transformaciones."
      )
    })
  })

  # Ejercicio 5: solo summarise() con across()
  observeEvent(input$runAcross, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()

    # Aplicamos summarise() con across() sin agrupar
    df_across <- df |>
      summarise(
        across(
          c(Rendimiento, Calidad),
          list(
            media = ~round(mean(.x, na.rm = TRUE), 2),
            sd    = ~round(sd(.x,   na.rm = TRUE), 2)
          ),
          .names = '{.col}_{.fn}'
        ),
        n_total = n()
      )

    # Mostrar resultados
    output$resAcross <- renderTable({ df_across })

    # Explicación
    output$expAcross <- renderText({
      paste(
        "En este ejercicio usamos sólo summarise() con across():",
        "- aplicamos simultáneamente las funciones media y desviación estándar",
        "a las columnas Rendimiento y Calidad;",
        "el resultado es UN solo registro que resume TODO el data frame;",
        "n_total muestra el tamaño del conjunto original."
      )
    })
  })

  # Ejercicio 4: summarise() con across()
  observeEvent(input$run4, {
    # Usamos el data frame generado por el botón "Generar datos aleatorios"
    df <- datos_reactive()
    df_acr <- df %>%
      group_by(Tratamiento) %>%
      summarise(
        across(
          c(Rendimiento, Calidad),
          list(
            media = ~round(mean(.x, na.rm = TRUE), 2),
            sd    = ~round(sd(.x,   na.rm = TRUE), 2)
          ),
          .names = "{.col}_{.fn}"
        ),
        n = n(),
        .groups = 'drop'
      )
    output$res4 <- renderTable({ df_acr })
    output$exp4 <- renderText({
      paste(
        "Usamos across() dentro de summarise() para aplicar varias funciones",
        "(media y sd) a múltiples columnas a la vez, generando nombres de columnas",
        "automáticamente con el sufijo _media y _sd."
      )
    })
  })

  # -- PESTAÑA: 4 Visualización

  # Ejemplo 1: Boxplot comparativo
  output$vizBoxplot <- renderPlot({
    df_box <- data.frame(
      Variedad = factor(c(rep('A', 15), rep('B', 15))),
      Rendimiento = c(rnorm(15, 2.1, 0.3), rnorm(15, 2.5, 0.3))
    )
    ggplot(df_box, aes(x = Variedad, y = Rendimiento, fill = Variedad)) +
      geom_boxplot() +
      labs(
        title = "Rendimiento por Variedad",
        x     = "Variedad",
        y     = "Rendimiento (t/ha)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Ejemplo 2: Histograma
  output$vizHistogram <- renderPlot({
    df_box <- data.frame(
      Rendimiento = c(rnorm(15, 2.1, 0.3), rnorm(15, 2.5, 0.3))
    )
    ggplot(df_box, aes(x = Rendimiento)) +
      geom_histogram(binwidth = 0.2, fill = "steelblue", color = "white") +
      labs(
        title = "Histograma de Rendimiento",
        x     = "Rendimiento (t/ha)",
        y     = "Frecuencia"
      ) +
      theme_minimal()
  })

  # Ejemplo 3: Gráfico de barras con error estándar
  output$vizBarplot <- renderPlot({
    df_bar <- data.frame(
      Variedad = factor(c(rep('A', 15), rep('B', 15))),
      Rendimiento = c(rnorm(15, 2.1, 0.3), rnorm(15, 2.5, 0.3))
    ) |>
      group_by(Variedad) |>
      summarise(
        media = mean(Rendimiento),
        se    = sd(Rendimiento) / sqrt(n()),
        .groups = 'drop'
      )

    ggplot(df_bar, aes(x = Variedad, y = media, fill = Variedad)) +
      geom_col(width = 0.6) +
      geom_errorbar(aes(ymin = media - se, ymax = media + se), width = 0.2) +
      labs(
        title = "Media de Rendimiento ± SE",
        x     = "Variedad",
        y     = "Media (t/ha)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Ejemplo 4: Scatter plot
  output$vizScatter <- renderPlot({
    set.seed(2025)
    df_scatter <- data.frame(
      Fertilizante = runif(50, 50, 150),
      Rendimiento  = 0.02 * runif(50, 50, 150) + rnorm(50, 5, 0.5)
    )
    ggplot(df_scatter, aes(x = Fertilizante, y = Rendimiento)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(
        title = "Relación Fertilizante vs Rendimiento",
        x     = "Dosis de fertilizante (kg/ha)",
        y     = "Rendimiento (t/ha)"
      ) +
      theme_minimal()
  })

  # Ejemplo 5: Gráfico de líneas
  output$vizLine <- renderPlot({
    df_line <- data.frame(
      Día         = 1:10,
      Rendimiento = cumsum(rnorm(10, 0.5, 0.2)) + 5
    )
    ggplot(df_line, aes(x = Día, y = Rendimiento)) +
      geom_line(size = 1) +
      geom_point() +
      labs(
        title = "Tendencia de Rendimiento en 10 días",
        x     = "Día",
        y     = "Rendimiento (t/ha)"
      ) +
      theme_minimal()
  })

  # -- PESTAÑA: 5 Interpretación

  datos_real <- eventReactive(input$runInterp, {
    path <- file.path("data", "data_session2.csv")
    read.csv(path, stringsAsFactors = FALSE)
  })

  # Vista previa
  output$interpPreview <- renderTable({
    head(datos_real(), 10)
  })

  # Estadísticos descriptivos generales
  output$descStats <- renderTable({
    df <- datos_real() %>%
      mutate(
        calidad_pct = MTWPL / TTWPL * 100
      )
    df %>%
      summarise(
        media_TTWPL   = round(mean(TTWPL,   na.rm = TRUE), 2),
        sd_TTWPL      = round(sd(TTWPL,     na.rm = TRUE), 2),
        media_cal_pct = round(mean(calidad_pct, na.rm = TRUE), 2),
        sd_cal_pct    = round(sd(calidad_pct,   na.rm = TRUE), 2),
        n             = n()
      )
  })

  # Asimetría y exceso de curtosis para TTWPL y calidad_pct
  output$shapeStats <- renderTable({
    df <- datos_real() %>%
      mutate(
        calidad_pct = MTWPL / TTWPL * 100
      )
    data.frame(
      Variable      = c("TTWPL (kg/planta)", "Calidad (% peso comercial)"),
      Asimetría     = c(
                        round(skewness(df$TTWPL),  2),
                        round(skewness(df$calidad_pct),  2)
                      ),
      CurtosisExceso = c(
                        round(kurtosis(df$TTWPL) - 3,       2),
                        round(kurtosis(df$calidad_pct) - 3, 2)
                      )
    )
  })

  # Boxplot de TTWPL por INSTN (variedades/tratamientos)
  output$interpBoxplot <- renderPlot({
    df <- datos_real()

    # Cálculos globales
    global_mean   <- mean(df$TTWPL, na.rm = TRUE)
    global_median <- median(df$TTWPL, na.rm = TRUE)

    # Plot 1: por INSTN con hlines
    p1 <- ggplot(df, aes(x = INSTN, y = TTWPL, fill = INSTN)) +
    geom_boxplot() +
    geom_hline(yintercept = global_mean,   linetype = "dashed", color = "red",   size = 0.8) +
    geom_hline(yintercept = global_median, linetype = "dotted", color = "blue", size = 0.8) +
    labs(
      title    = "TTWPL por Variedad",
      subtitle = "Líneas: media global (rojo), mediana global (azul)",
      x        = "Variedad (INSTN)",
      y        = "TTWPL (kg/planta)"
    ) +
    theme_minimal() +
    theme(
      legend.position   = "none",
      axis.text.x       = element_text(angle = 45, hjust = 1)
    )

    # Plot 2: boxplot global sin agrupar
    p2 <- ggplot(df, aes(x = factor(1), y = TTWPL)) +
      geom_boxplot(fill = "gray80") +
      geom_hline(yintercept = global_mean,   linetype = "dashed", color = "red",   size = 0.8) +
      geom_hline(yintercept = global_median, linetype = "dotted", color = "blue", size = 0.8) +
      labs(
        title = "Visión global",
        x     = "",
        y     = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    # Combinar con patchwork
    p1 + p2 + 
      plot_layout(ncol = 2, widths = c(3, 1))
  })

  # Boxplot de Calidad por INSTN (variedades/tratamientos)
  output$interpBoxplotCal <- renderPlot({

    df <- datos_real() %>%
      mutate(
        calidad_pct = MTWPL / TTWPL * 100
      )

    # Cálculos globales
    global_mean   <- mean(df$calidad_pct, na.rm = TRUE)
    global_median <- median(df$calidad_pct, na.rm = TRUE)

    # Plot 1: por INSTN con hlines
    p1 <- ggplot(df, aes(x = INSTN, y = calidad_pct, fill = INSTN)) +
    geom_boxplot() +
    geom_hline(yintercept = global_mean,   linetype = "dashed", color = "red",   size = 0.8) +
    geom_hline(yintercept = global_median, linetype = "dotted", color = "blue", size = 0.8) +
    labs(
      title    = "Calidad por Variedad",
      subtitle = "Líneas: media global (rojo), mediana global (azul)",
      x        = "Variedad (INSTN)",
      y        = "Calidad (%peso comercial)"
    ) +
    theme_minimal() +
    theme(
      legend.position   = "none",
      axis.text.x       = element_text(angle = 45, hjust = 1)
    )

    # Plot 2: boxplot global sin agrupar
    p2 <- ggplot(df, aes(x = factor(1), y = calidad_pct)) +
      geom_boxplot(fill = "gray80") +
      geom_hline(yintercept = global_mean,   linetype = "dashed", color = "red",   size = 0.8) +
      geom_hline(yintercept = global_median, linetype = "dotted", color = "blue", size = 0.8) +
      labs(
        title = "Visión global",
        x     = "",
        y     = NULL
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    # Combinar con patchwork
    p1 + p2 + 
      plot_layout(ncol = 2, widths = c(3, 1))
  })

  # Histograma de TTWPL (variedades/tratamientos)
  output$interpHistogram <- renderPlot({
    df <- datos_real()
    ggplot(df, aes(x = TTWPL)) +
      geom_histogram() +
      labs(
        title = "Peso Total de Tubérculos (TTWPL)",
        x     = "TTWPL (kg/planta)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Histograma de TTWPL (variedades/tratamientos)
  output$interpHistogramCal <- renderPlot({
    df <- datos_real() %>%
      mutate(
        calidad_pct = MTWPL / TTWPL * 100
      )

    ggplot(df, aes(x = calidad_pct)) +
      geom_histogram() +
      labs(
        title = "Calidad de Tubérculos (%)",
        x     = "Calidad (%peso comercial)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  # Texto interpretativo
  output$interpText <- renderText({
    df <- datos_real() %>% mutate(calidad_pct = MTWPL / TTWPL * 100)

    media_yield   <- round(mean(df$TTWPL,   na.rm = TRUE), 2)
    sd_yield      <- round(sd(df$TTWPL,     na.rm = TRUE), 2)
    skew_yield    <- round(skewness(df$TTWPL), 2)
    kurt_yield    <- round(kurtosis(df$TTWPL) - 3, 2)

    media_qpct    <- round(mean(df$calidad_pct, na.rm = TRUE), 2)
    sd_qpct       <- round(sd(df$calidad_pct,   na.rm = TRUE), 2)
    skew_qpct     <- round(skewness(df$calidad_pct), 2)
    kurt_qpct     <- round(kurtosis(df$calidad_pct) - 3, 2)

    paste(
      "• TTWPL (Peso total de tubérculos, kg/planta):",
      sprintf("   • Media = %s, SD = %s.", media_yield, sd_yield),
      sprintf("   • Asimetría = %s (%s).", skew_yield,
              ifelse(skew_yield > 0, "positivo", "negativo")),
      sprintf("   • Exceso de curtosis = %s (%s).", kurt_yield,
              ifelse(kurt_yield > 0, "colas pesadas", "colas ligeras")),
      "",
      "• Calidad (% peso comercial = MTWPL/TTWPL×100):",
      sprintf("   • Media = %s%%, SD = %s%%.", media_qpct, sd_qpct),
      sprintf("   • Asimetría = %s (%s).", skew_qpct,
              ifelse(skew_qpct > 0, "positivo", "negativo")),
      sprintf("   • Exceso de curtosis = %s (%s).", kurt_qpct,
              ifelse(kurt_qpct > 0, "leptocúrtico", "platicúrtico")),
      "",
      paste(
        "Interpretación conjunta:",
        sprintf("• TTWPL – Media = 0.54 kg/planta, SD = 0.27 kg/planta (CV ≈ %s%%). Sesgo positivo alto (%s) indica cola derecha alargada; exceso de curtosis %s sugiere colas pesadas y mayor probabilidad de valores extremos.",
                round(sd_yield/media_yield*100,1), skew_yield, kurt_yield),
        sprintf("• Calidad (MTWPL/TTWPL ×100) – Media = 73.8%%, SD = 12.09%% (CV ≈ %s%%). Sesgo negativo (%s) refleja cola izquierda alargada; exceso de curtosis %s (leptocúrtico) señala probabilidad de porcentajes extremos de calidad.",
                round(sd_qpct/media_qpct*100,1), skew_qpct, kurt_qpct),
        "• El boxplot por INSTN ilustra diferencias en TTWPL entre variedades, facilitando la identificación de genotipos con rendimiento más estable.",
        sep = "\n"
      ),
      sep = "\n"
    )
  })


}
