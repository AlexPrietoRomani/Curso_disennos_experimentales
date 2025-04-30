# R/modules/session2.R
library(ggplot2)

session2UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 2: Estadística Descriptiva Avanzada")
    ),
    tags$style(HTML("
      .r-code {
        background: #f5f5f5;
        padding: 12px;
        border-radius: 6px;
        font-family: 'Courier New', monospace;
        line-height: 1.4;
        white-space: pre-wrap;
        border: 1px solid #e0e0e0;
        margin: 10px 0;
      }
    ")),
    
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
  
      # Sección de contenido técnico
      tags$div(class = "session-content",
        h5("Medidas descriptivas"),
        p("Implementación en R:"),
        tags$pre(class="r-code", HTML("
# Datos de rendimiento
rendimiento <- c(2.3, 3.1, 2.8, 4.0, 3.5, 2.9)

# Medidas básicas
media <- mean(rendimiento)
mediana <- median(rendimiento)
desv_est <- sd(rendimiento)
varianza <- var(rendimiento)
rango <- range(rendimiento)
    ")),
        p("Resultado esperado:",
          tags$pre(class="r-output", "
[1] Media: 3.1
[1] Mediana: 3.0
[1] Desv. Est.: 0.56
    ")
        ),
        
        h5("Curtosis y asimetría"),
        p("Análisis de forma de distribución [[6]]:"),
        tags$pre(class="r-code", HTML("
library(moments)
curtosis <- kurtosis(rendimiento)
asimetria <- skewness(rendimiento)
    ")),
        p("Interpretación:",
          tags$ul(
            tags$li("Curtosis > 3: Distribución leptocúrtica (cola pesada)"),
            tags$li("Asimetría > 0: Sesgo hacia la derecha")
          )
        ),
        
        h5("Agrupación y visualización"),
        p("Flujo completo con tidyverse [[3]]:"),
        tags$pre(class="r-code", HTML("
library(dplyr)
library(ggplot2)

datos %>%
  group_by(tratamiento) %>%
  summarise(
    media = mean(rendimiento),
    sd = sd(rendimiento)
  ) %>%
  ggplot(aes(x=tratamiento, y=media)) +
  geom_boxplot(aes(ymin=media-sd, ymax=media+sd))
    ")),
        
        h5("Interpretación agronómica"),
        p("Ejemplo de aplicación:",
          tags$ul(
            tags$li("Varianza alta → posible heterogeneidad en campo"),
            tags$li("Asimetría positiva → respuesta diferencial a tratamientos")
          )
        )
      )
    ),

    # ——————————————
    # PESTAÑA: 1 Medidas básicas
    # ——————————————
    nav_panel(
          title = "1 Medidas básicas",
          h4(class = "section-header", "Medidas básicas"),

          # ----- Texto teórico introductorio -----
          h5(class = "section-header", "Explicación teorica"),
          tags$div(class = "theory-text",
            tags$p("En esta sesión se profundiza en la estadística descriptiva, presentando las medidas numéricas más comunes para resumir datos cuantitativos. Las medidas de tendencia central describen el centro de la distribución de datos, mientras que las medidas de dispersión describen la variabilidad de los datos alrededor de ese centro (Montgomery & Runger, 2018)."),
            tags$p("Las principales medidas de tendencia central son:"),
            tags$ul(
              tags$li(tags$b("Media aritmética:"), " es el promedio de los valores, calculado sumando todos los datos y dividiendo entre el número de observaciones. Útil para datos cuantitativos continuos, pero sensible a valores extremos."),
              tags$li(tags$b("Mediana:"), " es el valor central de los datos cuando se ordenan de menor a mayor. Divide al conjunto en dos mitades iguales. Es una medida robusta, menos afectada por valores atípicos que la media (Montgomery & Runger, 2018)."),
              tags$li(tags$b("Moda:"), " es el valor o categoría que aparece con mayor frecuencia; útil para variables categóricas o distribuciones discretas."),
              tags$p("A continuación, un histograma de datos generados con distribución normal, con líneas punteadas que indican dónde caen la media (rojo), la mediana (verde) y la moda estimada (azul)."),
              plotOutput(ns("theoryPlot"), height = "300px")
            ),
          ),
          
          tags$div(class = "theory-text-cuant",
            tags$p("Para cuantificar la dispersión o variabilidad de los datos:"),
            tags$ul(
              tags$li(tags$b("Rango (amplitud):"), " la diferencia entre el valor máximo y mínimo del conjunto de datos. Es fácil de calcular pero solo depende de dos valores extremos."),
              tags$li(tags$b("Varianza:"), " la media de las desviaciones al cuadrado de cada observación respecto a la media del conjunto. Sirve como base teórica, aunque en unidades al cuadrado."),
              tags$li(tags$b("Desviación estándar:"), " la raíz cuadrada de la varianza. Expresada en las mismas unidades que los datos originales, facilita la interpretación. Una desviación estándar alta indica que los datos están muy dispersos alrededor de la media, mientras que una baja significa que los datos están más concentrados cerca de la media (Montgomery & Runger, 2018)."),
              tags$li(tags$b("Coeficiente de variación (CV):"), " la razón entre la desviación estándar y la media (a menudo expresada en porcentaje). Es útil para comparar variabilidad relativa entre conjuntos de datos de magnitudes muy distintas. En agricultura, el CV se usa para evaluar la estabilidad de rendimientos: por ejemplo, un cultivo con CV alto en rendimiento presenta mucha variabilidad entre parcelas, lo que puede implicar inconsistencias en manejo o en condiciones.")
            ),
            tags$p("Por ejemplo, un rendimiento medio de 5.0 t/ha con desviación estándar 0.8 t/ha sugiere dispersión moderada; si la desviación fuera 2.0 t/ha, habría gran heterogeneidad entre parcelas, quizá por variaciones en suelo o manejo."),
          ),
          h5(class = "section-header", "Ejemplo práctico"),
          
          tags$div(class = "ejemplo-practico",
            tags$p("Supongamos que tenemos un conjunto de datos de rendimiento de trigo en parcelas agrícolas. Queremos calcular medidas descriptivas para entender la variabilidad de los rendimientos."),
            tags$p("Ejemplo práctico: Rendimientos de trigo en 10 parcelas (t/ha)"),
            tags$pre("
# Datos de ejemplo: rendimientos de trigo (t/ha) en 10 parcelas
rend_trigo <- c(4.8, 5.5, 5.0, 6.1, 4.9, 5.3, 5.8, 4.7, 5.0, 5.4)

# Cálculos de medidas descriptivas
mean(rend_trigo, na.rm = TRUE)      # media ≈ 5.25
median(rend_trigo, na.rm = TRUE)    # mediana = 5.15
sd(rend_trigo, na.rm = TRUE)        # desviación ≈ 0.45

# Rango (valor mínimo y máximo)
(min(rend_trigo), max(rend_trigo))  # rango = (4.7, 6.1)
range(rend_trigo)                   # alternativa que da min y max juntos

# Coeficiente de variación (sd/mean)
cv <- sd(rend_trigo) / mean(rend_trigo) * 100  
cv                                  # CV ≈ 8.5%
            "), 
            tags$p("Este análisis muestra baja variabilidad relativa (CV ~8.5%), indicando rendimientos consistentes."),
            
            h5(class = "section-header", "Graficos que ayudan a visualizar"),
            tags$p("Para visualizar la distribución de los datos y detectar posibles outliers, se pueden usar histogramas y boxplots. Estos gráficos permiten observar la forma de la distribución y la presencia de valores atípicos."),
            tags$pre("
# Histograma
hist(rend_trigo, 
     main = 'Histograma de rendimiento de trigo', 
     xlab = 'Rendimiento (t/ha)', 
     ylab = 'Frecuencia')

# Boxplot
boxplot(rend_trigo, 
        main = 'Boxplot de rendimiento de trigo', 
        ylab = 'Rendimiento (t/ha)')
  ")
    ),
        
          # ----- Texto práctico -----
          h5(class = "section-header", "Ejercicio práctico"),
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
            plotOutput(ns("histPlot"), height = "400px"),
            plotOutput(ns("boxPlot"),  height = "200px")
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

            tags$br(),
            # ----- Ejercicios prácticos -----
                tags$div(class = "exercise-section",
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
        tags$li("Tabachnick, B. G., & Fidell, L. S. (2013). _Using Multivariate Statistics_ (6th ed.). Pearson.")
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

}
