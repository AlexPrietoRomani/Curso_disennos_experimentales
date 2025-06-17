# R/modules/session2.R

session2UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(class = "session-title", "Sesión 2: Estadística Descriptiva Avanzada y Exploración de Datos"),
    
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
        title = "1. Medidas Básicas",
        
        # Usamos un navset anidado
        navset_card_pill(
            header = h4(class="section-header", "Resumiendo Datos: Del Centro a la Forma"),
            
            # ===== SUB-PESTAÑA 1: TEORÍA =====
            nav_panel(
                title = "Teoría: Centro y Dispersión",
                
                h5("Medidas de Tendencia Central"),
                tags$div(class = "theory-text plot-box",
                  tags$p("En esta sesión se profundiza en la estadística descriptiva, presentando las medidas numéricas más comunes para resumir datos cuantitativos. Las medidas de tendencia central describen el centro de la distribución de datos, mientras que las medidas de dispersión describen la variabilidad de los datos alrededor de ese centro (Montgomery & Runger, 2018)."),
                  tags$p("Las principales medidas de tendencia central son:"),
                  tags$ul(
                    tags$li(tags$b("Media aritmética:"), " es el promedio de los valores, calculado sumando todos los datos y dividiendo entre el número de observaciones. Útil para datos cuantitativos continuos, pero sensible a valores extremos."),
                    tags$li(tags$b("Mediana:"), " es el valor central de los datos cuando se ordenan de menor a mayor. Divide al conjunto en dos mitades iguales. Es una medida robusta, menos afectada por valores atípicos que la media (Montgomery & Runger, 2018)."),
                    tags$li(tags$b("Moda:"), " es el valor o categoría que aparece con mayor frecuencia; útil para variables categóricas o distribuciones discretas."),
                    tags$p("A continuación, un histograma de datos generados con distribución normal, con líneas punteadas que indican dónde caen la media (rojo), la mediana (verde) y la moda estimada (azul)."),
                  ),
                ),
                p("..."),
                plotOutput(ns("theoryPlot"), height = "300px"),
                
                hr(),
                
                h5("Medidas de Dispersión y Variabilidad"),
                tags$div(class = "theory-text-cuant plot-box",
                  tags$p("Para cuantificar la dispersión o variabilidad de los datos:"),
                  tags$ul(
                    tags$li(tags$b("Rango (amplitud):"), " la diferencia entre el valor máximo y mínimo del conjunto de datos. Es fácil de calcular pero solo depende de dos valores extremos."),
                    tags$li(tags$b("Varianza:"), " la media de las desviaciones al cuadrado de cada observación respecto a la media del conjunto. Sirve como base teórica, aunque en unidades al cuadrado."),
                    tags$li(tags$b("Desviación estándar:"), " la raíz cuadrada de la varianza. Expresada en las mismas unidades que los datos originales, facilita la interpretación. Una desviación estándar alta indica que los datos están muy dispersos alrededor de la media, mientras que una baja significa que los datos están más concentrados cerca de la media (Montgomery & Runger, 2018)."),
                    tags$li(tags$b("Coeficiente de variación (CV):"), " la razón entre la desviación estándar y la media (a menudo expresada en porcentaje). Es útil para comparar variabilidad relativa entre conjuntos de datos de magnitudes muy distintas. En agricultura, el CV se usa para evaluar la estabilidad de rendimientos: por ejemplo, un cultivo con CV alto en rendimiento presenta mucha variabilidad entre parcelas, lo que puede implicar inconsistencias en manejo o en condiciones.")
                  )
                ),
                p("..."),
                
                h6("Visualización Interactiva de la Dispersión"),
                p("Observa cómo al cambiar la Desviación Estándar (σ), cambia la forma de la distribución y la dispersión de los datos individuales. Una σ alta implica mayor 'ruido' o variabilidad en tus mediciones."),
                
                sliderInput(
                    inputId = ns("sigma"),
                    label   = "Ajusta la Desviación Estándar (σ):",
                    min = 0.5, max = 5, value = 1.5, step = 0.1,
                    animate = animationOptions(interval = 400, loop = TRUE)
                ),
                
                # Usamos fluidRow para poner los gráficos lado a lado
                fluidRow(
                    column(6, 
                        h5("A) Curva de Densidad Teórica", class="text-center"),
                        plotOutput(ns("densPlot"), height = "300px")
                    ),
                    column(6,
                        h5("B) Muestra de Datos Simulados", class="text-center"),
                        plotOutput(ns("stripPlot"), height = "300px")
                    )
                ),
                
                tags$div(class="alert alert-info mt-2",
                    htmlOutput(ns("dispersion_interpretation"))
                )
            ),
            
            # ===== SUB-PESTAÑA 2: LABORATORIO INTERACTIVO =====
            nav_panel(
                title = "Laboratorio de Distribuciones",
                h5("Explorando Diferentes Escenarios de Datos"),
                p("Selecciona un escenario para generar un conjunto de datos y observa cómo cambian las estadísticas descriptivas y las visualizaciones. Esto te ayudará a entrenar tu ojo para identificar patrones."),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        selectInput(
                            ns("escenario"), "Elige un escenario de datos:",
                            choices = c(
                                "Normal (Ideal)" = "normal",
                                "Con Outliers Extremos" = "outliers",
                                "CV Alto (Mucha Variabilidad Relativa)" = "cv_alto",
                                "Sesgo a la Derecha (Datos de conteo)" = "skew_right",
                                "Sesgo a la Izquierda" = "skew_left",
                                "Bimodal (Dos grupos mezclados)" = "bimodal"
                            )
                        ),
                        hr(),
                        h6("Estadísticas Descriptivas Clave:"),
                        tableOutput(ns("stats_table"))
                    ),
                    mainPanel(
                        width = 9,
                        fluidRow(
                            column(8, plotOutput(ns("histPlot_escenario"), height="300px")),
                            column(4, plotOutput(ns("boxPlot_escenario"), height="300px"))
                        ),
                        h6("Interpretación del Escenario:", class="mt-3"),
                        tags$div(class="note-cloud",
                            htmlOutput(ns("scenario_interpretation"))
                        )
                    )
                )
            ),

            # ===== SUB-PESTAÑA 3: EXPORTAR DATOS =====
            nav_panel(
                title = "Exportar Datos de Escenario",
                h5("Descarga los Datos para Practicar"),
                p("Aquí puedes descargar el conjunto de datos que seleccionaste en el 'Laboratorio de Distribuciones' para poder analizarlo en tu propio RStudio."),
                p("Selecciona el escenario que deseas descargar y haz clic en el botón."),
                selectInput(ns("export_escenario"), "Selecciona el escenario a descargar:",
                            choices = c(
                                "Normal (Ideal)" = "normal",
                                "Con Outliers Extremos" = "outliers",
                                "CV Alto (Mucha Variabilidad Relativa)" = "cv_alto",
                                "Sesgo a la Derecha (Datos de conteo)" = "skew_right",
                                "Sesgo a la Izquierda" = "skew_left",
                                "Bimodal (Dos grupos mezclados)" = "bimodal"
                            )),
                downloadButton(ns("download_data_csv"), "Descargar como CSV", class="btn-success"),
                downloadButton(ns("download_data_xlsx"), "Descargar como Excel (.xlsx)", class="btn-info")
            )
        )
      ),

      # ——————————————
      # PESTAÑA 2: Forma de la Distribución 
      # ——————————————
      nav_panel(
        title = "2. Forma de la Distribución",
        
        h4(class = "section-header", "Más Allá del Centro y la Dispersión"),
        p(
            "Una vez que sabemos dónde está el centro de nuestros datos y cuán dispersos están, el siguiente paso es entender su ", strong("forma."), 
            "¿Son los datos simétricos o están sesgados? ¿Tienen más o menos valores extremos de lo esperado? Estas preguntas son cruciales porque la forma de la distribución determina la validez de muchas de las pruebas estadísticas que usaremos más adelante."
        ),
        
        # Usamos navset para separar Asimetría y Curtosis
        navset_card_pill(
          # --- SUB-PESTAÑA: ASIMETRÍA ---
          nav_panel(
            title = "Asimetría (Skewness)",
            p(strong("La asimetría mide la falta de simetría de una distribución.")),
            p(
                "En un mundo ideal, muchos de nuestros datos seguirían una distribución normal perfectamente simétrica. Sin embargo, en la biología y la agronomía, los sesgos son comunes. Un sesgo nos indica que los datos no se distribuyen equitativamente alrededor de la media."
            ),
            tags$ul(
                tags$li(strong("Asimetría > 0 (Sesgo Positivo):"), " La 'cola' de la distribución se alarga hacia la derecha. Esto es muy común en datos que no pueden ser negativos, como el conteo de plagas, la biomasa o los días hasta la floración. Hay muchas observaciones agrupadas en valores bajos, pero unas pocas observaciones con valores muy altos 'estiran' la media hacia la derecha."),
                tags$li(strong("Asimetría < 0 (Sesgo Negativo):"), " La 'cola' se alarga hacia la izquierda. Es menos común, pero puede ocurrir con datos que tienen un límite superior, como el porcentaje de germinación (muchas parcelas pueden tener 95-100%, pero algunas pueden tener valores mucho más bajos).")
            ),
            p(
                "La relación entre la media, mediana y moda es un indicador clave del sesgo: en un sesgo positivo, típicamente ", code("Media > Mediana > Moda"), "."
            ),
            hr(),
            h6("Laboratorio Interactivo: Explorando el Sesgo con la Distribución Gamma"),
            p(
                "La ", strong("distribución Gamma"), " es un modelo de probabilidad muy útil en ciencias biológicas porque solo toma valores positivos y es naturalmente asimétrica. Se define por un parámetro de ", em("forma (shape)."), " En este laboratorio, puedes manipular este parámetro para ver cómo cambia el sesgo."
            ),
            sidebarLayout(
              sidebarPanel(
                width=3,
                sliderInput(ns("skew_shape"), "Parámetro de Forma (Gamma):", min=1.1, max=10, value=2, step=0.1),
                p(class="text-muted", "Un valor bajo crea un sesgo positivo fuerte. A medida que aumenta, la distribución se vuelve más simétrica."),
                hr(),
                h6("Estadísticas de la Distribución:"),
                verbatimTextOutput(ns("skew_stats_text"))
              ),
              mainPanel(
                width=9,
                plotOutput(ns("skew_plot"), height="350px")
              )
            )
          ),
          
          # --- SUB-PESTAÑA: CURTOSIS ---
          nav_panel(
            title = "Curtosis (Kurtosis)",
            p(strong("La curtosis es una medida del riesgo: la probabilidad de obtener valores extremos (outliers).")),
            p(
                "A menudo se malinterpreta como una medida del 'pico' de la distribución. Es más útil pensar en ella como una medida del 'peso' de las colas. Una alta curtosis no significa un pico más alto, sino colas más 'pesadas', lo que implica que los valores muy alejados de la media son más probables de lo que se esperaría en una distribución normal."
            ),
            p("El punto de referencia es la distribución Normal (mesocúrtica). El ", strong("Exceso de Curtosis"), " se calcula como `Curtosis - 3`."),
            tags$ul(
                tags$li(strong("Exceso de Curtosis > 0 (Leptocúrtica):"), " 'Colas pesadas'. El riesgo de outliers es alto. Imagina los datos de rendimiento de un año con una sequía inesperada o una plaga devastadora; la mayoría de los rendimientos son normales, pero unos pocos son catastróficamente bajos. Esto produce una distribución leptocúrtica."),
                tags$li(strong("Exceso de Curtosis < 0 (Platicúrtica):"), " 'Colas ligeras'. El riesgo de outliers es bajo. Los datos están más uniformemente distribuidos y es menos probable encontrar valores extremos. Esto podría ocurrir en un ambiente muy controlado, como un invernadero, donde las condiciones son muy homogéneas.")
            ),
            hr(),
            h6("Laboratorio Interactivo: Explorando la Curtosis con la Distribución t"),
            p(
                "La ", strong("distribución t de Student"), " es una familia de distribuciones que se parece a la normal pero, dependiendo de sus ", em("grados de libertad (df),"), " puede tener colas más pesadas. Es el ejemplo perfecto para entender la curtosis."
            ),
            sidebarLayout(
              sidebarPanel(
                width=3,
                sliderInput(ns("kurt_df"), "Grados de Libertad (Distribución t):", min=1, max=30, value=3, step=1),
                p(class="text-muted", "Pocos grados de libertad producen colas muy pesadas (alta curtosis). A medida que `df` aumenta, la distribución t se aproxima a la Normal."),
                hr(),
                h6("Estadísticas de la Distribución:"),
                verbatimTextOutput(ns("kurt_stats_text"))
              ),
              mainPanel(
                width=9,
                plotOutput(ns("kurt_plot"), height="350px")
              )
            )
          )
        ),
        hr(),
        # --- SECCIÓN FINAL DE CONCLUSIÓN ---
        h4(class="section-header", "¿Por Qué Nos Importa la Forma para el Diseño de Experimentos?"),
        tags$div(class="alert alert-success", role="alert",
          tags$h5(class="alert-heading", "La Conexión con el ANOVA y las Pruebas Paramétricas"),
          p(
              "El Análisis de Varianza (ANOVA), que es la herramienta central que usaremos a partir de la Sesión 4, es una ", strong("prueba paramétrica."), " Esto significa que su validez matemática se basa en varios supuestos, siendo uno de los más importantes que los ", strong("errores (residuales) del modelo sigan una distribución normal."), " Una distribución normal es, por definición, simétrica (asimetría = 0) y mesocúrtica (exceso de curtosis = 0)."
          ),
          p(
              "Si nuestros datos originales tienen un sesgo o una curtosis muy pronunciados, es muy probable que los residuales de nuestro modelo ANOVA también los tengan, violando así los supuestos de la prueba. Esto puede llevar a:",
              tags$ul(
                  tags$li("P-valores incorrectos, lo que nos puede hacer cometer Errores de Tipo I o II."),
                  tags$li("Estimaciones de los efectos de los tratamientos que no son fiables.")
              )
          ),
          p(
              "Por lo tanto, analizar la asimetría y la curtosis en la fase de exploración de datos es una ", strong("medida de diagnóstico crucial."), " Nos alerta sobre posibles problemas y nos permite tomar acciones correctivas (como la transformación de datos, ej. logarítmica para datos con sesgo positivo) antes de realizar el análisis inferencial. Es el equivalente a que un mecánico revise el motor antes de una carrera larga."
          )
        )
      ),

      # ——————————————
      # PESTAÑA 3: Análisis por Grupos
      # ——————————————
      nav_panel(
        title = "3. Análisis por Grupos",
        
        navset_card_pill(
          header = h4(class="section-header", "De Datos Crudos a Insights: Resumiendo por Grupos"),

          # --- SUB-PESTAÑA: TEORÍA ---
          nav_panel(
              title = "Guía y Funciones Clave",
              
              h5("La Estrategia 'Dividir-Aplicar-Combinar'"),
              p("En la mayoría de los análisis agronómicos, no queremos una sola media para todo el ensayo; queremos comparar las medias de diferentes tratamientos, variedades o condiciones. El flujo de trabajo para lograr esto se conoce como ", strong("Dividir-Aplicar-Combinar"), " (Split-Apply-Combine), una estrategia popularizada por Hadley Wickham."),
              tags$ol(
                  tags$li(strong("Dividir:"), " Se divide el conjunto de datos completo en subconjuntos más pequeños, basados en los niveles de una o más variables categóricas. Esto se hace con ", code("group_by()"), "."),
                  tags$li(strong("Aplicar:"), " A cada subconjunto, se le aplica una función de resumen (como `mean()`, `sd()`, `n()`)."),
                  tags$li(strong("Combinar:"), " Los resultados de cada subconjunto se combinan en una nueva tabla de resumen. La función ", code("summarise()"), " se encarga de los pasos 2 y 3.")
              ),
              
              p("El paquete ", code("dplyr"), " hace este proceso increíblemente fluido y legible usando el operador pipe (", code("%>%"), ")."),
              
              hr(),
              
              h5("Ampliando el Poder con `across()`"),
              p(
                  "¿Qué pasa si queremos calcular la media y la desviación estándar para tres variables diferentes (ej. rendimiento, altura, biomasa)? Escribir el código para cada una sería repetitivo. La función ", code("across()"), " es la solución moderna. Nos permite aplicar las mismas operaciones a múltiples columnas a la vez dentro de un ", code("summarise()"), " o ", code("mutate()"), "."
              ),
              pre(class="r-code", htmltools::HTML(
                  "datos %>% \n",
                  "  group_by(tratamiento) %>% \n",
                  "  summarise(\n",
                  "    across(c(rendimiento, altura, biomasa), # Columnas a resumir\n",
                  "           list(media = ~mean(.x, na.rm=T), # Funciones a aplicar\n",
                  "                sd = ~sd(.x, na.rm=T)),\n",
                  "           .names = '{.col}_{.fn}'), # Cómo nombrar las nuevas columnas\n",
                  "    n = n() # Contar observaciones por grupo\n",
                  "  )"
              ))
          ),
          
          # --- SUB-PESTAÑA: LABORATORIO INTERACTIVO ---
          nav_panel(
            title = "Laboratorio Interactivo de `dplyr`",
            p("Usa los controles para construir tu propio resumen descriptivo. Selecciona cómo agrupar los datos, qué variables resumir y qué estadísticas calcular."),
            sidebarLayout(
              sidebarPanel(
                width = 4,
                tags$h5("Controles de Agrupación y Resumen"),
                
                # Generar datos de ejemplo
                actionButton(ns("gen_data_s2"), "Generar/Refrescar Datos de Ejemplo", icon=icon("sync")),
                hr(),
                
                # Seleccionar variables para agrupar
                selectizeInput(ns("group_vars_s2"), "1. Agrupar por:", 
                              choices = c("Tratamiento", "Lote"), 
                              multiple = TRUE, 
                              options = list(placeholder = 'Selecciona una o más...')),
                
                # Seleccionar variables para resumir
                selectizeInput(ns("summary_vars_s2"), "2. Resumir las variables:",
                              choices = c("Rendimiento", "Calidad"),
                              selected = "Rendimiento",
                              multiple = TRUE),
                
                # Seleccionar funciones de resumen
                checkboxGroupInput(ns("summary_funs_s2"), "3. Calcular estas estadísticas:",
                                  choices = c("Media" = "mean", 
                                              "Desv. Estándar" = "sd", 
                                              "Coef. de Variación (%)" = "cv",
                                              "Conteo (n)" = "n"),
                                  selected = c("mean", "sd", "n"))
              ),
              mainPanel(
                  width = 8,
                  h6("Datos de Ejemplo Generados:"),
                  DT::dataTableOutput(ns("df_preview_s2")),
                  hr(),
                  h6("Tabla de Resumen Dinámica:"),
                  DT::dataTableOutput(ns("summary_table_s2")),
                  hr(),
                  h6("Visualización del Resumen (Media ± Error Estándar):"),
                  plotOutput(ns("summary_plot_s2"))
              )
            )
          )
        )
      ),

      # ——————————————
      # PESTAÑA: 4 Visualización
      # ——————————————
      nav_panel(
        title = "4. Visualización",
        
        navset_card_pill(
          header = h4(class="section-header", "El Arte de Ver los Datos: Guía y Laboratorio Práctico"),
          
          # --- SUB-PESTAÑA 1: GUÍA PARA ELEGIR GRÁFICOS ---
          nav_panel(
              title = "Guía para Elegir Gráficos",
              
              p(
                  "La elección del gráfico correcto depende de la pregunta que quieras responder. ¿Quieres comparar valores? ¿Mostrar la distribución de una variable? ¿Explorar la relación entre dos variables? Este diagrama de flujo (basado en el trabajo de A. Abela) es una excelente guía para tomar esa decisión."
              ),
              
              # --- La Imagen Guía como pieza central ---
              tags$div(class = "text-center p-3 border rounded bg-light",
                  tags$h5("Diagrama de Selección de Gráficos"),
                  tags$img(
                      src    = "images/elegir_graph.jpg", # Asegúrate de que tu imagen se llame así y esté en www/images/
                      alt    = "Guía para la selección de gráficos",
                      class  = "img-fluid" # 'img-fluid' la hace responsive
                  ),
                  tags$p(class="text-muted small mt-2", "Fuente: A. Abela. Una guía para la visualización de datos.")
              ),
              
              hr(),
              
              h5("Tipos de Gráficos Clave en Agronomía"),
              p("Basándonos en la guía, aquí están los gráficos que más usarás en este curso:"),
              tags$ul(
                  tags$li(strong("Para mostrar DISTRIBUCIÓN (1 variable):"), " El ", strong("Histograma"), " y el ", strong("Gráfico de Densidad"), " son tus herramientas principales para entender la forma de tus datos (ej. la distribución del rendimiento de todas tus parcelas)."),
                  tags$li(strong("Para COMPARAR entre ítems (1 variable por categoría):"), " El ", strong("Boxplot"), " y el ", strong("Gráfico de Violín"), " son perfectos para comparar una variable numérica (ej. rendimiento) entre diferentes categorías (ej. tratamientos o variedades)."),
                  tags$li(strong("Para mostrar RELACIÓN (2 variables):"), " El ", strong("Gráfico de Dispersión (Scatter Plot)"), " es esencial para ver si dos variables numéricas están correlacionadas (ej. dosis de fertilizante vs. contenido de nitrógeno en hoja).")
              )
          ),
          
          # --- SUB-PESTAÑA 2: LABORATORIO INTERACTIVO ---
          nav_panel(
              title = "4. Laboratorio de Visualización",
              
              h4(class = "section-header", "Constructor de Gráficos Guiado"),
              p(
                  "Esta herramienta interactiva sigue la lógica del diagrama de selección de gráficos. Comienza eligiendo tu objetivo principal (el tipo de gráfico) y luego ajusta las variables para explorar los datos sintéticos con un enfoque agronómico."
              ),
              
              sidebarLayout(
                  sidebarPanel(
                      width = 3,
                      tags$h5("1. Elige tu Objetivo"),
                      selectInput(
                          ns("plot_type"), "Tipo de Gráfico:",
                          choices = c(
                              "Comparación (Columnas)",
                              "Relación (Dispersión)",
                              "Relación (Burbuja)",
                              "Distribución (Histograma)",
                              "Composición Estática (Torta)",
                              "Composición en el Tiempo (Áreas Apiladas)"
                          )
                      ),
                      hr(),
                      tags$h5("2. Ajusta las Variables"),
                      # Controles dinámicos según el tipo de gráfico
                      uiOutput(ns("plot_controls_ui_lab"))
                  ),
                  
                  mainPanel(
                      width = 9,
                      # Texto explicativo dinámico
                      uiOutput(ns("plot_explanation_ui")),
                      hr(),
                      plotly::plotlyOutput(ns("dynamic_plot_output"), height = "450px"),
                      h5(class = "mt-4", "Código `ggplot2` Utilizado:"),
                      verbatimTextOutput(ns("dynamic_code_output"))
                  )
              )
          )
        )
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
  ns <- session$ns

  # =================================================================
  #-- PESTAÑA: 1 Medidas básicas
  # =================================================================

  # --- LÓGICA PARA LA SUB-PESTAÑA "TEORÍA" ---
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
  
  # Reactive que genera los datos para ambos gráficos de dispersión
  dispersion_data <- reactive({
      req(input$sigma)
      mu <- 10
      sigma <- input$sigma
      
      # Datos para la curva de densidad
      curve_df <- data.frame(x = seq(mu - 4*5, mu + 4*5, length.out=500)) %>%
                  mutate(dens = dnorm(x, mean = mu, sd = sigma))
      
      # Simular puntos de datos individuales
      points_df <- data.frame(
          valor = rnorm(150, mean = mu, sd = sigma),
          y_pos = 0 # Para el strip plot
      )
      
      list(curve = curve_df, points = points_df, mu = mu, sigma = sigma)
  })

  # Gráfico de Densidad (Curva)
  output$densPlot <- renderPlot({
      data <- dispersion_data()
      
      ggplot(data$curve, aes(x = x, y = dens)) +
          geom_area(fill = "skyblue", alpha = 0.5) +
          geom_line(color = "navy", size = 1) +
          geom_vline(xintercept = data$mu, linetype = "dashed", color = "red") +
          # FIJAR LOS LÍMITES DEL EJE Y
          coord_cartesian(
              xlim = c(data$mu - 20, data$mu + 20), 
              ylim = c(0, 0.85) # Límite máximo basado en dnorm(0,0,0.5)
          ) +
          labs(
              title = paste0("Forma de la Distribución (σ = ", round(data$sigma, 2), ")"),
              x = "Valor", y = "Densidad"
          ) +
          theme_minimal(base_size = 14)
  })
  
  # Gráfico de Puntos (Strip Plot)
  output$stripPlot <- renderPlot({
      data <- dispersion_data()
      
      ggplot(data$points, aes(x = valor, y = y_pos)) +
          # geom_jitter añade dispersión vertical para evitar sobreposición
          geom_jitter(color = "navy", alpha = 0.6, width = 0, height = 0.5) +
          geom_vline(xintercept = data$mu, linetype = "dashed", color = "red") +
          # Ocultar el eje Y ya que no tiene significado
          theme_minimal(base_size = 14) +
          theme(
              axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank()
          ) +
          coord_cartesian(xlim = c(data$mu - 20, data$mu + 20)) +
          labs(
              title = paste0("Dispersión de los Datos (σ = ", round(data$sigma, 2), ")"),
              x = "Valor", y = ""
          )
  })
  
  # Interpretación (ahora depende de dispersion_data())
  output$dispersion_interpretation <- renderUI({
      data <- dispersion_data()
      cv_pct <- (data$sigma / data$mu) * 100
      
      interpret_text <- if (cv_pct < 15) {
          "Un CV bajo indica que los datos son muy consistentes y están agrupados cerca de la media. ¡Ideal para un experimento controlado!"
      } else if (cv_pct < 30) {
          "Un CV moderado sugiere una variabilidad esperable en muchos ensayos de campo."
      } else {
          "Un CV alto indica una gran dispersión relativa. En agronomía, esto podría significar heterogeneidad en el suelo o un manejo inconsistente que debería investigarse."
      }
      HTML(paste("Con un CV de ", strong(paste0(round(cv_pct, 1), "%")), ",", interpret_text))
  })

  # --- LÓGICA PARA LA SUB-PESTAÑA "LABORATORIO DE DISTRIBUCIONES" ---

  # Reactive que genera los datos del escenario seleccionado
  datos_escenario <- reactive({
      set.seed(123) # Usamos semilla fija para consistencia
      switch(input$escenario,
          "normal"     = rnorm(150, mean = 50, sd = 10),
          "outliers"   = c(rnorm(147, 50, 8), 110, 125, -10),
          "cv_alto"    = rnorm(150, mean = 10, sd = 5), # SD es 50% de la media
          "skew_right" = rgamma(150, shape = 2, scale = 10),
          "skew_left"  = max(rgamma(150, shape=3, scale=15)) - rgamma(150, shape=3, scale=15),
          "bimodal"    = c(rnorm(75, mean = 30, sd = 5), rnorm(75, mean = 70, sd = 5))
      )
  })
  
  # Salida de la tabla de estadísticas
  output$stats_table <- renderTable({
      datos <- datos_escenario()
      req(datos)
      
      data.frame(
          Métrica = c("Media", "Mediana", "Desv. Estándar", "CV (%)", "Asimetría", "Exceso de Curtosis"),
          Valor = round(c(
              mean(datos),
              median(datos),
              sd(datos),
              (sd(datos) / mean(datos)) * 100,
              moments::skewness(datos),
              moments::kurtosis(datos) - 3
          ), 2)
      )
  }, bordered = TRUE)
  
  # Salida del histograma
  output$histPlot_escenario <- renderPlot({
      df <- data.frame(valor = datos_escenario())
      ggplot(df, aes(x=valor)) +
          geom_histogram(bins=20, fill="steelblue", alpha=0.8, color="white") +
          geom_vline(aes(xintercept = mean(valor)), color="red", linetype="dashed", size=1) +
          geom_vline(aes(xintercept = median(valor)), color="green", linetype="dotted", size=1) +
          labs(title="Histograma (Media=Rojo, Mediana=Verde)") +
          theme_bw()
  })
  
  # Salida del boxplot
  output$boxPlot_escenario <- renderPlot({
      df <- data.frame(valor = datos_escenario())
      ggplot(df, aes(y=valor)) +
          geom_boxplot(fill="lightblue", outlier.color="red", outlier.shape=8) +
          labs(title="Boxplot") +
          theme_minimal() +
          theme(axis.title.x=element_blank(), axis.text.x=element_blank())
  })
  
  # Salida de la interpretación del escenario
  output$scenario_interpretation <- renderUI({
      texto <- switch(input$escenario,
          "normal" = "Distribución simétrica y acampanada. La media y la mediana son casi idénticas. Cumple los supuestos para la mayoría de las pruebas paramétricas.",
          "outliers" = "La mayoría de los datos son normales, pero hay valores extremos (puntos rojos). Estos outliers 'arrastran' la media (roja) lejos de la mediana (verde), que es más robusta.",
          "cv_alto" = "Aunque la distribución puede ser simétrica, la dispersión (DE) es muy grande en comparación con la media. En agronomía, esto indica un ensayo con alta variabilidad, lo que dificulta detectar diferencias reales.",
          "skew_right" = "La distribución tiene una cola larga a la derecha (sesgo positivo). La media es mayor que la mediana. Esto es común en datos de conteo (ej. número de plagas). Podría requerir una transformación (ej. logarítmica) para el análisis.",
          "skew_left" = "La distribución tiene una cola larga a la izquierda (sesgo negativo). La media es menor que la mediana. Puede ocurrir en datos de porcentajes cercanos al 100% (ej. % de germinación).",
          "bimodal" = "¡Se ven claramente dos picos! Esto sugiere que tus datos provienen de dos poblaciones mezcladas (ej. rendimiento de dos variedades diferentes analizadas juntas). El primer paso sería separar y analizar estos grupos."
      )
      HTML(texto)
  })

  # --- LÓGICA PARA LA SUB-PESTAÑA "EXPORTAR DATOS" ---
  
  # Reactive que genera los datos para exportar
  datos_para_exportar <- reactive({
      req(input$export_escenario)
      set.seed(123) # Usamos la misma semilla para reproducibilidad
      datos <- switch(input$export_escenario,
          "normal"     = rnorm(150, mean = 50, sd = 10),
          "outliers"   = c(rnorm(147, 50, 8), 110, 125, -10),
          "cv_alto"    = rnorm(150, mean = 10, sd = 5),
          "skew_right" = rgamma(150, shape = 2, scale = 10),
          "skew_left"  = max(rgamma(150, shape=3, scale=15)) - rgamma(150, shape=3, scale=15),
          "bimodal"    = c(rnorm(75, mean = 30, sd = 5), rnorm(75, mean = 70, sd = 5))
      )
      data.frame(valor_simulado = datos)
  })
  
  # Handler para la descarga en CSV
  output$download_data_csv <- downloadHandler(
      filename = function() {
          paste0("datos_", input$export_escenario, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
          write.csv(datos_para_exportar(), file, row.names = FALSE)
      }
  )
  
  # Handler para la descarga en XLSX
  output$download_data_xlsx <- downloadHandler(
      filename = function() {
          paste0("datos_", input$export_escenario, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
          # Se necesita el paquete writexl
          writexl::write_xlsx(datos_para_exportar(), file)
      }
  )

  # =================================================================
  # --- LÓGICA PARA LA PESTAÑA 2: ASIMETRÍA Y CURTOSIS ---
  # =================================================================

  # --- Laboratorio de Asimetría ---
  
  skew_dist_data <- reactive({
      req(input$skew_shape)
      shape <- input$skew_shape
      scale <- 2 # Mantenemos la escala fija
      
      # Generar 10,000 puntos para una estimación estable
      datos_sim <- rgamma(10000, shape = shape, scale = scale)
      
      # Calcular estadísticas
      media <- mean(datos_sim)
      mediana <- median(datos_sim)
      dens <- density(datos_sim)
      moda <- dens$x[which.max(dens$y)]
      asimetria_val <- moments::skewness(datos_sim)
      
      list(
          datos = data.frame(valor = datos_sim),
          media = media,
          mediana = mediana,
          moda = moda,
          asimetria = asimetria_val
      )
  })
  
  output$skew_plot <- renderPlot({
      data <- skew_dist_data()
      
      ggplot(data$datos, aes(x = valor)) +
          geom_density(fill="orange", color="darkred", alpha=0.6) +
          geom_vline(xintercept = data$media, color = "red", linetype="dashed", size=1) +
          geom_vline(xintercept = data$mediana, color = "darkgreen", linetype="dotted", size=1) +
          geom_vline(xintercept = data$moda, color = "blue", linetype="solid", size=1) +
          annotate("text", x = data$media, y = 0, label="Media", color="red", vjust=-0.5, fontface="bold") +
          annotate("text", x = data$mediana, y = 0, label="Mediana", color="darkgreen", vjust=-1.5, fontface="bold") +
          annotate("text", x = data$moda, y = 0, label="Moda", color="blue", vjust=-2.5, fontface="bold") +
          labs(
              title = "Efecto del Sesgo en las Medidas de Tendencia Central",
              subtitle = paste("Asimetría Calculada ≈", round(data$asimetria, 2)),
              x = "Valor", y = "Densidad"
          ) +
          theme_minimal(base_size = 14)
  })
  
  output$skew_stats_text <- renderPrint({
      data <- skew_dist_data()
      cat(
          "Media:    ", round(data$media, 2), "\n",
          "Mediana:  ", round(data$mediana, 2), "\n",
          "Moda:     ", round(data$moda, 2), "\n",
          "Asimetría:", round(data$asimetria, 2)
      )
  })

  # --- Laboratorio de Curtosis ---
  
  kurt_dist_data <- reactive({
      req(input$kurt_df)
      df <- input$kurt_df
      
      # Generar 10,000 puntos para estimar la curtosis
      datos_t <- rt(10000, df = df)
      
      # Calcular exceso de curtosis
      # Para t-dist, kurtosis teórica es 6/(df-4) para df > 4
      curtosis_exceso <- if (df > 4) 6 / (df - 4) else Inf
      
      list(
          df_t = data.frame(valor = datos_t),
          df_norm = data.frame(valor = rnorm(10000)),
          curtosis_exceso_teorica = curtosis_exceso,
          curtosis_exceso_calculada = moments::kurtosis(datos_t) - 3
      )
  })
  
  output$kurt_plot <- renderPlot({
      data <- kurt_dist_data()
      
      ggplot() +
          # Distribución t (Leptocúrtica)
          geom_density(data = data$df_t, aes(x=valor, color="t de Student"), size=1.2) +
          # Distribución Normal (Mesocúrtica) para comparación
          geom_density(data = data$df_norm, aes(x=valor, color="Normal"), linetype="dashed", size=1) +
          scale_color_manual(name="Distribución", values=c("t de Student"="red", "Normal"="black")) +
          coord_cartesian(xlim=c(-5, 5)) +
          labs(
              title = paste("Comparación de Curtosis: t(df=", input$kurt_df, ") vs. Normal"),
              subtitle = paste("Exceso de Curtosis Teórico de la t ≈", round(data$curtosis_exceso_teorica, 2)),
              x = "Valor", y = "Densidad"
          ) +
          theme_minimal(base_size = 14) +
          theme(legend.position = "bottom")
  })
  
  output$kurt_stats_text <- renderPrint({
      data <- kurt_dist_data()
      cat(
          "Exceso de Curtosis (t-student):\n",
          "  - Teórico: ", round(data$curtosis_exceso_teorica, 2), "\n",
          "  - Calculado:", round(data$curtosis_exceso_calculada, 2), "\n\n",
          "Exceso de Curtosis (Normal):\n",
          "  - Teórico: 0"
      )
  })

  # =================================================================
  # --- LÓGICA PARA LA PESTAÑA 3: AGRUPACIÓN ---
  # =================================================================

  # Reactive para los datos de ejemplo
  datos_agrupacion <- eventReactive(input$gen_data_s2, {
      set.seed(Sys.time()) # Nueva semilla cada vez
      data.frame(
          Lote        = factor(rep(1:4, each = 10)),
          Tratamiento = factor(rep(c('Control', 'Fert_A', 'Fert_B', 'Organico'), times = 10)),
          Rendimiento = rnorm(40, mean = 5, sd = 0.8),
          Calidad     = rnorm(40, mean = 7, sd = 1.2)
      )
  }, ignoreNULL = FALSE)
  
  # Mostrar la tabla de datos generados
  output$df_preview_s2 <- DT::renderDataTable({
      DT::datatable(datos_agrupacion(), options = list(pageLength = 5, scrollX = TRUE))
  })

  # Reactive que construye la tabla de resumen dinámicamente
  resumen_dinamico <- reactive({
      df <- datos_agrupacion()
      req(df, input$summary_vars_s2, input$summary_funs_s2)
      
      # Iniciar la cadena de dplyr
      resumen <- if (!is.null(input$group_vars_s2) && length(input$group_vars_s2) > 0) {
          df %>% group_by(across(all_of(input$group_vars_s2)))
      } else {
          df
      }
      
      # Definir la lista de funciones de resumen seleccionadas
      funs_list <- list()
      if ("mean" %in% input$summary_funs_s2) funs_list$media <- ~mean(.x, na.rm = TRUE)
      if ("sd" %in% input$summary_funs_s2) funs_list$sd <- ~sd(.x, na.rm = TRUE)
      if ("cv" %in% input$summary_funs_s2) funs_list$cv <- ~((sd(.x, na.rm=TRUE) / mean(.x, na.rm=TRUE)) * 100)
      
      # Aplicar summarise con las funciones seleccionadas
      resumen <- resumen %>%
          summarise(
              across(all_of(input$summary_vars_s2), funs_list, .names = "{.col}_{.fn}"),
              .groups = 'drop'
          )
          
      # Añadir el conteo 'n' si está seleccionado
      if ("n" %in% input$summary_funs_s2) {
          # Necesitamos calcular 'n' por separado
          n_counts <- if (!is.null(input$group_vars_s2) && length(input$group_vars_s2) > 0) {
              df %>% group_by(across(all_of(input$group_vars_s2))) %>% summarise(n = n(), .groups = 'drop')
          } else {
              df %>% summarise(n = n())
          }
          # Unir los conteos con el resumen
          if (!is.null(input$group_vars_s2) && length(input$group_vars_s2) > 0) {
              resumen <- left_join(resumen, n_counts, by = input$group_vars_s2)
          } else {
              resumen <- bind_cols(resumen, n_counts)
          }
      }
      
      # Redondear todas las columnas numéricas para una mejor presentación
      resumen %>% mutate(across(where(is.numeric), ~round(., 2)))
  })

  # Mostrar la tabla de resumen
  output$summary_table_s2 <- DT::renderDataTable({
      req(resumen_dinamico())
      DT::datatable(resumen_dinamico(), options = list(pageLength = 10, scrollX = TRUE, searching = FALSE))
  })
  
  # Mostrar el gráfico de resumen
  output$summary_plot_s2 <- renderPlot({
      df_resumen <- resumen_dinamico()
      req(df_resumen)
      
      # El gráfico solo tiene sentido si se agrupa por una variable
      # y se calculan media y sd
      grouping_var <- input$group_vars_s2[1] # Tomar la primera variable de agrupación para el eje X
      summary_var <- input$summary_vars_s2[1] # Tomar la primera variable resumida para el eje Y
      
      # Nombres de columna esperados
      mean_col <- paste0(summary_var, "_media")
      sd_col <- paste0(summary_var, "_sd")
      n_col <- "n"
      
      # Validar que las columnas necesarias existan
      validate(
          need(!is.null(grouping_var), "Por favor, selecciona al menos una variable para agrupar."),
          need(all(c(mean_col, sd_col, n_col) %in% names(df_resumen)), 
                "Para graficar, por favor selecciona 'Media', 'Desv. Estándar' y 'Conteo (n)' como estadísticas a calcular.")
      )
      
      # Calcular error estándar
      df_plot <- df_resumen %>%
          mutate(se = .data[[sd_col]] / sqrt(.data[[n_col]]))
      
      ggplot(df_plot, aes_string(x = grouping_var, y = mean_col, fill = grouping_var)) +
          geom_col(alpha = 0.8, show.legend = FALSE) +
          geom_errorbar(aes(ymin = .data[[mean_col]] - se, ymax = .data[[mean_col]] + se), width = 0.2, linewidth=0.8) +
          labs(
              title = paste("Media de", summary_var, "por", grouping_var),
              subtitle = "Las barras de error representan ± 1 Error Estándar (SE)",
              x = grouping_var,
              y = paste("Media de", summary_var)
          ) +
          theme_minimal(base_size = 14)
  })

  # =================================================================
  # LÓGICA COMPARTIDA: Datos Sintéticos para toda la Sesión
  # =================================================================
  # Generamos datos sintéticos una sola vez. Esto asegura que siempre
  # haya datos disponibles y evita problemas de lectura de archivos.
  
  # 1. Crear varios dataframes sintéticos para distintos contextos agronómicos
  # --- Datos Sintéticos para el Laboratorio ---
  df_lab_data <- reactive({
      set.seed(123)
      list(
          comparacion = data.frame(
              tratamiento = factor(rep(c("Control", "N_Bajo", "N_Alto", "Compost"), each=10)),
              rendimiento = c(rnorm(10, 5.0, 0.8), rnorm(10, 6.0, 0.7), rnorm(10, 7.5, 0.9), rnorm(10, 5.5, 0.6))
          ),
          relacion = data.frame(
              dosis_n = runif(50, 0, 200),
              fosforo_ppm = runif(50, 10, 40),
              rendimiento = 2 + 0.02 * runif(50, 0, 200) + 0.05 * runif(50, 10, 40) + rnorm(50, 0, 0.5),
              n_frutos = rpois(50, lambda=20)
          ),
          distribucion = data.frame(
              diametro_tuberculo = rgamma(200, shape=5, scale=10)
          ),
          composicion = data.frame(
              anio = factor(rep(2020:2023, each=3)),
              cultivo = factor(rep(c("Maíz", "Soja", "Trigo"), 4)),
              area_sembrada = abs(rnorm(12, mean=100, sd=20))
          )
      )
  })
  
  # =================================================================
  # --- LÓGICA PARA LA PESTAÑA 4: VISUALIZACIÓN ---
  # =================================================================
  
  # --- B. Lógica para el Laboratorio Interactivo ---
  # --- UI dinámico para los Controles ---
  output$plot_controls_ui_lab <- renderUI({
      ns <- session$ns # Importante para que ns esté disponible
      req(input$plot_type)
      
      df_list <- df_lab_data()
      
      if (input$plot_type == "Comparación (Columnas)") {
          df <- df_list$comparacion
          tagList(
              selectInput(ns("comp_x"), "Variable Categórica (X):", choices = names(df)[sapply(df, is.factor)]),
              selectInput(ns("comp_y"), "Variable Numérica (Y):", choices = names(df)[sapply(df, is.numeric)])
          )
      } else if (input$plot_type %in% c("Relación (Dispersión)", "Relación (Burbuja)")) {
          df <- df_list$relacion
          numeric_cols <- names(df)[sapply(df, is.numeric)]
          tagList(
              selectInput(ns("rel_x"), "Variable Numérica (X):", choices = numeric_cols, selected="dosis_n"),
              selectInput(ns("rel_y"), "Variable Numérica (Y):", choices = numeric_cols, selected="rendimiento"),
              if (input$plot_type == "Relación (Burbuja)") {
                  selectInput(ns("rel_size"), "Variable para Tamaño (Burbuja):", choices = numeric_cols, selected="n_frutos")
              }
          )
      } else if (input$plot_type == "Distribución (Histograma)") {
          df <- df_list$distribucion
          tagList(
              selectInput(ns("dist_var"), "Variable Numérica:", choices = names(df)),
              sliderInput(ns("dist_bins"), "Número de Barras:", min=5, max=50, value=20)
          )
      }
      # Dejamos los de composición sin controles adicionales por simplicidad
  })

  # --- Reactive Principal para generar todo ---
  reactive_plot_bundle <- reactive({
      df_list <- df_lab_data()
      req(input$plot_type)
      
      plot_obj <- ggplot() + theme_void()
      code_string <- "# Selecciona una opción en el panel de control."
      explanation_string <- "Selecciona una opción para ver la explicación."
      
      try({
          # --- LÓGICA DE COMPARACIÓN ---
          if (input$plot_type == "Comparación (Columnas)") {
              req(input$comp_x, input$comp_y)
              df <- df_list$comparacion
              x_var <- input$comp_x; y_var <- input$comp_y
              df_summary <- df %>% group_by(!!sym(x_var)) %>% summarise(media = mean(!!sym(y_var)))
              code_string <- glue::glue("df_summary <- df %>% \n  group_by({x_var}) %>% \n  summarise(media = mean({y_var}))\n\n",
                                        "ggplot(df_summary, aes(x = {x_var}, y = media, fill = {x_var})) +\n  geom_col()")
              plot_obj <- ggplot(df_summary, aes_string(x=x_var, y="media", fill=x_var)) + geom_col(show.legend=F)
              explanation_string <- "Se usa un gráfico de columnas para comparar una cantidad numérica (rendimiento medio) entre diferentes categorías discretas (tratamientos). Es uno de los gráficos más comunes para visualizar los resultados de un ANOVA."
          }
          # --- LÓGICA DE RELACIÓN ---
          else if (input$plot_type == "Relación (Dispersión)") {
              req(input$rel_x, input$rel_y)
              df <- df_list$relacion
              code_string <- glue::glue("ggplot(df, aes(x = {input$rel_x}, y = {input$rel_y})) +\n  geom_point() + geom_smooth()")
              plot_obj <- ggplot(df, aes_string(x=input$rel_x, y=input$rel_y)) + geom_point(alpha=0.7, color="blue") + geom_smooth(method="loess")
              explanation_string <- "El gráfico de dispersión es esencial para visualizar la relación entre dos variables continuas. Ayuda a identificar tendencias (positivas, negativas, no lineales) y posibles correlaciones."
          }
          else if (input$plot_type == "Relación (Burbuja)") {
              req(input$rel_x, input$rel_y, input$rel_size)
              df <- df_list$relacion
              code_string <- glue::glue("ggplot(df, aes(x = {input$rel_x}, y = {input$rel_y})) +\n  geom_point(aes(size = {input$rel_size}), alpha=0.6)")
              plot_obj <- ggplot(df, aes_string(x=input$rel_x, y=input$rel_y)) + geom_point(aes_string(size=input$rel_size), alpha=0.6, color="darkred")
              explanation_string <- "Un gráfico de burbujas añade una tercera dimensión (el tamaño del punto) a un gráfico de dispersión. Es útil para visualizar cómo tres variables numéricas interactúan."
          }
          # --- LÓGICA DE DISTRIBUCIÓN ---
          else if (input$plot_type == "Distribución (Histograma)") {
              req(input$dist_var, input$dist_bins)
              df <- df_list$distribucion
              code_string <- glue::glue("ggplot(df, aes(x = {input$dist_var})) +\n  geom_histogram(bins = {input$dist_bins})")
              plot_obj <- ggplot(df, aes_string(x=input$dist_var)) + geom_histogram(bins=input$dist_bins, fill="seagreen", alpha=0.7)
              explanation_string <- "El histograma es la herramienta fundamental para entender la distribución de una única variable numérica. Nos permite ver si los datos son simétricos, sesgados, o si tienen múltiples picos."
          }
          # --- LÓGICA DE COMPOSICIÓN ---
          else if (input$plot_type == "Composición Estática (Torta)") {
              df_comp <- df_list$comparacion %>% group_by(tratamiento) %>% summarise(total_rend = sum(rendimiento))
              code_string <- "ggplot(df_comp, aes(x = '', y = total_rend, fill = tratamiento)) +\n  geom_col(width=1) + coord_polar(theta='y')"
              plot_obj <- ggplot(df_comp, aes(x="", y=total_rend, fill=tratamiento)) + geom_col(width=1) + coord_polar(theta="y") + theme_void()
              explanation_string <- "El gráfico de torta muestra la proporción de cada componente en un total estático. Aquí, muestra qué proporción del rendimiento total del ensayo aporta cada tratamiento."
          }
          else if (input$plot_type == "Composición en el Tiempo (Áreas Apiladas)") {
              df <- df_list$composicion
              code_string <- "ggplot(df, aes(x = anio, y = area_sembrada, fill = cultivo)) +\n  geom_area()"
              plot_obj <- ggplot(df, aes(x=anio, y=area_sembrada, fill=cultivo)) + geom_area()
              explanation_string <- "Un gráfico de áreas apiladas muestra cómo cambia la composición de un total a lo largo del tiempo. Es ideal para visualizar cómo ha evolucionado la proporción de diferentes cultivos en una región."
          }
          
          plot_obj <- plot_obj + theme_bw(base_size = 14) + labs(title = input$plot_type)
      })
      
      list(plot = plot_obj, code = code_string, explanation = explanation_string)
  })
  
  # --- Renderizar las Salidas Finales ---
  output$dynamic_plot_output <- plotly::renderPlotly({
      # CORRECCIÓN: Llamar al reactive con paréntesis
      res <- reactive_plot_bundle(); req(res, res$plot)
      if(inherits(res$plot, "ggplot")) plotly::ggplotly(res$plot)
  })
  
  output$dynamic_code_output <- renderText({
      # CORRECCIÓN: Llamar al reactive con paréntesis
      res <- reactive_plot_bundle(); req(res)
      res$code
  })
  
  output$plot_explanation_ui <- renderUI({
      # CORRECCIÓN: Llamar al reactive con paréntesis
      res <- reactive_plot_bundle(); req(res)
      tags$div(class="alert alert-light",
          tags$h6("Uso Agronómico:"),
          p(res$explanation)
      )
  })

  # =================================================================
  # -- PESTAÑA: 5 Interpretación
  # =================================================================

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
