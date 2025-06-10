# R/modules/session8.R

# UI para la Sesión 8
session8UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3(class = "session-title", "Sesión 8: Poder Estadístico y Tamaño de Muestra"),
        
        navset_tab(
            # ===== PESTAÑA 1: ¿Por qué Planificar? =====
            nav_panel(
                title = "1. ¿Por qué Planificar?",
                
                tags$h4(class = "section-header", "1.1 Más Allá del P-valor: La Anatomía de una Decisión Estadística"),
                tags$p(
                    "Durante todo el curso, hemos aprendido a analizar datos para llegar a una decisión: 'rechazar' o 'no rechazar' la hipótesis nula \\(\\(H_0\\)). Sin embargo, esta decisión se toma con información incompleta (una muestra) y siempre existe el riesgo de equivocarse. La calidad de nuestro experimento determinará cuán a menudo tomamos la decisión correcta."
                ),
                tags$p(
                    "Hay cuatro posibles resultados cuando comparamos nuestra decisión con la realidad desconocida del campo, que se pueden resumir en esta matriz:"
                ),
                
                # --- Matriz Visual de Errores ---
                tags$div(class="row justify-content-center",
                    tags$div(class="col-md-10",
                        tags$table(class="table table-bordered text-center", style="font-size: 1.1em;",
                            tags$thead(
                                tags$tr(
                                    tags$th(style="width: 25%;", ""),
                                    tags$th(colspan="2", "Realidad en el Campo (Verdad Desconocida)")
                                ),
                                tags$tr(
                                    tags$th("Nuestra Decisión Estadística"),
                                    tags$th("H₀ es Verdadera (No hay efecto)"),
                                    tags$th("H₀ es Falsa (Sí hay un efecto real)")
                                )
                            ),
                            tags$tbody(
                                tags$tr(
                                    tags$td(strong("No Rechazamos H₀")),
                                    tags$td(class="table-success", strong("Decisión Correcta"), br(), "(Prob = 1 - α)"),
                                    tags$td(class="table-warning", strong("Error Tipo II (β)"), br(), "Falso Negativo")
                                ),
                                tags$tr(
                                    tags$td(strong("Rechazamos H₀")),
                                    tags$td(class="table-danger", strong("Error Tipo I (α)"), br(), "Falso Positivo"),
                                    tags$td(class="table-success", strong("Decisión Correcta (Poder)"), br(), "(Prob = 1 - β)")
                                )
                            )
                        )
                    )
                ),

                # --- Desglose de los Errores con Analogías ---
                fluidRow(
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-header bg-danger text-white", strong("Error Tipo I (α) - La Falsa Alarma")),
                            tags$div(class="card-body",
                                p(strong("Definición:"), " Concluimos que un nuevo fertilizante funciona, pero en realidad no tiene ningún efecto sobre el control."),
                                p(strong("Consecuencia Agronómica:"), " Un agricultor invierte dinero y tiempo en un producto inútil basado en nuestra recomendación. Perdemos credibilidad y recursos económicos."),
                                p("Generalmente fijamos \\(\\alpha\\) en 0.05, aceptando un 5% de riesgo de cometer este error.")
                            )
                        )
                    ),
                    column(6,
                        tags$div(class="card h-100",
                            tags$div(class="card-header bg-warning text-dark", strong("Error Tipo II (β) - La Oportunidad Perdida")),
                            tags$div(class="card-body",
                                p(strong("Definición:"), " Concluimos que una nueva variedad resistente no es mejor que la antigua, cuando en realidad sí lo era."),
                                p(strong("Consecuencia Agronómica:"), " Una innovación valiosa (que podría haber aumentado rendimientos o reducido el uso de pesticidas) es descartada. Se frena el progreso y la adopción de mejores tecnologías."),
                                p("Este es el error que combatimos con el análisis de poder.")
                            )
                        )
                    )
                ),

                tags$hr(),

                tags$h4(class = "section-header", "1.2 Definiendo el Poder Estadístico: La Probabilidad de Acertar"),
                tags$p(
                    "El ", strong("Poder Estadístico (Statistical Power)"), " es la probabilidad de que nuestro experimento sea lo suficientemente sensible para detectar un efecto real. Es la celda verde inferior derecha de nuestra matriz: la probabilidad de tomar la decisión correcta cuando realmente hay algo que descubrir."
                ),
                withMathJax(helpText(
                    "$$\\text{Poder} = 1 - \\beta = P(\\text{Rechazar } H_0 \\mid H_0 \\text{ es Falsa})$$"
                )),

                # --- Gráfico Conceptual del Poder ---
                fluidRow(
                    column(8,
                        plotOutput(ns("power_concept_plot"), height="300px")
                    ),
                    column(4,
                        tags$h6("Interpretando el Gráfico"),
                        tags$ul(
                            tags$li(strong("Curva Azul (H₀):"), " Representa la distribución de los resultados si no hubiera efecto."),
                            tags$li(strong("Curva Roja (H₁):"), " Representa la distribución si SÍ hay un efecto real."),
                            tags$li(strong("α (Rojo):"), " Es el área bajo la curva azul que cortamos para rechazar H₀. Si un resultado cae aquí, lo consideramos 'significativo', pero hay una pequeña probabilidad de que viniera de H₀ (un falso positivo)."),
                            tags$li(strong("β (Amarillo):"), " Es el área bajo la curva roja que se solapa con la zona de 'no rechazo' de H₀. Si un resultado cae aquí, no lo consideramos significativo, aunque en realidad provenía de un efecto real (un falso negativo)."),
                            tags$li(strong("Poder (Verde):"), " Es el resto del área bajo la curva roja. La probabilidad de que un resultado de un efecto real caiga en la zona de rechazo, permitiéndonos detectarlo correctamente.")
                        )
                    )
                ),
                
                tags$h4(class = "section-header", "1.3 Las Consecuencias de un Experimento con Bajo Poder"),
                tags$p(
                    "Realizar un análisis de poder ", em("a priori"), " (antes de empezar) no es un lujo, es una responsabilidad ética y científica. Un experimento con bajo poder (ej. menos del 80%) es problemático porque:",
                    tags$ul(
                        tags$li("Es un ", strong("desperdicio de recursos:"), " se invierte tiempo, dinero, tierra y mano de obra en un estudio que tiene una alta probabilidad de no encontrar nada, incluso si hay algo que encontrar."),
                        tags$li("Produce ", strong("resultados no concluyentes:"), " un resultado no significativo (p > 0.05) en un estudio de bajo poder es ambiguo. ¿Significa que no hay efecto, o que nuestro experimento no fue lo suficientemente bueno para detectarlo?"),
                        tags$li("Tiene ", strong("implicaciones éticas,"), " especialmente en investigación con animales o recursos limitados, ya que se están utilizando sujetos o insumos en un experimento con pocas posibilidades de generar conocimiento útil.")
                    )
                )
            ),

            # ===== PESTAÑA 2: Los 4 Ingredientes del Poder (VERSIÓN MEJORADA) =====
            nav_panel(
                title = "2. Los 4 Ingredientes del Poder",
                
                tags$h4(class = "section-header", "2.1 La Ecuación del Poder: Un Juego de Equilibrio"),
                tags$p(
                    "El poder estadístico no es un valor fijo, sino el resultado de un delicado equilibrio entre cuatro componentes. Imagina una balanza: si cambias el peso de un lado, debes ajustar los otros para mantener el equilibrio. De la misma forma, en la planificación de un experimento, si fijas tres de estos componentes, el cuarto queda determinado. Estos son los cuatro 'pesos' de nuestra balanza:"
                ),

                # --- Visualización de los 4 componentes ---
                fluidRow(
                    column(3, div(class="text-center card h-100", div(class="card-body", icon("arrows-alt-h", "fa-3x text-primary"), h5("Tamaño del Efecto"), p(class="text-muted", "La magnitud de la diferencia que buscas.")))),
                    column(3, div(class="text-center card h-100", div(class="card-body", icon("users", "fa-3x text-success"), h5("Tamaño de Muestra"), p(class="text-muted", "El número de replicaciones.")))),
                    column(3, div(class="text-center card h-100", div(class="card-body", icon("exclamation-triangle", "fa-3x text-danger"), h5("Nivel de Significancia (α)"), p(class="text-muted", "Tu tolerancia a los falsos positivos.")))),
                    column(3, div(class="text-center card h-100", div(class="card-body", icon("bolt", "fa-3x text-warning"), h5("Poder Estadístico (1-β)"), p(class="text-muted", "Tu confianza en detectar un efecto real."))))
                ),
                
                tags$p(class="mt-3",
                "El objetivo del ", strong("análisis de poder a priori"), " es el más común en la planificación: fijamos los niveles deseados de α (generalmente 0.05) y Poder (generalmente 0.80), estimamos el Tamaño del Efecto que nos interesa, y la ecuación nos devuelve el ", strong("Tamaño de Muestra (n) que necesitamos.")
                ),

                tags$hr(),

                tags$h4(class = "section-header", "2.2 El Reto Central: Estimar el Tamaño del Efecto (", em("f"), " de Cohen)"),
                tags$p(
                    "De los cuatro ingredientes, el tamaño del efecto es el más abstracto y el más difícil de determinar antes de realizar el experimento. Representa qué tan 'fuerte' es la señal que esperamos encontrar. Un efecto grande es como un grito en una biblioteca (fácil de oír), mientras que un efecto pequeño es como un susurro en un concierto (difícil de oír). Para ANOVA, usamos la métrica ", strong("f de Cohen."), " Hay dos enfoques prácticos para estimarlo:"
                ),

                # --- Dos enfoques para estimar f ---
                navset_card_pill(
                    header = tags$h6("Selecciona un enfoque para estimar el tamaño del efecto:"),
                    
                    nav_panel(
                        "A) A partir de Estudios Previos",
                        p("Este es el método preferido si existe literatura o datos piloto."),
                        p("Si un estudio similar reportó un ", strong("Eta-cuadrado (\\(\\eta^2\\))"), ", que es la proporción de varianza explicada por los tratamientos, podemos convertirlo directamente a la 'f' de Cohen."),
                        withMathJax(helpText("$$f = \\sqrt{\\frac{\\eta^2}{1 - \\eta^2}}$$")),
                        tags$div(class="note-cloud",
                            strong("Ejemplo:"), " Un ensayo piloto sobre nuevas variedades encontró que el factor 'variedad' explicaba el 12% de la varianza en el rendimiento (\\(\\eta^2 = 0.12\\)).",
                            p("$$f = \\sqrt{\\frac{0.12}{1 - 0.12}} = \\sqrt{\\frac{0.12}{0.88}} \\approx \\sqrt{0.136} \\approx 0.37$$"),
                            "Este sería un efecto considerado 'grande', lo que nos dice que probablemente no necesitaremos un tamaño de muestra masivo para detectarlo."
                        )
                    ),
                    nav_panel(
                        "B) A partir de la Diferencia Mínima Relevante",
                        p("Este enfoque es más práctico y se basa en la experiencia agronómica. Nos preguntamos: ", strong("¿Cuál es la diferencia más pequeña entre tratamientos que me importaría detectar desde un punto de vista práctico o económico?")),
                        p("Para calcular 'f' de esta manera, necesitamos tres piezas de información:"),
                        tags$ol(
                            tags$li("Las medias esperadas de nuestros grupos (\\(\\mu_i\\))."),
                            tags$li("Una estimación de la desviación estándar dentro de cada grupo (\\(\\sigma\\)), a menudo obtenida de ensayos previos."),
                            tags$li("El número de grupos (\\(k\\)).")
                        ),
                        withMathJax(helpText("$$f = \\frac{\\sigma_m}{\\sigma} \\quad \\text{donde} \\quad \\sigma_m = \\sqrt{\\frac{\\sum_{i=1}^{k} (\\mu_i - \\bar{\\mu})^2}{k}}$$"
                        )),
                        tags$div(class="note-cloud",
                            strong("Ejemplo:"), " Queremos comparar 3 fertilizantes (k=3). Un fertilizante nuevo cuesta más, por lo que solo nos interesa si aumenta el rendimiento en al menos 0.5 t/ha. El control tiene una media de 5 t/ha. Esperamos que los otros dos sean similares al nuevo. Por datos históricos, sabemos que la desviación estándar (\\(\\sigma\\)) del rendimiento es de 0.8 t/ha.",
                            tags$ul(
                                tags$li("Medias esperadas (\\(\\mu_i\\)): {5.0, 5.5, 5.5}"),
                                tags$li("Media general (\\(\\bar{\\mu}\\)): (5.0 + 5.5 + 5.5) / 3 ≈ 5.33"),
                                tags$li("Desviación estándar de las medias (\\(\\sigma_m\\)): ", code("sqrt(((5.0-5.33)² + (5.5-5.33)² + (5.5-5.33)²)/3) ≈ 0.236")),
                                tags$li("Cálculo de 'f': ", code("f = 0.236 / 0.8 ≈ 0.295"))
                            ),
                            "Este valor de 'f' (cercano a 0.25) corresponde a un efecto 'mediano'."
                        )
                    )
                ),

                tags$hr(),
                tags$h4(class="section-header", "2.3 Puntos de Referencia para el Tamaño del Efecto"),
                tags$p("Cuando no hay información previa, Jacob Cohen (1988) propuso unas guías convencionales para la 'f' en ANOVA, que deben usarse con precaución pero sirven como un punto de partida:"),
                tags$table(class="table table-bordered text-center",
                    tags$thead(tags$tr(tags$th("Tamaño del Efecto"), tags$th("Valor de 'f' de Cohen"), tags$th("Interpretación Práctica"))),
                    tags$tbody(
                        tags$tr(tags$td("Pequeño"), tags$td(strong("0.10")), tags$td("Una diferencia sutil, difícil de percibir. Requerirá un tamaño de muestra grande para ser detectada.")),
                        tags$tr(tags$td("Mediano"), tags$td(strong("0.25")), tags$td("Una diferencia lo suficientemente grande como para ser visible a simple vista para un observador cuidadoso.")),
                        tags$tr(tags$td("Grande"), tags$td(strong("0.40")), tags$td("Una diferencia grande y obvia. Requerirá un tamaño de muestra relativamente pequeño para ser detectada."))
                    )
                )
            ),

            # ===== PESTAÑA 3: Calculadora de Poder y Muestra =====
            nav_panel(
                title = "3. Calculadora de Poder y Muestra",
                tags$h4(class = "section-header", "Planificación Interactiva para ANOVA de una vía (DCA)"),
                tags$p(
                    "Esta calculadora te permite realizar un análisis de poder ", em("a priori"), " para determinar el tamaño de muestra necesario para tu experimento. Manipula los 'ingredientes' del poder y observa el impacto directo en tus requerimientos de replicaciones y en la curva de poder."
                ),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 4,
                        tags$h5("Parámetros de Planificación"),
                        
                        # --- Panel 1: Parámetros del Experimento ---
                        sliderInput(ns("power_k"), "Número de Tratamientos a Comparar (k):", 
                                    min = 2, max = 10, value = 4, step = 1),
                        
                        # --- Panel 2: Definición del Tamaño del Efecto ---
                        navset_card_pill(
                            id = ns("f_method_selector"),
                            nav_panel("Por Convención",
                                selectInput(ns("power_f_conv"), "Tamaño del Efecto Esperado (f):", 
                                            choices = c("Pequeño (f=0.10)" = 0.10, 
                                                        "Mediano (f=0.25)" = 0.25,
                                                        "Grande (f=0.40)" = 0.40),
                                            selected = 0.25)
                            ),
                            nav_panel("Por Medias",
                                p(class="text-muted", "Define la diferencia mínima relevante."),
                                textInput(ns("power_means"), "Medias Esperadas (separadas por coma)", value = "5, 5.5, 5.5, 5"),
                                numericInput(ns("power_sd"), "Desv. Estándar Dentro del Grupo (σ)", value = 0.8, min = 0.1)
                            )
                        ),
                        
                        # --- Panel 3: Parámetros de la Prueba ---
                        sliderInput(ns("power_alpha"), "Nivel de Significancia (α):", 
                                    min = 0.01, max = 0.10, value = 0.05, step = 0.01),
                        sliderInput(ns("power_target"), "Poder Deseado (1-β):", 
                                    min = 0.70, max = 0.99, value = 0.80, step = 0.01)
                    ),
                    mainPanel(
                        width = 8,
                        # --- Tarjeta de Resultados ---
                        tags$div(class="card mb-4",
                            tags$div(class="card-header bg-primary text-white", strong("Resumen de la Planificación y Resultado")),
                            tags$div(class="card-body",
                                fluidRow(
                                    column(6,
                                        tags$h6("Parámetros de Entrada:"),
                                        textOutput(ns("summary_text"))
                                    ),
                                    column(6, style="border-left: 1px solid #ddd; text-align: center;",
                                        tags$h6("Replicaciones Necesarias (n)"),
                                        tags$p("Por cada tratamiento, necesitas aproximadamente:"),
                                        tags$p(style="font-size: 3.5em; font-weight: bold; color: #0d6efd;", textOutput(ns("power_result_n"))),
                                        tags$p(class="text-muted", textOutput(ns("total_n_text")))
                                    )
                                )
                            )
                        ),
                        
                        # --- Curva de Poder ---
                        tags$h5("Curva de Poder"),
                        tags$p("Este gráfico muestra cómo el poder de tu experimento aumenta a medida que añades más replicaciones. Observa la 'ley de rendimientos decrecientes': las primeras replicaciones aportan mucho poder, pero el beneficio disminuye a medida que el poder se acerca al 100%."),
                        plotOutput(ns("power_curve_plot"))
                    )
                )
            ),
            
            # ===== PESTAÑA 4: Poder para Diseños Avanzados =====
            nav_panel(
                title = "4. Poder para Diseños Avanzados",
                
                tags$h4(class = "section-header", "4.1 Limitaciones de las Fórmulas Simples"),
                tags$p(
                    "La calculadora que exploramos en la pestaña anterior es perfecta para un ANOVA de una vía (DCA). Sin embargo, en cuanto introducimos bloques (DBCA) o múltiples factores (Factoriales, Split-Plot), la ecuación del poder se complica enormemente. ¿Por qué? Porque ahora la varianza se particiona en más componentes, y el 'error' que se usa como denominador en las pruebas F cambia dependiendo del efecto que se esté probando (Stroup, 1989)."
                ),
                tags$p(
                    "Calcular el poder para estos diseños requiere herramientas más sofisticadas. A continuación, exploramos las dos soluciones más comunes y potentes."
                ),

                hr(),
                
                # --- G*Power ---
                tags$h4(class = "section-header", "4.2 Solución 1: Software Especializado (G*Power)"),
                tags$p(
                    "G*Power es un programa gratuito y el estándar de facto en muchas disciplinas para realizar análisis de poder. Su gran ventaja es que tiene menús pre-configurados para una gran variedad de pruebas estadísticas, incluyendo ANOVA con múltiples factores."
                ),
                tags$h5("Ejemplo: Cálculo de Poder para un ANOVA Factorial en G*Power"),
                tags$p(
                    "Imagina que quieres planificar un experimento factorial 2x3. En G*Power, seguirías estos pasos:"
                ),
                tags$div(class="row align-items-center",
                    tags$div(class="col-md-6",
                        tags$ol(
                            tags$li(strong("Test family:"), " Seleccionas 'F tests'."),
                            tags$li(strong("Statistical test:"), " Eliges 'ANOVA: Fixed effects, special, main effects and interactions'."),
                            tags$li(strong("Type of power analysis:"), " Eliges 'A priori: Compute required sample size'."),
                            tags$li(strong("Input Parameters:"), " Aquí introduces tus 'ingredientes':"),
                            tags$ul(
                                # ===== CORRECCIÓN APLICADA AQUÍ =====
                                tags$li(tags$span(code("Effect size f"), ": Lo estimas como aprendimos en la Pestaña 2.")),
                                tags$li(tags$span(code("α err prob"), ": Tu alfa (ej. 0.05).")),
                                tags$li(tags$span(code("Power (1-β err prob)"), ": Tu poder deseado (ej. 0.80).")),
                                tags$li(tags$span(code("Numerator df"), ": Los grados de libertad del efecto que te interesa (ej. para la interacción AxB, sería (a-1)(b-1)).")),
                                tags$li(tags$span(code("Number of groups"), ": El número total de combinaciones de tratamiento (ej. 2x3 = 6 grupos)."))
                            ),
                            tags$li("Haces clic en ", strong("Calculate"), " y G*Power te dará el 'Total sample size' necesario.")
                        )
                    ),
                    tags$div(class="col-md-6",
                        tags$img(src="images/gpower_screenshot.png", class="img-fluid border rounded", 
                                alt="Captura de pantalla de la interfaz de G*Power para ANOVA")
                    )
                ),
                
                tags$hr(),

                # --- Simulaciones ---
                tags$h4(class = "section-header", "4.3 Solución 2: Simulaciones de Monte Carlo (El Enfoque Universal)"),
                tags$p(
                    "Para los diseños más complejos, como los Split-Plot o modelos con efectos aleatorios, a menudo ni siquiera G*Power es suficiente. En estos casos, el enfoque más flexible y potente es ", strong("estimar el poder mediante simulación."), " La lógica es sorprendentemente intuitiva:"
                ),
                
                tags$div(class="text-center",
                    strong("Diagrama de Flujo de una Simulación de Poder")
                ),
                # Diagrama de flujo
                # (Esto podría ser un gráfico más elegante, pero el texto funciona bien)
                tags$div(class="card-deck text-center mt-3",
                    tags$div(class="card", div(class="card-body", "1. Define tu Hipótesis (ej. Medias, DE, efectos esperados)")),
                    tags$i(class="bi bi-arrow-right align-self-center fa-2x"),
                    tags$div(class="card", div(class="card-body", "2. Genera un dataset falso basado en esa hipótesis")),
                    tags$i(class="bi bi-arrow-right align-self-center fa-2x"),
                    tags$div(class="card", div(class="card-body", "3. Analiza el dataset falso y guarda el p-valor")),
                    tags$i(class="bi bi-arrow-right align-self-center fa-2x"),
                    tags$div(class="card", div(class="card-body", "4. Repite los pasos 2 y 3 miles de veces (ej. 5000 veces)")),
                    tags$i(class="bi bi-arrow-right align-self-center fa-2x"),
                    tags$div(class="card bg-success text-white", div(class="card-body", "5. El Poder es la proporción de veces que el p-valor fue < α"))
                ),
                
                tags$p(class="mt-3", "Aunque escribir el código para una simulación requiere más esfuerzo, te da un control total y te permite calcular el poder para ", em("cualquier"), " diseño experimental, sin importar cuán complejo sea. Paquetes de R como ", code("Superpower"), " automatizan este proceso para diseños factoriales."),

                tags$h5("Ejemplo Conceptual de Código para una Simulación de Poder"),
                tags$pre(class="r-code",
                    htmltools::HTML(
                        "# NO EJECUTAR - SOLO CONCEPTUAL\n\n",
                        "# 1. Definir parámetros\n",
                        "n_simulaciones <- 5000\n",
                        "n_por_grupo <- 15 # El tamaño de muestra que queremos probar\n",
                        "efecto_real <- 0.5 # La diferencia que esperamos\n",
                        "p_valores <- numeric(n_simulaciones) # Vector para guardar resultados\n\n",
                        "# 4. Bucle de simulación\n",
                        "for (i in 1:n_simulaciones) {\n",
                        "  # 2. Generar datos falsos\n",
                        "  datos_falsos <- data.frame(\n",
                        "    respuesta = c(rnorm(n_por_grupo, mean = 0, sd = 1),\n",
                        "                  rnorm(n_por_grupo, mean = efecto_real, sd = 1)),\n",
                        "    grupo = factor(rep(c('Control', 'Tratado'), each = n_por_grupo))\n",
                        "  )\n\n",
                        "  # 3. Analizar y guardar p-valor\n",
                        "  modelo <- t.test(respuesta ~ grupo, data = datos_falsos)\n",
                        "  p_valores[i] <- modelo$p.value\n",
                        "}\n\n",
                        "# 5. Calcular el poder\n",
                        "poder_estimado <- mean(p_valores < 0.05)\n",
                        "print(paste('Poder estimado para n =', n_por_grupo, 'es:', poder_estimado))"
                    )
                ),
                
                # --- Tabla Resumen ---
                tags$hr(),
                tags$h4(class="section-header", "4.4 Guía Rápida: ¿Qué Herramienta Usar?"),
                tags$table(class="table table-striped", 
                    tags$thead(
                        tags$tr(tags$th("Diseño Experimental"), tags$th("Herramienta de Poder Recomendada"))
                    ),
                    tags$tbody(
                        tags$tr(tags$td("DCA / ANOVA de una vía"), tags$td(tagList(code("pwr::pwr.anova.test"), " o G*Power"))),
                        tags$tr(tags$td("DBCA"), tags$td("G*Power (usando ANOVA de una vía y ajustando 'n' para compensar los gl perdidos) o Simulación.")),
                        tags$tr(tags$td("Factorial Completo"), tags$td(tagList("G*Power o el paquete de R ", code("Superpower"), " (que se basa en simulación)."))),
                        tags$tr(tags$td("Split-Plot / Diseños Mixtos"), tags$td(strong("Simulación de Monte Carlo."), " Es el único método verdaderamente fiable y flexible para estos diseños."))
                    )
                )
            ),

            # ===== PESTAÑA 5: Referencias =====
            nav_panel(
                title = "Referencias",
                tags$ul(
                    tags$li("Cohen, J. (1988). ", tags$em("Statistical power analysis for the behavioral sciences"), " (2nd ed.). Lawrence Erlbaum Associates."),
                    tags$li("Cohen, J. (1992). A power primer. ", tags$em("Psychological Bulletin, 112"),"(1), 155–159."),
                    tags$li("Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: A flexible statistical power analysis program for the social, behavioral, and biomedical sciences. ", tags$em("Behavior Research Methods, 39"), ", 175-191."),
                    tags$li("Faul, F., Erdfelder, E., Lang, A.-G., & Buchner, A. (2007). G*Power 3: A flexible statistical power analysis program for the social, behavioral, and biomedical sciences. ", tags$em("Behavior Research Methods, 39"), ", 175-191."),
                    tags$li("Lakens, D., & Caldwell, A. R. (2021). Simulation-based power analysis for factorial analysis of variance designs. ", tags$em("Advances in Methods and Practices in Psychological Science, 4"),"(1). ", tags$a(href="https://doi.org/10.1177/2515245920951503", "https://doi.org/10.1177/2515245920951503"), " (Base para el paquete Superpower)."),
                    tags$li("Stroup, W. W. (1989). Power analysis for detecting trends in the presence of serial correlation. En ", tags$em("Proceedings of the Biopharmaceutical Section of the American Statistical Association"), " (Vol. 2, pp. 110-115). (Artículo clásico que discute la complejidad del poder en modelos con estructuras de error complejas).")
                )
            )
        )
    )
}

# Server para la Sesión 6
session8Server <- function(input, output, session) {
    ns <- session$ns
    # --- LÓGICA PARA LA PESTAÑA 1: GRÁFICO CONCEPTUAL DE PODER ---
    output$power_concept_plot <- renderPlot({
        
        # Parámetros para la visualización
        media_h0 <- 0
        media_h1 <- 1.5
        sd_comun <- 1
        
        # Valor crítico para alfa = 0.05 (una cola)
        valor_critico <- qnorm(0.95, mean = media_h0, sd = sd_comun)
        
        # Crear la secuencia de x para el gráfico
        x_vals <- seq(media_h0 - 4*sd_comun, media_h1 + 4*sd_comun, length.out = 400)
        
        # Calcular las densidades
        densidad_h0 <- dnorm(x_vals, mean = media_h0, sd = sd_comun)
        densidad_h1 <- dnorm(x_vals, mean = media_h1, sd = sd_comun)
        
        df_plot <- data.frame(x = x_vals, h0 = densidad_h0, h1 = densidad_h1)
        
        ggplot(df_plot, aes(x = x)) +
            # Rellenar áreas
            geom_ribbon(data = subset(df_plot, x > valor_critico),
                        aes(ymin = 0, ymax = h0), fill = "red", alpha = 0.5) + # Alfa
            geom_ribbon(data = subset(df_plot, x < valor_critico),
                        aes(ymin = 0, ymax = h1), fill = "orange", alpha = 0.5) + # Beta
            geom_ribbon(data = subset(df_plot, x > valor_critico),
                        aes(ymin = 0, ymax = h1), fill = "green", alpha = 0.5) + # Poder
            
            # Dibujar las curvas de distribución
            geom_line(aes(y = h0), color = "blue", linewidth = 1.2) +
            geom_line(aes(y = h1), color = "red", linewidth = 1.2) +
            
            # Línea crítica
            geom_vline(xintercept = valor_critico, linetype = "dashed", linewidth = 1) +
            
            # Anotaciones
            annotate("text", x = media_h0, y = 0.25, label = "H₀: No hay efecto", color = "blue", size=5) +
            annotate("text", x = media_h1, y = 0.25, label = "H₁: Sí hay efecto", color = "red", size=5) +
            annotate("text", x = valor_critico + 0.7, y = 0.05, label = "α", size=6, color="darkred") +
            annotate("text", x = valor_critico - 0.7, y = 0.1, label = "β", size=6, color="darkorange") +
            annotate("text", x = valor_critico + 1.5, y = 0.15, label = "Poder (1-β)", size=6, color="darkgreen") +
            
            # Tema y etiquetas
            labs(title = "Visualización del Poder Estadístico",
                 x = "Resultado del Estadístico de Prueba (ej. valor F)",
                 y = "Densidad de Probabilidad") +
            theme_minimal(base_size = 14) +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })

    # --- LÓGICA PARA LA PESTAÑA 3: CALCULADORA DE PODER ---
    
    # Reactive para calcular 'f' de Cohen
    f_calculada <- reactive({
        if (input$f_method_selector == "Por Convención") {
            # Si se usa la convención, simplemente toma el valor del select input
            req(input$power_f_conv)
            return(as.numeric(input$power_f_conv))
        } else {
            # Si se calcula por medias
            req(input$power_means, input$power_sd)
            # Procesar el string de medias
            medias_vec <- as.numeric(unlist(strsplit(input$power_means, ",")))
            
            # Validar que el número de medias coincida con k
            validate(
                need(length(medias_vec) == input$power_k, 
                     paste("Error: Proporcionaste", length(medias_vec), "medias, pero k =", input$power_k))
            )
            
            # Calcular 'f'
            media_general <- mean(medias_vec)
            sigma_m <- sqrt(sum((medias_vec - media_general)^2) / length(medias_vec))
            f_val <- sigma_m / input$power_sd
            return(f_val)
        }
    })
    
    # Reactive principal que hace el cálculo de poder
    power_calculation <- reactive({
        req(input$power_k, f_calculada(), input$power_alpha, input$power_target)
        
        tryCatch({
            pwr::pwr.anova.test(
                k = input$power_k,
                f = f_calculada(),
                sig.level = input$power_alpha,
                power = input$power_target
            )
        }, error = function(e) {
            list(n = NA, error_msg = "No se puede alcanzar el poder deseado.")
        })
    })
    
    # Salidas para la tarjeta de resumen
    output$summary_text <- renderText({
        paste0(
            "Número de grupos (k): ", input$power_k, "\n",
            "Tamaño del efecto (f): ", round(f_calculada(), 3), "\n",
            "Nivel de significancia (α): ", input$power_alpha, "\n",
            "Poder deseado (1-β): ", input$power_target
        )
    })
    
    output$power_result_n <- renderText({
        calc <- power_calculation()
        if (!is.na(calc$n)) ceiling(calc$n) else "Inalcanzable"
    })
    
    output$total_n_text <- renderText({
        calc <- power_calculation()
        if (!is.na(calc$n)) {
            total_n <- ceiling(calc$n) * input$power_k
            paste0("(Total de ", total_n, " unidades experimentales)")
        } else {
            ""
        }
    })
    
    # Generar la curva de poder
    output$power_curve_plot <- renderPlot({
        f_val <- f_calculada()
        req(input$power_k, f_val, input$power_alpha, input$power_target)
        
        n_max_plot <- 200 # Límite superior para el gráfico, ajustable
        calc_n <- power_calculation()$n
        if (!is.na(calc_n) && calc_n > n_max_plot) {
             n_max_plot <- ceiling(calc_n) * 1.5 # Ajustar el eje si n es muy grande
        }
        if (n_max_plot > 1000) n_max_plot <- 1000 # Poner un límite final
        
        n_sequence <- seq(2, n_max_plot, by = 1)
        
        power_values <- sapply(n_sequence, function(n_i) {
            tryCatch(pwr::pwr.anova.test(k=input$power_k, f=f_val, sig.level=input$power_alpha, n=n_i)$power,
                     error = function(e) NA)
        })
        
        df_power <- data.frame(n = n_sequence, power = power_values) %>% na.omit()
        n_necesario <- if(!is.na(calc_n)) ceiling(calc_n) else NA
        
        ggplot(df_power, aes(x = n, y = power)) +
            geom_line(color = "blue", linewidth = 1.2) +
            geom_hline(yintercept = input$power_target, linetype = "dashed", color = "red") +
            geom_vline(xintercept = n_necesario, linetype = "dotted", color = "darkgreen") +
            geom_point(aes(x = n_necesario, y = input$power_target), color = "darkgreen", size = 4, shape = 18) +
            annotate("text", x = n_max_plot, y = input$power_target, label = paste0("Poder Objetivo: ", round(input$power_target*100),"%"), color = "red", hjust = 1.05, vjust = -0.5) +
            annotate("text", x = n_necesario, y = 0.1, label = paste0("n ≈ ", n_necesario), color = "darkgreen", hjust = -0.1) +
            scale_y_continuous(labels = scales::percent, limits = c(0, 1), breaks=seq(0,1,0.2)) +
            scale_x_continuous(limits = c(0, n_max_plot)) +
            labs(
                title = "Curva de Poder vs. Tamaño de Muestra por Grupo",
                x = "Número de Replicaciones por Tratamiento (n)",
                y = "Poder Estadístico (Prob. de detectar el efecto)"
            ) +
            theme_bw(base_size = 14)
    })
}