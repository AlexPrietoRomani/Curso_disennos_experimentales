# R/modules/session8.R

# UI para la Sesión 8
session8UI <- function(id) {
    ns <- NS(id)
    tagList(
        tags$h3(class = "session-title", "Sesión 8: Poder Estadístico y Tamaño de Muestra"),
        
        navset_tab(
            # ===== PESTAÑA 1: Introducción a Diseños Avanzados =====
            nav_panel(
                title = "1. Diseño en Cuadrado Latino (DCL)",
                
                h4(class = "section-header", "1.1 El Desafío del Campo Real: Heterogeneidad Bidireccional"),
                p(
                    "El Diseño en Bloques Completos al Azar (DBCA) es una herramienta poderosa que aprendimos para controlar ", strong("una"), " fuente de variación predecible, como una pendiente. Bloqueamos perpendicularmente a ese gradiente para crear mini-experimentos más homogéneos. Pero, ¿qué ocurre cuando la realidad del campo es más compleja?"
                ),

                # --- Presentación del Problema ---
                tags$div(class="card mb-4",
                    tags$div(class="card-body",
                        h5(class = "card-title text-center", "El Escenario del Doble Gradiente"),
                        p(
                            "Imagina un campo de ensayo en una ladera (lo que crea un gradiente de fertilidad de arriba hacia abajo) que, además, recibe un viento secante constante desde el oeste (creando un gradiente de humedad de izquierda a derecha). En este escenario, la productividad del suelo no es uniforme ni en una sola dirección; varía en dos dimensiones."
                        ),
                        # Usaremos una fila para poner los dos gráficos comparativos lado a lado
                        fluidRow(
                            # Columna para el DBCA y su problema
                            column(6, style="border-right: 1px solid #ddd;",
                                h6(strong("Intento 1: Usando un DBCA")),
                                p(class="text-muted", "Decidimos bloquear por filas para controlar el gradiente de fertilidad de la ladera."),
                                # El plotOutput para el DBCA
                                plotOutput(ns("plot_dbca_problema"), height = "300px")
                            ),
                            # Columna para el DCL y su solución
                            column(6,
                                h6(strong("La Solución: Bloqueo Bidireccional")),
                                p(class="text-muted", "Necesitamos un diseño que controle AMBOS gradientes simultáneamente."),
                                # El plotOutput para el DCL
                                plotOutput(ns("plot_doble_gradiente"), height = "300px")
                            )
                        )
                    )
                ),

                p(
                    "Como muestra el gráfico de la izquierda, si solo bloqueamos por filas (DBCA), la variación debida al gradiente de humedad (horizontal) sigue presente ", em("dentro"), " de nuestros bloques, inflando el error experimental. No hemos controlado todo el 'ruido' posible."
                ),
                p(
                    strong("La pregunta clave es:"), "¿Existe un diseño que pueda particionar la variación en tres componentes: Filas, Columnas y Tratamientos? La respuesta es sí: el ", strong("Diseño en Cuadrado Latino (DCL).")
                ),

                tags$hr(),

                h4(class = "section-header", "1.2 Anatomía del Diseño en Cuadrado Latino (DCL)"),
                p(
                    "El DCL es un diseño elegante y muy eficiente que impone una estructura de bloqueo en dos direcciones. Su poder reside en una regla de construcción simple pero muy estricta, que define toda su estructura."
                ),

                # --- Regla Fundamental ---
                tags$div(class="alert alert-info text-center", style="font-size: 1.2em;",
                    strong("Número de Tratamientos (t) = Número de Filas (r) = Número de Columnas (c)")
                ),

                # --- Explicación y Ejemplo Visual ---
                tags$div(class = "content-row",
                    # Columna principal para el ejemplo
                    tags$div(class = "main-content",
                        p(
                            "Esta regla significa que si tienes 4 tratamientos (A, B, C, D), necesitarás un campo experimental con exactamente 4 filas y 4 columnas. La asignación de estos tratamientos a las 16 parcelas resultantes no es completamente aleatoria; debe cumplir una restricción crucial:"
                        ),
                        tags$blockquote(class="blockquote",
                            p(class="mb-0", em("Cada tratamiento debe aparecer exactamente una vez en cada fila y exactamente una vez en cada columna."))
                        ),
                        p("Veamos un ejemplo de un Cuadrado Latino 4x4 aleatorizado:"),
                        
                        # Tabla visual del DCL
                        tags$table(class="table table-bordered text-center dcl-example-table",
                            tags$thead(
                                tags$tr(
                                    tags$th(""),
                                    tags$th("Columna 1"),
                                    tags$th("Columna 2"),
                                    tags$th("Columna 3"),
                                    tags$th("Columna 4")
                                )
                            ),
                            tags$tbody(
                                tags$tr(
                                    tags$th("Fila 1"),
                                    tags$td(class="dcl-A", "A"),
                                    tags$td(class="dcl-D", "D"),
                                    tags$td(class="dcl-B", "B"),
                                    tags$td(class="dcl-C", "C")
                                ),
                                tags$tr(
                                    tags$th("Fila 2"),
                                    tags$td(class="dcl-B", "B"),
                                    tags$td(class="dcl-A", "A"),
                                    tags$td(class="dcl-C", "C"),
                                    tags$td(class="dcl-D", "D")
                                ),
                                tags$tr(
                                    tags$th("Fila 3"),
                                    tags$td(class="dcl-C", "C"),
                                    tags$td(class="dcl-B", "B"),
                                    tags$td(class="dcl-D", "D"),
                                    tags$td(class="dcl-A", "A")
                                ),
                                tags$tr(
                                    tags$th("Fila 4"),
                                    tags$td(class="dcl-D", "D"),
                                    tags$td(class="dcl-C", "C"),
                                    tags$td(class="dcl-A", "A"),
                                    tags$td(class="dcl-B", "B")
                                )
                            )
                        )
                    ),
                    
                    # Nube lateral para la verificación de la regla
                    tags$div(class = "note-cloud",
                        tags$strong("Verificando la Regla del DCL:"),
                        p("Toma cualquier tratamiento, por ejemplo el ", strong("Tratamiento A"), ":"),
                        tags$ul(
                            tags$li(icon("check", class="text-success"), "Aparece en la Fila 1, Columna 1."),
                            tags$li(icon("check", class="text-success"), "Aparece en la Fila 2, Columna 2."),
                            tags$li(icon("check", class="text-success"), "Aparece en la Fila 3, Columna 4."),
                            tags$li(icon("check", class="text-success"), "Aparece en la Fila 4, Columna 3.")
                        ),
                        p("Cumple la regla: una vez por fila y una vez por columna. ¡Puedes verificarlo para B, C y D!"),
                        tags$hr(),
                        p(strong("La consecuencia:"), " Esta restricción doble es lo que permite al modelo estadístico aislar y eliminar la variabilidad asociada tanto a las filas como a las columnas, limpiando el error experimental de forma muy eficiente.")
                    )
                ),

                tags$hr(),

                h4(class = "section-header", "1.3 El Modelo Matemático y la Partición de la Varianza"),
                p(
                    "La elegancia del Diseño en Cuadrado Latino se refleja en su modelo estadístico. Para entenderlo, comparémoslo con el modelo del DBCA que ya conocemos."
                ),

                # --- Comparación de Modelos Lado a Lado ---
                fluidRow(
                    # Tarjeta para el Modelo DBCA (el conocido)
                    column(6, style="padding-right: 10px;",
                        tags$div(class="card h-100",
                            tags$div(class="card-header", strong("Recordatorio: Modelo del DBCA")),
                            tags$div(class="card-body",
                                withMathJax(helpText("$$Y_{ij} = \\mu + \\rho_i + \\tau_j + \\epsilon_{ij}$$")),
                                tags$p("Aquí, la variación total de los datos se descompone en:"),
                                tags$ul(
                                    tags$li(strong("Efecto del Bloque (\\(\\rho_i\\)):"), " La variación que logramos controlar."),
                                    tags$li(strong("Efecto del Tratamiento (\\(\\tau_j\\)):"), " La variación que nos interesa estudiar."),
                                    tags$li(strong("Error Aleatorio (\\(\\epsilon_{ij}\\)):"), " Toda la variación restante, el 'ruido'.")
                                )
                            )
                        )
                    ),
                    # Tarjeta para el Modelo DCL (el nuevo)
                    column(6, style="padding-left: 10px;",
                        tags$div(class="card h-100 border-primary",
                            tags$div(class="card-header bg-primary text-white", strong("El Modelo del DCL")),
                            tags$div(class="card-body",
                                withMathJax(helpText("$$Y_{ijk} = \\mu + \\rho_i + \\gamma_j + \\tau_k + \\epsilon_{ijk}$$")),
                                p("El modelo DCL da un paso más. Descompone la variación en:"),
                                tags$ul(
                                    tags$li(strong("Efecto de Fila (\\(\\rho_i\\)):"), " Variación del 1er gradiente."),
                                    tags$li(strong("Efecto de Columna (\\(\\gamma_j\\)):"), " ", span(class="text-primary", strong("¡Nuevo!"), " Variación del 2do gradiente.")),
                                    tags$li(strong("Efecto del Tratamiento (\\(\\tau_k\\)):"), " La variación de interés."),
                                    tags$li(strong("Error Aleatorio (\\(\\epsilon_{ijk}\\)):"), " El 'ruido' restante, ahora mucho más pequeño.")
                                )
                            )
                        )
                    )
                ),

                # --- Explicación Visual de la Partición de Varianza ---
                tags$div(class="card mt-4",
                    tags$div(class="card-body text-center",
                        h5("El Superpoder del DCL: 'Limpiar' el Error"),
                        p("La principal ventaja se ve al comparar cómo se particiona el Error. El DCL toma el error grande de un DBCA y le extrae la variación debida a las columnas."),
                        # Usando iconos para un diagrama de flujo simple
                        tags$div(class="d-flex justify-content-center align-items-center flex-wrap",
                            tags$div(class="alert alert-secondary m-2", strong("Varianza Total")),
                            tags$i(class="bi bi-arrow-right-short fa-2x mx-2"),
                            tags$div(class="alert alert-info m-2", strong("Var. por Tratamiento")),
                            tags$i(class="bi bi-plus fa-2x mx-2"),
                            tags$div(class="alert alert-info m-2", strong("Var. por Fila")),
                            tags$i(class="bi bi-plus fa-2x mx-2"),
                            tags$div(class="alert alert-info m-2", strong("Var. por Columna")),
                            tags$i(class="bi bi-plus fa-2x mx-2"),
                            tags$div(class="alert alert-success m-2", strong("Error 'Limpio' (más pequeño)"))
                        )
                    )
                ),

                # --- La Tabla ANOVA Esperada ---
                h4(class="section-header mt-4", "La Tabla ANOVA del DCL"),
                p("Esta partición de la varianza se refleja directamente en la tabla del Análisis de Varianza. A diferencia del DBCA, ahora tendremos una fila adicional para las 'Columnas'."),
                tags$table(class="table table-bordered table-sm text-center",
                    tags$thead(class="table-light",
                        tags$tr(
                            tags$th("Fuente de Variación"),
                            tags$th("Grados de Libertad (gl)"),
                            tags$th("Cuadrado Medio (CM)"),
                            tags$th("Estadístico F"),
                            tags$th("Interpretación de la Prueba F")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td("Filas"),
                            tags$td("t - 1"),
                            tags$td("CM(Filas)"),
                            tags$td(code("CM(Filas) / CME")),
                            tags$td("¿Fue efectivo el bloqueo por filas?")
                        ),
                        tags$tr(
                            tags$td("Columnas"),
                            tags$td("t - 1"),
                            tags$td("CM(Columnas)"),
                            tags$td(code("CM(Columnas) / CME")),
                            tags$td("¿Fue efectivo el bloqueo por columnas?")
                        ),
                        tags$tr(
                            tags$td(strong("Tratamientos")),
                            tags$td(strong("t - 1")),
                            tags$td(strong("CM(Trats)")),
                            tags$td(strong(code("CM(Trats) / CME"))),
                            tags$td(strong("¿Hay diferencias significativas entre los tratamientos?"))
                        ),
                        tags$tr(
                            tags$td("Error"),
                            tags$td("(t - 1)(t - 2)"),
                            tags$td("CME"),
                            tags$td(""),
                            tags$td("Varianza residual no explicada.")
                        )
                    )
                ),
                p(strong("Punto Crítico:"), " Observa los grados de libertad para el error: ", code("(t - 1)(t - 2)"), ". En un cuadrado pequeño (ej. 4x4), los g.l. del error son solo (3)(2) = 6. Este es un número muy bajo, lo que hace que la prueba para los tratamientos tenga menos potencia. Esta es la principal desventaja del DCL: se 'paga' por el control del doble gradiente con una pérdida de grados de libertad para el error."),

                tags$hr(),

                h4(class = "section-header", "1.4 Constructor Interactivo de un DCL"),
                p("Usa los controles para generar un layout aleatorizado de un Diseño en Cuadrado Latino. Observa cómo se cumple la regla de que cada tratamiento aparece solo una vez por fila y columna."),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Define tu Diseño"),
                        sliderInput(ns("dcl_size"), "Tamaño del Cuadrado (Nº Tratamientos):", 
                                    min = 3, max = 8, value = 4, step = 1),
                        actionButton(ns("generar_dcl"), "Generar Nuevo Layout DCL", icon=icon("table-cells"), class = "btn-success w-100 mt-3")
                    ),
                    mainPanel(
                        width = 9,
                        h5(textOutput(ns("dcl_titulo"))),
                        plotOutput(ns("plot_layout_dcl"), height="500px")
                    )
                ),
                
                tags$hr(),
                
                h4(class = "section-header", "1.5 Ventajas y Desventajas del DCL"),
                fluidRow(
                    column(6,
                        tags$div(class="card h-100 border-success",
                            tags$div(class="card-header bg-success text-white", strong("Ventajas")),
                            tags$div(class="card-body",
                                tags$ul(
                                    tags$li(strong("Máxima Eficiencia:"), " Es el diseño más eficiente para controlar dos fuentes de variación direccionales."),
                                    tags$li(strong("Reducción de Error:"), " Si los gradientes de fila y columna son reales, la reducción del CME es sustancial, aumentando enormemente la potencia."),
                                    tags$li(strong("Menos Unidades:"), " Requiere menos unidades experimentales (t²) que un factorial completo de tres factores (Tratamiento, Fila, Columna).")
                                )
                            )
                        )
                    ),
                    column(6,
                        tags$div(class="card h-100 border-danger",
                            tags$div(class="card-header bg-danger text-white", strong("Desventajas")),
                            tags$div(class="card-body",
                                tags$ul(
                                    tags$li(strong("Muy Rígido:"), " La restricción 'Nº Tratamientos = Nº Filas = Nº Columnas' es muy limitante."),
                                    tags$li(strong("Pocos Grados de Libertad para el Error:"), " Especialmente en cuadrados pequeños (3x3, 4x4), quedan muy pocos grados de libertad para estimar el error, lo que reduce la potencia si los gradientes no son fuertes."),
                                    tags$li(strong("No hay Interacción:"), " El modelo asume que no hay interacción entre los efectos de fila, columna y tratamiento. Si existe, se confunde con el error experimental.")
                                )
                            )
                        )
                    )
                ),

                tags$hr(),
            ),

            # ===== PESTAÑA 2: ¿Por qué Planificar? =====
            nav_panel(
                title = "2. Poder Estadístico y Anatomía de una Decisión Estadística",
                
                tags$h4(class = "section-header", "2.1 Más Allá del P-valor: La Anatomía de una Decisión Estadística"),
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
                
                tags$h4(class = "section-header", "2.3 Las Consecuencias de un Experimento con Bajo Poder"),
                tags$p(
                    "Realizar un análisis de poder ", em("a priori"), " (antes de empezar) no es un lujo, es una responsabilidad ética y científica. Un experimento con bajo poder (ej. menos del 80%) es problemático porque:",
                    tags$ul(
                        tags$li("Es un ", strong("desperdicio de recursos:"), " se invierte tiempo, dinero, tierra y mano de obra en un estudio que tiene una alta probabilidad de no encontrar nada, incluso si hay algo que encontrar."),
                        tags$li("Produce ", strong("resultados no concluyentes:"), " un resultado no significativo (p > 0.05) en un estudio de bajo poder es ambiguo. ¿Significa que no hay efecto, o que nuestro experimento no fue lo suficientemente bueno para detectarlo?"),
                        tags$li("Tiene ", strong("implicaciones éticas,"), " especialmente en investigación con animales o recursos limitados, ya que se están utilizando sujetos o insumos en un experimento con pocas posibilidades de generar conocimiento útil.")
                    )
                )
            ),

            # ===== PESTAÑA 3: Los 4 Ingredientes del Poder (VERSIÓN MEJORADA) =====
            nav_panel(
                title = "3. Los 4 Ingredientes del Poder",
                
                tags$h4(class = "section-header", "3.1 La Ecuación del Poder: Un Juego de Equilibrio"),
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

                tags$h4(class = "section-header", "3.2 El Reto Central: Estimar el Tamaño del Efecto (", em("f"), " de Cohen)"),
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
                tags$h4(class="section-header", "3.3 Puntos de Referencia para el Tamaño del Efecto"),
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

            # ===== PESTAÑA 4: Calculadora de Poder y Muestra =====
            nav_panel(
                title = "4. Calculadora de Poder y Muestra",
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
            
            # ===== PESTAÑA 5: Poder para Diseños Avanzados =====
            nav_panel(
                title = "5. Poder para Diseños Avanzados",
                
                tags$h4(class = "section-header", "5.1 Limitaciones de las Fórmulas Simples"),
                tags$p(
                    "La calculadora que exploramos en la pestaña anterior es perfecta para un ANOVA de una vía (DCA). Sin embargo, en cuanto introducimos bloques (DBCA) o múltiples factores (Factoriales, Split-Plot), la ecuación del poder se complica enormemente. ¿Por qué? Porque ahora la varianza se particiona en más componentes, y el 'error' que se usa como denominador en las pruebas F cambia dependiendo del efecto que se esté probando (Stroup, 1989)."
                ),
                tags$p(
                    "Calcular el poder para estos diseños requiere herramientas más sofisticadas. A continuación, exploramos las dos soluciones más comunes y potentes."
                ),

                hr(),
                
                # --- G*Power ---
                tags$h4(class = "section-header", "5.2 Solución 1: Software Especializado (G*Power)"),
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
                tags$h4(class = "section-header", "5.3 Solución 2: Simulaciones de Monte Carlo (El Enfoque Universal)"),
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
                tags$h4(class="section-header", "5.4 Guía Rápida: ¿Qué Herramienta Usar?"),
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

            # ===== PESTAÑA 6: ANÁLISIS MULTIVARIADO =====
            nav_panel(
                title = "6. MANOVA: Análisis Multivariado",
                
                h4(class = "section-header", "6.1 La Realidad Agronómica: Múltiples Respuestas Correlacionadas"),
                p(
                    "Hasta ahora, nuestro enfoque ha sido ", strong("univariado"), ": hemos analizado una única variable de respuesta a la vez (rendimiento, altura, etc.). Sin embargo, la agronomía es un sistema complejo. Un solo tratamiento, como la aplicación de un bioestimulante, no solo afecta el rendimiento, sino que puede influir simultáneamente en la calidad del fruto, la resistencia a enfermedades y la eficiencia en el uso del agua."
                ),
                p(
                    "Cuando medimos múltiples variables en la misma unidad experimental, entramos en el dominio del ", strong("análisis multivariado.")
                ),

                # Usaremos content-row para alinear el texto principal y la nota lateral
                tags$div(class = "content-row",
                    
                    # Columna principal con el texto y los problemas
                    tags$div(class = "main-content",
                        p(
                            "El problema fundamental es que estas variables de respuesta a menudo están ", strong("correlacionadas."), " Ignorar esta estructura de interdependencia y analizar cada variable por separado con un ANOVA individual es una práctica tentadora pero peligrosa, que conduce a dos problemas graves:"
                        ),
                        
                        # Tarjetas para los dos problemas principales
                        fluidRow(
                            column(6,
                                tags$div(class="card border-danger h-100",
                                    tags$div(class="card-header bg-danger text-white", strong("Problema 1: Inflación del Error Tipo I")),
                                    tags$div(class="card-body",
                                        p(class="card-text", "Imagina que realizas 20 ANOVAs separados, uno para cada variable, con un \\(\\alpha = 0.05\\). Por pura probabilidad, esperarías que uno de esos análisis (5% de 20) salga 'significativo' por azar, incluso si no hay ningún efecto real. Esto se llama ", strong("problema de las comparaciones múltiples."), " Aumentas drásticamente el riesgo de una 'falsa alarma'.")
                                    )
                                )
                            ),
                            column(6,
                                tags$div(class="card border-warning h-100",
                                    tags$div(class="card-header bg-warning text-dark", strong("Problema 2: Pérdida de Poder y de Información")),
                                    tags$div(class="card-body",
                                        p(class="card-text", "Un tratamiento podría tener un efecto sutil pero consistente en ", em("varias"), " variables a la vez. Cada efecto individual podría no ser lo suficientemente fuerte como para ser detectado por un ANOVA, pero el ", strong("efecto combinado"), " sí es significativo. Al analizar por separado, perdemos la capacidad de ver el patrón completo y podríamos descartar un tratamiento prometedor (un 'falso negativo').")
                                    )
                                )
                            )
                        ),
                        p(class="mt-3", "La solución es utilizar un enfoque que analice todas las variables de respuesta de forma conjunta, teniendo en cuenta su estructura de correlación: el ", strong("Análisis Multivariado."))
                    ),
                    
                    # Columna lateral (note-cloud) con el ejemplo práctico
                    tags$div(class = "note-cloud",
                        tags$strong("Ejemplo: Caracterización de Variedades"),
                        p("En un ensayo para comparar 5 variedades de quinua, medimos simultáneamente:"),
                        tags$ul(
                            tags$li("Rendimiento (kg/ha)"),
                            tags$li("Contenido de proteína (%)"),
                            tags$li("Peso de mil granos (g)"),
                            tags$li("Altura de la planta (cm)"),
                            tags$li("Días a la madurez")
                        ),
                        p(strong("Correlaciones esperadas:"), " Es probable que el rendimiento esté negativamente correlacionado con el contenido de proteína (efecto de dilución) y positivamente con la altura de la planta. Un análisis multivariado puede capturar estas relaciones.")
                    )
                ),

                tags$hr(),

                h4(class = "section-header", "6.2 MANOVA: El Análisis de Varianza Multivariado"),

                # --- Tarjeta de Aclaración: MANOVA vs. ANCOVA ---
                tags$div(class="card border-warning mb-4",
                    tags$div(class="card-body",
                        tags$div(class="row align-items-center",
                            tags$div(class="col-md-2 text-center",
                                p(icon("project-diagram", class="fa-4x text-warning"))
                            ),
                            tags$div(class="col-md-10",
                                h5(class="card-title", "Aclaración Crucial: MANOVA no es ANCOVA"),
                                p(
                                    "Es muy fácil confundir estos dos acrónimos, pero representan análisis fundamentalmente diferentes:"
                                ),
                                tags$ul(
                                    tags$li(strong("ANCOVA (Análisis de Covarianza):"), " Tiene ", strong("UNA"), " variable de respuesta continua y utiliza una o más covariables (continuas) para ", strong("AJUSTAR"), " esa respuesta y reducir el error. El objetivo es limpiar el 'ruido'."),
                                    tags$li(strong("MANOVA (Análisis Multivariado de Varianza):"), " Tiene ", strong("MÚLTIPLES"), " variables de respuesta continuas y las analiza ", strong("SIMULTÁNEAMENTE"), " para ver si los grupos son diferentes en general. El objetivo es controlar la inflación del Error Tipo I.")
                                )
                            )
                        )
                    )
                ),

                p(
                    "El MANOVA es la extensión directa del ANOVA al caso multivariado. Mientras que el ANOVA compara una media a la vez entre grupos, el MANOVA compara el ", strong("centro de gravedad multidimensional"), " de los grupos, conocido como ", strong("vector de medias") ," o ", strong("centroide.")
                ),

                # --- Visualización del Concepto ---
                tags$div(class="content-row",
                    tags$div(class="main-content",
                        p(strong("La Pregunta Clave del MANOVA:")),
                        tags$blockquote(class="blockquote",
                            p(class="mb-0", em("Considerando todas mis variables de respuesta juntas, ¿los centroides de los grupos de tratamiento están en la misma ubicación en el espacio multivariado, o al menos un tratamiento ha desplazado significativamente su centroide?"))
                        ),
                        p(strong("Hipótesis del MANOVA:")),
                        tags$ul(
                            tags$li(strong("\\(H_0\\):"), " Los vectores de medias son iguales en todos los grupos. ", withMathJax(helpText("$$\\mathbf{\\mu}_1 = \\mathbf{\\mu}_2 = \\dots = \\mathbf{\\mu}_k$$"))),
                            tags$li(strong("\\(H_1\\):"), " Al menos un vector de medias es diferente.")
                        ),
                        p(
                            "En lugar de calcular un único estadístico F, MANOVA calcula estadísticos de prueba multivariados (como la ", strong("Traza de Pillai,"), " Lambda de Wilks, Traza de Hotelling-Lawley, o la Raíz Máxima de Roy) que evalúan la separación entre estos centroides, teniendo en cuenta la correlación entre las variables."
                        )
                    ),
                    tags$div(class = "note-cloud",
                        tags$strong("¿Cuándo usar MANOVA? (Lista de Chequeo)"),
                        tags$ol(
                            tags$li("Tienes un diseño experimental claro (DCA, DBCA, Factorial)."),
                            tags$li("Has medido ", strong("dos o más"), " variables de respuesta continuas."),
                            tags$li("Tienes una razón teórica para creer que estas respuestas están ", strong("correlacionadas"), " (ej. rendimiento y calidad)."),
                            tags$li("Quieres una prueba 'ómnibus' (general) protegida antes de realizar ANOVAs individuales. Si el MANOVA es significativo (p < 0.05), te da luz verde para explorar cada variable por separado para ver cuál contribuyó a la diferencia global.")
                        )
                    )
                ),

                # --- Ejemplo Visual y de Código ---
                h5(class="section-header", "Ejemplo Práctico: Diferencias entre Especies de Iris"),
                p("Usaremos el dataset `iris` para preguntar: ¿El 'perfil del sépalo', definido por su Largo y Ancho, es diferente entre las tres especies?"),

                fluidRow(
                    # Columna para el gráfico conceptual
                    column(6,
                        h6(class="text-center", strong("Visualización del Concepto de Centroides")),
                        plotOutput(ns("manova_centroids_plot"))
                    ),
                    # Columna para el código y la interpretación
                    column(6, style="border-left: 1px solid #ddd; padding-left: 20px;",
                        h6(strong("Implementación en R")),
                        tags$pre(class="r-code",
                            htmltools::HTML(
                                "# El modelo se especifica uniendo las respuestas con cbind()\n",
                                "modelo_manova <- manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)\n\n",
                                "# Ver el resumen de la prueba multivariada\n",
                                "summary(modelo_manova, test = 'Pillai')"
                            )
                        ),
                        h6(strong("Interpretación de la Salida")),
                        p("La salida de `summary()` nos dará una tabla. Nos fijamos en la fila del factor (`Species`) y en su p-valor (`Pr(>F)`)."),
                        p(strong("Si p < 0.05:"), " Concluimos que existe una diferencia significativa en el perfil combinado de los sépalos entre al menos dos de las especies. Esto nos justifica para proceder con análisis de seguimiento, como ANOVAs individuales para `Sepal.Length` y `Sepal.Width`.")
                    )
                ),

                tags$hr(),

                h4(class = "section-header", "6.3 Modelo, Supuestos y Análisis de Seguimiento del MANOVA"),
                p(
                    "Una vez que el MANOVA nos da una 'luz verde' (un resultado significativo), nuestro trabajo no ha terminado. Debemos entender qué variables impulsaron esa diferencia y verificar que nuestro análisis sea válido. Esto implica comprender los supuestos del MANOVA y cómo realizar los análisis de seguimiento."
                ),

                h4(class = "section-header", "Supuestos del MANOVA"),
                p(
                    "El MANOVA es una prueba paramétrica y, como tal, se basa en varios supuestos. Su robustez varía, pero es crucial tenerlos en cuenta:"
                ),
                tags$ul(
                    tags$li(strong("Independencia de las Observaciones:"), " Al igual que en el ANOVA, este es el supuesto más crítico. Debe ser garantizado por un diseño experimental y una aleatorización adecuados."),
                    tags$li(strong("Normalidad Multivariada:"), " Este es el análogo multivariado de la normalidad. Asume que los vectores de los residuos siguen una distribución normal multivariada dentro de cada grupo. Es un supuesto difícil de verificar directamente. Afortunadamente, MANOVA es relativamente robusto a violaciones si cada variable individual tiene una distribución aproximadamente normal y los tamaños de muestra no son muy pequeños."),
                    tags$li(strong("Homogeneidad de las Matrices de Varianza-Covarianza (Homocedasticidad Multivariada):"), " Este es el análogo de la homogeneidad de varianzas del ANOVA. Asume que la matriz de varianza-covarianza es la misma en todos los grupos. Se puede probar con la ", strong("Prueba M de Box."), " Violaciones de este supuesto pueden ser problemáticas, especialmente si los tamaños de grupo son desiguales. La Traza de Pillai es el estadístico de MANOVA más robusto a esta violación.")
                ),

                tags$hr(),

                h4(class = "section-header", "El Flujo de Análisis Post-MANOVA"),
                p(
                    "Un p-valor significativo en el MANOVA es solo el comienzo. Nos dice que hay una diferencia en algún lugar, pero no dónde. El siguiente paso es un análisis de 'seguimiento' para identificar la fuente de la significancia."
                ),

                # --- Diagrama de Flujo del Análisis ---
                tags$div(class="card",
                    tags$div(class="card-body",
                        h5(class="text-center", "Ruta de Análisis después de un MANOVA Significativo"),
                        tags$div(class="d-flex justify-content-center align-items-center flex-wrap",
                            tags$div(class="alert alert-success m-2", strong("MANOVA p < 0.05")),
                            tags$i(class="bi bi-arrow-right-short fa-2x mx-2"),
                            tags$div(class="alert alert-info m-2", strong("Paso 1: ANOVAs de Seguimiento")),
                            tags$i(class="bi bi-arrow-right-short fa-2x mx-2"),
                            tags$div(class="alert alert-primary m-2", strong("Paso 2: Pruebas Post-Hoc (Tukey, etc.)"))
                        )
                    )
                ),

                tags$div(class="content-row mt-4",
                    tags$div(class="main-content",
                        p(strong("Paso 1: ANOVAs de Seguimiento (Follow-up ANOVAs)")),
                        p(
                            "Una vez que el MANOVA confirma una diferencia global, procedemos a realizar ANOVAs individuales para cada una de las variables de respuesta. El propósito es identificar qué variable(s) específica(s) contribuyen a la diferencia multivariada."
                        ),
                        p(
                            strong("Ajuste por Comparaciones Múltiples:"), " Dado que ahora estamos realizando múltiples pruebas (un ANOVA por cada variable de respuesta), corremos el riesgo de inflar el Error Tipo I. Para controlar esto, se debe usar un nivel de significancia más estricto, comúnmente el ", strong("ajuste de Bonferroni."), " Si tienes 3 variables de respuesta, tu nuevo nivel de alfa para cada ANOVA sería: \\(0.05 / 3 = 0.0167\\)."
                        ),
                        
                        p(strong("Paso 2: Pruebas Post-Hoc (si el ANOVA de seguimiento es significativo)")),
                        p(
                            "Si un ANOVA individual (ej. para 'Rendimiento') resulta significativo después del ajuste de Bonferroni, entonces procedemos como de costumbre con una prueba post-hoc (como Tukey HSD) sobre esa variable para ver qué pares de tratamientos son diferentes."
                        )
                    ),
                    tags$div(class="note-cloud",
                        tags$strong("¿Por qué no hacer solo los ANOVAs?"),
                        p("La protección del MANOVA es clave. Si el MANOVA inicial no es significativo, no deberíamos proceder con los ANOVAs individuales. Hacerlo sería 'pescar' resultados significativos ('fishing for significance'), lo que aumenta enormemente la probabilidad de reportar un falso positivo.")
                    )
                ),

                h4(class = "section-header", "Ejemplo Práctico en R: Análisis Completo"),
                p("Continuemos con el ejemplo de `iris`. Ya vimos que el MANOVA fue significativo. Ahora, realicemos el análisis de seguimiento."),
                tags$pre(class="r-code",
                    htmltools::HTML(
                        "# Paso 0: El MANOVA (ya lo hicimos)\n",
                        "modelo_manova <- manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)\n",
                        "summary(modelo_manova, test = 'Pillai') # Confirmamos que p < 0.05\n\n",
                        
                        "# Paso 1: ANOVAs de Seguimiento\n",
                        "# Podemos obtenerlos directamente del resumen del objeto MANOVA\n",
                        "summary.aov(modelo_manova)\n\n",
                        
                        "# --- Interpretación del summary.aov() ---\n",
                        "# La salida nos dará dos tablas ANOVA, una para Sepal.Length y otra para Sepal.Width.\n",
                        "# Debemos comparar el p-valor de 'Species' en cada tabla con nuestro alfa ajustado.\n",
                        "# Como tenemos 2 variables, el alfa de Bonferroni es 0.05 / 2 = 0.025.\n",
                        "# Vemos que para AMBAS variables, el p-valor es <<< 0.025. Ambas contribuyen a la diferencia.\n\n",

                        "# Paso 2: Pruebas Post-Hoc para cada variable significativa\n",
                        "# Como ambas fueron significativas, necesitamos una prueba de Tukey para cada una.\n",
                        "# Creamos modelos ANOVA individuales para pasarlos a TukeyHSD().\n",
                        "modelo_largo <- aov(Sepal.Length ~ Species, data = iris)\n",
                        "modelo_ancho <- aov(Sepal.Width ~ Species, data = iris)\n\n",
                        
                        "cat('--- Comparaciones Post-Hoc para Largo del Sépalo ---\\n')\n",
                        "TukeyHSD(modelo_largo)\n\n",
                        
                        "cat('--- Comparaciones Post-Hoc para Ancho del Sépalo ---\\n')\n",
                        "TukeyHSD(modelo_ancho)\n"
                    )
                ),

                tags$hr(),

                h4(class = "section-header", "6.4 Laboratorio Interactivo: El Poder del MANOVA con Datos Correlacionados"),

                # --- Usaremos navset_card_pill para organizar la explicación y el laboratorio ---
                navset_card_pill(
                    header = tags$h5("Guía del Laboratorio y Simulación"),
                    
                    # Pestaña con la explicación del escenario
                    nav_panel(
                        "El Escenario Simulado",
                        tags$h5("Contexto del Experimento"),
                        p(
                            "Vamos a simular un ensayo para evaluar el efecto de ", strong("dos nuevas variedades (Var_A, Var_B)"), " en comparación con un ", strong("Control"), " sobre un cultivo de papa. En cada parcela, medimos dos variables de respuesta que a menudo están correlacionadas:"
                        ),
                        tags$ul(
                            tags$li(strong("Respuesta Y₁:"), " Número de Tubérculos por Planta."),
                            tags$li(strong("Respuesta Y₂:"), " Peso Promedio por Tubérculo (g).")
                        ),
                        
                        tags$hr(),
                        
                        tags$h5("El Desafío: El 'Trade-off' Biológico"),
                        p(
                            "Existe un conocido 'trade-off' o compensación en muchos cultivos: si una planta produce ", strong("más"), " tubérculos, a menudo estos son más ", strong("pequeños"), " y viceversa. Esto crea una ", strong("correlación negativa"), " entre Y₁ e Y₂. Un tratamiento podría aumentar el número de tubérculos pero disminuir su tamaño promedio. Si analizamos cada variable por separado con un ANOVA, podríamos no ver un efecto claro. Sin embargo, el tratamiento sí está cambiando el 'perfil productivo' general de la planta."
                        ),
                        
                        tags$h5("Guía de los Controles de la Simulación"),
                        tags$dl(
                            tags$dt("Correlación (Número ↔ Peso):"),
                            tags$dd("Controla qué tan fuerte es esta compensación biológica. Un valor negativo fuerte (ej. -0.8) simula un trade-off muy pronunciado. Un valor cercano a cero simula variables casi independientes."),
                            tags$dt("Magnitud del Efecto del Tratamiento:"),
                            tags$dd("Controla qué tan diferentes son las variedades. Un valor bajo simula un efecto sutil donde Var_A aumenta ligeramente el número de tubérculos y Var_B aumenta ligeramente el tamaño.")
                        ),
                        
                        tags$div(class="alert alert-info", icon("lightbulb"),
                            strong("Tu Misión:"), " Encuentra un escenario donde los dos ANOVAs individuales den p-valores > 0.05 (no significativos), pero el MANOVA global dé un p-valor < 0.05 (significativo). Esto demuestra que el MANOVA puede detectar un efecto combinado que los análisis univariados pasarían por alto. ", em("Pista: necesitas un efecto de tratamiento sutil y una correlación negativa moderada a fuerte.")
                        )
                    ),
                    
                    # Pestaña con la herramienta interactiva
                    nav_panel(
                        "Laboratorio de Simulación",
                        sidebarLayout(
                            sidebarPanel(
                                width = 3,
                                tags$h5("Control de la Simulación"),
                                # Slider para la correlación entre las respuestas
                                sliderInput(ns("manova_cor_resp"), "Correlación (Número ↔ Peso):",
                                            min = -0.95, max = 0.95, value = -0.6, step = 0.05),
                                sliderInput(ns("manova_trat_effect_2"), "Magnitud del Efecto de la Variedad:", # Renombrado para evitar conflictos
                                            min = 0, max = 5, value = 2, step = 0.2),
                                actionButton(ns("run_manova_sim_2"), "Correr Simulación", icon=icon("play"), class="btn-primary w-100")
                            ),
                            mainPanel(
                                width = 9,
                                plotOutput(ns("manova_corr_plot")),
                                hr(),
                                fluidRow(
                                    column(6,
                                        h5("Resultados de ANOVAs Univariados"),
                                        verbatimTextOutput(ns("univar_anovas_output")),
                                        uiOutput(ns("univar_anovas_interpretation"))
                                    ),
                                    column(6,
                                        h5("Resultados del MANOVA (Análisis Conjunto)"),
                                        verbatimTextOutput(ns("multivar_manova_output")),
                                        uiOutput(ns("multivar_manova_interpretation"))
                                    )
                                )
                            )
                        )
                    )
                ),

                tags$hr(),
            ),

            # ===== PESTAÑA 7: Referencias =====
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

    # --- LÓGICA PARA LA PESTAÑA 1 ---
    ### ---- Subsección 1 ----
    # Gráfico conceptual de un campo con doble gradiente
    output$plot_doble_gradiente <- renderPlot({
        
        # Crear una cuadrícula de datos para el campo
        campo_grid <- expand.grid(Columna = 1:10, Fila = 1:10)
        
        # Simular los gradientes
        # El valor combinado será más alto en la esquina superior izquierda
        campo_grid$valor_combinado <- (11 - campo_grid$Fila) + (11 - campo_grid$Columna)
        
        ggplot(campo_grid, aes(x = Columna, y = Fila, fill = valor_combinado)) +
            geom_tile(color = "white") +
            
            # Añadir flechas y texto para indicar los gradientes
            annotate("segment", x = 5.5, xend = 5.5, y = 10.5, yend = 0.5,
                    arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 1.2) +
            annotate("text", x = 5.5, y = 0, label = "Gradiente de Fertilidad", fontface = "bold") +
            
            annotate("segment", x = 10.5, xend = 0.5, y = 5.5, yend = 5.5,
                    arrow = arrow(length = unit(0.3, "cm")), color = "black", linewidth = 1.2) +
            annotate("text", x = 0, y = 5.5, label = "Gradiente de Humedad", angle = 90, fontface = "bold", vjust = -0.5) +
            
            scale_fill_viridis_c(option = "cividis", guide = "none") + # Usar una paleta de colores perceptualmente uniforme
            scale_y_reverse() + # Poner la fila 1 arriba
            theme_void() + # Tema limpio sin ejes
            labs(title = "Campo Experimental con Doble Gradiente") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })

    # Gráfico conceptual de un campo con doble gradiente y cómo un DBCA lo manejaría
    output$plot_dbca_problema <- renderPlot({
        
        campo_grid <- expand.grid(Columna = 1:5, Fila = 1:5)
        campo_grid$valor_combinado <- (6 - campo_grid$Fila) + (6 - campo_grid$Columna)
        
        ggplot(campo_grid, aes(x = Columna, y = Fila, fill = valor_combinado)) +
            geom_tile(color = "white") +
            # Rectángulos que simulan los bloques de un DBCA
            geom_rect(aes(xmin=0.5, xmax=5.5, ymin=0.5, ymax=1.5), fill=NA, color="red", linewidth=1.5) +
            geom_rect(aes(xmin=0.5, xmax=5.5, ymin=1.5, ymax=2.5), fill=NA, color="red", linewidth=1.5) +
            geom_rect(aes(xmin=0.5, xmax=5.5, ymin=2.5, ymax=3.5), fill=NA, color="red", linewidth=1.5) +
            geom_rect(aes(xmin=0.5, xmax=5.5, ymin=3.5, ymax=4.5), fill=NA, color="red", linewidth=1.5) +
            geom_rect(aes(xmin=0.5, xmax=5.5, ymin=4.5, ymax=5.5), fill=NA, color="red", linewidth=1.5) +
            annotate("text", x = 0, y = 3, label = "Gradiente de Humedad No Controlado", angle = 90, fontface = "bold", color="blue", size=4) +
            scale_fill_viridis_c(option = "cividis", guide = "none") +
            scale_y_reverse() +
            theme_void() +
            labs(title = "DBCA: Bloqueo por Filas",
                subtitle = "El gradiente horizontal sigue afectando dentro de cada bloque (líneas rojas).") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"),
                plot.subtitle = element_text(hjust = 0.5, size=9))
    })

    ### ---- Subsección 2 ----
    dcl_design_rv <- eventReactive(input$generar_dcl, {

        req(input$dcl_size)
        t <- input$dcl_size
        
        nombres_tratamientos_vector <- paste0("T", 1:t)
        
        # Llamamos a la función con el vector de nombres
        design_obj <- try(agricolae::design.lsd(nombres_tratamientos_vector), silent = TRUE)
        
        if (inherits(design_obj, "try-error")) {
            showModal(modalDialog(
                title = "Error",
                "Asegúrate de que el paquete 'agricolae' está instalado para generar el diseño."
            ))
            return(NULL)
        }
        
        plan_campo <- design_obj$book
        
        # Convertir el plan a un data frame y renombrar las columnas
        df_plot <- plan_campo %>%
            rename(
                Fila = row, 
                Columna = col,
                Tratamiento = nombres_tratamientos_vector # Referencia directa al nombre de la columna
            ) %>%
            mutate(
                # Asegurarse de que los tipos de datos son correctos para ggplot
                Fila = as.factor(Fila),
                Columna = as.factor(Columna),
                Tratamiento = as.factor(Tratamiento)
            )
        
        return(df_plot)
        
    }, ignoreNULL = FALSE)

    # Título dinámico para el gráfico del DCL
    output$dcl_titulo <- renderText({
        req(dcl_design_rv())
        paste0("Ejemplo de un Diseño en Cuadrado Latino ", input$dcl_size, "x", input$dcl_size)
    })

    # Gráfico del layout del DCL
    output$plot_layout_dcl <- renderPlot({
        df_plot <- dcl_design_rv()
        req(df_plot)
        
        ggplot(df_plot, aes(x = Columna, y = Fila, fill = Tratamiento)) +
            geom_tile(color = "black", lwd = 1.5) +
            geom_text(aes(label = Tratamiento), color = "white", fontface = "bold", size = 6) +
            # Invertir el eje Y para que la Fila 1 esté arriba
            scale_y_discrete(limits = rev) + 
            labs(
                title = "Layout Aleatorizado de un DCL",
                subtitle = "Cada tratamiento aparece exactamente una vez por fila y por columna",
                x = "Columnas (Segunda Fuente de Bloqueo)",
                y = "Filas (Primera Fuente de Bloqueo)"
            ) +
            theme_minimal(base_size = 14) +
            coord_fixed() + # Celdas cuadradas
            theme(legend.position = "none") # La leyenda es redundante, los colores lo muestran
    })

    # --- LÓGICA PARA LA PESTAÑA 2: GRÁFICO CONCEPTUAL DE PODER ---
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

    # --- LÓGICA PARA LA PESTAÑA 4: CALCULADORA DE PODER ---
    
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

    # --- LÓGICA PARA LA PESTAÑA 6: ANÁLISIS MULTIVARIADO ---
    ### ---- Subsección 2 ----

    # Gráfico conceptual de los centroides para MANOVA
    output$manova_centroids_plot <- renderPlot({
        
        # Calcular los centroides (vectores de medias) para cada especie
        centroides <- iris %>%
            group_by(Species) %>%
            summarise(
                mean_length = mean(Sepal.Length),
                mean_width = mean(Sepal.Width)
            )

        ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
            # Puntos de datos individuales con transparencia
            geom_point(alpha = 0.4) +
            # Añadir los centroides como puntos grandes y sólidos
            geom_point(data = centroides, aes(x = mean_length, y = mean_width), 
                    shape = 18, size = 8, stroke = 1.5) +
            # Elipses de confianza (asumiendo normalidad multivariada)
            stat_ellipse(type = "t", level = 0.95, linetype = "dashed") +
            labs(
                title = "Espacio Multivariado de los Sépalos",
                subtitle = "MANOVA compara la ubicación de los centroides (♦)",
                x = "Largo del Sépalo (cm)",
                y = "Ancho del Sépalo (cm)"
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "bottom")
    })

    ### ---- Subsección 4 ----
    manova_corr_sim_results <- eventReactive(input$run_manova_sim_2, {
        
        set.seed(as.integer(Sys.time()))
        n_rep <- 25; n_trat <- 3
        
        # Parámetros de la simulación
        cor_resp <- input$manova_cor_resp
        efecto_mag <- input$manova_trat_effect_2
        
        # Crear el dataframe base
        df_base <- data.frame(
            tratamiento = factor(rep(c('Control', 'Var_A', 'Var_B'), each = n_rep))
        )
        
        # Definir el efecto del tratamiento
        # Var_A aumenta Y1 (número) y disminuye ligeramente Y2 (tamaño)
        # Var_B aumenta Y2 (tamaño) y disminuye ligeramente Y1 (número)
        efecto_y1 <- c(rep(0, n_rep), rep(efecto_mag, n_rep), rep(efecto_mag * -0.2, n_rep))
        efecto_y2 <- c(rep(0, n_rep), rep(efecto_mag * -0.2, n_rep), rep(efecto_mag, n_rep))

        # Generar errores correlacionados usando una matriz de covarianza
        matriz_cov <- matrix(c(1, cor_resp, cor_resp, 1), nrow = 2)
        errores <- MASS::mvrnorm(n = n_rep * n_trat, mu = c(0, 0), Sigma = matriz_cov)
        
        # Generar las variables de respuesta
        df_sim <- df_base %>%
        mutate(
            Num_Tubérculos = 10 + efecto_y1 + errores[, 1] * 3, # DE de 3 para el error de Y1
            Peso_Promedio = 80 + efecto_y2 + errores[, 2] * 8   # DE de 8 para el error de Y2
        )
        
        # Ajustar los tres modelos
        modelo_manova <- manova(cbind(Num_Tubérculos, Peso_Promedio) ~ tratamiento, data = df_sim)
        modelo_anova1 <- aov(Num_Tubérculos ~ tratamiento, data = df_sim)
        modelo_anova2 <- aov(Peso_Promedio ~ tratamiento, data = df_sim)
        
        list(
            datos = df_sim,
            manova_summary = summary(modelo_manova, test = "Pillai"),
            anova1_summary = summary(modelo_anova1),
            anova2_summary = summary(modelo_anova2)
        )
    }, ignoreNULL = FALSE)

    # Gráfico de dispersión para visualizar la correlación y los centroides
    output$manova_corr_plot <- renderPlot({
        res <- manova_corr_sim_results(); req(res)
        
        centroides <- res$datos %>%
            group_by(tratamiento) %>%
            summarise(across(c(Num_Tubérculos, Peso_Promedio), mean))

        ggplot(res$datos, aes(x = Num_Tubérculos, y = Peso_Promedio, color = tratamiento)) +
            geom_point(alpha = 0.5, size = 2.5) +
            geom_point(data = centroides, shape = 18, size = 8, stroke = 1.5) +
            labs(
                title = "Espacio de Respuesta: Número vs. Peso Promedio de Tubérculos",
                subtitle = "Los puntos grandes representan los centroides de cada tratamiento",
                x = "Número de Tubérculos por Planta",
                y = "Peso Promedio por Tubérculo (g)"
            ) +
            theme_minimal(base_size = 14) +
            theme(legend.position = "bottom")
    })

    # Salida para los ANOVAs univariados
    output$univar_anovas_output <- renderPrint({
        res <- manova_corr_sim_results(); req(res)
        cat("--- ANOVA para Número de Tubérculos ---\n")
        print(res$anova1_summary)
        cat("\n--- ANOVA para Peso Promedio ---\n")
        print(res$anova2_summary)
    })

    # Salida para el MANOVA
    output$multivar_manova_output <- renderPrint({
        res <- manova_corr_sim_results(); req(res)
        cat("--- Prueba Global MANOVA ---\n")
        print(res$manova_summary)
    })

    # Interpretaciones dinámicas
    output$univar_anovas_interpretation <- renderUI({
        res <- manova_corr_sim_results(); req(res)
        p_val1 <- res$anova1_summary[[1]]["tratamiento", "Pr(>F)"]
        p_val2 <- res$anova2_summary[[1]]["tratamiento", "Pr(>F)"]
        
        if (p_val1 < 0.05 || p_val2 < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"),
                strong("Conclusión: Significativo."), " Al menos uno de los ANOVAs individuales encontró una diferencia.")
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"),
                strong("Conclusión: No Significativo."), " Ningún ANOVA individual fue capaz de detectar un efecto significativo del tratamiento en las variables por separado.")
        }
    })

    output$multivar_manova_interpretation <- renderUI({
        res <- manova_corr_sim_results(); req(res)
        p_val_manova <- res$manova_summary$stats["tratamiento", "Pr(>F)"]
        
        if (p_val_manova < 0.05) {
            div(class="alert alert-success mt-2", icon("check-circle"),
                strong("Conclusión: Significativo."), " El MANOVA detectó una diferencia significativa en el 'perfil productivo' combinado (Número y Peso). Tuvo el poder de ver el efecto que los ANOVAs individuales pudieron haber omitido.")
        } else {
            div(class="alert alert-danger mt-2", icon("times-circle"),
                strong("Conclusión: No Significativo."), " Incluso considerando las variables conjuntamente, no hay evidencia de un efecto del tratamiento.")
        }
    })
}