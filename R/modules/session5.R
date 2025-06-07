# R/modules/session5.R

# UI para la Sesión 5
session5UI <- function(id) {
    ns <- NS(id)
    tagList(
        h3(class = "session-title", "Sesión 5: Diseño en Bloques Completos al Azar (DBCA)"),
        
        navset_tab(
            # ===== PESTAÑA 1: Introducción y Limitaciones del DCA =====
            nav_panel(
                title = "1. Del DCA al DBCA",
                
                h4(class = "section-header", "1.1 El Ideal del DCA: Un Mundo Homogéneo"),
                tags$div(class = "content-row",
                    tags$div(class = "main-content",
                        tags$p(
                            "Recordemos de la Sesión 4 que el ", tags$b("Diseño Completamente al Azar (DCA)"), 
                            " es nuestro punto de partida. Su belleza radica en la simplicidad: todos los tratamientos se asignan a las unidades experimentales con la misma probabilidad, como si sacáramos nombres de un sombrero. El modelo matemático subyacente es:"
                        ),
                        withMathJax(helpText(
                            "$$Y_{ij} = \\mu + \\tau_i + \\epsilon_{ij}$$"
                        )),
                        tags$p(
                            "Este modelo asume que toda la variabilidad no explicada por los tratamientos (\\(\\tau_i\\)) es capturada por un único término de error, \\(\\epsilon_{ij}\\), que representa el 'ruido' aleatorio e impredecible."
                        ),
                        tags$p(
                            tags$b("El supuesto clave:"), " Para que este modelo sea válido y eficiente, el 'ruido' debe ser verdaderamente aleatorio. Esto implica que todas las unidades experimentales son ", tags$b("fundamentalmente idénticas (homogéneas)"), " antes de aplicar los tratamientos. Cualquier diferencia entre ellas es despreciable y no sigue ningún patrón."
                        )
                    ),
                    tags$div(class = "note-cloud",
                        tags$strong("Analogía del Tiro al Blanco:"),
                        "Un DCA es como un tirador experto disparando a un blanco. Los tratamientos son diferentes tipos de rifles. Si todos los disparos se agrupan, pero los grupos de cada rifle están en lugares distintos, sabemos que los rifles son diferentes. El 'error' es la pequeña dispersión de los disparos alrededor del centro de su propio grupo."
                    )
                ),
                
                h4(class = "section-header", "1.2 El Talón de Aquiles del DCA: La Realidad del Campo"),
                tags$p(
                    "El mundo real, y en especial el campo agronómico, rara vez es homogéneo. La mayoría de las veces, nos enfrentamos a la ", tags$b("heterogeneidad:"), " fuentes de variación predecibles o sistemáticas que no son el foco de nuestro estudio, pero que inevitablemente afectan a nuestras mediciones. Estas son conocidas como ", tags$b("factores de molestia o nuisance factors"), "."
                ),

                # Representación visual de la heterogeneidad
                tags$div(class = "row align-items-center",
                    tags$div(class = "col-md-7",
                        tags$p(tags$strong("Fuentes comunes de heterogeneidad en campo:")),
                        tags$ul(
                            tags$li(tags$strong("Gradientes de Suelo:"), " Cambios en la textura, materia orgánica o pH a lo largo del campo."),
                            tags$li(tags$strong("Topografía:"), " Una pendiente suave puede causar que una parte del campo sea más húmeda (acumulación de agua) y otra más seca."),
                            tags$li(tags$strong("Efectos de Borde:"), " Las parcelas en los bordes del ensayo pueden estar expuestas a más viento, sol o competencia de la vegetación circundante."),
                            tags$li(tags$strong("Historial del Campo:"), " Zonas donde hubo una pila de estiércol años atrás o caminos antiguos pueden tener una fertilidad residual diferente."),
                            tags$li(tags$strong("Prácticas de Manejo:"), " Diferencias en la aplicación de riego o la compactación por el paso de maquinaria.")
                        )
                    ),
                    tags$div(class = "col-md-5 text-center",
                        tags$p(class="small-diagram", tags$strong("Visualizando un Gradiente")),
                        # SVG simple para representar un gradiente de campo
                        tags$svg(width="100%", height="150", viewBox="0 0 200 100", style="border: 1px solid #ccc;",
                            tags$defs(
                                tags$linearGradient(id="grad1", x1="0%", y1="0%", x2="100%", y2="0%",
                                    tags$stop(offset="0%", style="stop-color:#fde7b4;stop-opacity:1"),
                                    tags$stop(offset="100%", style="stop-color:#6c460d;stop-opacity:1")
                                )
                            ),
                            tags$rect(width="200", height="100", fill="url(#grad1)"),
                            tags$text(x="10", y="55", fill="white", `font-size`="14", "Zona Fértil (Húmeda)"),
                            tags$text(x="190", y="55", fill="white", `font-size`="14", `text-anchor`="end", "Zona Pobre (Seca)")
                        ),
                        tags$p(class = "small-diagram", "Figura 5.1: Un campo rara vez es uniforme. A menudo existe un gradiente de alguna propiedad.")
                    )
                ),
                
                h4(class = "section-header", "1.3 La Consecuencia: Inflación del Error y Pérdida de Poder"),
                tags$p(
                    "¿Qué sucede si ignoramos este gradiente y usamos un DCA? Imagina que, por pura mala suerte de la aleatorización, un tratamiento superior (ej. una nueva variedad) cae mayormente en la zona pobre del campo, mientras que un tratamiento control cae en la zona fértil."
                ),
                tags$div(class="alert alert-warning", role="alert",
                    tags$h5(class="alert-heading", "¡El Efecto de Confusión!"),
                    "El efecto del tratamiento se ", tags$em("confunde"), " con el efecto del gradiente. El rendimiento de la nueva variedad se verá artificialmente bajo, y el del control, artificialmente alto. El modelo DCA, al no poder separar estas dos fuentes de variación, atribuye toda esta gran diferencia al 'error aleatorio'."
                ),
                tags$p(
                    "Matemáticamente, esto significa que la variabilidad sistemática del campo (\\(SS_{Gradiente}\\)) se suma a la varianza del error experimental verdadero, inflando el Cuadrado Medio del Error:"
                ),
                withMathJax(helpText(
                    "$$\\text{CM}_{\\text{Error (DCA)}} = \\frac{\\text{SS}_{\\text{Error Verdadero}} + \\text{SS}_{\\text{Gradiente}}}{\\text{gl}_{\\text{Error}}}$$"
                )),
                tags$p(
                    "Recordemos que el estadístico F para probar los tratamientos es \\(F = \\frac{\\text{CM}_{\\text{Tratamientos}}}{\\text{CM}_{\\text{Error}}}\\). Al inflar el denominador (\\(\\text{CM}_{\\text{Error}}\\)), el valor de F se hace más pequeño. Un F más pequeño tiene menos probabilidad de ser estadísticamente significativo. En resumen:"
                ),
                tags$div(class="text-center",
                    tags$p(style="font-size: 1.2em;",
                    tags$strong("Ignorar la Heterogeneidad"), 
                    tags$i(class="bi bi-arrow-right"), " Aumenta el \\(\\text{CM}_{Error}\\) ", 
                    tags$i(class="bi bi-arrow-right"), " Disminuye el Estadístico F ", 
                    tags$i(class="bi bi-arrow-right"), tags$strong("Reduce la Potencia Estadística"),
                    tags$i(class="bi bi-arrow-right"), " Menor capacidad de detectar diferencias reales."
                    )
                ),
                tags$p(
                    "Hemos perdido ", tags$b("precisión"), " y ", tags$b("sensibilidad"), ". Es como intentar escuchar un susurro (el efecto del tratamiento) en medio de un concierto de rock (el ruido del campo). El bloqueo es la técnica que nos permite 'bajar el volumen' del concierto para poder escuchar el susurro con claridad."
                )
            ),
            
            # ===== PESTAÑA 2: Diseño en Bloques (DBCA) =====
            nav_panel(
                title = "2. Diseño en Bloques (DBCA)",
                
                h4(class = "section-header", "2.1 Principio del Bloqueo: 'Comparar lo Similar'"),
                tags$p(
                    "Habiendo establecido el problema de la heterogeneidad en la Pestaña 1, la solución lógica es ", tags$b("controlar"),
                    " la fuente de variación conocida. Este es el principio fundamental del bloqueo: ",
                    tags$em("“Bloquear lo que se puede, y aleatorizar lo que no se puede”"),
                    " (Box, Hunter, & Hunter, 2005). El objetivo es agrupar las unidades experimentales en ",
                    tags$b("bloques"), " o 'mini-ensayos' que sean lo más homogéneos posible internamente."
                ),
                tags$p(
                    "La regla de oro es que la variabilidad ", tags$em("dentro"), " de cada bloque sea mínima, mientras que la variabilidad ",
                    tags$em("entre"), " los bloques puede (y se espera que) sea grande. De esta forma, las comparaciones entre tratamientos se realizan en condiciones más uniformes, aumentando la precisión."
                ),
                
                tags$h5(tags$b("El Diseño en Bloques Completos al Azar (DBCA)")),
                tags$p(
                    "El DBCA (o RCBD en inglés) es la implementación más directa de este principio. Sus características definitorias son:",
                    tags$ul(
                        tags$li(tags$strong("Completos:"), " Cada bloque contiene todos y cada uno de los tratamientos a evaluar. Si tienes 5 tratamientos, cada bloque debe tener 5 unidades experimentales."),
                        tags$li(tags$strong("Al Azar:"), " Dentro de cada bloque, la asignación de los tratamientos a las unidades experimentales es completamente aleatoria. La aleatorización se realiza de forma independiente para cada bloque.")
                    )
                ),

                # Diagrama conceptual del DBCA
                h4(class = "section-header", "2.2 Estructura y Aleatorización del DBCA"),
                tags$div(class = "row align-items-center",
                    tags$div(class = "col-md-7",
                        tags$p(tags$strong("Pasos para configurar un DBCA en campo:")),
                        tags$ol(
                            tags$li("Identificar la principal fuente de heterogeneidad (ej., una pendiente que afecta la humedad)."),
                            tags$li("Definir los bloques de tal manera que sean ", tags$strong("perpendiculares al gradiente."), " Esto asegura que cada bloque sea internamente homogéneo, pero que los bloques difieran entre sí."),
                            tags$li("Subdividir cada bloque en un número de parcelas igual al número de tratamientos."),
                            tags$li("Para cada bloque por separado, asignar los tratamientos a las parcelas de forma completamente aleatoria.")
                        )
                    ),
                    tags$hr(),

                    p("La mejor manera de entender un DBCA es crearlo. Usa los controles para definir tu ensayo y observa cómo se aleatorizan los tratamientos dentro de cada bloque. Decide la orientación de los bloques para contrarrestar un gradiente de campo imaginario."),
    
                    # Layout interactivo para el diseño
                    sidebarLayout(
                        # Panel lateral con los controles
                        sidebarPanel(
                            width = 3,
                            tags$h5("Parámetros del Diseño"),
                            numericInput(ns("num_trat_diseno"), "Número de Tratamientos:", value = 4, min = 2, max = 10, step = 1),
                            numericInput(ns("num_bloques_diseno"), "Número de Bloques (Replicaciones):", value = 3, min = 2, max = 12, step = 1),
                            
                            radioButtons(ns("orientacion_bloque"), "Orientación de los Bloques:",
                                        choices = c("Bloques en Columnas (Gradiente Horizontal)" = "columnas",
                                                    "Bloques en Filas (Gradiente Vertical)" = "filas"),
                                        selected = "columnas"),
                                        
                            actionButton(ns("generar_diseno"), "Generar Nueva Aleatorización", icon = icon("random"), class = "btn-success w-100 mt-3")
                        ),
                        
                        # Panel principal con la visualización del diseño
                        mainPanel(
                            width = 9,
                            tags$h5("Layout del Campo Experimental Generado"),
                            # Usamos uiOutput para renderizar una tabla HTML compleja desde el servidor
                            uiOutput(ns("layout_campo_ui"))
                        )
                    ),

                    tags$hr(),
                ),

                h4(class = "section-header", "2.3 El Modelo Lineal y la Partición de la Varianza"),
                tags$p(
                    "El éxito del DBCA se formaliza en su modelo lineal. A diferencia del DCA, ahora tenemos un término explícito para el efecto del bloque:"
                ),
                withMathJax(helpText(
                    "$$Y_{ij} = \\mu + \\tau_i + \\beta_j + \\epsilon_{ij}$$"
                )),
                tags$p("Donde:"),
                tags$ul(
                    tags$li("\\(Y_{ij}\\) es la observación del tratamiento \\(i\\) en el bloque \\(j\\)."),
                    tags$li("\\(\\mu\\) es la media general."),
                    tags$li("\\(\\tau_i\\) es el efecto del \\(i\\)-ésimo tratamiento (lo que nos interesa medir)."),
                    tags$li("\\(\\beta_j\\) es el efecto del \\(j\\)-ésimo bloque (el factor de molestia controlado)."),
                    tags$li("\\(\\epsilon_{ij}\\) es el error aleatorio, que se asume \\(\\sim N(0, \\sigma^2)\\).")
                ),
                tags$p(
                    "La consecuencia directa es una nueva ", tags$b("partición de la Suma de Cuadrados Total (SCTotal)"), ". Mientras que en el DCA la SCTotal se dividía solo en Tratamientos y Error, aquí se divide en tres componentes:"
                ),
                withMathJax(helpText(
                    "$$\\text{SC}_{\\text{Total}} = \\text{SC}_{\\text{Tratamientos}} + \\text{SC}_{\\text{Bloques}} + \\text{SC}_{\\text{Error}}$$"
                )),
                tags$p(
                    "La tabla ANOVA de un DBCA refleja esta nueva partición:"
                ),
                tags$table(class="table table-bordered",
                    tags$thead(
                        tags$tr(
                            tags$th("Fuente de Variación"), tags$th("Grados de Libertad (gl)"), tags$th("Suma de Cuadrados (SC)"), tags$th("Cuadrado Medio (CM)"), tags$th("Estadístico F")
                        )
                    ),
                    tags$tbody(
                        tags$tr(
                            tags$td("Tratamientos"), tags$td("\\(t-1\\)"), tags$td("\\(SC_{Trat}\\)"), tags$td("\\(CM_{Trat} = \\frac{SC_{Trat}}{t-1}\\)"), tags$td("\\(F_{Trat} = \\frac{CM_{Trat}}{CM_{Error}}\\)")
                        ),
                        tags$tr(
                            tags$td("Bloques"), tags$td("\\(b-1\\)"), tags$td("\\(SC_{Bloq}\\)"), tags$td("\\(CM_{Bloq} = \\frac{SC_{Bloq}}{b-1}\\)"), tags$td("\\(F_{Bloq} = \\frac{CM_{Bloq}}{CM_{Error}}\\)")
                        ),
                        tags$tr(
                            tags$td("Error"), tags$td("\\((t-1)(b-1)\\)"), tags$td("\\(SC_{Error}\\)"), tags$td("\\(CM_{Error} = \\frac{SC_{Error}}{(t-1)(b-1)}\\)"), tags$td()
                        ),
                        tags$tr(
                            tags$td("Total"), tags$td("\\(tb-1\\)"), tags$td("\\(SC_{Total}\\)"), tags$td(), tags$td()
                        )
                    )
                ),
                tags$p(class="small-diagram mt-1", "Donde \\(t\\) es el número de tratamientos y \\(b\\) es el número de bloques."),
                
                h4(class = "section-header", "2.4 Supuestos del Modelo DBCA"),
                tags$p("Los supuestos son similares a los del DCA, pero con una consideración adicional:",
                    tags$ul(
                        tags$li(tags$strong("Aditividad:"), " Se asume que no hay interacción entre los bloques y los tratamientos. Esto significa que el efecto de un tratamiento es el mismo en todos los bloques (ej. el fertilizante A siempre supera al Control en una cantidad similar, sin importar si están en la zona húmeda o seca). Si un tratamiento se comporta excepcionalmente bien en un tipo de bloque y mal en otro, este supuesto se viola y el modelo DBCA no es el más adecuado (se necesitarían modelos más complejos)."),
                        tags$li(tags$strong("Normalidad:"), " Los errores \\(\\epsilon_{ij}\\) deben seguir una distribución normal. Se verifica con QQ-plots o pruebas de Shapiro-Wilk sobre los residuales del modelo."),
                        tags$li(tags$strong("Homocedasticidad:"), " La varianza de los errores debe ser constante para todos los tratamientos. Se verifica con gráficos de residuales vs. ajustados o pruebas de Levene."),
                        tags$li(tags$strong("Independencia:"), " Los errores deben ser independientes entre sí, lo cual se garantiza con una correcta aleatorización dentro de cada bloque.")
                    )
                )
            ),
            
            # ===== PESTAÑA 3: Análisis en R y Eficiencia =====
            nav_panel(
                title = "3. Análisis y Eficiencia en R",
                
                tags$p(
                    "Una vez que el ensayo está diseñado y los datos han sido recolectados, el siguiente paso es analizar los resultados para responder a nuestras preguntas de investigación. Esta sección detalla el flujo de trabajo completo para analizar un DBCA en R."
                ),

                h4(class = "section-header", "3.1 Flujo de Trabajo del Análisis"),
                tags$ol(
                    tags$li(tags$strong("Exploración de Datos:"), " Visualizar los datos para detectar patrones, outliers o errores de entrada."),
                    tags$li(tags$strong("Ajuste del Modelo ANOVA:"), " Implementar el modelo lineal aditivo en R."),
                    tags$li(tags$strong("Verificación de Supuestos:"), " Diagnosticar si los supuestos del modelo (aditividad, normalidad, homocedasticidad) se cumplen."),
                    tags$li(tags$strong("Interpretación del ANOVA:"), " Evaluar la significancia de los tratamientos y los bloques."),
                    tags$li(tags$strong("Análisis Post-Hoc:"), " Si los tratamientos son significativos, determinar qué medias difieren."),
                    tags$li(tags$strong("Evaluación del Diseño:"), " Cuantificar la efectividad del bloqueo mediante la Eficiencia Relativa (RE).")
                ),
                
                hr(),

                h4(class = "section-header", "3.2 Análisis Paso a Paso en R"),
                tags$p(
                    "A continuación, se muestra un ejemplo completo utilizando un conjunto de datos hipotético llamado ", tags$code("datos_ensayo"),
                    " que contiene las columnas: ", tags$code("tratamiento"), ", ", tags$code("bloque"), " y ", tags$code("rendimiento"), "."
                ),

                # --- Ajuste del Modelo y Verificación de Supuestos ---
                tags$div(class="practice-text",
                    tags$h5("Paso 1: Ajustar el Modelo y Verificar sus Supuestos"),
                    tags$h6(tags$b("Teoría: El Modelo y sus Condiciones de Validez")),
                    tags$p(
                        "Ajustar el modelo significa pedirle a R que estime los parámetros del modelo lineal aditivo \\(Y_{ij} = \\mu + \\tau_i + \\beta_j + \\epsilon_{ij}\\). El resultado de este ajuste, sin embargo, solo es confiable si se cumplen ciertos supuestos sobre los errores (\\(\\epsilon_{ij}\\))."
                    ),
                    tags$ul(
                        tags$li(tags$b("Aditividad:"), " El efecto de un tratamiento debe ser consistente a través de todos los bloques. La ausencia de una interacción significativa entre bloques y tratamientos es crucial. Si un tratamiento es el mejor en un bloque pero el peor en otro, el modelo aditivo simple no es apropiado."),
                        tags$li(tags$b("Normalidad:"), " Los errores (residuales) deben seguir una distribución normal. Esto es fundamental para la validez de los p-valores y los intervalos de confianza generados por las pruebas F."),
                        tags$li(tags$b("Homocedasticidad:"), " La varianza de los errores debe ser la misma para todos los tratamientos. Si algunos tratamientos son mucho más variables que otros (heterocedasticidad), las comparaciones pueden ser sesgadas. El modelo asume una varianza de error común (\\(\\sigma^2\\))."),
                        tags$li(tags$b("Independencia:"), " Los errores deben ser independientes entre sí. La correcta aleatorización dentro de los bloques es nuestra principal herramienta para asegurar esto.")
                    ),
                    tags$p("La verificación de estos supuestos no es una formalidad; es la base sobre la cual se construye la validez de nuestras conclusiones."),
                    tags$pre(class = "r-code",
                        htmltools::HTML(
                            "# Cargar paquetes necesarios\n",
                            "library(dplyr)\n",
                            "library(ggplot2)\n",
                            "library(car)      # Para Levene's Test\n",
                            "\n",
                            "# Asumiendo que 'datos_ensayo' está cargado\n",
                            "# Asegurarse de que tratamiento y bloque sean factores\n",
                            "datos_ensayo$tratamiento <- as.factor(datos_ensayo$tratamiento)\n",
                            "datos_ensayo$bloque <- as.factor(datos_ensayo$bloque)\n",
                            "\n",
                            "# 1. Ajustar el modelo ANOVA para DBCA\n",
                            "modelo_dbca <- aov(rendimiento ~ tratamiento + bloque, data = datos_ensayo)\n",
                            "\n",
                            "# 2. Verificación de supuestos sobre los residuales del modelo\n",
                            "residuos_modelo <- residuals(modelo_dbca)\n",
                            "\n",
                            "#   a) Normalidad de los residuales (visual y formal)\n",
                            "par(mfrow=c(1,2))\n",
                            "hist(residuos_modelo, main='Histograma de Residuales', col='lightblue')\n",
                            "qqnorm(residuos_modelo); qqline(residuos_modelo, col='red')\n",
                            "par(mfrow=c(1,1))\n",
                            "shapiro.test(residuos_modelo) # Si p > 0.05, se cumple la normalidad\n",
                            "\n",
                            "#   b) Homocedasticidad (homogeneidad de varianzas)\n",
                            "plot(fitted(modelo_dbca), residuos_modelo, \n",
                            "     xlab='Valores Ajustados', ylab='Residuales', main='Residuales vs. Ajustados')\n",
                            "abline(h=0, lty=2, col='red') # No debe haber patrones (ej. forma de embudo)\n",
                            "leveneTest(rendimiento ~ tratamiento, data = datos_ensayo) # Si p > 0.05, se cumple\n",
                            "\n",
                            "#   c) Aditividad (no interacción bloque:tratamiento)\n",
                            "# Una forma visual es un gráfico de interacción. Si las líneas son paralelas, apoya la aditividad.\n",
                            "interaction.plot(x.factor = datos_ensayo$bloque, \n",
                            "                 trace.factor = datos_ensayo$tratamiento, \n",
                            "                 response = datos_ensayo$rendimiento, \n",
                            "                 main='Gráfico de Interacción Bloque:Tratamiento', \n",
                            "                 xlab='Bloques', ylab='Rendimiento Medio')"
                        )
                    )
                ),

                # --- Interpretación de la Tabla ANOVA ---
                tags$div(class="practice-text",
                    tags$h5("Paso 2: Interpretar la Tabla ANOVA y el Tamaño del Efecto"),
                    tags$h6(tags$b("Teoría: Descomponiendo la Variación y Midiendo su Importancia")),
                    tags$p(
                        "La tabla ANOVA es el resumen de la partición de la varianza total. Nos presenta dos pruebas F clave:"
                    ),
                    tags$ul(
                        tags$li(tags$strong("Prueba F para Tratamientos:"), " Compara la varianza ", tags$em("entre"), " las medias de los tratamientos (la señal) con la varianza ", tags$em("dentro"), " de los tratamientos (el ruido). Un p-valor bajo (< 0.05) indica que la 'señal' es significativamente más fuerte que el 'ruido', lo que nos lleva a rechazar \\(H_0\\) y concluir que al menos un tratamiento tiene un efecto diferente."),
                        tags$li(tags$strong("Prueba F para Bloques:"), " Compara la varianza entre bloques con la varianza del error. Un p-valor bajo aquí es una ", tags$em("buena noticia"), ". No significa que un bloque sea 'mejor' que otro, sino que los bloques eran diferentes entre sí, y por lo tanto, la estrategia de bloqueo fue exitosa en remover una fuente significativa de variabilidad del error experimental.")
                    ),
                    tags$p(
                        "Sin embargo, la significancia estadística (p-valor) no lo es todo. El ", tags$b("Tamaño del Efecto (Eta-cuadrado parcial, \\(\\eta_p^2\\))"), " nos dice la importancia práctica de estos efectos. \\(\\eta_p^2\\) mide qué proporción de la varianza (después de controlar otros factores) es explicada por un factor específico. Un \\(\\eta_p^2 = 0.20\\) para tratamientos significa que el 20% de la varianza 'restante' es atribuible a las diferencias entre tratamientos, lo cual puede ser agronómicamente muy relevante."
                    ),
                    tags$pre(class = "r-code",
                        htmltools::HTML(
                            "# 1. Obtener el resumen de la tabla ANOVA\n",
                            "summary(modelo_dbca)\n",
                            "\n",
                            "# INTERPRETACIÓN:\n",
                            "# - Fila 'tratamiento': Si Pr(>F) < 0.05, concluimos que hay diferencias significativas entre al menos dos tratamientos.\n",
                            "# - Fila 'bloque': Si Pr(>F) < 0.05, significa que el bloqueo fue efectivo en capturar una fuente de variación significativa.\n",
                            "\n",
                            "# 2. Calcular el tamaño del efecto (Eta-cuadrado parcial, η²p)\n",
                            "library(effectsize)\n",
                            "eta_squared(modelo_dbca, partial = TRUE, ci = 0.95)\n",
                            "\n",
                            "# INTERPRETACIÓN de η²p:\n",
                            "# - η²p para 'tratamiento': Proporción de la varianza (una vez removido el efecto del bloque) que es explicada por los tratamientos.\n",
                            "# - η²p para 'bloque': Proporción de la varianza (una vez removido el efecto del tratamiento) que es explicada por los bloques."
                        )
                    )
                ),

                # --- Pruebas Post-Hoc ---
                tags$div(class="practice-text",
                    tags$h5("Paso 3: Realizar Comparaciones Múltiples (Post-Hoc)"),
                    tags$h6(tags$b("Teoría: ¿Quién es Diferente de Quién?")),
                    tags$p(
                        "Un ANOVA significativo es como una alarma de incendios: te dice que hay un problema (una diferencia), pero no dónde está el fuego (qué grupos difieren). Las pruebas post-hoc son los bomberos que inspeccionan habitación por habitación. Su objetivo es realizar comparaciones por pares entre las medias de los tratamientos, ajustando los p-valores para controlar la tasa de error por familia (la probabilidad de cometer al menos un falso positivo en todas las comparaciones)."
                    ),
                    tags$p(tags$strong("¿Qué prueba usar?")),
                    tags$ul(
                        tags$li(tags$b("Tukey HSD (Honestly Significant Difference):"), " Es el estándar de oro cuando se cumplen los supuestos de ANOVA (especialmente homocedasticidad) y se desean comparar todos los pares de medias. Es un buen equilibrio entre potencia y control de errores."),
                        tags$li(tags$b("Duncan o Student-Newman-Keuls (SNK):"), " Pruebas de rango múltiple, históricamente populares en agronomía. Son más potentes que Tukey (más propensas a encontrar diferencias) pero ofrecen un control más débil del error Tipo I, especialmente con muchos tratamientos. Se suelen presentar con letras para agrupar medias."),
                        tags$li(tags$b("Games-Howell:"), " Es la prueba de elección cuando se viola el supuesto de ", tags$strong("homogeneidad de varianzas (heterocedasticidad)."), " No requiere varianzas iguales y es robusta. Es la contraparte de Tukey para varianzas desiguales."),
                        tags$li(tags$b("Dunnett:"), " Se usa en una situación específica: cuando se quiere comparar varios tratamientos contra un único tratamiento ", tags$strong("control."), " Es más potente que Tukey para este propósito específico porque realiza menos comparaciones.")
                    ),
                    tags$pre(class = "r-code",
                        htmltools::HTML(
                            "# Se realiza SOLO SI el p-valor de 'tratamiento' en summary(modelo_dbca) es < 0.05\n",
                            "\n",
                            "# 1. Prueba de Tukey HSD\n",
                            "comparaciones_tukey <- TukeyHSD(modelo_dbca, which = 'tratamiento', conf.level = 0.95)\n",
                            "print(comparaciones_tukey)\n",
                            "\n",
                            "# INTERPRETACIÓN:\n",
                            "# Se observan los p-valores ajustados ('p adj'). Si p adj < 0.05 para un par (ej. 'B-A'),\n",
                            "# significa que la diferencia entre las medias de B y A es estadísticamente significativa.\n",
                            "\n",
                            "# 2. Visualización de los resultados de Tukey\n",
                            "plot(comparaciones_tukey, las=1) # las=1 para etiquetas horizontales\n",
                            "abline(v=0, lty=2, col='red') # Si un intervalo de confianza no cruza la línea roja (cero), la diferencia es significativa."
                        )
                    )
                ),

                # --- Eficiencia Relativa ---
                tags$div(class="practice-text",
                    tags$h5("Paso 4: Evaluar la Eficiencia del Diseño"),
                    tags$h6(tags$b("Teoría: ¿Valió la Pena Bloquear?")),
                    tags$p(
                        "El bloqueo tiene un costo: perdemos grados de libertad para la estimación del error (los gl de los bloques se 'restan' del total disponible para el error). Esta pérdida solo se justifica si la reducción en la Suma de Cuadrados del Error es lo suficientemente grande como para compensarla. La ",
                        tags$b("Eficiencia Relativa (RE)"), " es la métrica que nos permite cuantificar este balance."
                    ),
                    tags$p(
                        "La RE compara la precisión del DBCA con la precisión que ", tags$em("habríamos obtenido"), " con un DCA usando el mismo número de unidades experimentales. Esencialmente, estima cuál sería el Cuadrado Medio del Error de un DCA hipotético y lo compara con el CM(Error) que obtuvimos en nuestro DBCA."
                    ),
                    withMathJax(helpText(
                        "$$\\text{RE} = \\frac{\\text{CM}_{\\text{Error (DCA hipotético)}}}{\\text{CM}_{\\text{Error (DBCA)}}}$$"
                    )),
                    tags$p(
                        "Un valor de \\(\\text{RE} = 2.0\\) es una declaración poderosa: significa que el DBCA fue el doble de eficiente que un DCA. En otras palabras, para alcanzar la misma precisión (el mismo ancho de intervalo de confianza para las diferencias de medias), un investigador que usara un DCA habría necesitado el doble de replicaciones (y por lo tanto, el doble de recursos, tiempo y dinero). La RE traduce directamente el beneficio estadístico del bloqueo en un beneficio económico y logístico tangible."
                    ),
                    tags$p("La fórmula, como se vio en la teoría (Di Rienzo et al., 2005), compara la información obtenida por ambos diseños. Un valor de RE > 1 indica que el bloqueo fue exitoso."),
                    withMathJax(helpText(
                        "$$\\text{RE} = \\frac{(b-1)\\text{CM}_{Bloques} + b(t-1)\\text{CM}_{Error}}{(bt - 1)\\text{CM}_{Error}}$$ "
                    )),
                    tags$pre(class = "r-code",
                        htmltools::HTML(
                            "# Cálculo de RE en R a partir del modelo\n",
                            "anova_tabla <- anova(modelo_dbca)\n",
                            "\n",
                            "# Extraer Cuadrados Medios (Mean Sq)\n",
                            "cm_bloque <- anova_tabla['bloque', 'Mean Sq']\n",
                            "cm_error  <- anova_tabla['Residuals', 'Mean Sq']\n",
                            "\n",
                            "# Obtener número de bloques (b) y tratamientos (t)\n",
                            "b <- length(levels(datos_ensayo$bloque))\n",
                            "t <- length(levels(datos_ensayo$tratamiento))\n",
                            "\n",
                            "# Calcular RE\n",
                            "RE <- ((b - 1) * cm_bloque + b * (t - 1) * cm_error) / ((b * t - 1) * cm_error)\n",
                            "print(paste('Eficiencia Relativa (RE):', round(RE, 3)))\n",
                            "\n",
                            "# INTERPRETACIÓN:\n",
                            "# Si RE = 1.8, significa que para obtener la misma precisión que se logró con este DBCA,\n",
                            "# un DCA habría requerido un 80% más de unidades experimentales (replicaciones)."
                        )
                    )
                )
            ),
            
            # ===== PESTAÑA 4: Ejemplo Interactivo =====
            nav_panel(
                title = "4. Ejemplo Interactivo",
                
                h4(class = "section-header", "Simulación: El Poder del Bloqueo"),
                p("Usa los deslizadores para simular un ensayo agronómico. La salida te guiará a través de cada paso del análisis, comparando el enfoque correcto (DBCA) con el incorrecto (DCA). Observa cómo un gradiente de campo (efecto de bloque) cambia radicalmente las conclusiones."),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        tags$h5("Parámetros del Ensayo"),
                        sliderInput(ns("fuerza_bloque"), "Fuerza del Gradiente (Efecto de Bloque):", min = 0, max = 10, value = 5, step = 0.5),
                        sliderInput(ns("diferencia_trat"), "Diferencia Media Trat. vs Control:", min = 0, max = 5, value = 2, step = 0.2),
                        sliderInput(ns("sd_error"), "Variabilidad Aleatoria (Error, σ):", min = 0.5, max = 5, value = 1.5, step = 0.1),
                        sliderInput(ns("num_bloques"), "Número de Bloques:", min = 3, max = 10, value = 4, step = 1),
                        actionButton(ns("run_sim"), "Correr Simulación", icon = icon("play-circle"), class = "btn-primary w-100")
                    ),
                    mainPanel(
                        width = 9,
                        # Usamos navset_card_pill para una navegación limpia por los pasos del análisis
                        navset_card_pill(
                            header = tags$h4("Pasos del Análisis de la Simulación"),
                            
                            # 1. Exploración de Datos
                            nav_panel(
                                title = "1. Exploración Visual",
                                icon = icon("search"),
                                tags$h5("Mapa de Calor del Campo y Boxplots"),
                                plotOutput(ns("plot_exploratorio"), height = "400px"),
                                tags$p(class="mt-2", "El 'Mapa de Calor' a la izquierda visualiza la respuesta en cada parcela del campo. Un gradiente de color claro de izquierda a derecha indica un fuerte efecto de bloque. Los boxplots a la derecha muestran la distribución de la respuesta por tratamiento y por bloque.")
                            ),

                            # 2. Verificación de Supuestos
                            nav_panel(
                                title = "2. Supuestos del Modelo DBCA",
                                icon = icon("check-double"),
                                tags$h5("Diagnóstico Gráfico de los Residuales del Modelo DBCA"),
                                plotOutput(ns("plot_supuestos"), height = "400px"),
                                tags$p(class="mt-2", "Estos gráficos evalúan los supuestos del modelo DBCA (el correcto).",
                                    tags$ul(
                                        tags$li(tags$strong("Residuales vs. Ajustados:"), " Busca una dispersión aleatoria sin patrones (sin 'embudos'). Esto valida la homocedasticidad."),
                                        tags$li(tags$strong("Normal Q-Q:"), " Los puntos deben seguir la línea diagonal. Esto valida la normalidad de los residuales."),
                                        tags$li(tags$strong("Interacción Bloque:Tratamiento:"), " Las líneas deben ser aproximadamente paralelas. Esto valida la aditividad.")
                                    )
                                )
                            ),

                            # 3. Interpretación del ANOVA
                            nav_panel(
                                title = "3. Tablas ANOVA y Tamaño del Efecto",
                                icon = icon("table"),
                                fluidRow(
                                    column(6, 
                                        h5("Análisis Correcto: DBCA"),
                                        tableOutput(ns("tabla_anova_dbca")),
                                        h6("Tamaño del Efecto (η²p) para DBCA"),
                                        verbatimTextOutput(ns("eta_dbca"))
                                    ),
                                    column(6, 
                                        h5("Análisis Incorrecto: DCA (ignora bloques)"),
                                        tableOutput(ns("tabla_anova_dca")),
                                        h6("Tamaño del Efecto (η²) para DCA"),
                                        verbatimTextOutput(ns("eta_dca"))
                                    )
                                ),
                                tags$p(class="mt-2", "Compara las tablas. Nota cómo en el DBCA, la Suma de Cuadrados del Bloque 'roba' variabilidad del Error, resultando en un CM(Error) más pequeño y una prueba F para tratamientos más potente.")
                            ),

                            # 4. Post-Hoc y Eficiencia
                            nav_panel(
                                title = "4. Post-Hoc y Eficiencia",
                                icon = icon("sitemap"),
                                fluidRow(
                                    column(7,
                                        h5("Comparaciones Múltiples (Tukey HSD) para DBCA"),
                                        tags$p("Esta prueba se muestra solo si el factor 'tratamiento' es significativo en el modelo DBCA."),
                                        plotOutput(ns("plot_posthoc"), height="300px"),
                                        verbatimTextOutput(ns("texto_posthoc"))
                                    ),
                                    column(5,
                                        h5("Evaluación del Diseño"),
                                        tags$div(class="text-center", style="padding: 20px; border: 1px solid #ccc; border-radius: 10px; background-color: #f8f9fa;",
                                            tags$h6("Eficiencia Relativa (RE)"),
                                            tags$p(style="font-size: 2.5em; font-weight: bold; color: #007bff;", textOutput(ns("re_valor"))),
                                            tags$p(textOutput(ns("re_interpretacion")))
                                        )
                                    )
                                )
                            ),
                            
                            # 5. Conclusiones
                            nav_panel(
                                title = "5. Conclusiones",
                                icon = icon("clipboard-check"),
                                h5("Resumen General de la Simulación"),
                                verbatimTextOutput(ns("conclusion_final"))
                            )
                        )
                    )
                )
            ),
            
            # ===== PESTAÑA 5: Referencias =====
            nav_panel(
                title = "Referencias",
                tags$ul(
                    tags$li("Box, G. E. P., Hunter, J. S., & Hunter, W. G. (2005). ", tags$em("Statistics for experimenters: Design, innovation, and discovery"), " (2nd ed.). Wiley-Interscience."),
                    tags$li("Di Rienzo, J. A., Casanoves, F., González, L. A., Tablada, M., & Robledo, C. W. (2005). ", tags$em("Estadística para las ciencias agropecuarias"), ". Córdoba, Argentina: Editorial Brujas."),
                    tags$li("Kuehl, R. O. (2000). ", tags$em("Design of experiments: Statistical principles of research design and analysis"), " (2nd ed.). Duxbury Press."),
                    tags$li("Lenth, R. V. (2021). ", tags$em("emmeans: Estimated Marginal Means, aka Least-Squares Means."), " R package version 1.7.0. ", tags$a(href="https://CRAN.R-project.org/package=emmeans", "https://CRAN.R-project.org/package=emmeans")),
                    tags$li("Montgomery, D. C. (2017). ", tags$em("Design and analysis of experiments"), " (9th ed.). John Wiley & Sons.")
                )
            )
        )
    )
}


# Server para la Sesión 5
session5Server <- function(input, output, session) {
    
    # --- Pestaña 1: Exploración Visual ---

    # --- LÓGICA PARA EL DISEÑO INTERACTIVO DE LA PESTAÑA 2 ---
        
    # eventReactive para generar el diseño solo cuando se presiona el botón
    diseno_generado <- eventReactive(input$generar_diseno, {
        
        req(input$num_trat_diseno, input$num_bloques_diseno, input$orientacion_bloque)
        
        # Crear nombres de tratamientos (ej. T1, T2, ...)
        tratamientos <- paste0("T", 1:input$num_trat_diseno)
        
        # Usar agricolae para obtener la aleatorización
        # El seed cambia con el tiempo para que cada clic sea una nueva aleatorización
        set.seed(as.integer(Sys.time()))
        diseno_rcbd <- agricolae::design.rcbd(tratamientos, r = input$num_bloques_diseno)
        
        # Devolver la lista con el plan de campo (book) y los parámetros
        list(
            plan = diseno_rcbd$book,
            orientacion = input$orientacion_bloque,
            num_trat = input$num_trat_diseno,
            num_bloques = input$num_bloques_diseno
        )
    }, ignoreNULL = FALSE) # ignoreNULL=FALSE para que se ejecute la primera vez que se carga la app

    # Renderizar la tabla del diseño como un UI dinámico
    output$layout_campo_ui <- renderUI({
        
        diseno <- diseno_generado()
        req(diseno)
        
        # Paleta de colores para los tratamientos
        colores <- RColorBrewer::brewer.pal(max(3, diseno$num_trat), "Set2")
        names(colores) <- paste0("T", 1:diseno$num_trat)
        
        # Lógica para construir la tabla HTML
        if (diseno$orientacion == "columnas") {
            # --- Bloques como Columnas (Gradiente Horizontal) ---
            
            # Encabezados de la tabla (Bloque 1, Bloque 2, ...)
            header_cols <- lapply(1:diseno$num_bloques, function(j) {
                tags$th(style="border: 1px solid #dee2e6; background-color: #f8f9fa;", paste("Bloque", j))
            })
            
            # Filas de la tabla (cada fila es una unidad experimental)
            body_rows <- lapply(1:diseno$num_trat, function(i) {
                celdas <- lapply(1:diseno$num_bloques, function(j) {
                    # Encontrar el tratamiento para la parcela i, bloque j
                    trat <- diseno$plan[diseno$plan$block == j, "tratamientos"][i]
                    tags$td(style = paste0("background-color:", colores[trat], "; border: 1px solid #dee2e6; color: #333; font-weight: bold;"), trat)
                })
                tags$tr(celdas)
            })
            
            # Ensamblar la tabla completa
            tags$div(
                p(tags$i("Simulando un gradiente de izquierda a derecha (ej. humedad, fertilidad). Los bloques son las columnas verticales.")),
                tags$table(class = "table table-bordered text-center",
                    style = "width: 100%; table-layout: fixed;",
                    tags$thead(tags$tr(header_cols)),
                    tags$tbody(body_rows)
                )
            )
            
        } else {
            # --- Bloques como Filas (Gradiente Vertical) ---

            # Encabezados de las filas (Bloque 1, Bloque 2, ...)
            header_rows <- lapply(1:diseno$num_bloques, function(j) {
                tags$th(style="border: 1px solid #dee2e6; background-color: #f8f9fa; writing-mode: vertical-rl; text-orientation: mixed;", paste("Bloque", j))
            })

            # Celdas del cuerpo de la tabla
            body_celdas <- lapply(1:diseno$num_bloques, function(j) {
                fila_tratamientos <- diseno$plan[diseno$plan$block == j, "tratamientos"]
                celdas_fila <- lapply(fila_tratamientos, function(trat) {
                    tags$td(style = paste0("background-color:", colores[trat], "; border: 1px solid #dee2e6; color: #333; font-weight: bold;"), trat)
                })
                tags$tr(header_rows[[j]], celdas_fila)
            })

            # Ensamblar la tabla completa
            tags$div(
                p(tags$i("Simulando un gradiente de arriba hacia abajo (ej. pendiente, sombra). Los bloques son las filas horizontales.")),
                tags$table(class = "table table-bordered text-center",
                    style = "width: 100%; table-layout: fixed;",
                    tags$tbody(body_celdas)
                )
            )
        }
    })

    # --- Pestaña 4: Simulación Interactiva ---
    # 1. Usar eventReactive para que la simulación se ejecute solo al presionar el botón
    sim_results <- eventReactive(input$run_sim, {
        
        # --- Simulación de Datos ---
        req(input$fuerza_bloque, input$diferencia_trat, input$sd_error, input$num_bloques)
        
        set.seed(as.integer(Sys.time())) # Nueva semilla en cada simulación
        
        # Definir tratamientos y bloques
        tratamientos_vec <- c("A_Control", "B", "C", "D")
        num_trat <- length(tratamientos_vec)
        num_bloques <- input$num_bloques
        
        # Crear el esqueleto del diseño
        ensayo_df <- expand.grid(
            tratamiento = factor(tratamientos_vec),
            bloque = factor(1:num_bloques)
        )
        
        # Asignar efectos
        media_base <- 20
        
        # Efecto del tratamiento (B, C, D tienen un efecto incremental sobre el control A)
        efecto_trat <- c(0, input$diferencia_trat, input$diferencia_trat * 1.5, input$diferencia_trat * 0.5)
        names(efecto_trat) <- tratamientos_vec
        
        # Efecto de bloque (simula un gradiente lineal)
        efecto_bloque <- seq(from = 0, to = input$fuerza_bloque, length.out = num_bloques)
        
        # Error aleatorio
        error_aleatorio <- rnorm(n = nrow(ensayo_df), mean = 0, sd = input$sd_error)
        
        # Calcular la respuesta final
        ensayo_df <- ensayo_df %>%
            mutate(
                efecto_t = efecto_trat[tratamiento],
                efecto_b = efecto_bloque[as.numeric(bloque)],
                respuesta = media_base + efecto_t + efecto_b + error_aleatorio
            )

        # --- Análisis Estadístico ---
        modelo_dbca <- aov(respuesta ~ tratamiento + bloque, data = ensayo_df)
        modelo_dca <- aov(respuesta ~ tratamiento, data = ensayo_df)

        # --- Cálculos adicionales ---
        # Tamaño del efecto
        eta_dbca <- effectsize::eta_squared(modelo_dbca, partial = TRUE)
        eta_dca <- effectsize::eta_squared(modelo_dca)
        
        # Post-Hoc
        p_valor_trat_dbca <- broom::tidy(modelo_dbca) %>% filter(term == "tratamiento") %>% pull(p.value)
        tukey_results <- if (p_valor_trat_dbca < 0.05) {
            TukeyHSD(modelo_dbca, which = 'tratamiento')
        } else {
            NULL
        }

        # Eficiencia Relativa
        anova_dbca_summary <- anova(modelo_dbca)
        cm_bloque <- anova_dbca_summary["bloque", "Mean Sq"]
        cm_error_dbca <- anova_dbca_summary["Residuals", "Mean Sq"]
        num_trat <- length(unique(ensayo_df$tratamiento))
        num_bloques <- input$num_bloques
        RE <- ((num_bloques - 1) * cm_bloque + num_bloques * (num_trat - 1) * cm_error_dbca) / 
              ((num_bloques * num_trat - 1) * cm_error_dbca)

        # ===== CALCULAR EL TEXTO DE INTERPRETACIÓN DE LA RE AQUÍ =====
        re_interpretacion_texto <- if (RE > 1.1) {
            paste0("¡Excelente! El DBCA fue un ", round((RE - 1) * 100, 0), "% más eficiente que un DCA.")
        } else if (RE > 1) {
            "El bloqueo fue marginalmente útil."
        } else {
            "El bloqueo no fue efectivo. No hubo un gradiente claro que justifique su uso."
        }

        # Devolver lista expandida
        list(
            datos = ensayo_df,
            modelo_dbca = modelo_dbca,
            modelo_dca = modelo_dca,
            anova_dbca = broom::tidy(modelo_dbca),
            anova_dca = broom::tidy(modelo_dca),
            eta_dbca = effectsize::eta_squared(modelo_dbca, partial = TRUE),
            eta_dca = effectsize::eta_squared(modelo_dca),
            p_valor_trat_dbca = broom::tidy(modelo_dbca) %>% filter(term == "tratamiento") %>% pull(p.value),
            tukey_results = if (broom::tidy(modelo_dbca) %>% filter(term == "tratamiento") %>% pull(p.value) < 0.05) {
                                TukeyHSD(modelo_dbca, which = 'tratamiento')
                            } else {
                                NULL
                            },
            eficiencia_relativa = RE,
            # Guardamos el texto pre-calculado
            re_interpretacion_texto = re_interpretacion_texto 
        )
    })
    
    # --- Generación de Salidas para la UI ---
    
    # 1. Exploración Visual
    output$plot_exploratorio <- renderPlot({
        res <- sim_results(); req(res)
        
        p1 <- ggplot(res$datos, aes(x = bloque, y = tratamiento, fill = respuesta)) +
                geom_tile(color = "white") +
                scale_fill_viridis_c(option = "magma") +
                labs(title = "A) Mapa de Calor del Campo", x = "Bloques", y = "Tratamientos") +
                theme_minimal()

        p2 <- ggplot(res$datos, aes(x = tratamiento, y = respuesta, fill = tratamiento)) +
                geom_boxplot(show.legend = FALSE) + labs(title = "B) Respuesta por Tratamiento") + theme_minimal()

        p3 <- ggplot(res$datos, aes(x = bloque, y = respuesta, fill = bloque)) +
                geom_boxplot(show.legend = FALSE) + labs(title = "C) Respuesta por Bloque") + theme_minimal()
        
        # Combinar gráficos con patchwork
        (p1 | (p2 / p3)) + plot_layout(widths = c(2, 1))
    })

    # 2. Supuestos del Modelo
    output$plot_supuestos <- renderPlot({
        res <- sim_results(); req(res)
        
        p1 <- ggplot(res$modelo_dbca, aes(x = .fitted, y = .resid)) + 
                geom_point(alpha = 0.6) + 
                geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
                labs(title = "A) Residuales vs. Ajustados", x = "Valores Ajustados", y = "Residuales") +
                theme_bw()
        
        p2 <- ggplot(res$modelo_dbca, aes(sample = .resid)) +
                stat_qq() + stat_qq_line(color = "red") +
                labs(title = "B) Normal Q-Q Plot") + theme_bw()
        
        p3 <- ggplot(res$datos, aes(x = bloque, y = respuesta, group = tratamiento, color = tratamiento)) +
                stat_summary(fun = mean, geom = "line", size = 1) +
                stat_summary(fun = mean, geom = "point", size = 2) +
                labs(title = "C) Gráfico de Interacción (Aditividad)", x = "Bloques", y = "Respuesta Media") +
                theme_bw()
        
        p1 + p2 + p3
    })

    # 3. Tablas ANOVA y Tamaño del Efecto
    output$tabla_anova_dbca <- renderTable({
        res <- sim_results(); req(res)
        res$anova_dbca
    }, digits=4)
    output$tabla_anova_dca <- renderTable({
        res <- sim_results(); req(res)
        res$anova_dca
    }, digits=4)
    output$eta_dbca <- renderPrint({ res <- sim_results(); req(res); res$eta_dbca })
    output$eta_dca <- renderPrint({ res <- sim_results(); req(res); res$eta_dca })

    # 4. Post-Hoc y Eficiencia
    output$plot_posthoc <- renderPlot({
        res <- sim_results(); req(res)
        if (!is.null(res$tukey_results)) {
            plot(res$tukey_results, las = 1)
        }
    })
    output$texto_posthoc <- renderPrint({
        res <- sim_results(); req(res)
        if (!is.null(res$tukey_results)) {
            print(res$tukey_results)
        } else {
            cat("No se realizan pruebas post-hoc porque el factor 'tratamiento' no fue significativo (p >= 0.05) en el modelo DBCA.")
        }
    })
    output$re_valor <- renderText({
        res <- sim_results(); req(res)
        round(res$eficiencia_relativa, 2)
    })
    output$re_interpretacion <- renderText({
        res <- sim_results(); req(res)
        res$re_interpretacion_texto
    })

    # 5. Conclusiones
    output$conclusion_final <- renderPrint({
        res <- sim_results(); req(res)
        
        p_val_dca <- res$anova_dca %>% filter(term == "tratamiento") %>% pull(p.value)
        
        msg_comparacion <- if (res$p_valor_trat_dbca < 0.05 && p_val_dca >= 0.05) {
            "El hallazgo clave es que el DBCA detectó diferencias significativas que el DCA ignoró por completo. Esto demuestra el poder del bloqueo para aumentar la sensibilidad del experimento cuando existe un gradiente."
        } else if (res$p_valor_trat_dbca < 0.05 && p_val_dca < 0.05) {
            "Ambos modelos detectaron diferencias, pero el DBCA lo hizo con mayor certeza (menor p-valor y CM Error), lo que lo convierte en un análisis más robusto y preciso."
        } else {
            "En esta simulación, la diferencia entre tratamientos fue demasiado pequeña (o el error demasiado grande) para ser detectada incluso por el modelo DBCA."
        }
        
        cat(
            "--- CONCLUSIONES DE LA SIMULACIÓN ---\n\n",
            "1. ¿Hubo diferencias entre tratamientos? \n",
            "   - Modelo DBCA (Correcto): p-valor =", round(res$p_valor_trat_dbca, 4), "\n",
            "   - Modelo DCA (Incorrecto): p-valor =", round(p_val_dca, 4), "\n",
            "   - Veredicto:", msg_comparacion, "\n\n",
            "2. ¿Valió la pena bloquear? \n",
            "   - Eficiencia Relativa (RE):", round(res$eficiencia_relativa, 2), "\n",
            # AHORA USAMOS EL VALOR GUARDADO EN LA LISTA
            "   - Interpretación:", res$re_interpretacion_texto, "\n\n",
            "Lección Final: Ignorar una fuente de variación conocida (como un gradiente de campo) no la hace desaparecer; simplemente la convierte en 'ruido' que oculta los efectos reales que intentamos medir. El bloqueo es la herramienta fundamental para controlar ese ruido."
        )
    })
}