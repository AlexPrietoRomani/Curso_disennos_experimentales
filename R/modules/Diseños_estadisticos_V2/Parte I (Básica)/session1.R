# R/modules/session1.R

session1UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3(class = "session-title", "Sesión 1: Preparando el Terreno - Importación y Exploración de Datos en R"),
    
    navset_tab(
      # ===== PESTAÑA 1: Bienvenida y Contexto (VERSIÓN MEJORADA) =====
      nav_panel(
          title = "1. Bienvenida y Contexto",
          icon = icon("rocket"),
          
          h4(class = "section-header", "1.1 La Evolución de una Herramienta: Una Línea de Tiempo"),
          p("Para entender el poder de R, es útil conocer su origen. No es solo un programa, sino la culminación de décadas de innovación en computación estadística."),

          # --- Línea de Tiempo HTML/CSS ---
          tags$div(class = "timeline-container",
              tags$ul(class = "timeline",
                  # Hito 1: Nacimiento de S
                  tags$li(
                      tags$div(class="timeline-badge primary", icon("lightbulb")),
                      tags$div(class="timeline-panel",
                          tags$div(class="timeline-heading",
                              tags$h5(class="timeline-title", "Década de 1970: Nacimiento del Lenguaje S"),
                              tags$p(tags$small(class="text-muted", "Laboratorios Bell, EE. UU."))
                          ),
                          tags$div(class="timeline-body",
                              p("John Chambers y su equipo crean S, un lenguaje interactivo diseñado para que los estadísticos pudieran 'jugar' con los datos. La filosofía era clara: crear un entorno ", strong("para el análisis de datos, por analistas de datos."))
                          )
                      )
                  ),
                  
                  # Hito 2: Creación de R
                  tags$li(class="timeline-inverted",
                      tags$div(class="timeline-badge success", icon("r-project")),
                      tags$div(class="timeline-panel",
                          tags$div(class="timeline-heading",
                              tags$h5(class="timeline-title", "1993: La Creación de R"),
                              tags$p(tags$small(class="text-muted", "Universidad de Auckland, Nueva Zelanda"))
                          ),
                          tags$div(class="timeline-body",
                              p("Ross Ihaka y Robert Gentleman desarrollan R como una implementación de código abierto del lenguaje S. Al ser gratuito y abierto, se desata una explosión de colaboración global."),
                              p(strong("El momento clave:"), " La estadística de vanguardia se vuelve accesible para todos, no solo para las grandes corporaciones.")
                          )
                      )
                  ),

                  # Hito 3: Lanzamiento de RStudio IDE
                  tags$li(
                      tags$div(class="timeline-badge info", icon("desktop")),
                      tags$div(class="timeline-panel",
                          tags$div(class="timeline-heading",
                              tags$h5(class="timeline-title", "2011: Llega RStudio IDE"),
                              tags$p(tags$small(class="text-muted", "JJ Allaire y equipo"))
                          ),
                          tags$div(class="timeline-body",
                              p("RStudio (ahora Posit) crea un Entorno de Desarrollo Integrado (IDE) que revoluciona la experiencia de usuario. La combinación de editor de código, consola, visor de gráficos y ayuda en una sola ventana hace que R sea inmensamente más productivo y fácil de aprender.")
                          )
                      )
                  ),
                  
                  # Hito 4: El Auge del Tidyverse
                  tags$li(class="timeline-inverted",
                      tags$div(class="timeline-badge warning", icon("box-open")),
                      tags$div(class="timeline-panel",
                          tags$div(class="timeline-heading",
                              tags$h5(class="timeline-title", "Década de 2010: El Auge del `tidyverse`"),
                              tags$p(tags$small(class="text-muted", "Hadley Wickham y equipo"))
                          ),
                          tags$div(class="timeline-body",
                              p("Se desarrolla una colección de paquetes (`dplyr`, `ggplot2`, etc.) que comparten una filosofía de diseño común, haciendo el análisis de datos más intuitivo, legible y eficiente. Este es el ecosistema en el que nos centraremos en este curso.")
                          )
                      )
                  )
              )
          ),

          tags$h4(class = "section-header", "1.2 ¿Por qué R es la Herramienta Elegida en la Agronomía Moderna?"),
          tags$p(
              "Si bien existen muchos programas estadísticos, R se ha convertido en la herramienta preferida en la investigación agrícola por varias razones clave:"
          ),
          tags$ul(
              tags$li(strong("Especialización y Flexibilidad:"), " La comunidad ha desarrollado miles de paquetes especializados. ¿Necesitas analizar un diseño en bloques aumentados? Existe un paquete para ello (`agricolae`). ¿Modelar la distribución espacial de una plaga? Hay paquetes de geoestadística. R se adapta a tu problema, no al revés."),
              tags$li(strong("Reproducibilidad:"), " Un análisis en R es un script, un archivo de texto. Esto garantiza la total reproducibilidad de tu trabajo. Puedes compartir tu código con un colega o un revisor de una revista científica, y ellos podrán replicar tu análisis exactamente. Esto es el estándar de oro de la ciencia moderna."),
              tags$li(strong("Visualización de Calidad para Publicación:"), " Con paquetes como `ggplot2`, puedes crear gráficos complejos y estéticamente pulcros, listos para ser incluidos en tesis, informes y publicaciones de alto impacto."),
              tags$li(strong("Costo y Acceso:"), " R y RStudio son gratuitos y de código abierto, eliminando barreras económicas para estudiantes, investigadores e instituciones de todo el mundo.")
          ),
          
          tags$hr(),

          tags$h4(class = "section-header", "1.3 La Filosofía del `tidyverse`: Datos Ordenados para un Análisis Fluido"),
          p(
              "Dentro del ecosistema de R, el ", strong("`tidyverse`"), " es una colección de paquetes que comparten una filosofía de diseño y una gramática común, haciendo el análisis de datos más intuitivo y eficiente. Su piedra angular es el concepto de ", strong("Datos Ordenados (Tidy Data)"), " (Wickham, 2014)."
          ),
          fluidRow(
              column(5,
                  tags$img(src="images/tidy-data.png", width="200px", class="border rounded", alt="Diagrama de Tidy Data")
              ),
              column(7,
                  p("Un conjunto de datos es 'tidy' si cumple tres reglas simples:"),
                  tags$ol(
                      tags$li(strong("Cada variable forma una columna.")),
                      tags$li(strong("Cada observación forma una fila.")),
                      tags$li(strong("Cada tipo de unidad observacional forma una tabla."))
                  ),
                  tags$p("Al estructurar nuestros datos de esta manera, podemos usar un pequeño conjunto de verbos consistentes para resolver una gran variedad de problemas de manipulación de datos.")
              )
          ),
          
          tags$h5("El Ecosistema `tidyverse`:"),
          tags$p("Estos son algunos de los paquetes centrales que usaremos:"),
          tags$div(class="row text-center",
              tags$div(class="col", tags$strong(code("dplyr")), br(), "Manipulación"),
              tags$div(class="col", tags$strong(code("ggplot2")), br(), "Visualización"),
              tags$div(class="col", tags$strong(code("readr")), br(), "Importación"),
              tags$div(class="col", tags$strong(code("tidyr")), br(), "Ordenamiento"),
              tags$div(class="col", tags$strong(code("purrr")), br(), "Programación Funcional")
          ),
          
          tags$hr(),
          
          tags$h4(class="section-header", "1.4 Paquetes Estadísticos Clave para este Curso"),
          tags$p("Además del `tidyverse`, nos apoyaremos en paquetes especializados en estadística agrícola:"),
          tags$ul(
              tags$li(strong(code("agricolae")), ": Desarrollado por agrónomos para agrónomos. Es una navaja suiza para el diseño y análisis de experimentos, incluyendo pruebas post-hoc como Duncan y LSD, y la generación de diseños complejos."),
              tags$li(strong(code("emmeans")), ": El estándar moderno para calcular y comparar medias marginales estimadas. Es indispensable para interpretar correctamente las interacciones en diseños factoriales y mixtos."),
              tags$li(strong(code("pwr")), ": Una implementación en R de las herramientas clásicas para el análisis de poder y cálculo del tamaño de la muestra."),
              tags$li(strong(code("car")), ": Proporciona funciones avanzadas para la regresión y el ANOVA, incluyendo la robusta Prueba de Levene para la homogeneidad de varianzas.")
          )
      ),

      # ===== PESTAÑA 2: Fundamentos de R =====
      nav_panel(
        title = "2. Fundamentos de R",
        tags$h4(class = "section-header", "2.1 Operadores Fundamentales"),
        tags$div(class = "content-row",
          tags$div(class = "main-content",
            tags$p(
              "Los operadores son símbolos esenciales en R que le indican al intérprete cómo realizar operaciones específicas —ya sean matemáticas, de comparación, lógicas o de asignación— sobre valores individuales (escalares) o conjuntos de valores (como los vectores). Comprender y utilizar correctamente los operadores es el cimiento para escribir código funcional y eficiente en R (R Core Team, 2023)."
            ),
            tags$p("Los operadores se clasifican principalmente en los siguientes grupos:"),
            tags$ul(
              tags$li(
                tags$b("Operadores Aritméticos:"), " Estos operadores realizan cálculos numéricos básicos. Incluyen: \\(+\\) (suma), \\(-\\) (resta), \\(*\\) (multiplicación), \\(/\\) (división), \\(\\^\\) o \\(**\\) (exponenciación), \\(%%\\) (módulo, que devuelve el resto de una división), y \\(%/%\\) (división entera, que devuelve la parte entera de una división). Una característica poderosa de R es que estos operadores son ", tags$em("vectorizados"), ", lo que significa que pueden operar sobre cada elemento de un vector automáticamente, sin necesidad de bucles explícitos."
              ),
              tags$pre(
                class = "r-code",
                htmltools::HTML(
                  "# Datos agronómicos de ejemplo\n",
                  "area_hectareas <- 2.5\n",
                  "rendimiento_por_hectarea_kg <- 3500 # kg/ha\n",
                  "produccion_total_kg <- area_hectareas * rendimiento_por_hectarea_kg\n",
                  "cat('Producción Total (kg):', produccion_total_kg, '\\n')\n",
                  "\n",
                  "costo_semillas_por_ha <- 120 # USD\n",
                  "costo_fertilizantes_por_ha <- 250 # USD\n",
                  "costo_total_insumos_por_ha <- costo_semillas_por_ha + costo_fertilizantes_por_ha\n",
                  "cat('Costo Total Insumos (USD/ha):', costo_total_insumos_por_ha, '\\n')\n",
                  "\n",
                  "# Exponenciación: Si una población de insectos se duplica cada generación (2^n)\n",
                  "generaciones <- 4\n",
                  "factor_crecimiento_poblacional <- 2^generaciones\n",
                  "cat('Factor de Crecimiento en', generaciones, 'generaciones:', factor_crecimiento_poblacional, '\\n')\n",
                  "\n",
                  "# Módulo y división entera: distribuir 26 plantas en filas de 5\n",
                  "total_plantas <- 26\n",
                  "plantas_por_fila <- 5\n",
                  "filas_completas <- total_plantas %/% plantas_por_fila\n",
                  "plantas_sobrantes <- total_plantas %% plantas_por_fila\n",
                  "cat('Filas completas:', filas_completas, '- Plantas sobrantes:', plantas_sobrantes, '\\n')"
                )
              ),
              tags$p(tags$strong("Salida esperada:")),
              tags$pre(
                class = "r-output",
                htmltools::HTML(
                  "Producción Total (kg): 8750 \n",
                  "Costo Total Insumos (USD/ha): 370 \n",
                  "Factor de Crecimiento en 4 generaciones: 16 \n",
                  "Filas completas: 5 - Plantas sobrantes: 1 "
                )
              ),
              tags$li(
                tags$b("Operadores Relacionales (o de Comparación):"), " Estos operadores comparan dos valores (operandos) y devuelven un resultado lógico: \\(TRUE\\) (verdadero) o \\(FALSE\\) (falso). Son: \\(>\\) (mayor que), \\(<\\) (menor que), \\(==\\) (exactamente igual a), \\(!=\\) (distinto de), \\(>=\\) (mayor o igual que), y \\(<=\\) (menor o igual que). Es crucial notar la diferencia entre \\(==\\) (comparación) y \\(=\\) (que puede ser usado para asignación, aunque \\(<- \\) es preferido)."
              ),
              tags$pre(
                class = "r-code",
                htmltools::HTML(
                  "ph_suelo_optimo_min <- 6.0\n",
                  "ph_suelo_optimo_max <- 7.0\n",
                  "ph_actual_parcela1 <- 6.5\n",
                  "ph_actual_parcela2 <- 5.5\n",
                  "\n",
                  "parcela1_en_rango <- ph_actual_parcela1 >= ph_suelo_optimo_min & ph_actual_parcela1 <= ph_suelo_optimo_max\n",
                  "cat('pH Parcela 1 dentro del rango óptimo:', parcela1_en_rango, '\\n')\n",
                  "\n",
                  "parcela2_necesita_cal <- ph_actual_parcela2 < ph_suelo_optimo_min\n",
                  "cat('pH Parcela 2 necesita enmienda cálcica:', parcela2_necesita_cal, '\\n')\n",
                  "\n",
                  "son_ph_iguales <- ph_actual_parcela1 == ph_actual_parcela2\n",
                  "cat('¿Tienen ambas parcelas el mismo pH?:', son_ph_iguales, '\\n')"
                )
              ),
              tags$p(tags$strong("Salida esperada:")),
              tags$pre(
                class = "r-output",
                htmltools::HTML(
                  "pH Parcela 1 dentro del rango óptimo: TRUE \n",
                  "pH Parcela 2 necesita enmienda cálcica: TRUE \n",
                  "¿Tienen ambas parcelas el mismo pH?: FALSE "
                )
              ),
              tags$li(
                tags$b("Operadores Lógicos:"), " Se utilizan para combinar o modificar expresiones lógicas (que evalúan a \\(TRUE\\) o \\(FALSE\\)). Los principales son: \\(&\\) (Y lógico, vectorizado: \\(TRUE\\) si ambos operandos son \\(TRUE\\)), \\(|\\) (O lógico, vectorizado: \\(TRUE\\) si al menos un operando es \\(TRUE\\)), y \\(!\\) (NO lógico o negación, invierte el valor lógico). Existen también \\(&&\\) (Y lógico de cortocircuito) y \\(||\\) (O lógico de cortocircuito), que evalúan solo el primer elemento de los vectores y son más eficientes en estructuras de control como los condicionales \\(if\\), ya que dejan de evaluar tan pronto como el resultado es determinado."
              ),
              tags$pre(
                class = "r-code",
                htmltools::HTML(
                  "lluvia_suficiente <- TRUE\n",
                  "temperatura_adecuada <- FALSE\n",
                  "control_plagas_realizado <- TRUE\n",
                  "\n",
                  "# Condiciones ideales para siembra: lluvia Y temperatura adecuadas\n",
                  "condiciones_siembra_ideales <- lluvia_suficiente & temperatura_adecuada\n",
                  "cat('Condiciones de siembra ideales:', condiciones_siembra_ideales, '\\n')\n",
                  "\n",
                  "# Se puede proceder si hay lluvia O se realizó control de plagas (ejemplo simplificado)\n",
                  "se_puede_proceder <- lluvia_suficiente | control_plagas_realizado\n",
                  "cat('Se puede proceder con alguna condición favorable:', se_puede_proceder, '\\n')\n",
                  "\n",
                  "# Negación: ¿No hay temperatura adecuada?\n",
                  "no_temperatura_adecuada <- !temperatura_adecuada\n",
                  "cat('¿No hay temperatura adecuada?:', no_temperatura_adecuada, '\\n')"
                )
              ),
              tags$p(tags$strong("Salida esperada:")),
              tags$pre(
                class = "r-output",
                htmltools::HTML(
                  "Condiciones de siembra ideales: FALSE \n",
                  "Se puede proceder con alguna condición favorable: TRUE \n",
                  "¿No hay temperatura adecuada?: TRUE "
                )
              ),
              tags$li(
                tags$b("Operadores de Asignación:"), " Se utilizan para asignar un valor a un nombre de variable (u objeto) en R. El operador de asignación canónico y más recomendado en R es \\(<- \\) (la flecha apuntando hacia la variable). Aunque \\(=\\) también puede usarse para asignación en el nivel superior, se desaconseja para mantener la claridad y distinguirlo de la asignación de argumentos en llamadas a funciones. Los operadores \\(->\\) (flecha derecha), \\(<<-\\) y \\(->>\\) (asignación en entornos globales o superiores) existen pero se usan con menos frecuencia y requieren una comprensión más profunda de los entornos en R."
              ),
              tags$pre(
                class = "r-code",
                htmltools::HTML(
                  "nombre_variedad_trigo <- 'Klein Centauro'\n",
                  "ciclo_dias <- 150\n",
                  "tolerante_sequia <- FALSE\n",
                  "\n",
                  "cat('Variedad:', nombre_variedad_trigo, \n",
                  "    '| Ciclo (días):', ciclo_dias, \n",
                  "    '| Tolerante a Sequía:', tolerante_sequia, '\\n')"
                )
              ),
              tags$p(tags$strong("Salida esperada:")),
              tags$pre(
                class = "r-output",
                htmltools::HTML(
                  "Variedad: Klein Centauro | Ciclo (días): 150 | Tolerante a Sequía: FALSE "
                )
              ),
            )
          ),
          tags$div(class = "note-cloud",
            tags$strong("Precedencia de Operadores:"),
            tags$p("R evalúa las expresiones en un orden específico, conocido como precedencia de operadores. Generalmente, los paréntesis \\(()\\) tienen la precedencia más alta, forzando la evaluación de su contenido primero. Luego vienen la exponenciación \\(\\^\\), seguida de la multiplicación \\(*\\) y la división \\(/\\) (que tienen igual precedencia y se evalúan de izquierda a derecha), y después la suma \\(+\\) y la resta \\(-\\). Los operadores relacionales y lógicos tienen menor precedencia. En caso de duda o para asegurar un orden específico, ¡siempre es una buena práctica usar paréntesis \\(()\\)!"),
            tags$code("resultado <- (5 + 3) * 2  # Primero suma, luego multiplica. Resultado: 16"), br(),
            tags$code("resultado_sin_paren <- 5 + 3 * 2 # Primero multiplica, luego suma. Resultado: 11")
          )
        ),

        tags$h4(class = "section-header", "1.2 El Operador Pipe: Escribiendo Código Más Legible y Secuencial"),
        tags$div(class = "content-row",
          tags$div(class = "main-content",
            tags$p(
              "El operador \"pipe\" (tubería) es una herramienta poderosa para escribir secuencias de operaciones de una manera más intuitiva y legible. En lugar de anidar funciones (lo que puede volverse difícil de leer) o crear múltiples variables intermedias, el pipe toma el resultado de la expresión a su izquierda y lo \"entuba\" como el primer argumento de la función a su derecha."
            ),
            tags$ul(
              tags$li(tags$b("Pipe Nativo de R (\\(|>\\)):"), " Introducido oficialmente en la versión de R 4.1.0. Es simple, eficiente y no requiere la carga de paquetes adicionales. Su sintaxis es: \\(datos |> funcion_siguiente()\\)."),
              tags$li(tags$b("Pipe de Magrittr (\\(%>\\)):"), " Proveniente del paquete `magrittr` (que es una dependencia central del `tidyverse`). Fue el pipe más popular durante mucho tiempo y sigue siendo ampliamente utilizado. Ofrece algunas funcionalidades adicionales, como el uso del marcador de posición (placeholder) punto \\(.\\) para especificar explícitamente dónde debe insertarse el resultado de la izquierda si no es el primer argumento. Su sintaxis es: \\(datos %>% funcion_siguiente()\\).")
            ),
            tags$pre(
              class = "r-code",
              htmltools::HTML(
                "# Datos de ejemplo: contenido de humedad (%) de muestras de suelo\n",
                "humedad_suelo <- c(22.5, 25.1, 19.8, 23.5, 26.2, 20.4)\n",
                "\n",
                "# SIN PIPE: calcular la media, luego redondear a un decimal\n",
                "media_humedad <- mean(humedad_suelo)\n",
                "media_humedad_redondeada_v1 <- round(media_humedad, 1)\n",
                "cat('Media de humedad (sin pipe):', media_humedad_redondeada_v1, '%\\n')\n",
                "\n",
                "# CON PIPE NATIVO |>\n",
                "media_humedad_redondeada_v2 <- humedad_suelo |> mean() |> round(1)\n",
                "cat('Media de humedad (pipe nativo |>):', media_humedad_redondeada_v2, '%\\n')\n",
                "\n",
                "# CON PIPE DE MAGRITTR %>%\n",
                "# library(magrittr) # Asegúrate de que está cargado, usualmente con library(tidyverse)\n",
                "media_humedad_redondeada_v3 <- humedad_suelo %>% mean() %>% round(1)\n",
                "cat('Media de humedad (pipe magrittr %>%):', media_humedad_redondeada_v3, '%\\n')\n",
                "\n",
                "# Ejemplo de uso del placeholder (.) con magrittr:\n",
                "# Supongamos una función que toma los datos como segundo argumento\n",
                "funcion_ejemplo <- function(parametro1, datos, parametro3) {\n",
                "  paste(parametro1, mean(datos) * parametro3)\n",
                "}\n",
                "resultado_placeholder <- humedad_suelo %>% \n",
                "  funcion_ejemplo(parametro1 = 'Resultado escalado:', ., parametro3 = 10) \n",
                "cat(resultado_placeholder, '\\n')"
              )
            ),
            tags$p(tags$strong("Salida esperada:")),
            tags$pre(
              class = "r-output",
              htmltools::HTML(
                "Media de humedad (sin pipe): 22.9 %\n",
                "Media de humedad (pipe nativo |>): 22.9 %\n",
                "Media de humedad (pipe magrittr %>%): 22.9 %\n",
                "Resultado escalado: 229.166666666667 "
              )
            )
          ),
          tags$div(class = "note-cloud",
            tags$strong("Beneficios del Pipe:"),
            tags$ul(
                tags$li(tags$b("Legibilidad:"), "El código se lee de izquierda a derecha, siguiendo el flujo de los datos, lo que lo hace más natural de entender."),
                tags$li(tags$b("Menos Variables Intermedias:"), "Reduce la necesidad de crear y nombrar múltiples objetos temporales."),
                tags$li(tags$b("Facilita la Composición:"), "Es más fácil construir secuencias complejas de transformaciones de datos.")
            ),
            tags$p("Aunque ambos pipes son muy útiles, el pipe nativo \\(|>\\) está ganando popularidad por su integración directa en R base. Sin embargo, muchos códigos existentes y paquetes del `tidyverse` utilizan extensamente el pipe \\(%>\\).")
          )
        ),
        tags$br(),
        tags$h5("Tabla Resumen: Operadores Comunes en R para Agronomía"),
        tags$table(class = "table table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Tipo de operador"),
              tags$th("Símbolo(s) Clave"),
              tags$th("Descripción y Uso Principal"),
              tags$th("Ejemplo Breve en Contexto Agronómico")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("Aritméticos"),
              tags$td("\\(+\\), \\(*\\), \\(/\\), \\(\\^\\)"),
              tags$td("Cálculos numéricos directos."),
              tags$td(tags$code("dosis_total_N <- N_base + N_adicional"))
            ),
            tags$tr(
              tags$td("Relacionales"),
              tags$td("\\(>=\\), \\(<=\\), \\(==\\)"),
              tags$td("Comparaciones que resultan en \\(TRUE\\)/\\(FALSE\\)."),
              tags$td(tags$code("humedad_suelo_actual >= punto_marchitez"))
            ),
            tags$tr(
              tags$td("Lógicos"),
              tags$td("\\(&\\), \\(|\\), \\(!\\)"),
              tags$td("Combinar o negar condiciones lógicas."),
              tags$td(tags$code("(temperatura > 10) & (precipitacion > 5)"))
            ),
            tags$tr(
              tags$td("Asignación"),
              tags$td("\\(<- \\)"),
              tags$td("Almacenar un valor o resultado en una variable."),
              tags$td(tags$code("biomasa_aerea_g <- 57.3"))
            ),
            tags$tr(
              tags$td("Pipe"),
              tags$td("\\(|>\\), \\(%>\\)"),
              tags$td("Encadenar funciones para un flujo de datos legible."),
              tags$td(tags$code("datos_campo |> calcular_indices() |> graficar_resultados()"))
            )
          )
        ),
        
        tags$h4(class = "section-header", "2.2 Vectores: La Estructura de Datos Base"),
        h5("Creación de vectores"),
        p("Usando la función c() para concatenar elementos:"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# Numérico\n",
            "v_num <- c(2, 4, 6, 8)\n",
            "v_num\n\n",
            "# Carácter\n",
            "v_char <- c('rojo', 'verde', 'azul')\n",
            "v_char\n"
          )
        ),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 2 4 6 8\n",
            "[1] 'rojo' 'verde' 'azul'\n"
          )
        ),
        
        # Secuencias
        tags$h5("Generación automática"),
        tags$p("Con operador : y funciones seq():"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# Secuencia básica\n",
            "secuencia1 <- 1:10\n\n",
            "# Con paso definido\n",
            "secuencia2 <- seq(0, 20, by=2)\n\n",
            "# Repeticiones\n",
            "repeticiones <- rep('A', times=5)\n"
          )
        ),
        tags$p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1]  1  2  3  4  5  6  7  8  9 10\n",
            "[1]  0  2  4  6  8 10 12 14 16 18 20\n",
            "[1] 'A' 'A' 'A' 'A' 'A'\n"
          )
        ),
        
        # Indexación
        tags$h5("Acceso a elementos"),
        tags$p("Uso de índices numéricos y lógicos:"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# Por posición\n",
            "v_num[3]\n\n",
            "# Por condición\n",
            "v_num[v_num > 5]\n\n",
            "# Reemplazo\n",
            "v_char[2] <- 'amarillo'\n"
          )
        ),
        tags$p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 6\n",
            "[1] 6 8\n",
            "[1] 'rojo' 'amarillo' 'azul'\n"
          )
        ),
        
        # Operaciones vectorizadas
        tags$h5("Operaciones vectorizadas"),
        tags$p("Cálculos aplicados a todos los elementos:"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# Aritméticas\n",
            "v_num * 10\n",
            "v_num + c(1, 2, 3, 4)\n\n",
            "# Lógicas\n",
            "v_num >= 5\n"
          )
        ),
        tags$p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 20 40 60 80\n",
            "[1]  3  6  9 12\n",
            "[1] FALSE FALSE  TRUE  TRUE\n"
          )
        ),
      ),

      # ===== PESTAÑA 3: Importación y Limpieza =====
      nav_panel(
        title = "3. Importación y Limpieza",
        
        # Usamos un navset anidado para separar la teoría de la práctica
        navset_card_pill(
          header = tags$h4(class="section-header", "Flujo de Trabajo: Del Archivo Crudo al Dataset Tidy"),
          
          # --- SUB-PESTAÑA: GUÍA Y FUNCIONES ---
          nav_panel(
            title = "Guía y Funciones Clave",
            
            # --- Lectura de Datos ---
            tags$h5("Paso 1: Lectura de Datos (Importación)"),
            tags$p("El primer paso es traer los datos desde un archivo (CSV, Excel) a R. El `tidyverse` ofrece funciones modernas y eficientes para esto."),
            tags$div(class="row",
                column(6, 
                    tags$h6("Lectura de CSV con `readr`"),
                    tags$pre(class="r-code", htmltools::HTML("# Tidyverse (readr)\nlibrary(readr)\ndf_tidy <- read_csv('data/datos.csv')"))
                ),
                column(6,
                    tags$h6("Lectura de Excel con `readxl`"),
                    tags$pre(class="r-code", htmltools::HTML("# library(readxl)\ndf_excel <- read_excel('data/datos.xlsx', sheet = 'Hoja1')"))
                )
            ),
            
            # --- Limpieza de Nombres ---
            tags$h5("Paso 2: Estandarización de Nombres con `janitor`"),
            tags$p("Los nombres de columnas de archivos de campo o Excel suelen tener espacios, mayúsculas o caracteres especiales (ej. 'Rendimiento (kg/ha)'). Esto dificulta la programación. La función ", code("janitor::clean_names()"), " soluciona esto automáticamente."),
            tags$pre(class="r-code", htmltools::HTML(
                "library(janitor)\n# Antes: data.frame con nombres como 'ID Parcela' y 'Rendimiento (kg/ha)'\ndatos_limpios <- datos_crudos %>% clean_names()\n# Después: nombres como 'id_parcela' y 'rendimiento_kg_ha'"
            )),
            
            # --- Manejo de NAs (NUEVA SECCIÓN) ---
            tags$h5("Paso 3: Detección y Manejo de Valores Faltantes (NA)"),
            tags$p("Los datos del mundo real casi siempre están incompletos. Los valores faltantes en R se representan como `NA`. Ignorarlos puede llevar a errores o resultados sesgados. El primer paso es siempre detectar su presencia."),
            tags$ul(
                tags$li(strong("Contar NAs por columna:"), code("colSums(is.na(df))"), " Te da un resumen rápido de qué columnas tienen problemas."),
                tags$li(strong("Filtrar filas completas:"), code("na.omit(df)"), " o ", code("dplyr::drop_na(df)"), ". Es la solución más simple, pero puede ser peligrosa si pierdes muchas observaciones."),
                tags$li(strong("Imputación Simple:"), " Reemplazar los `NA` con un valor, como la media o la mediana de la columna. Es una técnica básica que debe usarse con precaución, ya que puede reducir artificialmente la varianza de los datos. Ejemplo: ", code("df %>% mutate(columna = ifelse(is.na(columna), mean(columna, na.rm=T), columna))"))
            ),
            
            # --- Conversión de Tipos ---
            tags$h5("Paso 4: Verificación y Coerción de Tipos de Datos"),
            tags$p("A menudo, R puede interpretar incorrectamente una columna (ej. leer una variable numérica como texto si contiene un carácter erróneo). Es crucial verificar y corregir los tipos de datos."),
            tags$p("Las funciones clave son las de la familia `as.*`, como ", code("as.numeric()"), ", ", code("as.factor()"), " y ", code("as.Date()"), ", usualmente dentro de un `dplyr::mutate()`."),
            tags$pre(class="r-code", htmltools::HTML(
                "datos_finales <- datos_limpios %>%\n  mutate(\n    tratamiento = as.factor(tratamiento),\n    rendimiento = as.numeric(rendimiento)\n  )"
            )),
            
            # --- Inspección Final ---
            tags$h5("Paso 5: Inspección Final"),
            tags$p("Después de la limpieza, siempre debemos volver a inspeccionar los datos para asegurarnos de que la estructura es la correcta."),
            tags$table(class="table",
                tags$thead(tags$tr(tags$th("Función"), tags$th("Propósito"))),
                tags$tbody(
                  tags$tr(tags$td(code("glimpse(df)")), tags$td("Vista compacta y rápida de cada columna, su tipo y algunos valores de ejemplo.")),
                  tags$tr(tags$td(code("summary(df)")), tags$td("Resumen estadístico para cada columna (mín, máx, media, etc. para numéricas; frecuencias para factores).")),
                  tags$tr(tags$td(code("head(df)")), tags$td("Muestra las primeras 6 filas del dataset."))
                )
              )
          ),
            
          # --- SUB-PESTAÑA: PRÁCTICA INTERACTIVA ---
          nav_panel(
            title = "Práctica Interactiva",
            # Aquí va el sidebarLayout que ya habíamos diseñado
            sidebarLayout(
              sidebarPanel(
                width = 4,
                tags$h5("Paso 1: Carga de Datos"),
                fileInput(ns("file_upload"), "Sube tu archivo CSV",
                          accept = c("text/csv", ".csv")),
                checkboxInput(ns("use_demo_data"), "O usar datos de demostración (rendimiento de maíz)", TRUE),
                tags$hr(),
                tags$h5("Paso 2: Opciones de Limpieza"),
                checkboxInput(ns("clean_names"), "Estandarizar nombres (janitor::clean_names())", TRUE),
                selectInput(ns("na_action"), "Manejo de Valores Faltantes (NA):",
                            choices = c("No hacer nada" = "none",
                                        "Eliminar filas con NAs" = "omit",
                                        "Imputar con la media (solo numérico)" = "impute_mean")),
                actionButton(ns("process_data"), "Procesar Datos", icon=icon("sync"), class="btn-primary w-100")
              ),
              mainPanel(
                width = 8,
                tags$h5("Paso 3: Inspección del Dataset"),
                tags$p("Una vez procesado, examinamos la estructura del data.frame resultante."),
                tags$h6("Vista Rápida (`glimpse`):"),
                verbatimTextOutput(ns("glimpse_output")),
                tags$h6("Resumen Estadístico (`summary`):"),
                verbatimTextOutput(ns("summary_output")),
                tags$h6("Conteo de Valores Faltantes (NAs) ANTES del manejo:"),
                verbatimTextOutput(ns("na_count_output")),
                tags$h6("Primeras Filas del Dataset Limpio:"),
                DT::dataTableOutput(ns("cleaned_data_table"))
              )
            )
          )
        )
      ),

      # ===== PESTAÑA 4: Exploración Visual =====
      nav_panel(
        title = "4. Exploración Visual",
        
        navset_card_pill(
          header = tags$h4(class="section-header", "Visualizando para Entender: Base R vs. `ggplot2`"),
          
          # --- SUB-PESTAÑA: GUÍA Y COMPARACIÓN ---
          nav_panel(
            title = "Guía y Comparación de Sistemas",
            
            tags$h5("Dos Filosofías para Graficar en R"),
            tags$p("R ofrece dos sistemas principales para crear gráficos. Entender sus diferencias es clave para elegir la herramienta adecuada para cada tarea."),
            
            fluidRow(
              column(6,
                  div(class="card h-100",
                    div(class="card-header", strong("Sistema de Gráficos Base (`plot`, `hist`, etc.)")),
                    div(class="card-body",
                      tags$h6("Filosofía: 'El Lienzo en Blanco'"),
                      tags$p("Las funciones base como `plot()` actúan como un pintor que dibuja directamente sobre un lienzo. Primero creas el gráfico principal, y luego añades elementos (puntos, líneas, leyendas) como capas sucesivas. Es rápido para gráficos simples y exploratorios."),
                      tags$p(strong("Ventajas:"), " Rápido, intuitivo para gráficos sencillos, no requiere paquetes adicionales."),
                      tags$p(strong("Desventajas:"), " La personalización avanzada es engorrosa, es difícil crear leyendas complejas y no se integra naturalmente con el `tidyverse`.")
                    )
                  )
                ),
                column(6,
                  tags$div(class="card h-100",
                    tags$div(class="card-header", strong("Sistema `ggplot2` (Parte del `tidyverse`)")),
                    tags$div(class="card-body",
                      tags$h6("Filosofía: 'La Gramática de Gráficos'"),
                      tags$p("`ggplot2` trata los gráficos como una composición de capas. Tú no dibujas, sino que describes los componentes del gráfico: los datos, el mapeo estético (`aes`) de variables a elementos visuales, y las capas geométricas (`geom`). Es un enfoque más estructurado y potente (Wickham, 2016)."),
                      tags$p(strong("Ventajas:"), " Extremadamente potente y flexible, personalización casi ilimitada, manejo automático de leyendas, integración perfecta con el `tidyverse`."),
                      tags$p(strong("Desventajas:"), " Curva de aprendizaje inicial más pronunciada, puede ser más verboso para gráficos muy simples.")
                    )
                  )
                )
            ),
              
              tags$hr(),
              tags$h5("Comparación Lado a Lado"),
              tags$p("Veamos cómo crear un gráfico de dispersión simple con ambos sistemas."),
              
              fluidRow(
                  column(6,
                    tags$h6("Base R: `plot()`"),
                    tags$pre(
                      class="r-code", 
                      htmltools::HTML(
                        "plot(x = iris$Petal.Length, y = iris$Petal.Width, \n",
                        "     main = 'Largo vs. Ancho del Pétalo',\n",
                        "     xlab = 'Largo del Pétalo (cm)',\n",
                        "     ylab = 'Ancho del Pétalo (cm)',\n",
                        "     col = iris$Species, # Color por especie\n",
                        "     pch = 19) # Símbolo de punto sólido\n",
                        "legend('topleft', legend = levels(iris$Species), \n",
                        "       col = 1:3, pch = 19)"
                      )
                    )
                  ),
                  column(6,
                      tags$h6("`ggplot2`"),
                      tags$pre(
                        class="r-code", 
                        htmltools::HTML(
                          "ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) +\n",
                          "  geom_point(aes(color = Species), size = 3, alpha = 0.7) +\n",
                          "  labs(title = 'Largo vs. Ancho del Pétalo',\n",
                          "       x = 'Largo del Pétalo (cm)',\n",
                          "       y = 'Ancho del Pétalo (cm)',\n",
                          "       color = 'Especie') +\n",
                          "  theme_minimal()"
                        )
                      )
                    )
              )
            ),
            
            # --- SUB-PESTAÑA: PRÁCTICA INTERACTIVA ---
          nav_panel(
            title = "Práctica Interactiva",
            p("Usa los controles para explorar el dataset que procesaste en la Pestaña 3. Elige el tipo de gráfico, las variables y el sistema de graficación para generar visualizaciones y ver el código correspondiente."),
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    tags$h5("Controles de Visualización"),
                    
                    # 1. Selector del tipo de gráfico
                    selectInput(ns("plot_type"), "Tipo de Gráfico:",
                                choices = c("Histograma", "Gráfico de Densidad", "Boxplot", "Gráfico de Dispersión")),
                    
                    # 2. Selector del sistema de graficación
                    radioButtons(ns("plot_system"), "Sistema de Graficación:",
                                 choices = c("ggplot2 (Interactivo)" = "ggplot2", 
                                             "Base R" = "base_r"), 
                                 selected = "ggplot2"),
                    
                    # 3. UI dinámico para las variables, que dependerá del tipo de gráfico
                    uiOutput(ns("plot_controls_ui"))
                ),
                mainPanel(
                    width = 9,
                    # Salida del gráfico (usaremos plotlyOutput para ggplot)
                    uiOutput(ns("plot_output_ui")),
                    
                    # Salida para el código R correspondiente
                    h5(class="mt-4", "Código R Utilizado:"),
                    verbatimTextOutput(ns("plot_code_output"))
                )
            )
          )
        )
      ),

      # ===== PESTAÑA FINAL: Recursos y Próximos Pasos =====
      nav_panel(
          title = "Recursos y Próximos Pasos",
          icon = icon("graduation-cap"),
          
          tags$h4(class = "section-header", "Ampliando tus Horizontes en R y la Estadística Agronómica"),
          tags$p(
              "¡Felicitaciones por completar el contenido del curso! El viaje del análisis de datos es continuo. Esta sección está diseñada para ser tu guía de referencia, con recursos cuidadosamente seleccionados para que puedas profundizar en los temas que más te interesen y seguir desarrollando tus habilidades."
          ),
          
          tags$hr(),
          
          fluidRow(
              # --- Columna 1: Fundamentos de R y Tidyverse ---
              column(4,
                  tags$div(class="card h-100",
                      tags$div(class="card-header bg-primary text-white", strong("Fundamentos de R y `tidyverse`")),
                      tags$div(class="card-body",
                          tags$h6("Libros Esenciales (Online y Gratuitos)"),
                          tags$ul(
                              tags$li(tags$a("R for Data Science (2nd Edition)", href="https://r4ds.hadley.nz/", target="_blank"), " - La 'biblia' del tidyverse, escrita por sus creadores. Indispensable."),
                              tags$li(tags$a("Advanced R", href="https://adv-r.hadley.nz/", target="_blank"), " - Para cuando quieras entender cómo funciona R 'por dentro'."),
                              tags$li(tags$a("R Cookbook, 2nd Edition", href="https://rc2e.com/", target="_blank"), " - Un recetario lleno de soluciones prácticas a problemas comunes.")
                          ),
                          tags$h6("Hojas de Trucos (Cheatsheets) de Posit"),
                          tags$ul(
                              tags$li(tags$a("Data Wrangling with dplyr", href="https://posit.co/resources/cheatsheets/data-wrangling-cheatsheet/", target="_blank")),
                              tags$li(tags$a("Data Visualization with ggplot2", href="https://posit.co/resources/cheatsheets/data-visualization-2-1/", target="_blank")),
                              tags$li(tags$a("RStudio IDE", href="https://posit.co/resources/cheatsheets/rstudio-ide/", target="_blank"))
                          )
                      )
                  )
              ),
              
              # --- Columna 2: Estadística y Diseño Experimental ---
              column(4,
                  tags$div(class="card h-100",
                      tags$div(class="card-header bg-success text-white", strong("Estadística y Diseño Experimental")),
                      tags$div(class="card-body",
                          tags$h6("Libros Clásicos de Referencia"),
                          tags$ul(
                              tags$li("Montgomery, D. C. (2017). ", em("Design and Analysis of Experiments.")),
                              tags$li("Kuehl, R. O. (2000). ", em("Design of experiments: Statistical principles of research design and analysis.")),
                              tags$li("Gomez, K. A., & Gomez, A. A. (1984). ", em("Statistical Procedures for Agricultural Research."))
                          ),
                          tags$h6("Recursos Específicos para Agronomía"),
                          tags$ul(
                              tags$li(tags$a("The R-Podcast", href="https://r-podcast.org/", target="_blank"), " - Episodios sobre aplicaciones de R en diferentes campos."),
                              tags$li(tags$a("CRAN Task View: Agriculture", href="https://cran.r-project.org/web/views/Agriculture.html", target="_blank"), " - Una lista curada de paquetes de R para la ciencia agrícola.")
                          )
                      )
                  )
              ),
              
              # --- Columna 3: Temas Avanzados y Comunidad ---
              column(4,
                  tags$div(class="card h-100",
                      tags$div(class="card-header bg-info text-dark", strong("Temas Avanzados y Comunidad")),
                      tags$div(class="card-body",
                          tags$h6("Modelos Mixtos y GLMM"),
                          tags$p("Para analizar diseños más complejos (bloques incompletos, efectos aleatorios), los modelos mixtos son la herramienta estándar."),
                          tags$ul(
                              tags$li("Paquetes clave: ", code("lme4"), " y ", code("nlme"), "."),
                              tags$li(tags$a("Tutorial sobre GLMM en R", href="https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html", target="_blank"), " (por Ben Bolker).")
                          ),
                          tags$h6("Únete a la Comunidad de R"),
                          tags$ul(
                              tags$li(tags$a("R-bloggers", href="https://www.r-bloggers.com/", target="_blank"), " - Un agregador de cientos de blogs sobre R."),
                              tags$li(tags$a("Stack Overflow (etiqueta [r])", href="https://stackoverflow.com/questions/tagged/r", target="_blank"), " - Para resolver dudas de programación."),
                              tags$li("Busca grupos locales de usuarios de R (R-Ladies, etc.) o conferencias como `useR!` y `Posit::conf`.")
                          )
                      )
                  )
              )
          ),
          
          tags$hr(),
          
          tags$h4(class="section-header", "Próximos Pasos Sugeridos"),
          tags$ol(
              tags$li(strong("Practica con tus Propios Datos:"), " La mejor manera de aprender es aplicar estas técnicas a un conjunto de datos que conozcas bien. Intenta importar, limpiar y analizar un archivo de Excel de uno de tus propios experimentos."),
              tags$li(strong("Domina `dplyr` y `ggplot2`:"), " Dedica tiempo a aprender más sobre la manipulación de datos con `dplyr` y la visualización con `ggplot2`. Son las dos habilidades que más retorno te darán en tu día a día."),
              tags$li(strong("Aprende a Crear Informes con R Markdown:"), " El siguiente paso lógico es aprender a combinar tu código, tus resultados y tu texto en un único informe reproducible usando R Markdown o Quarto. Esto cambiará por completo la forma en que comunicas tus resultados."),
              tags$li(strong("No Tengas Miedo de Pedir Ayuda:"), " La comunidad de R es increíblemente abierta y colaborativa. Usa los foros y las redes para hacer preguntas.")
          )
      )
    )
  )
}

session1Server <- function(input, output, session) {
  # Namespace para los IDs de los inputs y outputs
  ns <- session$ns
        
  # --- LÓGICA PARA LA PESTAÑA 3: IMPORTACIÓN Y LIMPIEZA ---

  # 1. Usar un reactiveVal para almacenar los datos procesados.
  # Lo inicializamos con los datos de demostración ya limpios.
  
  # Función para limpiar los datos de demostración
  get_demo_data <- function() {
      df <- data.frame(
          `ID Parcela` = 1:20,
          `Rendimiento (kg/ha)` = c(5500, 5800, 5300, NA, 6200, 5600, 5900, 4800, 5700, 6100, 5400, 5850, "error", 6300, 5550, NA, 6000, 5950, 5250, 6150),
          `Humedad Cosecha (%)` = runif(20, 14.5, 18.0),
          `Fecha Siembra` = "2023-10-15",
          check.names = FALSE
      )
      # Limpieza básica inicial
      df %>%
          janitor::clean_names() %>%
          mutate(rendimiento_kg_ha = as.numeric(as.character(rendimiento_kg_ha)))
  }
  
  # Inicializar el reactiveVal
  processed_data_rv <- reactiveVal(get_demo_data())

  # 2. Usar observeEvent para manejar el procesamiento de datos
  observeEvent(input$process_data, {
      
      df <- if (input$use_demo_data) {
          get_demo_data() # Cargar el demo limpio de nuevo
      } else {
          req(input$file_upload)
          # Limpiar los datos subidos
          janitor::clean_names(read.csv(input$file_upload$datapath))
      }
      
      # Guardar el conteo de NAs ANTES de cualquier acción
      na_counts_before(colSums(is.na(df)))
      
      # Aplicar la acción de manejo de NAs seleccionada
      if (input$na_action == "omit") {
          df <- na.omit(df)
      } else if (input$na_action == "impute_mean") {
          df <- df %>%
              mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .)))
      }
      
      # Actualizar el reactiveVal con los nuevos datos procesados
      processed_data_rv(df)
  })

  # Reactive para el conteo de NAs
  na_counts_before <- reactiveVal(colSums(is.na(get_demo_data())))

  # 3. Salidas para la inspección
  output$glimpse_output <- renderPrint({
      req(processed_data_rv())
      glimpse(processed_data_rv())
  })
  
  output$summary_output <- renderPrint({
      req(processed_data_rv())
      summary(processed_data_rv())
  })
  
  output$na_count_output <- renderPrint({
      req(na_counts_before())
      na_counts_before()
  })
  
  output$cleaned_data_table <- DT::renderDataTable({
      req(processed_data_rv())
      DT::datatable(processed_data_rv(), options=list(pageLength=5, scrollX=TRUE))
  })
  
  # --- LÓGICA PARA LA PESTAÑA 4: EXPLORACIÓN VISUAL ---

  # UI dinámico para los controles del gráfico. Ahora depende de processed_data_rv()
  output$plot_controls_ui <- renderUI({
      df <- processed_data_rv()
      req(df)
      
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      categorical_cols <- names(df)[sapply(df, function(c) is.factor(c) || is.character(c))]
      
      tagList(
          if (input$plot_type %in% c("Histograma", "Gráfico de Densidad")) {
              selectInput(ns("plot_x_hist"), "Variable Numérica:", choices = numeric_cols)
          },
          if (input$plot_type == "Boxplot") {
              tagList(
                  selectInput(ns("plot_x_box"), "Eje X (Categórico):", choices = categorical_cols),
                  selectInput(ns("plot_y_box"), "Eje Y (Numérico):", choices = numeric_cols)
              )
          },
          if (input$plot_type == "Gráfico de Dispersión") {
              tagList(
                  selectInput(ns("plot_x_scatter"), "Eje X (Numérico):", choices = numeric_cols),
                  selectInput(ns("plot_y_scatter"), "Eje Y (Numérico):", choices = numeric_cols, selected = if(length(numeric_cols)>1) numeric_cols[2] else numeric_cols[1])
              )
          },
          selectInput(ns("plot_color"), "Variable para Agrupar/Colorear:", 
                      choices = c("Ninguna", categorical_cols), selected = "Ninguna")
      )
  })
  
  # UI dinámico para el contenedor del gráfico
  output$plot_output_ui <- renderUI({
      ns <- session$ns # Asegurar que ns esté definido
      if (input$plot_system == "ggplot2") {
          plotly::plotlyOutput(ns("exploratory_plotly"))
      } else {
          plotOutput(ns("exploratory_plot_base"))
      }
  })
  
  # Reactive que genera el gráfico y el código
  reactive_plot_and_code <- reactive({
    df <- processed_data_rv()
    # Usar req con cancelOutput para detenerse limpiamente si los inputs no están listos
    req(df, input$plot_type, input$plot_system, cancelOutput = TRUE)
      
    code_string <- "# Selecciona un tipo de gráfico y variables"
    plot_obj <- NULL
    
    # Lógica para generar el gráfico y el código correspondiente
    tryCatch({
      if (input$plot_type == "Histograma") {
        req(input$plot_x_hist)
        if (input$plot_system == "ggplot2") {
          code_string <- glue::glue(
            "ggplot(df, aes(x = {input$plot_x_hist})) +\n  geom_histogram(aes(fill = ..count..), bins=20, alpha=0.7) +\n  scale_fill_gradient('Conteo', low='blue', high='red') +\n  labs(title = 'Histograma de {input$plot_x_hist}')"
          )
          plot_obj <- ggplot(df, aes_string(x = input$plot_x_hist)) + 
            geom_histogram(aes(fill = ..count..), bins=20, alpha=0.7) + 
            scale_fill_gradient("Conteo", low="blue", high="red") +
            labs(title = paste("Histograma de", input$plot_x_hist)) + theme_minimal()
        } else {
          code_string <- glue::glue("hist(df${input$plot_x_hist}, \n     main = 'Histograma de {input$plot_x_hist}', \n     xlab = '{input$plot_x_hist}', \n     col = 'skyblue', \n     breaks = 20)")
          # Se necesita un hack para capturar el gráfico base
          plot_obj <- recordPlot({ hist(df[[input$plot_x_hist]], main = paste("Histograma de", input$plot_x_hist), xlab = input$plot_x_hist, col = "skyblue", breaks = 20) })
        }
      } else if (input$plot_type == "Boxplot") {
        req(input$plot_x_box, input$plot_y_box)
        if (input$plot_system == "ggplot2") {
          code_string <- glue::glue("ggplot(df, aes(x = {input$plot_x_box}, y = {input$plot_y_box})) +\n  geom_boxplot(aes(fill = {input$plot_x_box}), alpha=0.7, show.legend=F)")
          plot_obj <- ggplot(df, aes_string(x = input$plot_x_box, y = input$plot_y_box)) + 
              geom_boxplot(aes_string(fill = input$plot_x_box), alpha=0.7, show.legend=FALSE) + theme_minimal()
        } else {
          code_string <- glue::glue("boxplot({input$plot_y_box} ~ {input$plot_x_box}, data = df, \n        main='Boxplot', col='lightblue')")
          plot_obj <- recordPlot({ boxplot(as.formula(paste(input$plot_y_box, "~", input$plot_x_box)), data = df, main="Boxplot", col="lightblue") })
        }
      } else if (input$plot_type == "Gráfico de Dispersión") {
        req(input$plot_x_scatter, input$plot_y_scatter)
        color_aes <- if(input$plot_color != "Ninguna") glue::glue(", color = {input$plot_color}") else ""
        
        if (input$plot_system == "ggplot2") {
          code_string <- glue::glue("ggplot(df, aes(x = {input$plot_x_scatter}, y = {input$plot_y_scatter})) +\n  geom_point(aes(color = {if(input$plot_color != 'Ninguna') input$plot_color else 'NULL'}), alpha=0.7, size=3)")
          plot_obj <- ggplot(df, aes_string(x = input$plot_x_scatter, y = input$plot_y_scatter))
          if(input$plot_color != "Ninguna") {
              plot_obj <- plot_obj + geom_point(aes_string(color = input$plot_color), alpha=0.7, size=3)
          } else {
              plot_obj <- plot_obj + geom_point(alpha=0.7, size=3)
          }
          plot_obj <- plot_obj + theme_minimal()
        } else {
          color_code <- if(input$plot_color != "Ninguna") glue::glue("col = as.factor(df${input$plot_color})") else "col = 'black'"
          code_string <- glue::glue("plot(df${input$plot_x_scatter}, df${input$plot_y_scatter}, \n     {color_code}, pch=19)")
          plot_obj <- recordPlot({ plot(df[[input$plot_x_scatter]], df[[input$plot_y_scatter]], col = if(input$plot_color != "Ninguna") as.factor(df[[input$plot_color]]) else "black", pch=19, xlab=input$plot_x_scatter, ylab=input$plot_y_scatter) })
        }
      } # Añadir más tipos de gráficos aquí si se desea (ej. Densidad)
    }, error = function(e) {
          # Manejar errores de graficación
          code_string <- paste("# Error al generar el gráfico:\n#", e$message)
          plot_obj <- NULL
      })
      
      list(code = code_string, plot = plot_obj)
  })

  # Renderizar los outputs
  output$exploratory_plotly <- plotly::renderPlotly({
      res <- reactive_plot_and_code()
      req(res, res$plot, inherits(res$plot, "ggplot"))
      plotly::ggplotly(res$plot)
  })
  
  output$exploratory_plot_base <- renderPlot({
      res <- reactive_plot_and_code()
      req(res, res$plot, inherits(res$plot, "recordedplot"))
      replayPlot(res$plot)
  })

  output$plot_code_output <- renderText({
      res <- reactive_plot_and_code()
      req(res)
      res$code
  })
}