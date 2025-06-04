# R/modules/session1.R
library(ggplot2)

session1UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 1: Importación y Exploración de Datos en R")
    ),
    
    # Usar navset_tab con nav_panel de bslib
    navset_tab(
      # ——————————————
      # PESTAÑA: TEMARIO
      # ——————————————
      nav_panel(title = "Temario",
        h4(class = "section-header", "Temario"),
        tags$table(class = "table activity-table",
          tags$thead(tags$tr(tags$th("Segmento"), tags$th("Tiempo"), tags$th("Actividad"))),
          tags$tbody(
            tags$tr(tags$td("1 Introducción a R"),    tags$td("0–20 min"),  tags$td("Operadores (aritméticos, relacionales, lógicos, asignación), precedencia. Introducción al pipe nativo \\(|>\\) y pipe de magrittr \\(%>\\).")),
            tags$tr(tags$td("2 Vectores"), tags$td("20–40 min"), tags$td("Creación de vectores (numéricos, de caracteres, lógicos) usando \\(c()\\), \\(seq()\\), \\(rep()\\). Indexación y operaciones vectorizadas elementales.")),
            tags$tr(tags$td("3 Gráficos"), tags$td("40–60 min"), tags$td("Introducción a gráficos básicos con \\(plot()\\) (ej. histograma, dispersión). Primer vistazo a \\(ggplot2\\) para la misma tarea, resaltando la gramática de gráficos.")),
            tags$tr(tags$td("4 Setup"),    tags$td("60–75 min"), tags$td("Creación de un proyecto en RStudio para organización. Instalación (\\(install.packages()\\)) y carga (\\(library()\\)) de paquetes esenciales: \\(tidyverse\\), \\(readxl\\), \\(janitor\\).")),
            tags$tr(tags$td("5 Import"),   tags$td("75–95 min"), tags$td("Lectura de archivos CSV (\\(read.csv()\\), \\(read_csv()\\)) y Excel (\\(read_excel()\\)). Inspección inicial con \\(glimpse()\\), \\(str()\\), \\(summary()\\).")),
            tags$tr(tags$td("6 Limpieza"), tags$td("95–115 min"),tags$td("Uso de \\(janitor::clean_names()\\) para estandarizar nombres de columnas. Coerción básica de tipos de variables (ej. de caracter a numérico o factor).")),
            tags$tr(tags$td("7 Cierre"),   tags$td("115–120 min"),tags$td("Recapitulación de los conceptos aprendidos, resolución de dudas y avance de la Sesión 2."))
          )
        ),
      ),
      
      # ---------------------
      # PESTAÑA: 1 Sintaxis Básica
      # ---------------------
      nav_panel(
        title = "1 Sintaxis Básica",
        h4(class = "section-header", "1.1 Operadores Fundamentales en R"),
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

        h4(class = "section-header", "1.2 El Operador Pipe: Escribiendo Código Más Legible y Secuencial"),
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
        )
      ),

      # ---------------------
      # PESTAÑA: 2 Vectores
      # ---------------------
      nav_panel(title = "2 Vectores",
        h4(class = "section-header", "2 Vectores: Creación y Manipulación"),
        
        # Creación básica
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
        h5("Generación automática"),
        p("Con operador : y funciones seq():"),
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
        p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1]  1  2  3  4  5  6  7  8  9 10\n",
            "[1]  0  2  4  6  8 10 12 14 16 18 20\n",
            "[1] 'A' 'A' 'A' 'A' 'A'\n"
          )
        ),
        
        # Indexación
        h5("Acceso a elementos"),
        p("Uso de índices numéricos y lógicos:"),
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
        p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 6\n",
            "[1] 6 8\n",
            "[1] 'rojo' 'amarillo' 'azul'\n"
          )
        ),
        
        # Operaciones vectorizadas
        h5("Operaciones vectorizadas"),
        p("Cálculos aplicados a todos los elementos:"),
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
        p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 20 40 60 80\n",
            "[1]  3  6  9 12\n",
            "[1] FALSE FALSE  TRUE  TRUE\n"
          )
        ),
      ),

      # ---------------------
      # PESTAÑA: 3 Gráficos
      # ---------------------

      nav_panel(title = "3 Gráficos",
        h4(class = "section-header", "3 Gráficos: Visualización Básica"),
    
        # Sección de ejercicios interactivos
        h5(class = "section-header", "Ejercicios Interactivos"),
        p("Ajusta el tamaño de muestra y observa los gráficos:"),
        sliderInput(ns("n"), "Tamaño de muestra simulada:", min = 50, max = 500, value = 200),
        
        fluidRow(
          column(6,
            div(class = "plot-box",
              h5("Gráfico base R"),
              plotOutput(ns("histPlot"), height = "200px")
            )
          ),
          column(6,
            div(class = "plot-box",
              h5("Gráfico con ggplot2"),
              plotOutput(ns("ggPlot"), height = "200px")
            )
          )
        ),
        
        # Explicación técnica
        h5("Sistemas de graficación"),

        # Sección plot()
        tags$div(class = "graph-system",
          h6("1. Sistema base (plot())"),
          p("Funciones básicas para gráficos rápidos:"),
          
          tags$div(class = "code-explanation",
            tags$pre(
              class = "r-code",
              htmltools::HTML(
                "# Gráfico de dispersión\n",
                "x <- rnorm(50)\n",
                "y <- x + rnorm(50)\n",
                "plot(x, y,\n",
                "     main='Relación entre X e Y',\n",
                "     xlab='Variable X',\n",
                "     ylab='Variable Y',\n",
                "     pch=19,        # Tipo de punto\n",
                "     col='#2980B9', # Color\n",
                "     cex=1.2)       # Tamaño\n"
              )
            ),
            p("Salida esperada:"),
            plotOutput(ns("GrafDispersion"), height = "400px")
          ),
          
          tags$div(class = "key-args",
            p("Argumentos clave:", 
              tags$ul(
                tags$li("pch: Tipo de símbolo (19 = círculo relleno)"),
                tags$li("col: Color en formato hexadecimal"),
                tags$li("cex: Escala de tamaño (1 = default)")
              )
            )
          )
        ),

        # Sección ggplot2
        tags$div(class = "graph-system",
          h6("2. Sistema ggplot2"),
          p("Gramática de gráficos para visualización avanzada:"),
          
          tags$div(class = "code-explanation",
            tags$pre(
              class = "r-code",
              htmltools::HTML(
                "# Gráfico de dispersión con ggplot2\n",
                "library(ggplot2)\n",
                "df <- data.frame(x = rnorm(50), y = rnorm(50))\n\n",
                "ggplot(df, aes(x=x, y=y)) +\n",
                "  geom_point(\n",
                "    aes(color = x + y),  # Mapeo estético\n",
                "    size = 3,            # Tamaño fijo\n",
                "    alpha = 0.7          # Transparencia\n",
                "  ) +\n",
                "  scale_color_viridis_c() +\n",
                "  labs(\n",
                "    title = 'Relación entre X e Y',\n",
                "    subtitle = 'Ejemplo con ggplot2',\n",
                "    x = 'Variable X',\n",
                "    y = 'Variable Y'\n",
                "  ) +\n",
                "  theme_bw()\n"
              )
            ),
          p("Salida esperada:"),
          plotOutput(ns("GrafDispersionggplot2"), height = "400px")
          ),
          
          tags$div(class = "key-args",
            p("Componentes esenciales:", 
              tags$ul(
                tags$li("aes(): Mapeo de variables a propiedades visuales"),
                tags$li("geom_*(): Capas geométricas (puntos, líneas, etc)"),
                tags$li("scale_*(): Escalas de color/posición"),
                tags$li("theme(): Personalización visual")
              )
            )
          )
        ),

        # Comparativa técnica
        tags$div(class = "comparison-table",
          h6("Comparativa clave"),
          tags$table(class = "table",
            tags$thead(
              tags$tr(
                tags$th("Característica"),
                tags$th("plot()"),
                tags$th("ggplot2")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("Curva de aprendizaje"),
                tags$td("Baja"),
                tags$td("Moderada")
              ),
              tags$tr(
                tags$td("Personalización"),
                tags$td("Básica"),
                tags$td("Avanzada")
              ),
              tags$tr(
                tags$td("Reproducibilidad"),
                tags$td("Manual"),
                tags$td("Automática")
              )
            )
          )
        )
      ),
      
      # ---------------------
      # PESTAÑA: 4 Setup
      # ---------------------

      nav_panel(title = "4 Setup",
        h4(class = "section-header", "4 Setup: Configuración de Proyecto"),
        
        # Creación de proyecto
        h5("1. Creación de proyecto RStudio"),
        p("Estructura recomendada para proyectos reproducibles:"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# En RStudio:\n",
            "# File → New Project → New Directory → Empty Project\n",
            "# Nombre: mi_proyecto\n"
          )
        ),
        p("Características clave:",
          tags$ul(
            tags$li("Directorio de trabajo definido"),
            tags$li("R versionado con .Rproj"),
            tags$li("Mejora la organización de scripts y datos")
          )
        ),
          
        # Instalación de paquetes
        h5("2. Instalación de paquetes esenciales"),
        tags$div(class = "package-install",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Instalar tidyverse (colección de paquetes)\n",
              "install.packages('tidyverse')\n\n",
              "# Instalar readxl para Excel\n",
              "install.packages('readxl')\n"
            )
          ),
          p("Documentación oficial:",
            tags$ul(
              tags$li(tags$a("Tidyverse", href="https://www.tidyverse.org/", target="_blank")),
              tags$li(tags$a("Readxl", href="https://readxl.tidyverse.org/", target="_blank"))
            )
          )
        ),
          
        # Carga de librerías
        h5("3. Carga de librerías"),
        tags$div(class = "code-explanation",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Cargar tidyverse (incluye dplyr, ggplot2, etc)\n",
              "library(tidyverse)\n\n",
              "# Cargar readxl para lectura de Excel\n",
              "library(readxl)\n"
            )
          ),
          p("Funcionalidades clave:",
            tags$ul(
              tags$li("tidyverse: Manipulación y visualización de datos"),
              tags$li("readxl: Lectura de archivos .xls/.xlsx")
            )
          )
        ),
        
        # Ejemplo práctico
        h5("4. Ejemplo de uso"),
        tags$div(class = "code-example",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Leer datos con readxl\n",
              "datos_excel <- read_excel('data/datos.xlsx')\n\n",
              "# Ver estructura con tidyverse\n",
              "glimpse(datos_excel)\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "Rows: 100\n",
              "Columns: 5\n",
              "$ ID       <dbl> 1, 2, 3, ...\n",
              "$ Nombre   <chr> 'Ana', 'Luis', ...\n",
              "$ Fecha    <date> 2023-01-01, ...\n"
            )
          )
        )
      ),

      # ---------------------
      # PESTAÑA: 5 Import
      # ---------------------

      nav_panel(title = "5 Import",
        h4(class = "section-header", "5 Import: Lectura de Datos"),
        
        # CSV
        h5("1. Lectura de CSV"),
        p("Métodos para importar archivos separados por comas:"),
        tags$div(class = "code-comparison",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Base R\n",
              "df_base <- read.csv('data/datos.csv', \n",
              "                   header=TRUE, \n",
              "                   sep=',', \n",
              "                   stringsAsFactors=FALSE)\n\n",
              "# Tidyverse (readr)\n",
              "library(readr)\n",
              "df_tidy <- read_csv('data/datos.csv')\n"
            )
          ),
          p("Diferencias clave:",
            tags$ul(
              tags$li("read.csv() retorna data.frame"),
              tags$li("read_csv() retorna tibble y muestra progreso")
            )
          )
        ),
          
        # Excel
        h5("2. Lectura de Excel"),
        p("Uso de readxl para archivos .xls/.xlsx:"),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "library(readxl)\n",
            "df_excel <- read_excel('data/datos.xlsx', \n",
            "                      sheet = 'Hoja1', \n",
            "                      col_types = NULL)\n"
          )
        ),
        p("Características:",
          tags$ul(
            tags$li("Detecta automáticamente tipos de datos"),
            tags$li("Soporta formato .xls y .xlsx")
          )
        ),
        
        # Exploración
        h5("3. Exploración inicial"),
        tags$div(class = "exploration-tools",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Estructura base R\n",
              "str(df_base)\n\n",
              "# Resumen estadístico\n",
              "summary(df_excel)\n\n",
              "# Vista rápida (tidyverse)\n",
              "library(dplyr)\n",
              "glimpse(df_tidy)\n"
            )
          ),
          p("Salida esperada:",
            tags$ul(
              tags$li("str() muestra estructura y tipos de datos"),
              tags$li("summary() proporciona estadísticos descriptivos"),
              tags$li("glimpse() ofrece vista compacta con tipos de datos")
            )
          ),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "# Ejemplo glimpse() [[5]]:\n",
              "Rows: 100\n",
              "Columns: 5\n",
              "$ ID       <dbl> 1, 2, 3, ...\n",
              "$ Nombre   <chr> 'Ana', 'Luis', ...\n",
              "$ Fecha    <date> 2023-01-01, ...\n"
            )
          )
        ),
  
        # Comparativa
        tags$div(class = "comparison-table",
          h6("Funciones de exploración"),
          tags$table(class = "table",
            tags$thead(
              tags$tr(
                tags$th("Función"),
                tags$th("Propósito"),
                tags$th("Paquete")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("str()"),
                tags$td("Estructura básica del dataset"),
                tags$td("base R")
              ),
              tags$tr(
                tags$td("summary()"),
                tags$td("Estadísticos descriptivos"),
                tags$td("base R")
              ),
              tags$tr(
                tags$td("glimpse()"),
                tags$td("Vista compacta con tipos de datos"),
                tags$td("dplyr")
              )
            )
          )
        )
      ),

      # ---------------------
      # PESTAÑA: 6 Limpieza
      # ---------------------

      nav_panel(title = "6 Limpieza",
        h4(class = "section-header", "6 Limpieza: Preprocesamiento de Datos"),
        
        # Sección clean_names()
        h5("1. Estandarización de nombres con janitor"),
        p("La función ", tags$code("clean_names()"), " realiza múltiples transformaciones automáticas:", 
          tags$ul(
            tags$li("Convierte a minúsculas"),
            tags$li("Reemplaza espacios por guiones bajos"),
            tags$li("Elimina caracteres especiales"),
            tags$li("Garantiza nombres únicos")
          )
        ),
        
        tags$div(class = "code-example",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Antes\n",
              "nombres_originales <- c('Código País', 'Tasa$Crecim.', 'Fecha-Actual')\n\n",
              "# Aplicación\n",
              "library(janitor)\n",
              "nombres_limpios <- clean_names(nombres_originales)\n",
              "nombres_limpios\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "[1] 'codigo_pais'   'tasa_crecim'   'fecha_actual'\n"
            )
          ),
          p("Ventajas:",
            tags$ul(
              tags$li("Facilita la manipulación de datos"),
              tags$li("Reduce errores en nombres de columnas")
            )
          ),
          p("Desventajas:",
            tags$ul(
              tags$li("Puede perder información semántica"),
              tags$li("No es reversible")
            )
          ),
          p("Uso típico:",
            tags$ul(
              tags$li("Limpieza de nombres de columnas en dataframes"),
              tags$li("Facilita la manipulación posterior")
            )
          )
        ),
        
        # Coerción de tipos
        h5("2. Conversión de tipos de datos"),
        tags$div(
          class = "type-conversion",
          h6("Base R"),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Numérico a factor\n",
              "as.factor(df$columna)\n\n",
              "# Carácter a fecha\n",
              "as.Date('2023-01-01', format='%Y-%m-%d')\n\n",
              "# Factor a numérico\n",
              "as.numeric(as.character(df$columna))\n"
            )
          ),
          h6("Tidyverse/dplyr"),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "library(dplyr)\n\n",
              "df %>%\n",
              "  mutate(\n",
              "    columna = as.factor(columna),\n",
              "    fecha = as.Date(fecha, format='%Y-%m-%d')\n",
              "  )\n"
            )
          ),          
          p("Salida esperada:",
            tags$ul(
              tags$li("df$columna ahora es un factor"),
              tags$li("df$fecha ahora es un objeto Date")
            )
          )
        ),
        
        # Flujo completo
        h5("3. Flujo de limpieza integrado"),
        tags$div(class = "full-workflow",
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "library(tidyverse)\n",
              "library(janitor)\n\n",
              "df_limpio <- read_csv('data/datos.csv') %>%\n",
              "  clean_names() %>%\n",
              "  mutate(\n",
              "    fecha = as.Date(fecha, format='%d/%m/%Y'),\n",
              "    categoria = as.factor(categoria),\n",
              "    ingresos = as.numeric(ingresos)\n",
              "  ) %>%\n",
              "  select(-starts_with('temp_'))  # Elimina columnas temporales\n"
            )
          ),
          p("Salida esperada:",
            tags$ul(
              tags$li("df_limpio tiene nombres estandarizados"),
              tags$li("fecha es un objeto Date"),
              tags$li("categoría es un factor"),
              tags$li("ingresos son numéricos")
            )
          ),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "# A tibble: 100 × 5\n",
              "   codigo_cliente fecha       categoria ingresos\n",
              "   <chr>          <date>      <fct>        <dbl>\n",
              " 1 C001           2023-01-01  A             2500\n",
              " 2 C002           2023-01-02  B             3200\n"
            )
          )
        ),
        
        # Buenas prácticas
        h5("4. Recomendaciones"),
        tags$div(class = "best-practices",
          tags$ul(
            tags$li("Siempre verifica clases con ", tags$code("str()"), " antes de limpiar"),
            tags$li("Usa ", tags$code("across()"), " para aplicar transformaciones a múltiples columnas"),
            tags$li("Combina con ", tags$code("janitor::tabyl()"), " para análisis de frecuencias")
          )
        ),
        
        # Documentación oficial
        h5("5. Referencias técnicas"),
        tags$div(class = "documentation-links",
          tags$ul(
            tags$li(tags$a("Janitor documentation", href="https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html", target="_blank")),
            tags$li(tags$a("dplyr cheatsheet", href="https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf", target="_blank")),
            tags$li(tags$a("Data transformation in R", href="https://dplyr.tidyverse.org/reference/index.html", target="_blank"))
          )
        )
      ),

      # ---------------------
      # PESTAÑA: 7 Cierre
      # ---------------------

      nav_panel(title = "7 Cierre",
      h4(class = "section-header", "7 Cierre: Recapitulación Final"),
      
      # Resumen técnico
      h5("Recorrido completo del flujo de trabajo"),
      tags$div(class = "recap-table",
        tags$table(class = "table",
          tags$thead(
            tags$tr(
              tags$th("Segmento"),
              tags$th("Herramientas clave"),
              tags$th("Conceptos fundamentales")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("1. Intro"),
              tags$td("Operadores, |>"),
              tags$td("Sintaxis básica y vectores [[7]]")
            ),
            tags$tr(
              tags$td("2. Vectores"),
              tags$td("c(), seq(), rep()"),
              tags$td("Indexación y operaciones vectorizadas")
            ),
            tags$tr(
              tags$td("3. Gráficos"),
              tags$td("plot(), ggplot2"),
              tags$td("Visualización base vs gramática de gráficos [[4]]")
            ),
            tags$tr(
              tags$td("4. Setup"),
              tags$td("RStudio, install.packages()"),
              tags$td("Gestión de proyectos y librerías")
            ),
            tags$tr(
              tags$td("5. Import"),
              tags$td("read.csv(), read_excel()"),
              tags$td("Lectura de datos y exploración inicial")
            ),
            tags$tr(
              tags$td("6. Limpieza"),
              tags$td("janitor, dplyr"),
              tags$td("Transformación de datos y coerción de tipos")
            )
          )
        )
      ),
      
      # Preguntas frecuentes
      h5("Preguntas clave"),
      tags$div(class = "faq-section",
        tags$ul(
          tags$li("¿Cómo seleccionar columnas específicas con dplyr? ", 
                  tags$code("select(nombre_columna)"), 
                  " o usando selectores como ", tags$code("starts_with('temp_')")),
          
          tags$li("¿Qué hacer si falla la lectura de un CSV? ",
                  "Verificar encoding y separadores con ", tags$code("readr::problems()"), 
                  " y especificar ", tags$code("locale = locale(encoding = 'UTF-8')")),
          
          tags$li("¿Cómo manejar valores faltantes? ",
                  "Usar ", tags$code("janitor::remove_empty()"), 
                  " o ", tags$code("dplyr::filter(!is.na(columna))")),
          
          tags$li("¿Cómo personalizar temas en ggplot2? ",
                  "Modificar con ", tags$code("theme_minimal()"), 
                  " o crear temas personalizados con ", tags$code("theme()")),
          
          tags$li("¿Qué hacer con fechas en formatos no estándar? ",
                  "Usar ", tags$code("lubridate::parse_date_time()"), 
                  " especificando formatos como 'ymd' o 'dmy'"),
          
          tags$li("¿Cómo organizar proyectos complejos? ",
                  "Seguir el flujo ", tags$code("ProjectTemplate"), 
                  " o estructura ", tags$code("RStudio Projects"))
        )
      ),
      
      # Recursos adicionales
      h5("Para profundizar"),
      tags$div(class = "resources",
        tags$ul(
          tags$li(tags$a("R for Data Science", href="https://r4ds.had.co.nz/", target="_blank"), " "),
          tags$li(tags$a("Janitor documentation", href="https://cran.r-project.org/web/packages/janitor/vignettes/janitor.html", target="_blank"), " "),
          tags$li(tags$a("ggplot2 cheatsheet", href="https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf", target="_blank"), " ")
        )
      )
    )

    )
  )
}

session1Server <- function(input, output, session) {
  
  # --- Pestaña 1: Intro ---

  
  # ---------------------
  # Gráficos interactivos para la pestaña 3
  # ---------------------
  output$histPlot <- renderPlot({
    datos <- rnorm(input$n, mean = 100, sd = 15)
    hist(datos, 
         breaks = 30,
         main = "Distribución muestral (base R)",
         xlab = "Valor",
         col = "#2C3E50",
         border = "white")
  })
  
  output$ggPlot <- renderPlot({
    datos <- rnorm(input$n, mean = 100, sd = 15)
    ggplot(data.frame(valor = datos), aes(x = valor)) +
      geom_histogram(bins = 30, fill = "#E74C3C", color = "white") +
      labs(title = "Distribución muestral (ggplot2)",
           x = "Valor",
           y = "Frecuencia") +
      theme_minimal()
  })

  output$GrafDispersion <- renderPlot({
    # Gráfico de dispersión
    x <- rnorm(50)
    y <- x + rnorm(50)
    plot(x, y,
        main='Relación entre X e Y',
        xlab='Variable X',
        ylab='Variable Y',
        pch=19,        # Tipo de punto
        col='#2980B9', # Color
        cex=1.2)       # Tamaño
  })

  output$GrafDispersionggplot2 <- renderPlot({
    # Gráfico de dispersión con ggplot2
    df <- data.frame(x = rnorm(50), y = rnorm(50))

    ggplot(df, aes(x=x, y=y)) +
      geom_point(
        aes(color = x + y),  # Mapeo estético
        size = 3,            # Tamaño fijo
        alpha = 0.7          # Transparencia
      ) +
      scale_color_viridis_c() +
      labs(
        title = 'Relación entre X e Y',
        subtitle = 'Ejemplo con ggplot2',
        x = 'Variable X',
        y = 'Variable Y'
      ) +
      theme_bw()
  })
}