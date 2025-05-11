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
            tags$tr(tags$td("1 Intro"),    tags$td("0–20 min"),  tags$td("Sintaxis básica: operadores, |> y vectores.")),
            tags$tr(tags$td("2 Vectores"), tags$td("20–40 min"), tags$td("Crear y manipular vectores ficticios.")),
            tags$tr(tags$td("3 Gráficos"), tags$td("40–60 min"), tags$td("Plot rápido con plot() / ggplot2.")),
            tags$tr(tags$td("4 Setup"),    tags$td("60–75 min"), tags$td("Proyecto y librerías (tidyverse, readxl).")),
            tags$tr(tags$td("5 Import"),   tags$td("75–95 min"), tags$td("Leer CSV/Excel y glimpse()/str()/summary().")),
            tags$tr(tags$td("6 Limpieza"), tags$td("95–115 min"),tags$td("janitor::clean_names() y coerción de tipos.")),
            tags$tr(tags$td("7 Cierre"),   tags$td("115–120 min"),tags$td("Recapitulación y preguntas."))
          )
        ),
      ),
      
      # ---------------------
      # PESTAÑA: 1 Intro
      # ---------------------
      nav_panel(
      title = "1 Intro",
      h4(class = "section-header", "1 Intro: Sintaxis Básica"),

      # Teoría extendida
      tags$div(class = "theory-text",
        tags$p(
          "En R, los operadores son símbolos que indican al intérprete cómo combinar valores escalares, vectores o expresiones para producir un resultado (Tutorialspoint, 2024).",
          "Se clasifican en cuatro grupos principales:"
        ),
        tags$ul(
          tags$li(
            tags$b("Aritméticos:"), " +, -, *, /, ^ (o **), %% (módulo), %/% (división entera).",
            "Actúan elemento a elemento sobre vectores (Programiz, 2024)."
          ),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "# Suma, resta, multiplicación, división, exponenciación, módulo, división entera\n",
              "3 + 5\n",
              "10 - 2\n",
              "4 * 2\n",
              "9 / 3\n",
              "6^2\n",
              "7 %% 3\n",
              "9 %/% 2\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "[1]  8\n",
              "[1]  8\n",
              "[1]  8\n",
              "[1]  3\n",
              "[1] 36\n",
              "[1]  1\n",
              "[1]  4\n"
            )
          ),
          tags$br(),
          tags$li(
            tags$b("Relacionales:"), " >, <, ==, !=, >=, <=.",
            "Comparan valores o vectores y devuelven TRUE/FALSE para cada elemento (W3Schools, 2024)."
          ),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "5 > 3\n",
              "2 == 4\n",
              "3 != 5\n",
              "7 >= 7\n",
              "1 < 0\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "[1] TRUE\n",
              "[1] FALSE\n",
              "[1] TRUE\n",
              "[1] TRUE\n",
              "[1] FALSE\n"
            )
          ),
          tags$br(),
          tags$li(
            tags$b("Lógicos:"), " &, | (element-wise), &&, || (primer elemento), ! (NOT).",
            "Se usan para combinar condiciones booleanas (Zero to Pro, 2024)."
          ),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "TRUE & FALSE   # element-wise\n",
              "TRUE && FALSE  # primer elemento\n",
              "FALSE | TRUE\n",
              "FALSE || TRUE\n",
              "!TRUE\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "[1] FALSE\n",
              "[1] FALSE\n",
              "[1] TRUE\n",
              "[1] TRUE\n",
              "[1] FALSE\n"
            )
          ),
          tags$br(),
          tags$li(
            tags$b("Asignación:"), " <-, ->, = (desaconsejado), <<- y ->> (global).",
            "Asignan valores a variables dentro de distintos entornos (Tutorialspoint, 2024)."
          ),
          tags$pre(
            class = "r-code",
            htmltools::HTML(
              "a <- 10\n",
              "20 -> b\n",
              "c = 5   # desaconsejado\n",
              "a + b + c\n"
            )
          ),
          p("Salida esperada:"),
          tags$pre(
            class = "r-output",
            htmltools::HTML(
              "# a y b quedan asignados\n",
              "[1] 35\n"
            )
          ),
        ),
        tags$p(
          "La precedencia de operadores en R sigue: paréntesis > exponenciación > multiplicación y división > suma y resta.",
          "Use paréntesis para garantizar el orden de evaluación deseado (Programiz, 2024)."
        ),
        tags$h5("Pipes"),
        tags$p(
          "Introducido en R 4.1.0, el pipe nativo `|>` toma la salida del LHS como primer argumento del RHS,",
          "facilitando la lectura secuencial de transformaciones sin variables intermedias (Tidyverse Blog, 2023)."
        ),
        tags$p(
          "El pipe de magrittr `%>%` ofrece más flexibilidad con pronombres (`.`) para posicionar el argumento dentro de la llamada (Ivelasq, 2020)."
        ),
        tags$pre(
          class = "r-code",
          htmltools::HTML(
            "# Pipe nativo\n",
            "c(3,5,7) |> mean() |> round(1)\n\n",
            "# Pipe magrittr\n",
            "library(magrittr)\n",
            "c(3,5,7) %>% mean() %>% round(1)\n"
          )
        ),
        p("Salida esperada:"),
        tags$pre(
          class = "r-output",
          htmltools::HTML(
            "[1] 5.0\n",
            "[2] 5.0\n"
          )
        )
      ),

      # Tabla de operadores
      tags$br(),
      tags$h5("Tabla 1: Operadores básicos de R"),
      tags$table(class = "table table-bordered",
        tags$thead(
          tags$tr(
            tags$th("Tipo de operador"),
            tags$th("Símbolo(s)"),
            tags$th("Descripción"),
            tags$th("Ejemplo")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Aritméticos"),
            tags$td("+, -, *, /, ^, %, %, %/%"),
            tags$td("Operaciones matemáticas elementales"),
            tags$td(tags$code("x <- 5 + 3"), ", ", tags$code("y <- 10 / 2"))
          ),
          tags$tr(
            tags$td("Relacionales"),
            tags$td(">, <, ==, !=, >=, <="),
            tags$td("Comparan valores y devuelven booleanos"),
            tags$td(tags$code("5 > 3"), ", ", tags$code("2 <= 4"))
          ),
          tags$tr(
            tags$td("Lógicos"),
            tags$td("!, &, &&, |, ||"),
            tags$td("Combina condiciones TRUE/FALSE"),
            tags$td(tags$code("TRUE & FALSE"), ", ", tags$code("!TRUE"))
          ),
          tags$tr(
            tags$td("Asignación"),
            tags$td("<-, ->, =, <<-, ->>"),
            tags$td("Asigna valores a variables"),
            tags$td(tags$code("a <- 10"), ", ", tags$code("20 -> b"))
          ),
          tags$tr(
            tags$td("Pipe"),
            tags$td("|>, %>%"),
            tags$td("Encadena operaciones de manera legible"),
            tags$td(tags$code("iris |> head()"), ", ", tags$code("iris %>% head()"))
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