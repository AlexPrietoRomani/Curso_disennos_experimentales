# R/modules/session4.R

# UI para la Sesión 4: Introducción a Diseños Estadísticos y ANOVA en R
session4UI <- function(id) {
  ns <- NS(id) # Namespace para asegurar IDs únicos en módulos
  tagList(
    tags$h3(class = "session-title", "Sesión 4: Introducción a Diseños Estadísticos y ANOVA en R"),

    # Contexto
    tags$h4(class = "section-header", "Contexto"),
    tags$p("En esta sesión, exploraremos cómo comparar los efectos de dos o más tratamientos agronómicos diferentes (por ejemplo, variedades de cultivos, dosis de fertilizantes, tipos de riego) sobre una variable de respuesta cuantitativa (como el rendimiento, la altura de la planta o el contenido de proteína). El objetivo es determinar si las diferencias observadas entre los grupos son estadísticamente significativas o simplemente el resultado de la variabilidad aleatoria intrínseca del sistema biológico."),

    # Objetivo de aprendizaje
    tags$h4(class = "section-header", "Objetivo de Aprendizaje"),
    tags$p("Al finalizar esta sesión, el participante comprenderá los fundamentos del Análisis de Varianza (ANOVA) y su implementación práctica en R para el análisis de diseños experimentales simples, con una sólida base en la verificación de supuestos y la interpretación de la relevancia agronómica de los resultados."),

    # 1. Hipótesis Nula y Alternativa
    tags$h4(class = "section-header", "4.1 Hipótesis Nula (\\(\\text{H}_0\\)) y Alternativa (\\(\\text{H}_1\\))"),
    tags$p(
      "En estadística inferencial, el punto de partida para cualquier prueba de hipótesis es la formulación de una ", tags$b("hipótesis nula (\\(\\text{H}_0\\))"), " y una ", tags$b("hipótesis alternativa (\\(\\text{H}_1\\))"), ". Estas hipótesis son afirmaciones sobre los parámetros de una población que deseamos contrastar con la evidencia de nuestros datos muestrales (Dalgaard, 2008)."
    ),
    tags$div(class = "content-row",
      tags$div(class = "main-content",
        tags$ul(
          tags$li(
            tags$b("Hipótesis Nula (\\(\\text{H}_0\\)):"), " Postula que no hay diferencia significativa entre los grupos o tratamientos, o que no existe un efecto del factor estudiado. Representa el 'statu quo' o la ausencia de efecto. Por ejemplo, en un ensayo de fertilizantes, la ", tags$b("H₀"), " sería: “El rendimiento promedio es el mismo para todos los fertilizantes aplicados”."
          ),
          tags$li(
            tags$b("Hipótesis Alternativa (\\(\\text{H}_1\\)):"), " Postula que existe una diferencia significativa entre los grupos o tratamientos; es decir, que el factor estudiado sí tiene un efecto. Es lo que el investigador espera o quiere probar. En el ejemplo anterior, la ", tags$b("H₁"), " sería: “Al menos un fertilizante produce un rendimiento promedio diferente de los demás”."
          ),
          tags$p("Para un ANOVA de una vía, la formalización matemática es:"),
          withMathJax(helpText("
            $$\\text{H}_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k$$
            $$\\text{H}_1: \\text{Al menos una } \\mu_i \\text{ es diferente de las demás}$$
          ")),
          tags$p("donde \\(\\mu_i\\) representa la media poblacional del rendimiento del grupo (o tratamiento) i y \\(k\\) es el número total de grupos o tratamientos en comparación. Nuestro objetivo es usar los datos para decidir si tenemos suficiente evidencia para rechazar \\(\text{H}_0\\) a favor de \\(\text{H}_1\\).")
        )
      ),
      tags$div(class = "note-cloud",
        tags$strong("Ejemplo Agrícola Detallado:"),
        tags$p("Imagina que un agricultor desea comparar el efecto de tres nuevas variedades de trigo (V1, V2, V3) sobre el rendimiento en toneladas por hectárea (t/ha)."),
        tags$p(tags$b("H₀:"), " Las medias de rendimiento son iguales para todas las variedades: \\(\\mu_{\\text{V1}} = \\mu_{\\text{V2}} = \\mu_{\\text{V3}}\\)."),
        tags$p(tags$b("H₁:"), " Al menos una de las medias de rendimiento de las variedades es diferente de las otras. Es decir, las variedades no producen el mismo rendimiento promedio.")
      )
    ),

    # 2. Diseño Completamente al Azar (DCA)
    tags$h4(class = "section-header", "4.2 Diseño Completamente al Azar (DCA)"),
    tags$p("El ", tags$b("Diseño Completamente al Azar (DCA)"), " es el diseño experimental más simple y fundamental, donde los tratamientos se asignan de forma aleatoria a las unidades experimentales (parcelas, plantas individuales, animales, etc.) sin ninguna restricción espacial o ambiental (Montgomery, 2017). Es el diseño de elección cuando las unidades experimentales se consideran homogéneas o las diferencias sistemáticas son insignificantes."),
    tags$div(class = "content-row",
      tags$div(class = "main-content",
        tags$p("Los componentes clave de un DCA son:"),
        tags$ul(
          tags$li(tags$b("Unidades Experimentales:"), " Las entidades más pequeñas a las que se aplica un tratamiento y se mide una respuesta (ej., una parcela de cultivo dentro de un campo homogéneo)."),
          tags$li(tags$b("Tratamientos:"), " Las condiciones o factores controlados que se quieren comparar (ej., diferentes dosis de un fertilizante, distintas variedades de un cultivo)."),
          tags$li(tags$b("Replicaciones:"), " Cada tratamiento se aplica a varias unidades experimentales. Las replicaciones son esenciales porque permiten estimar la variabilidad inherente dentro de un tratamiento (error experimental) y aumentar la precisión de las estimaciones del efecto de los tratamientos. Una mayor replicación generalmente conduce a una mayor potencia estadística."),
          tags$p("La característica definitoria del DCA es la ", tags$b("aleatorización completa"), ". Esto significa que cada unidad experimental tiene la misma probabilidad de recibir cualquier tratamiento. Esta aleatorización ayuda a asegurar que los efectos no controlados o 'ruido' se distribuyan de manera aleatoria entre los tratamientos, minimizando el sesgo."),
          tags$p("El DCA es ideal cuando la variabilidad dentro del área experimental es mínima. Sus principales supuestos incluyen la independencia de las observaciones y la homogeneidad de las unidades experimentales antes de la aplicación de los tratamientos.")
        )
      ),
      tags$div(class = "image-box",
        tags$p(class = "small-diagram", "Diagrama simplificado de un DCA:"),
        # Se asume que 'dca_diagram.png' está en 'www/images/'
        tags$img(src = "images/dca_diagram.png", alt = "Diagrama de Diseño Completamente al Azar", style = "max-width: 100%; height: auto; display: block; margin: auto;"),
        tags$p(class = "small-diagram", "Figura 4.1: Representación esquemática de un DCA con tratamientos A, B, C asignados aleatoriamente a unidades experimentales.")
      )
    ),
    tags$p(tags$strong("Nota:"), "Para visualizar el diagrama, asegúrate de tener una imagen representativa de un DCA (por ejemplo, 'dca_diagram.png') en la carpeta 'www/images/' de tu proyecto Shiny."),

    # 3. Análisis de Varianza (ANOVA) de una Vía
    tags$h4(class = "section-header", "4.3 Análisis de Varianza (ANOVA) de una Vía"),
    tags$p("El ", tags$b("Análisis de Varianza (ANOVA)"), " es una técnica estadística paramétrica desarrollada por Ronald Fisher, utilizada para comparar las medias de dos o más grupos y determinar si las diferencias observadas son estadísticamente significativas. Su nombre se deriva del hecho de que la variabilidad total de los datos se descompone en componentes atribuibles a diferentes fuentes de variación (Dalgaard, 2008)."),
    tags$div(class = "content-row",
      tags$div(class = "main-content",
        tags$p("En un ANOVA de una vía (o 'one-way ANOVA'), la varianza total observada en la variable de respuesta se particiona en dos componentes principales:"),
        tags$ul(
          tags$li(tags$b("Varianza entre grupos (o entre tratamientos):"), " Refleja las diferencias sistemáticas entre las medias de los grupos o tratamientos. Si los tratamientos tienen un efecto real, esta varianza será grande en relación con la varianza dentro de los grupos."),
          tags$li(tags$b("Varianza dentro de los grupos (o error residual/no explicada):"), " Refleja la variabilidad aleatoria inherente dentro de cada grupo. Esta variabilidad se atribuye al error experimental, las diferencias individuales entre las unidades experimentales no relacionadas con el tratamiento, o la variabilidad no explicada por el modelo."),
        ),
        tags$p("La lógica central de ANOVA reside en la ", tags$b("estadística F"), ", que es la razón de la varianza explicada por los tratamientos (Cuadrado Medio de Tratamientos) y la varianza no explicada (Cuadrado Medio del Error):"),
        withMathJax(helpText("
          $$F = \\frac{\\text{CM}_{\\text{Tratamientos}}}{\\text{CM}_{\\text{Error}}} = \\frac{\\text{MS}_{\\text{Between}}}{\\text{MS}_{\\text{Within}}}$$
        ")),
        tags$p("donde \\(\\text{CM}\\) (Cuadrado Medio) o \\(\\text{MS}\\) (Mean Square) son las varianzas estimadas. Un valor de F grande (y un valor p asociado pequeño) sugiere que las diferencias observadas entre las medias de los grupos son mayores de lo que se esperaría por mera casualidad, lo que nos lleva a rechazar la hipótesis nula."),
        tags$p("En R, la función principal para ajustar un modelo ANOVA es ", tags$code("aov()"), ", que utiliza la sintaxis de fórmulas de R: ", tags$code("respuesta ~ factor"),"."),
        tags$h5(tags$b("Ejemplo de código R con ", tags$code("aov()"), ":")),
        tags$pre(class = "r-code",
          htmltools::HTML(
            "# Simulación de datos de rendimiento (t/ha) para 3 tipos de fertilizantes (A, B, C)\n",
            "set.seed(123) # Para reproducibilidad de la simulación\n",
            "\n",
            "n_rep <- 30 # Número de replicaciones por fertilizante\n",
            "\n",
            "rendimiento <- c(\n",
            "  rnorm(n_rep, mean = 5.0, sd = 0.5), # Fertilizante A\n",
            "  rnorm(n_rep, mean = 5.8, sd = 0.5), # Fertilizante B (mayor rendimiento esperado)\n",
            "  rnorm(n_rep, mean = 5.2, sd = 0.5)  # Fertilizante C\n",
            ")\n",
            "\n",
            "fertilizante <- factor(rep(c('A', 'B', 'C'), each = n_rep))\n",
            "datos_agricolas <- data.frame(rendimiento, fertilizante)\n",
            "\n",
            "# Visualización inicial con boxplot para explorar diferencias por grupo\n",
            "library(ggplot2)\n",
            "ggplot(datos_agricolas, aes(x = fertilizante, y = rendimiento, fill = fertilizante)) +\n",
            "  geom_boxplot(alpha = 0.7) +\n",
            "  labs(title = 'Rendimiento por Tipo de Fertilizante', y = 'Rendimiento (t/ha)', x = 'Tipo de Fertilizante') +\n",
            "  theme_minimal() +\n",
            "  theme(legend.position = 'none')\n",
            "\n",
            "# Modelo ANOVA de una vía\n",
            "modelo_aov <- aov(rendimiento ~ fertilizante, data = datos_agricolas)\n",
            "\n",
            "# Resumen de la tabla ANOVA\n",
            "summary(modelo_aov)\n",
            "# La salida mostrará la Suma de Cuadrados (Sum Sq), Grados de Libertad (Df),\n",
            "# Cuadrados Medios (Mean Sq), el valor F y el valor p (Pr(>F)).\n",
            "# Un p-valor < 0.05 (generalmente) indica que al menos una media es diferente.\n"
          )
        ),
      ),
      tags$div(class = "note-cloud",
        tags$strong("Ventaja de ANOVA sobre múltiples pruebas t:"),
        tags$p("ANOVA es una herramienta superior a realizar múltiples pruebas t de Student por pares cuando se comparan más de dos grupos. La razón principal es que realizar múltiples pruebas t infla el riesgo de cometer un ", tags$b("Error Tipo I"), " (rechazar la \\(\text{H}_0\\) cuando es verdadera)."),
        tags$p("Si tienes \\(k\\) grupos, el número de comparaciones por pares es \\(k(k-1)/2\\). Para 5 grupos, son 10 pruebas t. Si cada prueba tiene un \\(\\alpha = 0.05\\), la probabilidad de cometer al menos un Error Tipo I en el conjunto de pruebas aumenta drásticamente. ANOVA controla este error a un nivel global.")
      )
    ),

    # 4. Verificación de Supuestos
    tags$h4(class = "section-header", "4.4 Verificación de Supuestos de ANOVA"),
    tags$p("La validez de las inferencias estadísticas derivadas de un ANOVA depende de que se cumplan ciertos supuestos sobre los errores del modelo. Es imperativo diagnosticar y, si es necesario, abordar las violaciones de estos supuestos antes de interpretar los resultados finales (Montgomery, 2017)."),
    tags$ul(
      tags$li(tags$b("1. Normalidad de los residuos:"), " Los residuos (las diferencias entre los valores observados y los valores predichos por el modelo) deben seguir una distribución normal. Este supuesto es importante para la validez de los p-valores. Se puede evaluar visualmente con un QQ-plot o formalmente con pruebas estadísticas como la prueba de Shapiro-Wilk."),
      tags$li(tags$b("2. Homocedasticidad (Homogeneidad de varianzas):"), " La varianza de los residuos debe ser constante (homogénea) en todos los grupos de tratamiento. La heterocedasticidad (varianzas desiguales) puede llevar a p-valores incorrectos. Una prueba común y robusta para esto es la prueba de Levene."),
      tags$li(tags$b("3. Independencia de los residuos:"), " Los residuos deben ser independientes entre sí. Esto suele garantizarse mediante una correcta aleatorización en el diseño experimental y la ausencia de factores de confusión no controlados o efectos de orden en la recolección de datos."),
    ),
    tags$p(tags$strong("Ejemplo de código R para verificar supuestos:")),
    tags$pre(class = "r-code",
      htmltools::HTML(
        "# Asumiendo que 'modelo_aov' y 'datos_agricolas' ya fueron creados\n",
        "\n",
        "# Extraer los residuos del modelo ANOVA\n",
        "residuos <- residuals(modelo_aov)\n",
        "\n",
        "# 1. Verificación de Normalidad de los Residuos\n",
        "# Gráfico Cuantil-Cuantil (QQ-plot): Ideal para inspección visual\n",
        "qqnorm(residuos, main = 'QQ-plot de los Residuos')\n",
        "qqline(residuos, col = 'red', lwd = 2)\n",
        "# Los puntos deben seguir la línea roja para indicar normalidad.\n",
        "\n",
        "# Prueba de Shapiro-Wilk: Prueba formal de normalidad\n",
        "shapiro.test(residuos)\n",
        "# H0: Los residuos provienen de una distribución normal.\n",
        "# Si el p-valor > 0.05 (nivel de significancia común), no rechazamos H0,\n",
        "# lo que apoya el supuesto de normalidad.\n",
        "\n",
        "# 2. Verificación de Homocedasticidad (Homogeneidad de varianzas)\n",
        "# Se requiere el paquete 'car' (se asume que está cargado en global.R)\n",
        "library(car) # Si no está cargado globalmente, cárgalo aquí.\n",
        "\n",
        "# Prueba de Levene: Robusta para evaluar homogeneidad de varianzas\n",
        "leveneTest(rendimiento ~ fertilizante, data = datos_agricolas)\n",
        "# H0: Las varianzas son iguales entre los grupos.\n",
        "# Si el p-valor > 0.05, no rechazamos H0, lo que apoya la homocedasticidad.\n",
        "\n",
        "# Gráfico de Residuos vs. Valores Ajustados: Para inspección visual de homocedasticidad\n",
        "plot(modelo_aov, which = 1) # Muestra Residuos vs. Valores Ajustados\n",
        "# Busca un patrón aleatorio de puntos, sin forma de embudo (indicador de heterocedasticidad).\n",
        "# Una línea horizontal roja indica una varianza constante.\n",
        "\n",
        "plot(modelo_aov, which = 2) # Confirmación visual del QQ-plot de residuos\n"
      )
    ),
    tags$p(tags$strong("Nota importante sobre los supuestos:"), "El ANOVA es relativamente robusto a pequeñas desviaciones de la normalidad, especialmente con tamaños de muestra grandes debido al Teorema Central del Límite. Sin embargo, la violación de la homocedasticidad es más crítica y puede requerir transformaciones de datos (ej., logarítmica, raíz cuadrada) o el uso de métodos alternativos como el ANOVA de Welch (cuando las varianzas son desiguales) o modelos lineales generalizados (GLM) si la distribución de los datos no es normal (Montgomery, 2017)."),

    # 5. Cálculo del Tamaño del Efecto (Eta-cuadrado)
    tags$h4(class = "section-header", "4.5 Cálculo del Tamaño del Efecto (Eta-cuadrado, \\(\\eta^2\\))"),
    tags$p("Un valor p significativo en un ANOVA nos informa que existe una diferencia estadísticamente significativa entre al menos dos medias de grupo. Sin embargo, el p-valor por sí solo no nos dice nada sobre la ", tags$b("magnitud"), " o ", tags$b("relevancia práctica"), " de esa diferencia. Para evaluar la importancia práctica del efecto de los tratamientos, utilizamos medidas del tamaño del efecto, como el Eta-cuadrado (\\(\\eta^2\\))."),
    tags$p("El ", tags$b("Eta-cuadrado (\\(\\eta^2\\))"), " representa la proporción de la varianza total de la variable dependiente (por ejemplo, rendimiento) que es explicada por la variable independiente categórica (el factor de tratamiento, como el tipo de fertilizante). Es decir, cuánto de la variabilidad observada en el rendimiento se debe a las diferencias entre los fertilizantes."),
    withMathJax(helpText("
      $$\\eta^2 = \\frac{\\text{SS}_{\\text{Tratamientos}}}{\\text{SS}_{\\text{Total}}}$$
    ")),
    tags$p("donde \\(\\text{SS}_{\\text{Tratamientos}}\\) es la Suma de Cuadrados de los tratamientos (variación explicada por el factor) y \\(\\text{SS}_{\\text{Total}}\\) es la Suma de Cuadrados Total (variación total en los datos). El valor de \\(\\eta^2\\) siempre oscila entre 0 y 1. Un valor más alto indica que una mayor proporción de la varianza en la variable dependiente es explicada por el factor."),
    tags$p("Las directrices de Cohen (1988) para interpretar \\(\\eta^2\\) son comúnmente citadas, aunque su aplicación debe ser contextual:"),
    tags$ul(
      tags$li("\\(\\eta^2 \\approx 0.01\\): se considera un efecto ", tags$b("pequeño"), "."),
      tags$li("\\(\\eta^2 \\approx 0.06\\): se considera un efecto ", tags$b("mediano"), "."),
      tags$li("\\(\\eta^2 \\approx 0.14\\): se considera un efecto ", tags$b("grande"), "."),
    ),
    tags$p(tags$strong("Ejemplo de código R para calcular Eta-cuadrado:")),
    tags$pre(class = "r-code",
      htmltools::HTML(
        "# Asumiendo que 'modelo_aov' ya fue creado y es el resultado de un aov()\n",
        "\n",
        "# 1. Extraer la tabla ANOVA para obtener las Sumas de Cuadrados\n",
        "anova_tabla <- anova(modelo_aov)\n",
        "print(anova_tabla)\n",
        "\n",
        "# Identificar la Suma de Cuadrados del factor (ej. 'fertilizante')\n",
        "ss_tratamiento <- anova_tabla['fertilizante', 'Sum Sq']\n",
        "\n",
        "# Calcular la Suma de Cuadrados Total\n",
        "# La Suma de Cuadrados Total es la suma de SS_Tratamientos y SS_Residuales (Error)\n",
        "ss_total <- sum(anova_tabla$`Sum Sq`)\n",
        "\n",
        "# Calcular Eta-cuadrado\n",
        "eta_cuadrado <- ss_tratamiento / ss_total\n",
        "print(paste('Eta-cuadrado (η²):', round(eta_cuadrado, 4)))\n",
        "\n",
        "# 2. Forma más directa y robusta con el paquete 'effectsize'\n",
        "# Si no tienes el paquete, instálalo: install.packages(\"effectsize\")\n",
        "library(effectsize)\n",
        "eta_squared(modelo_aov)\n",
        "# Este paquete proporciona funciones para calcular varios tamaños de efecto,\n",
        "# incluyendo Eta-cuadrado, con su interpretación y límites de confianza.\n"
      )
    ),

    # Resultado esperado
    tags$h4(class = "section-header", "Resultado Esperado"),
    tags$p("Al finalizar esta sesión, los participantes podrán:",
      tags$ul(
        tags$li("Formular de manera correcta las hipótesis nula y alternativa para problemas de comparación de tratamientos agronómicos."),
        tags$li("Comprender y aplicar el concepto de Diseño Completamente al Azar, incluyendo sus componentes y supuestos."),
        tags$li("Realizar un Análisis de Varianza de una vía en R utilizando la función ", tags$code("aov()"), "."),
        tags$li("Interpretar la tabla ANOVA resultante, identificando los componentes de variación y el p-valor."),
        tags$li("Diagnosticar los supuestos del modelo ANOVA (normalidad y homocedasticidad) utilizando pruebas estadísticas (Shapiro-Wilk, Levene) y gráficos de diagnóstico (QQ-plot, residuos vs. ajustados)."),
        tags$li("Calcular e interpretar el tamaño del efecto (Eta-cuadrado) para evaluar la relevancia agronómica y práctica de los hallazgos."),
        tags$li("Generar y documentar scripts de R que realicen estos análisis, listos para ser aplicados a sus propios conjuntos de datos experimentales.")
      )
    ),

    # Sección interactiva para ejemplos
    tags$hr(),
    tags$h4(class = "section-header", "Ejemplo Interactivo de ANOVA Simulado"),
    tags$p("Utilice los deslizadores para simular datos de rendimiento con diferentes efectos de fertilizantes y observe cómo cambian los resultados del ANOVA, las pruebas de supuestos y el tamaño del efecto. Esto le permitirá desarrollar una intuición sobre cómo las diferencias en las medias y la variabilidad afectan las conclusiones estadísticas."),
    tags$div(class = "plot-box", # Usa la clase plot-box para el contenedor del ejemplo
      fluidRow(
        column(4,
          sliderInput(ns("mean_fertB_diff"), "Diferencia Media Fertilizante B vs A (\\(\\mu_B - \\mu_A\\)):",
                      min = -1.0, max = 1.5, value = 0.8, step = 0.1),
          sliderInput(ns("mean_fertC_diff"), "Diferencia Media Fertilizante C vs A (\\(\\mu_C - \\mu_A\\)):",
                      min = -1.0, max = 1.5, value = 0.2, step = 0.1),
          sliderInput(ns("sd_error_sim"), "Desviación Estándar de Error (Variabilidad dentro de grupos):",
                      min = 0.1, max = 1.5, value = 0.5, step = 0.1),
          sliderInput(ns("n_reps_sim"), "Número de Replicaciones por Tratamiento:",
                      min = 5, max = 100, value = 30, step = 5)
        ),
        column(8,
          plotOutput(ns("boxplot_sim")),
          tags$h5(tags$b("Tabla ANOVA:")),
          tableOutput(ns("anova_table_sim")),
          tags$h5(tags$b("Prueba de Normalidad (Shapiro-Wilk):")),
          verbatimTextOutput(ns("shapiro_test_sim")),
          tags$h5(tags$b("Prueba de Homocedasticidad (Levene):")),
          verbatimTextOutput(ns("levene_test_sim")),
          tags$h5(tags$b("Eta-cuadrado (\\(\\eta^2\\)):")),
          textOutput(ns("eta_squared_sim"))
        )
      )
    )
  )
}

# Server para la Sesión 4
session4Server <- function(input, output, session) {
    ns <- session$ns

    # Generación de datos reactivos basada en los inputs del usuario
    sim_data <- reactive({
      req(input$mean_fertB_diff, input$mean_fertC_diff, input$sd_error_sim, input$n_reps_sim)
      set.seed(Sys.time()) # Usar el tiempo actual para variabilidad en cada re-renderizado

      n <- input$n_reps_sim
      sd_val <- input$sd_error_sim
      mean_A <- 5.0 # Media base para el fertilizante A
      mean_B <- mean_A + input$mean_fertB_diff
      mean_C <- mean_A + input$mean_fertC_diff

      rendimiento <- c(rnorm(n, mean = mean_A, sd = sd_val),
                       rnorm(n, mean = mean_B, sd = sd_val),
                       rnorm(n, mean = mean_C, sd = sd_val))
      fertilizante <- factor(rep(c('A', 'B', 'C'), each = n))
      data.frame(rendimiento, fertilizante)
    })

    # Renderizar boxplot de los datos simulados
    output$boxplot_sim <- renderPlot({
      ggplot(sim_data(), aes(x = fertilizante, y = rendimiento, fill = fertilizante)) +
        geom_boxplot(alpha = 0.7) +
        labs(title = 'Rendimiento Simulado por Tipo de Fertilizante', y = 'Rendimiento (t/ha)', x = 'Tipo de Fertilizante') +
        theme_minimal() +
        theme(legend.position = 'none')
    })

    # Realizar ANOVA y renderizar la tabla
    anova_model_sim <- reactive({
      aov(rendimiento ~ fertilizante, data = sim_data())
    })

    output$anova_table_sim <- renderTable({
      req(anova_model_sim())
      anova_sum <- summary(anova_model_sim())
      # Convertir el resumen de aov a un data frame para mejor visualización
      # Usamos dplyr::rownames_to_column y dplyr::rename que vienen con dplyr cargado en global.R
      as.data.frame(anova_sum[[1]]) %>%
        rownames_to_column(var = "Fuente de Variación") %>% # Convertir nombres de fila a una columna
        dplyr::rename(
          `Suma de Cuadrados` = `Sum Sq`,
          `Grados de Libertad` = `Df`,
          `Cuadrado Medio` = `Mean Sq`,
          `Valor F` = `F value`,
          `P-valor` = `Pr(>F)`
        )
    }, striped = TRUE, bordered = TRUE, na = " ",  # Añadir bordes y alternar filas para claridad
    # Opciones de formato de números para la tabla
    digits = 4, align = 'llllrr')


    # Prueba de Normalidad (Shapiro-Wilk)
    output$shapiro_test_sim <- renderPrint({
      req(anova_model_sim())
      residuos <- residuals(anova_model_sim())
      shapiro.test(residuos)
    })

    # Prueba de Homocedasticidad (Levene)
    output$levene_test_sim <- renderPrint({
      req(sim_data())
      # Se asume que el paquete 'car' está cargado en global.R
      car::leveneTest(rendimiento ~ fertilizante, data = sim_data())
    })

    # Cálculo de Eta-cuadrado
    output$eta_squared_sim <- renderText({
      req(anova_model_sim())
      anova_tbl <- anova(anova_model_sim())
      # Asegurarse de que el nombre del factor coincida con la tabla ANOVA
      # Usamos `grep` para encontrar la fila del factor 'fertilizante' de manera robusta
      factor_row_index <- grep("fertilizante", rownames(anova_tbl))

      if (length(factor_row_index) == 0) {
        return("Error: No se encontró el factor 'fertilizante' en la tabla ANOVA.")
      }

      ss_trat <- anova_tbl[factor_row_index, "Sum Sq"]
      ss_total <- sum(anova_tbl$`Sum Sq`) # Suma de todos los SS en la tabla

      eta2 <- ss_trat / ss_total
      paste0("Eta-cuadrado (η²): ", round(eta2, 4), " (interpretación según Cohen: ",
             case_when(
               eta2 < 0.01 ~ "muy pequeño",
               eta2 < 0.06 ~ "pequeño",
               eta2 < 0.14 ~ "mediano",
               TRUE        ~ "grande"
             ), " efecto)")
    })

}