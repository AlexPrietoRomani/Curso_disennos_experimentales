# R/modules/session4.R

# UI para la Sesión 4: Introducción a Diseños Estadísticos y ANOVA en R
session4UI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$h3(class = "session-title", "Sesión 4: Introducción a Diseños Estadísticos y ANOVA en R"),

    navset_tab(
      # ===== PESTAÑA 1: Fundamentos del ANOVA =====
      nav_panel(
        title = "1. Fundamentos del ANOVA",
        h4(class = "section-header", "1.1 El Problema: Comparar Múltiples Grupos"),
        p("Cuando tenemos más de dos grupos (ej. 3 variedades de cultivo), no podemos simplemente usar múltiples pruebas t de Student. Hacerlo inflaría la probabilidad de cometer un Error Tipo I (un falso positivo). La solución es el ", strong("Análisis de Varianza (ANOVA).")),

        h4(class = "section-header", "1.2 Hipótesis Nula (\\(H_0\\)) y Alternativa (\\(H_1\\))"),
        tags$p(
          "En estadística inferencial, el punto de partida para cualquier prueba de hipótesis es la formulación de una ", tags$b("hipótesis nula (\\(H_0\\))"), " y una ", tags$b("hipótesis alternativa (\\(H_1\\))"), ". Estas hipótesis son afirmaciones sobre los parámetros de una población que deseamos contrastar con la evidencia de nuestros datos muestrales (Dalgaard, 2008)."
        ),
        tags$div(class = "content-row",
          tags$div(class = "main-content",
            tags$ul(
              tags$li(
                tags$b("Hipótesis Nula (\\(H_0\\)):"), " Postula que no hay diferencia significativa entre los grupos o tratamientos, o que no existe un efecto del factor estudiado. Representa el 'statu quo' o la ausencia de efecto. Por ejemplo, en un ensayo de fertilizantes, la \\(H_0\\) sería: “El rendimiento promedio es el mismo para todos los fertilizantes aplicados”."
              ),
              tags$li(
                tags$b("Hipótesis Alternativa (\\(H_1\\)):"), " Postula que existe una diferencia significativa entre los grupos o tratamientos; es decir, que el factor estudiado sí tiene un efecto. Es lo que el investigador espera o quiere probar. En el ejemplo anterior, la \\(H_1\\) sería: “Al menos un fertilizante produce un rendimiento promedio diferente de los demás”."
              ),
              tags$p("Para un ANOVA de una vía, la formalización matemática es:"),
              withMathJax(helpText("
                $$H_0: \\mu_1 = \\mu_2 = \\dots = \\mu_k$$
                $$H_1: \\text{Al menos una } \\mu_i \\text{ es diferente de las demás}$$
              ")),
              tags$p("donde \\(\\mu_i\\) representa la media poblacional del rendimiento del grupo (o tratamiento) \\(i\\) y \\(k\\) es el número total de grupos o tratamientos en comparación. Nuestro objetivo es usar los datos para decidir si tenemos suficiente evidencia para rechazar \\(H_0\\) a favor de \\(H_1\\).")
            )
          ),
          tags$div(class = "note-cloud",
            tags$strong("Ejemplo Agrícola Detallado:"),
            tags$p("Imagina que un agricultor desea comparar el efecto de tres nuevas variedades de trigo (V1, V2, V3) sobre el rendimiento en toneladas por hectárea (t/ha)."),
            tags$p(tags$b("\\(H_0\\):"), " Las medias de rendimiento son iguales para todas las variedades: \\(\\mu_{\\text{V1}} = \\mu_{\\text{V2}} = \\mu_{\\text{V3}}\\)."),
            tags$p(tags$b("\\(H_1\\):"), " Al menos una de las medias de rendimiento de las variedades es diferente de las otras. Es decir, las variedades no producen el mismo rendimiento promedio.")
          )
        ),
        
        h4(class = "section-header", "1.3 La Lógica del ANOVA: Partición de la Varianza"),
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
                "# Resumen de la tabla ANOVA (usando broom para un data.frame ordenado)\n",
                "library(broom)\n",
                "tabla_anova <- tidy(modelo_aov)\n",
                "print(tabla_anova)\n",
                "# Un p.value < 0.05 (generalmente) para el término 'fertilizante' indica que al menos una media es diferente.\n"
              )
            ),
          ),
          tags$div(class = "note-cloud",
            tags$strong("Ventaja de ANOVA sobre múltiples pruebas t:"),
            tags$p("ANOVA es una herramienta superior a realizar múltiples pruebas t de Student por pares cuando se comparan más de dos grupos. La razón principal es que realizar múltiples pruebas t infla el riesgo de cometer un ", tags$b("Error Tipo I"), " (rechazar la \\(H_0\\) cuando es verdadera)."),
            tags$p("Si tienes \\(k\\) grupos, el número de comparaciones por pares es \\(k(k-1)/2\\). Para 5 grupos, son 10 pruebas t. Si cada prueba tiene un \\(\\alpha = 0.05\\), la probabilidad de cometer al menos un Error Tipo I en el conjunto de pruebas aumenta drásticamente. ANOVA controla este error a un nivel global.")
          )
        )
      ),

      # ===== PESTAÑA 2: Diseño (DCA) =====
      nav_panel(
        title = "2. Diseño Completamente al Azar (DCA)",
        h4(class = "section-header", "2.1 El Diseño más Simple"),
        tags$p("El DCA es el diseño experimental fundamental. Su única regla es que ", strong("todos los tratamientos se asignan a las unidades experimentales de forma completamente aleatoria."), " Esto se basa en un supuesto clave: que todas las unidades experimentales son homogéneas."),
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
          tags$div(class = "note-cloud",
            tags$strong("Representación Conceptual de un DCA:"),
            tags$p("Imagine un campo experimental dividido en varias parcelas (unidades experimentales). Si tenemos tres tratamientos (A, B, C), en un DCA, cada tratamiento se asigna a las parcelas de manera completamente aleatoria, sin ninguna restricción. Por ejemplo, la secuencia de tratamientos podría ser A, C, B, A, B, C, C, A, B... a lo largo de las parcelas.")
          )
        ),
        tags$p(tags$strong("Nota:"), "Para visualizar el diagrama, asegúrate de tener una imagen representativa de un DCA (por ejemplo, 'dca_diagram.png') en la carpeta 'www/images/' de tu proyecto Shiny."),
        tags$h5("Constructor de Diseños DCA"),
        tags$p("Usa los controles para ver cómo se vería la aleatorización de un DCA en un campo."),
        sidebarLayout(
            sidebarPanel(width=3,
                numericInput(ns("dca_trat"), "Número de Tratamientos:", 3, min=2, max=10),
                numericInput(ns("dca_rep"), "Número de Replicaciones:", 4, min=2, max=15),
                actionButton(ns("dca_gen"), "Generar Nuevo Diseño", icon=icon("random"))
            ),
            mainPanel(width=9,
                plotOutput(ns("dca_plot_layout"))
            )
        )
      ),

      # ===== PESTAÑA 3: Análisis en R =====
      nav_panel(
        title = "3. Análisis de un DCA en R",

        tags$h4(class = "section-header", "El Flujo de Trabajo del Análisis"),
        tags$p("Analizar un DCA implica más que solo correr una función. Sigue un flujo lógico para asegurar que tus conclusiones sean válidas y completas."),

        # --- Teoría de los Supuestos ---
        tags$h5(tags$b("Paso Clave 1: Verificación de Supuestos del ANOVA")),
        tags$p("La validez de las inferencias estadísticas derivadas de un ANOVA depende de que se cumplan ciertos supuestos sobre los errores del modelo. Es imperativo diagnosticar y, si es necesario, abordar las violaciones de estos supuestos antes de interpretar los resultados finales (Montgomery, 2017)."),
        tags$ul(
          tags$li(tags$b("1. Normalidad de los residuos:"), " Los residuos (las diferencias entre los valores observados y los valores predichos por el modelo) deben seguir una distribución normal. Este supuesto es importante para la validez de los p-valores. Se puede evaluar visualmente con un QQ-plot o formalmente con pruebas estadísticas como la prueba de Shapiro-Wilk."),
          tags$li(tags$b("2. Homocedasticidad (Homogeneidad de varianzas):"), " La varianza de los residuos debe ser constante (homogénea) en todos los grupos de tratamiento. La heterocedasticidad (varianzas desiguales) puede llevar a p-valores incorrectos. Una prueba común y robusta para esto es la prueba de Levene."),
          tags$li(tags$b("3. Independencia de los residuos:"), " Los residuos deben ser independientes entre sí. Esto suele garantizarse mediante una correcta aleatorización en el diseño experimental y la ausencia de factores de confusión no controlados o efectos de orden en la recolección de datos."),
        ),
        tags$p(tags$strong("Ejemplo de código R para verificar supuestos:")),
        tags$pre(class = "r-code",
          htmltools::HTML(
            "# Asumiendo que 'modelo_aov' y 'datos_agricolas' ya fueron creados como en el ejemplo anterior\n",
            "\n",
            "# Extraer los residuos del modelo ANOVA\n",
            "residuos <- residuals(modelo_aov)\n",
            "\n",
            "# 1. Verificación de Normalidad de los Residuos\n",
            "par(mfrow = c(1,2)) # Para mostrar dos gráficos juntos\n",
            "hist(residuos, main = 'Histograma de Residuos', xlab = 'Residuos', col = 'lightblue')\n",
            "qqnorm(residuos, main = 'QQ-plot de Residuos')\n",
            "qqline(residuos, col = 'red', lwd = 2)\n",
            "par(mfrow = c(1,1)) # Restaurar disposición de gráficos\n",
            "\n",
            "# Prueba de Shapiro-Wilk: Prueba formal de normalidad\n",
            "shapiro.test(residuos)\n",
            "# H0: Los residuos provienen de una distribución normal.\n",
            "# Si el p-valor > 0.05, no rechazamos H0.\n",
            "\n",
            "# 2. Verificación de Homocedasticidad (Homogeneidad de varianzas)\n",
            "# Gráfico de Residuos vs. Valores Ajustados: Para inspección visual\n",
            "plot(modelo_aov, which = 1) # Muestra Residuos vs. Valores Ajustados\n",
            "# Busca un patrón aleatorio de puntos, sin forma de embudo.\n",
            "\n",
            "# Prueba de Levene: Robusta para evaluar homogeneidad de varianzas\n",
            "library(car)\n",
            "leveneTest(rendimiento ~ fertilizante, data = datos_agricolas)\n",
            "# H0: Las varianzas son iguales entre los grupos.\n",
            "# Si el p-valor > 0.05, no rechazamos H0.\n"
          )
        ),
        tags$p(tags$strong("Nota importante sobre los supuestos:"), "El ANOVA es relativamente robusto a pequeñas desviaciones de la normalidad, especialmente con tamaños de muestra grandes (Teorema Central del Límite). Sin embargo, la violación de la homocedasticidad es más crítica y puede requerir transformaciones (ej., logarítmica) o el uso de ANOVA de Welch (Montgomery, 2017)."),
        tags$hr(),

        # --- Teoría del Tamaño del Efecto ---
        # 4.6 Cálculo del Tamaño del Efecto (Eta-cuadrado, η²)
        tags$h5(tags$b("Paso Clave 2: Medir la Relevancia con el Tamaño del Efecto")),
        tags$p("Un p-valor significativo solo nos dice que existe una diferencia, no cuán grande o importante es. Para eso usamos el ", tags$b("tamaño del efecto.")),
        tags$p("El ", tags$b("Eta-cuadrado (\\(\\eta^2\\))"), " representa la proporción de la varianza total en la respuesta que es explicada por los tratamientos. Se calcula como:"),
        withMathJax(helpText(
            "$$\\eta^2 = \\frac{\\text{SS}_{\\text{Tratamientos}}}{\\text{SS}_{\\text{Total}}}$$"
        )),
        p("Un valor más alto indica que el tratamiento es un factor más importante para explicar las diferencias observadas. Las guías de Cohen (1988) sugieren interpretar los valores en torno a 0.01, 0.06 y 0.14 como efectos pequeños, medianos y grandes, respectivamente, aunque el contexto agronómico siempre es el juez final."),
        tags$p("donde:"),
        tags$ul(
          tags$li("\\(\\text{SS}_{\\text{Tratamientos}}\\) es la Suma de Cuadrados debida a las diferencias entre los tratamientos (varianza explicada)."),
          tags$li("\\(\\text{SS}_{\\text{Total}}\\) es la Suma de Cuadrados Total (varianza total en los datos).")
        ),
        tags$p(
          "El valor de \\(\\eta^2\\) varía entre 0 y 1. Un \\(\\eta^2 = 0\\) significa que el tratamiento no explica nada de la varianza en la respuesta, mientras que un \\(\\eta^2 = 1\\) significaría que el tratamiento explica toda la varianza (lo cual es extremadamente raro en la práctica). Cuanto mayor sea \\(\\eta^2\\), mayor será la proporción de varianza explicada por el tratamiento, lo que sugiere un efecto más fuerte."
        ),

        tags$h5(tags$b("Interpretación de Eta-cuadrado (\\(\\eta^2\\)):")),
        tags$p(
          "Si bien cualquier interpretación debe considerar el contexto específico de la investigación, las directrices de Cohen (1988) son un punto de partida común para la magnitud del efecto en las ciencias del comportamiento, y a menudo se adaptan a otros campos:"
        ),
        tags$ul(
          tags$li("\\(\\eta^2 \\approx 0.01\\) (o 1%): Efecto ", tags$b("pequeño"), "."),
          tags$li("\\(\\eta^2 \\approx 0.06\\) (o 6%): Efecto ", tags$b("mediano"), "."),
          tags$li("\\(\\eta^2 \\approx 0.14\\) (o 14%): Efecto ", tags$b("grande"), ".")
        ),
        tags$p(
          "Es crucial entender que estas son solo guías. En agronomía, un efecto 'pequeño' según Cohen (ej., \\(\\eta^2 = 0.02\\), que explica el 2% de la varianza del rendimiento) podría ser económicamente muy significativo si se aplica a grandes extensiones o si el costo del tratamiento es bajo. Por el contrario, un efecto 'grande' podría no ser práctico si el tratamiento es prohibitivamente caro o difícil de implementar."
        ),

        tags$h5(tags$b("Eta-cuadrado Parcial (\\(\\eta_p^2\\)):")),
        tags$p(
          "Cuando se trabaja con diseños más complejos que un ANOVA de una vía (por ejemplo, ANOVA factorial, diseños en bloques), a menudo se informa el ", tags$b("Eta-cuadrado parcial (\\(\\eta_p^2\\))"), ". Para un factor específico, la \\(\\eta_p^2\\) es la proporción de la varianza explicada por ese factor, después de excluir la varianza explicada por otros factores en el modelo. Su fórmula es:"
        ),
        withMathJax(helpText("
          $$\\eta_p^2 = \\frac{\\text{SS}_{\\text{Factor}}}{\\text{SS}_{\\text{Factor}} + \\text{SS}_{\\text{Error}}}$$
        ")),
        tags$p("En un ANOVA de una vía, \\(\\eta^2\\) y \\(\\eta_p^2\\) son idénticos porque solo hay un factor. El paquete ", tags$code("effectsize"), " en R facilita el cálculo de ambos y su interpretación."),

        tags$h5(tags$b("Ejemplos y Casos de Uso en Agronomía:")),
        tags$ul(
          tags$li(
            tags$strong("Evaluación de Variedades:"), " Un ensayo compara 5 variedades de maíz. El ANOVA para el rendimiento es significativo. Un \\(\\eta^2 = 0.25\\) indicaría que el 25% de la variabilidad observada en el rendimiento se debe a las diferencias genéticas entre las variedades. Esto sugiere que la elección de la variedad es un factor importante que afecta el rendimiento."
          ),
          tags$li(
            tags$strong("Respuesta a Fertilizantes:"), " Se prueban 4 dosis de nitrógeno en un cultivo de trigo. Si el \\(\\eta^2\\) para el contenido de proteína en grano es 0.10, el 10% de la variación en la proteína se explica por las diferentes dosis de nitrógeno. El agricultor puede entonces sopesar este tamaño de efecto mediano contra el costo del fertilizante."
          ),
          tags$li(
            tags$strong("Control de Malezas:"), " Diferentes herbicidas se aplican para controlar una maleza específica. La variable respuesta es la biomasa de la maleza. Un \\(\\eta^2 = 0.03\\) podría ser estadísticamente significativo (p < 0.05) pero interpretado como un efecto pequeño de los herbicidas sobre la varianza total de la biomasa de malezas. Quizás otros factores (tipo de suelo, humedad) están influyendo más, o los herbicidas probados son similarmente efectivos (o inefectivos)."
          ),
          tags$li(
            tags$strong("Comparación de Métodos de Labranza:"), " Se comparan labranza convencional, labranza mínima y siembra directa en cuanto a la infiltración de agua. Un \\(\\eta_p^2 = 0.18\\) para el factor 'método de labranza' en un diseño en bloques indicaría un efecto grande y prácticamente relevante del método de labranza sobre la infiltración, controlando la variabilidad entre bloques."
          )
        ),
        tags$p(
          "Reportar el tamaño del efecto junto con el p-valor proporciona una imagen mucho más completa de los resultados experimentales, ayudando a tomar decisiones agronómicas más informadas."
        ),

        tags$p(tags$strong("Ejemplo de código R para calcular Eta-cuadrado con ", tags$code("effectsize"), ":")),
        tags$pre(class = "r-code",
          htmltools::HTML(
            "# Asumiendo que 'modelo_aov' es el resultado de un aov() de una vía\n",
            "library(effectsize)\n",
            "\n",
            "# Para ANOVA de una vía, eta_squared() por defecto da η² total\n",
            "eta_sq_resultado <- eta_squared(modelo_aov, ci = 0.95) # ci = 0.95 para intervalo de confianza del 95%\n",
            "print(eta_sq_resultado)\n",
            "# La salida incluye el valor de Eta2, su IC, y una interpretación basada en Cohen.\n",
            "\n",
            "# Si fuera un modelo con múltiples factores (ej. 'modelo_aov_factorial <- aov(resp ~ F1*F2, data=d))'\n",
            "# entonces querríamos Eta-cuadrado parcial para cada factor e interacción:\n",
            "# eta_sq_parcial_F1 <- eta_squared(modelo_aov_factorial, partial = TRUE, generalized = FALSE, ci = 0.95)\n",
            "# print(eta_sq_parcial_F1) # Mostraría η²p para F1, F2 y F1:F2\n",
            "\n",
            "# Interpretación manual de la salida de eta_squared():\n",
            "# Por ejemplo, si Eta2 = 0.25, significa que el 25% de la varianza en la variable\n",
            "# de respuesta es explicada por las diferencias entre los grupos del factor estudiado.\n"
          )
        ),
        tags$hr(),

        # --- Teoría de las Pruebas Post-Hoc ---
        tags$h5(tags$b("Paso Clave 3: Identificar Diferencias con Pruebas Post-Hoc")),
        tags$p("Si el ANOVA global es significativo, necesitamos saber ", tags$em("cuáles"), " grupos difieren. Las pruebas post-hoc realizan comparaciones por pares controlando la tasa de error general (FWER)."),
        tags$p(strong("Guía rápida para elegir una prueba post-hoc:")),
        tags$ul(
          tags$li(strong("Tukey HSD:"), " La opción estándar y más recomendada para comparar todos los pares cuando se cumplen los supuestos."),
          tags$li(strong("Duncan / SNK:"), " Más potentes (liberales) que Tukey, populares en agronomía, pero con menor control del error Tipo I."),
          tags$li(strong("Games-Howell:"), " La elección correcta si se viola el supuesto de homogeneidad de varianzas.")
        ),
        tags$p(
          "El objetivo principal de estas pruebas es realizar comparaciones par a par entre las medias de los tratamientos (ej., \\(\\mu_A\\) vs \\(\\mu_B\\), \\(\\mu_A\\) vs \\(\\mu_C\\), \\(\\mu_B\\) vs \\(\\mu_C\\)) de una manera que controle la tasa de error experimental general o Tasa de Error por Familia (Family-Wise Error Rate, FWER). La FWER es la probabilidad de cometer al menos un Error Tipo I (rechazar incorrectamente una hipótesis nula verdadera) en el conjunto de todas las comparaciones realizadas."
        ),
        tags$h5(tags$b("Principales Pruebas Post-Hoc y Cuándo Usarlas:")),
        tags$ul(
          tags$li(
            tags$b("Prueba de Diferencia Honesta Significativa de Tukey (Tukey HSD):"),
            tags$ul(
              tags$li("Es una de las pruebas más utilizadas y recomendadas cuando se desean comparar ", tags$em("todos los posibles pares de medias"), " y los tamaños de muestra por grupo son iguales o muy similares."),
              tags$li("Controla la FWER a un nivel \\(\\alpha\\) especificado."),
              tags$li("Se considera una prueba balanceada en términos de potencia y control del Error Tipo I."),
              tags$li(tags$strong("Condiciones:"), " Ideal cuando se cumplen los supuestos del ANOVA (normalidad de residuos, homocedasticidad, independencia).")
            )
          ),
          tags$li(
            tags$b("Prueba de Bonferroni:"),
            tags$ul(
              tags$li("Es un método muy general que ajusta el nivel de significancia para cada comparación individual. Si se realizan \\(m\\) comparaciones, el nivel de significancia para cada una se establece en \\(\\alpha / m\\)."),
              tags$li("Es simple de aplicar y controla fuertemente la FWER."),
              tags$li("Tiende a ser conservadora (menor potencia para detectar diferencias reales), especialmente si el número de comparaciones es grande."),
              tags$li(tags$strong("Condiciones:"), " Aplicable bajo supuestos de ANOVA, útil para un número pequeño de comparaciones planificadas incluso antes del ANOVA (aunque aquí se discute como post-hoc).")
            )
          ),
          tags$li(
            tags$b("Prueba de Scheffé:"),
            tags$ul(
              tags$li("Es la más conservadora de las pruebas post-hoc comunes."),
              tags$li("Permite probar no solo comparaciones par a par, sino también contrastes más complejos (ej., promedio de A y B vs. C)."),
              tags$li("Controla la FWER para cualquier número y tipo de contrastes lineales."),
              tags$li(tags$strong("Condiciones:"), " Aplicable bajo supuestos de ANOVA. Debido a su conservadurismo, es menos potente para comparaciones simples par a par que Tukey HSD.")
            )
          ),
          tags$li(
            tags$b("Prueba de Mínima Diferencia Significativa de Fisher (Fisher's LSD):"),
            tags$ul(
              tags$li("Consiste en realizar pruebas t individuales entre pares de medias, pero ", tags$em("solo si el ANOVA global es significativo"), " (esto se conoce como \"LSD protegido\")."),
              tags$li("Es más potente que Tukey HSD, pero no controla la FWER tan estrictamente cuando hay más de tres grupos; el riesgo de Error Tipo I aumenta con el número de grupos."),
              tags$li(tags$strong("Condiciones:"), " Supuestos de ANOVA cumplidos. Más adecuada para pocos grupos (ej., 3).")
            )
          ),
          tags$li(
            tags$b("Pruebas de Rango Múltiple (Duncan, Student-Newman-Keuls - SNK):"),
            tags$ul(
              tags$li("Estas pruebas comparan las diferencias entre medias con valores críticos que dependen del número de medias comprendidas en el rango de comparación."),
              tags$li("Duncan es más liberal (mayor potencia, mayor riesgo de Error Tipo I) que SNK. Ambas son más liberales que Tukey HSD."),
              tags$li("Históricamente populares en agronomía, a menudo presentadas con letras para agrupar medias no significativamente diferentes."),
              tags$li(tags$strong("Condiciones:"), " Supuestos de ANOVA cumplidos. Su control de la FWER es menos robusto que Tukey para todas las comparaciones.")
            )
          )
        ),

        tags$h5(tags$b("¿Qué hacer si no se cumplen los supuestos del ANOVA?")),
        tags$p(
          "Si el supuesto de ", tags$strong("homogeneidad de varianzas (homocedasticidad)"), " es violado de manera significativa (detectado por pruebas como Levene o Bartlett, o visualmente), las pruebas post-hoc estándar pueden no ser válidas. En este caso, se pueden considerar:"
        ),
        tags$ul(
          tags$li(
            tags$b("Prueba de Games-Howell:"),
            tags$ul(
              tags$li("Es una modificación de la prueba t de Welch para comparaciones múltiples cuando las varianzas son desiguales."),
              tags$li("No requiere homogeneidad de varianzas y es robusta cuando los tamaños de muestra son diferentes entre grupos."),
              tags$li("Es una excelente opción bajo heterocedasticidad.")
            )
          ),
          tags$li(
            tags$b("Prueba C de Dunnett o T3 de Dunnett:"),
            tags$ul(
              tags$li("Alternativas a Games-Howell para comparaciones par a par con varianzas desiguales.")
            )
          ),
          tags$li(
            tags$b("Transformación de datos:"),
            tags$ul(
              tags$li("Antes de recurrir a pruebas especializadas, se puede intentar transformar la variable de respuesta (ej., logarítmica, raíz cuadrada) para estabilizar las varianzas y/o normalizar los residuos. Si la transformación tiene éxito, se pueden usar las pruebas post-hoc estándar sobre los datos transformados.")
            )
          )
        ),
        tags$p("Si el supuesto de ", tags$strong("normalidad de los residuos"), " es violado, pero la homocedasticidad se mantiene, el ANOVA y las pruebas post-hoc asociadas son relativamente robustas, especialmente con tamaños de muestra grandes. Si la violación es severa, se podrían considerar métodos no paramétricos (ej., prueba de Kruskal-Wallis seguida de comparaciones post-hoc no paramétricas como la prueba de Dunn o Conover-Iman)."),

        tags$p(tags$strong("Ejemplo de código R para Tukey HSD (asumiendo supuestos cumplidos):")),
        tags$pre(class = "r-code",
          htmltools::HTML(
            "# Asumiendo que 'modelo_aov' es el resultado de un aov() y es significativo\n",
            "comparaciones_tukey <- TukeyHSD(modelo_aov, conf.level = 0.95)\n",
            "print(comparaciones_tukey)\n",
            "# La salida muestra: \n",
            "#   diff: la diferencia observada entre las medias de cada par.\n",
            "#   lwr: el límite inferior del intervalo de confianza para la diferencia.\n",
            "#   upr: el límite superior del intervalo de confianza para la diferencia.\n",
            "#   p adj: el p-valor ajustado para la comparación.\n",
            "# Si 'p adj' < 0.05, la diferencia es significativa.\n",
            "# Si el intervalo [lwr, upr] no incluye el cero, la diferencia es significativa.\n",
            "\n",
            "# Visualización de los resultados de Tukey HSD\n",
            "plot(comparaciones_tukey, las = 1, col = 'steelblue') # las=1 para etiquetas de eje y horizontales\n",
            "abline(v = 0, lty = 2, col = 'red') # Línea de referencia en cero\n",
            "\n",
            "# Para pruebas como Duncan o LSD, el paquete 'agricolae' es muy útil en agronomía:\n",
            "# library(agricolae)\n",
            "# out_duncan <- duncan.test(modelo_aov, 'fertilizante', console = TRUE)\n",
            "# plot(out_duncan) # Muestra grupos con letras\n",
            "\n",
            "# Para Games-Howell (si las varianzas son desiguales), el paquete 'rstatix' o 'userfriendlyscience':\n",
            "# library(rstatix)\n",
            "# games_howell_test(datos_agricolas, rendimiento ~ fertilizante)\n"
          )
        ),
        
        tags$hr(),
        # --- Análisis Interactivo ---
        tags$h4(class = "section-header", "Análisis Interactivo: Ponlo en Práctica"),
        tags$p("Ahora, aplica estos conceptos analizando el dataset `iris`. Usa los controles para seleccionar una variable y avanza por cada paso del análisis."),
        sidebarLayout(
            sidebarPanel(width=3,
                tags$h5("Control del Análisis"),
                selectInput(ns("iris_variable"), "Variable de Respuesta:", 
                            choices = c("Ancho del Sépalo" = "Sepal.Width", "Largo del Sépalo" = "Sepal.Length", 
                                        "Ancho del Pétalo" = "Petal.Width", "Largo del Pétalo" = "Petal.Length")),
                radioButtons(ns("dca_paso_analisis"), "Paso del Análisis:",
                    choiceNames = list(tags$span(icon("table"), "1. Ver Datos"),
                                        tags$span(icon("flask"), "2. Verif. Supuestos"),
                                        tags$span(icon("clipboard-list"), "3. ANOVA y Efectos"),
                                        tags$span(icon("sitemap"), "4. Post-Hoc")),
                    choiceValues = c("datos", "supuestos", "anova", "posthoc")
                )
            ),
            mainPanel(width=9,
                uiOutput(ns("dca_salida_analisis"))
            )
        ),
        tags$hr(),

        # Resultado esperado
        tags$h4(class = "section-header", "Resultado Esperado"),
        tags$p("Al finalizar esta sesión, los participantes podrán:",
          tags$ul(
            tags$li("Formular hipótesis nula y alternativa para problemas de comparación."),
            tags$li("Comprender y aplicar el DCA."),
            tags$li("Realizar un ANOVA de una vía en R con ", tags$code("aov()"), "."),
            tags$li("Interpretar la tabla ANOVA (con ayuda de ", tags$code("broom::tidy()"),")."),
            tags$li("Diagnosticar supuestos (Shapiro-Wilk, Levene, gráficos)."),
            tags$li("Realizar e interpretar pruebas post-hoc (Tukey HSD) si el ANOVA es significativo."),
            tags$li("Calcular e interpretar el tamaño del efecto (\\(\\eta^2\\) con ", tags$code("effectsize"),")."),
            tags$li("Generar y documentar scripts de R para estos análisis.")
          )
        ),
      ),

      # ===== PESTAÑA 4: Simulación Interactiva =====
      nav_panel(
        title = "4. Simulación de ANOVA",
        h4(class = "section-header", "Simulación: El Equilibrio entre Señal y Ruido"),
        p("Usa los deslizadores para ver cómo la diferencia entre medias ('señal') y la variabilidad ('ruido') afectan la capacidad del ANOVA para detectar diferencias."),
        tags$div(class = "plot-box",
          fluidRow(
            column(4,
              sliderInput(ns("mean_fertB_diff"), "Diferencia Media Fert. B vs A (\\(\\mu_B - \\mu_A\\)):",
                          min = -1.0, max = 1.5, value = 0.8, step = 0.1),
              sliderInput(ns("mean_fertC_diff"), "Diferencia Media Fert. C vs A (\\(\\mu_C - \\mu_A\\)):",
                          min = -1.0, max = 1.5, value = 0.2, step = 0.1),
              sliderInput(ns("sd_error_sim"), "Desv. Estándar Error (\\(\\sigma_{\\epsilon}\\)):",
                          min = 0.1, max = 1.5, value = 0.5, step = 0.1),
              sliderInput(ns("n_reps_sim"), "Número de Replicaciones (n):",
                          min = 5, max = 100, value = 30, step = 5)
            ),
            column(8,
              plotOutput(ns("boxplot_sim"), height = "300px"),
              tags$h5(tags$b("Tabla ANOVA:")),
              tableOutput(ns("anova_table_sim")),
              tags$h5(tags$b("Prueba de Normalidad de Residuos (Shapiro-Wilk):")),
              verbatimTextOutput(ns("shapiro_test_sim")),
              tags$h5(tags$b("Prueba de Homocedasticidad (Levene):")),
              verbatimTextOutput(ns("levene_test_sim")),
              tags$h5(tags$b("Eta-cuadrado (\\(\\eta^2\\)):")),
              verbatimTextOutput(ns("eta_squared_sim")),
              tags$h5(tags$b("Pruebas de Comparación Múltiple (Tukey HSD):")),
              verbatimTextOutput(ns("tukey_test_sim")),
              tags$h5(tags$b("Diagnóstico Gráfico de Supuestos:")),
              fluidRow(
                column(6, plotOutput(ns("residuals_vs_fitted_sim"), height = "250px")),
                column(6, plotOutput(ns("qqplot_residuals_sim"), height = "250px"))
              )
            )
          )
        ),
      ),
      # ===== PESTAÑA 5: Referencias =====
      nav_panel(
        title = "Referencia Bibliograficas",
        tags$h4(class = "section-header", "Referencia Bibliograficas"),
        tags$ul(
          tags$li("Cohen, J. (1988). ", tags$em("Statistical power analysis for the behavioral sciences"), " (2nd ed.). Lawrence Erlbaum Associates."),
          tags$li("Dalgaard, P. (2008). ", tags$em("Introductory Statistics with R"), ". Springer."),
          tags$li("Field, A. (2013). ", tags$em("Discovering statistics using IBM SPSS statistics"), " (4th ed.). Sage."),
          tags$li("Montgomery, D. C. (2017). ", tags$em("Design and analysis of experiments"), " (9th ed.). John Wiley & Sons."),
          tags$li("Kassambara, A. (2023). ", tags$em("rstatix: Pipe-Friendly Framework for Basic Statistical Tests"), ". R package version 0.7.2. ", tags$a(href="https://CRAN.R-project.org/package=rstatix", "https://CRAN.R-project.org/package=rstatix")),
          tags$li("Bakdash, J.Z. & Marusich, L.R. (2017). ", tags$em("effectsize: Estimation of Effect Size and Standardized Parameters"), ". R package. Ver también documentación de `effectsize` para R: ", tags$a(href="https://easystats.github.io/effectsize/", "https://easystats.github.io/effectsize/"))
        )
      )
    )
  )
}

# Server para la Sesión 4
session4Server <- function(input, output, session) {
  # Namespace para los IDs de los inputs y outputs
  ns <- session$ns
  
  # --- LÓGICA PARA LA PESTAÑA 2: Constructor de DCA ---
  dca_layout_data <- eventReactive(input$dca_gen, {
      req(input$dca_trat, input$dca_rep)
      tratamientos <- paste0("T", 1:input$dca_trat)
      # Crear un data frame con todas las parcelas y asignar tratamientos
      total_parcelas <- input$dca_trat * input$dca_rep
      diseno <- data.frame(
          parcela = 1:total_parcelas,
          tratamiento = factor(rep(tratamientos, times = input$dca_rep))
      )
      # Aleatorizar el orden
      diseno <- diseno[sample(nrow(diseno)), ]
      diseno$row <- ceiling(diseno$parcela / ceiling(sqrt(total_parcelas)))
      diseno$col <- (diseno$parcela - 1) %% ceiling(sqrt(total_parcelas)) + 1
      diseno
  }, ignoreNULL = FALSE)
  
  output$dca_plot_layout <- renderPlot({
      df <- dca_layout_data()
      ggplot(df, aes(x = col, y = row, fill = tratamiento)) +
          geom_tile(color = "white", lwd=1.5) +
          geom_text(aes(label = tratamiento), color="white", fontface="bold") +
          labs(title="Layout Aleatorizado de un DCA", x="", y="") +
          theme_void() +
          theme(legend.position = "bottom")
  })

  # --- LÓGICA PARA LA PESTAÑA 3: Análisis de un DCA ---
  
  # Reactivos para el modelo y los datos, basados en la selección del usuario
  dca_datos_analisis <- reactive({
      req(input$iris_variable)
      iris %>% rename(respuesta = !!sym(input$iris_variable), tratamiento = Species)
  })
  dca_modelo_analisis <- reactive({
      req(dca_datos_analisis())
      aov(respuesta ~ tratamiento, data = dca_datos_analisis())
  })

  # --- Salidas individuales para cada paso del análisis ---
  output$dca_tabla_datos <- DT::renderDataTable({
      DT::datatable(dca_datos_analisis(), options=list(pageLength=5, searching=F), rownames=F)
  })
  output$dca_boxplot_datos <- renderPlot({
      ggplot(dca_datos_analisis(), aes(x=tratamiento, y=respuesta, fill=tratamiento)) +
          geom_boxplot(alpha=0.7) + theme_minimal() + labs(y=input$iris_variable)
  })
  output$dca_plot_supuestos <- renderPlot({
      modelo <- dca_modelo_analisis()
      p1 <- ggplot(modelo, aes(.fitted, .resid)) + geom_point() + geom_hline(yintercept=0, lty=2, color="red") + labs(title="Residuales vs. Ajustados")
      p2 <- ggplot(modelo, aes(sample=.resid)) + stat_qq() + stat_qq_line(color="red") + labs(title="Normal Q-Q Plot")
      p1 + p2
  })
  output$dca_prueba_levene <- renderPrint({ car::leveneTest(respuesta ~ tratamiento, data = dca_datos_analisis()) })
  output$dca_tabla_anova <- renderPrint({ summary(dca_modelo_analisis()) })
  output$dca_eta_squared <- renderPrint({ effectsize::eta_squared(dca_modelo_analisis()) })
  output$dca_posthoc <- renderPrint({ TukeyHSD(dca_modelo_analisis()) })

  # --- UI Dinámico para mostrar los pasos ---
  output$dca_salida_analisis <- renderUI({
      paso <- input$dca_paso_analisis
      req(paso)
      
      if (paso == "datos") {
          tagList(
              h5("1. Exploración de Datos"),
              fluidRow(
                  column(6, DT::dataTableOutput(ns("dca_tabla_datos"))),
                  column(6, plotOutput(ns("dca_boxplot_datos")))
              )
          )
      } else if (paso == "supuestos") {
          tagList(
              h5("2. Verificación de Supuestos"),
              plotOutput(ns("dca_plot_supuestos")),
              h6("Prueba de Levene para Homocedasticidad:"),
              verbatimTextOutput(ns("dca_prueba_levene"))
          )
      } else if (paso == "anova") {
          tagList(
              h5("3. Tabla ANOVA y Tamaño del Efecto"),
              verbatimTextOutput(ns("dca_tabla_anova")),
              h6("Eta-cuadrado (η²):"),
              verbatimTextOutput(ns("dca_eta_squared"))
          )
      } else if (paso == "posthoc") {
          # Verificar si el ANOVA es significativo antes de mostrar post-hoc
          p_val <- broom::tidy(dca_modelo_analisis())$p.value[1]
          if(p_val < 0.05){
              tagList(
                  h5("4. Comparaciones Múltiples (Post-Hoc)"),
                  div(class="alert alert-success", "El ANOVA fue significativo. Se procede con la prueba de Tukey HSD."),
                  verbatimTextOutput(ns("dca_posthoc"))
              )
          } else {
                div(class="alert alert-warning", "El ANOVA no fue significativo (p > 0.05), no se realizan pruebas post-hoc.")
          }
      }
  })

  # --- LÓGICA PARA LA PESTAÑA 4: Simulación Interactiva ---
  # Generación de datos reactivos
  sim_data <- reactive({
    req(input$mean_fertB_diff, input$mean_fertC_diff, input$sd_error_sim, input$n_reps_sim)
    # Usar el tiempo para variabilidad, pero para consistencia en una demo, un seed fijo es mejor
    # Para el curso, Sys.time() está bien para mostrar variabilidad natural.
    set.seed(as.integer(Sys.time())) 

    n <- input$n_reps_sim
    sd_val <- input$sd_error_sim
    mean_A <- 5.0 
    mean_B <- mean_A + input$mean_fertB_diff
    mean_C <- mean_A + input$mean_fertC_diff

    rendimiento <- c(rnorm(n, mean = mean_A, sd = sd_val),
                      rnorm(n, mean = mean_B, sd = sd_val),
                      rnorm(n, mean = mean_C, sd = sd_val))
    fertilizante <- factor(rep(c('A', 'B', 'C'), each = n, levels = c('A', 'B', 'C'))) # Asegurar niveles
    data.frame(rendimiento, fertilizante)
  })

  # Boxplot
  output$boxplot_sim <- renderPlot({
    df_sim <- sim_data()
    ggplot(df_sim, aes(x = fertilizante, y = rendimiento, fill = fertilizante)) +
      geom_boxplot(alpha = 0.7, na.rm = TRUE) +
      labs(title = 'Rendimiento Simulado por Tipo de Fertilizante', 
            y = 'Rendimiento (t/ha)', x = 'Tipo de Fertilizante') +
      theme_minimal(base_size = 12) +
      theme(legend.position = 'none')
  })

  # Modelo ANOVA
  anova_model_sim <- reactive({
    aov(rendimiento ~ fertilizante, data = sim_data())
  })

  # Tabla ANOVA
  output$anova_table_sim <- renderTable({
    req(anova_model_sim())
    # Usar broom::tidy para un formato limpio
    df_tidy <- broom::tidy(anova_model_sim())
    # Renombrar columnas para mejor presentación en español
    df_tidy %>%
      dplyr::select(term, df, sumsq, meansq, statistic, p.value) %>%
      dplyr::rename(
        `Fuente de Variación` = term,
        `G.L.` = df,
        `Suma de Cuad.` = sumsq,
        `Cuadrado Medio` = meansq,
        `Estadístico F` = statistic,
        `P-valor` = p.value
      )
  }, striped = TRUE, bordered = TRUE, na = " ", digits = 4, align = 'lrrrrr')

  # Prueba de Normalidad (Shapiro-Wilk)
  output$shapiro_test_sim <- renderPrint({
    req(anova_model_sim())
    residuos <- residuals(anova_model_sim())
    if (length(residuos) >=3 && length(residuos) <= 5000) { # Shapiro necesita entre 3 y 5000 obs
        shapiro.test(residuos)
    } else {
        "No se puede realizar Shapiro-Wilk (Nº de residuos fuera de rango [3, 5000])."
    }
  })

  # Prueba de Homocedasticidad (Levene)
  output$levene_test_sim <- renderPrint({
    req(sim_data())
    # Asegúrate que 'car' esté cargado
    tryCatch({
      car::leveneTest(rendimiento ~ fertilizante, data = sim_data())
    }, error = function(e) {
      paste("Error en Levene Test:", e$message, "(Podría ser por varianza cero en algún grupo si SD es muy bajo)")
    })
  })

  # Cálculo de Eta-cuadrado
  output$eta_squared_sim <- renderPrint({
    req(anova_model_sim())
    # Usar effectsize para Eta-cuadrado (no parcial para ANOVA de una vía)
    es <- effectsize::eta_squared(anova_model_sim(), partial = FALSE, ci = 0.95)
    print(es) # effectsize::print.effectsize_table da una buena salida
    # Interpretación manual (effectsize ya lo hace, pero para reforzar)
    # paste0("Eta-cuadrado (η²): ", round(es$Eta2, 4), 
    #        " [IC 95%: ", round(es$CI_low, 4), ", ", round(es$CI_high, 4), "]\n",
    #        "Interpretación (Cohen): Efecto ", es$Interpretation) # Ajustar según la salida de effectsize
  })
  
  # Pruebas Post-Hoc (Tukey HSD)
  output$tukey_test_sim <- renderPrint({
    req(anova_model_sim())
    anova_summary_df <- broom::tidy(anova_model_sim())
    # Buscar el p-valor del factor 'fertilizante'
    # El término para el factor podría no ser siempre 'fertilizante' si la fórmula cambia,
    # pero aquí es fijo.
    p_value_factor_row <- anova_summary_df[anova_summary_df$term == "fertilizante", ]
    
    if (nrow(p_value_factor_row) == 0) {
      return("Factor 'fertilizante' no encontrado en el modelo ANOVA.")
    }
    p_value_factor <- p_value_factor_row$p.value

    if (!is.na(p_value_factor) && p_value_factor < 0.05) {
      tukey_results <- TukeyHSD(anova_model_sim())
      print(tukey_results)
    } else {
      "El ANOVA no es significativo para el factor 'fertilizante' (p ≥ 0.05), no se realizan comparaciones múltiples."
    }
  })

  # Gráficos de diagnóstico de supuestos
  output$residuals_vs_fitted_sim <- renderPlot({
    req(anova_model_sim())
    plot(anova_model_sim(), which = 1, main = "Residuos vs. Valores Ajustados", pch = 19, cex=0.8)
    abline(h = 0, col = "red", lty = 2)
  })

  output$qqplot_residuals_sim <- renderPlot({
    req(anova_model_sim())
    model_res <- residuals(anova_model_sim())
    # Usar qqnorm y qqline de stats
    qqnorm(model_res, main = "Normal Q-Q Plot de Residuos", pch = 19, cex=0.8)
    qqline(model_res, col = "red", lwd = 2)
  })
}