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

      # ===== PESTAÑA 5: PRUEBAS NO PARAMÉTRICAS =====
      nav_panel(
          title = "5. Pruebas No Paramétricas",
          h4(class = "section-header", "5.1 ¿Cuándo y por qué usar pruebas no paramétricas?"),
          tags$p("Las pruebas paramétricas como el ANOVA son potentes, pero dependen de supuestos estrictos, principalmente que los residuos del modelo sigan una distribución normal y que las varianzas entre grupos sean homogéneas. Cuando estos supuestos se violan gravemente, los resultados del ANOVA pueden no ser fiables."),
          tags$p("Las ", tags$b("pruebas no paramétricas"), " son la alternativa. No asumen una distribución específica para los datos y, en su lugar, trabajan con los ", tags$b("rangos"), " de las observaciones. Son ideales para:"),
          tags$ul(
            tags$li("Datos que no siguen una distribución normal (ej. datos muy asimétricos)."),
            tags$li("Datos donde se viola la homogeneidad de varianzas (aunque algunas pruebas no paramétricas también son sensibles a esto)."),
            tags$li("Datos ordinales (ej. escalas de severidad de una enfermedad: 'leve', 'moderada', 'severa')."),
            tags$li("Muestras pequeñas donde es difícil verificar la normalidad.")
          ),
          tags$p(tags$strong("La desventaja:"), " Generalmente, si los supuestos del ANOVA se cumplen, las pruebas no paramétricas son menos potentes, lo que significa que tienen una menor probabilidad de detectar una diferencia real entre los grupos."),

          tags$hr(),

          h4(class = "section-header", "5.2 La Prueba de Kruskal-Wallis: El 'ANOVA no paramétrico'"),
          tags$p("La prueba de Kruskal-Wallis es la alternativa no paramétrica al ANOVA de una vía. Se utiliza para determinar si existen diferencias estadísticamente significativas entre dos o más grupos independientes en una variable continua o ordinal."),
          tags$div(class = "content-row",
            tags$div(class = "main-content",
              tags$p("La lógica de la prueba es la siguiente:"),
              tags$ol(
                tags$li("Se combinan todos los datos de todos los grupos en una sola lista."),
                tags$li("Se ordenan los datos de menor a mayor y se les asigna un rango (1 para el más pequeño, 2 para el siguiente, etc.). En caso de empates, se asigna el rango promedio."),
                tags$li("Se calcula la suma de los rangos para cada grupo."),
                tags$li("El estadístico de prueba \\(H\\) evalúa si la suma de rangos observada en cada grupo es significativamente diferente de lo que se esperaría por puro azar si todos los grupos provinieran de la misma población.")
              ),
              tags$b("Hipótesis de la prueba de Kruskal-Wallis:"),
              tags$ul(
                tags$li(tags$strong("\\(H_0\\):"), " Las medianas de todos los grupos son iguales (o, más formalmente, todas las muestras provienen de la misma distribución)."),
                tags$li(tags$strong("\\(H_1\\):"), " Al menos una mediana de grupo es diferente.")
              )
            ),
            tags$div(class = "note-cloud",
              tags$strong("Analogía Conceptual:"),
              tags$p("Piensa en el ANOVA como una prueba sobre las medias, asumiendo una campana de Gauss. Kruskal-Wallis es una prueba sobre las medianas (o la localización central), sin asumir ninguna forma particular para la distribución de los datos.")
            )
          ),
          tags$pre(class = "r-code",
            htmltools::HTML(
              "# Ejemplo de Kruskal-Wallis en R\n",
              "# (Crearemos datos no normales para ilustrar)\n",
              "set.seed(456)\n",
              "datos_no_normales <- data.frame(\n",
              "  respuesta = c(rexp(30, rate = 1), rexp(30, rate = 0.5), rexp(30, rate = 1.2)),\n",
              "  tratamiento = factor(rep(c('A', 'B', 'C'), each = 30))\n",
              ")\n",
              "kruskal.test(respuesta ~ tratamiento, data = datos_no_normales)\n"
            )
          ),
          tags$h5("Interpretación de la Salida de `kruskal.test`"),
          tags$p("La salida de `kruskal.test()` es concisa y te proporcionará:"),
          tags$ul(
            tags$li(
              tags$code("Kruskal-Wallis chi-squared"),
              ": Es el valor del estadístico de prueba \\(H\\). Un valor más grande indica una mayor diferencia entre los rangos de los grupos."
            ),
            tags$li(
              tags$code("df"),
              ": Grados de libertad, que es el número de grupos menos 1 (\\(k-1\\))."
            ),
            tags$li(
              tags$code("p-value"), 
              ": El resultado más importante. Si este valor es menor que tu nivel de significancia (ej., 0.05), rechazas la hipótesis nula y concluyes que hay una diferencia significativa en las medianas de al menos dos grupos."
            )
          ),
          tags$pre(class = "r-output",
            htmltools::HTML(
              "        Kruskal-Wallis rank sum test\n\n",
              "data:  respuesta by tratamiento\n",
              "Kruskal-Wallis chi-squared = 7.1869, df = 2, p-value = 0.0275\n",
              "\n",
              "-> Interpretación: Con un p-valor de 0.0275 (< 0.05), rechazamos H0. Hay evidencia\n",
              "   estadística para afirmar que no todas las medianas de los tratamientos son iguales.\n"
            )
          ),

          tags$hr(),

          h4(class = "section-header", "5.3 Pruebas Post-Hoc No Paramétricas"),
          tags$p("Un resultado significativo en Kruskal-Wallis nos obliga a preguntar: ¿pero qué grupos son diferentes entre sí? Para esto, usamos pruebas post-hoc no paramétricas."),
          tags$p("Una excelente opción es la ", tags$b("Prueba de Dunn"), ", que realiza comparaciones por pares de las sumas de rangos ajustando los p-valores. Usaremos el paquete ", tags$code("rstatix"), " que nos da una salida clara y ordenada."),
          tags$pre(class = "r-code",
            htmltools::HTML(
              "# Prueba Post-Hoc de Dunn con el paquete 'rstatix'\n",
              "# install.packages('rstatix')\n",
              "library(rstatix)\n",
              "\n",
              "# Aplicar la prueba de Dunn a nuestros datos\n",
              "# La sintaxis es similar a una fórmula\n",
              "dunn_test(respuesta ~ tratamiento, data = datos_no_normales, p.adjust.method = 'bonferroni')\n"
            )
          ),
          tags$h5("Interpretación de la Salida de `rstatix::dunn_test`"),
          tags$p("La salida de `rstatix` es un `data.frame` (tibble) muy fácil de leer:"),
          tags$ul(
            tags$li(tags$code("group1"), " y ", tags$code("group2"), ": El par de grupos que se está comparando."),
            tags$li(
              tags$code("p"), # Usar coma en lugar de dos puntos
              ": El p-valor sin ajustar."
            ),
            tags$li(
              tags$code("p.adj"), # Usar coma en lugar de dos puntos
              ": El p-valor ajustado. ", tags$b("Este es el que debes interpretar.")
            ),
            tags$li(
              tags$code("p.adj.signif"), # Usar coma en lugar de dos puntos
              ": Una columna de conveniencia que muestra asteriscos de significancia (ej., '*' si p.adj < 0.05)."
            )
          ),
          tags$pre(class = "r-output",
            htmltools::HTML(
              "# A tibble: 3 × 9\n",
              "  .y.       group1 group2    n1    n2 statistic        p      p.adj p.adj.signif\n",
              "* <chr>     <chr>  <chr>  <int> <int>     <dbl>    <dbl>      <dbl> <chr>       \n",
              "1 respuesta A      B         30    30     -2.68 0.00739   0.0222     *           \n",
              "2 respuesta A      C         30    30      0.376 0.707     1          ns          \n",
              "3 respuesta B      C         30    30      2.30  0.0213    0.0640     ns          \n",
              "\n",
              "-> Interpretación: El p-valor ajustado ('p.adj') para la comparación A - B es 0.0222 (*),\n",
              "   lo que indica una diferencia significativa. Las otras comparaciones no son\n",
              "   significativas (ns).\n"
            )
          ),

          tags$hr(),
          
          h4(class = "section-header", "5.4 Análisis Interactivo No Paramétrico"),
          tags$p("Ahora, aplica estos conceptos. Selecciona un tipo de dato y compara los resultados de Kruskal-Wallis con un ANOVA tradicional. Observa cómo cada prueba maneja diferentes tipos de datos."),
          sidebarLayout(
              sidebarPanel(width=3,
                  tags$h5("1. Seleccionar Tipo de Datos"),
                  radioButtons(ns("np_dataset_choice"), "Tipo de Dataset:",
                      choices = c("Numérico (Rendimiento)" = "numeric",
                                  "Ordinal (Severidad de Enfermedad)" = "ordinal"),
                      selected = "numeric"
                  ),
                  tags$h5("2. Seleccionar Tipo de Prueba"),
                  radioButtons(ns("np_paso_analisis"), "Prueba a Realizar:",
                      choiceNames = list(tags$span(icon("chart-line"), "ANOVA (Paramétrica)"),
                                          tags$span(icon("sort-amount-up"), "Kruskal-Wallis (No Paramétrica)")),
                      choiceValues = c("anova", "kruskal")
                  )
              ),
              mainPanel(width=9,
                  uiOutput(ns("np_salida_analisis"))
              )
          )
      ),

      # ===== PESTAÑA 6: ANOVA vs. KRUSKAL-WALLIS: GUÍA DE DECISIÓN
      nav_panel(
        title = "6. ANOVA vs. Kruskal-Wallis",
        h4(class = "section-header", "6.1 Comparación Directa: ¿Cuándo usar cada prueba?"),
        p("Hemos visto dos formas de comparar más de dos grupos: el ANOVA (paramétrico) y la prueba de Kruskal-Wallis (no paramétrica). La elección entre ellas es una de las decisiones más comunes y cruciales en el análisis de datos. Aquí resumimos sus diferencias clave."),

        tags$table(class = "table table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Característica"),
              tags$th("ANOVA de una vía"),
              tags$th("Prueba de Kruskal-Wallis")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(tags$strong("Parámetro de Interés")),
              tags$td("Compara las ", tags$b("medias"), " (\\(\\mu\\)) de los grupos."),
              tags$td("Compara las ", tags$b("medianas"), " o, más generalmente, la localización central de las distribuciones.")
            ),
            tags$tr(
              tags$td(tags$strong("Supuestos Clave")),
              tags$td(tags$ul(
                  tags$li("Los residuos siguen una distribución ", tags$b("normal"), "."),
                  tags$li("Las varianzas entre grupos son ", tags$b("homogéneas"), " (homocedasticidad)."),
                  tags$li("Las observaciones son independientes.")
              )),
              tags$td(tags$ul(
                  tags$li("No asume una distribución específica (libre de distribución)."),
                  tags$li("Asume que las distribuciones de cada grupo tienen la ", tags$b("misma forma"), " (aunque pueden tener diferente localización)."),
                  tags$li("Las observaciones son independientes.")
              ))
            ),
            tags$tr(
              tags$td(tags$strong("Tipo de Datos")),
              tags$td("Numéricos (continuos o discretos)."),
              tags$td("Numéricos y también ", tags$b("ordinales"), " (ej. escalas de severidad).")
            ),
            tags$tr(
              tags$td(tags$strong("Potencia Estadística")),
              tags$td("Más potente (mayor probabilidad de detectar una diferencia real) ", tags$b("si se cumplen los supuestos"), "."),
              tags$td("Menos potente que el ANOVA si los datos son normales, pero ", tags$b("más potente si los supuestos del ANOVA son violados"), " severamente (ej. por outliers).")
            ),
            tags$tr(
              tags$td(tags$strong("Pruebas Post-Hoc Comunes")),
              tags$td("Tukey HSD, Scheffé, Duncan (si hay homocedasticidad). Games-Howell (si no hay homocedasticidad)."),
              tags$td("Prueba de Dunn, Conover-Iman.")
            )
          )
        ),
        
        h4(class = "section-header", "6.2 El Concepto de Potencia Estadística"),
        tags$div(class = "content-row",
            tags$div(class = "main-content",
                p("Cuando decimos que el ANOVA es 'más potente', nos referimos a la ", tags$b("potencia estadística (\\(1 - \beta\\))"), ", que es la probabilidad de rechazar correctamente una hipótesis nula falsa. En otras palabras, es la capacidad de la prueba para detectar un efecto o diferencia cuando realmente existe."),
                p("Imagina que un nuevo fertilizante realmente aumenta el rendimiento. Una prueba potente tendrá una alta probabilidad de detectar este aumento y dar un p-valor significativo. Una prueba menos potente podría pasar por alto esta diferencia y dar un p-valor no significativo."),
                p(tags$strong("La regla general es:"), " usa siempre la prueba más potente para la cual tus datos cumplan los supuestos. Si tus datos son normales y las varianzas son homogéneas, el ANOVA es la mejor opción. Si no, pierdes potencia y credibilidad, y Kruskal-Wallis se convierte en la alternativa más potente y adecuada.")
            ),
            tags$div(class = "note-cloud",
                tags$strong("Analogía Agronómica:"),
                p("La potencia es como la sensibilidad de un sensor de humedad del suelo. Un sensor potente (alta sensibilidad) puede detectar pequeños cambios en la humedad. Un sensor poco potente (baja sensibilidad) podría no registrar un cambio a menos que sea muy grande. Al elegir una prueba estadística, queremos el 'sensor' más sensible y apropiado para nuestras condiciones (nuestros datos).")
            )
        ),

        h4(class = "section-header", "6.3 Árbol de Decisión: ¿Qué prueba elijo?"),
        div(class="image-box", style="text-align: center;",
            p(strong("1. ¿Cuál es tu variable de respuesta?")),
            p(icon("arrow-down")),
            div(class="decision-box", "Numérica (continua/discreta) u Ordinal"),
            br(),
            div(class="row",
                div(class="col-md-6", style="border-right: 1px solid #ccc;",
                    p(strong("Si es Numérica...")),
                    p(icon("arrow-down")),
                    div(class="decision-box", "2. ¿Se cumple el supuesto de Normalidad de Residuos?"),
                    p("(Evaluar con QQ-plot, Shapiro-Wilk)"),
                    br(),
                    div(class="row",
                        div(class="col-md-6", style="border-right: 1px solid #eee;",
                            p(strong("SÍ")),
                            p(icon("arrow-down")),
                            div(class="decision-box", "3. ¿Se cumple la Homocedasticidad?"),
                            p("(Evaluar con Levene)"),
                            br(),
                            p(strong("SÍ ->"), span(class="text-success", "ANOVA + Tukey HSD")),
                            p(strong("NO ->"), span(class="text-warning", "ANOVA + Games-Howell"))
                        ),
                        div(class="col-md-6",
                            p(strong("NO")),
                            p(icon("arrow-down")),
                            span(class="text-info", strong("Kruskal-Wallis + Dunn"))
                        )
                    )
                ),
                div(class="col-md-6",
                    p(strong("Si es Ordinal...")),
                    p(icon("arrow-down")),
                    span(class="text-info", strong("Kruskal-Wallis + Dunn")),
                    p("(El ANOVA no es apropiado)")
                )
            )
        ),

        tags$hr(),
        
        h4(class = "section-header", "6.4 Laboratorio Comparativo Interactivo"),
        p("Selecciona un conjunto de datos simulados y observa los resultados de ambas pruebas lado a lado. Presta atención a los p-valores. ¿Coinciden las conclusiones? ¿Cuándo difieren?"),
        sidebarLayout(
            sidebarPanel(width=3,
                tags$h5("Control del Laboratorio"),
                radioButtons(ns("comp_dataset"), "Seleccionar Dataset Simulado:",
                    choices = c("Datos Normales (ANOVA ideal)" = "normal",
                                "Datos Asimétricos (K-W ideal)" = "skewed",
                                "Datos con Outliers" = "outliers")
                )
            ),
            mainPanel(width=9,
                plotOutput(ns("comp_plot")),
                fluidRow(
                    column(6,
                        h5(class="text-primary", "Resultados ANOVA"),
                        verbatimTextOutput(ns("comp_anova_output"))
                    ),
                    column(6,
                        h5(class="text-success", "Resultados Kruskal-Wallis"),
                        verbatimTextOutput(ns("comp_kruskal_output"))
                    )
                )
            )
        )
      ),

      # ===== PESTAÑA 7: Referencias =====
      nav_panel(
        title = "Referencia Bibliograficas",
        tags$h4(class = "section-header", "Referencia Bibliograficas"),
        tags$ul(
          tags$li("Cohen, J. (1988). ", tags$em("Statistical power analysis for the behavioral sciences"), " (2nd ed.). Lawrence Erlbaum Associates."),
          tags$li("Dalgaard, P. (2008). ", tags$em("Introductory Statistics with R"), ". Springer."),
          tags$li("Field, A. (2013). ", tags$em("Discovering statistics using IBM SPSS statistics"), " (4th ed.). Sage."),
          tags$li("Montgomery, D. C. (2017). ", tags$em("Design and analysis of experiments"), " (9th ed.). John Wiley & Sons."),
          tags$li("Kassambara, A. (2023). ", tags$em("rstatix: Pipe-Friendly Framework for Basic Statistical Tests"), ". R package version 0.7.2. ", tags$a(href="https://CRAN.R-project.org/package=rstatix", "https://CRAN.R-project.org/package=rstatix")),
          tags$li("Bakdash, J.Z. & Marusich, L.R. (2017). ", tags$em("effectsize: Estimation of Effect Size and Standardized Parameters"), ". R package. Ver también documentación de `effectsize` para R: ", tags$a(href="https://easystats.github.io/effectsize/", "https://easystats.github.io/effectsize/")),
            tags$li("Hollander, M., Wolfe, D. A., & Chicken, E. (2013). ", tags$em("Nonparametric statistical methods"), " (3rd ed.). John Wiley & Sons."),
            tags$li("Dunn, O. J. (1964). Multiple comparisons using rank sums. ", tags$em("Technometrics, 6"),"(3), 241-252."),
            tags$li("Cousineau, D., & Chartier, S. (2010). Outliers detection and treatment: a review. ", tags$em("International Journal of Psychological Research, 3"),"(1), 58-67.")
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

  # ===== LÓGICA PARA LA PESTAÑA 5: ANÁLISIS NO PARAMÉTRICO =====

  # Reactivo para generar los datos según la elección del usuario
  np_datos_analisis <- reactive({
      req(input$np_dataset_choice)
      set.seed(123) # Para consistencia en los datos generados
      
      if (input$np_dataset_choice == "numeric") {
          # Datos numéricos continuos con violación de normalidad (distribución exponencial)
          # T simula 3 tratamientos de control de plagas y la respuesta es el % de daño foliar
          df <- data.frame(
            respuesta = c(rexp(25, rate = 1.5), rexp(25, rate = 0.8), rexp(25, rate = 1.6)),
            tratamiento = factor(rep(c('Control', 'Tratamiento_A', 'Tratamiento_B'), each = 25))
          )
          attr(df, "tipo") <- "Numérico: % Daño Foliar"
          return(df)
      } else { # "ordinal"
          # Datos ordinales: escala de severidad de una enfermedad (ej. roya)
          # Los factores ordenados son clave aquí.
          niveles_severidad <- c("Ninguna", "Leve", "Moderada", "Severa")
          
          df <- data.frame(
              respuesta = factor(
                  c(sample(niveles_severidad, 20, replace = TRUE, prob = c(0.6, 0.3, 0.1, 0.0)), # Grupo Control
                    sample(niveles_severidad, 20, replace = TRUE, prob = c(0.1, 0.5, 0.3, 0.1)), # Fungicida A
                    sample(niveles_severidad, 20, replace = TRUE, prob = c(0.2, 0.6, 0.15, 0.05)) # Fungicida B
                  ),
                  levels = niveles_severidad,
                  ordered = TRUE # MUY IMPORTANTE: indicar que es una variable ordinal
              ),
              tratamiento = factor(rep(c('Control', 'Fungicida_A', 'Fungicida_B'), each = 20))
          )
          attr(df, "tipo") <- "Ordinal: Severidad de Roya"
          return(df)
      }
  })

  # --- UI Dinámico para mostrar los resultados del análisis no paramétrico ---
  output$np_salida_analisis <- renderUI({
      tipo_prueba <- input$np_paso_analisis
      req(tipo_prueba)
      datos <- np_datos_analisis()
      
      # Boxplot o Barplot dependiendo del tipo de datos
      plot_ui <- if (is.numeric(datos$respuesta)) {
          plotOutput(ns("np_boxplot_datos"))
      } else {
          plotOutput(ns("np_barplot_datos"))
      }
      
      # Título principal basado en el tipo de datos
      titulo_principal <- h5(paste("Análisis para Dataset:", attr(datos, "tipo")))
      
      # Lógica para ANOVA (advertencia para datos ordinales)
      if (tipo_prueba == "anova") {
          advertencia_anova <- if (is.ordered(datos$respuesta)) {
              div(class="alert alert-danger", "ADVERTENCIA: Aplicar ANOVA a datos ordinales es metodológicamente incorrecto. Los resultados (medias, varianzas) no tienen una interpretación clara. Se muestra solo con fines de comparación.")
          } else { NULL }

          # Convertir ordinal a numérico para poder correr ANOVA (con advertencia)
          if(is.ordered(datos$respuesta)) datos$respuesta <- as.numeric(datos$respuesta)
          
          modelo_anova <- aov(respuesta ~ tratamiento, data = datos)
          p_val_anova <- broom::tidy(modelo_anova)$p.value[1]
          
          post_hoc_ui <- if (p_val_anova < 0.05) {
              tagList(h6("Prueba Post-Hoc Paramétrica (Tukey HSD):"), verbatimTextOutput(ns("np_posthoc_tukey")))
          } else {
              div(class="alert alert-warning", "ANOVA no significativo, no se realiza post-hoc.")
          }
          
          tagList(
              titulo_principal,
              h5(class="text-primary", "Resultados del Análisis Paramétrico (ANOVA)"),
              advertencia_anova,
              plot_ui,
              h6("Tabla ANOVA:"), verbatimTextOutput(ns("np_tabla_anova")),
              post_hoc_ui
          )
      } else if (tipo_prueba == "kruskal") { # Lógica para Kruskal-Wallis
          modelo_kruskal <- kruskal.test(respuesta ~ tratamiento, data = datos)
          p_val_kruskal <- modelo_kruskal$p.value
          
          post_hoc_np_ui <- if (p_val_kruskal < 0.05) {
              tagList(h6("Prueba Post-Hoc No Paramétrica (Prueba de Dunn):"), verbatimTextOutput(ns("np_posthoc_dunn")))
          } else {
              div(class="alert alert-warning", "Kruskal-Wallis no significativo, no se realiza post-hoc.")
          }

          tagList(
              titulo_principal,
              h5(class="text-success", "Resultados del Análisis No Paramétrico (Kruskal-Wallis)"),
              plot_ui,
              h6("Resultados de Kruskal-Wallis:"), verbatimTextOutput(ns("np_prueba_kruskal")),
              post_hoc_np_ui
          )
      }
  })

  # --- Salidas para la sección no paramétrica ---
  # Gráficos (boxplot para numérico, barplot para ordinal)
  output$np_boxplot_datos <- renderPlot({
      ggplot(np_datos_analisis(), aes(x=tratamiento, y=respuesta, fill=tratamiento)) +
          geom_boxplot(alpha=0.7) + theme_minimal(base_size = 12) + 
          labs(title="Distribución de la Respuesta Numérica por Tratamiento", y = "Respuesta")
  })
  
  output$np_barplot_datos <- renderPlot({
      df <- np_datos_analisis() %>% count(tratamiento, respuesta) %>%
            group_by(tratamiento) %>% mutate(proporcion = n / sum(n))
            
      ggplot(df, aes(x = respuesta, y = proporcion, fill = respuesta)) +
          geom_bar(stat = "identity", position="dodge") +
          facet_wrap(~tratamiento) +
          theme_minimal(base_size = 12) +
          labs(title="Distribución de la Severidad Ordinal por Tratamiento", 
               x = "Nivel de Severidad", y = "Proporción") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
  })
  
  # Salidas de pruebas
  output$np_tabla_anova <- renderPrint({
      datos <- np_datos_analisis()
      if(is.ordered(datos$respuesta)) datos$respuesta <- as.numeric(datos$respuesta)
      modelo <- aov(respuesta ~ tratamiento, data = datos)
      summary(modelo)
  })
  
  output$np_posthoc_tukey <- renderPrint({
      datos <- np_datos_analisis()
      if(is.ordered(datos$respuesta)) datos$respuesta <- as.numeric(datos$respuesta)
      modelo <- aov(respuesta ~ tratamiento, data = datos)
      TukeyHSD(modelo)
  })
  
  output$np_prueba_kruskal <- renderPrint({
      kruskal.test(respuesta ~ tratamiento, data = np_datos_analisis())
  })
  
  output$np_posthoc_dunn <- renderPrint({
      datos <- np_datos_analisis()
      
      # Usar rstatix::dunn_test que maneja factores ordinales y da una salida limpia
      # y es más robusto que dunn.test::dunn.test.
      if (!requireNamespace("rstatix", quietly = TRUE)) {
          "Paquete 'rstatix' no instalado. Por favor, ejecute: install.packages('rstatix')"
      } else {
          # rstatix maneja la fórmula directamente, incluso con factores ordinales
          rstatix::dunn_test(data = datos, 
                             formula = respuesta ~ tratamiento, 
                             p.adjust.method = "bonferroni")
      }
  })

  # ===== LÓGICA PARA LA PESTAÑA 6: LABORATORIO COMPARATIVO =====
  # Reactivo para generar los datos para la comparación
  comp_datos_analisis <- reactive({
      req(input$comp_dataset)
      set.seed(321) # Para consistencia
      
      n_per_group <- 30
      tratamientos <- factor(rep(c('Trat_A', 'Trat_B', 'Trat_C'), each = n_per_group))
      
      if (input$comp_dataset == "normal") {
          # Datos ideales para ANOVA
          respuesta <- c(rnorm(n_per_group, mean = 20, sd = 4),
                         rnorm(n_per_group, mean = 25, sd = 4),
                         rnorm(n_per_group, mean = 20, sd = 4))
          titulo <- "Dataset Normal con Diferencia Real (B > A,C)"
      } else if (input$comp_dataset == "skewed") {
          # Datos con asimetría, donde K-W puede ser más apropiado
          respuesta <- c(rgamma(n_per_group, shape = 2, rate = 0.1),
                         rgamma(n_per_group, shape = 4, rate = 0.1),
                         rgamma(n_per_group, shape = 2, rate = 0.1))
          titulo <- "Dataset Asimétrico (Gamma)"
      } else { # "outliers"
          # Datos normales pero con outliers que violan el supuesto
          base <- c(rnorm(n_per_group, mean = 20, sd = 4),
                    rnorm(n_per_group, mean = 25, sd = 4),
                    rnorm(n_per_group, mean = 20, sd = 4))
          # Añadir outliers
          base[5] <- 50  # Outlier en el grupo A
          base[35] <- 5   # Outlier en el grupo B
          respuesta <- base
          titulo <- "Dataset con Outliers"
      }
      
      df <- data.frame(respuesta = respuesta, tratamiento = tratamientos)
      attr(df, "titulo") <- titulo
      return(df)
  })

  # Gráfico para la sección de comparación
  output$comp_plot <- renderPlot({
      datos <- comp_datos_analisis()
      ggplot(datos, aes(x=tratamiento, y=respuesta, fill=tratamiento)) +
          geom_boxplot() +
          geom_jitter(width=0.1, alpha=0.4) +
          theme_minimal(base_size = 14) +
          labs(title = attr(datos, "titulo"), y = "Valor de Respuesta")
  })
  
  # Salida para los resultados del ANOVA
  output$comp_anova_output <- renderPrint({
      datos <- comp_datos_analisis()
      modelo_anova <- aov(respuesta ~ tratamiento, data = datos)
      
      # Verificar supuestos para informar
      shapiro_res <- shapiro.test(residuals(modelo_anova))
      levene_res <- car::leveneTest(respuesta ~ tratamiento, data = datos)
      
      cat("--- Prueba ANOVA ---\n")
      print(summary(modelo_anova))
      
      cat("\n--- Diagnóstico de Supuestos ---\n")
      cat("Shapiro-Wilk (Normalidad de Residuos):\n")
      cat("  p-valor =", round(shapiro_res$p.value, 4), "\n")
      cat("Levene (Homocedasticidad):\n")
      cat("  p-valor =", round(levene_res$`Pr(>F)`[1], 4), "\n")
  })

  # Salida para los resultados de Kruskal-Wallis
  output$comp_kruskal_output <- renderPrint({
      datos <- comp_datos_analisis()
      
      cat("--- Prueba Kruskal-Wallis ---\n")
      print(kruskal.test(respuesta ~ tratamiento, data = datos))
      
      # Realizar prueba de Dunn si K-W es significativo
      kruskal_res <- kruskal.test(respuesta ~ tratamiento, data = datos)
      if (kruskal_res$p.value < 0.05) {
          cat("\n--- Post-Hoc (Prueba de Dunn) ---\n")
          dunn_res <- rstatix::dunn_test(data = datos, formula = respuesta ~ tratamiento, p.adjust.method = "bonferroni")
          print(dunn_res[, c("group1", "group2", "p.adj", "p.adj.signif")])
      }
  })
}