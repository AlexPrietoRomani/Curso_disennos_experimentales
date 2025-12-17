# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session10.R
# Sesión 10: MANOVA

# -------------------------------------------------------------------------
# UI per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (MANOVA)
pestanna1_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto (MANOVA)",
    
    # Título principal
    h4(class = "section-header",
       "¿Dónde encaja la MANOVA en la historia del curso?"),
    
    # Bloque introductorio
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "la MANOVA extiende el ANOVA a situaciones con ",
        strong("múltiples variables respuesta medidas en la misma unidad"),
        " (por ejemplo: rendimiento, °Brix y firmeza en el mismo arbusto). ",
        "En lugar de hacer muchas ANOVAs por separado, ",
        "probamos el efecto del tratamiento sobre el ",
        em("vector completo de respuestas.")
      )
    ),
    
    # Timeline / conexión con sesiones anteriores
    h5("Conexión con lo que ya vimos en el curso"),
    tags$ul(
      tags$li(
        strong("Sesiones 2–4: ANOVA univariante"),
        " → probamos el efecto de tratamientos sobre ",
        em("una sola respuesta a la vez"),
        " (Y: rendimiento, por ejemplo)."
      ),
      tags$li(
        strong("Sesión 9: ANCOVA"),
        " → ANOVA + covariable continua (Y ~ Trat + X)."
      ),
      tags$li(
        strong("Sesión 10: MANOVA"),
        " → mismo marco de diseño experimental, pero ahora el interés está en ",
        em("varias respuestas simultáneamente"),
        " (vector (Y₁, Y₂, …))."
      ),
      tags$li(
        strong("Sesión 11: MANCOVA"),
        " → MANOVA + covariables (respuestas múltiples + ajuste por X)."
      )
    ),
    
    hr(),
    
    # Pregunta guía
    h5("Pregunta guía de la sesión"),
    p(
      em("“Si tengo varias variables respuesta correlacionadas, ",
         "¿es buena idea hacer una ANOVA separada para cada una? ",
         "¿Qué información pierdo al hacerlo así?”")
    ),
    
    # Ejemplo agronómico ancla
    h5("Ejemplo ancla agronómico"),
    p(
      "Imagina un ensayo de variedades de arándano donde en cada parcela se mide:"
    ),
    tags$ul(
      tags$li(strong("Y₁:"), " rendimiento (kg/ha)."),
      tags$li(strong("Y₂:"), " °Brix (contenido de azúcares)."),
      tags$li(strong("Y₃:"), " firmeza del fruto.")
    ),
    p(
      "El factor principal es la ", strong("variedad"), 
      " (tratamiento fijo), y opcionalmente hay un ",
      strong("bloque"), " (sector del campo). ",
      "Las tres respuestas están fuertemente correlacionadas ",
      "(por ejemplo, variedades más productivas pueden tener diferente perfil de calidad)."
    ),
    div(
      class = "note-cloud",
      p(
        strong("Idea clave: "),
        "la MANOVA permite evaluar si la variedad tiene efecto ",
        "sobre el ", em("perfil conjunto de productividad y calidad"),
        ", aprovechando la correlación entre respuestas en lugar de ignorarla."
      )
    ),
    
    hr(),
    
    # Resultados de aprendizaje / outcomes
    h5("Resultados de aprendizaje esperados"),
    tags$ol(
      tags$li(
        "Poder explicar en lenguaje sencillo qué es una MANOVA ",
        "y en qué se diferencia de hacer varias ANOVAs por separado."
      ),
      tags$li(
        "Reconocer situaciones agronómicas típicas donde hay ",
        strong("múltiples respuestas correlacionadas"),
        " (rendimiento + calidad + firmeza, etc.)."
      ),
      tags$li(
        "Entender que la MANOVA prueba el efecto de tratamientos ",
        "sobre un ", em("vector de respuestas"), 
        " y que su potencia depende de la correlación entre ellas."
      ),
      tags$li(
        "Prepararse para las siguientes pestañas, donde se verá la notación formal, ",
        "la estructura de datos y ejemplos de análisis en R."
      )
    )
  )
}

# Pestaña 2: Conceptos clave MANOVA
pestanna2_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave & modelo",
    
    h4(class = "section-header", "Modelo MANOVA básico y sus hipótesis"),
    
    # Bloque introductorio con el modelo vectorial
    withMathJax(
      div(
        class = "alert alert-info",
        p(
          strong("Idea central: "),
          "en MANOVA no analizamos una sola respuesta Y, sino un ",
          strong("vector de respuestas"),
          " medido en la misma unidad experimental."
        ),
        p("Modelo multivariante (un factor de tratamiento, g grupos y p respuestas):"),
        helpText("
          $$\\mathbf{Y}_i \\sim \\mathcal{N}_p\\big(\\boldsymbol{\\mu}_{g(i)},\\, \\boldsymbol{\\Sigma}\\big),$$
          "),
        p(
          "donde ",
          em("\\(\\mathbf{Y}_i = (Y_{i1}, \\dots, Y_{ip})^T\\)"),
          " es el vector de respuestas de la unidad i;"
        ),
        p(
          em("\\(\\boldsymbol{\\mu}_{g(i)}\\)"),
          " es el vector de medias del grupo (tratamiento) al que pertenece la unidad i;"
        ),
        p(
          em("\\(\\boldsymbol{\\Sigma}\\)"),
          " es la matriz de covarianza común dentro de los grupos (se asume igual para todos los tratamientos)."
        )
      )
    ),
    
    fluidRow(
      # Columna izquierda: modelo e hipótesis
      column(
        width = 6,
        h5("Elementos del modelo MANOVA"),
        tags$ul(
          tags$li(
            strong("Factor de grupo (tratamiento): "),
            "por ejemplo, variedad de arándano, dosis de fertilizante, nivel de riego."
          ),
          tags$li(
            strong("Vector de respuestas: "),
            "múltiples variables continuas medidas en la misma parcela (ej. rendimiento, °Brix, firmeza)."
          ),
          tags$li(
            strong("Distribución conjunta: "),
            "se modela la respuesta como normal multivariante con una matriz de covarianza ",
            em("compartida"),
            " entre tratamientos."
          )
        ),
        withMathJax(
          div(
            h5("Hipótesis global"),
            p("En MANOVA queremos probar si los vectores de medias son iguales entre tratamientos:"),
            helpText("$$
              H_0: \\boldsymbol{\\mu}_1 = \\boldsymbol{\\mu}_2 = \\cdots = \\boldsymbol{\\mu}_g
            $$"),
            p(
              "Esto es el análogo multivariante de ANOVA, donde se prueba si todas las medias son iguales, ",
              "pero ahora la comparación es en el espacio de dimensión p."
            )
          )
        )
      ),
      
      # Columna derecha: H/E, estadísticos y supuestos
      column(
        width = 6,
        h5("Matrices H y E (entre y dentro de grupos)"),
        tags$ul(
          tags$li(
            strong("Matriz E (Error, dentro de grupos): "),
            "resume la variabilidad ",
            em("intra"),
            " tratamiento (suma de cuadrados y productos dentro de grupos)."
          ),
          tags$li(
            strong("Matriz H (Hipótesis, entre grupos): "),
            "resume la variabilidad ",
            em("entre"),
            " tratamientos (diferencias entre vectores de medias de grupo)."
          ),
          tags$li(
            "Los test MANOVA clásicos se construyen combinando H y E de distintas formas."
          )
        ),
        
        h5("Estadísticos de prueba clásicos"),
        tags$ul(
          tags$li(
            strong("Wilks' Lambda (Λ): "),
            "mide cuánta variación total queda sin explicar por las diferencias entre grupos; ",
            "valores pequeños sugieren diferencias multivariantes."
          ),
          tags$li(
            strong("Traza de Pillai: "),
            "considerada robusta frente a ciertas violaciones de supuestos; ",
            "acumula la proporción de varianza explicada por el efecto de grupo."
          ),
          tags$li(
            strong("Hotelling–Lawley, Roy: "),
            "otras formas de combinar H y E; útiles en contextos específicos."
          )
        ),
        
        h5("Supuestos básicos de MANOVA"),
        tags$ul(
          tags$li(
            strong("Normalidad multivariante: "),
            "el vector de respuestas es aproximadamente normal dentro de cada tratamiento."
          ),
          tags$li(
            strong("Homogeneidad de matrices de covarianza: "),
            "la matriz ",
            em("\\(\\boldsymbol{\\Sigma}\\)"),
            " es similar en todos los grupos (análogo multivariante de homocedasticidad)."
          ),
          tags$li(
            strong("Independencia: "),
            "las unidades experimentales son independientes (no hay correlación no modelada entre parcelas)."
          )
        ),
        
        div(
          class = "note-cloud",
          p(
            strong("Lectura práctica: "),
            "en R, funciones como ", code("manova()"), " y ",
            code("car::Anova(type = 'III')"),
            " producen Wilks, Pillai, etc. ",
            "Lo importante en esta etapa es entender ",
            em("qué"),
            " se está testeando: diferencias globales en el vector de medias."
          )
        )
      )
    )
  )
}

# Pestaña 3: Diseño & estructura de datos MANOVA
pestanna3_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & estructura de datos",
    
    h4(class = "section-header",
       "¿Cómo debe lucir un dataset para MANOVA?"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea clave: "),
        "en MANOVA trabajamos con un ",
        strong("vector de respuestas continuas"),
        " medido en la misma unidad experimental ",
        "(por ejemplo: rendimiento, °Brix y firmeza en la misma parcela)."
      )
    ),
    
    # Pequeña tabla conceptual
    h5("Estructura típica (formato ancho, una fila = una unidad experimental)"),
    tags$table(
      class = "table table-sm table-bordered",
      tags$thead(
        tags$tr(
          tags$th("block"),
          tags$th("trat"),
          tags$th("Y1_rend"),
          tags$th("Y2_brix"),
          tags$th("Y3_firmeza")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("1"),
          tags$td("Var1"),
          tags$td("9500"),
          tags$td("12.4"),
          tags$td("280")
        ),
        tags$tr(
          tags$td("1"),
          tags$td("Var2"),
          tags$td("9100"),
          tags$td("11.8"),
          tags$td("300")
        )
      )
    ),
    p(
      em(" block:"), " bloque o sector del campo (opcional). ",
      em("trat:"), " tratamiento (variedad, riego, etc.). ",
      em("Y1, Y2, Y3:"), " respuestas continuas medidas sobre la misma unidad."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Fuente de datos para explorar la estructura"),
        radioButtons(
          ns("manova_example_source"),
          "Selecciona los datos de ejemplo:",
          choices = c(
            "Dataset simulado (arándanos)" = "sim",
            "Cargar un CSV propio"         = "upload"
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("manova_example_source")),
          fileInput(
            ns("manova_file"),
            "Cargar archivo CSV:",
            accept = ".csv"
          ),
          helpText(
            "Se recomienda un CSV con al menos:",
            tags$br(),
            code("trat"), " (factor de tratamiento) y ",
            "2 o más columnas numéricas como respuestas (p.ej. ",
            code("rend"), ", ", code("brix"), ", ", code("firmeza"), ")."
          )
        ),
        hr(),
        helpText(
          "En esta pestaña el foco es ",
          strong("reconocer la estructura"),
          " y revisar correlaciones básicas entre las respuestas."
        )
      ),
      
      mainPanel(
        width = 8,
        h5("Vista previa de los datos"),
        DT::dataTableOutput(ns("manova_example_data")),
        hr(),
        
        h5("Resumen de estructura multivariante"),
        verbatimTextOutput(ns("manova_example_summary")),
        hr(),
        
        h5("Correlación entre respuestas (si hay ≥ 2 variables numéricas)"),
        verbatimTextOutput(ns("manova_example_cor")),
        hr(),
        
        h5("Gráfico Y1 vs Y2 por tratamiento"),
        helpText(
          "Se usa la primera y segunda variable numérica encontradas como Y1 e Y2. ",
          "El color representa el tratamiento (si existe una columna de factor)."
        ),
        plotOutput(ns("manova_example_plot"), height = "360px")
      )
    )
  )
}

# Pestaña 4: Simulación & análisis MANOVA
pestanna4_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & análisis MANOVA",
    
    p(
      "En esta pestaña simularemos un experimento con varias respuestas ",
      "correlacionadas (por ejemplo: rendimiento, °Brix y firmeza) para distintos tratamientos, ",
      "y compararemos ANOVA univariada vs MANOVA global."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros de simulación"),
        helpText(
          "Asumiremos 3 respuestas continuas (Y1 = rendimiento, Y2 = °Brix, Y3 = firmeza) ",
          "medidas en las mismas parcelas."
        ),
        
        numericInput(
          ns("manova_n_trat"),
          "Número de tratamientos:",
          value = 3, min = 2, step = 1
        ),
        numericInput(
          ns("manova_n_rep"),
          "Observaciones por tratamiento (n):",
          value = 10, min = 3, step = 1
        ),
        numericInput(
          ns("manova_rho"),
          "Correlación entre respuestas (ρ):",
          value = 0.6, min = -0.9, max = 0.9, step = 0.1
        ),
        numericInput(
          ns("manova_delta"),
          "Amplitud efecto de tratamiento (Δμ, en SD aproximadas):",
          value = 0.8, min = 0, step = 0.1
        ),
        
        helpText(
          "Δμ controla cuán separadas están las medias de los tratamientos en cada respuesta.",
          "ρ controla qué tan correlacionadas están las respuestas (por ejemplo, rendimiento y firmeza)."
        ),
        
        actionButton(
          ns("btn_sim_manova"),
          "Simular datos y ajustar modelos",
          class = "btn btn-success w-100 mt-2"
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # -----------------------------------------------------------------
          # Panel 4.1: Resultados numéricos
          # -----------------------------------------------------------------
          bslib::nav_panel(
            "4.1 Resultados numéricos",
            tags$h5("ANOVA univariada vs MANOVA global"),
            helpText(
              "Primero se ajusta una ANOVA por cada respuesta (Y1, Y2, Y3). ",
              "Luego se ajusta una MANOVA conjunta manova(cbind(Y1, Y2, Y3) ~ tratamiento)."
            ),
            verbatimTextOutput(ns("manova_sim_results"))
          ),
          
          # -----------------------------------------------------------------
          # Panel 4.2: Visualizaciones
          # -----------------------------------------------------------------
          bslib::nav_panel(
            "4.2 Medias & correlación",
            tags$h5("Heatmap de medias por tratamiento y respuesta"),
            helpText(
              "Cada celda muestra la media simulada de una respuesta (Y1, Y2, Y3) ",
              "para cada tratamiento. Esto resume el patrón multivariado."
            ),
            plotOutput(ns("manova_heatmap"), height = "320px"),
            hr(),
            tags$h5("Dispersión Y1 vs Y2 por tratamiento"),
            helpText(
              "Gráfico de dispersión de Y1 (rendimiento) vs Y2 (°Brix), ",
              "coloreado por tratamiento. Permite ver visualmente la correlación y ",
              "la separación entre grupos."
            ),
            plotOutput(ns("manova_scatter"), height = "320px")
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo datos reales MANOVA
pestanna5_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo datos reales",
    
    h4(class = "section-header",
       "Checklist para aplicar MANOVA a tu propio Excel"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "usar MANOVA cuando tienes varias respuestas cuantitativas ",
        "medidas sobre la misma unidad experimental y quieres evaluar ",
        "el efecto de uno o más factores sobre el conjunto completo."
      )
    ),
    
    tags$ol(
      # Paso 0
      tags$li(
        strong("Paso 0 – Definir si tiene sentido usar MANOVA"),
        tags$ul(
          tags$li(
            "Las variables respuesta deben ser ",
            strong("numéricas continuas"),
            " (por ejemplo: rendimiento, °Brix, firmeza, color, etc.)."
          ),
          tags$li(
            "Todas las respuestas se miden sobre la ",
            strong("misma unidad experimental"),
            " (la misma parcela, planta, muestra de fruta, etc.)."
          ),
          tags$li(
            "Debe tener sentido interpretar los efectos de tratamiento ",
            strong("de manera conjunta"),
            " (ejemplo: evaluar la 'calidad global' de una variedad "
            , "considerando varias respuestas)."
          ),
          tags$li(
            "Idealmente existe cierta ",
            strong("correlación"),
            " entre las respuestas; si son totalmente independientes, ",
            "la ganancia de usar MANOVA es menor."
          )
        )
      ),
      
      # Paso 1
      tags$li(
        strong("Paso 1 – Preparar y revisar los datos"),
        tags$ul(
          tags$li(
            "Organizar los datos en formato ",
            strong("ancho"),
            ": una fila por unidad experimental y una columna por respuesta."
          ),
          tags$li(
            "Ejemplo de estructura:",
            tags$br(),
            code("block   trat   Y1_rend   Y2_brix   Y3_firmeza")
          ),
          tags$li(
            "Verificar que todos los factores (por ejemplo ",
            code("trat"), ", ", code("block"), ") ",
            "están correctamente codificados como factores en R."
          ),
          tags$li(
            strong("Manejo de NA:"),
            " la función ",
            code("manova()"),
            " trabaja por defecto con casos completos; ",
            "si hay NA en alguna respuesta, esa fila suele quedar fuera."
          ),
          tags$li(
            "Antes de ajustar el modelo, es útil revisar una ",
            strong("matriz de correlación"),
            " entre las respuestas y algunos diagramas de dispersión ",
            "(ej. Y1 vs Y2 coloreado por tratamiento)."
          )
        )
      ),
      
      # Paso 2
      tags$li(
        strong("Paso 2 – Ajustar el modelo MANOVA básico"),
        tags$ul(
          tags$li(
            "Modelo base sin bloque:",
            tags$br(),
            code("fit_manova <- manova(cbind(Y1, Y2, Y3) ~ trat, data = datos)")
          ),
          tags$li(
            "Si el diseño incluye bloques fijos (por ejemplo un RCBD simple):",
            tags$br(),
            code("fit_manova <- manova(cbind(Y1, Y2, Y3) ~ block + trat, data = datos)")
          ),
          tags$li(
            "Obtener los tests globales (Wilks, Pillai, etc.):",
            tags$br(),
            code("summary(fit_manova, test = \"Pillai\")"),
            " (Pillai suele ser más robusto en muestras moderadas),",
            tags$br(),
            code("summary(fit_manova, test = \"Wilks\")"),
            " para comparar con la literatura clásica."
          ),
          tags$li(
            "Interpretación del test global: ",
            "si el estadístico (por ejemplo, Pillai) es significativo, ",
            "hay evidencia de que ",
            strong("al menos una combinación de respuestas"),
            " difiere entre tratamientos."
          )
        )
      ),
      
      # Paso 3
      tags$li(
        strong("Paso 3 – Diagnósticos y supuestos"),
        tags$ul(
          tags$li(
            strong("Normalidad marginal:"),
            " revisar QQ-plots de cada respuesta por tratamiento.",
            tags$br(),
            "Aunque MANOVA asume normalidad multivariante, en la práctica ",
            "se suele evaluar la normalidad univariante como aproximación."
          ),
          tags$li(
            strong("Homogeneidad de matrices de covarianza:"),
            " para un diagnóstico simple, comparar las matrices de covarianza ",
            "empíricas en cada grupo (tratamiento).",
            tags$br(),
            "En cursos más avanzados se puede usar el test de Box (Box's M), ",
            "pero aquí basta con tener la idea de que no deberían ser ",
            "dramáticamente distintas."
          ),
          tags$li(
            strong("Independencia:"),
            " como en ANOVA, la asignación aleatoria y el diseño correcto ",
            "son la base para suponer independencia entre unidades."
          )
        )
      ),
      
      # Paso 4
      tags$li(
        strong("Paso 4 – Interpretar y reportar resultados"),
        tags$ul(
          tags$li(
            strong("1) Reporte global MANOVA:"),
            " comentar si el factor (por ejemplo, variedad) tiene ",
            "un efecto significativo sobre el ",
            strong("conjunto"),
            " de variables de respuesta."
          ),
          tags$li(
            strong("2) ANOVAs univariadas de seguimiento:"),
            " una vez detectado un efecto global, es habitual revisar ",
            "ANOVAs separadas para cada respuesta (Y1, Y2, Y3), ",
            "idealmente con algún control del error por comparaciones múltiples."
          ),
          tags$li(
            strong("3) Patrón de respuestas:"),
            " describir en lenguaje agronómico cómo se comportan las variables:",
            tags$ul(
              tags$li("¿En qué respuestas se observan las mayores diferencias?"),
              tags$li("¿Hay trade-offs? (por ejemplo, mayor rendimiento pero menor firmeza).")
            )
          ),
          tags$li(
            strong("Ejemplo de redacción:"),
            " “La MANOVA indicó un efecto significativo de la variedad sobre el conjunto ",
            "de respuestas rendimiento–°Brix–firmeza (Pillai p < …). ",
            "A nivel univariado, las diferencias entre variedades fueron marcadas en ",
            "rendimiento y firmeza, mientras que °Brix se mantuvo relativamente estable. ",
            "En conjunto, las variedades V1 y V3 mostraron un desempeño superior en calidad global.”"
          )
        )
      )
    )
  )
}

# Pestaña 6: Ejercicios & tareas (MANOVA)
pestanna6_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios & tareas",
    
    h4("Ejercicios prácticos para consolidar MANOVA"),
    p(
      "Estos ejercicios están pensados para que combines la simulación de la pestaña ",
      strong("4) Simulación MANOVA"),
      " con el protocolo de la pestaña ",
      strong("5) Protocolo datos reales"),
      " y practiques tanto la parte computacional como la interpretación agronómica."
    ),
    
    tags$ol(
      # Ejercicio 1
      tags$li(
        tagList(
          strong("Ejercicio 1 – Efecto fuerte en una sola respuesta"),
          tags$ul(
            tags$li("Ve a la pestaña ", strong("4) Simulación & correlación entre respuestas"), "."),
            tags$li("Simula un experimento con 2–3 tratamientos y al menos 20 observaciones por tratamiento."),
            tags$li(
              "Define las medias de forma que el tratamiento tenga un efecto ",
              strong("fuerte en Y1"),
              " (por ejemplo, rendimiento), pero efectos ",
              em("muy pequeños o nulos"),
              " en Y2 y Y3 (por ejemplo, calidad)."
            ),
            tags$li(
              "Ajusta por separado ANOVA univariados: ",
              code("Y1 ~ trat"), ", ", code("Y2 ~ trat"), ", ", code("Y3 ~ trat"),
              " y anota los valores de F y p-valor de cada uno."
            ),
            tags$li(
              "Luego ajusta una MANOVA: ",
              code("manova(cbind(Y1, Y2, Y3) ~ trat)"),
              " y compara el test global (Pillai / Wilks) con los ANOVA univariados."
            ),
            tags$li(
              "Discute: ¿el test multivariante detecta el efecto global cuando solo una respuesta es claramente afectada?"
            )
          )
        )
      ),
      
      # Ejercicio 2
      tags$li(
        tagList(
          strong("Ejercicio 2 – Efectos moderados y respuestas correlacionadas"),
          tags$ul(
            tags$li("En la pestaña ", strong("4) Simulación"), ", genera ahora 3 respuestas (Y1, Y2, Y3) con:"),
            tags$li(
              "Efectos de tratamiento ",
              strong("moderados y del mismo signo"),
              " en las tres respuestas (p.ej. todas aumentan algo con el mejor tratamiento)."
            ),
            tags$li(
              "Especifica una correlación ",
              strong("positiva y alta"),
              " entre las respuestas (por ejemplo, corr(Y1,Y2) ≈ 0.7, corr(Y1,Y3) ≈ 0.6)."
            ),
            tags$li(
              "Ajusta MANOVA: ",
              code("manova(cbind(Y1, Y2, Y3) ~ trat)"),
              " y guarda el test global (Pillai / Wilks)."
            ),
            tags$li(
              "Luego ajusta ANOVAs univariados para cada Y y observa qué p-valores obtienes.",
            ),
            tags$li(
              "Reflexiona: ¿cómo ayuda la correlación entre respuestas a detectar un efecto global de tratamiento?"
            )
          )
        )
      ),
      
      # Ejercicio 3
      tags$li(
        tagList(
          strong("Ejercicio 3 – ANOVA por separado vs MANOVA + corrección"),
          tags$ul(
            tags$li(
              "Usa los datos simulados del Ejercicio 2 (o vuelve a simular algo parecido en la pestaña ",
              strong("4)"),
              ")."
            ),
            tags$li(
              "Plantea dos estrategias de análisis:"
            ),
            tags$li(
              strong("Estrategia A:"),
              " hacer 3 ANOVA independientes (Y1, Y2, Y3) sin corregir por múltiples comparaciones."
            ),
            tags$li(
              strong("Estrategia B:"),
              " realizar primero una MANOVA global, y solo si es significativa, mirar ANOVAs univariados ",
              "aplicando una corrección sencilla (por ejemplo, Bonferroni sobre los 3 p-valores)."
            ),
            tags$li(
              "Discute: ¿cómo cambia el riesgo de falsos positivos cuando pasas de la Estrategia A a la B?"
            )
          )
        )
      ),
      
      # Ejercicio 4
      tags$li(
        tagList(
          strong("Ejercicio 4 – Diseño real o base de datos de calidad de fruta"),
          tags$ul(
            tags$li(
              "Si dispones de un dataset real (por ejemplo, ensayo de variedades con respuestas ",
              "de rendimiento, °Brix y firmeza), prepara los datos en formato ancho y aplica el ",
              strong("Protocolo de la pestaña 5"),
              "."
            ),
            tags$li(
              "Calcula la matriz de correlación entre las respuestas y revisa si tiene sentido un enfoque multivariante."
            ),
            tags$li(
              "Ajusta un modelo MANOVA y, si hay efecto global significativo, realiza los ANOVA univariados de seguimiento."
            ),
            tags$li(
              "Redacta un breve reporte (4–5 líneas) con una interpretación agronómica multivariada, ",
              "señalando en qué respuestas se concentran las diferencias entre tratamientos."
            )
          )
        )
      ),
      
      # Ejercicio 5
      tags$li(
        tagList(
          strong("Ejercicio 5 – Sensibilidad a la inclusión/exclusión de respuestas"),
          tags$ul(
            tags$li(
              "Parte de cualquier conjunto de datos (simulado o real) donde tengas al menos 3 respuestas.",
            ),
            tags$li(
              "Ajusta un modelo MANOVA con las 3 respuestas: ",
              code("manova(cbind(Y1, Y2, Y3) ~ trat)"),
              " y registra el estadístico Pillai/Wilks y su p-valor."
            ),
            tags$li(
              "Luego repite el análisis excluyendo una respuesta (por ejemplo, Y3): ",
              code("manova(cbind(Y1, Y2) ~ trat)"),
              " y compara los resultados."
            ),
            tags$li(
              "Discute: ¿la respuesta excluida aportaba señal útil (aumentaba la potencia) ",
              "o más bien introducía ruido? ¿Cómo cambia tu conclusión sobre el efecto global del tratamiento?"
            )
          )
        )
      )
    )
  )
}

# Pestaña Extra: Diagramas & Conceptos Visuales MANOVA
pestanna_extra_session10_v3UI <- function(ns) {
  
  # Definición de rutas (tal como solicitaste)
  base_path <- "images/sesiones/Diseños_estadisticos_V3/"
  img_path  <- paste0(base_path, "session10/")
  
  bslib::nav_panel(
    title = "Extra: Diagramas & Conceptos Visuales",
    icon = icon("project-diagram"), # Icono opcional para la pestaña
    
    div(
      class = "container-fluid",
      style = "padding-top: 15px;",
      
      h4(class = "section-header", "Galería Visual: Entendiendo la MANOVA"),
      p(class = "text-muted", 
        "Esta sección desglosa la complejidad matemática y conceptual de la MANOVA a través de diagramas explicativos. Navega por las pestañas para visualizar cada componente."),
      
      br(),
      
      bslib::navset_card_pill(
        
        # -----------------------------------------------------------------
        # 1. El Concepto Vectorial
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "1. El Enfoque Vectorial",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                # Imagen
                img(src = paste0(img_path, "manova_concept_vector.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                # Explicación
                div(class = "alert alert-light text-start",
                    h5("Univariado vs. Multivariado"),
                    p("En el enfoque clásico (izquierda), tratamos el rendimiento, los grados Brix y la firmeza como entes separados. Esto ignora la biología de la planta: un fruto más grande (rendimiento) suele tener menos azúcar (dilución)."),
                    p(strong("La visión MANOVA (derecha):"), " Vemos la planta como un sistema. El 'resultado' no es un número, es un vector. Analizamos cómo el tratamiento mueve todo el vector simultáneamente en el espacio.")
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 2. Geometría (Elipses)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "2. Geometría de Grupos",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "manova_geometry_ellipses.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("La Paradoja de la Separación"),
                    p("Observa cómo las curvas en los ejes (marginales) se solapan: si hicieras una ANOVA solo en X o solo en Y, quizás no encontrarías diferencias significativas."),
                    p(strong("El poder de la correlación:"), " Al considerar X e Y juntas, los grupos forman elipses separadas. MANOVA detecta esta separación diagonal que los análisis univariados pierden.")
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 3. La Pizarra Matemática
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "3. Modelo Matricial",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "manova_matrix_formula.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #333; padding: 2px; background-color: #222;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("El Modelo Lineal General Multivariante"),
                    withMathJax(), # Asegura renderizado de ecuaciones si se necesita
                    p("La ecuación fundamental es idéntica a la ANOVA, pero en mayúsculas (Matrices):"),
                    tags$ul(
                      tags$li(strong("Y (n x p):"), " Ya no es un vector columna, es una matriz con 'p' columnas (respuestas)."),
                      tags$li(strong("E (n x p):"), " Los errores ahora tienen una estructura de covarianza interna (Sigma) que conecta las columnas."),
                      tags$li("El objetivo es estimar B para minimizar el 'volumen' de E.")
                    )
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 4. Estadísticos de Prueba
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "4. Estadísticos (H vs E)",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "manova_he_matrices.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("¿Cómo medimos la significancia?"),
                    p("En ANOVA simple dividimos Varianzas (F = MS_trat / MS_error). En MANOVA, dividimos 'volúmenes' de matrices."),
                    tags$ul(
                      tags$li(strong("Wilks' Lambda:"), " Es el ratio de volumen de error sobre volumen total. Valores pequeños = Alta significancia (el error es pequeño comparado al efecto)."),
                      tags$li(strong("Pillai Trace:"), " Suma de la varianza explicada. Es el más robusto a violaciones de supuestos."),
                      tags$li(strong("Roy's Root:"), " Se enfoca solo en la dimensión (autovalor) más fuerte. Muy potente si el efecto se concentra en una sola dirección.")
                    )
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 5. Checklist de Supuestos
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "5. Supuestos Críticos",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "manova_assumptions_checklist.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("Validación del Modelo"),
                    p("Antes de confiar en los p-valores, debemos pasar esta lista de verificación:"),
                    tags$ol(
                      tags$li(strong("Normalidad Multivariante:"), " Más estricta que la univariada. Se revisa con test de Mardia o distancias de Mahalanobis (QQ-plot multivariante)."),
                      tags$li(strong("Homogeneidad de Matrices (Box's M):"), " La forma de las nubes de dispersión debe ser similar entre tratamientos, no solo su varianza."),
                      tags$li(strong("Multicolinealidad:"), " Si dos respuestas tienen correlación perfecta (r=1), MANOVA falla matemáticamente. Deben estar correlacionadas, pero no ser redundantes.")
                    )
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 6. Flujo de Trabajo
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "6. Flujo de Decisión",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "manova_decision_tree.png"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 500px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("Ruta de Análisis Sugerida"),
                    p("Este diagrama muestra el procedimiento estándar en investigación agronómica:"),
                    p("1. Primero demostramos que existe un efecto global (Protección del Error Tipo I)."),
                    p("2. Solo si el paso 1 es positivo, procedemos a 'disecar' el resultado con ANOVAs univariadas o Análisis Discriminante para entender ", em("qué"), " variables son responsables de la diferencia.")
                )
            )
          )
        )
      ) # Fin navset_card_pill
    )
  )
}

# Pestaña 7: Referencias (MANOVA)
pestanna7_session10_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias (MANOVA)",
    
    h4("Lecturas recomendadas para profundizar en MANOVA"),
    
    # ----------------------------------------------------
    # A) Textos de análisis multivariante (base teórica)
    # ----------------------------------------------------
    h5("A) Textos de análisis multivariante (base teórica)"),
    tags$ul(
      tags$li(
        strong("Rencher, A. C. (2002). "),
        em("Methods of Multivariate Analysis"),
        ". Wiley.",
        br(),
        "• Tratamiento muy completo de MANOVA, matrices H y E, y estadísticos de prueba ",
        "como Wilks, Pillai, Hotelling–Lawley y Roy."
      ),
      tags$li(
        strong("Johnson, R. A. & Wichern, D. W. (2007). "),
        em("Applied Multivariate Statistical Analysis"),
        ". Prentice Hall.",
        br(),
        "• Libro clásico de análisis multivariante aplicado; capítulos dedicados a MANOVA, ",
        "contrastes y pruebas globales."
      ),
      tags$li(
        strong("Tabachnick, B. G. & Fidell, L. S. (2013). "),
        em("Using Multivariate Statistics"),
        ". Pearson.",
        br(),
        "• Muy usado en ciencias sociales, con explicaciones detalladas y ejemplos de MANOVA ",
        "orientados a usuarios no estadísticos."
      )
    ),
    
    # ----------------------------------------------------
    # B) Aplicaciones en biología / agronomía
    # ----------------------------------------------------
    h5("B) Aplicaciones en biología y agronomía"),
    tags$ul(
      tags$li(
        strong("Quinn, G. P. & Keough, M. J. (2002). "),
        em("Experimental Design and Data Analysis for Biologists"),
        ". Cambridge University Press.",
        br(),
        "• Incluye capítulos de MANOVA aplicada a datos ecológicos y biológicos, ",
        "con diseños de campo similares a los de agronomía."
      ),
      tags$li(
        strong("Scheiner, S. M. & Gurevitch, J. (eds.) (2001). "),
        em("Design and Analysis of Ecological Experiments"),
        ". Oxford University Press.",
        br(),
        "• Discute usos de MANOVA en experimentos ecológicos (múltiples respuestas de una misma unidad), ",
        "con ideas transferibles a ensayos de variedades y calidad de fruta."
      ),
      tags$li(
        strong("Ejemplos de calidad de fruto / postcosecha"),
        br(),
        "• Muchos artículos de calidad de fruta (ej. arándanos, uva, manzana) usan MANOVA para evaluar ",
        "simultáneamente rendimiento, firmeza, color, °Brix, etc. ",
        "Son buenas fuentes para ver cómo se redactan resultados multivariantes."
      )
    ),
    
    # ----------------------------------------------------
    # C) Recursos en R para MANOVA
    # ----------------------------------------------------
    h5("C) Recursos en R (implementación práctica)"),
    tags$ul(
      tags$li(
        strong("Función base "),
        code("manova()"),
        " (paquete ",
        code("stats"),
        ").",
        br(),
        "• Permite ajustar modelos del tipo ",
        code("manova(cbind(Y1, Y2, Y3) ~ trat)"),
        ". Con ",
        code("summary()"),
        " se obtienen Wilks, Pillai, Hotelling–Lawley y Roy."
      ),
      tags$li(
        strong("Paquete "),
        code("car"),
        " (Companion to Applied Regression).",
        br(),
        "• Funciones como ",
        code("Anova()"),
        " para modelos lineales y MANOVA, con distintas parametrizaciones y tests robustos. ",
        "• Útil para contrastes y ANOVAs de seguimiento después de un MANOVA global."
      ),
      tags$li(
        strong("Paquete "),
        code("heplots"),
        " (Hypothesis–Error plots).",
        br(),
        "• Proporciona gráficos H–E y biplots para visualizar la separación entre grupos en MANOVA. ",
        "Muy didáctico para entender qué significa un efecto multivariante significativo."
      ),
      tags$li(
        strong("Paquete "),
        code("candisc"),
        " (Canonical Discriminant Analysis).",
        br(),
        "• Permite obtener y graficar variables canónicas asociadas a un MANOVA, ",
        "lo que ayuda a ver qué combinación lineal de respuestas separa mejor los tratamientos."
      )
    ),
    
    # ----------------------------------------------------
    # D) Notas y recursos didácticos
    # ----------------------------------------------------
    h5("D) Notas de cursos y recursos didácticos"),
    tags$ul(
      tags$li(
        "Apuntes de cursos de Análisis Multivariante o Diseño de Experimentos que incluyan MANOVA ",
        "(normalmente después de ANOVA y regresión lineal)."
      ),
      tags$li(
        "Vignettes y documentación de los paquetes ",
        code("car"),
        ", ",
        code("heplots"),
        " y ",
        code("candisc"),
        " para ver ejemplos en R con código reproducible."
      ),
      tags$li(
        "Material de universidades (presentaciones PDF, notas en línea) que expliquen la construcción ",
        "de las matrices H y E y la interpretación de Wilks, Pillai, etc."
      )
    ),
    
    div(
      class = "alert alert-secondary mt-3",
      p(
        strong("Sugerencia didáctica: "),
        "elige 1–2 capítulos “base” (por ejemplo, Rencher o Johnson & Wichern) como referencia formal, ",
        "y combina eso con ejemplos en R usando ",
        code("manova()"),
        " + ",
        code("car"),
        "/",
        code("heplots"),
        " para tus prácticas."
      )
    )
  )
}

# -------------------------------------------------------------------------
# Main UI Sesión 10
# -------------------------------------------------------------------------

session10_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 10: MANOVA")
    ),
    navset_tab(
      pestanna1_session10_v3UI(ns),
      pestanna2_session10_v3UI(ns),
      pestanna3_session10_v3UI(ns),
      pestanna4_session10_v3UI(ns),
      pestanna5_session10_v3UI(ns),
      pestanna6_session10_v3UI(ns),
      pestanna_extra_session10_v3UI(ns),
      pestanna7_session10_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server per Tab Sesión 10
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (MANOVA)
pestanna1_session10_v3_server <- function(input, output, session) {
  # Sin lógica reactiva: esta pestaña es puramente conceptual / introductoria
}

# Pestaña 2: Conceptos clave MANOVA
pestanna2_session10_v3_server <- function(input, output, session) {
  # Sin lógica: esta pestaña es puramente conceptual / teórica
}

# Pestaña 3: Diseño & estructura de datos MANOVA
pestanna3_session10_v3_server <- function(input, output, session) {
  ns <- session$ns
  
  # ------------------------------------------------------------------
  # 1) Reactive: obtener el data.frame (simulado o cargado por el usuario)
  # ------------------------------------------------------------------
  manova_data <- reactive({
    src <- input$manova_example_source
    
    if (is.null(src) || src == "sim") {
      # Dataset simulado tipo "arándanos": bloque, variedad, 3 respuestas
      set.seed(123)
      n_block <- 4
      n_trat  <- 5
      bloques <- paste0("B", 1:n_block)
      trats   <- paste0("Var", 1:n_trat)
      
      df <- expand.grid(
        block = bloques,
        trat  = trats
      )
      
      # Efectos de tratamiento para las tres respuestas
      eff_trat_rend <- rnorm(n_trat, mean = 0, sd = 800)
      names(eff_trat_rend) <- trats
      
      eff_trat_brix <- rnorm(n_trat, mean = 0, sd = 0.6)
      names(eff_trat_brix) <- trats
      
      eff_trat_firm <- rnorm(n_trat, mean = 0, sd = 20)
      names(eff_trat_firm) <- trats
      
      # Valores base y correlaciones "plausibles"
      mu_rend <- 10000
      mu_brix <- 12.0
      mu_firm <- 280
      
      # Generamos las respuestas con algo de ruido
      df$Y1_rend    <- mu_rend + eff_trat_rend[df$trat] + rnorm(nrow(df), 0, 600)
      df$Y2_brix    <- mu_brix + eff_trat_brix[df$trat] + rnorm(nrow(df), 0, 0.4)
      df$Y3_firmeza <- mu_firm + eff_trat_firm[df$trat] + rnorm(nrow(df), 0, 15)
      
      return(df)
    }
    
    # Caso: archivo cargado por el usuario
    inFile <- input$manova_file
    validate(
      need(!is.null(inFile), "Sube un archivo CSV para continuar.")
    )
    
    df <- tryCatch(
      {
        read.csv(inFile$datapath, header = TRUE, sep = ",", dec = ".", stringsAsFactors = FALSE)
      },
      error = function(e) {
        showNotification(
          paste("Error leyendo el CSV:", e$message),
          type = "error"
        )
        return(NULL)
      }
    )
    
    validate(
      need(!is.null(df), "No se pudo leer el CSV. Verifica formato y separadores.")
    )
    
    df
  })
  
  # ------------------------------------------------------------------
  # 2) Vista previa de datos
  # ------------------------------------------------------------------
  output$manova_example_data <- DT::renderDataTable({
    df <- manova_data(); shiny::req(df)
    DT::datatable(
      head(df, 20),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ------------------------------------------------------------------
  # 3) Resumen de estructura multivariante
  # ------------------------------------------------------------------
  output$manova_example_summary <- renderPrint({
    df <- manova_data(); shiny::req(df)
    
    # Detectar variables numéricas (candidatas a respuestas)
    is_num <- vapply(df, is.numeric, logical(1))
    num_names <- names(df)[is_num]
    
    # Detectar una posible columna de tratamiento
    # Buscamos nombres típicos: "trat", "tratamiento", "group", "genotipo", etc.
    trat_candidates <- grep("trat|group|genot|varie", names(df), ignore.case = TRUE, value = TRUE)
    if (length(trat_candidates) > 0) {
      trat_col <- trat_candidates[1]
    } else {
      # Si no encontramos, buscamos la primera columna no numérica
      is_fac_like <- !is_num
      if (any(is_fac_like)) {
        trat_col <- names(df)[which(is_fac_like)[1]]
      } else {
        trat_col <- NULL
      }
    }
    
    cat("=== Resumen de la estructura de datos ===\n\n")
    cat("Nº observaciones:", nrow(df), "\n")
    
    if (!is.null(trat_col)) {
      cat("Columna de tratamiento detectada:", trat_col, "\n")
      cat("Niveles de tratamiento:", length(unique(df[[trat_col]])), "\n")
    } else {
      cat("No se detectó claramente una columna de tratamiento (factor).\n")
    }
    
    cat("\nVariables numéricas (posibles respuestas multivariantes):\n")
    if (length(num_names) >= 2) {
      cat("  -", paste(num_names, collapse = ", "), "\n")
    } else if (length(num_names) == 1) {
      cat("  Solo se detectó 1 variable numérica:", num_names, "\n")
      cat("  Para MANOVA se requieren al menos 2 respuestas continuas.\n")
    } else {
      cat("  No se detectaron variables numéricas.\n")
    }
  })
  
  # ------------------------------------------------------------------
  # 4) Matriz de correlación entre respuestas
  # ------------------------------------------------------------------
  output$manova_example_cor <- renderPrint({
    df <- manova_data(); shiny::req(df)
    is_num <- vapply(df, is.numeric, logical(1))
    num_names <- names(df)[is_num]
    
    shiny::validate(
      shiny::need(length(num_names) >= 2,
           "Se necesitan al menos dos variables numéricas para calcular correlaciones.")
    )
    
    Y <- df[, num_names, drop = FALSE]
    cat("Matriz de correlación (entre variables numéricas):\n\n")
    print(round(stats::cor(Y, use = "pairwise.complete.obs"), 3))
  })
  
  # ------------------------------------------------------------------
  # 5) Gráfico Y1 vs Y2 por tratamiento
  # ------------------------------------------------------------------
  output$manova_example_plot <- renderPlot({
    df <- manova_data(); shiny::req(df)
    
    is_num <- vapply(df, is.numeric, logical(1))
    num_names <- names(df)[is_num]
    
    shiny::validate(
      shiny::need(length(num_names) >= 2,
           "Se necesitan al menos dos variables numéricas para el gráfico Y1 vs Y2.")
    )
    
    y1 <- num_names[1]
    y2 <- num_names[2]
    
    # Buscar columna de tratamiento
    trat_candidates <- grep("trat|group|genot|varie", names(df), ignore.case = TRUE, value = TRUE)
    if (length(trat_candidates) > 0) {
      trat_col <- trat_candidates[1]
      df[[trat_col]] <- as.factor(df[[trat_col]])
    } else {
      # Si no hay factor claro, creamos uno ficticio
      trat_col <- NULL
    }
    
    library(ggplot2)
    
    if (!is.null(trat_col)) {
      ggplot(df, aes(x = .data[[y1]], y = .data[[y2]], color = .data[[trat_col]])) +
        geom_point(size = 2, alpha = 0.8) +
        labs(
          x = y1,
          y = y2,
          color = "Tratamiento",
          title = paste("Dispersión", y1, "vs", y2, "por tratamiento")
        ) +
        theme_minimal(base_size = 12)
    } else {
      ggplot(df, aes_string(x = y1, y = y2)) +
        geom_point(size = 2, alpha = 0.8) +
        labs(
          x = y1,
          y = y2,
          title = paste("Dispersión", y1, "vs", y2)
        ) +
        theme_minimal(base_size = 12)
    }
  })
}

# Pestaña 4: Simulación & análisis MANOVA
pestanna4_session10_v3_server <- function(input, output, session) {
  
  # -----------------------------------------------------------------------
  # 1) Simulación de datos multivariantes para MANOVA
  # -----------------------------------------------------------------------
  sim_manova <- eventReactive(input$btn_sim_manova, {
    
    # Verificar que MASS esté disponible (para mvrnorm)
    if (!requireNamespace("MASS", quietly = TRUE)) {
      shiny::showNotification(
        "Se requiere el paquete 'MASS' para simular normales multivariantes. Instálalo e inténtalo de nuevo.",
        type = "error"
      )
      return(NULL)
    }
    
    set.seed(123)  # Semilla fija para reproducibilidad didáctica
    
    G     <- input$manova_n_trat    # número de tratamientos
    n_rep <- input$manova_n_rep     # observaciones por tratamiento
    rho   <- input$manova_rho       # correlación entre respuestas
    delta <- input$manova_delta     # “tamaño” del efecto de tratamiento
    
    # -------------------- Medias base por respuesta ----------------------
    # Interpretación agronómica aproximada:
    #  Y1 = rendimiento (kg/ha),
    #  Y2 = °Brix,
    #  Y3 = firmeza (N o g/mm)
    mu_base <- c(
      Y1_rend = 10000,  # rendimiento base
      Y2_brix = 12,     # ºBrix base
      Y3_firm = 280     # firmeza base
    )
    
    # Queremos que los tratamientos difieran en las medias.
    # Usamos una secuencia centrada (por ejemplo, -1, 0, 1 para 3 tratamientos),
    # escalada por "delta" y una escala distinta por respuesta (en unidades).
    offset_scale <- c(
      Y1_rend = 800,  # ≈ 0.8 ton/ha de diferencia base
      Y2_brix = 0.5,
      Y3_firm = 15
    )
    
    idx        <- seq_len(G)
    center_seq <- idx - mean(idx)  # vector centrado (e.g. -1,0,1)
    
    mu_list <- lapply(seq_len(G), function(g) {
      mu_base + center_seq[g] * delta * offset_scale
    })
    
    # -------------------- Matriz de covarianza Σ -------------------------
    # Desvíos estándar por respuesta (ruido intra-tratamiento)
    sd_vec <- c(
      Y1_rend = 1000,  # variación residual en rendimiento
      Y2_brix = 1,     # variación residual en ºBrix
      Y3_firm = 20     # variación residual en firmeza
    )
    
    # Matriz de correlaciones (simplificada: misma ρ entre todas las parejas)
    Rmat <- matrix(rho, nrow = 3, ncol = 3)
    diag(Rmat) <- 1
    # Σ = D * R * D, donde D = diag(sd)
    Sigma <- diag(sd_vec) %*% Rmat %*% diag(sd_vec)
    
    # -------------------- Generar datos por tratamiento ------------------
    df_list <- lapply(seq_len(G), function(g) {
      Y <- MASS::mvrnorm(
        n   = n_rep,
        mu  = mu_list[[g]],
        Sigma = Sigma
      )
      data.frame(
        trat    = factor(paste0("T", g)),
        Y1_rend = Y[, 1],
        Y2_brix = Y[, 2],
        Y3_firm = Y[, 3]
      )
    })
    
    datos <- do.call(rbind, df_list)
    rownames(datos) <- NULL
    
    list(
      datos   = datos,
      mu_base = mu_base,
      mu_list = mu_list,
      Sigma   = Sigma
    )
  })
  
  # -----------------------------------------------------------------------
  # 2) Resultados numéricos: ANOVA univariada vs MANOVA
  # -----------------------------------------------------------------------
  output$manova_sim_results <- renderPrint({
    sim <- sim_manova()
    shiny::req(sim)
    
    datos <- sim$datos
    
    # ANOVA univariada para cada respuesta
    aov_y1 <- stats::aov(Y1_rend ~ trat, data = datos)
    aov_y2 <- stats::aov(Y2_brix ~ trat, data = datos)
    aov_y3 <- stats::aov(Y3_firm ~ trat, data = datos)
    
    cat("========================================================\n")
    cat("ANOVA univariados por respuesta (una Y a la vez)\n")
    cat("========================================================\n\n")
    
    cat("Respuesta Y1 = rendimiento (kg/ha)\n")
    print(summary(aov_y1))
    cat("\n--------------------------------------------------------\n\n")
    
    cat("Respuesta Y2 = ºBrix\n")
    print(summary(aov_y2))
    cat("\n--------------------------------------------------------\n\n")
    
    cat("Respuesta Y3 = firmeza\n")
    print(summary(aov_y3))
    cat("\n========================================================\n\n")
    
    # MANOVA global
    fit_manova <- stats::manova(
      cbind(Y1_rend, Y2_brix, Y3_firm) ~ trat,
      data = datos
    )
    
    cat("MANOVA global: todas las respuestas juntas\n")
    cat("========================================================\n\n")
    
    cat("Prueba global usando traza de Pillai:\n")
    print(summary(fit_manova, test = "Pillai"))
    
    cat("\n--------------------------------------------------------\n\n")
    
    cat("Prueba global usando Wilks' Lambda:\n")
    print(summary(fit_manova, test = "Wilks"))
    
    cat("\nComentario didáctico:\n")
    cat("- Si la MANOVA es significativa, indica que el tratamiento afecta, ",
        "en conjunto, al vector (Y1, Y2, Y3).\n",
        "- Luego se revisan las ANOVA univariadas para ver qué respuestas ",
        "contribuyen más a esa diferencia.\n")
  })
  
  # -----------------------------------------------------------------------
  # 3) Visualización: heatmap de medias por tratamiento × respuesta
  # -----------------------------------------------------------------------
  output$manova_heatmap <- renderPlot({
    sim <- sim_manova()
    shiny::req(sim)
    datos <- sim$datos
    
    # Medias por tratamiento y respuesta
    means_long <- datos |>
      dplyr::group_by(trat) |>
      dplyr::summarise(
        Y1_rend = mean(Y1_rend),
        Y2_brix = mean(Y2_brix),
        Y3_firm = mean(Y3_firm),
        .groups = "drop"
      ) |>
      tidyr::pivot_longer(
        cols      = dplyr::starts_with("Y"),
        names_to  = "respuesta",
        values_to = "media"
      )
    
    ggplot2::ggplot(
      means_long,
      ggplot2::aes(x = respuesta, y = trat, fill = media)
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.1f", media)),
        size = 3
      ) +
      ggplot2::scale_fill_viridis_c(name = "Media") +
      ggplot2::labs(
        x = "Respuesta",
        y = "Tratamiento",
        title = "Medias simuladas por tratamiento y respuesta (heatmap)"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  # -----------------------------------------------------------------------
  # 4) Visualización: dispersión Y1 vs Y2 por tratamiento
  # -----------------------------------------------------------------------
  output$manova_scatter <- renderPlot({
    sim <- sim_manova()
    shiny::req(sim)
    datos <- sim$datos
    
    ggplot2::ggplot(
      datos,
      ggplot2::aes(x = Y1_rend, y = Y2_brix, color = trat)
    ) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::labs(
        x = "Y1: Rendimiento (kg/ha)",
        y = "Y2: ºBrix",
        color = "Tratamiento",
        title = "Dispersión Y1 vs Y2 por tratamiento"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
}

# Pestaña 5: Protocolo datos reales MANOVA
pestanna5_session10_v3_server <- function(input, output, session) {
  # Solo texto estático; no se requiere lógica de servidor en esta pestaña.
}

# Pestaña 6: Ejercicios & tareas (MANOVA)
pestanna6_session10_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor: la pestaña es puramente textual/guía.
}

# Pestaña 7: Referencias (MANOVA)
pestanna7_session10_v3_server <- function(input, output, session) {
  # Pestaña solo informativa: no se requiere lógica de servidor
}

# -------------------------------------------------------------------------
# Main Server Sesión 10
# -------------------------------------------------------------------------

session10_v3Server <- function(input, output, session) {
  # has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
  
  pestanna1_session10_v3_server(input, output, session)
  pestanna2_session10_v3_server(input, output, session)
  pestanna3_session10_v3_server(input, output, session)
  pestanna4_session10_v3_server(input, output, session)
  pestanna5_session10_v3_server(input, output, session)
  pestanna6_session10_v3_server(input, output, session)
  pestanna7_session10_v3_server(input, output, session)
}
