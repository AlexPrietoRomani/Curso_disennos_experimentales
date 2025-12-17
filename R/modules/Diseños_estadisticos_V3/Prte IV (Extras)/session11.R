# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session11.R
# Sesión 11: MANCOVA

# -------------------------------------------------------------------------
# UI per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (MANCOVA)
pestanna1_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto (MANCOVA)",
    
    # Título general
    h4(class = "section-header", "¿Dónde encaja la MANCOVA en la historia del curso?"),
    
    # Bloque introductorio
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "la MANCOVA extiende la MANOVA a situaciones donde, además de varios ",
        em("resultados cuantitativos medidos en la misma unidad"),
        ", tenemos una o más ",
        strong("covariables continuas"),
        " que queremos usar para ajustar y ganar precisión (igual que en ANCOVA, pero ahora en forma multivariada)."
      )
    ),
    
    # Conexión con ANOVA / ANCOVA / MANOVA
    fluidRow(
      column(
        width = 6,
        h5("De ANOVA a MANCOVA: un mismo hilo conductor"),
        tags$ul(
          tags$li(
            strong("ANOVA: "),
            em("1 respuesta (Y) + factores categóricos."),
            br(),
            code("Y ~ Trat"), " o ", code("Y ~ Bloque + Trat"), " (RCBD)."
          ),
          tags$li(
            strong("ANCOVA: "),
            em("1 respuesta (Y) + factores + covariable(s) continua(s)."),
            br(),
            code("Y ~ Trat + X"), ", o con bloque: ", code("Y ~ Bloque + Trat + X")
          ),
          tags$li(
            strong("MANOVA: "),
            em("varias respuestas (Y1, Y2, …) + factores."),
            br(),
            code("cbind(Y1, Y2, Y3) ~ Trat")
          ),
          tags$li(
            strong("MANCOVA: "),
            em("varias respuestas + factores + covariable(s)."),
            br(),
            code("cbind(Y1, Y2, Y3) ~ Trat + X"),
            " (y eventualmente bloque u otros factores)."
          )
        )
      ),
      column(
        width = 6,
        h5("Ejemplo agronómico ancla para toda la sesión"),
        p(
          "Imagina un ensayo de variedades de arándanos donde, en cada parcela, mides:"
        ),
        tags$ul(
          tags$li(strong("Y1:"), " rendimiento (kg/ha)."),
          tags$li(strong("Y2:"), " °Brix (sólidos solubles)."),
          tags$li(strong("Y3:"), " firmeza de fruto.")
        ),
        p("Además cuentas con una covariable continua:"),
        tags$ul(
          tags$li(
            strong("X (covariable):"),
            " vigor inicial o biomasa antes de iniciar la temporada, ",
            "o bien una medida de sombreamiento / altitud / índice de vigor (NDVI) previo."
          )
        ),
        p("Y tienes uno o más factores de diseño:"),
        tags$ul(
          tags$li(strong("Variedad:"), " factor principal de interés."),
          tags$li(strong("Bloque / sector del campo:"), " factor de bloqueo para controlar heterogeneidad espacial."),
          tags$li("Opcionalmente, un segundo factor como nivel de riego o fertilización.")
        ),
        div(
          class = "note-cloud",
          p(
            "Pregunta guía: ¿el efecto de la variedad sobre ",
            em("el perfil de calidad completo"),
            " (rendimiento, °Brix y firmeza) sigue siendo significativo ",
            "cuando ajustamos por el vigor inicial X?"
          )
        )
      )
    ),
    
    # Resultados esperados (learning outcomes)
    h5("Resultados esperados de la sesión (MANCOVA)"),
    tags$ol(
      tags$li(
        "Ser capaz de explicar en lenguaje sencillo qué es una MANCOVA: ",
        em("una MANOVA con covariables continuas que ajustan el análisis.")
      ),
      tags$li(
        "Reconocer cuándo es preferible una MANCOVA frente a:",
        tags$ul(
          tags$li("varias ANCOVA independientes (una por respuesta)."),
          tags$li("una MANOVA sin covariables.")
        )
      ),
      tags$li(
        "Identificar los elementos básicos en un dataset real para MANCOVA: ",
        code("trat"), ", covariable(s) ", code("X"), 
        " y varias respuestas ", code("Y1, Y2, ..."), 
        " medidas en la misma unidad experimental."
      ),
      tags$li(
        "Entender que los supuestos combinan los de ANCOVA (linealidad, homogeneidad de pendientes) ",
        "y MANOVA (normalidad multivariante, homogeneidad de matrices de covarianza) ",
        "y que estos se revisarán en las pestañas posteriores."
      )
    )
  )
}

# Pestaña 2: Conceptos clave MANCOVA
pestanna2_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave & modelo MANCOVA",
    
    h4(class = "section-header", "Modelo MANCOVA básico (vista conceptual)"),
    
    # Bloque principal con el modelo en notación matricial
    withMathJax(
      div(
        class = "alert alert-info",
        p(
          strong("Idea central: "),
          "la MANCOVA extiende la ANCOVA (una respuesta + covariable) ",
          "y la MANOVA (múltiples respuestas) a un modelo que combina ",
          "múltiples respuestas continuas y una o más covariables."
        ),
        p("Un modelo simple con una covariable X y un factor de grupos g(i) puede escribirse como:"),
        helpText(
          "$$\\mathbf{Y}_i = \\boldsymbol{\\mu} + \\boldsymbol{\\tau}_{g(i)} + \\mathbf{B}\\,(X_i - \\bar X) + \\boldsymbol{\\varepsilon}_i$$"
        ),
        p(
          "donde:"
        ),
        tags$ul(
          tags$li(em("\\(\\mathbf{Y}_i\\)"),
                  " es el vector de respuestas para la unidad i ",
                  "(por ejemplo: rendimiento, °Brix, firmeza)."),
          tags$li(em("\\(\\boldsymbol{\\mu}\\)"),
                  " es el vector de medias globales."),
          tags$li(em("\\(\\boldsymbol{\\tau}_{g(i)}\\)"),
                  " es el efecto del grupo/tratamiento al que pertenece la unidad i."),
          tags$li(em("\\(\\mathbf{B}\\)"),
                  " es el vector (o matriz) de pendientes que describe el efecto de la covariable sobre cada respuesta."),
          tags$li(em("\\(\\boldsymbol{\\varepsilon}_i\\)"),
                  " es el vector de errores, que se asume con distribución normal multivariante ",
                  "de media cero y matriz de covarianza ", em("\\(\\boldsymbol{\\Sigma}\\)"), ".")
        )
      )
    ),
    
    fluidRow(
      # Columna 1: componentes del modelo
      column(
        width = 6,
        h5("Elementos de un modelo MANCOVA"),
        tags$ul(
          tags$li(
            strong("Respuestas múltiples (Y): "),
            "2 o más variables cuantitativas medidas en la misma unidad experimental ",
            "(ej. rendimiento, tamaño de fruto, firmeza, color)."
          ),
          tags$li(
            strong("Factores de diseño (tratamientos): "),
            "uno o varios factores categóricos (variedad, riego, manejo, dosis de fertilizante, etc.)."
          ),
          tags$li(
            strong("Covariable(s) continua(s) (X): "),
            "mediciones numéricas que se incluyen para ajustar por diferencias iniciales o gradientes ",
            "(por ejemplo, vigor inicial, biomasa, altitud, índice de vigor de planta, etc.)."
          ),
          tags$li(
            strong("Efectos de grupo: "),
            "las diferencias sistemáticas entre tratamientos se representan en el vector ",
            "de efectos ", em("\\(\\boldsymbol{\\tau}_{g(i)}\\)"), "."
          ),
          tags$li(
            strong("Matriz de covarianza residual (Σ): "),
            "captura simultáneamente la variabilidad residual y la correlación entre las respuestas ",
            "después de ajustar por tratamientos y covariables."
          )
        )
      ),
      
      # Columna 2: supuestos
      column(
        width = 6,
        h5("Supuestos clave en MANCOVA"),
        tags$ul(
          tags$li(
            strong("Normalidad multivariante: "),
            "el vector de respuestas residuales se aproxima a una distribución normal multivariante ",
            "en cada combinación de tratamientos."
          ),
          tags$li(
            strong("Homogeneidad de covarianzas: "),
            "la matriz de covarianza residual es similar entre grupos/tratamientos ",
            "(análogo multivariante a la homogeneidad de varianzas en ANOVA)."
          ),
          tags$li(
            strong("Relación lineal: "),
            "para cada respuesta, la relación con la(s) covariable(s) es aproximadamente lineal."
          ),
          tags$li(
            strong("Homogeneidad de pendientes: "),
            "el efecto de la covariable sobre las respuestas es parecido en todos los tratamientos ",
            "cuando se usa un modelo con pendiente común (sin interacción Trat×X)."
          ),
          tags$li(
            strong("Independencia: "),
            "las observaciones (parcelas/unidades experimentales) son independientes entre sí."
          )
        )
      )
    ),
    
    # Bloque comparativo con ANCOVA univariante
    withMathJax(
      div(
        class = "alert alert-secondary mt-3",
        h5("Analogía con la ANCOVA univariante"),
        p(
          "En ANCOVA clásica, para una sola respuesta Y, el modelo base es:"
        ),
        helpText(
          "$$Y_{ij} = \\mu + \\tau_i + \\beta (X_{ij} - \\bar X) + \\varepsilon_{ij}$$"
        ),
        p(
          "En MANCOVA aplicamos la misma lógica a un ",
          strong("vector de respuestas"), " en lugar de una sola Y. ",
          "La covariable ayuda a reducir el ruido en todas las respuestas simultáneamente, ",
          "y la matriz de covarianza permite aprovechar la información de la correlación entre ellas."
        )
      )
    )
  )
}

# Pestaña 3: Diseño & estructura de datos (MANCOVA)
pestanna3_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & estructura de datos",
    
    h4(class = "section-header",
       "¿Cómo debe lucir un 'fieldbook' para MANCOVA?"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea clave: "),
        "una MANCOVA es, en la práctica, una MANOVA (múltiples respuestas) ",
        "con una o más covariables al estilo ANCOVA. ",
        "Eso se traduce en un data frame ancho con columnas de: bloque, tratamiento, ",
        "covariable(s) y varias respuestas continuas."
      )
    ),
    
    p(
      "La estructura típica de datos es algo como:"
    ),
    tags$table(
      class = "table table-sm",
      tags$thead(
        tags$tr(
          tags$th("block"),
          tags$th("trat"),
          tags$th("X_cov"),
          tags$th("Y1_rend"),
          tags$th("Y2_brix"),
          tags$th("Y3_firmeza")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("1"),
          tags$td("Var1"),
          tags$td("23.4"),
          tags$td("9500"),
          tags$td("12.4"),
          tags$td("280")
        ),
        tags$tr(
          tags$td("1"),
          tags$td("Var2"),
          tags$td("21.1"),
          tags$td("9100"),
          tags$td("11.8"),
          tags$td("300")
        )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Fuente de datos para ilustrar la estructura"),
        radioButtons(
          ns("mancova_example_source"),
          "Selecciona los datos de ejemplo:",
          choices = c(
            "Dataset simulado (interno)" = "sim",
            "Cargar un CSV propio"       = "upload"
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'upload'", ns("mancova_example_source")),
          fileInput(
            ns("mancova_file"),
            "Cargar archivo CSV:",
            accept = ".csv"
          ),
          helpText(
            "Sugerencia mínima de columnas para MANCOVA:",
            tags$ul(
              tags$li(code("block"), " (opcional) – bloque o sector del campo."),
              tags$li(code("trat"), " – tratamiento / variedad / grupo."),
              tags$li(code("X_cov"), " – covariable numérica (p.ej. vigor inicial)."),
              tags$li(code("Y1, Y2, ..."), " – respuestas continuas (rendimiento, Brix, firmeza, etc.).")
            )
          )
        ),
        hr(),
        helpText(
          "Si no cargas un archivo, se generará un ejemplo simulado con columnas: ",
          code("block, trat, X_cov, Y1_rend, Y2_brix, Y3_firmeza"), "."
        )
      ),
      
      mainPanel(
        width = 8,
        h5("Vista previa del 'fieldbook' multivariado"),
        DT::dataTableOutput(ns("mancova_example_data")),
        hr(),
        
        h5("¿Qué revisar en la estructura antes de hacer MANCOVA?"),
        tags$ul(
          tags$li("Que todas las respuestas (Y1, Y2, ...) estén en columnas numéricas separadas."),
          tags$li("Que la(s) covariable(s) estén en columna(s) propia(s) (p.ej. ", code("X_cov"), ")."),
          tags$li("Que la columna de tratamiento (", code("trat"), ") identifique grupos bien definidos."),
          tags$li("Si hay bloque (", code("block"), "), que tenga sentido como factor de diseño común a todas las respuestas.")
        ),
        hr(),
        
        h5("Visualización básica de la estructura (dos respuestas)"),
        plotOutput(ns("mancova_example_plot"), height = "320px"),
        helpText(
          "La idea es ver, de forma muy simple, cómo se relacionan dos respuestas numéricas ",
          "y cómo se agrupan según el tratamiento."
        ),
        hr(),
        
        h5("Recordatorio estadístico (solo texto)"),
        verbatimTextOutput(ns("mancova_example_model"))
      )
    )
  )
}

# Pestaña 4: Simulación & análisis MANCOVA
pestanna4_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & análisis (MANCOVA)",
    
    p(
      "En esta pestaña simulamos un experimento con varios tratamientos, ",
      "una covariable continua X (por ejemplo vigor inicial) y ",
      "tres respuestas continuas (por ejemplo: rendimiento, °Brix y firmeza). ",
      "Luego comparamos MANOVA sin covariable, MANCOVA aditiva (Trat + X) ",
      "y un modelo con interacción Trat×X."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Parámetros de simulación"),
        
        numericInput(
          ns("mancova_g"),
          "N° de tratamientos:",
          value = 3, min = 2, max = 6, step = 1
        ),
        numericInput(
          ns("mancova_n"),
          "N° observaciones por tratamiento:",
          value = 30, min = 6, step = 2
        ),
        tags$hr(),
        h5("Covariable y estructura de error"),
        numericInput(
          ns("mancova_sigma"),
          "Desvío estándar residual (σ):",
          value = 8, min = 0.1, step = 0.5
        ),
        numericInput(
          ns("mancova_rho"),
          "Correlación base entre respuestas (ρ):",
          value = 0.5, min = -0.9, max = 0.9, step = 0.1
        ),
        helpText(
          "ρ controla la correlación entre Y1, Y2 y Y3. ",
          "Valores positivos simulan respuestas 'alineadas' (ej. mejor calidad en todas)."
        ),
        tags$hr(),
        h5("Pendientes verdaderas de la covariable (β)"),
        numericInput(
          ns("mancova_beta1"),
          HTML("&beta;<sub>1</sub> para Y1 (ej. rendimiento):"),
          value = 2.0, step = 0.1
        ),
        numericInput(
          ns("mancova_beta2"),
          HTML("&beta;<sub>2</sub> para Y2 (ej. &deg;Brix):"),
          value = 0.8, step = 0.1
        ),
        numericInput(
          ns("mancova_beta3"),
          HTML("&beta;<sub>3</sub> para Y3 (ej. firmeza):"),
          value = 1.2, step = 0.1
        ),
        helpText(
          "Estas pendientes son las relaciones verdaderas entre la covariable X ",
          "y cada respuesta. La MANCOVA intentará recuperarlas."
        ),
        tags$hr(),
        actionButton(
          ns("btn_sim_mancova"),
          "Simular y ajustar modelos",
          class = "btn btn-success w-100 mt-2"
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # 4.1 – Datos simulados
          bslib::nav_panel(
            title = "4.1 Datos simulados",
            h5("Vista rápida del dataset simulado"),
            DT::dataTableOutput(ns("mancova_sim_data")),
            hr(),
            h5("Correlaciones empíricas"),
            verbatimTextOutput(ns("mancova_sim_cor"))
          ),
          
          # 4.2 – MANOVA vs MANCOVA
          bslib::nav_panel(
            title = "4.2 MANOVA vs MANCOVA",
            h5("Comparación de modelos globales"),
            div(
              class = "alert alert-secondary",
              p(
                "Se comparan tres modelos con respuestas (Y1, Y2, Y3):"
              ),
              tags$ul(
                tags$li(strong("Modelo 1 (MANOVA): "), code("cbind(Y1,Y2,Y3) ~ trat")),
                tags$li(strong("Modelo 2 (MANCOVA aditiva): "), code("cbind(Y1,Y2,Y3) ~ trat + X")),
                tags$li(strong("Modelo 3 (MANCOVA con interacción): "), code("cbind(Y1,Y2,Y3) ~ trat * X"))
              ),
              p(
                "El foco está en cómo cambia el test global (Pillai) ",
                "y la magnitud del error al incluir la covariable."
              )
            ),
            verbatimTextOutput(ns("mancova_sim_results"))
          ),
          
          # 4.3 – Gráfico Y1 ~ X por tratamiento
          bslib::nav_panel(
            title = "4.3 Gráfico Y1 ~ X",
            h5("Relación entre Y1 y la covariable X"),
            plotOutput(ns("mancova_sim_plot"), height = "360px"),
            helpText(
              "Las rectas ajustadas muestran cómo se relaciona Y1 con X en cada tratamiento. ",
              "Si el modelo aditivo es correcto, las pendientes deberían ser aproximadamente paralelas."
            )
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo datos reales MANCOVA
pestanna5_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo datos reales (MANCOVA)",
    
    h4("Checklist para aplicar MANCOVA a tus datos reales"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "la MANCOVA permite evaluar el efecto de uno o más tratamientos sobre ",
        em("un conjunto de respuestas cuantitativas correlacionadas"),
        " (por ejemplo, rendimiento, °Brix y firmeza), ",
        "ajustando al mismo tiempo por una o más covariables continuas (por ejemplo, vigor inicial)."
      )
    ),
    
    # ------------------------------------------------------------------
    # Paso 0 – ¿Realmente corresponde usar MANCOVA?
    # ------------------------------------------------------------------
    h5("Paso 0 – ¿Realmente corresponde usar MANCOVA?"),
    tags$ul(
      tags$li(
        strong("Respuestas múltiples (Y1, Y2, …): "),
        "son variables ",
        em("cuantitativas continuas"),
        " medidas sobre la misma unidad experimental (la misma parcela, planta o bloque)."
      ),
      tags$li(
        strong("Interés conjunto: "),
        "te interesa el patrón global de calidad / desempeño (por ejemplo, ",
        "rendimiento + calidad de fruta), no solo cada variable por separado."
      ),
      tags$li(
        strong("Covariable(s) X: "),
        "existen una o más covariables continuas (vigor, altitud, sombreamiento, etc.) ",
        "que podrían explicar parte de la variación de las respuestas y que ",
        em("no fueron manipuladas como tratamiento"),
        " (es decir, no son un factor experimental)."
      ),
      tags$li(
        strong("Diseño original: "),
        "el diseño base suele ser un CRD, RCBD o similar — ",
        "la MANCOVA actúa sobre las respuestas, no cambia el diseño subyacente."
      )
    ),
    
    tags$hr(),
    
    # ------------------------------------------------------------------
    # Paso 1 – Preparar y revisar la base de datos
    # ------------------------------------------------------------------
    h5("Paso 1 – Preparar y revisar la base de datos"),
    tags$ul(
      tags$li(
        strong("Formato ancho (wide): "),
        "cada fila es una unidad experimental, con columnas del tipo:",
        tags$ul(
          tags$li(code("block"), " (si existe): bloque / sector / repetición."),
          tags$li(code("trat"), " o ", code("variedad"), ": factor de tratamiento."),
          tags$li(code("X_cov"), ": covariable continua (ej. vigor inicial)."),
          tags$li(code("Y1_rend"), " (rendimiento), ", code("Y2_brix"), ", ", code("Y3_firmeza"), ", etc.")
        )
      ),
      tags$li(
        strong("Coherencia de identificadores: "),
        "asegúrate de que los niveles de ", code("trat"), " y ", code("block"),
        " sean consistentes en toda la tabla (sin errores tipográficos ni niveles vacíos)."
      ),
      tags$li(
        strong("Manejo de NA: "),
        "los procedimientos clásicos de MANOVA/MANCOVA requieren ",
        em("casos completos"),
        ": observa cuántas filas tienen NA en alguna respuesta (Y) o en las covariables (X). ",
        "Decide si las eliminas o si necesitas un manejo más sofisticado (imputación, etc.)."
      ),
      tags$li(
        strong("Covariables pre-tratamiento: "),
        "valida que las covariables se midieron ",
        em("antes de aplicar los tratamientos"),
        " o que, conceptualmente, no han sido afectadas por ellos."
      )
    ),
    
    tags$hr(),
    
    # ------------------------------------------------------------------
    # Paso 2 – Plantear los modelos MANOVA vs MANCOVA
    # ------------------------------------------------------------------
    h5("Paso 2 – Plantear modelos MANOVA y MANCOVA"),
    p("En notación de R, asumiendo tres respuestas Y1, Y2 y Y3, un factor ", code("trat"),
      " y una covariable continua ", code("X_cov"), ":"),
    
    tags$ul(
      tags$li(
        strong("Modelo MANOVA base (sin covariable):"),
        tags$br(),
        code("manova(cbind(Y1, Y2, Y3) ~ trat)"),
        " (y eventualmente ", code("~ block + trat"), " si el bloque es fijo)."
      ),
      tags$li(
        strong("Modelo MANCOVA (ajustando por covariable):"),
        tags$br(),
        code("manova(cbind(Y1, Y2, Y3) ~ trat + X_cov)"),
        " (o ", code("~ block + trat + X_cov"), " si corresponde)."
      ),
      tags$li(
        strong("Chequeo de homogeneidad de pendientes multivariante:"),
        tags$br(),
        code("manova(cbind(Y1, Y2, Y3) ~ trat * X_cov)"),
        br(),
        "El término de interacción ", code("trat:X_cov"), 
        " permite evaluar si la relación entre X y las respuestas es similar en todos los tratamientos ",
        "(pendientes comunes) o si hay pendientes distintas por grupo."
      )
    ),
    
    div(
      class = "alert alert-secondary",
      p(
        strong("Interpretación conceptual: "),
        "el modelo MANCOVA busca responder si el tratamiento tiene efecto sobre el ",
        "conjunto de respuestas ",
        em("después de ajustar"),
        " por diferencias en la covariable (por ejemplo, vigor inicial). Al mismo tiempo, ",
        "permite evaluar si la covariable explica una parte importante de la variabilidad conjunta de las respuestas."
      )
    ),
    
    tags$hr(),
    
    # ------------------------------------------------------------------
    # Paso 3 – Diagnósticos y validación de supuestos
    # ------------------------------------------------------------------
    h5("Paso 3 – Diagnósticos y validación de supuestos"),
    tags$ul(
      tags$li(
        strong("Normalidad marginal: "),
        "para cada Y (Y1, Y2, Y3), revisa histogramas y gráficos QQ-plot de los residuos del modelo. ",
        "Aunque la normalidad multivariante es un supuesto ideal, estos chequeos univariados ya dan una buena señal."
      ),
      tags$li(
        strong("Homogeneidad de covarianzas: "),
        "las matrices de covarianza dentro de cada tratamiento deberían ser comparables. ",
        "En la práctica, se puede:",
        tags$ul(
          tags$li("Comparar matrices de covarianza empíricas por tratamiento."),
          tags$li("Usar tests específicos (por ejemplo, test de Box M, si se dispone de la función).")
        )
      ),
      tags$li(
        strong("Relación lineal Y–X: "),
        "para cada respuesta, graficar Yk vs X_cov por tratamiento y revisar si la relación parece aproximadamente lineal."
      ),
      tags$li(
        strong("Homogeneidad de pendientes: "),
        "si el término ", code("trat:X_cov"), " en el modelo MANOVA/MANCOVA no es significativo, ",
        "la suposición de pendientes comunes (modelo aditivo) es razonable. ",
        "Si es claramente significativo, la interpretación de efectos de tratamiento requiere más cuidado: ",
        "las diferencias dependen del nivel de la covariable."
      )
    ),
    
    tags$hr(),
    
    # ------------------------------------------------------------------
    # Paso 4 – Interpretación y redacción de resultados
    # ------------------------------------------------------------------
    h5("Paso 4 – Interpretación y redacción agronómica"),
    p("Una posible secuencia de reporte, en lenguaje técnico pero entendible, podría ser:"),
    
    tags$ul(
      tags$li(
        strong("1) Efecto global del tratamiento (test multivariante): "),
        "reportar un estadístico global (Pillai, Wilks, etc.) para el efecto de ", code("trat"),
        " en el modelo que incluye la covariable. ",
        "Por ejemplo: “La MANCOVA mostró un efecto multivariante significativo de la variedad ",
        "sobre el conjunto de variables de calidad (Pillai = 0.42, p < 0.01), ",
        "ajustando por el vigor inicial”."
      ),
      tags$li(
        strong("2) Efecto de la(s) covariable(s): "),
        "indicar si la covariable tuvo efecto multivariante significativo. ",
        "Por ejemplo: “El vigor inicial presentó un efecto multivariante significativo ",
        "sobre las variables de calidad (p < 0.05), con mayor impacto sobre rendimiento y firmeza”."
      ),
      tags$li(
        strong("3) ANOVAs univariados de seguimiento: "),
        "tras un efecto global significativo, examinar los ANOVA univariados (una por Y) ",
        "idealmente con alguna corrección por multiplicidad (Bonferroni u otra). ",
        "Comentar en qué variables se concentran las diferencias entre tratamientos."
      ),
      tags$li(
        strong("4) Resumen integrado: "),
        "cerrar con 3–5 líneas que integren los hallazgos, por ejemplo:",
        tags$ul(
          tags$li(
            "“En general, las variedades A y B mostraron mejor desempeño global que C, ",
            "principalmente por mayor rendimiento y firmeza, mientras que las diferencias en °Brix fueron pequeñas.”"
          ),
          tags$li(
            "“Tras ajustar por vigor inicial, las conclusiones sobre variedades se mantienen, ",
            "lo que indica que las diferencias no se explican únicamente por plantas más vigorosas al inicio.”"
          )
        )
      )
    )
  )
}

# Pestaña 6: Ejercicios prácticos & tareas (MANCOVA)
pestanna6_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios prácticos & tareas",
    
    h4("Guía de ejercicios para practicar MANCOVA"),
    div(
      class = "alert alert-info",
      p(
        "Usa esta lista como checklist de trabajo combinado con las pestañas ",
        strong("4) Simulación & ajuste por covariable"), " y ",
        strong("5) Protocolo datos reales"),
        ". La idea es que puedas pasar de ejemplos simulados a un análisis real."
      )
    ),
    
    tags$ol(
      tags$li(
        strong("Ejercicio 1 – MANOVA vs MANCOVA en datos simulados"),
        tags$ul(
          tags$li("Ve a la pestaña ", strong("4) Simulación & ajuste por covariable"), 
                  " y genera un conjunto de datos con varias respuestas (Y1, Y2, …) y una covariable X."),
          tags$li("Ajusta primero un modelo ", code("MANOVA"), " sin covariable, por ejemplo:",
                  br(), code("manova(cbind(Y1, Y2, ...) ~ trat)"), "."),
          tags$li("Luego ajusta un modelo ", code("MANCOVA"), " incluyendo la covariable, por ejemplo:",
                  br(), code("manova(cbind(Y1, Y2, ...) ~ trat + X)"), "."),
          tags$li(
            "Compara los tests globales (Wilks, Pillai, etc.) para el efecto de ",
            strong("trat"), " antes y después de ajustar por X."
          ),
          tags$li(
            "Discute en tus palabras: ",
            em("¿La covariable ayuda a detectar mejor el efecto de tratamiento o lo hace menos significativo?")
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 2 – Impacto de la covariable en las conclusiones"),
        tags$ul(
          tags$li(
            "Usando los modelos del Ejercicio 1, anota los p-valores del factor ",
            strong("trat"), " en MANOVA y en MANCOVA."
          ),
          tags$li(
            "Compara también los p-valores univariados (por cada Y) con y sin X en el modelo."
          ),
          tags$li(
            "Redacta 3–4 líneas respondiendo: ",
            em("¿Cómo cambia tu conclusión sobre las variedades/tratamientos al incluir la covariable?"), 
            " ¿En qué respuesta se nota más el ajuste?"
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 3 – Covariable fuertemente asociada al tratamiento"),
        tags$ul(
          tags$li(
            "Modifica la simulación (pestaña 4) para que, por ejemplo, un tratamiento tenga valores de X sistemáticamente más altos ",
            "(por ejemplo, un grupo con plantas mucho más vigorosas)."
          ),
          tags$li(
            "Repite el análisis MANOVA vs MANCOVA."
          ),
          tags$li(
            "Discute por qué, cuando X está muy correlacionada con el tratamiento, la interpretación causal de",
            "ajustar por X se vuelve delicada (la covariable ya no es un simple 'ruido' pre-tratamiento)."
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 4 – Aplicación a un dataset real (si está disponible)"),
        tags$ul(
          tags$li(
            "Selecciona un conjunto de datos reales con varias respuestas de calidad (ej. rendimiento, °Brix, firmeza) ",
            "y al menos una covariable relevante (ej. vigor inicial, altitud, sombreamiento)."
          ),
          tags$li(
            "Sigue el ", strong("checklist"), " de la pestaña ",
            strong("5) Protocolo datos reales"), 
            " para preparar los datos y plantear el modelo."
          ),
          tags$li(
            "Ajusta un modelo MANOVA sin covariable y luego un MANCOVA con covariable(s)."
          ),
          tags$li(
            "Haz un breve reporte (4–5 líneas) donde expliques:",
            tags$ul(
              tags$li("si el tratamiento mantiene su efecto una vez ajustado por la covariable"), 
              tags$li("qué tan importante es la covariable en el conjunto de respuestas"), 
              tags$li("qué implicancias agronómicas tiene esto para la interpretación del ensayo")
            )
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 5 – Síntesis y reflexión"),
        tags$ul(
          tags$li(
            "En un párrafo, compara el rol de la covariable en:",
            strong("ANCOVA"), " (una sola respuesta) vs ",
            strong("MANCOVA"), " (varias respuestas)."
          ),
          tags$li(
            "Responde: ",
            em("¿En qué tipo de situaciones de campo consideras indispensable usar MANCOVA en lugar de analizar cada respuesta por separado?" )
          )
        )
      )
    )
  )
}

# Pestaña Extra: Diagramas Conceptuales (MANCOVA)
pestanna_extra_session11_v3UI <- function(ns) {
  
  # Definición de rutas
  base_path <- "images/sesiones/Disenos_estadisticos_V3/optimizada/"
  img_path  <- paste0(base_path, "session11/")
  
  bslib::nav_panel(
    title = "Extra: Diagramas Conceptuales",
    icon = icon("chalkboard-teacher"), # Icono de pizarra/enseñanza
    
    div(
      class = "container-fluid",
      style = "padding-top: 15px;",
      
      h4(class = "section-header", "Galería Visual: Entendiendo la MANCOVA"),
      p(class = "text-muted", 
        "La MANCOVA combina la complejidad multivariante con el ajuste de covariables. Estas visualizaciones desglosan la geometría, la matemática y el flujo de trabajo del análisis."),
      
      br(),
      
      bslib::navset_card_pill(
        
        # -----------------------------------------------------------------
        # 1. Jerarquía (El Mapa)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "1. El Mapa Estadístico",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                # Imagen
                img(src = paste0(img_path, "mancova_hierarchy_quadrant.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                # Explicación
                div(class = "alert alert-light text-start",
                    h5("¿Dónde estamos?"),
                    p("Este diagrama ubica la técnica en función de la complejidad de tus datos:"),
                    tags$ul(
                      tags$li("Si tienes 1 respuesta y no usas covariables: ", strong("ANOVA"), "."),
                      tags$li("Si añades covariables a 1 respuesta: ", strong("ANCOVA"), "."),
                      tags$li("Si tienes múltiples respuestas sin covariables: ", strong("MANOVA"), "."),
                      tags$li("Si combinas múltiples respuestas Y ajuste por covariables: ", strong("MANCOVA"), ".")
                    )
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 2. Geometría (El Ajuste)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "2. Geometría del Ajuste",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "mancova_geometric_adjustment.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("Ajustando los 'Centroides'"),
                    p("En MANCOVA, no comparamos las medias crudas (los centros originales de las nubes de puntos). Comparar eso sería injusto si un tratamiento tenía plantas más vigorosas desde el inicio."),
                    p("El análisis 'desliza' las nubes de datos a lo largo de la pendiente de la covariable hasta un punto común (la media de la covariable). Las ", strong("Medias Ajustadas"), " son las posiciones finales de estos centroides proyectados.")
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 3. La Pizarra (Fórmula)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "3. La Ecuación Matricial",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "mancova_matrix_equation.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #333; padding: 2px; background-color: #222;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("Desglosando el Modelo General"),
                    withMathJax(), 
                    p("La ecuación se expande para incluir la matriz Z (Covariables):"),
                    p("$$ \\mathbf{Y} = \\mathbf{X}\\mathbf{B} + \\mathbf{Z}\\mathbf{\\Gamma} + \\mathbf{E} $$"),
                    tags$ul(
                      tags$li(strong("Y:"), " Lo que medimos (Rendimiento, Brix, Firmeza)."),
                      tags$li(strong("X:"), " Los tratamientos (Variedades)."),
                      tags$li(strong("Z:"), " Lo que queremos controlar (Vigor Inicial)."),
                      tags$li(strong("E:"), " El error residual (ahora más pequeño gracias a Z).")
                    )
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 4. Supuestos (Planos Paralelos)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "4. Homogeneidad de Pendientes",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "mancova_parallel_planes.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("El Supuesto Crítico: Paralelismo"),
                    p("Para que el ajuste sea válido, la relación entre la covariable y las respuestas debe ser similar para todos los tratamientos."),
                    p("En 3D, esto se visualiza como ", strong("planos paralelos"), ". Si los planos se cruzan drásticamente, significa que hay interacción (el efecto de la covariable depende del tratamiento), y la MANCOVA estándar no se debe interpretar directamente.")
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 5. Reducción de Varianza
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "5. Ganancia de Precisión",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "mancova_error_reduction.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("¿Por qué molestarse en medir la covariable?"),
                    p("Esta imagen ilustra el beneficio. La variabilidad natural de las plantas (ruido) infla la Matriz de Error."),
                    p("Al introducir la Covariable en el modelo, 'filtramos' esa variabilidad explicable. La Matriz de Error resultante es más pequeña. Como los tests estadísticos (Wilks, Pillai) son básicamente una división de [Efecto / Error], ", strong("un error más pequeño hace que sea más fácil detectar diferencias significativas entre tratamientos."), ".")
                )
            )
          )
        ),
        
        # -----------------------------------------------------------------
        # 6. Workflow (Flujo de Decisión)
        # -----------------------------------------------------------------
        bslib::nav_panel(
          title = "6. Flujo de Trabajo",
          div(
            class = "row",
            div(class = "col-md-8 offset-md-2 text-center",
                img(src = paste0(img_path, "mancova_workflow.webp"), 
                    class = "img-fluid rounded shadow-lg", 
                    style = "max-height: 550px; border: 1px solid #ddd; padding: 5px;"),
                br(), br(),
                div(class = "alert alert-light text-start",
                    h5("Ruta de Análisis Sugerida"),
                    tags$ol(
                      tags$li("Explorar datos: Scatterplots y correlaciones."),
                      tags$li(strong("Test de Interacción:"), " Ajustar modelo Trat * Covariable. ¿Es significativo?"),
                      tags$li("Si NO es significativo (Paralelismo OK) -> Ajustar modelo aditivo (Trat + Covariable)."),
                      tags$li("Evaluar Test Global (Pillai/Wilks)."),
                      tags$li("Si es significativo -> Analizar ANOVAs univariados corregidos o Medias Ajustadas (emmeans).")
                    )
                )
            )
          )
        )
      )
    )
  )
}

# Pestaña 7: Referencias (MANCOVA)
pestanna7_session11_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    
    h4("Referencias para profundizar en MANCOVA"),
    
    # -------------------------------------------------------------------
    # 1) MANOVA / MANCOVA teórico
    # -------------------------------------------------------------------
    h5("1) MANOVA / MANCOVA teórico"),
    tags$ul(
      tags$li(
        "Capítulos de MANOVA y MANCOVA en textos clásicos de análisis multivariado, ",
        "donde se discuten: vectores de medias, matrices H y E, y pruebas globales ",
        "tipo Wilks, Pillai, Hotelling–Lawley y Roy."
      ),
      tags$li(
        "Libros de modelos lineales / modelos lineales generalizados donde MANCOVA ",
        "se presenta como extensión natural de la ANCOVA y de la MANOVA, ",
        "enmarcada dentro del modelo lineal general (GLM)."
      ),
      tags$li(
        "Notas de cursos avanzados de estadística multivariada que incluyan ejemplos ",
        "con varias respuestas y una o más covariables, y discutan con detalle los ",
        "supuestos (normalidad multivariante, homogeneidad de covarianzas, ",
        "homogeneidad de pendientes)."
      )
    ),
    
    # -------------------------------------------------------------------
    # 2) Aplicaciones en biología y agronomía
    # -------------------------------------------------------------------
    h5("2) Aplicaciones en biología / agronomía"),
    tags$ul(
      tags$li(
        "Artículos de mejoramiento genético donde se analizan simultáneamente ",
        "variables de calidad (rendimiento, tamaño de fruto, color, firmeza, °Brix) ",
        "y se ajusta por covariables ambientales (vigor inicial, altitud, sombra, etc.)."
      ),
      tags$li(
        "Estudios de fisiología de cultivos donde se evalúa el efecto de tratamientos ",
        "(riego, fertilización, manejo de canopia) sobre un conjunto de respuestas ",
        "correlacionadas y se incluyen covariables para controlar gradientes de sitio."
      ),
      tags$li(
        "Tesis de posgrado en agronomía / biología que usen MANCOVA para integrar ",
        "información de múltiples rasgos en una sola inferencia global sobre tratamientos."
      )
    ),
    
    # -------------------------------------------------------------------
    # 3) Implementación en R
    # -------------------------------------------------------------------
    h5("3) Implementación en R"),
    tags$ul(
      tags$li(
        "Funciones base de R: ",
        code("manova()"),
        " y ",
        code("summary.manova()"),
        " para ajustar y resumir MANOVA / MANCOVA con fórmulas del tipo ",
        code("cbind(Y1, Y2, ...) ~ trat + X"),
        "."
      ),
      tags$li(
        "Paquetes como ",
        code("car"),
        " y ",
        code("heplots"),
        " para contrastes multivariantes, gráficos de elipses (HE plots) y ",
        "diagnósticos visuales de MANOVA/MANCOVA."
      ),
      tags$li(
        "Ejemplos y viñetas donde se combina el ajuste multivariante (MANOVA/MANCOVA) ",
        "con ANOVAs univariados de seguimiento y corrección por comparaciones múltiples."
      ),
      tags$li(
        "Scripts que muestran cómo pasar de la MANCOVA global a la interpretación ",
        "agronómica: extracción de medias ajustadas, construcción de contrastes y ",
        "resumen conjunto de los efectos de tratamiento sobre todas las respuestas."
      )
    ),
    
    hr(),
    p(
      em(
        "Sugerencia didáctica: invita a los alumnos a escoger al menos una referencia teórica, ",
        "una aplicada y una de implementación en R para profundizar después de la sesión."
      )
    )
  )
}

# -------------------------------------------------------------------------
# Main UI Sesión 11
# -------------------------------------------------------------------------

session11_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 11: MANCOVA")
    ),
    navset_tab(
      pestanna1_session11_v3UI(ns),
      pestanna2_session11_v3UI(ns),
      pestanna3_session11_v3UI(ns),
      pestanna4_session11_v3UI(ns),
      pestanna5_session11_v3UI(ns),
      pestanna6_session11_v3UI(ns),
      pestanna_extra_session11_v3UI(ns),
      pestanna7_session11_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server per Tab Sesión 11
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (MANCOVA)
pestanna1_session11_v3_server <- function(input, output, session) {
  # Solo texto estático en esta pestaña: no se requiere lógica de server
}

# Pestaña 2: Conceptos clave MANCOVA
pestanna2_session11_v3_server <- function(input, output, session) {
  # Pestaña puramente conceptual: no se requiere lógica reactiva.
}

# Pestaña 3: Diseño & estructura de datos (MANCOVA)
pestanna3_session11_v3_server <- function(input, output, session) {
  
  # -----------------------------------------------------------------
  # 1) Construimos un reactive con los datos de ejemplo
  #    - O bien simulamos un dataset típico de MANCOVA
  #    - O bien leemos un CSV que suba el usuario
  # -----------------------------------------------------------------
  mancova_data <- reactive({
    # Caso 1: el usuario quiere cargar un CSV
    if (identical(input$mancova_example_source, "upload")) {
      file <- input$mancova_file
      validate(
        need(!is.null(file),
             "Sube un archivo CSV o selecciona la opción de 'Dataset simulado (interno)'.")
      )
      
      df <- tryCatch(
        utils::read.csv(file$datapath, stringsAsFactors = FALSE),
        error = function(e) NULL
      )
      validate(
        need(!is.null(df), "No se pudo leer el archivo CSV. Revisa el formato.")
      )
      return(df)
    }
    
    # Caso 2: dataset simulado interno (estructura 'ideal' de MANCOVA)
    set.seed(123)
    n_block  <- 3
    n_trat   <- 4
    n_rep    <- 5  # n° de parcelas por combinación block × trat
    
    blocks <- factor(rep(paste0("B", 1:n_block), each = n_trat * n_rep))
    trats  <- factor(rep(paste0("Var", 1:n_trat), times = n_block * n_rep))
    
    # Covariable (por ejemplo, vigor inicial), ligeramente distinta por bloque
    X_cov_block_mean <- c(20, 22, 24)
    X_cov <- rnorm(length(blocks),
                   mean = rep(X_cov_block_mean, each = n_trat * n_rep),
                   sd   = 2)
    
    # Efectos de tratamiento sobre las respuestas
    eff_trat_Y1 <- c(0, 300, 600, 900)      # efecto sobre rendimiento
    eff_trat_Y2 <- c(0, -0.3, 0.2, 0.5)     # efecto sobre Brix
    eff_trat_Y3 <- c(0, 10, 20, 30)         # efecto sobre firmeza
    
    idx_trat <- as.integer(trats)
    
    # Respuestas simuladas (Y1, Y2, Y3)
    Y1_rend    <- 8000 + eff_trat_Y1[idx_trat] + 50 * (X_cov - mean(X_cov)) + rnorm(length(blocks), 0, 400)
    Y2_brix    <- 11   + eff_trat_Y2[idx_trat] +  0.1 * (X_cov - mean(X_cov)) + rnorm(length(blocks), 0, 0.4)
    Y3_firmeza <- 250  + eff_trat_Y3[idx_trat] +  1.5 * (X_cov - mean(X_cov)) + rnorm(length(blocks), 0, 10)
    
    data.frame(
      block      = blocks,
      trat       = trats,
      X_cov      = X_cov,
      Y1_rend    = Y1_rend,
      Y2_brix    = Y2_brix,
      Y3_firmeza = Y3_firmeza
    )
  })
  
  # -----------------------------------------------------------------
  # 2) Vista previa del data frame
  # -----------------------------------------------------------------
  output$mancova_example_data <- DT::renderDataTable({
    df <- mancova_data()
    DT::datatable(
      df,
      options  = list(pageLength = 5, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -----------------------------------------------------------------
  # 3) Mensaje recordatorio sobre la estructura (no es un modelo real)
  # -----------------------------------------------------------------
  output$mancova_example_model <- renderPrint({
    df <- mancova_data()
    
    cat("Esta pestaña se enfoca en la ESTRUCTURA de los datos para una MANCOVA.\n\n")
    cat("Columnas detectadas en el dataset cargado / simulado:\n")
    print(names(df))
    
    # Intentamos identificar columnas típicas
    has_block <- any(grepl("^block$|^bloque$", names(df), ignore.case = TRUE))
    has_trat  <- any(grepl("^trat$|^treat$|^treatment$", names(df), ignore.case = TRUE))
    
    # Columnas numéricas candidatas a covariables y respuestas
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    cat("\nSugerencias de interpretación:\n")
    if (has_block) {
      cat("- Se detecta una columna tipo 'block': podría usarse como factor de bloque en el diseño.\n")
    } else {
      cat("- No se detecta una columna llamada 'block'; el diseño podría ser CRD o el nombre es distinto.\n")
    }
    if (has_trat) {
      cat("- Se detecta una columna tipo 'trat': candidata a factor de tratamiento/grupo.\n")
    } else {
      cat("- No se detecta una columna llamada 'trat'; verifica dónde está el factor de tratamiento.\n")
    }
    
    if (length(num_cols) >= 2) {
      cat("- Columnas numéricas detectadas (candidatas a X_cov y Y1, Y2, ...):\n")
      print(num_cols)
      cat("\nEn una MANCOVA típica necesitarás:\n",
          "  * Al menos una covariable numérica (p.ej. X_cov).\n",
          "  * Dos o más respuestas numéricas (Y1, Y2, ...).\n",
          "  * Un factor de tratamiento común a todas las respuestas.\n", sep = "")
    } else {
      cat("- No hay suficientes columnas numéricas para ilustrar una MANCOVA.\n")
    }
    
    cat(
      "\nRecuerda: la MANCOVA se ajustará en la pestaña de simulación/análisis, ",
      "con fórmulas del estilo:\n",
      "  manova(cbind(Y1, Y2, Y3) ~ trat + X_cov)\n",
      "o añadiendo bloque si corresponde.\n", sep = ""
    )
  })
  
  # -----------------------------------------------------------------
  # 4) Visualización básica: dos respuestas numéricas (si existen)
  # -----------------------------------------------------------------
  output$mancova_example_plot <- renderPlot({
    df <- mancova_data()
    
    # Identificar columnas numéricas
    num_cols <- names(df)[sapply(df, is.numeric)]
    
    if (length(num_cols) < 2) {
      plot.new()
      text(0.5, 0.5,
           "No hay suficientes columnas numéricas\npara construir un gráfico Y vs Y.",
           cex = 1.1)
      return(invisible(NULL))
    }
    
    # Tomamos las dos primeras columnas numéricas como ejemplo de respuestas
    y1_name <- num_cols[1]
    y2_name <- num_cols[2]
    
    # Tratamiento para colorear, si existe
    trat_col <- NULL
    if ("trat" %in% names(df)) {
      trat_col <- "trat"
    } else {
      # busco algo tipo “tratamiento”
      cand <- grep("trat|treat", names(df), ignore.case = TRUE, value = TRUE)
      if (length(cand) > 0) trat_col <- cand[1]
    }
    
    if (is.null(trat_col)) {
      ggplot2::ggplot(df, ggplot2::aes_string(x = y1_name, y = y2_name)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          x     = y1_name,
          y     = y2_name,
          title = "Ejemplo de relación entre dos respuestas numéricas"
        )
    } else {
      ggplot2::ggplot(df, ggplot2::aes_string(x = y1_name, y = y2_name, color = trat_col)) +
        ggplot2::geom_point(alpha = 0.7) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(
          x     = y1_name,
          y     = y2_name,
          color = trat_col,
          title = "Respuestas numéricas por tratamiento (estructura MANCOVA)"
        )
    }
  })
}

# Pestaña 4: Simulación & análisis MANCOVA
pestanna4_session11_v3_server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  # 1) SIMULACIÓN DE DATOS MANCOVA
  #    - Trat: factor con g niveles
  #    - X: covariable continua (centrada)
  #    - Y1, Y2, Y3: respuestas correlacionadas
  # -------------------------------------------------------------------
  sim_mancova_data <- eventReactive(input$btn_sim_mancova, {
    req(input$mancova_g, input$mancova_n)
    
    set.seed(123)  # Reproducibilidad didáctica
    
    g        <- input$mancova_g
    n_per    <- input$mancova_n
    N        <- g * n_per
    sigma_e  <- input$mancova_sigma
    rho      <- input$mancova_rho
    
    # Tratamientos como factor T1, T2, ..., Tg
    trat <- factor(rep(paste0("T", seq_len(g)), each = n_per))
    
    # Covariable X ~ Normal(0,1), centrada
    X_raw <- stats::rnorm(N, mean = 0, sd = 1)
    X     <- X_raw - mean(X_raw)
    
    # Pendientes verdaderas para cada respuesta
    beta_vec <- c(input$mancova_beta1,
                  input$mancova_beta2,
                  input$mancova_beta3)
    
    # Medias base de las respuestas (ejemplo agronómico)
    # Y1: rendimiento, Y2: °Brix, Y3: firmeza
    mu_base <- c(9000, 12, 280)
    
    # Efectos de tratamiento sobre cada respuesta
    # (simulados para ilustrar diferencias entre tratamientos)
    # g x 3 matriz: fila = tratamiento, columna = Y1,Y2,Y3
    eff_trat <- matrix(
      stats::rnorm(g * 3, mean = 0, sd = 400),
      nrow = g, ncol = 3
    )
    
    # Matriz de covarianza residual Σ con correlación ρ
    # Varianzas iguales = sigma_e^2 para cada Yk
    p <- 3
    Sigma <- matrix(rho * sigma_e^2, nrow = p, ncol = p)
    diag(Sigma) <- sigma_e^2
    
    # Descomposición de Cholesky para generar errores correlacionados
    chol_S <- chol(Sigma)
    
    # Matrices para medias y errores
    mu_mat <- matrix(NA_real_, nrow = N, ncol = p)
    
    # Para cada tratamiento, asignar medias
    levs_trat <- levels(trat)
    for (k in seq_len(g)) {
      idx_k <- which(trat == levs_trat[k])
      # Para cada respuesta j = 1,2,3
      for (j in seq_len(p)) {
        mu_mat[idx_k, j] <- mu_base[j] +
          eff_trat[k, j] +
          beta_vec[j] * X[idx_k]
      }
    }
    
    # Errores multivariantes
    Z <- matrix(stats::rnorm(N * p), nrow = N, ncol = p)
    E <- Z %*% chol_S  # N x 3
    
    Y_mat <- mu_mat + E
    colnames(Y_mat) <- c("Y1", "Y2", "Y3")
    
    # Construir data.frame final
    df <- data.frame(
      trat = trat,
      X    = X,
      Y1   = Y_mat[, "Y1"],
      Y2   = Y_mat[, "Y2"],
      Y3   = Y_mat[, "Y3"]
    )
    
    df
  })
  
  # -------------------------------------------------------------------
  # 2) SALIDAS: TABLA DE DATOS Y CORRELACIONES
  # -------------------------------------------------------------------
  output$mancova_sim_data <- DT::renderDataTable({
    df <- sim_mancova_data()
    req(df)
    
    DT::datatable(
      head(df, 20),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$mancova_sim_cor <- renderPrint({
    df <- sim_mancova_data()
    req(df)
    
    cat("Correlaciones empíricas entre respuestas (Y1, Y2, Y3):\n")
    print(stats::cor(df[, c("Y1", "Y2", "Y3")]))
    
    cat("\nCorrelaciones entre X y las respuestas:\n")
    print(stats::cor(df[, c("X", "Y1", "Y2", "Y3")]))
  })
  
  # -------------------------------------------------------------------
  # 3) AJUSTE DE MANOVA / MANCOVA
  #    - MANOVA sin X
  #    - MANCOVA aditiva con X
  #    - MANCOVA con interacción trat * X
  # -------------------------------------------------------------------
  output$mancova_sim_results <- renderPrint({
    df <- sim_mancova_data()
    req(df)
    
    cat("=== Resumen de la simulación MANCOVA ===\n")
    cat("N° tratamientos:", nlevels(df$trat),
        "| Obs. por tratamiento:", table(df$trat)[1], "\n\n")
    
    # Modelo 1: MANOVA sin covariable
    fit0 <- stats::manova(cbind(Y1, Y2, Y3) ~ trat, data = df)
    # Modelo 2: MANCOVA aditiva (sin interacción)
    fit1 <- stats::manova(cbind(Y1, Y2, Y3) ~ trat + X, data = df)
    # Modelo 3: MANCOVA con interacción trat:X
    fit2 <- stats::manova(cbind(Y1, Y2, Y3) ~ trat * X, data = df)
    
    cat("--- Modelo 1: MANOVA sin covariable ---\n")
    print(summary(fit0, test = "Pillai"))
    
    cat("\n--- Modelo 2: MANCOVA aditiva (trat + X) ---\n")
    print(summary(fit1, test = "Pillai"))
    
    cat("\n--- Modelo 3: MANCOVA con interacción (trat * X) ---\n")
    print(summary(fit2, test = "Pillai"))
    
    cat(
      "\nInterpretación didáctica:\n",
      "- Compara el estadístico Pillai y el p-valor del efecto de tratamiento entre el Modelo 1 y el Modelo 2.\n",
      "- Si la covariable X explica parte importante de la variación conjunta de (Y1,Y2,Y3),\n",
      "  el test global de tratamiento suele ganar potencia al incluir X (Modelo 2).\n",
      "- El término trat:X en el Modelo 3 permite evaluar si las pendientes de X son similares entre tratamientos\n",
      "  (homogeneidad de pendientes multivariante).\n"
    )
  })
  
  # -------------------------------------------------------------------
  # 4) GRÁFICO: RELACIÓN Y1 ~ X POR TRATAMIENTO
  # -------------------------------------------------------------------
  output$mancova_sim_plot <- renderPlot({
    df <- sim_mancova_data()
    req(df)
    
    ggplot2::ggplot(df, ggplot2::aes(x = X, y = Y1, colour = trat)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_smooth(method = "lm", se = FALSE) +
      ggplot2::labs(
        x = "Covariable X (centrada)",
        y = "Y1 (por ejemplo, rendimiento)",
        colour = "Tratamiento",
        title = "Relación simulada entre Y1 y X por tratamiento"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
}

# Pestaña 5: Protocolo datos reales MANCOVA
pestanna5_session11_v3_server <- function(input, output, session) {
  # Esta pestaña solo muestra texto estático y no requiere lógica de servidor.
}

# Pestaña 6: Ejercicios prácticos & tareas (MANCOVA)
pestanna6_session11_v3_server <- function(input, output, session) {
  # Pestaña solo informativa: no se requiere lógica de servidor.
}

# Pestaña 7: Referencias (MANCOVA)
pestanna7_session11_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor: todo es texto estático guiado.
}

# -------------------------------------------------------------------------
# Main Server Sesión 11
# -------------------------------------------------------------------------

session11_v3Server <- function(input, output, session) {
  # has_pkg <- function(p) requireNamespace(p, quietly = TRUE)
  
  pestanna1_session11_v3_server(input, output, session)
  pestanna2_session11_v3_server(input, output, session)
  pestanna3_session11_v3_server(input, output, session)
  pestanna4_session11_v3_server(input, output, session)
  pestanna5_session11_v3_server(input, output, session)
  pestanna6_session11_v3_server(input, output, session)
  pestanna7_session11_v3_server(input, output, session)
}
