# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session9.R
# Sesión 9: ANCOVA

# -------------------------------------------------------------------------
# UI per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (ANCOVA)
pestanna1_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto (ANCOVA)",
    
    # Título principal
    h4(class = "section-header",
       "¿Dónde encaja la ANCOVA en la familia de modelos del curso?"),
    
    # Bloque de idea central
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "la ANCOVA es un modelo que combina ",
        strong("ANOVA (factores de tratamiento)"),
        " con ",
        strong("regresión (covariables continuas)"),
        " para ganar precisión. Ajusta la respuesta por diferencias iniciales o gradientes ",
        em("medidos"),
        " en cada unidad experimental."
      )
    ),
    
    # Timeline / historia dentro del curso
    h5("Línea de tiempo dentro del curso"),
    tags$ul(
      tags$li(
        strong("ANOVA clásico:"),
        " modelo del tipo ",
        code("Y ~ Tratamiento"),
        " para comparar medias entre tratamientos."
      ),
      tags$li(
        strong("Regresión simple:"),
        " modelo del tipo ",
        code("Y ~ X"),
        " para estudiar la relación de la respuesta con una covariable continua (X)."
      ),
      tags$li(
        strong("ANCOVA:"),
        " modelo del tipo ",
        code("Y ~ Tratamiento + X"),
        " que combina ambos: comparamos tratamientos ",
        em("ajustando"),
        " por la covariable X."
      ),
      tags$li(
        strong("Con bloqueo en campo:"),
        " podemos incluir bloques y, si es necesario, usar un modelo mixto (LMM), por ejemplo:",
        " ",
        code("Y ~ Bloque + Tratamiento + X"),
        " o bien ",
        code("Y ~ Tratamiento + X + (1|Bloque)"),
        " cuando consideramos el bloque como efecto aleatorio."
      )
    ),
    
    hr(),
    
    # Ejemplo agronómico ancla
    fluidRow(
      column(
        width = 6,
        h5("Ejemplo ancla agronómico"),
        div(
          class = "alert alert-success",
          p(
            "Imaginemos un ensayo de riego con 3 niveles (R1, R2, R3) en parcelas de arándanos. "
          ),
          tags$ul(
            tags$li(
              strong("Respuesta (Y):"),
              " rendimiento (kg/ha) al final de la campaña."
            ),
            tags$li(
              strong("Factor de tratamiento:"),
              " sistema de riego (3 niveles)."
            ),
            tags$li(
              strong("Covariable (X):"),
              " vigor inicial o biomasa al inicio de la temporada, medida ",
              strong("antes"),
              " de aplicar los tratamientos."
            ),
            tags$li(
              strong("Bloque:"),
              " sector del campo (diferentes zonas de fertilidad o pendiente)."
            )
          ),
          p(
            "La ANCOVA nos permite comparar los sistemas de riego en rendimiento, ",
            "ajustando por diferencias iniciales de vigor entre plantas o parcelas."
          )
        )
      ),
      column(
        width = 6,
        h5("¿Por qué no basta con solo ANOVA o solo regresión?"),
        tags$ul(
          tags$li(
            strong("Solo ANOVA (Y ~ Tratamiento):"),
            " ignora que algunas parcelas estaban más vigorosas que otras al inicio."
          ),
          tags$li(
            strong("Solo regresión (Y ~ X):"),
            " modela la relación con el vigor, pero no permite comparar formalmente los niveles de riego."
          ),
          tags$li(
            strong("ANCOVA (Y ~ Tratamiento + X):"),
            " combina ambas ideas: ",
            "usa X para explicar parte de la variabilidad y deja un 'residuo' más limpio ",
            "para comparar tratamientos."
          )
        ),
        # Pequeño guiño de modelo, sin entrar en demasiados detalles (lo formal irá en P2)
        withMathJax(
          div(
            class = "note-cloud",
            p(
              strong("Esquema de modelo básico (sin bloque):")
            ),
            helpText("$$Y_{ij} = \\mu + \\tau_i + \\beta X_{ij} + \\varepsilon_{ij}$$"),
            p(
              "donde ",
              "\\(\\tau_i\\) es el efecto del tratamiento i, ",
              "\\(\\beta\\) mide el efecto lineal de la covariable X y ",
              "\\(\\varepsilon_{ij}\\) es el error residual."
            )
          )
        )
      )
    ),
    
    hr(),
    
    # Resultados de aprendizaje / outcomes
    h5("Resultados de aprendizaje de la Sesión 9 (ANCOVA)"),
    tags$ol(
      tags$li(
        "Explicar con palabras sencillas qué es una ANCOVA y en qué se diferencia de ANOVA y de la regresión simple."
      ),
      tags$li(
        "Identificar situaciones agronómicas donde tiene sentido incluir una covariable (medida antes del tratamiento) para reducir ruido."
      ),
      tags$li(
        "Reconocer la forma básica del modelo ANCOVA y su extensión con bloques o modelos mixtos, por ejemplo ",
        code("Y ~ Tratamiento + X + (1|Bloque)"),
        "."
      )
    )
  )
}

# Pestaña 2: Conceptos clave & modelo ANCOVA
pestanna2_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave & modelo ANCOVA",
    withMathJax(
      tagList(
        h4(class = "section-header", "Modelo estadístico de la ANCOVA"),
        
        # -------------------------------------------------------------------
        # Bloque 1: Modelo sin interacción (ANCOVA clásica)
        # -------------------------------------------------------------------
        div(
          class = "alert alert-info",
          p(
            strong("Idea central: "),
            "la ANCOVA es un modelo que combina un factor de tratamiento ",
            "(categórico) con una covariable continua para explicar una respuesta ",
            "Y con mayor precisión."
          ),
          helpText(
            "Modelo básico (un factor de tratamientos y una covariable continua X, ",
            "sin interacción Trat × X):"
          ),
          helpText("$$
          Y_{ij} = \\mu + \\tau_i + \\beta\\,(X_{ij} - \\bar{X}_{..}) + \\epsilon_{ij}
          $$"),
          tags$ul(
            tags$li(HTML("<strong>Y<sub>ij</sub></strong>: respuesta en el tratamiento i, unidad j (ej. rendimiento).")),
            tags$li(HTML("<strong>\\(\\mu\\)</strong>: media general del experimento.")),
            tags$li(HTML("<strong>\\(\\tau_i\\)</strong>: efecto del tratamiento i (desviación de la media).")),
            tags$li(HTML("<strong>\\(X_{ij}\\)</strong>: valor observado de la covariable en la unidad ij (ej. vigor inicial).")),
            tags$li(HTML("<strong>\\(\\bar{X}_{..}\\)</strong>: media global de la covariable X.")),
            tags$li(HTML("<strong>\\(\\beta\\)</strong>: pendiente común de X (efecto lineal de la covariable).")),
            tags$li(HTML("<strong>\\(\\epsilon_{ij}\\)</strong>: error aleatorio, usualmente se asume \\(\\epsilon_{ij} \\sim N(0,\\sigma^2)\\)."))
          )
        ),
        
        # -------------------------------------------------------------------
        # Bloque 2: Modelo con interacción Trat × X (chequeo de pendientes)
        # -------------------------------------------------------------------
        fluidRow(
          column(
            width = 6,
            h5("Extensión: interacción Trat × X (pendientes no homogéneas)"),
            helpText(
              "Para comprobar si la relación Y–X es similar en todos los tratamientos, ",
              "podemos agregar un término de interacción Trat × X:"
            ),
            helpText("$$
            Y_{ij} = \\mu + \\tau_i + \\beta\\,(X_{ij} - \\bar{X}_{..})
                     + \\gamma_i\\,(X_{ij} - \\bar{X}_{..}) + \\epsilon_{ij}
            $$"),
            tags$ul(
              tags$li(HTML("<strong>\\(\\gamma_i\\)</strong>: ajuste específico de la pendiente en el tratamiento i.")),
              tags$li("Si todas las γᵢ ≈ 0, las pendientes son similares → ANCOVA clásica con pendiente común."),
              tags$li("Si alguna γᵢ es importante, cada tratamiento tiene una pendiente distinta Y–X.")
            )
          ),
          column(
            width = 6,
            h5("Interpretación didáctica"),
            tags$ul(
              tags$li("Primero se ajusta el efecto de la covariable X (tendencia lineal global o por tratamiento)."),
              tags$li("Luego se comparan tratamientos en un nivel común de X (ej. media de vigor inicial)."),
              tags$li("Esto reduce el ‘ruido’ asociado a diferencias iniciales entre unidades.")
            ),
            div(
              class = "note-cloud",
              p(
                strong("Regla práctica: "),
                "antes de usar una ANCOVA con pendiente común, ",
                "es recomendable probar el término de interacción Trat × X. ",
                "Si no es significativo, se mantiene el modelo con pendiente común."
              )
            )
          )
        ),
        
        hr(),
        
        # -------------------------------------------------------------------
        # Bloque 3: Supuestos clave de la ANCOVA
        # -------------------------------------------------------------------
        fluidRow(
          column(
            width = 6,
            h5("Supuestos estadísticos principales"),
            tags$ul(
              tags$li(strong("Normalidad de los residuos:"), " los errores εᵢⱼ siguen aproximadamente una distribución normal."),
              tags$li(strong("Varianza constante:"), " la dispersión de los residuos es similar en todos los niveles de tratamiento y valores de X."),
              tags$li(strong("Independencia:"), " las observaciones de distintas unidades experimentales son independientes."),
              tags$li(strong("Linealidad:"), " la relación entre Y y X es aproximadamente lineal dentro del rango observado de X.")
            )
          ),
          column(
            width = 6,
            h5("Supuestos específicos de ANCOVA"),
            tags$ul(
              tags$li(
                strong("Homogeneidad de pendientes: "),
                "en la ANCOVA clásica se asume una pendiente común β para todos los tratamientos."
              ),
              tags$li(
                "La covariable X idealmente se mide ",
                strong("antes de aplicar los tratamientos"),
                " (pre-tratamiento), de modo que no esté afectada por éstos."
              ),
              tags$li(
                "La distribución de X debería ser ‘razonablemente balanceada’ entre tratamientos. ",
                "Si un tratamiento tiene valores de X muy distintos de los demás, la interpretación se vuelve más delicada."
              )
            )
          )
        ),
        
        hr(),
        
        # -------------------------------------------------------------------
        # Bloque 4: ¿Qué hace la ANCOVA con las medias?
        # -------------------------------------------------------------------
        h5("¿Qué hace la ANCOVA con las medias de tratamiento?"),
        tags$ul(
          tags$li(
            strong("1. Ajuste por covariable: "),
            "las medias de los tratamientos se corrigen a un mismo nivel de X (por ejemplo, X = media global)."
          ),
          tags$li(
            strong("2. Reducción de error: "),
            "si X explica una parte importante de la variación de Y, el término de error residual ",
            "se hace más pequeño y las pruebas F ganan potencia."
          ),
          tags$li(
            strong("3. Comparaciones más justas: "),
            "dos tratamientos con distinto valor medio de X se comparan ‘como si’ hubiesen tenido el mismo valor de la covariable."
          )
        ),
        
        div(
          class = "alert alert-secondary",
          p(
            strong("Ejemplo agronómico guía: "),
            "ensayo de riego con 3 niveles de Tratamiento, covariable X = vigor o biomasa inicial, ",
            "respuesta Y = rendimiento (kg/ha). La ANCOVA permite comparar los niveles de riego ",
            "ajustando por diferencias iniciales en vigor."
          )
        )
      )
    )
  )
}

# Pestaña 3: Diseño & estructura de datos para ANCOVA
pestanna3_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & estructura de datos",
    
    h4(class = "section-header", "¿Cómo debe lucir el dataset para una ANCOVA?"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea práctica: "),
        "para poder ajustar una ANCOVA en un ensayo agronómico, ",
        "necesitamos un archivo tipo 'libro de campo' donde ",
        em("cada fila es una unidad experimental"),
        " y las columnas incluyan al menos: bloque (opcional), tratamiento, ",
        "covariable continua (X) y respuesta (Y)."
      )
    ),
    
    fluidRow(
      column(
        width = 6,
        h5("Estructura mínima de columnas"),
        tags$ul(
          tags$li(
            code("block"), ": bloque o sector del campo (opcional, tipo RCBD)."
          ),
          tags$li(
            code("trat"), ": tratamiento o nivel del factor principal ",
            "(por ejemplo, niveles de riego o fertilización)."
          ),
          tags$li(
            code("X_cov"), ": covariable continua medida en cada unidad, ",
            "por ejemplo ", em("vigor inicial, biomasa al inicio, humedad del suelo, NDVI pre-tratamiento"), "."
          ),
          tags$li(
            code("Y"), ": respuesta de interés (rendimiento, firmeza, °Brix, etc.)."
          )
        ),
        h5("Ejemplo de tabla (formato 'una fila = unidad experimental')"),
        tags$table(
          class = "table table-sm table-bordered",
          tags$thead(
            tags$tr(
              tags$th("block"),
              tags$th("trat"),
              tags$th("X_cov"),
              tags$th("Y")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td("1"),
              tags$td("Riego_bajo"),
              tags$td("23.4"),
              tags$td("8 900")
            ),
            tags$tr(
              tags$td("1"),
              tags$td("Riego_medio"),
              tags$td("21.1"),
              tags$td("9 200")
            ),
            tags$tr(
              tags$td("1"),
              tags$td("Riego_alto"),
              tags$td("22.0"),
              tags$td("9 800")
            ),
            tags$tr(
              tags$td("2"),
              tags$td("Riego_bajo"),
              tags$td("24.0"),
              tags$td("9 050")
            )
          )
        ),
        p(
          em("En las siguientes pestañas, esta misma estructura se usará para ajustar el modelo ANCOVA.")
        )
      ),
      column(
        width = 6,
        h5("Puntos de diseño importantes"),
        tags$ul(
          tags$li(
            strong("La covariable X se mide idealmente "),
            "antes de aplicar los tratamientos (estado inicial)."
          ),
          tags$li(
            "X no es un factor experimental; es una medición de contexto que explica parte de la variabilidad de Y."
          ),
          tags$li(
            "En ensayos con bloque, X puede variar dentro de cada bloque → ANCOVA 'dentro de bloque': ",
            code("Y ~ block + trat + X_cov"),
            " o en versión mixta: ",
            code("Y ~ trat + X_cov + (1 | block)")
          )
        ),
        h5("Validaciones exploratorias recomendadas"),
        tags$ul(
          tags$li(
            strong("1) Dispersión Y vs X_cov por tratamiento: "),
            "ver si la relación es aproximadamente lineal."
          ),
          tags$li(
            strong("2) Boxplot de X_cov por tratamiento: "),
            "ver si hay grandes diferencias en la distribución de X entre tratamientos."
          ),
          tags$li(
            "Si la covariable está muy desbalanceada entre tratamientos, la interpretación causal de ANCOVA se vuelve complicada."
          )
        )
      )
    ),
    
    hr(),
    h4("Ejemplo simulado de estructura de datos (ANCOVA en un RCBD simple)"),
    p(
      "A continuación se muestra un pequeño dataset simulado con 3 tratamientos de riego y 4 bloques. ",
      "La covariable ", code("X_cov"), " puede interpretarse como una medida de vigor inicial. ",
      "La respuesta ", code("Y"), " es el rendimiento en kg/ha."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        helpText(
          "Este ejemplo está simulado dentro de la app. ",
          "La idea es que puedas reconocer el formato que deberías tener en tu propio Excel."
        ),
        tags$ul(
          tags$li("3 tratamientos: Riego_bajo, Riego_medio, Riego_alto."),
          tags$li("4 bloques (block = 1, 2, 3, 4)."),
          tags$li("1 observación por combinación bloque × tratamiento (diseño RCBD sencillo).")
        )
      ),
      mainPanel(
        width = 8,
        h5("Vista de datos simulados"),
        DT::dataTableOutput(ns("ancova_data_table")),
        hr(),
        h5("Exploración 1: Y vs X_cov coloreado por tratamiento"),
        plotOutput(ns("ancova_scatter"), height = "320px"),
        hr(),
        h5("Exploración 2: Distribución de X_cov por tratamiento"),
        plotOutput(ns("ancova_boxplot"), height = "320px")
      )
    )
  )
}

# Pestaña 4: Simulación & análisis (ANCOVA)
pestanna4_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & análisis (ANCOVA)",
    
    h4(class = "section-header", "¿Qué gana la ANCOVA frente a un ANOVA simple?"),
    
    p(
      "En esta pestaña simulamos datos de un experimento simple con tratamientos categóricos ",
      "y una covariable continua X. Compararemos:",
      tags$ul(
        tags$li(strong("Modelo 1 (ANOVA): "), code("Y ~ trat")),
        tags$li(strong("Modelo 2 (ANCOVA aditiva): "), code("Y ~ trat + X")),
        tags$li(strong("Modelo 3 (pendientes específicas): "), code("Y ~ trat * X"))
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("1. Dimensión del experimento"),
        numericInput(
          ns("ancova_k_trat"),
          "N° de tratamientos:",
          value = 3, min = 2, max = 8, step = 1
        ),
        numericInput(
          ns("ancova_n_per_trat"),
          "N° de unidades por tratamiento:",
          value = 20, min = 5, max = 200, step = 1
        ),
        hr(),
        
        h5("2. Parámetros 'verdaderos' del modelo"),
        numericInput(
          ns("ancova_mu"),
          "Media general (μ):",
          value = 100, step = 1
        ),
        numericInput(
          ns("ancova_tau_amp"),
          "Amplitud de efectos de tratamiento (±τ):",
          value = 8, min = 0, step = 0.5
        ),
        numericInput(
          ns("ancova_beta_true"),
          "Pendiente verdadera de la covariable (β):",
          value = 1.5, step = 0.1
        ),
        numericInput(
          ns("ancova_mu_x"),
          "Media de la covariable X:",
          value = 50, step = 1
        ),
        numericInput(
          ns("ancova_sd_x"),
          "Desvío estándar de X (σ_X):",
          value = 5, min = 0.1, step = 0.5
        ),
        numericInput(
          ns("ancova_sigma_e"),
          "Desvío estándar residual (σ):",
          value = 5, min = 0.1, step = 0.5
        ),
        hr(),
        
        h5("3. (Opcional) Desbalancear X por tratamiento"),
        checkboxInput(
          ns("ancova_unbalance_X"),
          "Introducir medias de X distintas por tratamiento",
          value = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("ancova_unbalance_X")),
          helpText("Si se activa, cada tratamiento tendrá una media de X distinta."),
          sliderInput(
            ns("ancova_shift_X"),
            "Separación entre medias de X (en unidades de X):",
            min = 0, max = 10, value = 3, step = 0.5
          )
        ),
        hr(),
        
        actionButton(
          ns("btn_sim_ancova"),
          "Simular y ajustar modelos",
          class = "btn btn-success w-100 mt-2"
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          bslib::nav_panel(
            "4.1 ANOVA vs ANCOVA",
            h5("Comparación de modelos sin y con covariable"),
            verbatimTextOutput(ns("ancova_sim_results"))
          ),
          
          bslib::nav_panel(
            "4.2 Test de pendientes (Trat×X)",
            h5("Chequeo de homogeneidad de pendientes"),
            verbatimTextOutput(ns("ancova_slope_test"))
          ),
          
          bslib::nav_panel(
            "4.3 Gráfico Y vs X por tratamiento",
            h5("Rectas de regresión y datos simulados"),
            plotOutput(ns("ancova_sim_plot"), height = "360px")
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales (ANCOVA)
pestanna5_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo datos reales (ANCOVA)",
    
    h4(class = "section-header",
       "Checklist para aplicar ANCOVA a tu propio ensayo"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea práctica: "),
        "usar la covariable (X) para ",
        strong("reducir el ruido"),
        " y obtener comparaciones de tratamientos más precisas, ",
        "siempre que se respeten los supuestos del modelo."
      )
    ),
    
    # Paso 0 ---------------------------------------------------------------
    h5("Paso 0 – Entender el diseño y la covariable"),
    tags$ul(
      tags$li(
        strong("Variables mínimas en tu Excel:"),
        tags$ul(
          tags$li(code("trat"), " = tratamiento (factor: riego, variedad, fertilizante, etc.)."),
          tags$li(code("X_cov"), " = covariable numérica (ej.: vigor, biomasa inicial, humedad de suelo, etc.)."),
          tags$li(code("Y"), " = respuesta principal (ej.: rendimiento, firmeza, °Brix)."),
          tags$li(code("block"), " (opcional) = bloque o sector del campo, si el diseño es RCBD.")
        )
      ),
      tags$li(
        strong("Preguntas clave sobre la covariable:"),
        tags$ul(
          tags$li("¿La covariable se midió ", em("antes"), " de aplicar los tratamientos? (ideal)."),
          tags$li("¿La covariable ", strong("no fue manipulada"), " por el tratamiento? Es una característica del estado inicial o del ambiente."),
          tags$li(
            "¿La distribución de la covariable parece razonablemente similar entre tratamientos? ",
            "Si un tratamiento tiene sistemáticamente X mucho más alto/bajo, se complica la interpretación."
          )
        )
      ),
      tags$li(
        strong("Contexto del diseño:"),
        tags$ul(
          tags$li("CRD (diseño completamente al azar): no hay bloque, solo tratamientos."),
          tags$li("RCBD: hay ", code("block"), " y tratamientos asignados aleatoriamente dentro de cada bloque."),
          tags$li(
            "Otros diseños (split-plot, fila-columna, etc.): ",
            "la lógica de ANCOVA es similar, pero la estructura de error es más compleja."
          )
        )
      )
    ),
    
    hr(),
    
    # Paso 1 ---------------------------------------------------------------
    h5("Paso 1 – Explorar la relación entre Y, X y tratamientos"),
    tags$ul(
      tags$li(
        strong("Gráficos recomendados:"),
        tags$ul(
          tags$li(
            "Diagrama de dispersión ",
            code("Y vs X"),
            " coloreado por tratamiento: ",
            "permite ver si la relación entre Y y X es aproximadamente lineal."
          ),
          tags$li(
            "Boxplots o densidades de ", code("X"), " por tratamiento: ",
            "para ver si hay grandes diferencias en la distribución de la covariable entre tratamientos."
          )
        )
      ),
      tags$li(
        strong("Si hay bloques:"),
        tags$ul(
          tags$li(
            "Explora posibles gradientes de la covariable: por ejemplo, ",
            code("boxplot(X_cov ~ block)"),
            " o un gráfico espacial si tienes coordenadas."
          ),
          tags$li(
            "Si X cambia fuerte entre bloques, puede ser útil incluir ambos: ",
            "bloque y covariable en el modelo."
          )
        )
      ),
      tags$li(
        strong("Advertencia conceptual:"),
        " si la covariable está muy asociada al tratamiento (p.ej. un tratamiento tiene siempre X alto), ",
        "la ANCOVA deja de ser “ajuste fino” y se vuelve más difícil separar efecto de tratamiento y de covariable."
      )
    ),
    
    hr(),
    
    # Paso 2 ---------------------------------------------------------------
    h5("Paso 2 – Definir el modelo ANCOVA"),
    p("La forma del modelo depende del diseño base (CRD, RCBD, etc.)."),
    tags$ul(
      tags$li(
        strong("Caso CRD (sin bloque):"),
        tags$ul(
          tags$li(
            "Modelo ANOVA simple (sin covariable): ",
            code("Y ~ trat")
          ),
          tags$li(
            "Modelo ANCOVA: ",
            code("Y ~ trat + X_cov")
          )
        )
      ),
      tags$li(
        strong("Caso RCBD (con bloques fijos):"),
        tags$ul(
          tags$li(
            "Modelo ANOVA clásico: ",
            code("Y ~ block + trat")
          ),
          tags$li(
            "Modelo ANCOVA fijo: ",
            code("Y ~ block + trat + X_cov")
          )
        )
      ),
      tags$li(
        strong("Extensión a modelo mixto (LMM):"),
        tags$ul(
          tags$li(
            "Si consideras el bloque como aleatorio: ",
            code("Y ~ trat + X_cov + (1 | block)")
          ),
          tags$li(
            "Este enfoque es coherente con la lógica que ya viste en RCBD + LMM."
          )
        )
      ),
      tags$li(
        strong("Chequeo de homogeneidad de pendientes (opcional pero recomendable):"),
        tags$ul(
          tags$li(
            "Modelo con interacción: ",
            code("Y ~ block + trat * X_cov"),
            " o en LMM: ",
            code("Y ~ trat * X_cov + (1 | block)")
          ),
          tags$li(
            "Si el término ", code("trat:X_cov"), " NO es significativo: ",
            "las pendientes pueden tratarse como comunes (modelo ANCOVA clásico)."
          ),
          tags$li(
            "Si ", code("trat:X_cov"), " es significativo: ",
            "cada tratamiento tiene su propia pendiente; la interpretación se concentra en esas pendientes."
          )
        )
      )
    ),
    
    hr(),
    
    # Paso 3 ---------------------------------------------------------------
    h5("Paso 3 – Ajustar el modelo y validar supuestos"),
    tags$ul(
      tags$li(
        strong("Ajuste de modelos:"),
        tags$ul(
          tags$li("Ajusta primero el modelo sin covariable (ANOVA simple)."),
          tags$li("Luego ajusta el modelo con covariable (ANCOVA)."),
          tags$li("Compara la varianza residual (MS_residual) y los F/p-valores de tratamiento entre ambos.")
        )
      ),
      tags$li(
        strong("Diagnósticos recomendados:"),
        tags$ul(
          tags$li(
            "Gráfico de residuos vs predicciones: ",
            "debería verse sin patrones claros (nube sin forma)."
          ),
          tags$li(
            "QQ-plot de residuos: ",
            "para evaluar si la distribución residual es aproximadamente normal."
          ),
          tags$li(
            "Residuos vs X_cov: ",
            "para confirmar que, una vez ajustado el modelo, no queda estructura fuerte en función de la covariable."
          )
        )
      ),
      tags$li(
        strong("Interpretación de la pendiente β:"),
        tags$ul(
          tags$li(
            "β > 0: al aumentar la covariable, aumenta la respuesta (ej.: más vigor inicial → mayor rendimiento)."
          ),
          tags$li(
            "β < 0: al aumentar la covariable, disminuye la respuesta."
          ),
          tags$li(
            "Es útil reportar ", em("β̂"), " y su intervalo de confianza, no solo el p-valor."
          )
        )
      )
    ),
    
    hr(),
    
    # Paso 4 ---------------------------------------------------------------
    h5("Paso 4 – Comunicar resultados en lenguaje agronómico"),
    p(
      "La ANCOVA no se comunica solo con fórmulas; hay que traducir los resultados ",
      "a un mensaje claro para agrónomos, mejoradores o tomadores de decisiones."
    ),
    tags$ul(
      tags$li(
        strong("Elementos mínimos de un reporte:"),
        tags$ul(
          tags$li("Descripción del diseño (CRD/RCBD, nº de bloques, nº de tratamientos, nº de observaciones)."),
          tags$li("Definición de la covariable (qué mide, cuándo se midió)."),
          tags$li("Resumen de la relación entre Y y X (signo y magnitud de β̂)."),
          tags$li("Efecto de tratamiento ajustado por X (F, p, o comparaciones de medias ajustadas).")
        )
      ),
      tags$li(
        strong("Plantilla orientativa de redacción:"),
        div(
          class = "note-cloud",
          HTML(
            paste0(
              "<em>“Se evaluaron </em>N<em> tratamientos de riego en un diseño RCBD con </em>b<em> bloques. ",
              "Como covariable se registró el vigor inicial de las plantas (biomasa a T<sub>0</sub>). ",
              "El análisis de covarianza indicó un efecto significativo del tratamiento de riego sobre el rendimiento ",
              "ajustado por vigor inicial (F = …, p = …). ",
              "La covariable mostró una relación positiva con el rendimiento (</em>\\(\\hat{\\beta}\\)<em> = … kg/ha por unidad de biomasa, p = …), ",
              "reduciendo la varianza residual de … a …. ",
              "No se detectó interacción significativa Trat×Biomasa (p = …), por lo que se asumió una pendiente común entre tratamientos. ",
              "Las medias ajustadas sugieren que el tratamiento R2 supera a R1 y R3 en aproximadamente … kg/ha, ",
              "para un nivel promedio de vigor inicial.”</em>"
            )
          )
        )
      )
    ),
    
    br(),
    p(
      em("Sugerencia: "),
      "usa esta pestaña como guía cuando traigas tu propio archivo CSV a la sesión ",
      "o cuando prepares un informe técnico / tesis donde quieras justificar el uso de ANCOVA."
    )
  )
}

# Pestaña 6: Ejercicios prácticos & tareas (ANCOVA)
pestanna6_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios prácticos & tareas",
    
    h4(class = "section-header", "Ejercicios prácticos: poner en uso la ANCOVA"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Objetivo de esta pestaña: "),
        "que puedas practicar el uso de la ANCOVA con datos simulados (pestaña 4) ",
        "y luego trasladar el flujo a tus propios datos reales usando el protocolo de la pestaña 5."
      )
    ),
    
    h5("A) Ejercicios con datos simulados (usar pestaña 4: \"Simulación & efecto de la covariable\")"),
    tags$ol(
      tags$li(
        HTML(
          "<b>Comparar ANOVA vs ANCOVA.</b> 
          En la pestaña 4, simula un experimento con 2–3 tratamientos y una covariable X. 
          Luego, ajusta en R dos modelos:<br>
          &nbsp;&nbsp;&bull; ANOVA simple: <code>Y ~ trat</code><br>
          &nbsp;&nbsp;&bull; ANCOVA: <code>Y ~ trat + X</code><br>
          Compara los valores de F y p para el efecto de tratamiento, 
          y observa cómo cambia el cuadrado medio residual (MS_residual)."
        )
      ),
      tags$li(
        HTML(
          "<b>Cuánta variación explica la covariable.</b> 
          Con los mismos datos simulados, calcula o interpreta el porcentaje de varianza explicada por X. 
          Por ejemplo, compara el MS_residual del modelo sin covariable vs el modelo con covariable 
          (si MS_res_ANCOVA &lt; MS_res_ANOVA, la covariable está ayudando a explicar parte de la variación)."
        )
      ),
      tags$li(
        HTML(
          "<b>Explorar el efecto de la pendiente verdadera (β).</b> 
          En la pestaña 4, repite la simulación cambiando la pendiente verdadera de la covariable 
          (por ejemplo, β = 0, 0.5, 1.5, 3). 
          Para cada valor:<br>
          &nbsp;&nbsp;&bull; Ajusta ANOVA y ANCOVA.<br>
          &nbsp;&nbsp;&bull; Observa cómo cambia la significancia de Tratamiento.<br>
          &nbsp;&nbsp;&bull; Discute cuándo la ANCOVA aporta mucha ganancia de precisión 
          (cuando β es grande) y cuándo casi no ayuda (β cerca de 0)."
        )
      ),
      tags$li(
        HTML(
          "<b>Introducir un desequilibrio en la covariable X.</b> 
          Simula o modifica un conjunto de datos donde un tratamiento tenga, en promedio, 
          valores de X mucho mayores (o menores) que los otros. 
          Luego ajusta el modelo <code>Y ~ trat + X</code> y analiza:<br>
          &nbsp;&nbsp;&bull; ¿Cómo cambia la interpretación de las medias ajustadas por tratamiento?<br>
          &nbsp;&nbsp;&bull; ¿Qué riesgos hay cuando X no está bien balanceada entre tratamientos?<br>
          Relaciona esto con la idea de que, idealmente, X debe ser \"pre-tratamiento\" 
          y suficientemente independiente de la asignación de tratamientos."
        )
      )
    ),
    
    hr(),
    
    h5("B) Ejercicios con datos reales (tarea guiada, usar pestaña 5: \"Protocolo datos reales\")"),
    tags$ol(
      tags$li(
        HTML(
          "<b>Selecciona un caso real.</b> 
          Por ejemplo, un ensayo de riego, fertilización o manejo con:<br>
          &nbsp;&nbsp;&bull; un factor de tratamiento (trat),<br>
          &nbsp;&nbsp;&bull; una covariable medida al inicio (X, p.ej. vigor, biomasa, humedad inicial),<br>
          &nbsp;&nbsp;&bull; una respuesta Y (rendimiento, firmeza, °Brix, etc.),<br>
          &nbsp;&nbsp;&bull; opcionalmente bloques (RCBD)."
        )
      ),
      tags$li(
        HTML(
          "<b>Aplicar el protocolo de la pestaña 5.</b> 
          Siguiendo el checklist:<br>
          &nbsp;&nbsp;&bull; Explora Y vs X por tratamiento (diagramas de dispersión).<br>
          &nbsp;&nbsp;&bull; Revisa la distribución de X por tratamiento (boxplots).<br>
          &nbsp;&nbsp;&bull; Define el modelo adecuado (CRD: <code>Y ~ trat + X</code>; 
          RCBD: <code>Y ~ block + trat + X</code> o LMM <code>Y ~ trat + X + (1|block)</code>)."
        )
      ),
      tags$li(
        HTML(
          "<b>Comparar modelos en tus datos reales.</b> 
          Ajusta primero un modelo sin covariable (ANOVA) y luego un modelo con covariable (ANCOVA). 
          Compara:<br>
          &nbsp;&nbsp;&bull; El MS_residual y los residuos (¿mejoran?).<br>
          &nbsp;&nbsp;&bull; La F y el p-valor del efecto de tratamiento.<br>
          &nbsp;&nbsp;&bull; La estimación de β (pendiente de la covariable) y su significancia."
        )
      ),
      tags$li(
        HTML(
          "<b>Redacción agronómica.</b> 
          Fuera de la app (en un informe, RMarkdown o Quarto), redacta un mini-reporte 
          de 3–5 líneas que responda a:<br>
          &nbsp;&nbsp;&bull; ¿El tratamiento tiene efecto significativo sobre Y después de ajustar por X?<br>
          &nbsp;&nbsp;&bull; ¿La covariable X está asociada con Y (signo y magnitud de β)?<br>
          &nbsp;&nbsp;&bull; ¿Cómo cambian tus conclusiones si ignoras la covariable versus si la incluyes?"
        )
      ),
      tags$li(
        HTML(
          "<b>(Opcional) Extender a LMM.</b> 
          Si tu diseño tiene bloques o estructuras más complejas, intenta ajustar un modelo mixto 
          tipo <code>Y ~ trat + X + (1|block)</code> y compara las conclusiones con el modelo fijo. 
          Relaciona esto con lo visto en las sesiones de RCBD y diseños avanzados (Parte III)."
        )
      )
    ),
    
    br(),
    div(
      class = "alert alert-secondary",
      p(
        strong("Sugerencia: "),
        "usa esta pestaña como guía de trabajo autónomo. ",
        "La idea es que combines lo aprendido en las pestañas 3–5 (estructura de datos, simulación y protocolo) ",
        "para desarrollar tu propio flujo de análisis ANCOVA en R."
      )
    )
  )
}

# Pestaña Extra: Esquemas Visuales y Galería Conceptual (ANCOVA)
pestanna_extra_session9_v3UI <- function(ns) {
  
  # Definición de rutas (estándar del proyecto)
  base_path <- "images/sesiones/Diseños_estadisticos_V3/"
  img_path  <- paste0(base_path, "session9/")
  
  bslib::nav_panel(
    title = "Extra: Esquemas Visuales",
    icon  = icon("images"),
    
    tags$div(
      class = "container-fluid py-3",
      
      tags$h4(class = "text-primary mb-3", 
              "Galería Visual: Entendiendo la ANCOVA"),
      
      tags$p(
        class = "lead",
        "La ANCOVA es una técnica que combina la lógica de comparación de grupos (ANOVA) ",
        "con la predicción lineal (Regresión). Esta galería desglosa los conceptos mecánicos, ",
        "geométricos y de flujo de decisión."
      ),
      
      tags$hr(),
      
      bslib::navset_card_underline(
        
        # --- SUB-PESTAÑA A: Conceptos Fundamentales ---
        bslib::nav_panel(
          title = "A. Conceptos Fundamentales",
          
          # Fila 1: Evolución del Modelo (Tab 1)
          tags$div(
            class = "row mb-5 align-items-center",
            tags$div(
              class = "col-md-7",
              tags$img(src = paste0(img_path, "ancova_evolution_concept.png"),
                       class = "img-fluid shadow-sm border rounded",
                       style = "width: 100%;",
                       alt = "Evolución visual: ANOVA + Regresión = ANCOVA")
            ),
            tags$div(
              class = "col-md-5",
              tags$h5("1. La Fusión de Dos Mundos"),
              tags$p("Muchas veces enseñamos ANOVA y Regresión por separado. La ANCOVA es la intersección exacta."),
              tags$ul(
                tags$li(strong("ANOVA:"), " Mira diferencias entre grupos (medias)."),
                tags$li(strong("Regresión:"), " Mira tendencias continuas (pendientes)."),
                tags$li(strong("ANCOVA:"), " Mira diferencias entre grupos, asumiendo que comparten una tendencia común.")
              )
            )
          ),
          
          # Fila 2: La Línea de Tiempo (Tab 3)
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-5 order-md-2",
              tags$img(src = paste0(img_path, "ancova_covariate_timeline.png"),
                       class = "img-fluid shadow-sm border rounded",
                       style = "width: 100%;",
                       alt = "Línea de tiempo: La covariable debe ser medida antes del tratamiento")
            ),
            tags$div(
              class = "col-md-7 order-md-1",
              tags$h5("2. La Regla de Oro: 'Pre-Tratamiento'"),
              tags$p("Para que una ANCOVA sea válida, la covariable (X) no puede ser afectada por el tratamiento."),
              tags$div(class = "alert alert-warning",
                       strong("¡Cuidado!"), 
                       " Si mides X al final del experimento (junto con Y), corres el riesgo de ajustar por un efecto del tratamiento mismo, anulando tus resultados."
              )
            )
          )
        ),
        
        # --- SUB-PESTAÑA B: Mecánica Matemática ---
        bslib::nav_panel(
          title = "B. Mecánica Matemática",
          
          # Fila 3: Ajuste Geométrico (Tab 2)
          tags$div(
            class = "row mb-5 align-items-center",
            tags$div(
              class = "col-md-8 mx-auto text-center",
              tags$img(src = paste0(img_path, "ancova_geometric_adjustment.png"),
                       class = "img-fluid shadow border rounded",
                       style = "max-height: 450px;",
                       alt = "Geometría del ajuste de medias hacia la media global de X")
            ),
            tags$div(
              class = "col-md-12 mt-3",
              tags$h5("3. El Ajuste Geométrico de Medias"),
              tags$p("Esta es la esencia de la ANCOVA. No comparamos los promedios brutos (círculos vacíos)."),
              tags$p("Proyectamos cada grupo a lo largo de su pendiente común hasta la ", strong("Media Global de X"), ". Esas proyecciones (círculos llenos) son las 'Medias Ajustadas' o 'Medias Marginales Estimadas (EMMs)'. Así comparamos peras con peras.")
            )
          ),
          
          # Fila 4: Reducción de Ruido (Tab 4)
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-6",
              tags$img(src = paste0(img_path, "ancova_noise_reduction.png"),
                       class = "img-fluid shadow-sm border rounded",
                       style = "width: 100%;",
                       alt = "Visualización de la reducción de la varianza del error")
            ),
            tags$div(
              class = "col-md-6",
              tags$h5("4. El Filtro de Ruido"),
              tags$p("¿Por qué molestarse en medir una covariable?"),
              tags$p("Observa la reducción de la campana de error. Todo ese 'ancho' extra en la curva gris era variabilidad explicable por el vigor inicial. Al quitarla (curva azul), es mucho más fácil detectar diferencias pequeñas entre tratamientos significativos.")
            )
          )
        ),
        
        # --- SUB-PESTAÑA C: Protocolo y Práctica ---
        bslib::nav_panel(
          title = "C. Protocolo y Práctica",
          
          # Fila 5: Árbol de Decisión (Tab 5)
          tags$div(
            class = "row mb-5 align-items-center",
            tags$div(
              class = "col-md-12",
              tags$h5("5. Pipeline de Decisión"),
              tags$div(class = "d-flex justify-content-center",
                       tags$img(src = paste0(img_path, "ancova_decision_tree.png"),
                                class = "img-fluid border rounded mb-2",
                                style = "max-height: 400px;",
                                alt = "Árbol de decisión: Test de homogeneidad de pendientes")
              ),
              tags$p(class = "text-muted small text-center", 
                     "El paso crítico que a menudo se olvida: verificar la interacción Tratamiento x Covariable.")
            )
          ),
          
          # Fila 6: Precisión (Tab 6)
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-5",
              tags$h5("6. El Objetivo Final: Precisión"),
              tags$p("Al final del día, usamos ANCOVA para mejorar nuestra puntería."),
              tags$p("En experimentos agrícolas donde el suelo o el material vegetal es heterogéneo, la ANCOVA actúa como un estabilizador, permitiéndonos ver el efecto puro del tratamiento.")
            ),
            tags$div(
              class = "col-md-7",
              tags$img(src = paste0(img_path, "ancova_precision_target.png"),
                       class = "img-fluid shadow-lg border rounded",
                       style = "width: 100%;",
                       alt = "Metáfora de diana mostrando mayor precisión con ANCOVA")
            )
          )
        )
      )
    )
  )
}

# Pestaña 7: Referencias (ANCOVA)
pestanna7_session9_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias (ANCOVA)",
    
    h4(class = "section-header", "Referencias para profundizar en ANCOVA"),
    p(
      "Esta pestaña reúne libros y recursos que puedes consultar para estudiar la ",
      strong("ANálisis de COVARIAnza (ANCOVA)"),
      " en contexto de diseño experimental, modelos lineales y aplicaciones agronómicas."
    ),
    
    # ----------------------------------------------------------------------
    # 1) Diseño experimental y ANCOVA en agronomía
    # ----------------------------------------------------------------------
    h5("1) Diseño experimental y ANCOVA en agronomía"),
    tags$ul(
      tags$li(
        strong("Gómez, K. A. & Gómez, A. A. (1984). "),
        em("Statistical Procedures for Agricultural Research."),
        " Wiley.",
        br(),
        "Texto clásico en agronomía. Incluye capítulos de diseños clásicos (CRD, RCBD, factoriales) ",
        "y una sección específica de ",
        strong("análisis de covarianza"),
        " aplicada a ensayos de campo."
      ),
      tags$li(
        strong("Mead, R., Curnow, R. N. & Hasted, A. M. (2002). "),
        em("Statistical Methods in Agriculture and Experimental Biology."),
        " Chapman & Hall/CRC.",
        br(),
        "Aborda diseños experimentales en agricultura y biología, con discusión de ",
        "modelos lineales, ANCOVA y ejemplos con covariables en ensayos agronómicos."
      ),
      tags$li(
        strong("Steel, R. G. D., Torrie, J. H. & Dickey, D. A. (1997). "),
        em("Principles and Procedures of Statistics: A Biometrical Approach."),
        " McGraw-Hill.",
        br(),
        "Presenta ANCOVA dentro del marco de modelos lineales y su uso para ajustar ",
        "comparaciones de tratamientos por efectos de covariables."
      )
    ),
    
    # ----------------------------------------------------------------------
    # 2) Modelos lineales y modelos mixtos (GLM / LMM) con covariables
    # ----------------------------------------------------------------------
    h5("2) Modelos lineales y mixtos con covariables (GLM / LMM)"),
    tags$ul(
      tags$li(
        strong("Montgomery, D. C. (últimas ediciones). "),
        em("Design and Analysis of Experiments."),
        " Wiley.",
        br(),
        "Libro muy usado en cursos de diseño de experimentos. Presenta ANCOVA ",
        "como caso particular de modelos lineales, con ejemplos y comparación ",
        "con ANOVA y regresión."
      ),
      tags$li(
        strong("Kutner, M. H., Nachtsheim, C. J., Neter, J. & Li, W. (2005). "),
        em("Applied Linear Statistical Models."),
        " McGraw-Hill.",
        br(),
        "Marco general de modelos lineales: ANOVA, regresión múltiple y ANCOVA. ",
        "Incluye discusión de ",
        strong("homogeneidad de pendientes"),
        " y pruebas de interacción Trat×X."
      ),
      tags$li(
        strong("Rencher, A. C. & Schaalje, G. B. (2008). "),
        em("Linear Models in Statistics."),
        " Wiley.",
        br(),
        "Texto más teórico, útil para entender con detalle la formulación matricial ",
        "de ANOVA, ANCOVA y extensiones multivariadas."
      ),
      tags$li(
        strong("Stroup, W. W. (2013). "),
        em("Generalized Linear Mixed Models: Modern Concepts, Methods and Applications."),
        " CRC Press.",
        br(),
        "Aunque se centra en GLMM, discute cómo integrar covariables en ",
        strong("modelos mixtos"),
        " y cómo interpretar efectos fijos en presencia de estructuras aleatorias ",
        "como bloques o parcelas."
      ),
      tags$li(
        strong("Littell, R. C., Milliken, G. A., Stroup, W. W., Wolfinger, R. D. & Schabenberger, O. (2006). "),
        em("SAS for Mixed Models."),
        " SAS Institute.",
        br(),
        "Enfocado en SAS, pero muy útil para ver ejemplos de ",
        strong("ANCOVA en modelos mixtos"),
        " (bloques aleatorios, covariables y estructuras de correlación)."
      )
    ),
    
    # ----------------------------------------------------------------------
    # 3) Notas de cursos y recursos abiertos sobre ANCOVA
    # ----------------------------------------------------------------------
    h5("3) Notas de cursos y recursos abiertos sobre ANCOVA"),
    tags$ul(
      tags$li(
        "Apuntes de cursos universitarios de ",
        strong("Diseño Experimental / Estadística Agronómica"),
        " que incluyan secciones de ANCOVA. ",
        "Suelen cubrir: definición del modelo, homogeneidad de pendientes, ",
        "ejemplos con datos reales de campo y ejercicios guiados."
      ),
      tags$li(
        "Materiales de cursos de ",
        strong("Modelos Lineales"),
        " (por ejemplo, STAT 502 / STAT 512 en varias universidades), ",
        "donde se presenta ANCOVA como un ",
        em("modelo lineal con factor + covariable"),
        " y se enfatiza la comprobación de supuestos."
      ),
      tags$li(
        "Tutoriales de software (R, SAS, SPSS, etc.) sobre ANCOVA, ",
        "que muestran paso a paso cómo ajustar modelos del tipo ",
        code("Y ~ tratamiento + X"),
        " y cómo interpretar las salidas (tablas ANOVA, estimaciones de β, ",
        "pruebas de interacción Trat×X)."
      )
    ),
    
    # ----------------------------------------------------------------------
    # 4) Comentario final para el estudiante
    # ----------------------------------------------------------------------
    h5("4) Cómo usar estas referencias en la sesión"),
    tags$ul(
      tags$li(
        strong("Para la intuición agronómica: "),
        "prioriza textos como Gómez & Gómez o Mead et al., donde verás ejemplos ",
        "de ANCOVA en ensayos reales (fertilización, riego, variedades, etc.)."
      ),
      tags$li(
        strong("Para la parte matemática del modelo: "),
        "usa libros de modelos lineales (Kutner et al., Rencher & Schaalje) ",
        "para profundizar en la formulación y los supuestos."
      ),
      tags$li(
        strong("Para diseños complejos y LMM: "),
        "consulta Stroup o Littell et al. para entender cómo integrar covariables ",
        "dentro de un marco de modelos mixtos con bloques, parcelas, etc."
      )
    )
  )
}

# -------------------------------------------------------------------------
# Main UI Sesión 9
# -------------------------------------------------------------------------

session9_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 9: ANCOVA")
    ),
    navset_tab(
      pestanna1_session9_v3UI(ns),
      pestanna2_session9_v3UI(ns),
      pestanna3_session9_v3UI(ns),
      pestanna4_session9_v3UI(ns),
      pestanna5_session9_v3UI(ns),
      pestanna6_session9_v3UI(ns),
      pestanna_extra_session9_v3UI(ns),
      pestanna7_session9_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server per Tab Sesión 9
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (ANCOVA)
pestanna1_session9_v3_server <- function(input, output, session) {
  # Solo texto estático; no se requiere lógica de servidor en esta pestaña.
}

# Pestaña 2: Conceptos clave & modelo ANCOVA
pestanna2_session9_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor para esta pestaña teórica
}

# Pestaña 3: Diseño & estructura de datos para ANCOVA
pestanna3_session9_v3_server <- function(input, output, session) {
  
  # ------------------------------------------------------------------
  # 1) Dataset simulado con estructura típica para ANCOVA
  #    - 3 tratamientos de riego
  #    - 4 bloques
  #    - 1 observación por combinación bloque × tratamiento
  # ------------------------------------------------------------------
  ancova_data <- reactive({
    set.seed(123)  # para reproducibilidad en la sesión
    
    # Definimos factores
    bloques <- factor(1:4)
    tratamientos <- factor(c("Riego_bajo", "Riego_medio", "Riego_alto"),
                           levels = c("Riego_bajo", "Riego_medio", "Riego_alto"))
    
    # Expandimos todas las combinaciones bloque × tratamiento
    diseño <- expand.grid(
      block = bloques,
      trat  = tratamientos
    )
    
    # Covariable: vigor inicial (por ejemplo, índice de biomasa)
    # Permitimos pequeñas diferencias por bloque y ruido individual.
    efecto_bloque_X <- rnorm(length(bloques), mean = 0, sd = 2)
    names(efecto_bloque_X) <- levels(bloques)
    
    n <- nrow(diseño)
    X_cov <- 50 +                       # nivel promedio de vigor
      efecto_bloque_X[as.character(diseño$block)] +
      rnorm(n, mean = 0, sd = 3)        # ruido entre plantas
    
    # Efectos de tratamiento sobre la respuesta Y (rendimiento)
    efecto_trat <- c(
      "Riego_bajo"  = -600,   # rinde algo menos
      "Riego_medio" =    0,   # referencia
      "Riego_alto"  =  500    # rinde más
    )
    
    # Efecto de bloque sobre Y (por ejemplo, gradiente de suelo)
    efecto_bloque_Y <- rnorm(length(bloques), mean = 0, sd = 300)
    names(efecto_bloque_Y) <- levels(bloques)
    
    # Construimos Y con:
    # - media base 9000 kg/ha
    # - efecto de tratamiento
    # - efecto de covariable (pendiente positiva)
    # - efecto de bloque
    # - error residual
    beta_X <- 80  # incremento (aprox) en kg/ha por unidad de X_cov
    media_global <- 9000
    
    Y <- media_global +
      efecto_trat[as.character(diseño$trat)] +
      beta_X * (X_cov - mean(X_cov)) +
      efecto_bloque_Y[as.character(diseño$block)] +
      rnorm(n, mean = 0, sd = 250)  # error residual
    
    data.frame(
      block = diseño$block,
      trat  = diseño$trat,
      X_cov = round(X_cov, 1),
      Y     = round(Y, 0)
    )
  })
  
  # ------------------------------------------------------------------
  # 2) Tabla de datos
  # ------------------------------------------------------------------
  output$ancova_data_table <- DT::renderDataTable({
    df <- ancova_data()
    DT::datatable(
      df,
      options = list(pageLength = 6, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # ------------------------------------------------------------------
  # 3) Exploración 1: Dispersión Y vs X_cov por tratamiento
  # ------------------------------------------------------------------
  output$ancova_scatter <- renderPlot({
    df <- ancova_data()
    
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = X_cov, y = Y, colour = trat, shape = block)
    ) +
      ggplot2::geom_point(size = 3) +
      ggplot2::labs(
        x = "Covariable X_cov (por ejemplo, vigor inicial)",
        y = "Respuesta Y (por ejemplo, rendimiento kg/ha)",
        colour = "Tratamiento",
        shape = "Bloque",
        title = "Relación Y vs X_cov por tratamiento y bloque"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  # ------------------------------------------------------------------
  # 4) Exploración 2: Distribución de X_cov por tratamiento
  # ------------------------------------------------------------------
  output$ancova_boxplot <- renderPlot({
    df <- ancova_data()
    
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = trat, y = X_cov, fill = trat)
    ) +
      ggplot2::geom_boxplot(alpha = 0.7) +
      ggplot2::geom_jitter(
        width = 0.1,
        alpha = 0.7,
        size = 2,
        colour = "black"
      ) +
      ggplot2::labs(
        x = "Tratamiento",
        y = "Covariable X_cov",
        fill = "Tratamiento",
        title = "Distribución de la covariable X_cov por tratamiento"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  })
}

# Pestaña 4: Simulación & análisis (ANCOVA)
pestanna4_session9_v3_server <- function(input, output, session) {
  ns <- session$ns
  
  # -------------------------------------------------------------------
  # 1) Simulación de datos: evento central
  # -------------------------------------------------------------------
  sim_ancova <- eventReactive(input$btn_sim_ancova, {
    set.seed(123)  # si quieres, luego puedes exponer la semilla en la UI
    
    k      <- input$ancova_k_trat
    n_per  <- input$ancova_n_per_trat
    mu     <- input$ancova_mu
    tauAmp <- input$ancova_tau_amp
    beta   <- input$ancova_beta_true
    mu_x   <- input$ancova_mu_x
    sd_x   <- input$ancova_sd_x
    sd_e   <- input$ancova_sigma_e
    
    # Niveles de tratamiento
    trat_levels <- paste0("T", seq_len(k))
    
    # Efectos verdaderos de tratamiento ~ Uniform(-tauAmp, tauAmp)
    tau_vec <- stats::runif(k, -tauAmp, tauAmp)
    names(tau_vec) <- trat_levels
    
    # Construir data.frame (CRD): n_per observaciones por tratamiento
    # Una fila = una unidad experimental
    df_list <- lapply(seq_along(trat_levels), function(i) {
      tr_i <- trat_levels[i]
      
      # Media de X para este tratamiento
      if (!isTRUE(input$ancova_unbalance_X)) {
        mean_x_i <- mu_x
      } else {
        # Secuencia centrada alrededor de mu_x, separadas por ancova_shift_X
        shift <- input$ancova_shift_X
        # i va de 1 a k. Centramos alrededor de (k+1)/2 para equilibrar.
        offset <- (i - (k + 1) / 2) * shift
        mean_x_i <- mu_x + offset
      }
      
      X_i <- stats::rnorm(n_per, mean = mean_x_i, sd = sd_x)
      
      data.frame(
        trat = tr_i,
        X    = X_i,
        stringsAsFactors = FALSE
      )
    })
    
    df <- do.call(rbind, df_list)
    df$trat <- factor(df$trat, levels = trat_levels)
    
    # Centrar la covariable X en la media global (X - X̄..)
    X_centered <- df$X - mean(df$X, na.rm = TRUE)
    
    # Error aleatorio
    error <- stats::rnorm(nrow(df), mean = 0, sd = sd_e)
    
    # Generar Y de acuerdo al modelo verdadero
    df$Y <- mu + tau_vec[as.character(df$trat)] + beta * X_centered + error
    
    # Devolvemos datos + parámetros verdaderos
    list(
      data   = df,
      params = list(
        mu   = mu,
        tau  = tau_vec,
        beta = beta,
        mu_x = mu_x,
        sd_x = sd_x,
        sd_e = sd_e
      )
    )
  })
  
  # -------------------------------------------------------------------
  # 2) Comparación ANOVA vs ANCOVA (panel 4.1)
  # -------------------------------------------------------------------
  output$ancova_sim_results <- renderPrint({
    sim <- sim_ancova()
    shiny::req(sim)
    df <- sim$data
    par <- sim$params
    
    # Modelo 1: ANOVA simple (sin covariable)
    mod_aov  <- stats::lm(Y ~ trat, data = df)
    
    # Modelo 2: ANCOVA aditiva (trat + X)
    mod_anc  <- stats::lm(Y ~ trat + X, data = df)
    
    # MS residual (ANOVA vs ANCOVA)
    ms_resid_aov <- sum(stats::residuals(mod_aov)^2) / stats::df.residual(mod_aov)
    ms_resid_anc <- sum(stats::residuals(mod_anc)^2) / stats::df.residual(mod_anc)
    
    cat("== Datos simulados (resumen) ==\n")
    cat("N° tratamientos:", length(levels(df$trat)), "\n")
    cat("N° observaciones totales:", nrow(df), "\n\n")
    
    cat("Parámetros verdaderos:\n")
    cat("  μ      =", par$mu, "\n")
    cat("  β (true) =", par$beta, "\n")
    cat("  σ_X    =", par$sd_x, " | σ (error) =", par$sd_e, "\n\n")
    
    cat("== Modelo 1: ANOVA sin covariable (Y ~ trat) ==\n")
    print(anova(mod_aov))
    cat("\nMS_residual (modelo 1):", round(ms_resid_aov, 3), "\n\n")
    
    cat("== Modelo 2: ANCOVA aditiva (Y ~ trat + X) ==\n")
    print(anova(mod_anc))
    cat("\nMS_residual (modelo 2):", round(ms_resid_anc, 3), "\n")
    cat("Reducción relativa de MS_residual:",
        round(100 * (ms_resid_aov - ms_resid_anc) / ms_resid_aov, 1), "%\n\n")
    
    # Estimación de β y su intervalo de confianza
    coefs <- stats::coef(mod_anc)
    if (!("X" %in% names(coefs))) {
      cat("Advertencia: no se encontró el coeficiente 'X' en el modelo (algo salió mal).\n")
      return(invisible(NULL))
    }
    
    beta_hat <- coefs["X"]
    ci_beta  <- stats::confint(mod_anc, "X", level = 0.95)
    
    cat("Estimación de la pendiente β (modelo 2):\n")
    cat("  β_hat =", round(beta_hat, 3),
        " | IC 95% = [", round(ci_beta[1], 3), ",", round(ci_beta[2], 3), "]\n")
    cat("  β verdadero (usado en la simulación) =", par$beta, "\n")
  })
  
  # -------------------------------------------------------------------
  # 3) Test de homogeneidad de pendientes (panel 4.2)
  # -------------------------------------------------------------------
  output$ancova_slope_test <- renderPrint({
    sim <- sim_ancova()
    shiny::req(sim)
    df <- sim$data
    
    # Modelo aditivo (pendiente común)
    mod_add <- stats::lm(Y ~ trat + X, data = df)
    
    # Modelo con interacción trat:X (pendientes específicas por tratamiento)
    mod_int <- stats::lm(Y ~ trat * X, data = df)
    
    cat("== Comparación de modelos para chequear homogeneidad de pendientes ==\n")
    cat("Modelo reducido (pendiente común):  Y ~ trat + X\n")
    cat("Modelo completo (pendientes específicas):  Y ~ trat * X\n\n")
    
    comp <- anova(mod_add, mod_int)
    print(comp)
    
    cat("\nInterpretación didáctica:\n")
    cat("- Si el p-valor del test (fila 2) es grande, no hay evidencia fuerte de que las pendientes difieran entre tratamientos.\n")
    cat("- Si el p-valor es pequeño, las pendientes parecen distintas y conviene interpretar un modelo con interacción (pendientes específicas por tratamiento).\n")
  })
  
  # -------------------------------------------------------------------
  # 4) Gráfico Y vs X por tratamiento (panel 4.3)
  # -------------------------------------------------------------------
  output$ancova_sim_plot <- renderPlot({
    sim <- sim_ancova()
    shiny::req(sim)
    df <- sim$data
    
    # Ajustamos el modelo aditivo para dibujar rectas paralelas (pendiente común)
    mod_add <- stats::lm(Y ~ trat + X, data = df)
    
    # Rango de X para dibujar las rectas
    x_range <- range(df$X, na.rm = TRUE)
    x_seq   <- seq(x_range[1], x_range[2], length.out = 50)
    
    # Construimos un data.frame con predicciones por tratamiento
    trat_levels <- levels(df$trat)
    pred_list <- lapply(trat_levels, function(tr) {
      newdat <- data.frame(
        trat = factor(tr, levels = trat_levels),
        X    = x_seq
      )
      newdat$Y_hat <- stats::predict(mod_add, newdata = newdat)
      newdat
    })
    pred_df <- do.call(rbind, pred_list)
    
    ggplot2::ggplot() +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(x = X, y = Y, colour = trat),
        alpha = 0.6
      ) +
      ggplot2::geom_line(
        data = pred_df,
        ggplot2::aes(x = X, y = Y_hat, colour = trat),
        linewidth = 1
      ) +
      ggplot2::labs(
        x = "Covariable X",
        y = "Respuesta Y",
        colour = "Tratamiento",
        title = "Rectas de ANCOVA (pendiente común) por tratamiento"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
}

# Pestaña 5: Protocolo para datos reales (ANCOVA)
pestanna5_session9_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor para esta pestaña:
  # todo el contenido es textual / pedagógico.
}

# Pestaña 6: Ejercicios prácticos & tareas (ANCOVA)
pestanna6_session9_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor para esta pestaña:
  # todo el contenido es instructivo / estático.
}

# Pestaña 7: Referencias (ANCOVA)
pestanna7_session9_v3_server <- function(input, output, session) {
  # Pestaña solo informativa: no se requiere lógica de servidor
}

# -------------------------------------------------------------------------
# Main Server Sesión 9
# -------------------------------------------------------------------------

session9_v3Server <- function(input, output, session) {
  
  pestanna1_session9_v3_server(input, output, session)
  pestanna2_session9_v3_server(input, output, session)
  pestanna3_session9_v3_server(input, output, session)
  pestanna4_session9_v3_server(input, output, session)
  pestanna5_session9_v3_server(input, output, session)
  pestanna6_session9_v3_server(input, output, session)
  pestanna7_session9_v3_server(input, output, session)
}
