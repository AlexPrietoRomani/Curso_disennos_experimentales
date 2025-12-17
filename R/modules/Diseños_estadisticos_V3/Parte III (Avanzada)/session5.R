# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session5.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & Contexto
pestanna1_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto",
    
    # -------------------------
    # Bloque 1: ¿De dónde venimos?
    # -------------------------
    tags$div(
      class = "mb-3",
      tags$h4(class = "section-header", "Punto de partida: de RCBD + LMM a split-plot"),
      tags$p(
        "En la ", strong("Sesión 4"), " trabajaste con ",
        strong("Diseños en Bloques Completos al Azar (RCBD)"),
        " y modelos mixtos del tipo ",
        code("rend ~ tratamiento + (1|bloque)"), "."
      ),
      tags$ul(
        tags$li("Identificaste ", strong("tratamientos"), " como efectos fijos y ", strong("bloque"), " como efecto aleatorio."),
        tags$li("Usaste ", code("lmer()"), " / ", code("lmerTest"), " para manejar desbalance y separar varianza de bloque y residual."),
        tags$li("Interpretaste ", strong("VarCorr"), " e ", strong("ICC"), " para cuantificar la importancia del bloqueo.")
      ),
      tags$p(
        "En esta sesión damos un paso más: aparece un ",
        strong("factor logístico aplicado a parcelas grandes (parcela principal)"),
        " y otro factor aplicado a subparcelas. Esto es lo que da origen a los ",
        strong("diseños de parcelas divididas (split-plot).")
      )
    ),
    
    # -------------------------
    # Bloque 2: Objetivo y mapa de la sesión
    # -------------------------
    tags$div(
      class = "mb-3",
      tags$h4(class = "section-header", "Objetivo y mapa de la sesión"),
      tags$ul(
        tags$li(
          strong("Contexto:"),
          " factores “difíciles de cambiar” (por ejemplo, sistemas de riego) se asignan a ",
          strong("parcelas principales"), 
          ", y factores “fáciles” (por ejemplo, dosis de N) se asignan a ",
          strong("subparcelas"),
          ". La aleatorización se hace en dos etapas y aparecen ",
          strong("dos errores experimentales"),
          " (Error A y Error B)."
        ),
        tags$li(
          strong("Meta de aprendizaje:"),
          " construir el ", code("LMM"), " adecuado para un split-plot y ",
          "evitar inflar el ", strong("Error Tipo I"),
          " para el factor de parcela principal (A)."
        ),
        tags$li(
          strong("Entrega esperada:"),
          " un script reproducible en R con comparación entre ",
          code("aov()"), " (GLM y ANOVA estratificado) y ",
          code("lmer()"), ", incluyendo componentes de varianza y análisis post-hoc con ",
          code("emmeans()"), "."
        )
      ),
      div(
        class = "alert alert-info",
        tags$p(
          "Los diseños ", strong("split-plot"), " son muy comunes en campo, pero con frecuencia se analizan como si fueran ",
          "un RCBD simple. El resultado típico es usar un único “Residual” para todo y calcular mal el F del factor principal. ",
          "Modelar explícitamente ", em("Bloque"), " y la ", em("Parcela principal"), 
          " como efectos aleatorios en un ", strong("LMM"), " es la ruta robusta."
        ),
        tags$p(
          class = "small text-muted mb-0",
          "Más adelante verás cómo el modelo mixto refleja naturalmente los dos niveles de unidad experimental ",
          "(parcela principal y subparcela)."
        )
      )
    ),
    
    # -------------------------
    # Bloque 3: Ejemplo ancla (Riego × Nitrógeno)
    # -------------------------
    tags$div(
      class = "mb-3",
      tags$h4("Ejemplo motivador que usaremos en toda la sesión"),
      tags$p(
        "Para fijar ideas trabajaremos con un ensayo hipotético de ",
        strong("Riego (A) × Nitrógeno (B)"),
        " en un cultivo perenne."
      ),
      tags$ol(
        tags$li(
          "En cada ", strong("bloque"), " se definen grandes parcelas (camas/lotes) a las que se asigna el factor ",
          strong("Riego (A)"), " (por ejemplo: riego bajo, medio, alto). Estas son las ",
          strong("parcelas principales"),
          "."
        ),
        tags$li(
          "Cada parcela principal se subdivide en varias ", strong("subparcelas"), 
          " donde se asignan los niveles del factor ",
          strong("Nitrógeno (B)"), " (por ejemplo, 0, 50, 100, 150 kg N/ha)."
        ),
        tags$li(
          "El resultado es que hay dos niveles de variación: ",
          strong("entre parcelas principales"), " (Error A, ligado al factor A) y ",
          strong("entre subparcelas dentro de cada parcela principal"), " (Error B, ligado a B y a la interacción A×B)."
        )
      ),
      tags$p(
        class = "text-muted small",
        "Toda la sesión usará esta estructura como ejemplo ancla: A = Riego (parcela principal), B = Nitrógeno (subparcela). ",
        "Los mismos principios se aplican a otros casos (p.ej. maquinaria × fertilización, labranza × densidad, etc.)."
      )
    ),
    
    # -------------------------
    # Bloque 4: Resultados de aprendizaje (learning outcomes)
    # -------------------------
    bslib::card(
      bslib::card_header("Resultados de aprendizaje de la sesión"),
      tags$p(
        "Al finalizar la sesión deberías ser capaz de:"
      ),
      tags$ul(
        tags$li(
          strong("1) Identificar las unidades experimentales A y B:"),
          " reconocer qué es la ", strong("parcela principal (UE A)"),
          " y qué es la ", strong("subparcela (UE B)"),
          " en un diseño split-plot típico de campo."
        ),
        tags$li(
          strong("2) Escribir la estructura aleatoria de un split-plot:"),
          " formular en R la parte aleatoria de un modelo mixto del tipo ",
          code("(1|Bloque) + (1|Bloque:A)"),
          " para reflejar el Error A (entre parcelas principales) y el Error B (entre subparcelas)."
        ),
        tags$li(
          strong("3) Comparar enfoques de modelado en R:"),
          " ver en un ejemplo simulado cómo cambian los F y p-valores al pasar de un ",
          strong("GLM simple"), " a un ",
          strong("ANOVA estratificado"), " (", code("aov(Error(Bloque/ParcelaA))"), ") y finalmente a un ",
          strong("LMM correctamente especificado"), " con ", code("lmer()"), "."
        )
      ),
      tags$p(
        class = "small text-muted",
        "Los conceptos de esta pestaña se conectan directamente con: ",
        strong("P2"), " (dos unidades experimentales y dos errores), ",
        strong("P3"), " (comparación GLM vs ANOVA estratificado vs LMM en datos simulados) y ",
        strong("P5"), " (ejercicios prácticos y script reproducible)."
      )
    ),
    
    # -------------------------
    # Notas de referencia
    # -------------------------
    tags$hr(),
    tags$p(
      class = "text-muted small",
      "Notas de apoyo: materiales de split-plot (ETH Zürich), artículos sobre modelos mixtos en agricultura ",
      "y documentación de paquetes ", code("lme4"), ", ", code("lmerTest"), " y ", code("emmeans"), "."
    )
  )
}

# Pestaña 2: Conceptos clave — UE & errores
pestanna2_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave: UE & errores",
    
    # Encabezado general
    tags$div(class = "mb-3",
      h4(class = "section-header", "Dos unidades experimentales → dos errores"),
      tags$p(
        "En un diseño split-plot clásico (por ejemplo, Riego × Nitrógeno):"
      ),
      tags$ul(
        tags$li(
          strong("UE A (Parcela principal): "),
          "unidad grande donde se aplica el factor principal (Riego, A). ",
          strong("Error A: "),
          "variación entre parcelas principales dentro de cada bloque."
        ),
        tags$li(
          strong("UE B (Subparcela): "),
          "unidad más pequeña donde se aplica el factor secundario (Nitrógeno, B). ",
          strong("Error B: "),
          "variación entre subparcelas dentro de una misma parcela principal."
        )
      ),
      tags$p(
        class = "small text-muted",
        "Si mezclamos estas dos unidades experimentales en un único “Residual”, ",
        "terminamos usando un denominador de F demasiado pequeño para A ",
        "(parcela principal) y se infla el Error Tipo I."
      )
    ),
    
    # Bloque 1: Dibujo mental / mapa del split-plot
    bslib::card(
      bslib::card_header("Dibujo mental: bloques, parcelas principales y subparcelas"),
      fluidRow(
        column(
          width = 7,
          tags$p(
            "Imagina cada bloque como una franja del campo. En cada bloque:"
          ),
          tags$ul(
            tags$li("Pocas ", strong("parcelas principales (A)"), " grandes, una por nivel de Riego."),
            tags$li(
              "Cada parcela principal se subdivide en varias ",
              strong("subparcelas (B)"),
              " con distintas dosis de Nitrógeno."
            ),
            tags$li(
              "La misma combinación A×B se repite en todos los bloques, ",
              "pero con diferentes valores de la respuesta (rendimiento)."
            )
          ),
          tags$div(
            class = "alert alert-info small",
            strong("Resumen para recordar: "),
            "dos unidades experimentales (A y B) → ",
            "dos errores distintos (Error A entre parcelas principales, ",
            "Error B entre subparcelas)."
          )
        ),
        column(
          width = 5,
          tags$p(class = "small text-muted mb-1", "Esquema conceptual (no a escala):"),
          tags$table(
            class = "table table-sm table-bordered text-center",
            tags$thead(
              tags$tr(
                tags$th("Bloque"),
                tags$th("Parcelas A (Riego)"),
                tags$th("Subparcelas B (Nitrógeno)")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td("B1"),
                tags$td("A1, A2, A3"),
                tags$td("Dentro de cada Ai: N1, N2, N3, N4")
              ),
              tags$tr(
                tags$td("B2"),
                tags$td("A1, A2, A3"),
                tags$td("Dentro de cada Ai: N1, N2, N3, N4")
              ),
              tags$tr(
                tags$td("B3"),
                tags$td("A1, A2, A3"),
                tags$td("…")
              )
            )
          ),
          tags$p(
            class = "small text-muted",
            "La clave: A se replica a nivel de parcela principal (una vez por bloque), ",
            "B se replica dentro de cada parcela A (varias subparcelas)."
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Bloque 2: Matriz de denominadores F
    bslib::card(
      bslib::card_header("¿Con qué denominador F se prueba cada efecto?"),
      fluidRow(
        column(
          width = 7,
          tags$p(
            "En un split-plot clásico Riego (A) × Nitrógeno (B), las fuentes de variación básicas son:"
          ),
          tags$ul(
            tags$li("Bloque"),
            tags$li("Parcela principal (A dentro de bloque) → ", strong("Error A")),
            tags$li("Subparcela (B)"),
            tags$li("Interacción A×B"),
            tags$li("Residual dentro de subparcela → ", strong("Error B"))
          ),
          tags$p(
            "La tabla siguiente resume, de forma pedagógica, qué ",
            em("cuadrado medio"),
            " se usa como denominador para cada efecto en tres enfoques:"
          ),
          tags$ul(
            tags$li(strong("GLM simple (incorrecto):"), " todo prueba con CM(Residual)."),
            tags$li(strong("ANOVA estratificado:"), " ", code("aov(..., Error(Bloque/ParcelaA))"), "."),
            tags$li(
              strong("LMM (lmer):"),
              " estima las varianzas de Bloque y Bloque:A y construye internamente ",
              "F equivalentes a los del ANOVA estratificado."
            )
          ),
          tags$p(class = "small text-muted mb-0",
            "Idea clave: el problema no es el split-plot, sino usar un modelo que no respete sus dos niveles de error."
          )
        ),
        column(
          width = 5,
          h5("Matriz de denominadores F (pedagógica)"),
          tableOutput(ns("tabla_denominadores_F")),
          tags$div(
            class = "alert alert-warning small mt-2",
            tags$strong("Error GLM clásico: "),
            "si usamos un único 'Residual' como denominador para A, B y A×B, ",
            "el F de A (parcela principal) se calcula con un denominador demasiado pequeño ",
            "→ se inflan los p-valores significativos (Error Tipo I). ",
            "Los modelos mixtos corrigen esto al separar Error A y Error B."
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Bloque 3: Fijos vs aleatorios en el split-plot ancla
    bslib::card(
      bslib::card_header("Espacio de inferencia: fijos y aleatorios en el split-plot Riego × N"),
      tags$p(
        "En el ejemplo ancla de toda la sesión trabajaremos con:"
      ),
      tags$ul(
        tags$li(
          strong("Efectos fijos (tratamientos científicos):"),
          " Riego (A: bajo, medio, alto) y Nitrógeno (B: 0, 50, 100, 150 kg N/ha)."
        ),
        tags$li(
          strong("Efectos aleatorios (logística / variación de campo):"),
          " Bloque (franjas del campo) y ParcelaA = Bloque:A (parcelas principales)."
        )
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
'## Esqueleto del modelo mixto para el split-plot Riego × N
lmer(
  y ~ A * B +
    (1 | Bloque) +      # variación entre bloques
    (1 | Bloque:A),     # variación entre parcelas principales (Error A)
  data = datos,
  REML = TRUE
)'
      ),
      tags$div(
        class = "alert alert-info small",
        tags$strong("Para memorizar: "),
        tags$ul(
          tags$li("UE A → Parcela principal → Error A → componente aleatorio (1 | Bloque:A)."),
          tags$li("UE B → Subparcela → Error B (residual)."),
          tags$li("A y B son fijos; Bloque y Bloque:A son aleatorios.")
        )
      ),
      tags$p(
        class = "small text-muted",
        "Si puedes responder rápidamente: ",
        em("¿Cuáles son las dos unidades experimentales? ¿Cuál es Error A? ¿Cuál es Error B?"),
        " ya tienes el 80% del diseño conceptual de un split-plot."
      )
    ),
    
    tags$p(
      class = "text-muted small mt-3",
      "Apoyos sugeridos para profundizar: Yang (2010, mixed-model analysis en agricultura); ",
      "Stroup (GLMM, 2ª ed.); notas de ETH Zürich y recursos de split-plot en R."
    )
  )
}

# Pestaña 3: Split-plot balanceado — GLM vs ANOVA estratificado vs LMM
pestanna3_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Split-plot balanceado: GLM vs ANOVA vs LMM",
    
    # Encabezado didáctico
    tags$div(
      class = "mb-3",
      h4(class = "section-header", "Comparación de enfoques en un split-plot balanceado"),
      tags$p(
        "En esta pestaña trabajaremos con un diseño de parcelas divididas (split-plot) ",
        strong("perfectamente balanceado"),
        ": todos los bloques tienen las mismas combinaciones de Riego (A, parcela principal) ",
        "y Nitrógeno (B, subparcela), sin datos faltantes."
      ),
      tags$p(
        strong("Objetivo:"),
        " comparar tres enfoques de análisis sobre los mismos datos simulados y ver qué ocurre con los ",
        strong("efectos A, B y A×B"),
        ":"
      ),
      tags$ul(
        tags$li(code("GLM simple: aov(y ~ Bloque + A*B)"), " — usa un único Residual para todo."),
        tags$li(code("ANOVA estratificado: aov(y ~ A*B + Error(Bloque/ParcelaA))"),
                " — separa Error A (parcela principal) y Error B (subparcela)."),
        tags$li(code("LMM: lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A))"),
                " — modelo lineal mixto que traduce esos errores en componentes de varianza.")
      ),
      tags$p(
        class = "small text-muted",
        "Mensaje que queremos ver: en el caso ",
        strong("balanceado"),
        ", el ANOVA estratificado y el LMM dan esencialmente ",
        "el mismo mensaje para A, B y A×B; el GLM simple puede empezar a fallar para A ",
        "si hay varianza importante en el Error A."
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Parámetros de simulación (caso balanceado)"),
        tags$p(
          class = "small text-muted",
          "Simularemos un split-plot balanceado Riego (A, parcela principal) × Nitrógeno (B, subparcela) ",
          "con la misma estructura en todos los bloques."
        ),
        selectInput(
          ns("n_bloques"),
          "Bloques (réplicas de A):",
          choices  = c(3, 4, 5, 6),
          selected = 4
        ),
        selectInput(
          ns("niv_A"),
          "Niveles de Riego (A, parcela principal):",
          choices  = c(2, 3),
          selected = 3
        ),
        selectInput(
          ns("niv_B"),
          "Niveles de Nitrógeno (B, subparcela):",
          choices  = c(3, 4),
          selected = 4
        ),
        tags$hr(),
        h6("Magnitud de efectos (en unidades de respuesta)"),
        sliderInput(
          ns("eff_A"),
          "Rango efecto A (max – min):",
          min = 0, max = 10, value = 4, step = 0.5
        ),
        sliderInput(
          ns("eff_B"),
          "Rango efecto B (max – min):",
          min = 0, max = 12, value = 6, step = 0.5
        ),
        sliderInput(
          ns("eff_AB"),
          "Rango interacción A×B:",
          min = -6, max = 6, value = 2, step = 0.5
        ),
        tags$hr(),
        h6("Componentes de error (desviaciones estándar)"),
        sliderInput(
          ns("sd_A"),
          "σ_A (Error parcela principal):",
          min = 0, max = 10, value = 4, step = 0.5
        ),
        sliderInput(
          ns("sd_B"),
          "σ_B (Error subparcela / residual):",
          min = 0, max = 10, value = 2, step = 0.5
        ),
        tags$hr(),
        h6("Desbalance (faltantes)"),
        tags$p(
          class = "small text-muted mb-1",
          strong("En esta pestaña trabajamos exclusivamente el caso balanceado."),
          " Fijamos la proporción de subparcelas faltantes en 0%."
        ),
        # Mantener el input por compatibilidad con session5_v3Server,
        # pero fijado a 0 (sin desbalance).
        sliderInput(
          ns("prop_miss"),
          "Proporción de subparcelas faltantes:",
          min = 0, max = 0, value = 0, step = 0
        ),
        actionButton(
          ns("simular_fit"),
          "Simular diseño balanceado & Ajustar modelos",
          icon  = icon("play"),
          class = "btn-primary w-100 mt-2"
        ),
        tags$p(
          class = "small text-muted mt-2",
          "Sugerencia: juega con σ_A y σ_B. Cuando σ_A es grande, el GLM simple suele ",
          "sobreestimar la evidencia a favor de A (usa un Residual demasiado “optimista”)."
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # Panel 3.1: Resultados numéricos
          bslib::nav_panel(
            "3.1 Resultados (caso balanceado)",
            tags$p(
              strong("Resumen rápido:"),
              " F y p-valores para A, B y A×B en los tres enfoques. ",
              "En un split-plot balanceado, el ANOVA estratificado y el LMM deberían ",
              "contar la misma historia."
            ),
            tableOutput(ns("tab_resumen_F")),
            tags$hr(),
            fluidRow(
              column(
                width = 6,
                h6("GLM simple (incorrecto)"),
                tags$p(
                  class = "small text-muted",
                  "Trata todo como si fuera un solo error (Residual). En split-plot esto ",
                  "subestima el Error A cuando hay varianza importante entre parcelas principales."
                ),
                verbatimTextOutput(ns("glm_bad_anova"))
              ),
              column(
                width = 6,
                h6("ANOVA estratificado (puente): aov(y ~ A*B + Error(Bloque/ParcelaA))"),
                tags$p(
                  class = "small text-muted",
                  "Separa el Error A (estrato Bloque:ParcelaA) del Error B (estrato Within). ",
                  "Es la forma clásica de ANOVA para split-plot balanceado."
                ),
                verbatimTextOutput(ns("aov_strata"))
              )
            ),
            tags$hr(),
            h6("LMM correcto: lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A))"),
            tags$p(
              class = "small text-muted",
              "Modelo lineal mixto con Bloque y ParcelaA como efectos aleatorios. ",
              "Usamos la tabla ANOVA Type III (", code("lmerTest"), ") para comparar A, B y A×B."
            ),
            verbatimTextOutput(ns("lmm_anova")),
            tags$hr(),
            fluidRow(
              column(
                width = 6,
                h6("Componentes de varianza (VarCorr)"),
                verbatimTextOutput(ns("lmm_varcorr"))
              ),
              column(
                width = 6,
                h6("ICCs por nivel (Bloque y ParcelaA)"),
                tableOutput(ns("tabla_icc")),
                tags$p(
                  class = "small text-muted",
                  "Estos ICCs indican cuánta dependencia hay entre subparcelas dentro de la misma ",
                  "parcela principal y dentro del mismo bloque."
                )
              )
            )
          ),
          
          # Panel 3.2: Visualización
          bslib::nav_panel(
            "3.2 Medias & layout",
            tags$h6("Tabla de medias observadas por combinación A×B"),
            tags$p(
              class = "small text-muted",
              "Cada celda resume la respuesta media en la combinación (Riego, Nitrógeno) ",
              "en todos los bloques."
            ),
            DT::dataTableOutput(ns("tab_means_ab")),
            tags$hr(),
            plotOutput(ns("plot_means"), height = "320px"),
            tags$p(
              class = "small text-muted",
              "El gráfico muestra la interacción A×B a partir de las medias observadas. ",
              "Líneas aproximadamente paralelas sugieren interacción débil; cruces o cambios ",
              "fuertes de pendiente sugieren interacción fuerte."
            ),
            tags$hr(),
            plotOutput(ns("plot_layout"), height = "320px"),
            div(
              class = "text-muted small mt-2",
              "El croquis ilustra cómo se disponen las parcelas: filas = bloques; columnas = combinaciones A:B. ",
              "Cada bloque contiene varias parcelas principales (A), y cada una se subdivide en subparcelas (B)."
            )
          ),
          
          # Panel 3.3: Post-hoc con emmeans
          bslib::nav_panel(
            "3.3 Post-hoc (emmeans)",
            tags$p(
              "Una vez ajustado el LMM, podemos obtener medias marginales estimadas (EMMs) y ",
              "comparaciones múltiples de N (B) dentro de cada nivel de Riego (A) usando ",
              code("emmeans"), "."
            ),
            tags$p(
              strong("Comparaciones de B dentro de cada A: "),
              code("pairwise ~ B | A")
            ),
            verbatimTextOutput(ns("emm_pairwise")),
            tags$hr(),
            tags$p(
              "Gráfico de EMMs para la interacción A×B (con intervalos de confianza):"
            ),
            plotOutput(ns("emm_plot"), height = "340px"),
            tags$p(
              class = "small text-muted",
              "Este enfoque conecta directamente el modelo mixto con las decisiones agronómicas: ",
              "¿qué nivel de N recomendar dentro de cada régimen de riego A?"
            )
          )
        ),
        
        tags$div(
          class = "alert alert-info mt-3",
          strong("Mensaje clave de la pestaña 3: "),
          "en un split-plot ", strong("balanceado"), " el ANOVA estratificado y el LMM ",
          "producen conclusiones coherentes para A, B y A×B. ",
          "La ventaja del LMM es que ofrece una sintaxis más unificada (en línea con RCBD + LMM de la Sesión 4) ",
          "y entrega componentes de varianza e ICCs que cuantifican la estructura jerárquica del diseño."
        )
      )
    )
  )
}

# Pestaña 4: Split-plot desbalanceado / con faltantes
pestanna4_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Split-plot desbalanceado",
    
    # Encabezado y framing
    tags$div(
      class = "mb-3",
      tags$h4("Split-plot con faltantes: ¿hasta dónde aguanta el ANOVA clásico?"),
      tags$p(
        "En un diseño split-plot ideal (balanceado), el ANOVA estratificado y el modelo mixto (LMM) ",
        "dan prácticamente el mismo mensaje para los efectos de ", strong("Riego (A)"), ", ",
        strong("Nitrógeno (B)"), " y su interacción ", strong("A×B"), "."
      ),
      tags$p(
        "En la práctica, siempre hay ", strong("subparcelas perdidas"),
        " (plantas enfermas, errores de registro, etc.). Esta pestaña muestra qué ocurre cuando:",
        tags$ul(
          tags$li("Usamos un ", strong("GLM simple"), " como si fuera un DBCA (un solo 'Residual')."),
          tags$li("Usamos el ", strong("ANOVA estratificado clásico"), " con ", code("Error(Bloque/ParcelaA)"), "."),
          tags$li("Usamos un ", strong("LMM correcto"),
                  " con ", code("(1|Bloque)"), " y ", code("(1|Bloque:A)"), ".")
        )
      ),
      tags$div(
        class = "alert alert-info small",
        tags$strong("Meta de esta pestaña: "),
        "ver que con faltantes el GLM simple rompe el F de A (parcela principal), ",
        "el ANOVA estratificado se vuelve incómodo cuando hay muchas celdas vacías y el ",
        "LMM absorbe mejor el desbalance, manteniendo la misma sintaxis vista en la P3."
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Simulador split-plot con faltantes (subparcelas)"),
        tags$p(
          class = "small text-muted",
          "Cada bloque contiene varias parcelas principales (A) y dentro de cada una varias subparcelas (B). ",
          "El factor A (Riego) actúa en la parcela principal; B (Nitrógeno) en la subparcela."
        ),
        
        # Parámetros de diseño
        tags$h6("Diseño básico"),
        selectInput(
          ns("sp4_n_bloques"),
          "Bloques (réplicas de A):",
          choices = c(3, 4, 5, 6),
          selected = 4
        ),
        selectInput(
          ns("sp4_niv_A"),
          "Niveles de Riego (A, parcela principal):",
          choices = c(2, 3),
          selected = 3
        ),
        selectInput(
          ns("sp4_niv_B"),
          "Niveles de Nitrógeno (B, subparcela):",
          choices = c(3, 4),
          selected = 4
        ),
        
        # Efectos de tratamiento
        tags$h6("Magnitud de efectos (en unidades de respuesta)"),
        sliderInput(
          ns("sp4_eff_A"),
          "Efecto A (max – min):",
          min = 0, max = 10, value = 4, step = 0.5
        ),
        sliderInput(
          ns("sp4_eff_B"),
          "Efecto B (max – min):",
          min = 0, max = 12, value = 6, step = 0.5
        ),
        sliderInput(
          ns("sp4_eff_AB"),
          "Interacción A×B (rango):",
          min = -6, max = 6, value = 2, step = 0.5
        ),
        
        # Componentes de error
        tags$h6("Componentes de error (desviaciones estándar)"),
        sliderInput(
          ns("sp4_sd_A"),
          "σ_A (Error parcela principal):",
          min = 0, max = 10, value = 4, step = 0.5
        ),
        sliderInput(
          ns("sp4_sd_B"),
          "σ_B (Error subparcela / residual):",
          min = 0, max = 10, value = 2, step = 0.5
        ),
        
        # Faltantes
        tags$h6("Faltantes en subparcelas (desbalance)"),
        sliderInput(
          ns("sp4_prop_miss"),
          "Proporción de subparcelas faltantes (MCAR):",
          min = 0, max = 0.40, value = 0.20, step = 0.05
        ),
        tags$p(
          class = "small text-muted",
          "Se eliminan aleatoriamente subparcelas (B) dentro de cada combinación Bloque×A×B. ",
          "Prueba valores ≥ 0.20 para ver un desbalance visible."
        ),
        
        actionButton(
          ns("sp4_simular_fit"),
          "Simular & Ajustar modelos",
          icon  = icon("play"),
          class = "btn btn-primary w-100 mt-2"
        )
      ),
      
      mainPanel(
        width = 8,
        
        bslib::navset_card_pill(
          
          # -------------------------------------------------------------------
          # Panel 4.1 – Datos y desbalance
          # -------------------------------------------------------------------
          bslib::nav_panel(
            "4.1 Datos & desbalance",
            tags$p(
              class = "small",
              "Empezamos revisando la estructura del dataset simulado y cuántas celdas ",
              "A×B×Bloque quedaron observadas después de eliminar subparcelas."
            ),
            fluidRow(
              column(
                width = 6,
                tags$h6("Primeras filas de los datos"),
                DT::dataTableOutput(ns("sp4_tab_datos_head"))
              ),
              column(
                width = 6,
                tags$h6("Conteos por Bloque × A × B"),
                tags$p(
                  class = "small text-muted",
                  "Las combinaciones con conteo 0 representan celdas (subparcelas) perdidas."
                ),
                DT::dataTableOutput(ns("sp4_tab_counts"))
              )
            )
          ),
          
          # -------------------------------------------------------------------
          # Panel 4.2 – Comparación de modelos
          # -------------------------------------------------------------------
          bslib::nav_panel(
            "4.2 Modelos & F de A",
            tags$p(
              class = "small",
              "Comparamos tres enfoques sobre la misma base de datos: ",
              code("aov(y ~ Bloque + A*B)"), " (GLM simple), ",
              code("aov(y ~ A*B + Error(Bloque/ParcelaA))"),
              " (ANOVA estratificado) y ",
              code("lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A))"),
              " (modelo mixto)."
            ),
            
            # Tabla resumen de p-valores para A
            tags$h6("Resumen del efecto A (parcela principal)"),
            DT::dataTableOutput(ns("sp4_tab_resumen_A")),
            tags$p(
              class = "small text-muted",
              "En presencia de desbalance, el GLM simple tiende a usar un denominador demasiado pequeño ",
              "para el F de A (Error B), inflando la evidencia a favor de diferencias en Riego. ",
              "El ANOVA estratificado y el LMM utilizan correctamente el Error A (nivel de parcela principal)."
            ),
            tags$hr(),
            
            # Salidas completas
            fluidRow(
              column(
                width = 4,
                tags$h6("GLM simple (incorrecto)"),
                verbatimTextOutput(ns("sp4_glm_bad_anova"))
              ),
              column(
                width = 4,
                tags$h6("ANOVA estratificado"),
                tags$small(code("aov(y ~ A*B + Error(Bloque/ParcelaA))")),
                verbatimTextOutput(ns("sp4_aov_strata"))
              ),
              column(
                width = 4,
                tags$h6("LMM correcto"),
                tags$small(code("lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A))")),
                verbatimTextOutput(ns("sp4_lmm_anova"))
              )
            )
          ),
          
          # -------------------------------------------------------------------
          # Panel 4.3 – VarCorr, ICC y mensaje final
          # -------------------------------------------------------------------
          bslib::nav_panel(
            "4.3 VarCorr & ICC",
            tags$p(
              class = "small",
              "El modelo mixto permite descomponer la varianza en componentes de Bloque, ParcelaA y Residual, ",
              "y calcular coeficientes de correlación intraclase (ICC) relevantes para la interpretación del diseño."
            ),
            fluidRow(
              column(
                width = 6,
                tags$h6("Componentes de varianza (VarCorr)"),
                verbatimTextOutput(ns("sp4_varcorr"))
              ),
              column(
                width = 6,
                tags$h6("ICC ParcelaA y mensaje interpretativo"),
                verbatimTextOutput(ns("sp4_icc_msg")),
                tags$p(
                  class = "small text-muted",
                  "Un ICC elevado a nivel de parcela principal indica que las subparcelas de una misma ParcelaA ",
                  "son muy similares entre sí. Analizar el ensayo como si fuera DBCA (GLM simple) ",
                  "equivale a tratar estas observaciones fuertemente correlacionadas como si fueran independientes ",
                  "→ riesgo de F(A) inflado y falsos positivos para el factor de parcela principal."
                )
              )
            ),
            tags$hr(),
            tags$div(
              class = "alert alert-info small",
              tags$strong("Mensaje clave de la pestaña: "),
              "en split-plot con faltantes, el LMM no es un lujo sino una necesidad: ",
              "usa correctamente las dos unidades experimentales (parcela principal y subparcela), ",
              "tolera el desbalance y entrega varianzas e ICCs consistentes con la aleatorización."
            )
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales & ejercicios guiados
pestanna5_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo datos reales & ejercicios",
    
    # Encabezado general
    tags$div(
      class = "mb-3",
      h4("De simulaciones a datos reales: checklist split-plot"),
      tags$p(
        class = "text-muted",
        "Objetivo: que puedas tomar un dataset real con estructura split-plot ",
        "(Bloque / Parcela principal / Subparcela) y seguir un flujo reproducible ",
        "para analizarlo con modelos mixtos."
      )
    ),
    
    sidebarLayout(
      # --------------------------------------------------------------
      # Lado izquierdo: generador de datos de práctica (igual lógica)
      # --------------------------------------------------------------
      sidebarPanel(
        width = 4,
        h5("Generar datos de práctica (split-plot Riego × N)"),
        tags$p(
          class = "small text-muted",
          "Usa este simulador solo como fuente de un CSV de ejemplo. ",
          "El análisis completo se hará fuera de la app, con la plantilla Quarto de la derecha."
        ),
        tags$h6("Diseño"),
        numericInput(ns("ex_nb"), "Bloques:", 4, min = 2, max = 12, step = 1),
        numericInput(ns("ex_na"), "Niveles A (Riego, parcela principal):", 3, min = 2, max = 6, step = 1),
        numericInput(ns("ex_nbsub"), "Niveles B (Nitrógeno, subparcela):", 4, min = 2, max = 8, step = 1),
        
        tags$h6("Magnitudes de efecto (en unidades de respuesta)"),
        numericInput(ns("ex_effA"),  "Rango efecto A:",    4,  min = 0, max = 20, step = .5),
        numericInput(ns("ex_effB"),  "Rango efecto B:",    6,  min = 0, max = 20, step = .5),
        numericInput(ns("ex_effAB"), "Rango interacción:", 2,  min = -20, max = 20, step = .5),
        
        tags$h6("Errores (DE)"),
        numericInput(ns("ex_sdA"), "σ_A (Error parcela principal):", 4, min = 0, max = 20, step = .5),
        numericInput(ns("ex_sdB"), "σ_B (Error subparcela / residual):", 2, min = 0, max = 20, step = .5),
        
        tags$h6("Faltantes"),
        sliderInput(ns("ex_miss"), "% subparcelas faltantes:", min = 0, max = 30, value = 0, step = 2),
        
        tags$hr(),
        actionButton(
          ns("ex_run"),
          "Generar datos de ejercicio",
          icon  = icon("play"),
          class = "btn-success w-100"
        ),
        tags$br(), tags$br(),
        uiOutput(ns("sp5_dl_ex_csv_ui"))
      ),
      
      # --------------------------------------------------------------
      # Lado derecho: protocolo + datos + plantilla Quarto
      # --------------------------------------------------------------
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # ----------------------------------------------------------
          # Panel 1: Protocolo paso a paso
          # ----------------------------------------------------------
          bslib::nav_panel(
            title = "5.1 Protocolo paso a paso",
            bslib::card(
              bslib::card_header("Paso 0 – Entender el diseño split-plot"),
              tags$p(
                "Antes de correr modelos, identifica claramente la estructura del ensayo. ",
                "Para un diseño clásico Riego × Nitrógeno:"
              ),
              tags$ul(
                tags$li(
                  strong("Bloque:"),
                  " unidad superior que agrupa varias parcelas principales ",
                  "(p. ej., lotes o franjas grandes)."
                ),
                tags$li(
                  strong("A (Parcela principal, Riego):"),
                  " se asigna por parcelas grandes dentro de cada bloque ",
                  "(UE A)."
                ),
                tags$li(
                  strong("ParcelaA:"),
                  " identificador de cada parcela principal dentro de Bloque ",
                  "(combinación Bloque × A)."
                ),
                tags$li(
                  strong("B (Subparcela, Nitrógeno):"),
                  " niveles de N asignados a subparcelas dentro de cada parcela principal ",
                  "(UE B)."
                ),
                tags$li(
                  strong("y:"),
                  " respuesta continua (rendimiento, biomasa, etc.)."
                )
              ),
              tags$div(
                class = "alert alert-info small",
                strong("Preguntas clave: "),
                "¿Quién es UE A (parcela principal)? ¿Quién es UE B (subparcela)? ",
                "En split-plot siempre hay al menos dos unidades experimentales y por tanto ",
                "dos fuentes de error: Error A (entre parcelas principales) y Error B (entre subparcelas)."
              )
            ),
            
            bslib::card(
              bslib::card_header("Paso 1 – Explorar balance y faltantes"),
              tags$p(
                "Con los datos cargados en R (data frame ", code("d"), ") revisa la estructura mínima:"
              ),
              tags$pre(
                class = "r-code",
                style = "white-space: pre-wrap;",
'library(dplyr)

d %>% count(Bloque, A, B)          # Conteos por combinación Bloque × A × B
xtabs(~ Bloque + A + B, data = d)  # Tabla de frecuencias (si es razonable)'
              ),
              tags$ul(
                tags$li("Si la tabla está prácticamente completa → caso parecido a la pestaña 3 (balanceado)."),
                tags$li("Si hay muchos huecos / tamaños muy desiguales → caso parecido a la pestaña 4 (desbalanceado).")
              ),
              tags$div(
                class = "alert alert-warning small",
                strong("Regla práctica: "),
                "a mayor desbalance y mayor varianza entre parcelas principales (σ²_A), ",
                "más peligrosa es la aproximación con GLM simple ",
                code("aov(y ~ Bloque + A*B)"),
                " para el factor A."
              )
            ),
            
            bslib::card(
              bslib::card_header("Paso 2 – Modelos candidatos"),
              tags$p(
                "Con la estructura clara y el desbalance diagnosticado, puedes comparar tres enfoques: "
              ),
              tags$ol(
                tags$li(
                  strong("GLM simple (incorrecto para A): "),
                  code("aov(y ~ Bloque + A*B, data = d)"),
                  " usa un único residual para todo. Tiende a inflar el F de A si hay variación importante ",
                  "entre parcelas principales."
                ),
                tags$li(
                  strong("ANOVA estratificado (puente clásico): "),
                  code("aov(y ~ A*B + Error(Bloque/ParcelaA), data = d)"),
                  ". Da denominadores F diferentes para A (usa Bloque:ParcelaA) ",
                  "y para B, A×B (usa el residual), pero es incómodo con desbalance fuerte."
                ),
                tags$li(
                  strong("Modelo lineal mixto (recomendado): "),
                  code("lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A), data = d)"),
                  ". Usa el mismo lenguaje de la Sesión 4 (RCBD + LMM) y maneja mejor faltantes."
                )
              ),
              tags$div(
                class = "alert alert-secondary small",
                strong("Idea: "),
                "el LMM reproduce la lógica del ANOVA estratificado, pero en lugar de “Errores A y B” ",
                "tienes componentes de varianza para Bloque y Bloque:A que se combinan internamente para ",
                "construir los F de A, B y A×B."
              )
            ),
            
            bslib::card(
              bslib::card_header("Paso 3 – Outputs clave del LMM"),
              fluidRow(
                column(
                  width = 6,
                  tags$h5("3.1 Tabla ANOVA (A, B, A×B)"),
                  tags$p(
                    "Usando ", code("lmerTest"), ":"
                  ),
                  tags$pre(
                    class = "r-code",
                    style = "white-space: pre-wrap;",
'anova(m_lmm, type = 3, ddf = "Satterthwaite")'
                  ),
                  tags$p(
                    "La tabla muestra F y p-valores para A, B y A×B. ",
                    "A diferencia del GLM, aquí el F de A incorpora correctamente la varianza de Error A ",
                    "(entre parcelas principales)."
                  )
                ),
                column(
                  width = 6,
                  tags$h5("3.2 VarCorr e ICCs"),
                  tags$p(
                    "Con ", code("VarCorr(m_lmm)"), " obtienes las varianzas para ",
                    code("Bloque"), ", ", code("Bloque:A"), " y el residual. ",
                    "A partir de ahí puedes construir ICCs análogos a los de la Sesión 4:"
                  ),
                  tags$pre(
                    class = "r-code",
                    style = "white-space: pre-wrap;",
'vc <- as.data.frame(lme4::VarCorr(m_lmm))
get_var <- function(grp) {
  x <- vc[vc$grp == grp & vc$var1 == "(Intercept)", "vcov"]
  if (length(x) == 0) 0 else x[1]
}
v_blk   <- get_var("Bloque")
v_PA    <- get_var("Bloque:A")
v_res   <- get_var("Residual")
ICC_PA  <- ifelse(v_PA + v_res > 0, v_PA / (v_PA + v_res), NA_real_)
ICC_blk <- ifelse(v_blk + v_PA + v_res > 0,
                  (v_blk + v_PA) / (v_blk + v_PA + v_res),
                  NA_real_)'
                  ),
                  tags$p(
                    strong("Lectura simple: "),
                    "ICC(ParcelaA) alto + desbalance + GLM simple ⇒ riesgo de falso positivo para A. ",
                    "El LMM corrige ese problema."
                  )
                )
              ),
              tags$hr(),
              tags$h5("3.3 Diagnóstico gráfico"),
              tags$p(
                "Como en la Sesión 4, revisa residuos vs. ajustados y Q–Q normal sobre residuales de ",
                code("m_lmm"),
                " para verificar homocedasticidad aproximada y ausencia de outliers extremos."
              )
            ),
            
            bslib::card(
              bslib::card_header("Paso 4 – Post-hoc y reporte agronómico"),
              tags$p(
                "Una vez validado el modelo, puedes obtener medias marginales y comparaciones de tratamientos ",
                "con ", code("emmeans"), ":"
              ),
              tags$pre(
                class = "r-code",
                style = "white-space: pre-wrap;",
'library(emmeans)

emm_AB  <- emmeans(m_lmm, ~ A*B)          # Medias marginales por combinación A×B
pairs_B <- emmeans(m_lmm, pairwise ~ B | A)  # Comparaciones de N dentro de cada riego'
              ),
              tags$ul(
                tags$li("Reportar medias y errores estándar para cada combinación Riego × N."),
                tags$li("Resumir el efecto global de A (riego) y B (nitrógeno)."),
                tags$li("Comentar si la interacción A×B es relevante (planos que se cruzan o no)."),
                tags$li("Incluir ICC(ParcelaA) para cuantificar cuánta variación se captura al dividir la parcela grande.")
              ),
              tags$div(
                class = "alert alert-success small",
                strong("Producto final esperado: "),
                "un reporte (Quarto/R Markdown) donde quede claro: diseño, modelo, ANOVA LMM, VarCorr/ICC, ",
                "post-hoc (emmeans) y conclusión agronómica en lenguaje claro."
              )
            )
          ),
          
          # ----------------------------------------------------------
          # Panel 2: Datos simulados (vista rápida)
          # ----------------------------------------------------------
          bslib::nav_panel(
            title = "5.2 Datos simulados",
            tags$p(
              "Cuando generes datos de ejercicio (botón verde de la izquierda), ",
              "podrás ver aquí una muestra de las filas y el balance Bloque × A × B."
            ),
            fluidRow(
              column(
                width = 6,
                bslib::card(
                  bslib::card_header("Vista rápida (primeras filas)"),
                  DT::dataTableOutput(ns("sp5_ex_tab_head"))
                )
              ),
              column(
                width = 6,
                bslib::card(
                  bslib::card_header("Conteo Bloque × A × B"),
                  tags$small(
                    class = "text-muted",
                    "Si todos los conteos son iguales, el diseño está balanceado; ",
                    "si hay muchos ceros o números muy distintos, el diseño está desbalanceado."
                  ),
                  DT::dataTableOutput(ns("sp5_ex_tab_counts"))
                )
              )
            ),
            tags$hr(),
            tags$p(
              class = "small text-muted",
              "Estos mismos datos son los que podrás descargar como CSV para analizarlos en tu propio script Quarto."
            )
          ),
          
          # ----------------------------------------------------------
          # Panel 3: Plantilla Quarto & ejercicios guiados
          # ----------------------------------------------------------
          bslib::nav_panel(
            title = "5.3 Plantilla Quarto & ejercicios",
            
            bslib::card(
              bslib::card_header("Plantilla Quarto para split-plot (Riego × N)"),
              tags$p(
                "La siguiente plantilla asume un archivo ", code("splitplot_ejercicio.csv"),
                " con columnas mínimas: ", code("Bloque"), ", ", code("A"), ", ",
                code("ParcelaA"), ", ", code("B"), ", ", code("y"), "."
              ),
              tags$pre(
                class = "r-code",
                style = "white-space: pre-wrap;",
'```{r}
# Paquetes recomendados
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)  # ANOVA tipo III con ddf de Satterthwaite
library(emmeans)

# Paso 0: leer datos y revisar estructura
d <- read.csv("splitplot_ejercicio.csv")
str(d)

# Paso 1: explorar balance y faltantes
d %>% count(Bloque, A, B)
xtabs(~ Bloque + A + B, data = d)

# Paso 2: modelos candidatos
m_glm  <- aov(y ~ Bloque + A*B, data = d)                 # GLM simple (no recomendado para A)
m_sp   <- aov(y ~ A*B + Error(Bloque/ParcelaA), data = d) # ANOVA estratificado (puente)
m_lmm  <- lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A),
               data = d, REML = TRUE)                     # LMM recomendado

summary(m_glm)
summary(m_sp)

# Paso 3a: ANOVA LMM (A, B, A×B)
anova(m_lmm, type = 3, ddf = "Satterthwaite")

# Paso 3b: VarCorr e ICCs
vc <- as.data.frame(VarCorr(m_lmm))
get_var <- function(grp) {
  x <- vc[vc$grp == grp & vc$var1 == "(Intercept)", "vcov"]
  if (length(x) == 0) 0 else x[1]
}
v_blk   <- get_var("Bloque")
v_PA    <- get_var("Bloque:A")
v_res   <- get_var("Residual")
ICC_PA  <- ifelse(v_PA + v_res > 0, v_PA / (v_PA + v_res), NA_real_)
ICC_blk <- ifelse(v_blk + v_PA + v_res > 0,
                  (v_blk + v_PA) / (v_blk + v_PA + v_res),
                  NA_real_)
ICC_PA; ICC_blk

# Paso 3c: diagnósticos
par(mfrow = c(1, 2))
plot(fitted(m_lmm), resid(m_lmm),
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, lty = 2, col = 2)
qqnorm(resid(m_lmm)); qqline(resid(m_lmm), col = 2)

# Paso 4: EMMs y comparaciones
emm_AB  <- emmeans(m_lmm, ~ A*B)
pairs_B <- emmeans(m_lmm, pairwise ~ B | A)

emm_AB
pairs_B
```'
              ),
              tags$p(
                class = "small text-muted",
                "Puedes adaptar esta plantilla a datos reales cambiando los nombres de columnas y la interpretación agronómica."
              )
            ),
            
            bslib::card(
              bslib::card_header("Ejercicios guiados sugeridos"),
              tags$ol(
                tags$li(
                  "Simula un split-plot con el generador de la izquierda (elige n° de bloques, niveles de A y B, ",
                  "y magnitudes de efecto). Descarga el CSV y analiza con la plantilla Quarto."
                ),
                tags$li(
                  "Compara el F y p-valor de A entre ",
                  code("m_glm"), " y ", code("m_lmm"),
                  ". Escribe en 2–3 líneas por qué el GLM simple puede ser engañoso para el efecto de riego."
                ),
                tags$li(
                  "Introduce un 20 % de subparcelas faltantes (", code("% subparcelas faltantes = 20"),
                  ") y repite el análisis. Comenta qué pasa con la estabilidad del ANOVA estratificado ",
                  "y cómo el LMM sigue funcionando con la misma sintaxis."
                ),
                tags$li(
                  "Calcula ICC(ParcelaA) y discute, en lenguaje agronómico, ",
                  "si valió la pena dividir la parcela grande en subparcelas (¿cuánta variación se capturó?)."
                ),
                tags$li(
                  "Redacta una conclusión agronómica breve (3–4 líneas) recomendando combinaciones de Riego × N ",
                  "y explicando por qué el análisis se hizo con un modelo mixto."
                )
              )
            )
          )
        )
      )
    )
  )
}

# Pestaña 6: Notas avanzadas & advertencias
pestanna6_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Notas avanzadas & advertencias",
    
    tags$div(
      class = "mb-3",
      h4(class = "section-header", "Notas avanzadas y advertencias para diseños split-plot"),
      tags$p(
        "Esta pestaña funciona como un ", strong("resumen avanzado"),
        " de temas que no desarrollaremos paso a paso en vivo, pero que son críticos ",
        "para análisis serios de diseños split-plot y split-split-plot con modelos mixtos. ",
        "La idea es que puedas volver aquí cuando hagas tu tesis o analices ensayos reales."
      ),
      tags$p(
        class = "text-muted small",
        "En particular: REML vs ML, uso correcto de AIC, pruebas para efectos aleatorios, ",
        "pseudorreplicación y extensión a diseños más complejos (split-split-plot)."
      )
    ),
    
    # -------------------------------------------------------------------
    # 1) REML vs ML
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("1. REML vs ML en modelos lineales mixtos"),
      tags$p(
        strong("Idea básica:"),
        " en modelos mixtos (", code("lmer()"), "), el argumento ",
        code("REML = TRUE/FALSE"),
        " controla cómo se estiman los componentes de varianza."
      ),
      tags$ul(
        tags$li(
          strong("REML = TRUE (por defecto en lmer):"),
          " recomendado cuando ya decidiste la estructura de efectos fijos ",
          "(qué tratamientos/contrastes estarán en el modelo). ",
          "Produce estimaciones menos sesgadas de las varianzas."
        ),
        tags$li(
          strong("REML = FALSE (ML):"),
          " útil cuando quieres comparar modelos con ",
          strong("distintas estructuras de efectos fijos"),
          " usando criterios de información (AIC, BIC). ",
          "En ese caso, refiteas los modelos con ", code("REML = FALSE"),
          " y comparas AIC solo entre modelos que comparten la ",
          strong("misma estructura de efectos aleatorios")
          , "."
        )
      ),
      tags$div(
        class = "alert alert-info small",
        tags$strong("Regla práctica: "),
        "para un solo modelo bien especificado, quédate con ",
        code("REML = TRUE"),
        " (estimación de varianzas). ",
        "Solo cambia a ", code("REML = FALSE"),
        " cuando quieras comparar modelos con distintos términos fijos ",
        "(por ejemplo, con o sin interacción A×B)."
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        HTML(
'## Modelo mixto base (split-plot)
m_REML  <- lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A),
                data = datos, REML = TRUE)

## Comparar estructuras de fijos → refitear con REML = FALSE
m_ML_1  <- lmer(y ~ A + B + (1|Bloque) + (1|Bloque:A),
                data = datos, REML = FALSE)
m_ML_2  <- lmer(y ~ A*B      + (1|Bloque) + (1|Bloque:A),
                data = datos, REML = FALSE)

anova(m_ML_1, m_ML_2)  # LRT / AIC entre estructuras de fijos'
        )
      )
    ),
    
    # -------------------------------------------------------------------
    # 2) AIC en LMM de split-plot
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("2. AIC y comparación de modelos en LMM (split-plot)"),
      tags$p(
        strong("Criterios de información (AIC, BIC)"),
        " permiten comparar modelos, pero bajo reglas claras."
      ),
      tags$ul(
        tags$li(
          strong("Regla 1:"),
          " compara AIC solo entre modelos que tienen la ",
          strong("misma estructura de efectos aleatorios"),
          " (mismos términos ", code("(1|Bloque)"), ", ", code("(1|Bloque:A)"), ", etc.)."
        ),
        tags$li(
          strong("Regla 2:"),
          " si cambias los efectos fijos (p. ej. quitas A×B), refitea todos los modelos con ",
          code("REML = FALSE"),
          " antes de comparar AIC."
        ),
        tags$li(
          strong("Regla 3:"),
          " no tiene sentido comparar AIC de un ",
          strong("GLM simple"),
          " (", code("aov(y ~ Bloque + A*B)"), ") con el AIC de un ",
          strong("LMM"),
          " (", code("lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A))"), "); ",
          "son familias de modelos diferentes."
        )
      ),
      tags$div(
        class = "alert alert-warning small",
        tags$strong("Mensaje: "),
        "usa AIC como herramienta para refinar el modelo, ",
        "pero no como sustituto de entender el diseño. ",
        "Primero respeta la estructura de parcelas (Bloque / ParcelaA / Subparcela); ",
        "luego decide si vale la pena simplificar algunos términos fijos."
      )
    ),
    
    # -------------------------------------------------------------------
    # 3) Tests para efectos aleatorios
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("3. Tests para efectos aleatorios (Bloque, Parcela principal)"),
      tags$p(
        "En split-plot, típicamente tratamos ", code("Bloque"),
        " y ", code("ParcelaA"), " como efectos aleatorios. ",
        "A veces surge la pregunta: “¿de verdad necesito el término ",
        code("(1|Bloque:A)"),
        "?”"
      ),
      tags$ul(
        tags$li(
          strong("Enfoque clásico: Likelihood Ratio Test (LRT)"),
          " entre un modelo completo y uno reducido:"
        )
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        HTML(
'## Modelo completo (con efecto aleatorio de ParcelaA)
m_full <- lmer(y ~ A*B + (1|Bloque) + (1|Bloque:A),
               data = datos, REML = FALSE)

## Modelo reducido (sin ParcelaA)
m_red  <- lmer(y ~ A*B + (1|Bloque),
               data = datos, REML = FALSE)

anova(m_red, m_full)   # LRT para el componente de ParcelaA'
        )
      ),
      tags$ul(
        tags$li(
          strong("Advertencia importante:"),
          " los efectos aleatorios tienen restricciones de frontera (σ² ≥ 0). ",
          "Los p-valores del LRT pueden ser conservadores o difíciles de interpretar ",
          "cuando la varianza estimada está cerca de 0."
        ),
        tags$li(
          strong("Recomendación de muchos autores (Stroup, Yang, etc.):"),
          " en lugar de “testear si el efecto aleatorio es 0”, ",
          "diseñar el experimento de forma que tenga sentido científico ",
          "incluir ese estrato (p. ej. parcelas principales reales) ",
          "y mantener el efecto aleatorio en el modelo final."
        )
      ),
      tags$div(
        class = "alert alert-secondary small",
        tags$strong("Regla práctica: "),
        "en diseños split-plot bien planteados, ",
        code("(1|Bloque)"),
        " y ",
        code("(1|Bloque:A)"),
        " suelen considerarse parte fija de la estructura aleatoria ",
        "(no se “testean fuera” salvo razones muy fuertes)."
      )
    ),
    
    # -------------------------------------------------------------------
    # 4) Pseudorreplicación en split-plot
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("4. Pseudorreplicación en split-plot"),
      tags$p(
        "La ", strong("pseudorreplicación"),
        " ocurre cuando analizamos datos como si hubiera más réplicas independientes ",
        "de las que realmente existen. En split-plot, el riesgo clásico está en el factor ",
        strong("A (parcela principal)"),
        "."
      ),
      tags$ul(
        tags$li(
          strong("Escenario problemático: "),
          "analizar el factor A como si cada subparcela fuera una réplica independiente, ",
          "usando un GLM simple ", code("y ~ Bloque + A*B"),
          " y tomando siempre el Residual como error."
        ),
        tags$li(
          "En realidad, las subparcelas dentro de la misma parcela principal comparten ",
          "condiciones (riego, manejo, micro-sitio); no son réplicas independientes para A."
        ),
        tags$li(
          "El efecto A debería ser probado contra el ",
          strong("Error A"),
          " (variación entre parcelas principales) y no contra el Residual ",
          "que incluye variación a nivel de subparcela (Error B)."
        )
      ),
      tags$div(
        class = "alert alert-warning small",
        tags$strong("Consecuencia: "),
        "si ignoras la estructura de parcela principal, inflas el tamaño muestral efectivo ",
        "para A y aumentas la probabilidad de encontrar diferencias “significativas” ",
        "que en realidad son artefactos del diseño (pseudorreplicación)."
      ),
      tags$p(
        "El modelo mixto adecuado para split-plot, ",
        code("y ~ A*B + (1|Bloque) + (1|Bloque:A)"),
        ", separa explícitamente ",
        strong("Error A"), " (varianza entre parcelas principales) y ",
        strong("Error B"), " (varianza residual entre subparcelas), ",
        "evitando exactamente este problema."
      )
    ),
    
    # -------------------------------------------------------------------
    # 5) Extensión a split-split-plot y diseños más complejos
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("5. Extensión a split-split-plot (A × B × C)"),
      tags$p(
        "Los principios de split-plot se extienden de forma natural a diseños ",
        strong("split-split-plot"),
        " y otras estructuras jerárquicas (parcelas divididas sucesivamente)."
      ),
      tags$p(
        "Un esquema típico con tres factores podría ser:"
      ),
      tags$ul(
        tags$li(strong("A:"), " factor en parcela principal (p.ej., sistema de riego)."),
        tags$li(strong("B:"), " factor en subparcela (p.ej., dosis de nitrógeno)."),
        tags$li(strong("C:"), " factor en sub-subparcela (p.ej., variedad o densidad de plantas).")
      ),
      tags$p(
        "La estructura de anidación sería algo como:",
        tags$br(),
        code("Bloque / A / A:B / A:B:C")
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        HTML(
'# Ejemplo de fórmula para un diseño split-split-plot
m_ssp <- lmer(y ~ A*B*C +
                (1 | Bloque) +        # variación entre bloques
                (1 | Bloque:A) +      # Error A (parcela principal)
                (1 | Bloque:A:B),     # Error B (subparcela)
              data = datos, REML = TRUE)'
        )
      ),
      tags$ul(
        tags$li(
          code("(1|Bloque:A)"),
          " representa el estrato de ", strong("parcela principal (Error A)"), "."
        ),
        tags$li(
          code("(1|Bloque:A:B)"),
          " representa el estrato de ", strong("subparcela (Error B)"), "."
        ),
        tags$li(
          "El Residual pasa a ser el ", strong("Error C"),
          " (variación entre sub-subparcelas dentro de cada subparcela)."
        )
      ),
      tags$div(
        class = "alert alert-success small",
        tags$strong("Spoiler para tesis/trabajos avanzados: "),
        "una vez que dominas el caso RCBD (Sesión 4) y el split-plot (Sesión 5), ",
        "la extensión a split-split-plot es principalmente ",
        strong("contable"),
        ": identificar correctamente las unidades experimentales en cada estrato ",
        "y traducirlas en términos aleatorios dentro de ", code("lmer()"), "."
      )
    ),
    
    # -------------------------------------------------------------------
    # 6) Cierre
    # -------------------------------------------------------------------
    tags$div(
      class = "mt-3 text-muted small",
      tags$strong("Cierre: "),
      "usa esta pestaña como referencia cuando te enfrentes a diseños mixtos reales ",
      "en tu tesis o trabajo profesional. En combinación con la pestaña de ",
      strong("Referencias"),
      " tendrás una base sólida para justificar tus decisiones de modelado ",
      "ante revisores, asesores y colegas."
    )
  )
}

# Pestaña Extra: Esquemas Visuales
pestanna_extra_session5_v3UI <- function(ns) {
  # Definir la ruta base para las imágenes de esta sesión
  base_path <- "images/sesiones/Disenos_estadisticos_V3/optimizada/"
  img_path  <- paste0(base_path, "session5/")
  
  bslib::nav_panel(
    title = "Extra: Esquemas Visuales",
    icon = icon("project-diagram"), # Icono de diagrama/estructura
    
    tags$div(
      class = "container-fluid py-3",
      tags$h4(class = "text-primary mb-4", "Galería Conceptual: Diseños Split-Plot"),
      tags$p(
        class = "lead",
        "Los diseños de parcelas divididas tienen una estructura jerárquica que puede ser difícil de visualizar. ",
        "Esta pestaña utiliza esquemas para clarificar la disposición física, la sintaxis del modelo y la interpretación de errores."
      ),
      tags$hr(),
      
      # Navegación interna con pestañas subrayadas
      bslib::navset_card_underline(
        
        # --- Sub-pestaña 1: Jerarquía Física ---
        bslib::nav_panel(
          title = "1. Jerarquía Física",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-8",
              tags$img(
                src = paste0(img_path, "split_plot_hierarchy_diagram.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Diagrama de la jerarquía física de un diseño split-plot",
                style = "width: 100%; object-fit: contain;"
              )
            ),
            tags$div(
              class = "col-md-4",
              tags$div(
                class = "alert alert-secondary mt-3 mt-md-0",
                tags$h5("El Campo Anidado"),
                tags$p(
                  "Observa cómo el factor A (Riego) no se asigna al azar a cualquier cuadrado pequeño, sino a toda una franja grande (Parcela Principal)."
                ),
                tags$p(
                  "Esto significa que todas las subparcelas dentro de esa franja comparten el mismo error de aplicación de A. Por eso el ", strong("Error A"), " es distinto y suele ser mayor que el Error B."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 2: Sintaxis lmer() ---
        bslib::nav_panel(
          title = "2. Anatomía de la Fórmula",
          tags$div(
            class = "row justify-content-center",
            tags$div(
              class = "col-md-10",
              tags$img(
                src = paste0(img_path, "lmer_split_plot_formula.webp"),
                class = "img-fluid shadow border rounded mx-auto d-block",
                alt = "Desglose de la fórmula lmer para split-plot",
                style = "width: 100%;"
              ),
              tags$div(
                class = "mt-3 p-3 bg-light border rounded",
                tags$h6(class = "text-success", "El componente clave: (1 | Bloque:A)"),
                tags$p(
                  "Este término es el que le dice a R: 'Oye, hay un error extra asociado a cada parcela principal'. ",
                  "Si lo omites, R asumirá que todas las subparcelas son independientes, convirtiendo tu modelo en un GLM incorrecto."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 3: El Peligro del GLM ---
        bslib::nav_panel(
          title = "3. ¿Por qué no usar GLM?",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-12 mb-3",
              tags$img(
                src = paste0(img_path, "glm_vs_lmm_error_comparison.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Comparación de errores entre GLM y LMM",
                style = "width: 100%;"
              )
            ),
            tags$div(
              class = "col-md-12",
              tags$div(
                class = "alert alert-danger border-danger",
                tags$strong("El riesgo de Falsos Positivos:"),
                " Al usar un GLM simple, comparas el efecto de A contra un ruido muy pequeño (Error B). ",
                "Esto hace que A parezca mucho más significativo de lo que realmente es. El LMM usa el ruido correcto (Error A) para probar A."
              )
            )
          )
        ),
        
        # --- Sub-pestaña 4: Interpretación de Interacción ---
        bslib::nav_panel(
          title = "4. Leyendo la Interacción",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-7",
              tags$img(
                src = paste0(img_path, "interaction_profiles_interpretation.webp"),
                class = "img-fluid shadow border rounded",
                alt = "Guía para interpretar gráficos de interacción",
                style = "width: 100%;"
              )
            ),
            tags$div(
              class = "col-md-5",
              tags$div(
                class = "card",
                tags$div(
                  class = "card-header bg-info text-white", 
                  "Agronomía de la Interacción"
                ),
                tags$div(
                  class = "card-body",
                  tags$p("En un split-plot, la interacción A×B suele ser lo más interesante."),
                  tags$ul(
                    tags$li(strong("Sin Interacción:"), " Puedes recomendar el mejor nivel de N sin importar el Riego."),
                    tags$li(strong("Con Interacción:"), " Tu recomendación de N DEPENDE del Riego disponible. Debes analizar cada escenario por separado.")
                  )
                )
              )
            )
          )
        )
      ) # Fin navset_card_underline
    )
  )
}

# Pestaña 7: Referencias
pestanna7_session5_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    
    tags$h4("Bibliografía y recursos recomendados"),
    tags$p(
      class = "text-muted small",
      "Esta lista reúne artículos, libros y notas de curso que sirven como base teórica ",
      "para los modelos mixtos en agricultura, diseños split-plot/split-split-plot y ",
      "el uso de ANOVA estratificado, ", code("lmer()"), " y ", code("emmeans()"), "."
    ),
    
    tags$ul(
      # Yang (2010) – mixed models en agricultura
      tags$li(
        "Yang, R.-C. (2010). ",
        em("Towards understanding and use of mixed-model analysis of agricultural experiments."),
        " Canadian Journal of Plant Science, 90(5), 605–627. ",
        tags$a(
          href = "https://doi.org/10.4141/CJPS10049",
          "https://doi.org/10.4141/CJPS10049",
          target = "_blank"
        ),
        " (visión aplicada de modelos mixtos en ensayos agrícolas, incluyendo RCBD y split-plot)."
      ),
      
      # Bates et al. (2015) – lme4
      tags$li(
        "Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). ",
        em("Fitting linear mixed-effects models using lme4."),
        " Journal of Statistical Software, 67(1), 1–48. ",
        tags$a(
          href = "https://www.jstatsoft.org/article/view/v067i01/946",
          "JSS link",
          target = "_blank"
        ),
        " (referencia principal para la sintaxis y la implementación de ",
        code("lme4::lmer()"), ")."
      ),
      
      # Kuznetsova et al. (2017) – lmerTest
      tags$li(
        "Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). ",
        em("lmerTest package: Tests in linear mixed effects models."),
        " Journal of Statistical Software, 82(13). ",
        tags$a(
          href = "https://backend.orbit.dtu.dk/ws/portalfiles/portal/140635100/lmerTestJStatSoft2017.pdf",
          "JSS link",
          target = "_blank"
        ),
        " (cómo obtener tablas ANOVA Type III y p-valores con Satterthwaite/Kenward–Roger)."
      ),
      
      # Lenth – emmeans
      tags$li(
        "Lenth, R. V. (2025). ",
        em("emmeans: Estimated marginal means."),
        " CRAN manual. ",
        tags$a(
          href = "https://cran.r-project.org/package=emmeans",
          "CRAN: emmeans",
          target = "_blank"
        ),
        " (guía para medias marginales estimadas y comparaciones post-hoc en modelos mixtos)."
      ),
      
      # Stroup – GLMM, capítulos de split-plot
      tags$li(
        "Stroup, W. W. (2013). ",
        em("Generalized Linear Mixed Models: Modern Concepts, Methods and Applications."),
        " Boca Raton: CRC Press. ",
        "Capítulos dedicados a diseños en parcelas divididas (split-plot) y subdivididas ",
        "(split-split-plot), con énfasis en la conexión entre diseños clásicos y GLMM."
      ),
      
      # ETH Zürich – notas split-plot ANOVA & mixed
      tags$li(
        "ETH Zürich (s.f.). ",
        em("Split-Plot Designs – ANOVA and Mixed Models."),
        " ",
        tags$a(
          href = "https://people.math.ethz.ch/~meier/teaching/anova/split-plot-designs.html",
          "course notes",
          target = "_blank"
        ),
        " (notas de curso que muestran claramente el paso de ANOVA estratificado a modelos mixtos)."
      ),
      
      # Stats4SD – recurso práctico en R
      tags$li(
        "Stats4SD (s.f.). ",
        em("Chapter 4: Split-plot designs (R)."),
        " ",
        tags$a(
          href = "https://shiny.stats4sd.org/AgAnalysis/split-plot-designs.html",
          "resource",
          target = "_blank"
        ),
        " (ejemplos prácticos en R para analizar diseños split-plot desde un enfoque aplicado)."
      ),
      
      # PSU / STAT 502 – expected mean squares y denominadores F
      tags$li(
        "Penn State University (s.f.). ",
        em("STAT 502 – Analysis of Variance and Design of Experiments."),
        " Notas de curso con ejemplos de ",
        strong("expected mean squares"),
        " y elección de denominadores F en diseños factoriales y split-plot. ",
        tags$a(
          href = "https://online.stat.psu.edu/stat502",
          "STAT 502 online",
          target = "_blank"
        )
      )
    ),
    
    tags$hr(),
    tags$p(
      class = "text-muted small",
      "Sugerencia para tesis y trabajos aplicados: ",
      "usar Yang (2010) y Stroup (2013) como base conceptual para modelos mixtos en agricultura, ",
      "Bates et al. (2015) / Kuznetsova et al. (2017) para la implementación en R, ",
      "y las notas de ETH Zürich, Stats4SD y STAT 502 para reforzar la parte de ANOVA estratificado, ",
      "expected mean squares y denominadores F en split-plot."
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session5_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 5: Diseños de Parcelas Divididas y Subdivididas con LMM")
    ),
    navset_tab(
      pestanna1_session5_v3UI(ns),
      pestanna2_session5_v3UI(ns),
      pestanna3_session5_v3UI(ns),
      pestanna4_session5_v3UI(ns),
      pestanna5_session5_v3UI(ns),
      pestanna6_session5_v3UI(ns),
      pestanna_extra_session5_v3UI(ns),
      pestanna7_session5_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Server de la Pestaña 1
pestanna1_session5_v3_server <- function(input, output, session) {
  # Pestaña puramente conceptual/estática; no se requiere lógica de servidor.
}

# Server de la Pestaña 2
pestanna2_session5_v3_server <- function(input, output, session) {
  
  output$tabla_denominadores_F <- renderTable({
    # Tabla pedagógica de denominadores F para un split-plot A × B
    # con Bloque, ParcelaA = Bloque:A y Residual (Error B).
    data.frame(
      `Fuente` = c(
        "Bloque",
        "Parcela principal (A dentro de Bloque)",
        "Bloque×A (Error A)",
        "Subparcela (B)",
        "Interacción A×B",
        "Residual (Error B)"
      ),
      `GLM simple\n(incorrecto)` = c(
        "—",
        "CM(Residual)",
        "—",
        "CM(Residual)",
        "CM(Residual)",
        "CM(Residual)"
      ),
      `ANOVA estratificado\n aov(..., Error(Bloque/ParcelaA))` = c(
        "—",
        "CM(Bloque:A)",
        "—",
        "CM(Residual)",
        "CM(Residual)",
        "CM(Residual)"
      ),
      `LMM (lmer)\nmodelo mixto` = c(
        "Efecto aleatorio",
        "Efecto fijo probado\ncontra Var(Bloque:A)",
        "Efecto aleatorio\n(Error A)",
        "Efecto fijo probado\ncontra Var(Residual)",
        "Efecto fijo probado\ncontra Var(Residual)",
        "Residual (Error B)"
      ),
      check.names = FALSE
    )
  },
  striped  = TRUE,
  bordered = TRUE,
  align    = "c")
}

# Server de la Pestaña 3
pestanna3_session5_v3_server <- function(input, output, session, sim_react, icc_from_varcorr) {
  
  # IMPORTANTE (comentario pedagógico):
  # En el servidor principal (session5_v3Server), se recomienda que sim_react()
  # use prop_miss = 0 para esta pestaña (caso balanceado). En el código actual
  # se hace vía input$prop_miss, pero en esta UI dejamos el slider fijo en 0.
  #
  # sim_react() debe devolver un data.frame con columnas:
  #   - Bloque (factor)
  #   - A      (factor de parcela principal)
  #   - B      (factor de subparcela)
  #   - ParcelaA (factor Bloque:A)
  #   - y      (respuesta numérica)
  
  #--------------------------------------------------------------------
  # 1) Tabla resumen de F y p-valores para A, B, A×B en los 3 enfoques
  #--------------------------------------------------------------------
  output$tab_resumen_F <- renderTable({
    df <- sim_react()
    shiny::req(df)
    
    # GLM simple (incorrecto para split-plot)
    mod_glm <- stats::aov(y ~ Bloque + A*B, data = df)
    sum_glm <- summary(mod_glm)[[1]]
    
    # ANOVA estratificado clásico para split-plot
    mod_sp  <- stats::aov(y ~ A*B + Error(Bloque/ParcelaA), data = df)
    sum_sp  <- summary(mod_sp)
    
    # LMM correcto
    suppressPackageStartupMessages({
      requireNamespace("lmerTest", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
                              data = df, REML = TRUE)
    an_lmm  <- anova(mod_lmm, type = 3)  # por defecto ddf = "Satterthwaite"
    an_lmm_df <- as.data.frame(an_lmm)
    
    # Extraer F y p para A, B, A:B en cada enfoque --------------------
    get_F_p_glm <- function(term) {
      if (!term %in% rownames(sum_glm)) return(c(F=NA_real_, p=NA_real_))
      c(
        F  = sum_glm[term, "F value"],
        p  = sum_glm[term, "Pr(>F)"]
      )
    }
    
    # Para aov con Error(Bloque/ParcelaA):
    # Intentamos ubicar los estratos de forma segura
    nm_sp <- names(sum_sp)
    idx_A   <- grep("ParcelaA", nm_sp, fixed = TRUE)
    # Usually "Within" or just the last one
    idx_W   <- grep("Within", nm_sp, fixed = TRUE)
    
    sum_sp_A <- if(length(idx_A) > 0) sum_sp[[idx_A[1]]][[1]] else NULL
    sum_sp_W <- if(length(idx_W) > 0) sum_sp[[idx_W[1]]][[1]] else NULL
    
    get_F_p_sp <- function(term) {
      if (term == "A") {
        if (is.null(sum_sp_A) || !"A" %in% rownames(sum_sp_A)) {
          return(c(F=NA_real_, p=NA_real_))
        }
        c(
          F = sum_sp_A["A", "F value"],
          p = sum_sp_A["A", "Pr(>F)"]
        )
      } else {
        if (is.null(sum_sp_W) || !term %in% rownames(sum_sp_W)) {
          return(c(F=NA_real_, p=NA_real_))
        }
        c(
          F = sum_sp_W[term, "F value"],
          p = sum_sp_W[term, "Pr(>F)"]
        )
      }
    }
    
    get_F_p_lmm <- function(term) {
      if (!term %in% rownames(an_lmm_df)) return(c(F=NA_real_, p=NA_real_))
      c(
        F = an_lmm_df[term, "F.value"],
        p = an_lmm_df[term, "Pr(>F)"]
      )
    }
    
    # Construir tabla
    efectos <- c("A", "B", "A:B")
    etiquetas <- c("A (Riego)", "B (Nitrógeno)", "A×B")
    
    res <- lapply(seq_along(efectos), function(i) {
      term <- efectos[i]
      lab  <- etiquetas[i]
      
      fg <- get_F_p_glm(term)
      fs <- get_F_p_sp(term)
      fl <- get_F_p_lmm(term)
      
      # Usamos unname() para evitar que data.frame se confunda con nombres NA
      data.frame(
        Efecto          = lab,
        `F (GLM simple)`        = unname(fg["F"]),
        `p (GLM simple)`        = unname(fg["p"]),
        `F (ANOVA estrat.)`     = unname(fs["F"]),
        `p (ANOVA estrat.)`     = unname(fs["p"]),
        `F (LMM)`               = unname(fl["F"]),
        `p (LMM)`               = unname(fl["p"]),
        check.names = FALSE
      )
    })
    
    res_tab <- do.call(rbind, res)
    
    # Formateo leve para mostrar 3 decimales
    res_tab$`F (GLM simple)`    <- round(res_tab$`F (GLM simple)`, 3)
    res_tab$`p (GLM simple)`    <- signif(res_tab$`p (GLM simple)`, 3)
    res_tab$`F (ANOVA estrat.)` <- round(res_tab$`F (ANOVA estrat.)`, 3)
    res_tab$`p (ANOVA estrat.)` <- signif(res_tab$`p (ANOVA estrat.)`, 3)
    res_tab$`F (LMM)`           <- round(res_tab$`F (LMM)`, 3)
    res_tab$`p (LMM)`           <- signif(res_tab$`p (LMM)`, 3)
    
    res_tab
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  #--------------------------------------------------------------------
  # 2) GLM simple e ANOVA estratificado (salidas completas)
  #--------------------------------------------------------------------
  output$glm_bad_anova <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_glm <- stats::aov(y ~ Bloque + A*B, data = df)
    summary(mod_glm)
  })
  
  output$aov_strata <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_sp <- stats::aov(y ~ A*B + Error(Bloque/ParcelaA), data = df)
    summary(mod_sp)
  })
  
  #--------------------------------------------------------------------
  # 3) LMM: ANOVA Type III, VarCorr e ICCs
  #--------------------------------------------------------------------
  output$lmm_anova <- renderPrint({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("lmerTest", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(
      y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
      data = df,
      REML = TRUE
    )
    anova(mod_lmm, type = 3)  # Satterthwaite por defecto
  })
  
  output$lmm_varcorr <- renderPrint({
    df <- sim_react(); shiny::req(df)
    mod_lmm <- lmerTest::lmer(
      y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
      data = df,
      REML = TRUE
    )
    print(lme4::VarCorr(mod_lmm), comp = c("Variance", "Std.Dev."))
  })
  
  output$tabla_icc <- renderTable({
    df <- sim_react(); shiny::req(df)
    mod_lmm <- lmerTest::lmer(
      y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
      data = df,
      REML = TRUE
    )
    icc_from_varcorr(lme4::VarCorr(mod_lmm))
  }, striped = TRUE, bordered = TRUE, align = "c")
  
  #--------------------------------------------------------------------
  # 4) Tabla de medias A×B y gráficos (interacción + layout)
  #--------------------------------------------------------------------
  output$tab_means_ab <- DT::renderDataTable({
    df <- sim_react(); shiny::req(df)
    tab <- df |>
      dplyr::group_by(A, B) |>
      dplyr::summarise(
        n    = dplyr::n(),
        media = mean(y, na.rm = TRUE),
        .groups = "drop"
      )
    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = min(10, nrow(tab)),
        dom        = "tip"
      )
    )
  })
  
  output$plot_means <- renderPlot({
    df <- sim_react(); shiny::req(df)
    library(ggplot2)
    ggplot(df, aes(B, y, color = A, group = A)) +
      stat_summary(fun = mean, geom = "line", linewidth = 1) +
      stat_summary(fun = mean, geom = "point", size = 2) +
      labs(
        title = "Medias observadas por combinación A×B (caso balanceado)",
        x     = "B (Nitrógeno)",
        y     = "Respuesta media",
        color = "A (Riego)"
      ) +
      theme_bw()
  })
  
  output$plot_layout <- renderPlot({
    df <- sim_react(); shiny::req(df)
    library(ggplot2)
    df$Row <- as.numeric(df$Bloque)
    df$Col <- as.numeric(interaction(df$A, df$B, drop = TRUE))
    ggplot(df, aes(Col, Row, fill = A)) +
      geom_tile(color = "white") +
      geom_text(aes(label = B), size = 3) +
      scale_y_reverse(breaks = sort(unique(df$Row))) +
      labs(
        title = "Croquis ilustrativo (Bloque × ParcelaA × Subparcelas)",
        x     = "Combinación A:B",
        y     = "Bloque"
      ) +
      theme_minimal() +
      coord_fixed()
  })
  
  #--------------------------------------------------------------------
  # 5) Post-hoc con emmeans (comparaciones B|A y gráfico de EMMs)
  #--------------------------------------------------------------------
  output$emm_pairwise <- renderPrint({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("lmerTest", quietly = TRUE)
      requireNamespace("emmeans", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(
      y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
      data = df,
      REML = TRUE
    )
    print(emmeans::emmeans(mod_lmm, pairwise ~ B | A))
  })
  
  output$emm_plot <- renderPlot({
    df <- sim_react(); shiny::req(df)
    suppressPackageStartupMessages({
      requireNamespace("lmerTest", quietly = TRUE)
      requireNamespace("emmeans", quietly = TRUE)
    })
    mod_lmm <- lmerTest::lmer(
      y ~ A*B + (1 | Bloque) + (1 | Bloque:A),
      data = df,
      REML = TRUE
    )
    em <- emmeans::emmeans(mod_lmm, ~ A*B)
    tryCatch({
      plot(em, comparisons = TRUE)
    }, error = function(e) {
      showNotification("Nota: No se dibujan flechas de comparación (ajuste singular o varianza cero).", type = "warning")
      plot(em, comparisons = FALSE)
    })
  })
}

# Server de la Pestaña 4
pestanna4_session5_v3_server <- function(input, output, session) {
  
  # -------------------------------------------------------------------
  # 1) Simulador split-plot con faltantes (local a esta pestaña)
  # -------------------------------------------------------------------
  sim_splitplot_sp4 <- function(nB = 4, nA = 3, nBsub = 4,
                                effA = 4, effB = 6, effAB = 2,
                                sdA = 4, sdB = 2,
                                prop_miss = 0) {
    set.seed(1234)  # reproducibilidad pedagógica
    
    Bloque <- factor(paste0("B", seq_len(nB)))
    A      <- factor(paste0("A", seq_len(nA)))
    B      <- factor(paste0("N", seq_len(nBsub)))
    
    # Secuencias para generar efectos lineales/dosificables
    a_seq <- seq(0, 1, length.out = nA) - 0.5
    b_seq <- seq(0, 1, length.out = nBsub) - 0.5
    
    df <- expand.grid(
      Bloque = Bloque,
      A      = A,
      B      = B,
      KEEP.OUT.ATTRS = FALSE
    )
    df <- df[order(df$Bloque, df$A, df$B), , drop = FALSE]
    
    # Parcela principal = combinación Bloque×A
    df$ParcelaA <- interaction(df$Bloque, df$A, drop = TRUE)
    
    # Efectos fijos de A, B y A×B (centrados)
    mu    <- 50
    a_map <- stats::setNames(a_seq / max(1, abs(max(a_seq))), levels(A))
    b_map <- stats::setNames(b_seq / max(1, abs(max(b_seq))), levels(B))
    
    df$fx_A  <- a_map[as.character(df$A)] * effA
    df$fx_B  <- b_map[as.character(df$B)] * effB
    df$fx_AB <- (a_map[as.character(df$A)] * b_map[as.character(df$B)]) * effAB
    
    # Efectos aleatorios:
    #   - Aquí mantenemos Var(Bloque) = 0 (para simplificar el foco en ParcelaA),
    #     pero podrías darle una SD distinta de 0 si lo deseas.
    u_Bloque <- stats::rnorm(length(Bloque), mean = 0, sd = 0)
    names(u_Bloque) <- levels(Bloque)
    
    u_ParcelaA <- stats::rnorm(length(levels(df$ParcelaA)), mean = 0, sd = sdA)
    names(u_ParcelaA) <- levels(df$ParcelaA)
    
    # Error residual (subparcela)
    eps <- stats::rnorm(nrow(df), mean = 0, sd = sdB)
    
    df$y <- mu +
      df$fx_A + df$fx_B + df$fx_AB +
      u_Bloque[as.character(df$Bloque)] +
      u_ParcelaA[as.character(df$ParcelaA)] +
      eps
    
    # Introducir faltantes en y (subparcelas perdidas al azar)
    prop_miss <- max(0, min(1, prop_miss))
    if (prop_miss > 0) {
      n_miss <- floor(nrow(df) * prop_miss)
      if (n_miss > 0 && n_miss < nrow(df)) {
        idx <- sample(seq_len(nrow(df)), size = n_miss, replace = FALSE)
        df$y[idx] <- NA
        df <- df[!is.na(df$y), , drop = FALSE]
      }
    }
    
    rownames(df) <- NULL
    df
  }
  
  # Reactive: datos simulados para esta pestaña
  sp4_datos <- eventReactive(input$sp4_simular_fit, {
    d <- sim_splitplot_sp4(
      nB       = as.integer(input$sp4_n_bloques),
      nA       = as.integer(input$sp4_niv_A),
      nBsub    = as.integer(input$sp4_niv_B),
      effA     = input$sp4_eff_A,
      effB     = input$sp4_eff_B,
      effAB    = input$sp4_eff_AB,
      sdA      = input$sp4_sd_A,
      sdB      = input$sp4_sd_B,
      prop_miss = input$sp4_prop_miss
    )
    shiny::req(nrow(d) > 0)
    d
  }, ignoreInit = TRUE)
  
  # -------------------------------------------------------------------
  # 2) Datos & desbalance
  # -------------------------------------------------------------------
  output$sp4_tab_datos_head <- DT::renderDataTable({
    d <- sp4_datos(); shiny::req(d)
    DT::datatable(
      head(d, 10),
      options = list(pageLength = 10, dom = "tip"),
      rownames = FALSE
    )
  })
  
  output$sp4_tab_counts <- DT::renderDataTable({
    d <- sp4_datos(); shiny::req(d)
    counts <- d |>
      dplyr::count(Bloque, A, B, name = "n_obs") |>
      tidyr::pivot_wider(
        names_from  = c(A, B),
        values_from = n_obs,
        values_fill = 0
      )
    
    DT::datatable(
      counts,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -------------------------------------------------------------------
  # 3) Ajuste de modelos (GLM simple, ANOVA estratificado, LMM)
  # -------------------------------------------------------------------
  mod_glm_bad <- reactive({
    d <- sp4_datos(); shiny::req(d)
    stats::aov(y ~ Bloque + A * B, data = d)
  })
  
  mod_aov_strata <- reactive({
    d <- sp4_datos(); shiny::req(d)
    stats::aov(y ~ A * B + Error(Bloque / ParcelaA), data = d)
  })
  
  mod_lmm_sp4 <- reactive({
    d <- sp4_datos(); shiny::req(d)
    lmerTest::lmer(
      y ~ A * B + (1 | Bloque) + (1 | Bloque:A),
      data = d,
      REML = TRUE
    )
  })
  
  # Salidas completas
  output$sp4_glm_bad_anova <- renderPrint({
    m <- mod_glm_bad(); shiny::req(m)
    summary(m)
  })
  
  output$sp4_aov_strata <- renderPrint({
    m <- mod_aov_strata(); shiny::req(m)
    summary(m)
  })
  
  output$sp4_lmm_anova <- renderPrint({
    m <- mod_lmm_sp4(); shiny::req(m)
    anova(m, type = 3)
  })
  
  # -------------------------------------------------------------------
  # 4) Resumen del efecto A: p-valores en los tres enfoques
  # -------------------------------------------------------------------
  output$sp4_tab_resumen_A <- DT::renderDataTable({
    d  <- sp4_datos(); shiny::req(d)
    m1 <- mod_glm_bad()
    m2 <- mod_aov_strata()
    m3 <- mod_lmm_sp4()
    
    # 4.1 GLM simple: aov(y ~ Bloque + A*B)
    aov1_tbl <- broom::tidy(m1)
    pA_glm <- aov1_tbl |>
      dplyr::filter(term == "A") |>
      dplyr::pull(p.value)
    if (length(pA_glm) == 0) pA_glm <- NA_real_
    
    # 4.2 ANOVA estratificado: sacamos el p de A en el estrato Bloque:ParcelaA
    # summary(m2) devuelve una lista; buscamos el componente correspondiente a Error: Bloque:ParcelaA
    summ2 <- summary(m2)
    # Por simplicidad pedagógica: intentamos extraer la tabla "A" del estrato Bloque:ParcelaA
    pA_aov_strata <- NA_real_
    for (comp in summ2) {
      if (inherits(comp, "anova")) {
        rn <- rownames(comp)
        if ("A" %in% rn) {
          pA_aov_strata <- comp["A", "Pr(>F)"]
        }
      }
    }
    
    # 4.3 LMM: ANOVA Type III (Satterthwaite)
    an_lmm <- suppressMessages(anova(m3, type = 3))
    an_lmm_df <- as.data.frame(an_lmm)
    if ("A" %in% rownames(an_lmm_df)) {
      pA_lmm <- an_lmm_df["A", "Pr(>F)"]
    } else {
      pA_lmm <- NA_real_
    }
    
    resumen <- tibble::tibble(
      Enfoque = c(
        "GLM simple (aov: y ~ Bloque + A*B)",
        "ANOVA estratificado (Error(Bloque/ParcelaA))",
        "LMM (lmer: y ~ A*B + (1|Bloque) + (1|Bloque:A))"
      ),
      `Tratamiento A (Riego)` = c("Fijo", "Fijo", "Fijo"),
      `Error usado para A` = c(
        "Residual (Error B, incorrecto)",
        "Estrato Bloque:ParcelaA (Error A)",
        "Componente Var(Bloque:A) (Error A)"
      ),
      `p-valor de A` = c(pA_glm, pA_aov_strata, pA_lmm)
    )
    
    resumen$`← Recomendado` <- c("", "✓ (caso balanceado)", "★ (general, robusto)")
    
    DT::datatable(
      resumen,
      rownames = FALSE,
      options  = list(pageLength = 3, dom = "t")
    )
  })
  
  # -------------------------------------------------------------------
  # 5) VarCorr, ICC de ParcelaA y mensaje interpretativo
  # -------------------------------------------------------------------
  output$sp4_varcorr <- renderPrint({
    m <- mod_lmm_sp4(); shiny::req(m)
    print(lme4::VarCorr(m), comp = c("Variance", "Std.Dev."))
  })
  
  output$sp4_icc_msg <- renderPrint({
    m  <- mod_lmm_sp4(); shiny::req(m)
    vc <- as.data.frame(lme4::VarCorr(m))
    
    # Extraer σ²_ParcelaA (grupo "Bloque:A") y σ²_residual
    sig_parcelaA <- vc$vcov[vc$grp == "Bloque:A" & vc$var1 == "(Intercept)"]
    sig_resid    <- vc$vcov[vc$grp == "Residual"]
    
    if (length(sig_parcelaA) == 0 || length(sig_resid) == 0) {
      cat("No se pudieron extraer σ²_ParcelaA y/o σ²_residual desde VarCorr.\n")
      return(invisible(NULL))
    }
    
    ICC_parcelaA <- sig_parcelaA / (sig_parcelaA + sig_resid)
    
    cat(sprintf(
      "ICC ParcelaA = %.3f → aproximadamente %.1f%% de la varianza total (a nivel subparcela)\n",
      ICC_parcelaA, 100 * ICC_parcelaA
    ))
    
    if (!is.finite(ICC_parcelaA)) {
      cat("Interpretación: ICC no finito (revisar componentes de varianza).\n")
    } else if (ICC_parcelaA < 0.05) {
      cat("Interpretación: la variación entre parcelas principales es muy baja; el diseño se parece a un DBCA.\n")
    } else if (ICC_parcelaA < 0.20) {
      cat("Interpretación: la variación entre parcelas principales es moderada; conviene respetar la estructura split-plot.\n")
    } else {
      cat("Interpretación: la variación entre parcelas principales es alta; tratar el ensayo como DBCA (GLM simple)\n",
          "equivale a ignorar una fuerte correlación intra-parcela, con riesgo de F(A) inflado y falsos positivos.\n")
    }
  })
}

# Server de la Pestaña 5
pestanna5_session5_v3_server <- function(input, output, session, ex_df, icc_from_varcorr) {
  # Nota: icc_from_varcorr se mantiene en la firma por compatibilidad,
  # pero ya no es necesario dentro de esta pestaña.
  
  # --------------------------------------------------------------
  # 1) Vista rápida de datos simulados
  # --------------------------------------------------------------
  output$sp5_ex_tab_head <- DT::renderDataTable({
    d <- ex_df()
    shiny::req(d, nrow(d) > 0)
    
    DT::datatable(
      head(d, 10),
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  
  output$sp5_ex_tab_counts <- DT::renderDataTable({
    d <- ex_df()
    shiny::req(d, nrow(d) > 0)
    
    # Verificamos que existan las columnas clave
    cols_needed <- c("Bloque", "A", "B")
    if (!all(cols_needed %in% names(d))) {
      msg <- paste0(
        "El data frame no contiene todas las columnas requeridas: ",
        paste(cols_needed, collapse = ", "), "."
      )
      return(DT::datatable(
        data.frame(mensaje = msg),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    tab_counts <- d |>
      dplyr::count(Bloque, A, B, name = "n") |>
      tidyr::pivot_wider(
        names_from  = c(A, B),
        values_from = n,
        values_fill = 0,
        names_sep   = ":"
      )
    
    DT::datatable(
      tab_counts,
      rownames = FALSE,
      options = list(pageLength = 10, dom = "tip", scrollX = TRUE)
    )
  })
  
  # --------------------------------------------------------------
  # 2) Botón de descarga de CSV (datos simulados)
  # --------------------------------------------------------------
  output$sp5_dl_ex_csv_ui <- renderUI({
    shiny::req(ex_df(), nrow(ex_df()) > 0)
    downloadButton(
      session$ns("sp5_dl_ex_csv"),
      "Descargar datos simulados (CSV)",
      class = "btn-outline-secondary w-100"
    )
  })
  
  output$sp5_dl_ex_csv <- downloadHandler(
    filename = function() {
      paste0("splitplot_ejercicio_", Sys.Date(), ".csv")
    },
    content = function(file) {
      d <- ex_df()
      shiny::req(d, nrow(d) > 0)
      utils::write.csv(d, file, row.names = FALSE)
    }
  )
}

# Server de la Pestaña 6
pestanna6_session5_v3_server <- function(input, output, session) {
  # Pestaña puramente conceptual/explicativa.
  # No se requiere lógica de servidor.
}

# Server de la Pestaña Extra
pestanna_extra_session5_v3_server <- function(input, output, session) {
  # Esta pestaña es estática y educativa.
  # No requiere lógica de servidor dinámica.
  return(NULL)
}

# Server de la Pestaña 7
pestanna7_session5_v3_server <- function(input, output, session) {
  # Pestaña puramente informativa (sin lógica de servidor).
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session5_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  sim_splitplot <- function(nB = 4, nA = 3, nBsub = 4,
                            effA = 4, effB = 6, effAB = 2,
                            sdA = 4, sdB = 2,
                            prop_miss = 0
                          ) {
    set.seed(1234)
    Bloque <- factor(paste0("B", seq_len(nB)))
    A <- factor(paste0("A", seq_len(nA)))
    B <- factor(paste0("N", seq_len(nBsub)))

    a_seq <- seq(0, 1, length.out = nA) - 0.5
    b_seq <- seq(0, 1, length.out = nBsub) - 0.5

    df <- expand.grid(Bloque = Bloque, A = A, B = B, KEEP.OUT.ATTRS = FALSE)
    df <- df[order(df$Bloque, df$A, df$B), , drop = FALSE]
    df$ParcelaA <- interaction(df$Bloque, df$A, drop = TRUE)

    mu <- 50
    a_map <- setNames(a_seq / max(1, abs(max(a_seq))), levels(A))
    b_map <- setNames(b_seq / max(1, abs(max(b_seq))), levels(B))

    df$fx_A  <- a_map[as.character(df$A)] * effA
    df$fx_B  <- b_map[as.character(df$B)] * effB
    df$fx_AB <- (a_map[as.character(df$A)] * b_map[as.character(df$B)]) * effAB

    u_Bloque   <- rnorm(nB, 0, sd = 0)
    names(u_Bloque) <- levels(Bloque)
    u_ParcelaA <- rnorm(length(levels(df$ParcelaA)), 0, sd = sdA)
    names(u_ParcelaA) <- levels(df$ParcelaA)

    df$y <- mu +
      df$fx_A + df$fx_B + df$fx_AB +
      u_Bloque[as.character(df$Bloque)] +
      u_ParcelaA[as.character(df$ParcelaA)] +
      rnorm(nrow(df), 0, sd = sdB)

    if (prop_miss > 0) {
      n_miss <- floor(nrow(df) * prop_miss)
      idx <- sample(seq_len(nrow(df)), n_miss)
      df$y[idx] <- NA
      df <- df[!is.na(df$y), , drop = FALSE]
    }
    rownames(df) <- NULL
    df
  }

  icc_from_varcorr <- function(vlist) {
    vc <- as.data.frame(vlist)
    get_var <- function(grp) {
      x <- vc[vc$grp == grp & vc$var1 == "(Intercept)", "vcov"]
      if (length(x) == 0) 0 else x[1]
    }
    v_blockA <- get_var("Bloque:A")
    v_block  <- get_var("Bloque")
    v_resid  <- get_var("Residual")
    v_tot_PA <- v_blockA + v_resid
    v_tot_all <- v_block + v_blockA + v_resid

    data.frame(
      Nivel = c("ICC ParcelaA (entre subparcelas misma parcelaA)",
                "ICC Bloque (entre parcelasA mismo bloque)"),
      Formula = c("σ²_ParcelaA / (σ²_ParcelaA + σ²_Residual)",
                  "(σ²_Bloque + σ²_ParcelaA) / (σ²_Bloque + σ²_ParcelaA + σ²_Residual)"),
      ICC = c(ifelse(v_tot_PA > 0, v_blockA / v_tot_PA, NA_real_),
              ifelse(v_tot_all > 0, (v_block + v_blockA) / v_tot_all, NA_real_))
    )
  }

  # Reactives
  sim_react <- eventReactive(input$simular_fit, {
    df <- sim_splitplot(
      nB     = as.integer(input$n_bloques),
      nA     = as.integer(input$niv_A),
      nBsub  = as.integer(input$niv_B),
      effA   = input$eff_A,
      effB   = input$eff_B,
      effAB  = input$eff_AB,
      sdA    = input$sd_A,
      sdB    = input$sd_B,
      prop_miss = input$prop_miss
    )
    shiny::req(nrow(df) > 0)
    df
  })

  ex_df <- eventReactive(input$ex_run, {
    sim_splitplot(
      nB = input$ex_nb,
      nA = input$ex_na,
      nBsub = input$ex_nbsub,
      effA = input$ex_effA,
      effB = input$ex_effB,
      effAB = input$ex_effAB,
      sdA = input$ex_sdA,
      sdB = input$ex_sdB,
      prop_miss = input$ex_miss / 100
    )
  })

  # Call tab servers
  pestanna1_session5_v3_server(input, output, session)
  pestanna2_session5_v3_server(input, output, session)
  pestanna3_session5_v3_server(input, output, session, sim_react, icc_from_varcorr)
  pestanna4_session5_v3_server(input, output, session)
  pestanna5_session5_v3_server(input, output, session, ex_df, icc_from_varcorr)
  pestanna6_session5_v3_server(input, output, session)
  pestanna_extra_session5_v3_server(input, output, session)
  pestanna7_session5_v3_server(input, output, session)
}
