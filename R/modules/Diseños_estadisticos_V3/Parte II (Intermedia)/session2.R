# R/modules/Diseños_estadisticos_V3/Parte II (Intermedia)/session2.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Contexto y objetivos
pestanna1_session2_v3UI <- function(ns) {
  nav_panel(
    title = "1. Contexto y objetivos",
    # Usamos MathJax para las expresiones de los modelos
    withMathJax(
      h4(class = "section-header", "Motivación agronómica: variabilidad y bloqueo"),
      p(
        "En la práctica agronómica casi nunca trabajamos en condiciones perfectamente homogéneas. ",
        "Suelos con gradiente de fertilidad, pendientes, cambios de humedad o sombra generan ",
        strong("variación no interesante"),
        " que inflan el error experimental si no se controla."
      ),
      p(
        "El objetivo de esta sesión es comparar dos estrategias clásicas para asignar tratamientos:",
        strong(" Diseño Completamente al Azar (DCA/CRD) "),
        "y",
        strong(" Diseño en Bloques Completos al Azar (RCBD)"),
        ", y entender cómo afectan la precisión (", em("CM(Error)"), ") y la potencia de los contrastes."
      ),

      hr(),

      div(class = "row g-3",
        # Columna izquierda: DCA/CRD
        div(class = "col-md-6",
          div(class = "card shadow-sm",
            div(class = "card-header bg-light",
              strong("DCA / CRD: cuando el ambiente es ~homogéneo")
            ),
            div(class = "card-body",
              p(
                "En un ", strong("Diseño Completamente al Azar (DCA/CRD)"),
                " las unidades experimentales se consideran ",
                em("intercambiables"),
                " respecto a fuentes de variación sistemática. ",
                "Los tratamientos se asignan al azar sobre todo el conjunto."
              ),
              tags$ul(
                tags$li("Útil en invernadero, bancales muy uniformes o ensayos de laboratorio."),
                tags$li("Sencillo de implementar y analizar (ANOVA de una vía)."),
                tags$li("Sensibilidad alta a gradientes no modelados de suelo, pendiente, etc.")
              ),
              tags$p(class = "mt-2 mb-1 text-muted small", strong("Modelo lineal (CRD):")),
              tags$p(
                class = "small",
                "$$Y_{ij} = \\mu + \\tau_i + \\varepsilon_{ij}, \\quad \\varepsilon_{ij} \\sim \\text{iid } N(0, \\sigma^2)$$"
              ),
              tags$small(
                em("Ver, por ejemplo, NIST/SEMATECH e-Handbook — Completely Randomized Design (CRD).")
              )
            )
          )
        ),

        # Columna derecha: RCBD
        div(class = "col-md-6",
          div(class = "card shadow-sm",
            div(class = "card-header bg-light",
              strong("RCBD: bloquear para ganar precisión")
            ),
            div(class = "card-body",
              p(
                "Cuando sabemos o sospechamos la existencia de un gradiente (p. ej. fertilidad, ",
                "profundidad efectiva de suelo, posición en ladera), usamos un ",
                strong("Diseño en Bloques Completos al Azar (RCBD)"),
                " para comparar tratamientos ",
                em("dentro de grupos relativamente homogéneos"),
                "."
              ),
              tags$ul(
                tags$li("Cada bloque contiene todas las alternativas de tratamiento (bloques completos)."),
                tags$li("El factor bloque se incluye en el modelo para extraer variación no interesante."),
                tags$li("Suele reducir la ", strong("CM(Error)"), " y aumentar la potencia cuando el bloqueo es pertinente.")
              ),
              tags$p(class = "mt-2 mb-1 text-muted small", strong("Modelo lineal (RCBD):")),
              tags$p(
                class = "small",
                "$$Y_{ij} = \\mu + \\beta_j + \\tau_i + \\varepsilon_{ij}, \\quad \\varepsilon_{ij} \\sim \\text{iid } N(0, \\sigma^2)$$",
                " donde ", "\\(\\beta_j\\) representa el efecto del bloque ", em("(no nos interesa en sí mismo).")
              ),
              tags$small(
                em("Ver textos clásicos de diseño experimental en agricultura, p. ej. Montgomery o Gómez & Gómez.")
              )
            )
          )
        )
      ),

      hr(),

      h4(class = "section-header", "Supuestos, unidad experimental y tamaño del efecto"),
      div(class = "row g-3",
        div(class = "col-md-8",
          tags$ul(
            tags$li(
              strong("Unidad experimental:"),
              " es la unidad física a la que se asigna un tratamiento de manera independiente ",
              "(parcela, maceta, animal). Toda la inferencia se refiere a esta unidad."
            ),
            tags$li(
              strong("Supuestos de ANOVA:"),
              " residuos aproximadamente normales, varianzas homogéneas e independencia. ",
              "En la sesión aplicamos diagnósticos estándar (residuos vs ajustados, QQ-plot) ",
              "y pruebas como Shapiro–Wilk y Levene."
            ),
            tags$li(
              strong("Tamaño del efecto:"),
              " complementa al p-valor. Se sugiere reportar ",
              code("eta^2"),
              " o ",
              code("eta^2 parcial"),
              " (por ejemplo, con ",
              code("effectsize::eta_squared()"),
              ")."
            )
          ),
          tags$small(
            a("afex: ANOVA en R y supuestos",
              href = "https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html",
              target = "_blank"
            )
          ), br(),
          tags$small(
            a("effectsize::eta_squared()",
              href = "https://cran.r-project.org/web/packages/effectsize/index.html",
              target = "_blank"
            )
          )
        ),
        div(class = "col-md-4",
          div(class = "card border-0 bg-light",
            div(class = "card-header bg-light",
              strong("Resultado de aprendizaje (resumen)")
            ),
            div(class = "card-body small",
              tags$ul(
                tags$li("Distinguir conceptualmente DCA vs RCBD a partir del contexto agronómico."),
                tags$li("Escribir e interpretar los modelos lineales asociados a cada diseño."),
                tags$li("Relacionar el uso de bloques con la reducción de CM(Error) y el aumento de potencia."),
                tags$li("Interpretar resultados de ANOVA y tamaños de efecto en ejemplos simulados.")
              )
            )
          )
        )
      ),

      hr(),

      h4(class = "section-header", "Mapa de la sesión 2"),
      tags$ol(
        tags$li(
          strong("Pestaña 2 — DCA:"),
          " construir un CRD, simular datos y analizar con ANOVA de una vía (", code("aov()"), ")."
        ),
        tags$li(
          strong("Pestaña 3 — Heterogeneidad simulada:"),
          " introducir un gradiente de bloque y comparar modelos DCA vs RCBD."
        ),
        tags$li(
          strong("Pestaña 4 — RCBD:"),
          " generar un plan realista con ",
          code("agricolae::design.rcbd()"),
          " y analizar el ANOVA con bloqueo."
        ),
        tags$li(
          strong("Pestaña 5 — Eficiencia:"),
          " cuantificar la ganancia de precisión con la ",
          em("Eficiencia Relativa (RE)"),
          " entre RCBD y DCA."
        )
      ),

      br(),
      tags$small(
        em("Referencias sugeridas para profundizar: NIST/SEMATECH e-Handbook (CRD/RCBD) y STAT Online (Penn State, lecciones de diseños completamente al azar y en bloques).")
      )
    )
  )
}

# Pestaña 2: DCA — diseño y análisis
pestanna2_session2_v3UI <- function(ns) {
  nav_panel(
    title = "2. DCA: diseño y análisis",
    withMathJax(
      h4(class = "section-header", "Diseño Completamente al Azar (DCA/CRD): del plan al ANOVA"),
      p(
        "En esta pestaña configuras un ",
        strong("Diseño Completamente al Azar (DCA/CRD)"),
        ", simulas la respuesta y analizas el modelo ",
        code("aov(y ~ tratamiento)"),
        " junto con el tamaño del efecto ",
        HTML("&eta;^2"),
        "."
      ),
      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 1 · Configura el diseño CRD"
          ),
          numericInput(
            ns("dca_k"), "N° tratamientos (k):",
            value = 3, min = 2, max = 12, step = 1
          ),
          numericInput(
            ns("dca_r"), "Replicaciones por tratamiento (r):",
            value = 5, min = 2, max = 30, step = 1
          ),
          tags$hr(),
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 2 · Parámetros de simulación"
          ),
          sliderInput(
            ns("dca_diff"), "Rango de diferencias de medias (0 = iguales):",
            min = 0, max = 5, value = 1.5, step = 0.1
          ),
          sliderInput(
            ns("dca_sigma"), "Desv. estándar del error (σ):",
            min = 0.1, max = 5, value = 1, step = 0.1
          ),
          numericInput(
            ns("dca_mu"), "Media base (μ):",
            value = 20, min = -1e3, max = 1e3
          ),
          div(class = "mt-3 mb-2",
              actionButton(ns("dca_go"), "Generar & Analizar", icon = icon("play"))
          ),
          div(
            class = "small text-muted",
            "Cada clic genera un nuevo conjunto simulado con el mismo diseño.",
            br(),
            textOutput(ns("dca_design_summary"), inline = FALSE)
          )
        ),

        mainPanel(
          width = 9,

          # Bloque 1: Layout + distribución por tratamiento
          div(class = "row g-3",
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Layout CRD (orden aleatorio)")
                ),
                div(class = "card-body",
                  plotOutput(ns("dca_layout_plot"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "Cada celda representa una unidad experimental; los tratamientos se asignan ",
                    "aleatoriamente sobre todas las parcelas."
                  )
                )
              )
            ),
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Respuesta por tratamiento")
                ),
                div(class = "card-body",
                  plotOutput(ns("dca_box"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "El boxplot resume la distribución de la respuesta simulada por tratamiento ",
                    "bajo el modelo DCA/CRD."
                  )
                )
              )
            )
          ),

          hr(),

          # Bloque 2: ANOVA + tamaño del efecto
          h5("ANOVA (aov) y tamaño del efecto ", HTML("(&eta;^2)")),
          div(class = "card shadow-sm mb-3",
            div(class = "card-body",
              div(class = "row g-3",
                div(class = "col-lg-7",
                  tableOutput(ns("dca_anova_tbl"))
                ),
                div(class = "col-lg-5",
                  div(class = "small text-muted mb-1", "Resumen del efecto de tratamiento"),
                  textOutput(ns("dca_anova_caption")),
                  tags$hr(class = "my-2"),
                  tags$strong("Tamaño del efecto (&eta;^2):"),
                  verbatimTextOutput(ns("dca_eta"))
                )
              )
            )
          ),

          # Bloque 3: Diagnóstico de supuestos
          h5("Diagnóstico de supuestos"),
          div(class = "row g-3",
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Residuos vs ajustados")
                ),
                div(class = "card-body",
                  plotOutput(ns("dca_resid_vs_fit"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "Buscamos ausencia de patrón sistemático; la banda LOESS ayuda a detectar ",
                    "curvaturas o heterocedasticidad."
                  )
                )
              )
            ),
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("QQ-plot de residuos")
                ),
                div(class = "card-body",
                  plotOutput(ns("dca_qq"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "Si los puntos siguen aproximadamente la línea, la normalidad de los residuos ",
                    "es razonable para el ANOVA."
                  )
                )
              )
            )
          ),
          div(class = "row g-3 mt-3",
            div(class = "col-md-6",
              tags$strong("Prueba de normalidad (Shapiro–Wilk)"),
              verbatimTextOutput(ns("dca_shapiro"))
            ),
            div(class = "col-md-6",
              tags$strong("Homogeneidad de varianzas (Levene)"),
              verbatimTextOutput(ns("dca_levene"))
            )
          )
        )
      )
    )
  )
}

# Pestaña 3: Heterogeneidad simulada
pestanna3_session2_v3UI <- function(ns) {
  nav_panel(
    title = "3. Heterogeneidad simulada (¿por qué RCBD?)",
    withMathJax(
      h4(class = "section-header", "Heterogeneidad simulada: DCA vs RCBD"),
      p(
        "En esta pestaña se simula un campo con gradiente de bloque. ",
        "Primero se ajusta un modelo que ", strong("ignora los bloques"),
        " (DCA: ", code("aov(y ~ tratamiento)"), "), y luego un modelo que ",
        "los incorpora explícitamente (RCBD: ", code("aov(y ~ tratamiento + bloque)"), ")."
      ),
      p(
        class = "small text-muted",
        "La idea central: si existe un gradiente entre bloques, el RCBD debería ",
        "reducir la ", strong("CM(Error)"), " y, en consecuencia, aumentar la potencia ",
        "para detectar diferencias de tratamiento."
      ),

      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 1 · Estructura del campo"
          ),
          numericInput(
            ns("het_k"), "N° tratamientos (k):",
            value = 4, min = 2, max = 10
          ),
          numericInput(
            ns("het_b"), "N° bloques (b):",
            value = 4, min = 2, max = 12
          ),
          tags$hr(),
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 2 · Gradiente y efecto de tratamientos"
          ),
          sliderInput(
            ns("het_grad"), "Fuerza del gradiente de bloque:",
            min = 0, max = 10, value = 4, step = 0.5
          ),
          sliderInput(
            ns("het_diff"), "Rango de diferencias de tratamiento:",
            min = 0, max = 5, value = 2, step = 0.1
          ),
          sliderInput(
            ns("het_sigma"), "Desv. estándar del error (σ):",
            min = 0.1, max = 5, value = 1.2, step = 0.1
          ),
          div(class = "mt-3 mb-2",
              actionButton(ns("het_go"), "Simular & Comparar", icon = icon("chart-line"))
          ),
          div(
            class = "small text-muted",
            "Cada clic genera un nuevo ensayo simulado con el mismo diseño.",
            br(),
            textOutput(ns("het_summary"), inline = FALSE)
          )
        ),

        mainPanel(
          width = 9,

          # Bloque 1: mapa de calor + distribución por tratamiento
          div(class = "row g-3",
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Mapa de calor (gradiente por bloque)")
                ),
                div(class = "card-body",
                  plotOutput(ns("het_heat"), height = "280px"),
                  p(class = "small text-muted mb-0",
                    "Cada celda es una combinación bloque × tratamiento. ",
                    "El gradiente de color refleja cómo cambia la respuesta media entre bloques."
                  )
                )
              )
            ),
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Distribución por tratamiento")
                ),
                div(class = "card-body",
                  plotOutput(ns("het_box_by_trt"), height = "280px"),
                  p(class = "small text-muted mb-0",
                    "Los boxplots muestran la variación dentro y entre tratamientos cuando ",
                    "hay un gradiente de bloque en el campo."
                  )
                )
              )
            )
          ),

          hr(),

          # Bloque 2: comparación de modelos DCA vs RCBD
          h5("Comparación de modelos (tratamiento)"),
          div(class = "card shadow-sm mb-3",
            div(class = "card-body",
              tags$table(class = "table table-bordered table-sm mb-2",
                tags$thead(
                  tags$tr(
                    tags$th("Modelo"),
                    tags$th("F (trat)"),
                    tags$th("p-valor (trat)"),
                    tags$th("CM(Error)")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("DCA: aov(y ~ tratamiento)"),
                    tags$td(textOutput(ns("het_F_dca"))),
                    tags$td(textOutput(ns("het_p_dca"))),
                    tags$td(textOutput(ns("het_mse_dca")))
                  ),
                  tags$tr(
                    tags$td("RCBD: aov(y ~ tratamiento + bloque)"),
                    tags$td(textOutput(ns("het_F_rcbd"))),
                    tags$td(textOutput(ns("het_p_rcbd"))),
                    tags$td(textOutput(ns("het_mse_rcbd")))
                  )
                )
              ),
              div(
                class = "small text-muted",
                textOutput(ns("het_mse_compare_msg"), inline = FALSE)
              )
            )
          ),

          # Bloque 3: diagnóstico del modelo RCBD
          h5("Diagnóstico del modelo RCBD"),
          div(class = "row g-3",
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("Residuos vs ajustados (RCBD)")
                ),
                div(class = "card-body",
                  plotOutput(ns("het_resid_vs_fit_rcbd"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "Verificamos ausencia de patrón sistemático en los residuos; ",
                    "si el bloqueo captura bien el gradiente, la nube debería verse más aleatoria."
                  )
                )
              )
            ),
            div(class = "col-md-6",
              div(class = "card shadow-sm h-100",
                div(class = "card-header bg-light",
                  strong("QQ-plot de residuos (RCBD)")
                ),
                div(class = "card-body",
                  plotOutput(ns("het_qq_rcbd"), height = "260px"),
                  p(class = "small text-muted mb-0",
                    "Comprobamos la plausibilidad de la normalidad de residuos en el modelo con bloques."
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

# Pestaña 4: RCBD — diseño y análisis
pestanna4_session2_v3UI <- function(ns) {
  nav_panel(
    title = "4. RCBD: diseño y análisis",
    withMathJax(
      h4(class = "section-header", "RCBD con agricolae::design.rcbd() + ANOVA"),
      p(
        "En esta pestaña se construye un ",
        strong("Diseño de Bloques Completos al Azar (RCBD)"),
        " usando ", code("agricolae::design.rcbd()"), ". Cada bloque contiene ",
        em("todos"), " los tratamientos, y dentro de cada bloque se aleatoriza el orden ",
        "de las unidades experimentales."
      ),
      p(
        class = "small text-muted",
        "El objeto devuelto por ", code("design.rcbd()"),
        " incluye un ", code("fieldbook (out$book)"),
        " que actúa como plan de campo: columnas típicas para el bloque (",
        code("block"), "), el tratamiento (p. ej. ", code("trt"), " o similares) ",
        "y el identificador de parcela (", code("plots"), "). Esta pestaña estandariza ",
        "esos nombres para que funcionen distintas versiones de ", code("agricolae"), "."
      ),

      sidebarLayout(
        sidebarPanel(
          width = 3,
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 1 · Parámetros del diseño"
          ),
          numericInput(
            ns("rcbd_k"), "N° tratamientos (k):",
            value = 4, min = 2, max = 12, step = 1
          ),
          numericInput(
            ns("rcbd_b"), "N° bloques (b):",
            value = 4, min = 2, max = 12, step = 1
          ),
          tags$hr(),
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 2 · Diferencias y variabilidad"
          ),
          sliderInput(
            ns("rcbd_diff"), "Rango de diferencias de tratamiento:",
            min = 0, max = 5, value = 2, step = 0.1
          ),
          sliderInput(
            ns("rcbd_sigma"), "Desv. estándar del error (σ):",
            min = 0.1, max = 5, value = 1.2, step = 0.1
          ),
          div(
            class = "mt-3 mb-2",
            actionButton(ns("rcbd_go"), "Generar & Analizar", icon = icon("shuffle"))
          ),
          div(
            class = "small text-muted",
            textOutput(ns("rcbd_summary"), inline = FALSE)
          )
        ),

        mainPanel(
          width = 9,

          # Bloque 1: Layout + fieldbook
          div(
            class = "row g-3",
            div(
              class = "col-md-6",
              div(
                class = "card shadow-sm h-100",
                div(
                  class = "card-header bg-light",
                  strong("Esquema de campo (RCBD)")
                ),
                div(
                  class = "card-body",
                  uiOutput(ns("rcbd_layout_ui")),
                  p(
                    class = "small text-muted mb-0",
                    "Cada columna representa un bloque; cada celda, una unidad experimental ",
                    "con su tratamiento. En RCBD, cada bloque contiene todos los tratamientos una vez."
                  )
                )
              )
            ),
            div(
              class = "col-md-6",
              div(
                class = "card shadow-sm h-100",
                div(
                  class = "card-header bg-light",
                  strong("Fieldbook del diseño (primeras filas)")
                ),
                div(
                  class = "card-body",
                  tableOutput(ns("rcbd_book_head")),
                  p(
                    class = "small text-muted mb-0",
                    "Esta tabla resume la estructura del diseño: bloque, tratamiento, parcela y ",
                    "la respuesta simulada ", code("y"), ". En un ensayo real sustituirías ",
                    "esta respuesta simulada por tus datos observados."
                  )
                )
              )
            )
          ),

          hr(),

          # Bloque 2: ANOVA y tamaño del efecto
          h5("ANOVA (RCBD) y tamaño del efecto del tratamiento"),
          div(
            class = "card shadow-sm mb-3",
            div(
              class = "card-body",
              tableOutput(ns("rcbd_anova_tbl")),
              div(
                class = "row mt-3",
                div(
                  class = "col-md-6",
                  tags$strong("Salida de effectsize::eta_squared()"),
                  verbatimTextOutput(ns("rcbd_eta"))
                ),
                div(
                  class = "col-md-6",
                  tags$strong("Lectura rápida del tamaño del efecto"),
                  p(
                    class = "small text-muted",
                    "Regla de referencia (Cohen): ",
                    "η² parcial ≈ 0.01 (pequeño), 0.06 (medio), 0.14 (grande)."
                  ),
                  textOutput(ns("rcbd_eta_msg"))
                )
              )
            )
          ),

          # Bloque 3: Comparaciones múltiples (Tukey)
          h5("Comparaciones múltiples (Tukey sobre tratamientos)"),
          div(
            class = "card shadow-sm",
            div(
              class = "card-body",
              div(
                class = "row g-3",
                div(
                  class = "col-md-6",
                  plotOutput(ns("rcbd_tukey_plot"), height = "280px")
                ),
                div(
                  class = "col-md-6",
                  verbatimTextOutput(ns("rcbd_tukey_text")),
                  p(
                    class = "small text-muted mb-0",
                    "Tukey se calcula sólo cuando el efecto de tratamiento en el ANOVA RCBD ",
                    "es significativo (p < 0.05). Puedes jugar con el ",
                    code("rango de diferencias"), " y la ", code("σ"),
                    " para ver distintos escenarios."
                  )
                )
              )
            )
          ),

          br(),
          p(
            class = "small text-muted",
            "Nota: este flujo usa ", code("design.rcbd()"),
            " como generador de plan de campo. Para análisis más avanzados ",
            "(p. ej., bloques como efectos aleatorios, varios ambientes, datos desbalanceados), ",
            "la extensión natural es un modelo mixto (REML) con paquetes como ",
            code("lme4"), " o herramientas específicas de mejoramiento genético."
          )
        )
      )
    )
  )
}

# Pestaña 5: Eficiencia y varianzas (UI mejorada)
pestanna5_session2_v3UI <- function(ns) {
  nav_panel(
    title = "5. Eficiencia y varianzas",
    withMathJax(
      h4(class = "section-header", "¿Valió la pena bloquear?"),

      p(
        "En esta pestaña se resume numéricamente el beneficio de bloquear. ",
        "Usamos la simulación de la pestaña 3 (mismo conjunto de datos) ",
        "para comparar la precisión de:",
        tags$ul(
          tags$li(strong("DCA / CRD"), " (analizar ignorando bloques)."),
          tags$li(strong("RCBD"), " (analizar respetando el diseño en bloques).")
        )
      ),
      
      bs_callout(
        title = "Teoría: Eficiencia Relativa",
        type = "info",
        "La Eficiencia Relativa (RE) estima cuántas repeticiones necesitaría un DCA para igualar la precisión de un RCBD. ",
        "Si RE = 1.5, significa que un RCBD con 4 bloques equivale a un DCA con 4 * 1.5 = 6 repeticiones. ",
        "Bloquear es 'gratis' en términos de recursos si el gradiente existe, pues ganamos potencia sin aumentar n."
      ),

      p(
        "La comparación se hace a partir del ",
        strong("Cuadrado Medio del Error (CM(Error))"),
        " de ambos modelos y una medida práctica de ",
        strong("Eficiencia Relativa (RE)"),
        " del RCBD respecto al DCA:"
      ),
      div(
        class = "mb-3",
        tags$code("RE_{RCBD:DCA} \\approx \\dfrac{CM_e(\\text{DCA})}{CM_e(\\text{RCBD})}"),
        br(),
        tags$span(
          class = "small text-muted",
          "Si RE > 1, el RCBD logra menor CM(Error) que el DCA (bloquear fue útil); ",
          "si RE ≈ 1, el bloqueo no cambió mucho la precisión; ",
          "si RE < 1, en esta simulación el bloqueo no ayudó."
        )
      ),

      fluidRow(
        # Columna izquierda: tabla comparativa
        column(
          width = 7,
          div(
            class = "card shadow-sm mb-3",
            div(class = "card-header bg-light",
                strong("Comparación de CM(Error) entre modelos")
            ),
            div(
              class = "card-body",
              tableOutput(ns("eff_table")),
              p(
                class = "small text-muted mb-0",
                "Ambos modelos usan exactamente los mismos datos simulados; ",
                "la diferencia está en si se incluye o no el factor bloque en el modelo."
              )
            )
          )
        ),

        # Columna derecha: panel de interpretación de RE
        column(
          width = 5,
          div(
            class = "card shadow-sm mb-3 text-center",
            div(class = "card-header bg-light",
                strong("Eficiencia Relativa (RE) del RCBD")
            ),
            div(
              class = "card-body",
              div(
                style = "font-size: 2.6em; font-weight: 700; margin-bottom: 0.25rem;",
                textOutput(ns("eff_re"))
              ),
              p(
                class = "small text-muted",
                "RE ≈ CM_e(DCA) / CM_e(RCBD)"
              ),
              hr(),
              p(textOutput(ns("eff_msg"))),
              p(
                class = "small text-muted mb-0",
                textOutput(ns("eff_extra"))
              )
            )
          )
        )
      ),

      br(),
      p(
        class = "small text-muted",
        "En un informe o artículo, es habitual reportar: (i) CM(Error) de cada diseño o modelo, ",
        "(ii) la Eficiencia Relativa (RE) y (iii) una frase interpretativa del tipo: ",
        em("\"el RCBD fue un ", code("X"), "% más eficiente que el DCA para esta variable\"."),
        " Esta lógica sigue las notas clásicas de comparación de diseños completos vs. sin bloqueo ",
        "usando CM(Error) como base de la eficiencia."
      )
    )
  )
}

# Pestaña 6: Scripts listos para copiar (UI mejorada)
pestanna6_session2_v3UI <- function(ns) {
  nav_panel(
    title = "6. Scripts listos para copiar",
    h4(class = "section-header", "Plantillas mínimas en R: DCA (CRD) y RCBD"),

    p(
      "Esta pestaña reúne dos scripts autocontenidos para que puedas reproducir, ",
      "fuera de la app, los ejemplos vistos en la sesión. ",
      "Ambos casos siguen el flujo estándar en diseños agronómicos: ",
      strong("diseñar → simular respuesta → ajustar ANOVA → cuantificar tamaño del efecto (η²).")
    ),

    tags$ul(
      tags$li(strong("DCA / CRD:"), " ensayo con unidad experimental homogénea; aleatorización completa."),
      tags$li(strong("RCBD:"), " necesidad de controlar heterogeneidad entre bloques (suelo, pendiente, gradiente hídrico, etc.).")
    ),

    p(
      class = "small text-muted",
      "Los scripts usan R base, ", code("aov()"),
      " y, para RCBD, el generador de diseño de ",
      code("agricolae::design.rcbd()"),
      " y el tamaño del efecto vía ",
      code("effectsize::eta_squared()"), "."
    ),

    br(),

    # ----------------------------------------------------------
    # Fila con dos tarjetas: DCA a la izquierda, RCBD a la derecha
    # ----------------------------------------------------------
    fluidRow(
      # ------------------------- DCA / CRD -------------------------
      column(
        width = 6,
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-light",
            h5(class = "mb-0", "DCA (CRD) con ", code("sample()"), " + ", code("aov()"))
          ),
          div(
            class = "card-body",
            p(
              class = "small",
              "Ejemplo mínimo de un ", strong("Diseño Completamente al Azar (DCA/CRD)"),
              " con ", code("k"), " tratamientos, ",
              code("r"), " repeticiones por tratamiento y respuesta continua. ",
              "Se asume unidad experimental homogénea y errores ~N(0, σ²)."
            ),
            pre(
              class = "r-code",
"
# --- DCA (CRD) mínimo reproducible ---------------------------------
# Requisitos: base R + efecto de tamaño opcional con {effectsize}
# install.packages('effectsize')  # si aún no está instalado

set.seed(123)  # para poder replicar el ejemplo

# 1) Parámetros de diseño
k     <- 3      # número de tratamientos
r     <- 5      # repeticiones por tratamiento
mu    <- 20     # media base (control o promedio global)
diff  <- 1.5    # rango de diferencia entre tratamientos
sigma <- 1      # desvío estándar del error (σ)

# 2) Nombres de tratamientos (T1, T2, ..., Tk)
tratamientos <- paste0('T', 1:k)

# 3) Plan de campo (CRD: asignación completamente al azar)
plan <- data.frame(
  parcela     = 1:(k * r),
  tratamiento = factor(sample(rep(tratamientos, each = r)))
)

# 4) Efectos de tratamiento (lineales entre 0 y diff)
ef_trat <- seq(0, diff, length.out = k)
names(ef_trat) <- tratamientos

# 5) Generación de la respuesta (y)
plan$y <- mu + ef_trat[plan$tratamiento] +
  rnorm(nrow(plan), mean = 0, sd = sigma)

# 6) ANOVA clásico
mod_dca <- aov(y ~ tratamiento, data = plan)
summary(mod_dca)

# 7) Tamaño del efecto (η²) para tratamiento
effectsize::eta_squared(mod_dca)
"
            ),
            p(
              class = "small text-muted mb-0",
              "Adaptación típica en un trabajo real: reemplazar ",
              code("tratamientos"), " por los factores agronómicos de interés ",
              "(variedades, dosis de fertilizante, estrategias de riego, etc.), ",
              "y ", code("y"), " por el rendimiento o variable de respuesta medida."
            )
          )
        )
      ),

      # ------------------------- RCBD -------------------------
      column(
        width = 6,
        div(
          class = "card shadow-sm mb-4",
          div(
            class = "card-header bg-light",
            h5(class = "mb-0", "RCBD con ", code("agricolae::design.rcbd()"), " + ", code("aov()"))
          ),
          div(
            class = "card-body",
            p(
              class = "small",
              "Ejemplo mínimo de un ",
              strong("Diseño de Bloques Completos al Azar (RCBD)"),
              " con ", code("k"), " tratamientos y ", code("b"), " bloques. ",
              "Se genera el plan de campo con ", code("agricolae"), " y se simula ",
              "un gradiente de bloque (p.ej., fertilidad, drenaje)."
            ),
            pre(
              class = "r-code",
"
# --- RCBD mínimo reproducible --------------------------------------
# Requisitos:
#   install.packages('agricolae')
#   install.packages('effectsize')

set.seed(123)  # para reproducibilidad

# 1) Parámetros de diseño
k     <- 4      # número de tratamientos
b     <- 4      # número de bloques (repeticiones)
mu    <- 20     # media base
diff  <- 2      # rango de diferencia entre tratamientos
sigma <- 1.2    # desvío estándar del error (σ)

tratamientos <- paste0('T', 1:k)

# 2) Plan de campo RCBD (fieldbook) con agricolae
out  <- agricolae::design.rcbd(trt = tratamientos, r = b)
book <- out$book  # columnas típicas: block, plot, tratamientos, etc.

# 3) Efectos de tratamiento y de bloque (gradiente sencillo)
ef_trat <- seq(0, diff, length.out = k); names(ef_trat) <- tratamientos
ef_bloq <- seq(0, 5, length.out = b)    # gradiente ilustrativo entre bloques

book$bloque      <- factor(book$block)
book$tratamiento <- factor(book$tratamientos, levels = tratamientos)

# 4) Generación de la respuesta simulada
book$y <- mu +
  ef_trat[book$tratamiento] +
  ef_bloq[book$bloque] +
  rnorm(nrow(book), mean = 0, sd = sigma)

# 5) ANOVA RCBD: tratamiento + bloque
mod_rcbd <- aov(y ~ tratamiento + bloque, data = book)
summary(mod_rcbd)

# 6) Tamaño del efecto (η² parcial) para tratamiento
effectsize::eta_squared(mod_rcbd, partial = TRUE)
"
            ),
            p(
              class = "small text-muted mb-0",
              "En un ensayo real, ", code("book"), " sería el cuaderno de campo: ",
              "una fila por parcela con bloque, tratamiento, coordenadas, etc. ",
              "La lógica de análisis (", code("y ~ tratamiento + bloque"), ") ",
              "es la misma que se ha usado en la pestaña 4."
            )
          )
        )
      )
    ),

    br(),

    # Notas finales de uso
    div(
      class = "alert alert-secondary mt-2",
      tags$strong("Notas prácticas para tus propios experimentos:"),
      tags$ul(
        tags$li(
          "Sustituye los nombres genéricos (", code("T1"), ", ", code("T2"), ", ...) por tus tratamientos reales ",
          "(p.ej., variedades, dosis, estrategias de manejo)."
        ),
        tags$li(
          "Asegura que tu variable de respuesta (", code("y"), ") cumple, al menos aproximadamente, ",
          "los supuestos de ANOVA: normalidad de residuos e igualdad de varianzas; ",
          "usa los diagnósticos vistos en pestañas 2 y 4."
        ),
        tags$li(
          "En informes o artículos, cita tanto el diseño (CRD/RCBD) como el software usado ",
          "(p.ej., ", code("agricolae"), " para el plan del RCBD y ",
          code("effectsize"), " para η²)."
        )
      )
    )
  )
}

# Pestaña 7: Referencias
pestanna7_session2_v3UI <- function(ns) {
  nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("NIST/SEMATECH (CRD): ",
              a("Completely Randomized Design", href="https://www.itl.nist.gov/div898/handbook/pri/section3/pri3331.htm", target="_blank")),
      tags$li("Penn State (STAT Online): ",
              a("Índice de lecciones (incluye CRD y RCBD)", href="https://online.stat.psu.edu/stat502/", target="_blank")),
      tags$li("afex (vignette): ",
              a("ANOVA example & assumptions", href="https://cran.r-project.org/web/packages/afex/vignettes/afex_anova_example.html", target="_blank")),
      tags$li("effectsize (CRAN): ",
              a("eta_squared()", href="https://cran.r-project.org/web/packages/effectsize/index.html", target="_blank")),
      tags$li("Uso de design.rcbd(): ",
              a("agricolaeplotr docs (ejemplo con design.rcbd)", href="https://kwstat.github.io/agricolaeplotr/reference/plot_rcbd.html", target="_blank"))
    ),
    p(class="text-muted small",
      "La justificación conceptual de bloqueo (comparar lo similar y remover variación no interesante) y sus efectos sobre CM(Error) y potencia se alinea con cursos estándar de diseños experimentales (p. ej., STAT Online, Penn State)."
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session2_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 2: Diseño Completamente al Azar (DCA/CRD) y Bloques Completos al Azar (RCBD)")
    ),
    navset_tab(
      pestanna1_session2_v3UI(ns),
      pestanna2_session2_v3UI(ns),
      pestanna3_session2_v3UI(ns),
      pestanna4_session2_v3UI(ns),
      pestanna5_session2_v3UI(ns),
      pestanna6_session2_v3UI(ns),
      pestanna7_session2_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Server (por ahora sólo contenido estático)
pestanna1_session2_v3_server <- function(input, output, session) {
  # Esta pestaña actualmente sólo muestra contenido estático (texto y fórmulas).
  # Dejamos el server explícito para mantener simetría con las demás pestañas
  # y habilitar fácilmente futuras extensiones interactivas.
  invisible(NULL)
}

# Pestaña 2: DCA — diseño y análisis
pestanna2_session2_v3_server <- function(input, output, session) {
  # -------------------------------
  # 1) Generación de datos (CRD)
  # -------------------------------
  dca_data <- eventReactive(input$dca_go, {
    k <- input$dca_k
    r <- input$dca_r
    trts <- paste0("T", seq_len(k))

    # Aleatorización CRD
    set.seed(as.integer(Sys.time()))
    plan <- data.frame(
      parcela     = seq_len(k * r),
      tratamiento = factor(
        sample(rep(trts, each = r), size = k * r),
        levels = trts
      )
    )

    # Efectos de tratamiento (lineales 0..diff)
    ef_t <- seq(0, input$dca_diff, length.out = k)
    names(ef_t) <- trts

    # Respuesta simulada
    y <- input$dca_mu +
      ef_t[plan$tratamiento] +
      stats::rnorm(nrow(plan), mean = 0, sd = input$dca_sigma)

    plan$y <- as.numeric(y)
    plan
  }, ignoreInit = TRUE)

  # Resumen textual del diseño
  output$dca_design_summary <- renderText({
    req(input$dca_k, input$dca_r, input$dca_mu, input$dca_diff, input$dca_sigma)
    paste0(
      "CRD con ", input$dca_k, " tratamientos × ", input$dca_r,
      " repeticiones (n = ", input$dca_k * input$dca_r, " parcelas). ",
      "Medias de tratamiento simuladas aproximadamente en [",
      round(input$dca_mu, 2), ", ",
      round(input$dca_mu + input$dca_diff, 2), "] con σ = ",
      round(input$dca_sigma, 2), "."
    )
  })

  # -------------------------------
  # 2) Gráficos descriptivos
  # -------------------------------
  output$dca_layout_plot <- renderPlot({
    df <- shiny::req(dca_data())
    ggplot2::ggplot(df, ggplot2::aes(x = parcela, y = 1, fill = tratamiento)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_brewer(palette = "Set2") +
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "bottom",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      ) +
      ggplot2::labs(title = "Layout CRD (orden aleatorio de tratamientos)", y = NULL, x = NULL)
  })

  output$dca_box <- renderPlot({
    df <- shiny::req(dca_data())
    ggplot2::ggplot(df, ggplot2::aes(tratamiento, y, fill = tratamiento)) +
      ggplot2::geom_boxplot(alpha = 0.8, outlier.alpha = 0.5, show.legend = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Distribución de la respuesta por tratamiento",
        y = "Respuesta simulada", x = NULL
      )
  })

  # -------------------------------
  # 3) Modelo ANOVA (aov)
  # -------------------------------
  dca_model <- reactive({
    df <- shiny::req(dca_data())
    stats::aov(y ~ tratamiento, data = df)
  })

  output$dca_anova_tbl <- renderTable({
    mod_tidy <- broom::tidy(shiny::req(dca_model()))
    mod_tidy |>
      dplyr::select(term, df, sumsq, meansq, statistic, p.value) |>
      dplyr::mutate(
        term = dplyr::recode(
          term,
          "(Intercept)" = "Intercepto",
          "tratamiento" = "Tratamiento",
          "Residuals"   = "Residuos"
        )
      ) |>
      dplyr::rename(
        `Fuente` = term, `gl` = df, `SC` = sumsq,
        `CM` = meansq, `F` = statistic, `p` = p.value
      )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  # Resumen compacto F y p-valor de tratamiento
  output$dca_anova_caption <- renderText({
    mod_tidy <- broom::tidy(shiny::req(dca_model()))
    fila_trat <- mod_tidy[mod_tidy$term == "tratamiento", , drop = FALSE]
    fila_res  <- mod_tidy[mod_tidy$term == "Residuals", , drop = FALSE]

    if (!nrow(fila_trat) || !nrow(fila_res)) return("")

    sprintf(
      "Tratamiento: F(%d, %d) = %.2f, p = %.4f.",
      fila_trat$df[1], fila_res$df[1],
      fila_trat$statistic[1], fila_trat$p.value[1]
    )
  })

  # Tamaño del efecto eta^2 + interpretación rápida
  output$dca_eta <- renderPrint({
    m  <- shiny::req(dca_model())
    es <- effectsize::eta_squared(m, partial = FALSE)
    print(es)

    # Intentar extraer el eta^2 del tratamiento, si existe
    if ("Parameter" %in% names(es)) {
      eta_trat <- es$Eta2[es$Parameter == "tratamiento"]
      if (length(eta_trat) == 1 && !is.na(eta_trat)) {
        magn <- effectsize::interpret_eta_squared(eta_trat, rules = "cohen")
        cat(
          "\nInterpretación aproximada (Cohen) para 'tratamiento':",
          as.character(magn), "\n"
        )
      }
    }
  })

  # -------------------------------
  # 4) Diagnóstico de supuestos
  # -------------------------------
  output$dca_resid_vs_fit <- renderPlot({
    m <- shiny::req(dca_model())
    df_diag <- tibble::tibble(
      ajustados = stats::fitted(m),
      residuos  = stats::resid(m)
    )

    ggplot2::ggplot(df_diag, ggplot2::aes(x = ajustados, y = residuos)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::geom_smooth(
        method = "loess", se = FALSE,
        linewidth = 0.6, colour = "#2B6CB0"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        x = "Valores ajustados",
        y = "Residuos",
        title = "Residuos vs ajustados (CRD)"
      )
  })

  output$dca_qq <- renderPlot({
    m <- shiny::req(dca_model())
    df_diag <- tibble::tibble(
      resid_std = stats::rstandard(m)
    )

    ggplot2::ggplot(df_diag, ggplot2::aes(sample = resid_std)) +
      ggplot2::stat_qq(alpha = 0.8) +
      ggplot2::stat_qq_line(colour = "red") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        x = "Cuantiles teóricos",
        y = "Residuos estandarizados",
        title = "QQ-plot de residuos (CRD)"
      )
  })

  output$dca_shapiro <- renderPrint({
    m <- shiny::req(dca_model())
    stats::shapiro.test(stats::residuals(m))
  })

  output$dca_levene <- renderPrint({
    df <- shiny::req(dca_data())
    car::leveneTest(y ~ tratamiento, data = df)
  })
}

# Pestaña 3: Heterogeneidad simulada (DCA vs RCBD)
pestanna3_session2_v3_server <- function(input, output, session, helpers) {
  # -------------------------------------------------
  # 1) Simulación de heterogeneidad y ajuste de modelos
  # -------------------------------------------------
  het_res <- eventReactive(input$het_go, {
    k <- input$het_k
    b <- input$het_b
    trts <- paste0("T", seq_len(k))

    set.seed(as.integer(Sys.time()))

    # Diseño factorial bloque × tratamiento (RCBD balanceado)
    base <- expand.grid(
      bloque      = factor(seq_len(b)),
      tratamiento = factor(trts, levels = trts)
    )

    # Efectos de tratamiento (0..het_diff)
    ef_t <- seq(0, input$het_diff, length.out = k)
    names(ef_t) <- trts

    # Gradiente de bloque (0..het_grad)
    ef_b <- seq(0, input$het_grad, length.out = b)

    # Respuesta simulada
    mu0 <- 20
    y <- mu0 +
      ef_t[base$tratamiento] +
      ef_b[as.integer(base$bloque)] +
      stats::rnorm(nrow(base), mean = 0, sd = input$het_sigma)

    df <- transform(base, y = as.numeric(y))

    # Modelos: DCA (ignora bloques) vs RCBD (incluye bloque)
    m_dca  <- stats::aov(y ~ tratamiento, data = df)
    m_rcbd <- stats::aov(y ~ tratamiento + bloque, data = df)

    list(
      df      = df,
      m_dca   = m_dca,
      m_rcbd  = m_rcbd,
      aov_dca = broom::tidy(m_dca),
      aov_rcbd = broom::tidy(m_rcbd)
    )
  }, ignoreInit = TRUE)

  # Resumen textual del escenario simulado
  output$het_summary <- renderText({
    req(input$het_k, input$het_b, input$het_grad, input$het_diff, input$het_sigma)
    paste0(
      "RCBD balanceado con ", input$het_k, " tratamientos × ",
      input$het_b, " bloques (n = ", input$het_k * input$het_b, " parcelas). ",
      "Gradiente de bloque en [0, ", round(input$het_grad, 2), "]; ",
      "diferencias de tratamiento en un rango de ~", round(input$het_diff, 2),
      " unidades; σ = ", round(input$het_sigma, 2), "."
    )
  })

  # -------------------------------------------------
  # 2) Visualización: mapa de calor y boxplots
  # -------------------------------------------------
  output$het_heat <- renderPlot({
    res <- shiny::req(het_res())
    ggplot2::ggplot(res$df, ggplot2::aes(x = bloque, y = tratamiento, fill = y)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_viridis_c() +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Mapa de calor: bloque × tratamiento",
        x = "Bloque", y = "Tratamiento", fill = "Respuesta (y)"
      )
  })

  output$het_box_by_trt <- renderPlot({
    res <- shiny::req(het_res())
    ggplot2::ggplot(res$df, ggplot2::aes(tratamiento, y, fill = tratamiento)) +
      ggplot2::geom_boxplot(show.legend = FALSE) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        title = "Distribución por tratamiento \n(con gradiente en el campo)",
        y = "Respuesta (y)", x = NULL
      )
  })

  # -------------------------------------------------
  # 3) Comparación de F, p y CM(Error)
  # -------------------------------------------------
  output$het_F_dca <- renderText({
    res <- shiny::req(het_res())
    helpers$fmtF(res$aov_dca)
  })

  output$het_p_dca <- renderText({
    res <- shiny::req(het_res())
    helpers$fmtp(res$aov_dca)
  })

  output$het_mse_dca <- renderText({
    res <- shiny::req(het_res())
    sprintf("%.3f", helpers$ms_error(res$m_dca))
  })

  output$het_F_rcbd <- renderText({
    res <- shiny::req(het_res())
    helpers$fmtF(res$aov_rcbd)
  })

  output$het_p_rcbd <- renderText({
    res <- shiny::req(het_res())
    helpers$fmtp(res$aov_rcbd)
  })

  output$het_mse_rcbd <- renderText({
    res <- shiny::req(het_res())
    sprintf("%.3f", helpers$ms_error(res$m_rcbd))
  })

  # Mensaje interpretativo sobre CM(Error)
  output$het_mse_compare_msg <- renderText({
    res <- shiny::req(het_res())
    mse_dca  <- as.numeric(helpers$ms_error(res$m_dca))
    mse_rcbd <- as.numeric(helpers$ms_error(res$m_rcbd))

    if (!is.finite(mse_dca) || !is.finite(mse_rcbd)) return("")

    re <- mse_dca / mse_rcbd

    if (re > 1.05) {
      paste0(
        "En esta simulación, el RCBD reduce la CM(Error) respecto al DCA (RE ≈ ",
        sprintf("%.2f", re),
        "). El bloqueo está capturando parte importante del gradiente entre bloques."
      )
    } else if (re > 0.95 && re <= 1.05) {
      paste0(
        "La CM(Error) de RCBD y DCA son muy similares (RE ≈ ",
        sprintf("%.2f", re),
        "). El bloqueo aporta poca ganancia en precisión con esta combinación de parámetros."
      )
    } else {
      paste0(
        "Aquí la CM(Error) del RCBD no es claramente menor que la del DCA (RE ≈ ",
        sprintf("%.2f", re),
        "). Puede que el gradiente de bloque sea débil o que la variabilidad aleatoria domine."
      )
    }
  })

  # -------------------------------------------------
  # 4) Diagnóstico del modelo RCBD (ggplot)
  # -------------------------------------------------
  output$het_resid_vs_fit_rcbd <- renderPlot({
    res <- shiny::req(het_res())
    m <- res$m_rcbd

    df_diag <- tibble::tibble(
      ajustados = stats::fitted(m),
      residuos  = stats::resid(m)
    )

    ggplot2::ggplot(df_diag, ggplot2::aes(x = ajustados, y = residuos)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      ggplot2::geom_point(alpha = 0.8) +
      ggplot2::geom_smooth(
        method = "loess", se = FALSE,
        linewidth = 0.6, colour = "#2B6CB0"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        x = "Valores ajustados (RCBD)",
        y = "Residuos",
        title = "Residuos vs ajustados (modelo RCBD)"
      )
  })

  output$het_qq_rcbd <- renderPlot({
    res <- shiny::req(het_res())
    m <- res$m_rcbd

    df_diag <- tibble::tibble(
      resid_std = stats::rstandard(m)
    )

    ggplot2::ggplot(df_diag, ggplot2::aes(sample = resid_std)) +
      ggplot2::stat_qq(alpha = 0.8) +
      ggplot2::stat_qq_line(colour = "red") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::labs(
        x = "Cuantiles teóricos",
        y = "Residuos estandarizados",
        title = "QQ-plot de residuos (modelo RCBD)"
      )
  })

  # Devuelve el objeto reactivo para usar en la pestaña 5 (eficiencia)
  return(het_res)
}

# Pestaña 4: RCBD — diseño y análisis
pestanna4_session2_v3_server <- function(input, output, session) {

  # -------------------------------------------------
  # 1) Generación del diseño RCBD + simulación de respuesta
  # -------------------------------------------------
  rcbd_res <- eventReactive(input$rcbd_go, {
    k    <- input$rcbd_k
    b    <- input$rcbd_b
    trts <- paste0("T", seq_len(k))

    # Semilla basada en tiempo para variar en cada click
    set.seed(as.integer(Sys.time()))

    # Genera diseño RCBD con agricolae (tratamientos por bloques)
    out  <- agricolae::design.rcbd(trt = trts, r = b)
    book <- out$book

    # -----------------------------
    # 1.1 Detectar columna de tratamiento de forma robusta
    # -----------------------------
    cand_trt_names <- c(
      "tratamientos", "tratamiento",
      "trt", "treatments", "T1", "T", "treatment",
      "gen", "geno"
    )

    trt_col <- intersect(cand_trt_names, names(book))
    if (length(trt_col) >= 1) {
      trt_col <- trt_col[1]
    } else {
      # Fallback: tomar la primera columna no claramente "meta"
      non_meta <- setdiff(
        names(book),
        c("plots", "plot", "block", "row", "col", "ROW", "COL",
          "rep", "Rep", "REP")
      )
      if (length(non_meta) == 0) {
        stop(
          "No se pudo identificar una columna de tratamiento en out$book.\n",
          "Nombres de columnas: ", paste(names(book), collapse = ", ")
        )
      }
      trt_col <- non_meta[1]
    }

    # Estandarizar nombre "tratamientos"
    if (!"tratamientos" %in% names(book)) {
      book$tratamientos <- book[[trt_col]]
    }

    # -----------------------------
    # 1.2 Detectar columna de bloque de forma robusta
    # -----------------------------
    cand_block_names <- c("block", "bloque", "Block", "BLOCK", "rep", "Rep", "REP")
    block_col <- intersect(cand_block_names, names(book))
    if (length(block_col) >= 1) {
      block_col <- block_col[1]
    } else {
      # Si no existe, construir un bloque ficticio 1..b repetido
      book$block_tmp <- rep(seq_len(b), each = k)
      block_col <- "block_tmp"
    }

    bloque <- factor(book[[block_col]])

    # -----------------------------
    # 1.3 Simulación de respuesta
    # -----------------------------
    ef_t <- seq(0, input$rcbd_diff, length.out = k)
    names(ef_t) <- trts

    ef_b <- seq(0, 5, length.out = b)   # gradiente ilustrativo entre bloques
    mu0  <- 20

    y <- mu0 +
      ef_t[as.character(book$tratamientos)] +
      ef_b[as.integer(bloque)] +
      stats::rnorm(nrow(book), mean = 0, sd = input$rcbd_sigma)

    # Armamos un data.frame "df" estandarizado para el resto de la app
    df <- book
    df$bloque      <- bloque
    df$tratamiento <- factor(book$tratamientos, levels = trts)
    df$y           <- as.numeric(y)

    # Modelo ANOVA RCBD (trat + bloque)
    m <- stats::aov(y ~ tratamiento + bloque, data = df)

    aov_tbl <- broom::tidy(m)

    # p-valor del tratamiento
    p_trat <- aov_tbl |>
      dplyr::filter(term == "tratamiento") |>
      dplyr::pull(p.value)
    p_trat <- p_trat[1]

    tuk <- if (!is.na(p_trat) && p_trat < 0.05) {
      stats::TukeyHSD(m, which = "tratamiento")
    } else {
      NULL
    }

    eta <- effectsize::eta_squared(m, partial = TRUE)

    list(
      out   = out,   # objeto original de agricolae (por si quieres usarlo luego)
      df    = df,    # data estandarizada con bloque, tratamiento, y
      model = m,
      aov   = aov_tbl,
      eta   = eta,
      tukey = tuk
    )
  }, ignoreInit = TRUE)

  # -------------------------------------------------
  # 1.4 Resumen textual del diseño
  # -------------------------------------------------
  output$rcbd_summary <- renderText({
    req(input$rcbd_k, input$rcbd_b, input$rcbd_diff, input$rcbd_sigma)
    paste0(
      "RCBD completo con ", input$rcbd_k, " tratamientos × ",
      input$rcbd_b, " bloques (n = ", input$rcbd_k * input$rcbd_b, " parcelas). ",
      "Rango aproximado de diferencias de tratamiento: ±",
      round(input$rcbd_diff, 2),
      " unidades; σ = ", round(input$rcbd_sigma, 2), "."
    )
  })

  # -------------------------------------------------
  # 2) Layout RCBD (tabla coloreada) + fieldbook
  # -------------------------------------------------
  output$rcbd_layout_ui <- renderUI({
    res <- shiny::req(rcbd_res())
    df  <- res$df

    # Usamos columnas estandarizadas
    k <- length(unique(df$tratamiento))
    b <- length(unique(df$bloque))

    # Niveles de bloque (para encabezados) y orden dentro de bloque
    bloques <- levels(df$bloque)
    if (is.null(bloques)) {
      bloques <- sort(unique(df$bloque))
    }

    # Paleta por tratamiento
    trt_levels <- levels(df$tratamiento)
    if (is.null(trt_levels)) trt_levels <- sort(unique(df$tratamiento))

    cols <- RColorBrewer::brewer.pal(max(3, min(8, length(trt_levels))), "Set2")
    names(cols) <- trt_levels

    # Construir filas: una fila por posición dentro del bloque
    filas <- lapply(seq_len(k), function(i) {
      celdas <- lapply(seq_along(bloques), function(j) {
        sub_df <- df[df$bloque == bloques[j], , drop = FALSE]

        # Ordenar por plots si existe, si no, como está
        if ("plots" %in% names(sub_df)) {
          sub_df <- sub_df[order(sub_df$plots), , drop = FALSE]
        }

        if (nrow(sub_df) < i) {
          tr <- NA_character_
        } else {
          tr <- as.character(sub_df$tratamiento[i])
        }

        bg_col <- if (!is.na(tr) && tr %in% names(cols)) cols[tr] else "#f9fafb"

        tags$td(
          style = paste0(
            "background-color:", bg_col, "; ",
            "border:1px solid #e5e7eb; font-weight:bold;"
          ),
          tr
        )
      })
      tags$tr(celdas)
    })

    # Encabezado con nombres de bloque
    header <- tags$tr(
      lapply(
        seq_along(bloques),
        function(j) {
          tags$th(
            style = "border:1px solid #e5e7eb; background:#f8fafc;",
            paste("Bloque", bloques[j])
          )
        }
      )
    )

    tagList(
      p(
        em("Layout RCBD: columnas = bloques; filas = posiciones dentro del bloque ",
           "(orden según numeración de parcelas si existe).")
      ),
      tags$table(
        class = "table table-bordered text-center",
        style = "width:100%; table-layout:fixed; font-size:0.9rem;",
        tags$thead(header),
        tags$tbody(filas)
      )
    )
  })

  output$rcbd_book_head <- renderTable({
    res <- shiny::req(rcbd_res())
    head(res$df, 8)
  }, striped = TRUE, bordered = TRUE, digits = 3)

  # -------------------------------------------------
  # 3) ANOVA RCBD y tamaño del efecto
  # -------------------------------------------------
  output$rcbd_anova_tbl <- renderTable({
    res <- shiny::req(rcbd_res())
    res$aov |>
      dplyr::select(term, df, sumsq, meansq, statistic, p.value) |>
      dplyr::rename(
        `Fuente` = term,
        `gl`     = df,
        `SC`     = sumsq,
        `CM`     = meansq,
        `F`      = statistic,
        `p`      = p.value
      )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  output$rcbd_eta <- renderPrint({
    res <- shiny::req(rcbd_res())
    print(res$eta)
  })

  output$rcbd_eta_msg <- renderText({
    res <- shiny::req(rcbd_res())
    eta_tbl <- res$eta

    if (!"Parameter" %in% names(eta_tbl)) return("")

    fila_trat <- eta_tbl[eta_tbl$Parameter == "tratamiento", , drop = FALSE]
    if (nrow(fila_trat) == 0) {
      return("No se pudo extraer η² parcial para el tratamiento.")
    }

    eta_col <- intersect(
      c("Eta2_partial", "Eta2_partial_H", "Eta2"),
      names(fila_trat)
    )
    if (length(eta_col) == 0) {
      return("No se encontró columna de η² en el resultado de effectsize.")
    }

    eta_val <- as.numeric(fila_trat[[eta_col[1]]])
    if (!is.finite(eta_val)) {
      return("η² parcial del tratamiento no es finito.")
    }

    interpretacion <- if (eta_val < 0.01) {
      "Efecto muy pequeño: el tratamiento explica poca variación adicional."
    } else if (eta_val < 0.06) {
      "Efecto pequeño: el tratamiento explica una fracción limitada de la variación."
    } else if (eta_val < 0.14) {
      "Efecto medio: el tratamiento tiene un impacto moderado sobre la respuesta."
    } else {
      "Efecto grande: el tratamiento explica una porción importante de la variación entre parcelas."
    }

    sprintf("η² parcial (tratamiento) ≈ %.3f. %s", eta_val, interpretacion)
  })

  # -------------------------------------------------
  # 4) Tukey HSD para tratamientos
  # -------------------------------------------------
  output$rcbd_tukey_plot <- renderPlot({
    res <- shiny::req(rcbd_res())
    if (!is.null(res$tukey)) {
      plot(res$tukey, las = 1)
      abline(v = 0, lty = 2, col = "red")
    }
  })

  output$rcbd_tukey_text <- renderPrint({
    res <- shiny::req(rcbd_res())
    if (is.null(res$tukey)) {
      cat(
        "Tratamientos no significativos en RCBD (p ≥ 0.05) — no se aplica Tukey.\n",
        "Puedes aumentar el rango de diferencias de tratamiento (slider) o reducir σ ",
        "para ver un ejemplo con comparaciones significativas.\n"
      )
    } else {
      cat("Comparaciones múltiples de Tukey (α = 0.05) para el factor tratamiento:\n\n")
      print(res$tukey)
    }
  })
}

# Pestaña 5: Eficiencia relativa (RE) del RCBD vs DCA
pestanna5_session2_v3_server <- function(input, output, session, het_res, helpers) {

  # -------------------------------------------------
  # Tabla de comparación de CM(Error)
  # -------------------------------------------------
  output$eff_table <- renderTable({
    res <- shiny::req(het_res())

    mse_dca  <- helpers$ms_error(res$m_dca)
    mse_rcbd <- helpers$ms_error(res$m_rcbd)

    re <- as.numeric(mse_dca) / as.numeric(mse_rcbd)

    # Interpretación corta por fila
    interp_dca <- "Análisis como DCA (ignora bloques)."
    interp_rcbd <- if (is.finite(re) && re > 1) {
      "Incluye bloques; CM(Error) debería ser menor si el gradiente es relevante."
    } else if (is.finite(re) && re == 1) {
      "Incluye bloques; en esta simulación no hubo ganancia en CM(Error)."
    } else if (is.finite(re) && re < 1) {
      "Incluye bloques; aquí el CM(Error) resultó algo mayor que en DCA."
    } else {
      "Incluye bloques; no se pudo calcular RE de forma estable."
    }

    tibble::tibble(
      Modelo       = c("DCA (ignora bloques)", "RCBD (con bloques)"),
      `CM(Error)`  = c(mse_dca, mse_rcbd),
      `Comentario` = c(interp_dca, interp_rcbd)
    )
  }, digits = 4, striped = TRUE, bordered = TRUE)

  # -------------------------------------------------
  # Valor numérico de RE
  # -------------------------------------------------
  output$eff_re <- renderText({
    res <- shiny::req(het_res())
    mse_dca  <- helpers$ms_error(res$m_dca)
    mse_rcbd <- helpers$ms_error(res$m_rcbd)

    re <- as.numeric(mse_dca) / as.numeric(mse_rcbd)

    if (!is.finite(re) || re <= 0) {
      return("–")
    }
    sprintf("%.2f", re)
  })

  # -------------------------------------------------
  # Mensaje principal de interpretación
  # -------------------------------------------------
  output$eff_msg <- renderText({
    res <- shiny::req(het_res())
    mse_dca  <- helpers$ms_error(res$m_dca)
    mse_rcbd <- helpers$ms_error(res$m_rcbd)

    re <- as.numeric(mse_dca) / as.numeric(mse_rcbd)

    if (!is.finite(re) || re <= 0) {
      return("No se pudo evaluar la eficiencia relativa (CM(Error) no finito).")
    }

    perc_gain <- (re - 1) * 100

    if (re > 1.10) {
      paste0(
        "Bloquear fue claramente útil: el RCBD es ~",
        round(re, 2), " veces más eficiente que el DCA. ",
        "Equivalente a reducir el CM(Error) en ~",
        round(100 - 100 / re), "%."
      )
    } else if (re > 1) {
      paste0(
        "El RCBD fue ligeramente más eficiente que el DCA (RE ≈ ",
        round(re, 2), "). La ganancia en precisión es modesta, ",
        "pero el bloqueo sigue siendo consistente con el diseño."
      )
    } else if (abs(re - 1) <= 0.05) {
      paste0(
        "En esta simulación, bloquear no cambió de forma apreciable la precisión (RE ≈ ",
        round(re, 2), "). Podría deberse a un gradiente débil o alta variabilidad residual."
      )
    } else {
      paste0(
        "En este escenario, el RCBD no mejoró la precisión (RE ≈ ",
        round(re, 2), " < 1). El CM(Error) en el modelo con bloques es mayor que en el DCA."
      )
    }
  })

  # -------------------------------------------------
  # Mensaje extra: lectura en términos de tamaño muestral
  # -------------------------------------------------
  output$eff_extra <- renderText({
    res <- shiny::req(het_res())
    mse_dca  <- helpers$ms_error(res$m_dca)
    mse_rcbd <- helpers$ms_error(res$m_rcbd)

    re <- as.numeric(mse_dca) / as.numeric(mse_rcbd)

    if (!is.finite(re) || re <= 0) {
      return("Interpretación adicional en términos de tamaño muestral no disponible.")
    }

    if (re > 1) {
      paste0(
        "Regla de lectura clásica: un experimento RCBD con este gradiente entrega ",
        "similar precisión que un DCA con ~", round(re, 2),
        " veces más parcelas. Es decir, el bloqueo equivale a ganar potencia ",
        "sin aumentar el tamaño de muestra."
      )
    } else {
      paste0(
        "Si este patrón se replicara en datos reales, para lograr la misma precisión ",
        "que el RCBD, el DCA necesitaría menos parcelas (1/RE ≈ ",
        round(1 / re, 2),
        " veces menos). Esto indica que, bajo este esquema particular de simulación, ",
        "el factor de bloqueo no captura bien el gradiente o introduce ruido adicional."
      )
    }
  })
}


# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session2_v3Server <- function(input, output, session) {
  # Dependencias
  req <- shiny::req
  library(ggplot2); library(dplyr); library(broom)
  library(effectsize); library(car)
  library(RColorBrewer); library(patchwork)

  # Helpers
  helpers <- list(
    fmtF = function(tidy_aov_df) {
      row <- tidy_aov_df[ tidy_aov_df$term %in% c("tratamiento","group","treatments","trat"), ]
      if (!nrow(row)) return("—")
      sprintf("%.3f", row$statistic[1])
    },
    fmtp = function(tidy_aov_df) {
      row <- tidy_aov_df[ tidy_aov_df$term %in% c("tratamiento","group","treatments","trat"), ]
      if (!nrow(row)) return("—")
      p <- row$p.value[1]; if (is.na(p)) return("—")
      if (p < 1e-4) "< 0.0001" else sprintf("%.4f", p)
    },
    ms_error = function(model) {
      an <- anova(model)
      as.numeric(an["Residuals","Mean Sq"])
    }
  )

  # Pestaña 1 (Contexto)
  pestanna1_session2_v3_server(input, output, session)
  
  # Pestaña 2 (DCA)
  pestanna2_session2_v3_server(input, output, session)

  # Pestaña 3 (Heterogeneidad) - Returns reactive for Pestaña 5
  het_res <- pestanna3_session2_v3_server(input, output, session, helpers)

  # Pestaña 4 (RCBD)
  pestanna4_session2_v3_server(input, output, session)

  # Pestaña 5 (Eficiencia) - Uses het_res
  pestanna5_session2_v3_server(input, output, session, het_res, helpers)
  
  # Pestaña 6 (Scripts): No server logic needed
  # Pestaña 7 (Referencias): No server logic needed
}
