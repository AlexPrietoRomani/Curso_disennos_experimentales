# R/modules/Diseños_estadisticos_V3/Parte II (Intermedia)/session3.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: MRLS & Datos
pestanna1_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) MRLS & Datos",
    
    # -------------------------------------------------------------------
    # Bloque 0: Contexto y objetivos de la pestaña
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("Contexto y objetivos de la Sesión 3 (MRLS)"),
      tags$p(
        "En esta pestaña trabajaremos con el ",
        strong("Modelo de Regresión Lineal Simple (MRLS)"),
        " de la forma ",
        HTML("&nbsp;"), 
        "\\( y = \\beta_0 + \\beta_1 x + \\varepsilon \\), ",
        "donde estimamos ", HTML("&beta;"), " por ",
        strong("Mínimos Cuadrados Ordinarios (OLS)"),
        " minimizando la suma de residuos al cuadrado."
      ),
      tags$ul(
        tags$li("Construir un conjunto de datos práctico: ",
                strong("simulado"), " (x = insumo, y = respuesta) ",
                "o ", strong("importado desde CSV"), "."),
        tags$li("Ajustar un MRLS con ", code("lm(y ~ x)"), 
                " y revisar un resumen clásico (`summary(lm)`)."),
        tags$li("Fijar una línea base con β₀, β₁, R² y p-valor antes de pasar a diagnósticos y modelos no lineales.")
      ),
      tags$p(
        class = "small text-muted",
        "Referencias sugeridas: Gelman & Hill (2007) para regresión aplicada; ",
        "OLS como estimador estándar en MRLS. Ver documentación de R base y libros de regresión para más detalles."
      )
    ),

    # -------------------------------------------------------------------
    # Bloque 1: Datos de práctica (simulación o CSV)
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("Datos de práctica (x = insumo, y = respuesta)"),
      fluidRow(
        # ---------------- Columna izquierda: controles de datos ----------------
        column(
          width = 4,
          
          div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 1 · Elegir fuente de datos"
          ),
          radioButtons(
            ns("s3_data_mode"), "Fuente de datos:",
            choices  = c("Simular (modo docente)" = "sim",
                         "Cargar CSV propio"      = "csv"),
            selected = "sim"
          ),
          
          # ---- Modo simulación ----
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sim'", ns("s3_data_mode")),
            
            tags$small(
              class = "text-muted",
              "Contexto sugerido: x = dosis o nivel de insumo (kg/ha, plantas/ha, etc.); ",
              "y = respuesta (rendimiento, biomasa, índice, etc.)."
            ),
            tags$hr(),
            
            sliderInput(
              ns("s3_n"),
              "n (observaciones):",
              min = 20, max = 500, value = 100, step = 10
            ),
            sliderInput(
              ns("s3_beta0"),
              HTML("&beta;₀ (intercepto):"),
              min = -200, max = 1000, value = 500, step = 5
            ),
            sliderInput(
              ns("s3_beta1"),
              HTML("&beta;₁ (pendiente):"),
              min = -10, max = 10, value = 2, step = 0.1
            ),
            sliderInput(
              ns("s3_sigma"),
              HTML("&sigma; (ruido):"),
              min = 1, max = 100, value = 40, step = 1
            ),
            
            tags$hr(class = "my-2"),
            div(
              class = "mb-1 small text-muted text-uppercase fw-bold",
              "Verdad subyacente (opcionalmente curva)"
            ),
            checkboxInput(
              ns("s3_quadratic_truth"),
              "Generar verdad con curvatura suave (parabólica)",
              value = TRUE
            ),
            sliderInput(
              ns("s3_beta2_truth"),
              HTML("&beta;₂ (curvatura de la 'verdad')"),
              min = -0.05, max = 0.05, value = -0.01, step = 0.001
            ),
            
            actionButton(
              ns("s3_gen"),
              "Generar datos simulados",
              icon  = icon("random"),
              class = "btn-primary w-100 mt-2"
            ),
            tags$small(
              class = "text-muted",
              "Nota: si β₂ ≠ 0, la 'verdad' es ligeramente curva, pero aquí ajustaremos un modelo lineal ",
              "(intencionalmente simplificado)."
            )
          ),
          
          # ---- Modo CSV ----
          conditionalPanel(
            condition = sprintf("input['%s'] == 'csv'", ns("s3_data_mode")),
            tags$small(
              class = "text-muted",
              "Sube un archivo con dos columnas numéricas llamadas ",
              code("x"), " (insumo) y ", code("y"), " (respuesta)."
            ),
            tags$hr(),
            fileInput(
              ns("s3_file"),
              "Sube un CSV con columnas 'x' y 'y'",
              accept = c(".csv", "text/csv")
            ),
            checkboxInput(
              ns("s3_has_header"),
              "CSV con cabecera",
              TRUE
            ),
            selectInput(
              ns("s3_sep"),
              "Separador",
              choices  = c("Coma (,)" = ",", "Punto y coma (;)" = ";", "Tab" = "\t"),
              selected = ","
            ),
            checkboxInput(
              ns("s3_show_preview"),
              "Previsualizar primeras filas",
              TRUE
            ),
            tags$small(
              class = "text-muted",
              "Valores no numéricos o NA se omiten automáticamente al leer el archivo."
            )
          )
        ),
        
        # ---------------- Columna derecha: tabla + gráfico base ----------------
        column(
          width = 8,
          bslib::card(
            bslib::card_header("Exploración de los datos (tabla + nube de puntos)"),
            tags$small(
              class = "text-muted",
              "Primero revisa si los datos tienen sentido en la escala y si la relación aparente es razonablemente ",
              "monótona/lineal antes de ajustar modelos."
            ),
            DT::dataTableOutput(ns("s3_tbl_preview")),
            tags$hr(),
            plotOutput(ns("s3_scatter_base"), height = "330px")
          )
        )
      )
    ),

    # -------------------------------------------------------------------
    # Bloque 2: MRLS – Ajuste e interpretación
    # -------------------------------------------------------------------
    bslib::card(
      bslib::card_header("MRLS: Ajuste, resumen clásico y lectura rápida"),
      fluidRow(
        # -------- Columna izquierda: teoría + summary(lm) --------
        column(
          width = 6,
          tags$p(
            "Ajustamos el modelo lineal: ",
            "\\( y = \\beta_0 + \\beta_1 x + \\varepsilon \\), ",
            "donde \\(\\varepsilon \\sim N(0, \\sigma^2)\\) bajo los supuestos clásicos. ",
            "La estimación de ", HTML("&beta;"), " se realiza por Mínimos Cuadrados Ordinarios (OLS), ",
            "minimizando la suma de cuadrados de los residuos. ",
            "R² mide proporción de variabilidad explicada; p-valores evalúan evidencia contra H₀: β₁ = 0."
          ),
          tags$small(
            class = "text-muted",
            "Buenas prácticas: antes de interpretar β₁ como efecto de 'x' sobre 'y', ",
            "verificar supuestos con diagnóstico gráfico (Pestaña 2)."
          ),
          tags$hr(),
          verbatimTextOutput(ns("s3_lm_summary"))
        ),
        
        # -------- Columna derecha: gráfico + TL;DR del modelo --------
        column(
          width = 6,
          plotOutput(ns("s3_fit_plot"), height = "330px"),
          tags$small(
            "La banda sombreada representa el intervalo de confianza (95%) de la recta ajustada. ",
            "Visualmente, buscamos una tendencia aproximadamente lineal y una dispersión homogénea ",
            "alrededor de la recta."
          ),
          tags$hr(),
          tags$strong("Resumen rápido del MRLS (lectura para informe):"),
          tags$br(),
          tags$small(
            class = "text-muted",
            "Útil para escribir la conclusión del análisis sin revisar todo el `summary(lm)`."
          ),
          tags$pre(
            style = "background:#0f172a; color:#e5e7eb; border-radius:0.5rem; padding:0.5rem; font-size:0.8rem;",
            textOutput(ns("s3_lm_tldr"))
          )
        )
      )
    )
  )
}

# Pestaña 2: Diagnóstico
pestanna2_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Diagnóstico",
    withMathJax(
      # 1) Introducción teórica breve
      tags$div(class = "mb-3",
        tags$p(
          strong("Objetivo: "),
          "verificar si el Modelo de Regresión Lineal Simple (MRLS) es compatible con sus supuestos básicos."
        ),
        tags$ul(
          tags$li(
            strong("Linealidad:"),
            " la relación media entre \\(x\\) e \\(y\\) es aproximadamente lineal."
          ),
          tags$li(
            strong("Homoscedasticidad:"),
            " la varianza de los residuos es aproximadamente constante a lo largo del rango de \\(x\\) ",
            "(no se abre ni se cierra el 'abanico')."
          ),
          tags$li(
            strong("Normalidad (aprox.):"),
            " los residuos se distribuyen de forma aproximadamente normal; importante para IC y p-valores."
          ),
          tags$li(
            strong("Independencia:"),
            " las observaciones son independientes (no hay autocorrelación temporal/espacial evidente)."
          )
        ),
        tags$p(
          class = "small text-muted",
          "Aquí trabajaremos sobre el modelo lineal base \\( y = \\beta_0 + \\beta_1 x + \\varepsilon \\). ",
          "Las ideas siguen textos como Gelman & Hill (2007) y Fox & Weisberg (2019, paquete ",
          code("car"),
          ")."
        )
      ),

      fluidRow(
        # Columna izquierda: configuración + pruebas formales
        column(
          width = 4,
          bslib::card(
            bslib::card_header("Configuración de diagnóstico"),
            tags$p(
              "Usaremos por defecto el modelo lineal base ",
              code("y ~ x"),
              ". En extensiones futuras se podría añadir el cuadrático/logarítmico."
            ),
            selectInput(
              ns("s3_diag_model"),
              "Modelo para diagnosticar:",
              choices = c("Lineal (y ~ x)" = "lin"),
              selected = "lin"
            ),
            sliderInput(
              ns("s3_bins_groups"),
              "Agrupar x en k quantiles (para Levene):",
              min = 2, max = 8, value = 4
            ),
            actionButton(
              ns("s3_run_diag"),
              "Ejecutar diagnóstico",
              class = "btn-secondary w-100"
            ),
            tags$hr(class = "my-2"),
            tags$small(
              strong("Guía rápida: "),
              "1) Haz clic en 'Ejecutar diagnóstico'. 2) Revisa la nube de residuos ",
              "(¿patrones claros?). 3) Revisa el Q-Q plot (¿cola muy pesada?). ",
              "4) Lee el resumen de Levene (si hay grupos de varianza muy distintos)."
            )
          ),

          bslib::card(
            bslib::card_header("Prueba formal (opcional)"),
            tags$p(
              "Si está disponible el paquete ", code("car"), ", se ejecuta ",
              code("leveneTest(resid ~ grupo_x)"),
              " sobre grupos de ", code("x"), " definidos por cuantiles."
            ),
            verbatimTextOutput(ns("s3_levene_out")),
            tags$small(
              class = "text-muted",
              "Regla práctica: p-valor Levene < 0.05 ⇒ evidencia de varianzas distintas entre grupos ",
              "(heterocedasticidad). p-valor grande ⇒ no se detecta una diferencia clara en varianzas."
            )
          )
        ),

        # Columna derecha: gráficos + lectura rápida
        column(
          width = 8,
          bslib::card(
            bslib::card_header("Gráficos de diagnóstico del MRLS"),
            fluidRow(
              column(
                width = 6,
                plotOutput(ns("s3_resid_fitted"), height = "280px"),
                tags$small(
                  strong("Residuos vs. Ajustados: "),
                  "busque una 'nube sin forma'. Evitar: ",
                  "patrones en U, V o abanico (varianza que aumenta/disminuye con el nivel)."
                )
              ),
              column(
                width = 6,
                plotOutput(ns("s3_qqplot"), height = "280px"),
                tags$small(
                  strong("Q-Q Normal: "),
                  "puntos aproximadamente sobre la línea roja ⇒ residuos ~ normales. ",
                  "Desviaciones sistemáticas en colas pueden indicar asimetría o colas pesadas."
                )
              )
            ),
            tags$hr(),
            tags$strong("Lectura rápida del diagnóstico"),
            tags$p(
              class = "small text-muted mb-1",
              "Este resumen combina la inspección visual con el resultado de Levene ",
              "(si aplica). Úselo como apoyo; la interpretación final debe considerar el ",
              "contexto agronómico y la magnitud de los efectos."
            ),
            textOutput(ns("s3_diag_tldr"))
          )
        )
      )
    )
  )
}

# Pestaña 3: No lineales & Selección
pestanna3_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) No lineales & Selección",
    
    # Introducción teórica breve
    tags$div(
      class = "mb-3",
      tags$p(
        "En agronomía muchas respuestas no son estrictamente lineales: ",
        "aparecen rendimientos decrecientes, óptimos de dosis o densidad y efectos de saturación. ",
        "En esta pestaña comparamos, sobre el mismo conjunto de datos, tres familias sencillas:"
      ),
      tags$ul(
        tags$li(
          strong("Lineal: "), code("y ~ x"),
          " — relación aproximadamente proporcional; sin óptimo interno."
        ),
        tags$li(
          strong("Cuadrático: "), code("y ~ x + I(x^2)"),
          " — permite curvatura y un óptimo interno ",
          HTML("\\(x^* = -\\beta_1 / (2\\beta_2)\\) cuando \\(\\beta_2 < 0\\); "),
          "típico en curvas dosis–respuesta o densidad–rendimiento."
        ),
        tags$li(
          strong("Logarítmico: "), code("y ~ log(x)"),
          " — rendimientos decrecientes: el impacto marginal de aumentar ",
          "x se reduce a medida que crece el insumo; precursor de modelos más complejos ",
          "(Gompertz, logístico, etc.)."
        )
      ),
      tags$p(
        "La selección se basa en dos ejes: ",
        strong("parsimonia (AIC)"),
        " y ",
        strong("capacidad explicativa (R² ajustado)"),
        ". Cuando varios modelos tienen ΔAIC pequeño, la decisión final debe ser",
        " biológica (forma de la curva, plausibilidad agronómica) más que puramente numérica."
      )
    ),

    # ------------------------------------------------------------------
    # 1) Ajuste paralelo y tabla de selección
    # ------------------------------------------------------------------
    bslib::card(
      bslib::card_header("Ajuste paralelo de modelos y selección (AIC + R²aj)"),
      fluidRow(
        column(
          width = 4,
          tags$div(
            class = "mb-2 small text-muted text-uppercase fw-bold",
            "Paso 1 · Elige modelos y base de datos"
          ),
          checkboxGroupInput(
            ns("s3_models_to_fit"),
            "Modelos a considerar",
            choices = c(
              "Lineal: y ~ x"             = "lin",
              "Cuadrático: y ~ x + I(x^2)" = "quad",
              "Logarítmico: y ~ log(x)"   = "log"
            ),
            selected = c("lin", "quad", "log")
          ),
          checkboxInput(
            ns("s3_lock_same_data"),
            "Forzar misma base de datos para todos (usa solo x > 0 si incluye log)",
            value = TRUE
          ),
          actionButton(
            ns("s3_run_compare"),
            "Ajustar y comparar",
            class = "btn btn-primary w-100 mt-2"
          ),
          tags$hr(),
          tags$div(
            class = "small text-muted",
            tags$strong("Lectura rápida de la tabla:"),
            tags$ul(
              tags$li(code("R2_aj"), ": fracción de variación explicada, penalizada por nº de parámetros."),
              tags$li(code("AIC"), ": menor ⇒ mejor equilibrio ajuste–complejidad."),
              tags$li(
                code("ΔAIC"), ": diferencia respecto al mejor AIC.",
                " Regla orientativa: ΔAIC ≤ 2 (modelos prácticamente equivalentes), ",
                "entre 4–7 (soporte sustancialmente menor), > 10 (poco soporte)."
              ),
              tags$li(
                code("Peso_AIC"), ": probabilidad relativa (normalizada) de que el modelo sea el ‘mejor’ ",
                "dentro del conjunto evaluado."
              )
            )
          )
        ),
        column(
          width = 8,
          DT::dataTableOutput(ns("s3_tbl_models")),
          tags$div(
            class = "mt-2 small",
            tags$strong("Interpretación automática (AIC):"),
            tags$br(),
            textOutput(ns("s3_best_model_msg"))
          )
        )
      )
    ),

    # ------------------------------------------------------------------
    # 2) Comparación visual de curvas
    # ------------------------------------------------------------------
    bslib::card(
      bslib::card_header("Comparación visual de curvas sobre los mismos datos"),
      plotOutput(ns("s3_compare_plot"), height = "360px"),
      tags$small(
        "Los puntos muestran los datos observados (x ~ insumo; y ~ respuesta). ",
        "Las curvas de colores representan las predicciones de cada modelo ",
        "ajustado sobre un rango común de x. El modelo logarítmico sólo se dibuja ",
        "para x > 0 (dominio de log(x)). La leyenda indica la familia de modelo."
      )
    ),

    # ------------------------------------------------------------------
    # 3) Notas de interpretabilidad agronómica
    # ------------------------------------------------------------------
    bslib::card(
      bslib::card_header("Notas de interpretabilidad y buenas prácticas"),
      tags$ul(
        tags$li(
          strong("No todo es R²: "),
          "un modelo más curvo puede inflar R²aj pero ser agronómicamente poco plausible ",
          "(p.ej., predice rendimientos decrecientes excesivos o máximos en rangos no muestreados)."
        ),
        tags$li(
          strong("Cuadrático (óptimos de dosis/densidad): "),
          "si \\(\\beta_2 < 0\\), el óptimo parabólico ",
          HTML("\\(x^* = -\\beta_1 / (2\\beta_2)\\)"),
          " puede interpretarse como dosis o densidad recomendada, ",
          "siempre que caiga dentro del rango de x observado."
        ),
        tags$li(
          strong("Logarítmico (rendimientos decrecientes): "),
          "útil cuando cada incremento adicional del insumo aporta menos que el previo; ",
          "permite discutir conceptos tipo “mitad de la respuesta máxima con el ",
          "primer tramo de dosis” sin llegar a un máximo estricto."
        ),
        tags$li(
          strong("Selección final: "),
          "si varios modelos tienen ΔAIC ≤ 2, considéralos candidatos similares; ",
          "elige apoyándote en la biología del cultivo, el rango de x disponible y la ",
          "destinación del modelo (extrapolar, definir óptimos, comunicar a no estadísticos)."
        )
      )
    )
  )
}

# Pestaña 4: Ejercicios prácticos
pestanna4_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Ejercicios prácticos",
    bslib::card(
      bslib::card_header("Checklist de tareas"),
      tags$ol(
        tags$li("Carga o simula un conjunto de datos con columnas ", code("x"), " (insumo) y ", code("y"), " (respuesta)."),
        tags$li("Ajusta MRLS y verifica supuestos (residuos vs. ajustados; Q-Q)."),
        tags$li("Ajusta modelos cuadrático y logarítmico."),
        tags$li("Compara R²aj y AIC; justifica selección con criterio biológico."),
        tags$li("Grafica las curvas elegidas sobre los datos."),
        tags$li("Reporta β, IC95%, R²aj, AIC y una conclusión agronómica.")
      )
    ),

    bslib::card(
      bslib::card_header("Panel de trabajo"),
      fluidRow(
        column(
          width = 4,
          selectInput(ns("s3_ex_model"),
                      "Modelo focal para reporte:",
                      choices = c("Lineal" = "lin", "Cuadrático" = "quad", "Logarítmico" = "log"),
                      selected = "quad"),
          checkboxInput(ns("s3_ex_use_weights"),
                        "Usar ponderación 1/ŷ (demo de WLS)*", value = FALSE),
          tags$small(
            "* Demo sencilla de ponderación tipo varianza ∝ ŷ². Use con cautela; base conceptual fuera del alcance. ",
            "Revise diagnóstico antes."
          ),
          downloadButton(ns("s3_dl_report"), "Descargar resumen (.txt)",
                         class = "btn-success w-100 mt-2")
        ),
        column(
          width = 8,
          verbatimTextOutput(ns("s3_explain_model")),
          plotOutput(ns("s3_ex_plot"), height = "320px")
        )
      )
    )
  )
}

# Pestaña Extra: Esquemas Visuales y Conceptuales
pestanna_extra_session3_v3UI <- function(ns) {
  # Definir la ruta base para facilitar cambios futuros
  base_path <- "images/sesiones/Disenos_estadisticos_V3/optimizada/"
  img_path  <- paste0(base_path, "session3/")
  
  bslib::nav_panel(
    title = "Extra: Esquemas Visuales",
    icon = icon("chalkboard"), # Icono de pizarra
    
    div(class = "container-fluid",
      h4(class = "section-header", "Conceptos Gráficos de Regresión"),
      p("Esta sección traduce las ecuaciones matemáticas a intuiciones visuales para facilitar la interpretación biológica y el diagnóstico."),
      
      # Navegación interna con pestañas subrayadas para un look limpio
      bslib::navset_card_underline(
        
        # --- Sub-pestaña 1: La Mecánica de OLS ---
        bslib::nav_panel(
          title = "La Máquina OLS",
          div(class = "row align-items-center",
            div(class = "col-md-7",
              tags$img(
                src = paste0(img_path, "ols_mechanism_visualization.webp"), 
                class = "img-fluid shadow-sm border rounded",
                alt = "Interpretación geométrica de Mínimos Cuadrados Ordinarios",
                style = "width: 100%; min-height: 300px; object-fit: contain; background-color: #ffffff;"
              )
            ),
            div(class = "col-md-5",
              div(class = "card bg-light border-0",
                div(class = "card-body",
                  h5("¿Qué significa 'Mínimos Cuadrados'?"),
                  p("Cuando R ejecuta ", code("lm(y ~ x)"), ", no está adivinando. Está resolviendo un problema geométrico:"),
                  tags$ol(
                    tags$li("Calcula la distancia vertical (residuo) de cada punto a la línea."),
                    tags$li("Eleva esa distancia al cuadrado (formando los cuadrados azules de la imagen)."),
                    tags$li("Mueve la línea hasta que la ", strong("suma total del área"), " de esos cuadrados sea la mínima posible.")
                  ),
                  hr(),
                  p(class = "small text-muted", 
                    "Por esto los valores atípicos (outliers) 'tiran' tanto de la línea: un residuo grande, al elevarse al cuadrado, penaliza muchísimo al modelo.")
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 2: Diagnóstico Visual ---
        bslib::nav_panel(
          title = "Guía de Diagnóstico",
          div(class = "row",
            div(class = "col-md-12 mb-2",
              p("El gráfico de 'Residuos vs. Ajustados' es el detector de mentiras del modelo. Aprende a leer sus patrones.")
            ),
            div(class = "col-md-8 offset-md-2",
              tags$img(
                src = paste0(img_path, "diagnostic_patterns_panel.webp"), 
                class = "img-fluid shadow border rounded mb-3",
                alt = "Patrones de diagnóstico: Homocedasticidad vs Heterocedasticidad vs No linealidad",
                style = "width: 100%;"
              )
            ),
            div(class = "col-md-12",
              div(class = "d-flex justify-content-around",
                div(class = "p-3 bg-light border rounded", style = "width: 30%;",
                    strong("1. Nube sin forma"), br(), "Significa que el modelo ha capturado toda la señal y solo queda ruido aleatorio. ¡Es lo que buscamos!"),
                div(class = "p-3 bg-light border rounded", style = "width: 30%;",
                    strong("2. El Megáfono"), br(), "Indica Heterocedasticidad. La varianza crece con la media. Solución: Transformar Y (log) o usar modelos ponderados (WLS)."),
                div(class = "p-3 bg-light border rounded", style = "width: 30%;",
                    strong("3. La Sonrisa (U)"), br(), "Indica que falta un término cuadrático. Estás intentando ajustar una curva con una recta.")
              )
            )
          )
        ),
        
        # --- Sub-pestaña 3: Interpretación Biológica ---
        bslib::nav_panel(
          title = "Modelos en la Biología",
          div(class = "row align-items-center",
             div(class = "col-md-4",
               div(class = "card h-100 border-primary",
                 div(class = "card-header bg-primary text-white", "Lineal"),
                 div(class = "card-body",
                   p("$$y = \\beta_0 + \\beta_1 x$$"),
                   p(tags$small("Uso: Rangos cortos de insumo.")),
                   hr(),
                   p("Asume que la planta responde igual a la primera dosis que a la última. No contempla saciedad ni toxicidad.")
                 )
               )
             ),
             div(class = "col-md-8",
               tags$img(
                 src = paste0(img_path, "agronomic_curves_interpretation.webp"), 
                 class = "img-fluid shadow-sm rounded",
                 alt = "Comparación de curvas lineal, cuadrática y logarítmica",
                 style = "width: 100%;"
               )
             ),
             div(class = "col-md-12 mt-3",
               div(class = "alert alert-secondary",
                 h6("Claves de decisión agronómica:"),
                 tags$ul(
                   tags$li(strong("Cuadrático:"), " Úsalo si esperas un óptimo biológico (dosis máxima antes de quemar el cultivo). Busca el vértice de la parábola."),
                   tags$li(strong("Logarítmico:"), " Úsalo para 'Ley de rendimientos decrecientes'. Añadir más fertilizante ayuda, pero cada vez menos.")
                 )
               )
             )
          )
        )
      )
    )
  )
}

# Pestaña 5: Referencias
pestanna5_session3_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "Referencias",
    tags$ul(
      tags$li("Akaike, H. (1974). A new look at the statistical model identification. ", tags$em("IEEE Transactions on Automatic Control"), ". ",
              tags$a(href="https://eclass.uoa.gr/modules/document/file.php/MATH452/%CE%86%CF%81%CE%B8%CF%81%CE%B1/Akaike_1974.pdf", "PDF"), " ", tags$span(style="opacity:.6;","(AIC)")),
      tags$li("Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference: understanding AIC and BIC in model selection. ",
              tags$em("Sociological Methods & Research"), ". ",
              tags$a(href="https://sites.warnercnr.colostate.edu/wp-content/uploads/sites/73/2017/05/Burnham-and-Anderson-2004-SMR.pdf", "PDF")),
      tags$li("Burnham, K. P., & Anderson, D. R. (2002). ", tags$em("Model Selection and Multimodel Inference (2nd ed.)"), ". ",
              tags$a(href="https://link.springer.com/content/pdf/10.1007/b97636.pdf", "Springer PDF (preview)")),
      tags$li("Gelman, A., & Hill, J. (2007). ", tags$em("Data Analysis Using Regression and Multilevel/Hierarchical Models"), ". ",
              tags$a(href="https://api.pageplace.de/preview/DT0400.9780511266836_A23690811/preview-9780511266836_A23690811.pdf", "Preview PDF")),
      tags$li("Fox, J., & Weisberg, S. (2019/2024). ", tags$em("An R Companion to Applied Regression"), " & manual del paquete ", code("car"), ". ",
              tags$a(href="https://cran.r-project.org/package=car/car.pdf", "Manual (CRAN)")),
      tags$li("Wickham, H. (2016/2023). ", tags$em("ggplot2: Elegant Graphics for Data Analysis"), ". ",
              tags$a(href="https://ggplot2-book.org/", "Libro online"), " | ",
              tags$a(href="https://ggplot2.tidyverse.org/reference/geom_smooth.html", "geom_smooth()")),
      tags$li("Yahuza, I. (2011). Yield-density equations and their application for agronomic research: a review. ",
              tags$a(href="https://www.cabidigitallibrary.org/doi/pdf/10.5555/20113335598", "PDF CABI")),
      tags$li("Besteiro, R., et al. (2023/2024). Linear and Nonlinear Mixed Models to Determine the Growth Curves of Weaned Piglets. ",
              tags$a(href="https://www.mdpi.com/2077-0472/14/1/79", "MDPI link")),
      tags$li("R base docs: ", code("stats::AIC"), " y ", code("stats::extractAIC"), ". ",
              tags$a(href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/AIC", "AIC"), " | ",
              tags$a(href="https://stat.ethz.ch/R-manual//R-patched/library/stats/help/extractAIC.html", "extractAIC"))
    ),
    tags$hr(),
    tags$small("Estas referencias sustentan la selección por AIC/parsimonia, el uso de MRLS y el diagnóstico estándar en regresión; se recomiendan para profundizar.")
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session3_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(class = "session-title", "Sesión 3: Análisis de Regresión (Lineal y No Lineal)"),
    withMathJax(),
    bslib::navset_tab(
      pestanna1_session3_v3UI(ns),
      pestanna2_session3_v3UI(ns),
      pestanna3_session3_v3UI(ns),
      pestanna4_session3_v3UI(ns),
      pestanna_extra_session3_v3UI(ns),
      pestanna5_session3_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: MRLS & Datos
pestanna1_session3_v3_server <- function(
  input, output, session,
  df_rv,          # reactiveVal con el data.frame (x, y)
  make_sim_data,  # función para simular datos
  read_csv_xy,    # función para leer CSV (x, y)
  s3_model_lin    # reactive: lm(y ~ x, data = df_rv())
) {
  # ---------------------------------------------------------------
  # 1) Generación / carga de datos
  # ---------------------------------------------------------------
  
  # Modo simulación
  observeEvent(input$s3_gen, {
    df_rv(
      make_sim_data(
        n     = input$s3_n,
        b0    = input$s3_beta0,
        b1    = input$s3_beta1,
        sigma = input$s3_sigma,
        quad  = input$s3_quadratic_truth,
        b2    = input$s3_beta2_truth
      )
    )
  })
  
  # Modo CSV
  observeEvent(input$s3_file, {
    req(input$s3_data_mode == "csv")
    f <- input$s3_file$datapath
    
    tryCatch(
      {
        df_rv(
          read_csv_xy(
            path   = f,
            header = isTRUE(input$s3_has_header),
            sep    = input$s3_sep
          )
        )
      },
      error = function(e) {
        showModal(
          modalDialog(
            title = "Error al leer CSV",
            tags$p(
              "No se pudo leer el archivo. Revisa que tenga columnas ",
              code("x"), " y ", code("y"), 
              " numéricas y que el separador sea el correcto."
            ),
            tags$pre(conditionMessage(e)),
            easyClose = TRUE
          )
        )
      }
    )
  })
  
  # ---------------------------------------------------------------
  # 2) Tabla de exploración
  # ---------------------------------------------------------------
  output$s3_tbl_preview <- DT::renderDataTable({
    df <- df_rv()
    req(nrow(df) > 1)
    
    if (isTRUE(input$s3_show_preview)) {
      DT::datatable(
        head(df, 10),
        rownames = FALSE,
        options  = list(dom = "tip", pageLength = 10)
      )
    } else {
      DT::datatable(
        df,
        rownames = FALSE,
        options  = list(pageLength = 10)
      )
    }
  })
  
  # ---------------------------------------------------------------
  # 3) Gráfico base (nube de puntos)
  # ---------------------------------------------------------------
  output$s3_scatter_base <- renderPlot({
    df <- df_rv()
    req(nrow(df) > 1)
    
    n_obs <- nrow(df)
    modo  <- if (identical(input$s3_data_mode, "csv")) "Datos CSV" else "Datos simulados"
    
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.65, size = 2.5) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title    = sprintf("%s (x ~ insumo; y ~ respuesta)", modo),
        subtitle = sprintf("n = %d observaciones", n_obs),
        x        = "x (insumo)",
        y        = "y (respuesta)"
      )
  })
  
  # ---------------------------------------------------------------
  # 4) Resumen clásico del MRLS (summary(lm))
  # ---------------------------------------------------------------
  output$s3_lm_summary <- renderPrint({
    m <- s3_model_lin()
    summary(m)
  })
  
  # ---------------------------------------------------------------
  # 5) Gráfico del ajuste MRLS con IC95%
  # ---------------------------------------------------------------
  output$s3_fit_plot <- renderPlot({
    df <- df_rv()
    req(nrow(df) > 1)
    
    ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::geom_smooth(
        method  = "lm",
        formula = y ~ x,
        se      = TRUE
      ) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "MRLS: y ~ x (recta ajustada + IC95%)",
        x     = "x (insumo)",
        y     = "y (respuesta)"
      )
  })
  
  # ---------------------------------------------------------------
  # 6) Resumen rápido (TL;DR) del modelo lineal
  # ---------------------------------------------------------------
  output$s3_lm_tldr <- renderText({
    m  <- s3_model_lin()
    sm <- summary(m)
    
    # Tamaño de muestra, R2 y R2 ajustado
    n      <- stats::nobs(m)
    r2     <- sm$r.squared
    r2_adj <- sm$adj.r.squared
    
    coefs <- sm$coefficients
    # β0 y β1
    b0  <- coefs[1, 1]
    b1  <- coefs[2, 1]
    se1 <- coefs[2, 2]
    p1  <- coefs[2, 4]
    
    # Texto p-valor amigable
    p_txt <- if (is.na(p1)) {
      "p-valor(β₁) = NA"
    } else if (p1 < 1e-4) {
      "p-valor(β₁) < 0.0001"
    } else {
      sprintf("p-valor(β₁) = %.4f", p1)
    }
    
    paste0(
      "Modelo: y ~ x\n",
      sprintf("n = %d | R2 = %.3f | R2 ajustado = %.3f\n", n, r2, r2_adj),
      sprintf("β0 (intercepto) ≈ %.3f\n", b0),
      sprintf("β1 (pendiente)  ≈ %.3f (SE = %.3f)\n", b1, se1),
      p_txt, "\n\n",
      if (!is.na(p1) && p1 < 0.05) {
        "Interpretación rápida: hay evidencia estadística de asociación lineal entre x e y (β₁ ≠ 0). "
      } else {
        "Interpretación rápida: con estos datos no hay evidencia estadística fuerte de una pendiente distinta de 0. "
      },
      "Recuerda revisar los diagnósticos (Pestaña 2) antes de interpretar causalmente."
    )
  })
}

# Pestaña 2: Diagnóstico
pestanna2_session3_v3_server <- function(input, output, session, df_rv, s3_model_lin) {

  # Disparador explícito (para que req() funcione con botón)
  observeEvent(input$s3_run_diag, {
    # No hacemos nada; solo sirve para que los req(input$s3_run_diag > 0)
    # sepan que el usuario pidió diagnóstico.
    invisible(TRUE)
  })

  # -----------------------------
  # 1) Gráfico Residuos vs Ajustados
  # -----------------------------
  output$s3_resid_fitted <- renderPlot({
    req(input$s3_run_diag > 0)

    m <- s3_model_lin()
    df_plot <- tibble::tibble(
      fitted = stats::fitted(m),
      resid  = stats::residuals(m)
    )

    ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_hline(
        yintercept = 0,
        linetype   = "dashed",
        color      = "gray50"
      ) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Residuos vs. Valores ajustados",
        x     = "Valores ajustados (ŷ)",
        y     = "Residuos (y - ŷ)"
      )
  })

  # -----------------------------
  # 2) Q-Q plot de residuos
  # -----------------------------
  output$s3_qqplot <- renderPlot({
    req(input$s3_run_diag > 0)

    m  <- s3_model_lin()
    df <- tibble::tibble(resid = stats::residuals(m))

    ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq(alpha = 0.7) +
      ggplot2::stat_qq_line(color = "red") +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Q-Q Normal de Residuos",
        x     = "Cuantiles teóricos N(0,1)",
        y     = "Cuantiles de residuos"
      )
  })

  # -----------------------------
  # 3) Prueba de Levene (opcional)
  # -----------------------------
  output$s3_levene_out <- renderPrint({
    req(input$s3_run_diag > 0)

    df <- df_rv()
    req(nrow(df) > 3)

    # Agrupar x en k quantiles (bins para Levene)
    k <- input$s3_bins_groups
    # Evitar problemas si hay muchos valores iguales
    probs <- seq(0, 1, length.out = k + 1)

    qx <- tryCatch(
      stats::quantile(df$x, probs = probs, na.rm = TRUE),
      error = function(e) NA
    )
    if (anyNA(qx)) {
      cat("No se pudieron calcular cuantiles de x para Levene (valores faltantes o constantes).\n")
      return(invisible(NULL))
    }

    bins <- cut(
      df$x,
      breaks = qx,
      include.lowest = TRUE,
      right = TRUE
    )

    m     <- s3_model_lin()
    resid <- stats::residuals(m)

    tmp <- tibble::tibble(
      resid = resid,
      bin   = bins
    )

    if (requireNamespace("car", quietly = TRUE)) {
      out <- car::leveneTest(resid ~ bin, data = tmp, center = median)
      print(out)
    } else {
      cat(
        "Paquete 'car' no disponible.\n",
        "Para ejecutar Levene instale y cargue 'car':\n",
        "  install.packages('car')\n",
        "  library(car)\n"
      )
    }
  })

  # -----------------------------
  # 4) Resumen textual TL;DR del diagnóstico
  # -----------------------------
  output$s3_diag_tldr <- renderText({
    req(input$s3_run_diag > 0)

    df <- df_rv()
    req(nrow(df) > 3)

    m      <- s3_model_lin()
    resid  <- stats::residuals(m)
    fitted <- stats::fitted(m)

    # a) Medida simple de heterocedasticidad (correlación |resid| ~ fitted)
    cor_abs <- suppressWarnings(
      stats::cor(abs(resid), fitted, use = "complete.obs")
    )

    # b) Intentar extraer p-valor de Levene si 'car' está disponible
    p_lev <- NA_real_
    if (requireNamespace("car", quietly = TRUE)) {
      # Reproducimos lo de arriba de forma silenciosa
      k <- input$s3_bins_groups
      probs <- seq(0, 1, length.out = k + 1)
      qx <- tryCatch(
        stats::quantile(df$x, probs = probs, na.rm = TRUE),
        error = function(e) NA
      )
      if (!anyNA(qx)) {
        bins <- cut(
          df$x,
          breaks = qx,
          include.lowest = TRUE,
          right = TRUE
        )
        tmp <- tibble::tibble(
          resid = resid,
          bin   = bins
        )
        lev_out <- tryCatch(
          car::leveneTest(resid ~ bin, data = tmp, center = median),
          error = function(e) NULL
        )
        if (!is.null(lev_out)) {
          # Suponiendo estructura típica: columna "Pr(>F)"
          if ("Pr(>F)" %in% colnames(lev_out)) {
            p_lev <- as.numeric(lev_out[1, "Pr(>F)"])
          }
        }
      }
    }

    # c) Construir mensaje amigable
    partes <- list()

    # Linealidad / homocedasticidad (proxy por cor_abs)
    if (is.finite(cor_abs)) {
      if (abs(cor_abs) < 0.1) {
        partes <- c(
          partes,
          "No se observa un patrón fuerte de aumento/disminución de la dispersión de residuos con el nivel de ŷ (correlación |resid|–ŷ cercana a 0)."
        )
      } else {
        partes <- c(
          partes,
          paste0(
            "La correlación entre |residuo| y ŷ es ~",
            sprintf("%.2f", cor_abs),
            ", lo que sugiere cierto patrón de heterocedasticidad (abanico). Conviene revisar si hay transformaciones o modelos no lineales más apropiados."
          )
        )
      }
    }

    # Levene (si disponible)
    if (is.finite(p_lev)) {
      if (p_lev < 0.05) {
        partes <- c(
          partes,
          paste0(
            "La prueba de Levene detecta diferencias de varianza entre grupos de x (p ≈ ",
            sprintf("%.3f", p_lev),
            "). Esto respalda la sospecha de heterocedasticidad."
          )
        )
      } else {
        partes <- c(
          partes,
          paste0(
            "La prueba de Levene no detecta diferencias claras de varianza entre grupos de x (p ≈ ",
            sprintf("%.3f", p_lev),
            ")."
          )
        )
      }
    } else {
      partes <- c(
        partes,
        "No se pudo calcular un p-valor de Levene (paquete 'car' ausente o problema numérico)."
      )
    }

    # Nota final
    partes <- c(
      partes,
      "Use estos resultados como guía: si los gráficos son razonables y no hay señales fuertes de heterocedasticidad, el MRLS es una aproximación útil. Si ve patrones claros, considere modelos no lineales (pestaña 3) o transformaciones."
    )

    paste(partes, collapse = " ")
  })
}

# Pestaña 3: No lineales & Selección
pestanna3_session3_v3_server <- function(input, output, session, df_rv) {

  # Helper para ajustar lm de forma segura (devuelve NULL si falla)
  safe_lm <- function(formula, data) {
    tryCatch(
      stats::lm(formula, data = data),
      error = function(e) NULL
    )
  }

  # ------------------------------------------------------------------
  # 1) Ajuste paralelo de modelos y tabla de resumen
  # ------------------------------------------------------------------
  s3_fit_models <- eventReactive(input$s3_run_compare, {
    df <- df_rv()
    shiny::req(nrow(df) > 3)

    want <- input$s3_models_to_fit
    if (length(want) == 0) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Sin modelos seleccionados",
          "Seleccione al menos un modelo (lineal, cuadrático y/o logarítmico).",
          easyClose = TRUE
        )
      )
      return(NULL)
    }

    # Si se incluye log, y está activo el check, usamos una base común con x > 0
    if (isTRUE(input$s3_lock_same_data) && "log" %in% want) {
      df_use <- dplyr::filter(df, is.finite(x), x > 0)
    } else {
      df_use <- df
    }

    if (nrow(df_use) < 5) {
      shiny::showModal(
        shiny::modalDialog(
          title = "Datos insuficientes",
          "Menos de 5 observaciones tras el filtrado (p.ej. x > 0). Amplíe el rango o revise el CSV.",
          easyClose = TRUE
        )
      )
      return(NULL)
    }

    # Ajustar modelos candidatos
    fits <- list()
    if ("lin"  %in% want) fits$lin  <- safe_lm(y ~ x,                data = df_use)
    if ("quad" %in% want) fits$quad <- safe_lm(y ~ x + I(x^2),       data = df_use)
    if ("log"  %in% want) fits$log  <- safe_lm(y ~ log(x),           data = dplyr::filter(df_use, x > 0))

    # Construir tabla de resumen (R2_aj, AIC, coeficientes)
    res <- purrr::imap_dfr(fits, function(mod, name) {
      if (is.null(mod)) return(NULL)
      sm <- summary(mod)
      tibble::tibble(
        Modelo      = name,
        R2_aj       = unname(sm$adj.r.squared),
        AIC         = tryCatch(stats::AIC(mod), error = function(e) NA_real_),
        n           = stats::nobs(mod),
        `β0`        = unname(stats::coef(mod)[1]),
        `β1`        = if (length(stats::coef(mod)) >= 2) unname(stats::coef(mod)[2]) else NA_real_,
        `β2`        = ifelse(
          name == "quad" && length(stats::coef(mod)) >= 3,
          unname(stats::coef(mod)[3]),
          NA_real_
        )
      )
    })

    if (nrow(res) > 0) {
      # Calcular ΔAIC y peso de Akaike
      finite_idx <- which(is.finite(res$AIC))
      if (length(finite_idx) > 0) {
        AIC_min <- min(res$AIC[finite_idx])
        res$Delta_AIC <- res$AIC - AIC_min
        lik <- exp(-0.5 * res$Delta_AIC)
        lik[!is.finite(lik)] <- NA_real_
        sum_lik <- sum(lik[finite_idx], na.rm = TRUE)
        res$Peso_AIC <- if (sum_lik > 0) lik / sum_lik else NA_real_
      } else {
        res$Delta_AIC <- NA_real_
        res$Peso_AIC  <- NA_real_
      }

      # Etiquetas legibles
      res <- dplyr::mutate(
        res,
        Modelo = dplyr::recode(
          Modelo,
          lin  = "Lineal (y ~ x)",
          quad = "Cuadrático (y ~ x + I(x^2))",
          log  = "Logarítmico (y ~ log(x))"
        )
      )

      # Ordenar por AIC creciente
      res <- dplyr::arrange(res, AIC)
    }

    list(
      fits   = fits,
      table  = res,
      df_use = df_use
    )
  })

  # Tabla de resumen con estrella en el mejor AIC
  output$s3_tbl_models <- DT::renderDataTable({
    fm <- s3_fit_models()
    shiny::req(!is.null(fm), nrow(fm$table) > 0)

    out <- fm$table

    # Redondeos para visualización
    out$R2_aj      <- round(out$R2_aj, 4)
    out$AIC        <- round(out$AIC, 2)
    out$Delta_AIC  <- round(out$Delta_AIC, 2)
    out$Peso_AIC   <- round(out$Peso_AIC, 3)
    out$`β0`       <- round(out$`β0`, 3)
    out$`β1`       <- round(out$`β1`, 3)
    out$`β2`       <- round(out$`β2`, 5)

    # Marcar mejor modelo por AIC (ΔAIC mínimo entre valores finitos)
    out$`← Mejor (AIC)` <- ""
    finite_idx <- which(is.finite(out$AIC))
    if (length(finite_idx) > 0) {
      best_i <- finite_idx[which.min(out$AIC[finite_idx])]
      out$`← Mejor (AIC)`[best_i] <- "★"
    }

    DT::datatable(
      out,
      rownames = FALSE,
      options = list(
        pageLength = 5,
        dom        = "tip"
      )
    )
  })

  # Mensaje interpretativo automático sobre AIC
  output$s3_best_model_msg <- renderText({
    fm <- s3_fit_models()
    shiny::req(!is.null(fm), nrow(fm$table) > 0)

    tab <- fm$table
    finite_idx <- which(is.finite(tab$AIC))
    if (length(finite_idx) == 0) {
      return("No se pudo calcular AIC para los modelos ajustados. Revise el ajuste o el conjunto de datos.")
    }

    best_i <- finite_idx[which.min(tab$AIC[finite_idx])]
    best   <- tab[best_i, ]

    # Texto base con nombre + AIC + peso
    msg <- sprintf(
      "Mejor según AIC: %s (AIC ≈ %.2f, ΔAIC = 0, Peso_AIC ≈ %.3f).",
      best$Modelo,
      best$AIC,
      best$Peso_AIC
    )

    # Buscar modelos competidores con ΔAIC ≤ 2
    compet_idx <- which(
      is.finite(tab$Delta_AIC) &
        tab$Delta_AIC <= 2 &
        seq_len(nrow(tab)) != best_i
    )

    if (length(compet_idx) > 0) {
      compet_names <- paste(tab$Modelo[compet_idx], collapse = "; ")
      msg <- paste0(
        msg,
        " Hay modelos competidores con ΔAIC ≤ 2: ",
        compet_names,
        ". En este caso, use criterio agronómico (forma de la curva, óptimos dentro del rango, ",
        "simplicidad para comunicar) más que una diferencia mínima de AIC."
      )
    } else {
      msg <- paste0(
        msg,
        " No hay otros modelos con ΔAIC ≤ 2; el modelo seleccionado tiene claramente ",
        "más soporte dentro del conjunto evaluado."
      )
    }

    msg
  })

  # ------------------------------------------------------------------
  # 2) Comparación visual de curvas sobre el rango de x
  # ------------------------------------------------------------------
  output$s3_compare_plot <- renderPlot({
    fm <- s3_fit_models()
    shiny::req(!is.null(fm), nrow(fm$table) > 0)

    df   <- df_rv()
    fits <- fm$fits
    want <- input$s3_models_to_fit

    # Gráfico base con puntos
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Comparación visual de modelos (curvas sobre los datos)",
        x     = "x (insumo)",
        y     = "y (respuesta)"
      )

    # Secuencia de x para dibujar las curvas
    x_range <- range(df$x, na.rm = TRUE)
    x_grid  <- seq(from = x_range[1], to = x_range[2], length.out = 200)

    curves_list <- list()

    # Lineal
    if ("lin" %in% want && !is.null(fits$lin)) {
      curves_list$lin <- tibble::tibble(
        x      = x_grid,
        y_hat  = stats::predict(fits$lin, newdata = data.frame(x = x_grid)),
        Modelo = "Lineal"
      )
    }

    # Cuadrático
    if ("quad" %in% want && !is.null(fits$quad)) {
      curves_list$quad <- tibble::tibble(
        x      = x_grid,
        y_hat  = stats::predict(fits$quad, newdata = data.frame(x = x_grid)),
        Modelo = "Cuadrático"
      )
    }

    # Logarítmico (solo para x > 0)
    if ("log" %in% want && !is.null(fits$log)) {
      x_pos <- x_grid[x_grid > 0]
      if (length(x_pos) > 0) {
        curves_list$log <- tibble::tibble(
          x      = x_pos,
          y_hat  = stats::predict(fits$log, newdata = data.frame(x = x_pos)),
          Modelo = "Logarítmico"
        )
      }
    }

    if (length(curves_list) > 0) {
      df_curves <- dplyr::bind_rows(curves_list)

      p <- p +
        ggplot2::geom_line(
          data = df_curves,
          ggplot2::aes(x = x, y = y_hat, color = Modelo),
          linewidth = 1.1
        ) +
        ggplot2::scale_color_manual(
          values = c(
            "Lineal"       = "#1f77b4",
            "Cuadrático"   = "#d62728",
            "Logarítmico"  = "#2ca02c"
          ),
          name = "Modelo"
        )
    }

    p
  })
}

# Pestaña 4: Ejercicios prácticos
pestanna4_session3_v3_server <- function(input, output, session, df_rv) {

  # ------------------------------------------------------------------
  # 1) Ajuste del modelo focal (OLS o WLS) según selección del usuario
  # ------------------------------------------------------------------
  s3_fit_focal <- reactive({
    df  <- df_rv()
    req(nrow(df) > 3)

    exm <- req(input$s3_ex_model)

    # Subconjuntos para el caso logarítmico
    df_log <- dplyr::filter(df, is.finite(x), x > 0)

    # Chequeo: si el modelo escogido es log y no hay suficientes datos x>0
    if (exm == "log" && nrow(df_log) < 5) {
      showModal(
        modalDialog(
          title = "Datos insuficientes para modelo logarítmico",
          "Para el modelo y ~ log(x) se requieren suficientes observaciones con x>0. ",
          "En tus datos actuales, hay menos de 5 observaciones con x>0. ",
          "Elige otro modelo o revisa el rango de x.",
          easyClose = TRUE
        )
      )
      return(NULL)
    }

    # Definición de fórmula y data base según modelo
    if (exm == "lin") {
      form <- y ~ x
      df_use <- df
    } else if (exm == "quad") {
      form <- y ~ x + I(x^2)
      df_use <- df
    } else { # "log"
      form <- y ~ log(x)
      df_use <- df_log
    }

    use_wls <- isTRUE(input$s3_ex_use_weights)

    if (!use_wls) {
      # Mínimos cuadrados ordinarios (OLS)
      mod <- stats::lm(form, data = df_use)
    } else {
      # Mínimos cuadrados ponderados (WLS) con pesos 1/ŷ^2
      # 1) Ajuste preliminar para obtener ŷ
      m0   <- stats::lm(form, data = df_use)
      yhat <- pmax(stats::fitted(m0), .Machine$double.eps)
      w    <- 1 / (yhat^2)

      # 2) Ajuste ponderado
      mod  <- stats::lm(form, data = df_use, weights = w)
    }

    mod
  })

  # ------------------------------------------------------------------
  # 2) Salida explicativa del modelo focal (para usar como base de reporte)
  # ------------------------------------------------------------------
  output$s3_explain_model <- renderPrint({
    m <- s3_fit_focal()
    req(!is.null(m))

    sm  <- summary(m)
    aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)

    # Tipo de modelo
    form_chr <- deparse(formula(m))

    # Texto introductorio
    cat("== Resumen del modelo focal ==\n\n")
    cat("Fórmula ajustada:\n")
    print(sm$call)
    cat("\n")

    # Métricas globales
    cat(
      sprintf(
        "n = %d | R2 ajustado = %.4f | AIC = %.2f\n\n",
        stats::nobs(m),
        sm$adj.r.squared,
        aic
      )
    )

    # Recordatorio conceptual sobre R²aj y AIC
    cat("Notas rápidas:\n")
    cat("- R2 ajustado cuantifica la proporción de variación explicada, penalizando predictores extra.\n")
    cat("- AIC más bajo indica modelo más parsimonioso (mejor equilibrio ajuste-complejidad);\n")
    cat("  diferencias de AIC < 2 suelen considerarse evidencia similar entre modelos competidores.\n\n")

    # Tabla de coeficientes
    cat("Coeficientes (estimado, error estándar, t, p-valor):\n")
    print(sm$coefficients)
    cat("\n")

    # Interpretación básica de coeficientes (cuando aplica)
    coefs <- stats::coef(m)

    if ("x" %in% names(coefs)) {
      b1 <- unname(coefs["x"])
      cat(
        sprintf(
          "Interpretación β1 (x): un incremento de 1 unidad en x se asocia,\n",
          "en promedio, con un cambio de %.3f unidades en y (manteniendo el resto igual).\n",
          b1
        )
      )
    }

    # Caso cuadrático: vértice / dosis óptima
    if (length(coefs) >= 3 && "I(x^2)" %in% names(coefs)) {
      b1 <- unname(coefs["x"])
      b2 <- unname(coefs["I(x^2)"])
      if (!is.na(b2) && b2 != 0) {
        x_opt <- -b1 / (2 * b2)
        cat("\nModelo cuadrático detectado.\n")
        cat(
          sprintf(
            "Óptimo parabólico (vértice): x* = -β1 / (2β2) ≈ %.3f.\n",
            x_opt
          )
        )

        # Rango de x observado
        df_all <- df_rv()
        xr     <- range(df_all$x, na.rm = TRUE)

        cat(
          sprintf(
            "Rango observado de x en los datos: [%.3f, %.3f].\n",
            xr[1], xr[2]
          )
        )

        if (x_opt >= xr[1] && x_opt <= xr[2]) {
          cat(
            "x* cae DENTRO del rango observado: interpretable como dosis o densidad óptima aproximada.\n"
          )
        } else {
          cat(
            "x* cae FUERA del rango observado: interpretar con cautela, puede ser una extrapolación.\n"
          )
        }
      }
    }

    # Nota sobre ponderación WLS, si se está usando
    if (isTRUE(input$s3_ex_use_weights)) {
      cat(
        "\nPonderación:\n",
        "- Modelo ajustado con Weighted Least Squares (WLS) usando pesos 1/ŷ^2.\n",
        "- Esta elección es coherente con un patrón de heterocedasticidad donde var(ε) crece\n",
        "  aproximadamente con el cuadrado de la media (var(ε) ≈ c·μ^2).\n",
        "  Es una DEMO didáctica; en práctica, la estructura de varianzas se debe justificar\n",
        "  con diagnóstico y conocimiento del sistema.\n",
        sep = ""
      )
    } else {
      cat(
        "\nPonderación:\n",
        "- Modelo ajustado por Mínimos Cuadrados Ordinarios (OLS) sin pesos.\n",
        "  Revise la pestaña de Diagnóstico para confirmar que la varianza residual\n",
        "  es aproximadamente constante y que no hay patrones sistemáticos.\n",
        sep = ""
      )
    }

    cat("\n")
  })

  # ------------------------------------------------------------------
  # 3) Gráfico del modelo focal sobre los datos
  # ------------------------------------------------------------------
  output$s3_ex_plot <- renderPlot({
    df  <- df_rv()
    req(nrow(df) > 3)

    exm <- req(input$s3_ex_model)

    base_plot <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(alpha = 0.6) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        title = "Modelo focal superpuesto a los datos",
        x     = "x (insumo)",
        y     = "y (respuesta)"
      )

    if (exm == "lin") {
      base_plot +
        ggplot2::geom_smooth(
          method  = "lm",
          formula = y ~ x,
          se      = TRUE,
          color   = "#1f77b4"
        )

    } else if (exm == "quad") {
      base_plot +
        ggplot2::geom_smooth(
          method  = "lm",
          formula = y ~ x + I(x^2),
          se      = TRUE,
          color   = "#d62728"
        )

    } else { # log
      base_plot +
        ggplot2::geom_smooth(
          method  = "lm",
          formula = y ~ log(x),
          se      = TRUE,
          data    = dplyr::filter(df, is.finite(x), x > 0),
          color   = "#2ca02c"
        )
    }
  })

  # ------------------------------------------------------------------
  # 4) Descarga de reporte de texto (.txt)
  # ------------------------------------------------------------------
  output$s3_dl_report <- downloadHandler(
    filename = function() {
      paste0("Sesion3_regresion_resumen_", Sys.Date(), ".txt")
    },
    content = function(file) {
      m <- s3_fit_focal()
      req(!is.null(m))

      sm  <- summary(m)
      aic <- tryCatch(stats::AIC(m), error = function(e) NA_real_)

      coefs <- stats::coef(m)

      sink(file)
      cat("Sesión 3 – Resumen de modelo focal\n")
      cat("=================================\n\n")

      cat("Fórmula del modelo:\n")
      print(sm$call)
      cat("\n")

      cat(sprintf("n = %d\nR2 ajustado = %.4f\nAIC = %.2f\n\n",
                  stats::nobs(m), sm$adj.r.squared, aic))

      cat("Tabla de coeficientes:\n")
      print(sm$coefficients)
      cat("\n")

      # Óptimo cuadrático si aplica
      if (length(coefs) >= 3 && "I(x^2)" %in% names(coefs)) {
        b1 <- unname(coefs["x"])
        b2 <- unname(coefs["I(x^2)"])
        if (!is.na(b2) && b2 != 0) {
          x_opt <- -b1 / (2 * b2)
          cat(sprintf("Óptimo (parabólico) estimado: x* = %.3f\n", x_opt))

          df_all <- df_rv()
          xr     <- range(df_all$x, na.rm = TRUE)
          cat(sprintf("Rango observado de x: [%.3f, %.3f]\n", xr[1], xr[2]))
          if (x_opt >= xr[1] && x_opt <= xr[2]) {
            cat("Nota: x* se encuentra dentro del rango observado (interpretación más confiable).\n\n")
          } else {
            cat("Nota: x* cae fuera del rango observado (extrapolación; interpretar con cautela).\n\n")
          }
        }
      }

      cat("Notas adicionales:\n")
      cat("- Selección de modelo: AIC más bajo suele indicar mejor equilibrio ajuste-complejidad.\n")
      cat("  Diferencias de AIC inferiores a ~2 sugieren soporte similar entre modelos.\n")
      cat("- R2 ajustado indica proporción de variación explicada, penalizando por número de parámetros.\n")
      cat("- Verifique supuestos mediante diagnóstico gráfico (residuos vs ajustados; Q-Q de residuos).\n")
      if (isTRUE(input$s3_ex_use_weights)) {
        cat("- Este ajuste utilizó Weighted Least Squares (WLS) con pesos 1/ŷ^2\n")
        cat("  como demostración de manejo de heterocedasticidad (varianza que crece con la media).\n")
      } else {
        cat("- Este ajuste utilizó Mínimos Cuadrados Ordinarios (OLS) sin ponderación.\n")
      }

      cat("\nSugerencia para conclusión agronómica:\n")
      cat("- Describa el sentido de los coeficientes (signo y magnitud),\n")
      cat("  comente la forma de la curva (lineal, óptimo, rendimientos decrecientes),\n")
      cat("  e indique si la evidencia estadística respalda una respuesta del cultivo\n")
      cat("  al rango de insumo ensayado.\n")
      sink()
    }
  )
}

# Pestaña 5: Referencias
pestanna5_session3_v3_server <- function(input, output, session) {
  # No server logic needed
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session3_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  ## Simulate data function
  make_sim_data <- function(n, b0, b1, sigma, quad, b2) {
    set.seed(123)
    x <- sort(runif(n, min = 1, max = 200))
    mu <- b0 + b1 * x
    if (isTRUE(quad)) mu <- mu + b2 * x^2
    y <- mu + rnorm(n, 0, sigma)
    tibble::tibble(x = x, y = y)
  }

  ## Read CSV function
  read_csv_xy <- function(path, header = TRUE, sep = ",") {
    df <- utils::read.table(path, header = header, sep = sep, dec = ".", quote = "\"'",
                            comment.char = "", stringsAsFactors = FALSE)
    if (!all(c("x", "y") %in% names(df))) {
      stop("El CSV debe incluir columnas 'x' y 'y'.")
    }
    df <- df[, c("x", "y")]
    df$x <- suppressWarnings(as.numeric(df$x))
    df$y <- suppressWarnings(as.numeric(df$y))
    df <- stats::na.omit(df)
    tibble::as_tibble(df)
  }

  # Reactives
  ## Data reactiveVal
  df_rv <- reactiveVal({
    make_sim_data(
      n     = 100,
      b0    = 500,
      b1    = 2,
      sigma = 40,
      quad  = TRUE,
      b2    = -0.01
    )
  })

  ## Linear model reactive
  s3_model_lin <- reactive({
    df <- df_rv(); req(nrow(df) > 1)
    stats::lm(y ~ x, data = df)
  })

  # Call tab servers

  ## Pestaña1: MRLS & Datos
  pestanna1_session3_v3_server(input, output, session, df_rv, make_sim_data, read_csv_xy, s3_model_lin)

  ## Pestaña2: Diagnóstico
  pestanna2_session3_v3_server(input, output, session, df_rv, s3_model_lin)

  ## Pestaña3: No lineales & Selección
  pestanna3_session3_v3_server(input, output, session, df_rv)

  ## Pestaña4: Ejercicios prácticos
  pestanna4_session3_v3_server(input, output, session, df_rv)

  ## Pestaña5: Referencias
  pestanna5_session3_v3_server(input, output, session)
}
