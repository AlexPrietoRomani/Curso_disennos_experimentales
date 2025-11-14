# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA (V3)
# Reorganizado en funciones UI por pestaña
# ------------------------------------------------------------

# ---------- UI helpers por pestaña (reciben ns) -----------------

pestanna1_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña1: Qué es la IA",

    # --- Fila 1: definición + mapa de círculos ---
    layout_column_wrap(
      width = 1/2,

      # Definición rápida, amigable
      card(
        card_header("Definición operativa de Inteligencia Artificial"),
        p(
          "En este curso seguiremos la definición de Russell y Norvig: ",
          "la Inteligencia Artificial (IA) es el estudio de ",
          strong("agentes que perciben su entorno y actúan sobre él"),
          " para alcanzar objetivos."
        ),
        p(
          "Dicho de forma simple: diseñamos programas que toman decisiones ",
          "de manera (más o menos) autónoma a partir de datos, sensores o texto."
        ),
        tags$ul(
          tags$li(
            strong("Agente:"),
            " algo que percibe el entorno y puede actuar (software, robot, servicio en la nube)."
          ),
          tags$li(
            strong("Entorno:"),
            " datos de sensores, imágenes, texto, registros agrícolas, etc."
          ),
          tags$li(
            strong("Objetivo:"),
            " una medida de desempeño: reducir error, maximizar rendimiento, detectar riesgos, etc."
          )
        )
      ),

      # Mapa interactivo: círculos
      card(
        card_header("Mapa interactivo: ¿dónde se ubican los LLM dentro de la IA?"),

        # Contenedor de círculos
        div(
          class = "ia-map-wrapper",

          # Círculo grande: IA general (clicable)
          div(
            class = "ia-circle-main",
            actionButton(
              ns("ia_btn_general"),
              label = "Inteligencia\nArtificial",
              class  = "btn-light ia-circle-btn-main"
            ),

            # Círculos internos: subcampos
            div(
              class = "ia-circle-sub ia-circle-simbolica",
              actionButton(
                ns("ia_btn_simb"),
                label = "IA simbólica",
                class  = "btn-outline-primary ia-circle-btn"
              )
            ),
            div(
              class = "ia-circle-sub ia-circle-ml",
              actionButton(
                ns("ia_btn_ml"),
                label = "Aprendizaje\nautomático",
                class  = "btn-outline-primary ia-circle-btn"
              )
            ),
            div(
              class = "ia-circle-sub ia-circle-robotica",
              actionButton(
                ns("ia_btn_robotica"),
                label = "Robótica\ny agentes",
                class  = "btn-outline-primary ia-circle-btn"
              )
            ),
            div(
              class = "ia-circle-sub ia-circle-llm",
              actionButton(
                ns("ia_btn_llm"),
                label = "LLM /\nmodelos\nfundacionales",
                class  = "btn-primary ia-circle-btn"
              )
            )
          )
        ),

        hr(),
        # Aquí pintamos la ficha según el círculo seleccionado
        uiOutput(ns("ia_map_detail"))
      )
    ),

    # --- Fila 2: Historia + ejemplos con tabs tipo “botón” ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Historia y ejemplos de IA (interactivo)"),
        navset_pill(   # pills quedan como botones
          id = ns("ia_hist_tabs"),

          nav_panel(
            "Línea histórica",
            tags$ul(
              class = "ia-timeline",
              tags$li(
                strong("1950 – Turing: "),
                "“Computing Machinery and Intelligence” y el juego de la imitación."
              ),
              tags$li(
                strong("1956 – Dartmouth: "),
                "se acuña el término ",
                em("Artificial Intelligence"),
                " y nace el campo como disciplina."
              ),
              tags$li(
                strong("1960–1970: "),
                "IA simbólica y sistemas expertos para diagnóstico y planificación."
              ),
              tags$li(
                strong("1980s – inviernos de la IA: "),
                "expectativas infladas, límites computacionales y caída de financiamiento."
              ),
              tags$li(
                strong("1990–2000: "),
                "revolución del ",
                em("machine learning"),
                " estadístico (SVM, árboles, redes neuronales)."
              ),
              tags$li(
                strong("2010–2016: "),
                em("deep learning"),
                " en visión, voz y texto gracias a GPUs y grandes datasets."
              ),
              tags$li(
                strong("2017 – Transformer: "),
                "se introduce la arquitectura ",
                em("Transformer"),
                " que da origen a los LLM modernos."
              ),
              tags$li(
                strong("2018–hoy – LLM y modelos fundacionales: "),
                "modelos como BERT, GPT-3/4 se adaptan a muchas tareas mediante ",
                em("fine-tuning"),
                " o ",
                em("prompting"),
                "."
              )
            )
          ),

          nav_panel(
            "Ejemplos en ciencia y agro",
            layout_column_wrap(
              width = 1/2,
              card(
                card_header("IA simbólica"),
                tags$ul(
                  tags$li("Sistemas expertos para diagnóstico de plagas según reglas IF–THEN."),
                  tags$li("Motores de reglas para decidir labores según umbrales climáticos.")
                )
              ),
              card(
                card_header("Aprendizaje automático clásico"),
                tags$ul(
                  tags$li("Modelos de regresión / random forest para predecir rendimiento (kg/ha)."),
                  tags$li("Clasificación de lotes según riesgo fitosanitario a partir de clima + manejo.")
                )
              ),
              card(
                card_header("Deep learning / visión"),
                tags$ul(
                  tags$li("Redes convolucionales para conteo de plantas o frutos en imágenes de dron."),
                  tags$li("Segmentación de canopia para estimar cobertura y vigor.")
                )
              ),
              card(
                card_header("NLP y LLM"),
                tags$ul(
                  tags$li("Modelos tipo GPT que resumen artículos, generan código en R/Python."),
                  tags$li("Asistentes que proponen hipótesis y diseños experimentales a partir de tu biblioteca.")
                )
              )
            )
          )
        )
      )
    )
  )
}

pestanna2_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña2: Qué es un LLM",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("¿Qué hace un LLM?"),
        tags$ol(
          tags$li("Convierte tu texto a ", strong("tokens"), " y vectores (embeddings)."),
          tags$li("Usa un ", strong("Transformer"), " (auto-atención) para ponderar contexto relevante."),
          tags$li("Predice el ", strong("siguiente token"), " miles de veces hasta completar la respuesta."),
          tags$li("Limitaciones: conocimiento ‘congelado’, ", em("alucinaciones"), " y falta de citas nativas.")
        ),
        tags$pre(style = "white-space:pre-wrap;",
"Texto → tokens → embeddings → [Bloques Transformer con atención] → probabilidad del siguiente token → texto")
      ),
      card(
        card_header("Qué hace bien y qué no"),
        tags$ul(
          tags$li(strong("Muy bueno:"), " idear, reescribir, resumir, proponer estructuras de código o análisis."),
          tags$li(strong("Riesgoso solo:"), " dar números, parámetros o citas específicas sin comprobar."),
          tags$li(strong("Necesita apoyo de:"), " bases de papers, PDFs propios, flujos de verificación.")
        ),
        div(
          class = "alert alert-warning",
          tags$strong("Regla de oro:"),
          " ideación libre con LLM ≠ evidencia. Para afirmaciones fácticas: cita o no lo digas."
        )
      )
    )
  )
}

pestanna3_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña3: Qué es la alucinación de un LLM",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Alucinación: definición operativa"),
        p("Un LLM ‘alucina’ cuando entrega una respuesta plausible en estilo, ",
          "pero ", strong("falsa, inventada o sin soporte verificable"), "."),
        tags$ul(
          tags$li("Referencias inexistentes o mezcladas (autor + año falsos)."),
          tags$li("Resultados numéricos que no aparecen en ningún paper."),
          tags$li("Métodos o diseños experimentales que suenan correctos pero no están descritos en las fuentes.")
        )
      ),
      card(
        card_header("Cómo reducir alucinaciones en investigación"),
        tags$ol(
          tags$li(strong("Siempre preguntar por la fuente"), " (paper, DOI, página)."),
          tags$li(strong("Trabajar con RAG/NotebookLM"), " y limitar al modelo a tus PDFs/notas curadas."),
          tags$li("Cruzar respuestas con motores académicos (Semantic Scholar, OpenAlex, etc.)."),
          tags$li("Documentar qué vino del modelo y qué fue verificado manualmente.")
        ),
        div(
          class = "alert alert-info",
          "Un LLM siempre ‘tiene que responder’. Tu trabajo es decidir ",
          strong("cuándo creerle"),
          " según las citas y la triangulación con otras fuentes."
        )
      )
    )
  )
}

pestanna4_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña4: Cómo usar aplicaciones de RAG (NotebookLM)",
    layout_column_wrap(
      width = 1,
      card(
        card_header("LLM general vs RAG/NotebookLM"),
        p(
          strong("LLM general:"), " excelente para idear/redactar, pero puede alucinar.",
          " ", strong("RAG/NotebookLM:"), " conecta el modelo a tus fuentes curadas y entrega ",
          strong("citas in-line"), " y trazabilidad."
        )
      ),
      card(
        card_header("NotebookLM en la práctica"),
        tags$ol(
          tags$li("Crea un cuaderno y carga 15–30 PDFs/notas clave (revisiones, métodos, ensayos)."),
          tags$li("Haz preguntas de extracción y síntesis (con citas in-line)."),
          tags$li("Genera ‘audio overview’ y capítulos para comunicar a no-técnicos."),
          tags$li("Repite: depura la biblioteca y documenta decisiones de inclusión/exclusión.")
        ),
        p(
          a(
            href = "https://support.google.com/a/users/answer/16013403",
            target = "_blank",
            "Aprender sobre NotebookLM →"
          )
        )
      )
    )
  )
}

pestanna5_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña5: Guías: ResearchRabbit & NotebookLM",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("ResearchRabbit (mapa de literatura)"),
        tags$ol(
          tags$li("Crea una colección con 5–10 artículos ‘semilla’."),
          tags$li("Explora redes: ‘anteriores’, ‘posteriores’, ‘similares’."),
          tags$li("Itera: agrega/quita para refinar el campo y evitar ruido."),
          tags$li("Exporta la colección (BibTeX/ris) para tu gestor o para NotebookLM.")
        ),
        p(a(
          href = "https://www.researchrabbitapp.com/",
          target = "_blank",
          "Abrir ResearchRabbit →"
        ))
      ),
      card(
        card_header("NotebookLM (RAG con tus fuentes)"),
        tags$ol(
          tags$li("Carga PDFs/notas clave (revisiones, métodos, ensayos representativos)."),
          tags$li("Haz preguntas específicas sobre diseño, métodos, resultados."),
          tags$li("Usa las citas para rastrear cada afirmación a un paper concreto."),
          tags$li("Construye resúmenes que luego usarás como ‘contexto’ en tus prompts.")
        )
      )
    )
  )
}

pestanna6_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña6: Flujo 4 pasos para correctas búsquedas con IA",
    navset_tab(
      id = ns("steps_tabs"),

      nav_panel(
        title = "1) Descubrir",
        card(
          card_header("Estrategias"),
          p(
            "Combina palabra clave + mapas de citas + bases con ranking semántico para cubrir ",
            "lo seminal y lo reciente."
          )
        ),
        card(
          card_header("Generador de búsqueda booleana"),
          layout_column_wrap(
            width = 1/2,
            textInput(
              ns("kw_core"),
              "Término núcleo (obligatorio)",
              placeholder = "blueberry OR Vaccinium corymbosum"
            ),
            textInput(
              ns("kw_syn"),
              "Sinónimos (OR separados por ; )",
              placeholder = "yield; productivity; 'fruit set'"
            ),
            textInput(
              ns("kw_method"),
              "Métodos/condiciones (AND)",
              placeholder = "NDVI OR hyperspectral"
            ),
            textInput(
              ns("kw_filter"),
              "Filtros (site:, filetype:, year:)",
              placeholder = "site:gov OR site:nih.gov"
            ),
            textAreaInput(ns("kw_preview"), "Vista previa", rows = 4)
          ),
          layout_column_wrap(
            width = 1/3,
            actionButton(ns("kw_build"), "Construir cadena"),
            downloadButton(ns("kw_download"), "Descargar (.txt)")
          )
        )
      ),

      nav_panel(
        title = "2) Curar",
        card(
          card_header("Colección deliberada (GIGO)"),
          tags$ul(
            tags$li("Define criterios de inclusión/exclusión y documenta por qué."),
            tags$li("Prioriza revisiones/meta-análisis y métodos bien citados."),
            tags$li("Evita mezclar temas heterogéneos en un mismo cuaderno.")
          )
        )
      ),

      nav_panel(
        title = "3) Consultar",
        card(
          card_header("Preguntas típicas para RAG/NotebookLM"),
          tags$ol(
            tags$li("“Con base en estas fuentes, resume metodologías para X y limita por especie/dosis.”"),
            tags$li("“Tabla comparativa Autor A/B/C (año) con variable, diseño y efecto.”"),
            tags$li("“Extrae métricas de rendimiento (kg/ha, °Brix, firmeza) bajo condición Y.”"),
            tags$li("“¿Qué vacíos de investigación declaran en Discusión?”")
          )
        )
      ),

      nav_panel(
        title = "4) Idear",
        card(
          card_header("Prompting (rol, contexto, formato, restricciones)"),
          p("Parte de tu síntesis citada; luego pide hipótesis, diseño y plan de análisis reproducible.")
        )
      )
    )
  )
}

pestanna7_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña7: Guías: ResearchRabbit, NotebookLM & Rayyan.ai",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Catálogo de herramientas IA para literatura"),
        p("Filtra por categoría para ver herramientas de descubrimiento, consulta con citas, ",
          "gestión bibliográfica y screening sistemático (incluye Rayyan)."),
        DT::dataTableOutput(ns("tbl_tools")),
        div(
          class = "mt-2",
          downloadButton(ns("tools_download"), "Descargar catálogo (.csv)")
        )
      )
    )
  )
}

pestanna8_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña8: El prompt perfecto",
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Constructor de prompts"),
        textInput(ns("pr_rol"), "Rol", placeholder = "p.ej., Fitopatólogo + estadístico"),
        textAreaInput(
          ns("pr_contexto"),
          "Contexto (desde NotebookLM)",
          placeholder = "Síntesis con citas...",
          rows = 6
        ),
        textAreaInput(
          ns("pr_tarea"),
          "Tarea",
          placeholder = "3 hipótesis; diseño DBCA; plan de análisis en R...",
          rows = 5
        ),
        textInput(ns("pr_formato"), "Formato", placeholder = "Lista numerada; tabla; código R comentado..."),
        textInput(
          ns("pr_restricciones"),
          "Restricciones (opcional)",
          placeholder = "No inventar refs; α=0.05; etc."
        ),
        hr(),
        actionButton(ns("pr_reset"), "Limpiar"),
        downloadButton(ns("pr_descargar"), "Descargar prompt (.txt)")
      ),
      card(
        card_header("Vista previa de prompt"),
        textAreaInput(ns("pr_preview"), NULL, value = "", rows = 12)
      ),
      card(
        card_header("Banco de prompts efectivos"),
        layout_column_wrap(
          width = 1,
          card(
            card_header("Ideación disciplinar"),
            tags$pre(style = "white-space:pre-wrap;",
"Actúa como un agrónomo de suelos y un especialista en teledetección.
Contexto: (pega síntesis con citas desde NotebookLM).
Tarea: Propón 5 hipótesis comprobables que vinculen reflectancia de dosel con actividad microbiana en rizosfera.
Formato: Lista numerada; cada hipótesis con métrica, método y control.")
          ),
          card(
            card_header("Crítica metodológica"),
            tags$pre(style = "white-space:pre-wrap;",
"Actúa como revisor de Plant and Soil, estricto en diseño y análisis.
Contexto: (resúmenes metodológicos citados)
Tarea: Señala debilidades en muestreo, sesgos y análisis; sugiere mejoras concretas.
Formato: Tabla: Aspecto | Hallazgo | Riesgo | Recomendación.")
          ),
          card(
            card_header("Extracción/tablas comparativas"),
            tags$pre(style = "white-space:pre-wrap;",
"Rol: Asistente de revisión sistemática.
Contexto: (artículos sobre Trichoderma en raíces)
Tarea: Tabla con Especie/cepa | Dosis | Cultivo | Condición | Métrica | Efecto | Fuente (cita).
Formato: Tabla Markdown. Sin inventar datos.")
          ),
          card(
            card_header("Hipótesis + diseño + análisis en R"),
            tags$pre(style = "white-space:pre-wrap;",
"Rol: Fitopatólogo + estadístico.
Contexto: (lagunas fundamentadas)
Tarea: 3 hipótesis; DBCA (4 tratamientos × 5 reps); código R para ANOVA + supuestos + post-hoc.
Formato: Lista + bloque de código R comentado.")
          )
        )
      )
    )
  )
}

pestanna9_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña9: Referencias bibliográficas",
    layout_column_wrap(
      width = 1,
      card(
        card_header("Bibliografía y recursos"),
        tags$ul(
          tags$li("Bornmann & Mutz (2015). Growth rates of modern science. JASIST. https://doi.org/10.1002/asi.23329"),
          tags$li("Vaswani et al. (2017). Attention Is All You Need. NeurIPS."),
          tags$li("Google — NotebookLM: https://support.google.com/a/users/answer/16013403"),
          tags$li("scite — Smart Citations: https://scite.ai/"),
          tags$li("Semantic Scholar: https://www.semanticscholar.org/"),
          tags$li("OpenAlex: https://openalex.org/"),
          tags$li("ResearchRabbit: https://www.researchrabbitapp.com/"),
          tags$li("Litmaps: https://www.litmaps.com/"),
          tags$li("Connected Papers: https://www.connectedpapers.com/"),
          tags$li("Consensus: https://consensus.app/"),
          tags$li("Rayyan: https://www.rayyan.ai/"),
          tags$li("Zotero: https://www.zotero.org/")
        )
      )
    )
  )
}

# ---------- UI principal: ensambla las pestañas -------------------

session1_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 1: Flujo de Trabajo Moderno con Inteligencia Artificial")
    ),
    navset_tab(
      pestanna1_session1_v3UI(ns),
      pestanna2_session1_v3UI(ns),
      pestanna3_session1_v3UI(ns),
      pestanna4_session1_v3UI(ns),
      pestanna5_session1_v3UI(ns),
      pestanna6_session1_v3UI(ns),
      pestanna7_session1_v3UI(ns),
      pestanna8_session1_v3UI(ns),
      pestanna9_session1_v3UI(ns)
    )
  )
}

# ============================================
# SERVER: Sesión 1 — Flujo de Trabajo con IA
# ============================================

# ---- Helpers de server por pestaña -------------------------

# Pestaña 1: Que es la IA
pestanna1_session1_v3_server <- function(input, output, session) {

  # Qué círculo está seleccionado
  selected_domain <- reactiveVal("ia_general")

  observeEvent(input$ia_btn_general,  { selected_domain("ia_general")  })
  observeEvent(input$ia_btn_simb,     { selected_domain("ia_simb")     })
  observeEvent(input$ia_btn_ml,       { selected_domain("ia_ml")       })
  observeEvent(input$ia_btn_robotica, { selected_domain("ia_robotica") })
  observeEvent(input$ia_btn_llm,      { selected_domain("ia_llm")      })

  output$ia_map_detail <- renderUI({
    dom <- selected_domain()

    switch(
      dom,

      # --- Vista general IA ---
      ia_general = tagList(
        h4("Inteligencia Artificial (vista general)"),
        p(
          "La IA engloba técnicas muy distintas: desde reglas simbólicas escritas por personas ",
          "hasta modelos de aprendizaje profundo entrenados con millones de ejemplos."
        ),
        tags$ul(
          tags$li(strong("Entrada:"), " datos (sensores, imágenes, texto, registros)."),
          tags$li(strong("Salida:"),  " decisiones, predicciones, recomendaciones."),
          tags$li(
            strong("Rol del humano:"),
            " definir objetivos, supervisar el sistema y validar resultados."
          )
        )
      ),

      # --- IA simbólica / reglas ---
      ia_simb = tagList(
        h4("IA simbólica / basada en reglas"),
        p(
          "Trabaja con símbolos y reglas explícitas del tipo IF–THEN. ",
          "Es transparente y fácil de auditar, pero cuesta escalarla a problemas complejos."
        ),
        tags$ul(
          tags$li("Sistemas expertos para diagnóstico de plagas o enfermedades."),
          tags$li("Motores de reglas para recomendar fertilización o riego según umbrales.")
        )
      ),

      # --- Machine Learning ---
      ia_ml = tagList(
        h4("Aprendizaje automático (Machine Learning)"),
        p(
          "En vez de escribir reglas a mano, dejamos que el modelo las ‘aprenda’ a partir de datos etiquetados ",
          "o de patrones encontrados en los datos."
        ),
        tags$ul(
          tags$li("Regresión, árboles, random forest, SVM, redes neuronales."),
          tags$li("Predicción de rendimiento, clasificación de lotes, detección de anomalías."),
          tags$li("Dentro de ML vive el ", strong("deep learning"), " que usamos para visión y texto.")
        )
      ),

      # --- Robótica y agentes físicos ---
      ia_robotica = tagList(
        h4("Robótica y agentes físicos"),
        p(
          "Combina percepción (sensores, cámaras), planificación y control para actuar en el mundo físico."
        ),
        tags$ul(
          tags$li("Robots móviles para labores agrícolas o monitoreo de invernaderos."),
          tags$li("Vehículos autónomos, drones, brazos robóticos de cosecha."),
          tags$li("Suelen integrar visión por computador, control en tiempo real y a veces aprendizaje por refuerzo.")
        )
      ),

      # --- LLM / modelos fundacionales ---
      ia_llm = tagList(
        h4("Modelos de lenguaje grandes (LLM)"),
        p(
          "Son modelos de deep learning entrenados sobre enormes corpus de texto para predecir el siguiente token. ",
          "Al usar arquitecturas tipo ",
          em("Transformer"),
          ", capturan patrones complejos en el lenguaje."
        ),
        tags$ul(
          tags$li("Se ubican dentro de ", strong("NLP"), " → deep learning → machine learning → IA."),
          tags$li("Pueden resumir, traducir, responder preguntas, generar código y más."),
          tags$li("En investigación, son potentes para idear y redactar, pero necesitan apoyo de fuentes (RAG, NotebookLM) para ser trazables.")
        )
      )
    )
  })
}

# Pestaña 6: Flujo 4 pasos — subpestaña "Descubrir"
pestanna6_descubrir_server <- function(input, output, session) {
  ns <- session$ns
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  observeEvent(input$kw_build, {
    core   <- trimws(input$kw_core %||% "")
    syn    <- trimws(input$kw_syn %||% "")
    meth   <- trimws(input$kw_method %||% "")
    filt   <- trimws(input$kw_filter %||% "")

    syn_vec  <- unlist(strsplit(syn, ";"))
    syn_part <- if (length(syn_vec) && any(nzchar(syn_vec))) {
      paste0(
        "(",
        paste(trimws(syn_vec[nzchar(syn_vec)]), collapse = " OR "),
        ")"
      )
    } else ""

    parts <- c(core, syn_part, meth)
    parts <- parts[nzchar(parts)]
    q     <- paste(parts, collapse = " AND ")
    if (nzchar(filt)) q <- paste(q, filt)

    updateTextAreaInput(session, "kw_preview", value = q)
  })

  output$kw_download <- downloadHandler(
    filename = function() paste0(
      "busqueda_",
      format(Sys.time(), "%Y%m%d_%H%M"),
      ".txt"
    ),
    content  = function(file) {
      writeLines(input$kw_preview %||% "", con = file, useBytes = TRUE)
    }
  )
}

# Pestaña 8: El prompt perfecto
pestanna8_prompt_server <- function(input, output, session) {
  ns <- session$ns
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  prompt_r <- reactive({
    rol   <- input$pr_rol           %||% ""
    ctx   <- input$pr_contexto      %||% ""
    tarea <- input$pr_tarea         %||% ""
    forma <- input$pr_formato       %||% ""
    restr <- input$pr_restricciones %||% ""

    parts <- c(
      if (shiny::isTruthy(rol))   paste0("Actúa como: ", rol, ".") else NULL,
      if (shiny::isTruthy(ctx))   paste0("Contexto (citado):\n", ctx) else NULL,
      if (shiny::isTruthy(tarea)) paste0("Tu tarea:\n", tarea) else NULL,
      if (shiny::isTruthy(forma)) paste0("Formato esperado:\n", forma) else NULL,
      if (shiny::isTruthy(restr)) paste0("Restricciones:\n", restr) else NULL
    )
    paste(parts, collapse = "\n\n")
  })

  observeEvent(
    list(
      input$pr_rol,
      input$pr_contexto,
      input$pr_tarea,
      input$pr_formato,
      input$pr_restricciones
    ),
    ignoreInit = TRUE,
    handlerExpr = {
      updateTextAreaInput(session, "pr_preview", value = prompt_r())
    }
  )

  observeEvent(input$pr_reset, {
    updateTextInput(session, "pr_rol", value = "")
    updateTextAreaInput(session, "pr_contexto", value = "")
    updateTextAreaInput(session, "pr_tarea", value = "")
    updateTextInput(session, "pr_formato", value = "")
    updateTextInput(session, "pr_restricciones", value = "")
    updateTextAreaInput(session, "pr_preview", value = "")
  })

  output$pr_descargar <- downloadHandler(
    filename = function() paste0(
      "prompt_sesion1_",
      format(Sys.time(), "%Y%m%d_%H%M"),
      ".txt"
    ),
    content  = function(file) {
      writeLines(prompt_r(), con = file, useBytes = TRUE)
    }
  )
}

# Pestaña 7: Catálogo de herramientas IA
pestanna7_tools_server <- function(input, output, session) {

  tools_df <- reactive({
    data.frame(
      Categoria = c(
        "Descubrir","Descubrir","Descubrir","Descubrir","Descubrir","Descubrir",
        "Consultar con citas","Consultar con citas","Consultar con citas",
        "Gestión bibliográfica","Gestión bibliográfica","Screening"
      ),
      Herramienta = c(
        "ResearchRabbit","Litmaps","Connected Papers","Semantic Scholar","OpenAlex","Lens.org",
        "NotebookLM","scite (smart citations)","Consensus",
        "Zotero","Paperpile","Rayyan"
      ),
      Descripcion = c(
        "Mapas de literatura por redes (anteriores/posteriores/similares)",
        "Mapas + seguimiento de alertas y expansión por citas",
        "Grafo de trabajos relacionados por similitud",
        "Búsqueda semántica y sugerencias de papers relacionados",
        "Base abierta de metadatos (autores, venues, citas)",
        "Metabuscador con métricas de patentes y publicaciones",
        "RAG con tus PDFs/notas; capítulos, audio overview, citas",
        "Clasifica citas como apoya/contradice/menciona",
        "Respuestas a partir de papers con citas (claim-first)",
        "Gestor de referencias, captura web y anotación de PDFs",
        "Gestión en la nube integrada a Gmail/Google Docs",
        "Cribado/etiquetado rápido para revisiones sistemáticas"
      ),
      URL = c(
        "https://www.researchrabbitapp.com/",
        "https://www.litmaps.com/",
        "https://www.connectedpapers.com/",
        "https://www.semanticscholar.org/",
        "https://openalex.org/",
        "https://www.lens.org/",
        "https://support.google.com/a/users/answer/16013403",
        "https://scite.ai/",
        "https://consensus.app/",
        "https://www.zotero.org/",
        "https://paperpile.com/",
        "https://www.rayyan.ai/"
      ),
      stringsAsFactors = FALSE
    )
  })

  output$tbl_tools <- DT::renderDataTable({
    DT::datatable(
      tools_df(),
      rownames   = FALSE,
      filter     = "top",
      escape     = FALSE,
      options    = list(pageLength = 10, dom = "ftip", autoWidth = TRUE)
    )
  })

  output$tools_download <- downloadHandler(
    filename = function() paste0(
      "catalogo_herramientas_IA_",
      format(Sys.time(), "%Y%m%d_%H%M"),
      ".csv"
    ),
    content  = function(file) {
      utils::write.csv(
        tools_df(),
        file,
        row.names   = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
}

# ---- Server principal del módulo ------------------------------

session1_v3Server <- function(input, output, session) {

  # Pestaña1 (Qué es la IA)
  pestanna1_session1_v3_server(input, output, session)

  # Pestaña2 (Qué es un LLM)
  # Pestaña3 (Alucinación)
  # Pestaña4 (RAG / NotebookLM)
  # Pestaña5 (Guías ResearchRabbit & NotebookLM)
  # Pestaña9 (Referencias)

  # Pestaña6: Flujo 4 pasos (subpestaña Descubrir)
  pestanna6_descubrir_server(input, output, session)

  # Pestaña7: Catálogo herramientas
  pestanna7_tools_server(input, output, session)

  # Pestaña8: Prompt perfecto
  pestanna8_prompt_server(input, output, session)
}
