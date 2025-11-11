# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA (V3)
# Dependencias: shiny, bslib, DT (ya cargadas en global.R)
# ------------------------------------------------------------

session1_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 1: Flujo de Trabajo Moderno con Inteligencia Artificial")
    ),

    navset_tab(

      # 0) Por qué usar herramientas IA en investigación
      nav_panel(
        title = "¿Por qué ahora?",
        layout_column_wrap(
          width = 1,
          card(
            card_header("El cuello de botella se movió a la síntesis"),
            p("La producción científica crece de forma sostenida; el reto ya no es hallar ‘algo’, ",
              "sino ", strong("curar, sintetizar y verificar"), " múltiples fuentes y métodos de forma eficiente."),
            p("Las herramientas IA (descubrimiento visual, RAG/NotebookLM, smart citations) aceleran el ciclo ",
              em("Descubrir → Curar → Consultar → Idear"), " con mejor trazabilidad.")
          ),
          card(
            card_header("Objetivo de aprendizaje"),
            tags$ul(
              tags$li("Comprender rápidamente cómo funciona un LLM y por qué necesita apoyo de fuentes."),
              tags$li("Aplicar un flujo moderno con ResearchRabbit (mapas de citas) y NotebookLM (RAG con citas)."),
              tags$li("Usar un catálogo curado de herramientas IA para investigación y un generador de búsquedas.")
            )
          )
        )
      ),

      # 1) LLM en 5 minutos
      nav_panel(
        title = "LLM en 5 minutos",
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
            card_header("Cómo lo compensamos"),
            tags$ul(
              tags$li(strong("RAG / NotebookLM:"), " respuestas fundamentadas en tus PDFs/notas con citas."),
              tags$li(strong("Smart citations (scite):"), " lee si un paper ", em("apoya/contradice"), " a otro."),
              tags$li(strong("Flujo 4 pasos:"), " Descubrir → Curar → Consultar (citado) → Idear.")
            ),
            div(class = "alert alert-warning",
                tags$strong("Regla de oro:"),
                " ideación libre con LLM ≠ evidencia. Para afirmaciones fácticas: cita o no lo digas."
            )
          )
        )
      ),

      # 2) LLM vs RAG (NotebookLM)
      nav_panel(
        title = "LLM vs RAG (NotebookLM)",
        layout_column_wrap(
          width = 1,
          card(
            card_header("Conceptos"),
            p(
              strong("LLM general:"), " excelente para idear/redactar, pero puede alucinar.",
              " ", strong("RAG/NotebookLM:"), " conecta el modelo a tus fuentes curadas y entrega ",
              strong("citas in-line"), " y trazabilidad."
            )
          ),
          card(
            card_header("Tabla comparativa"),
            div(class = "table-responsive",
                tags$table(class = "table table-sm",
                  tags$thead(
                    tags$tr(
                      tags$th("Característica"),
                      tags$th("LLM general"),
                      tags$th("Sistema fundamentado (RAG/NotebookLM)")
                    )
                  ),
                  tags$tbody(
                    tags$tr(tags$td("Fuente de conocimiento"),
                            tags$td("Corpus estático de entrenamiento"),
                            tags$td("Tus PDFs/notas curadas (actualizable al instante)")),
                    tags$tr(tags$td("Riesgo de alucinación"),
                            tags$td("Más alto"),
                            tags$td("Bajo: responde desde tus fuentes + citas")),
                    tags$tr(tags$td("Caso de uso"),
                            tags$td("Ideación, borradores"),
                            tags$td("Extracción fáctica, síntesis trazable"))
                  )
                )
            ),
            div(class = "alert alert-info",
                tags$strong("NotebookLM:"),
                " carga PDFs/notas, genera capítulos, ‘audio overview’ y respuestas con ", strong("citas de tus fuentes"), "."
            )
          )
        )
      ),

      # 3) ResearchRabbit y NotebookLM (paso a paso)
      nav_panel(
        title = "Guías: ResearchRabbit & NotebookLM",
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
            p(a(href = "https://www.researchrabbitapp.com/", target = "_blank", "Abrir ResearchRabbit →"))
          ),
          card(
            card_header("NotebookLM (RAG con tus fuentes)"),
            tags$ol(
              tags$li("Crea un cuaderno y carga 15–30 PDFs/notas clave (revisiones, métodos, ensayos)."),
              tags$li("Haz preguntas de extracción y síntesis (citas in-line)."),
              tags$li("Genera ‘audio overview’ y capítulos para comunicar a no-técnicos."),
              tags$li("Repite: depura la biblioteca y documenta decisiones de inclusión/exclusión.")
            ),
            p(a(href = "https://support.google.com/a/users/answer/16013403", target = "_blank",
                "Aprender sobre NotebookLM →"))
          )
        )
      ),

      # 4) Flujo de 4 pasos (con generador de búsquedas)
      nav_panel(
        title = "Flujo 4 pasos",
        navset_tab(id = ns("steps_tabs"),

          nav_panel(
            title = "1) Descubrir",
            card(
              card_header("Estrategias"),
              p("Combina palabra clave + mapas de citas + bases con ranking semántico para cubrir lo seminal y lo reciente.")
            ),
            card(
              card_header("Generador de búsqueda booleana"),
              layout_column_wrap(
                width = 1/2,
                textInput(ns("kw_core"), "Término núcleo (obligatorio)", placeholder = "blueberry OR Vaccinium corymbosum"),
                textInput(ns("kw_syn"), "Sinónimos (OR separados por ; )", placeholder = "yield; productivity; 'fruit set'"),
                textInput(ns("kw_method"), "Métodos/condiciones (AND)", placeholder = "NDVI OR hyperspectral"),
                textInput(ns("kw_filter"), "Filtros (site:, filetype:, year:)", placeholder = "site:gov OR site:nih.gov"),
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
      ),

      # 5) Práctica guiada: Constructor de prompts
      nav_panel(
        title = "Práctica guiada",
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Constructor de prompts"),
            textInput(ns("pr_rol"), "Rol", placeholder = "p.ej., Fitopatólogo + estadístico"),
            textAreaInput(ns("pr_contexto"), "Contexto (desde NotebookLM)", placeholder = "Síntesis con citas...", rows = 6),
            textAreaInput(ns("pr_tarea"), "Tarea", placeholder = "3 hipótesis; diseño DBCA; plan de análisis en R...", rows = 5),
            textInput(ns("pr_formato"), "Formato", placeholder = "Lista numerada; tabla; código R comentado..."),
            textInput(ns("pr_restricciones"), "Restricciones (opcional)", placeholder = "No inventar refs; α=0.05; etc."),
            hr(),
            actionButton(ns("pr_reset"), "Limpiar"),
            downloadButton(ns("pr_descargar"), "Descargar prompt (.txt)")
          ),
          card(
            card_header("Vista previa"),
            textAreaInput(ns("pr_preview"), NULL, value = "", rows = 20)
          )
        )
      ),

      # 6) Catálogo de herramientas IA (interactivo)
      nav_panel(
        title = "Catálogo de herramientas",
        layout_column_wrap(
          width = 1,
          card(
            card_header("Explora y filtra"),
            DT::dataTableOutput(ns("tbl_tools")),
            div(class = "mt-2",
                downloadButton(ns("tools_download"), "Descargar catálogo (.csv)")
            )
          )
        )
      ),

      # 7) Prompts efectivos (banco)
      nav_panel(
        title = "Prompts efectivos",
        layout_column_wrap(
          width = 1,
          card(
            card_header("Ideación disciplinar"),
            tags$pre(style="white-space:pre-wrap;",
"Actúa como un agrónomo de suelos y un especialista en teledetección.
Contexto: (pega síntesis con citas desde NotebookLM).
Tarea: Propón 5 hipótesis comprobables que vinculen reflectancia de dosel con actividad microbiana en rizosfera.
Formato: Lista numerada; cada hipótesis con métrica, método y control.")
          ),
          card(
            card_header("Crítica metodológica"),
            tags$pre(style="white-space:pre-wrap;",
"Actúa como revisor de Plant and Soil, estricto en diseño y análisis.
Contexto: (resúmenes metodológicos citados)
Tarea: Señala debilidades en muestreo, sesgos y análisis; sugiere mejoras concretas.
Formato: Tabla: Aspecto | Hallazgo | Riesgo | Recomendación.")
          ),
          card(
            card_header("Extracción/tablas comparativas"),
            tags$pre(style="white-space:pre-wrap;",
"Rol: Asistente de revisión sistemática.
Contexto: (artículos sobre Trichoderma en raíces)
Tarea: Tabla con Especie/cepa | Dosis | Cultivo | Condición | Métrica | Efecto | Fuente (cita).
Formato: Tabla Markdown. Sin inventar datos.")
          ),
          card(
            card_header("Hipótesis + diseño + análisis en R"),
            tags$pre(style="white-space:pre-wrap;",
"Rol: Fitopatólogo + estadístico.
Contexto: (lagunas fundamentadas)
Tarea: 3 hipótesis; DBCA (4 tratamientos × 5 reps); código R para ANOVA + supuestos + post-hoc.
Formato: Lista + bloque de código R comentado.")
          )
        )
      ),

      # 8) Referencias
      nav_panel(
        title = "Referencias",
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

    ) # /navset_tab
  )
}

session1_v3Server <- function(input, output, session) {

  ns <- session$ns
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  # ---- Generador de búsqueda booleana
  observeEvent(input$kw_build, {
    core   <- trimws(input$kw_core %||% "")
    syn    <- trimws(input$kw_syn %||% "")
    meth   <- trimws(input$kw_method %||% "")
    filt   <- trimws(input$kw_filter %||% "")

    syn_vec  <- unlist(strsplit(syn, ";"))
    syn_part <- if (length(syn_vec) && any(nzchar(syn_vec))) {
      paste0("(", paste(trimws(syn_vec[nzchar(syn_vec)]), collapse = " OR "), ")")
    } else ""

    parts <- c(core, syn_part, meth)
    parts <- parts[nzchar(parts)]
    q     <- paste(parts, collapse = " AND ")
    if (nzchar(filt)) q <- paste(q, filt)

    updateTextAreaInput(session, "kw_preview", value = q)
  })

  output$kw_download <- downloadHandler(
    filename = function() paste0("busqueda_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"),
    content  = function(file) writeLines(input$kw_preview %||% "", con = file, useBytes = TRUE)
  )

  # ---- Constructor de prompts
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
    list(input$pr_rol, input$pr_contexto, input$pr_tarea, input$pr_formato, input$pr_restricciones),
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
    filename = function() paste0("prompt_sesion1_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt"),
    content  = function(file) writeLines(prompt_r(), con = file, useBytes = TRUE)
  )

  # ---- Catálogo de herramientas (tabla interactiva)
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
    filename = function() paste0("catalogo_herramientas_IA_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content  = function(file) utils::write.csv(tools_df(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}
