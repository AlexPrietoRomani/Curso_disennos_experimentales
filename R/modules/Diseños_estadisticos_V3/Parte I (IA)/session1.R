# session1.R
# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA
# Dependencias: shiny, bslib
# ------------------------------------------------------------

session1_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "session-title",
      h3("Sesión 1: Flujo de Trabajo Moderno con Inteligencia Artificial")
    ),

    bslib::navset_tab(

      # --------------------
      # 0) Por qué IA + Fundamentos LLM
      # --------------------
      bslib::nav_panel(
        title = "¿Por qué IA y cómo funciona un LLM?",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("¿Por qué adoptar asistentes de IA en investigación?"),
            tags$ul(
              tags$li(strong("Volumen:"), " más de 3 millones de artículos al año hacen inviable una revisión manual completa."),
              tags$li(strong("Velocidad:"), " los asistentes aceleran la detección de vacíos, comparativos y preguntas emergentes."),
              tags$li(strong("Rigor:"), " herramientas con citas (NotebookLM, Scite) ayudan a mantener trazabilidad y auditar respuestas."),
              tags$li(strong("Creatividad guiada:"), " modelos generativos permiten prototipar hipótesis, métodos y narrativas con control humano.")
            ),
            bs_callout(
              type = "info",
              title = "Principios clave",
              p(strong("Humano en el circuito"), ": valida, corrige y decide."),
              p(strong("Fuentes curadas"), ": la calidad de la colección determina la calidad de las respuestas."),
              p(strong("Documenta"), ": guarda prompts efectivos y trazabilidad para repetir análisis." )
            )
          ),
          bslib::card(
            bslib::card_header("LLMs en 3 minutos"),
            tags$ol(
              tags$li("Un LLM se entrena con billones de palabras para aprender patrones estadísticos del lenguaje."),
              tags$li("Predice la siguiente palabra dada una secuencia, lo que permite redactar, traducir o resumir."),
              tags$li("No ‘entiende’ hechos; responde según probabilidad. Por ello puede inventar (alucinar) si no tiene evidencia."),
              tags$li("Para respuestas fundamentadas se conecta a fuentes externas mediante ", strong("retrieval-augmented generation (RAG)"), "."),
              tags$li("La ingeniería de prompts define rol, contexto y formato para guiar la salida.")
            ),
            div(
              class = "alert alert-warning",
              tags$h5("Buenas prácticas inmediatas"),
              tags$ul(
                tags$li("Cita siempre el documento original antes de incorporar resultados."),
                tags$li("Contrasta hallazgos automatizados con el diseño experimental y métricas de calidad."),
                tags$li("Versiona tus prompts para replicar procesos." )
              )
            )
          )
        )
      ),

      # --------------------
      # 1) Contexto y objetivos
      # --------------------
      bslib::nav_panel(
        title = "Contexto y objetivos",
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Contexto"),
            p("La producción científica crece de manera sostenida, y el cuello de botella se ha desplazado hacia la ",
              em("síntesis eficiente"), " y la ", em("contextualización de literatura.")),
            p("Esta sesión propone un flujo de trabajo moderno que combina herramientas de IA para acelerar la ",
              "revisión, la extracción fáctica con citas y la ideación responsable.")
          ),
          bslib::card(
            bslib::card_header("Objetivo de aprendizaje"),
            tags$ul(
              tags$li("Aplicar un flujo estructurado con IA para descubrir, ",
                      "curar, consultar y ", strong("idear"), " a partir de literatura científica."),
              tags$li("Diferenciar LLMs de ideación vs. sistemas fundamentados (RAG/NotebookLM) para extracción de hechos."),
              tags$li("Practicar ingeniería de prompts: rol, contexto y formato."),
              tags$li("Implementar herramientas especializadas como ResearchRabbit y NotebookLM a lo largo del flujo.")
            )
          ),
          div(
            class = "alert alert-info",
            tags$h5("Resultado esperado"),
            p("Un flujo de 4 pasos (Descubrir → Curar → Consultar → Idear) y un conjunto de prompts listos para acelerar tareas de investigación.")
          )
        )
      ),

      # --------------------
      # 2) LLM vs RAG (NotebookLM)
      # --------------------
      bslib::nav_panel(
        title = "LLM vs RAG (NotebookLM)",
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Conceptos clave"),
            p(
              strong("LLMs generales"), " son excelentes para ", em("ideación y redacción"),
              ", pero pueden ", em("alucinar"), " y su conocimiento base es estático.",
              " Los ", strong("sistemas fundamentados (RAG/NotebookLM)"), " conectan un LLM a ",
              "tus fuentes curadas y responden con ", em("citas in-line"), " y trazabilidad."
            )
          ),
          bslib::card(
            bslib::card_header("Tabla comparativa"),
            div(
              class = "table-responsive",
              tags$table(
                class = "table table-sm",
                tags$thead(
                  tags$tr(
                    tags$th("Característica"),
                    tags$th("LLM general (p.ej., Gemini/ChatGPT)"),
                    tags$th("Sistema fundamentado (p.ej., NotebookLM/RAG)")
                  )
                ),
                tags$tbody(
                  tags$tr(
                    tags$td("Fuente de conocimiento"),
                    tags$td("Corpus estático de entrenamiento"),
                    tags$td("Colección curada y dinámica de tus PDFs/notas")
                  ),
                  tags$tr(
                    tags$td("Analogía"),
                    tags$td("Examen a libro cerrado"),
                    tags$td("Examen a libro abierto")
                  ),
                  tags$tr(
                    tags$td("Riesgo de alucinación"),
                    tags$td("Más alto (necesita verificación)"),
                    tags$td("Bajo: respuestas basadas en tus fuentes con citas")
                  ),
                  tags$tr(
                    tags$td("Actualización de datos"),
                    tags$td("Reentrenamiento/limitado"),
                    tags$td("Inmediata: agregas/quitas documentos")
                  ),
                  tags$tr(
                    tags$td("Caso de uso principal"),
                    tags$td("Ideación, lluvia de ideas, borradores"),
                    tags$td("Extracción fáctica, síntesis con trazabilidad")
                  )
                )
              )
            )
          ),
          div(
            class = "alert alert-warning",
            tags$h5("Nota práctica"),
            p("En NotebookLM puedes cargar PDFs y luego ",
              strong("chatear con tu cuaderno"),
              " para obtener respuestas ",
              strong("fundamentadas en esas fuentes"),
              " con ",
              strong("citas in-line"), ".")
          )
        )
      ),

      # --------------------
      # 3) Mapa de herramientas IA
      # --------------------
      bslib::nav_panel(
        title = "Mapa de herramientas IA",
        bslib::layout_column_wrap(
          width = 1/3,
          bslib::card(
            bslib::card_header("ResearchRabbit"),
            tags$p("Explora redes de citación en modo visual: detecta clusters, artículos seminales y nuevas líneas."),
            tags$ul(
              tags$li("Importa colecciones de referencias desde Zotero/Mendeley."),
              tags$li("Genera mapas de trabajos anteriores, posteriores y similares en segundos."),
              tags$li("Comparte colecciones con tu equipo.")
            ),
            tags$a(href = "https://www.researchrabbitapp.com/", target = "_blank", "Crear cuenta" )
          ),
          bslib::card(
            bslib::card_header("NotebookLM"),
            tags$p("Cuaderno inteligente de Google que conecta tus PDFs, Docs y notas con un LLM para respuestas con citas."),
            tags$ul(
              tags$li("Carga hasta 20 fuentes por cuaderno y agrúpalas por proyecto."),
              tags$li("Haz preguntas, solicita resúmenes o tablas con referencias automáticas."),
              tags$li("Genera podcasts de síntesis o resúmenes dirigidos a distintos públicos." )
            ),
            tags$a(href = "https://notebooklm.google/", target = "_blank", "Abrir NotebookLM" )
          ),
          bslib::card(
            bslib::card_header("Otras herramientas"),
            tags$ul(
              tags$li(strong("Litmaps"), ": seguimiento de nuevos artículos vía mapas dinámicos."),
              tags$li(strong("Connected Papers"), ": visualiza relaciones temáticas cuando parte de un paper semilla."),
              tags$li(strong("Consensus"), ": respuestas apoyadas en papers revisados por pares."),
              tags$li(strong("Elicit"), ": extracción rápida de secciones clave y variables desde PDFs."),
              tags$li(strong("Scite"), ": verifica contexto de citas (apoya/contradice) antes de usar evidencia."),
              tags$li(strong("Perplexity"), ": búsqueda conversacional con citas verificables.")
            )
          ),
          bslib::card(
            bslib::card_header("Recomendaciones según etapa"),
            selectInput(
              ns("tool_stage"),
              "Selecciona la etapa de tu proceso",
              choices = c(
                "Descubrimiento" = "descubrimiento",
                "Curación" = "curacion",
                "Consulta analítica" = "consulta",
                "Ideación y escritura" = "ideacion"
              ),
              selected = "descubrimiento"
            ),
            uiOutput(ns("tool_recommendation"))
          )
        )
      ),

      # --------------------
      # 4) Flujo de 4 pasos
      # --------------------
      bslib::nav_panel(
        title = "Flujo de 4 pasos",
        bslib::navset_tab(
          id = ns("steps_tabs"),

          # 4.1 Descubrir
          bslib::nav_panel(
            title = "1) Descubrir",
            bslib::card(
              bslib::card_header("Descubrimiento visual por redes"),
              p("Además de búsquedas por palabra clave, utiliza mapas de citas para explorar el campo de manera no lineal."),
              tags$ul(
                tags$li("Trabajos anteriores (base fundacional)"),
                tags$li("Trabajos posteriores (desarrollos recientes)"),
                tags$li("Trabajos similares (superposición de referencias)")
              ),
              p(
                "Herramientas clave: ",
                a(href = "https://www.researchrabbitapp.com/", target = "_blank", "ResearchRabbit"),
                ", ",
                a(href = "https://www.connectedpapers.com/", target = "_blank", "Connected Papers"),
                " y alertas inteligentes en ",
                a(href = "https://www.litmaps.com/", target = "_blank", "Litmaps")
              )
            )
          ),

          # 4.2 Curar
          bslib::nav_panel(
            title = "2) Curar",
            bslib::card(
              bslib::card_header("Colección deliberada (GIGO)"),
              p("Curar intencionalmente 15–30 artículos clave (revisiones, meta-análisis, métodos y experimentos seminales)."),
              tags$ul(
                tags$li("Define el enfoque temático y criterios de inclusión/exclusión."),
                tags$li("Prioriza calidad sobre cantidad; evita mezclar temas heterogéneos."),
                tags$li("Carga esta colección en NotebookLM o en un stack de RAG propio para que sea tu ‘terreno de hechos’.")
              )
            )
          ),

          # 4.3 Consultar
          bslib::nav_panel(
            title = "3) Consultar",
            bslib::card(
              bslib::card_header("Consultas fundamentadas (RAG en acción)"),
              p("Ejemplos de prompts de consulta para NotebookLM:"),
              tags$ol(
                tags$li("“Con base en las fuentes cargadas, resume las metodologías usadas para medir fijación de N en leguminosas.”"),
                tags$li("“Construye una tabla comparativa de Autor A (2020), Autor B (2022) y Autor C (2023) sobre efectos de ", em("Trichoderma"), " en raíces (especie/dosis/resultados).”"),
                tags$li("“Extrae todas las métricas de rendimiento reportadas (kg/ha, biomasa seca, etc.) bajo estrés hídrico en cebada.”"),
                tags$li("“¿Qué vacíos de conocimiento declaran los autores en ‘Discusión’? Complementa con contexto de Scite.”")
              )
            )
          ),

          # 4.4 Idear
          bslib::nav_panel(
            title = "4) Idear",
            bslib::card(
              bslib::card_header("Ingeniería de prompts (rol, contexto, formato)"),
              tags$ul(
                tags$li(strong("Rol:"), " define la ‘personalidad’ experta (p.ej., revisor crítico de Plant and Soil)."),
                tags$li(strong("Contexto:"), " pega tu síntesis fundamentada (del Paso 3)."),
                tags$li(strong("Formato:"), " especifica la salida esperada (lista numerada, tabla, pseudo-código, etc.).")
              ),
              div(
                class = "alert alert-info",
                tags$h5("Ejemplo de ciclo completo (agronomía)"),
                p("Basado en lagunas sobre micoparasitismo de ", em("T. harzianum"), " contra ", em("F. graminearum"),
                  ", pide 3 hipótesis, un diseño de invernadero y un borrador de análisis en R para un DBCA con 4 tratamientos × 5 repeticiones.")
              )
            )
          )
        )
      ),

      # --------------------
      # 5) Práctica guiada (interactivo)
      # --------------------
      bslib::nav_panel(
        title = "Práctica guiada",
        bslib::layout_column_wrap(
          width = 1/2,
          bslib::card(
            bslib::card_header("Constructor de prompts"),
            textInput(ns("pr_rol"), "Rol (quién debe ‘ser’ la IA)", placeholder = "p.ej., Fitopatólogo experto y estadístico de campo"),
            textAreaInput(ns("pr_contexto"), "Contexto (pega tu síntesis del Paso 3)", placeholder = "Resumen/tabla que salió de NotebookLM...", rows = 6),
            textAreaInput(ns("pr_tarea"), "Tarea (qué debe hacer)", placeholder = "Genera 3 hipótesis, propone un diseño X, redacta código Y...", rows = 5),
            textInput(ns("pr_formato"), "Formato de salida", placeholder = "Lista numerada; tabla con columnas A/B/C; código R con comentarios..."),
            textInput(ns("pr_restricciones"), "Restricciones (opcional)", placeholder = "Citar supuestos; no inventar referencias; usar α=0.05; etc."),
            hr(),
            bslib::tooltip(
              actionButton(ns("pr_reset"), "Limpiar"),
              "Vacía los campos para empezar de nuevo.",
              placement = "top"
            ),
            bslib::tooltip(
              downloadButton(ns("pr_descargar"), "Descargar prompt (.txt)"),
              "Descarga el prompt ensamblado.",
              placement = "top"
            )
          ),
          bslib::card(
            bslib::card_header("Vista previa del prompt"),
            textAreaInput(ns("pr_preview"), NULL, value = "", rows = 20) # (si tu Shiny soporta `resize`, puedes añadirlo)
          )
        )
      ),

      # --------------------
      # 6) Prompts efectivos (banco)
      # --------------------
      bslib::nav_panel(
        title = "Prompts efectivos",
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Ideación disciplinar"),
            tags$pre(style="white-space:pre-wrap;",
"Actúa como un agrónomo de suelos y un especialista en teledetección.
Contexto: Tengo estudios fundamentados que muestran X, Y, Z (pegar síntesis).
Tarea: Propón 5 hipótesis comprobables que vinculen reflectancia de dosel con actividad microbiana en rizosfera.
Formato: Lista numerada; cada hipótesis debe incluir una métrica, un método de medición y una variable de control.")
          ),
          bslib::card(
            bslib::card_header("Crítica metodológica"),
            tags$pre(style="white-space:pre-wrap;",
"Actúa como revisor de Plant and Soil, estricto en diseño y análisis.
Contexto: (pegar resúmenes metodológicos fundamentados)
Tarea: Señala debilidades en muestreo, control de sesgos y análisis estadístico; sugiere mejoras concretas.
Formato: Tabla con columnas: Aspecto | Hallazgo | Riesgo | Recomendación.")
          ),
          bslib::card(
            bslib::card_header("Extracción/tablas comparativas"),
            tags$pre(style="white-space:pre-wrap;",
"Rol: Asistente de revisión sistemática.
Contexto: (pegar artículos sobre Trichoderma en raíces)
Tarea: Construye una tabla con Especie/cepa | Dosis | Cultivo | Condición | Métrica de respuesta | Efecto | Fuente (con cita).
Formato: Tabla en Markdown (| |). Sin inventar datos.")
          ),
          bslib::card(
            bslib::card_header("Hipótesis + diseño + análisis en R"),
            tags$pre(style="white-space:pre-wrap;",
"Rol: Fitopatólogo + estadístico.
Contexto: (pegar lagunas fundamentadas sobre micoparasitismo)
Tarea: 3 hipótesis; diseño DBCA (4 tratamientos × 5 repeticiones); código R para ANOVA + diagnóstico de supuestos + comparaciones post-hoc.
Formato: Lista numerada; luego bloque de código en R fuertemente comentado.")
          )
        )
      ),

      # --------------------
      # 7) Referencias (APA 7)
      # --------------------
      bslib::nav_panel(
        title = "Referencias",
        bslib::layout_column_wrap(
          width = 1,
          bslib::card(
            bslib::card_header("Bibliografía y recursos"),
            tags$ul(
              tags$li("Bornmann, L., & Mutz, R. (2015). Growth rates of modern science. JASIST. https://doi.org/10.1002/asi.23329"),
              tags$li("Lewis, P. et al. (2020). Retrieval-Augmented Generation for Knowledge-Intensive NLP. NeurIPS. https://doi.org/10.48550/arXiv.2005.11401"),
              tags$li("Ji, Z. et al. (2023). Survey of Hallucination in Natural Language Generation. TMLR. https://openreview.net/forum?id=VvR4WRKZ4h"),
              tags$li("ResearchRabbit — How it works: https://www.researchrabbitapp.com/how-it-works"),
              tags$li("NotebookLM Help — Learn about NotebookLM: https://support.google.com/notebooklm/answer/16164461"),
              tags$li("Litmaps — Product tour: https://www.litmaps.com/product"),
              tags$li("Consensus — Evidence-based answers: https://consensus.app/"),
              tags$li("Elicit — The AI research assistant: https://elicit.com/"),
              tags$li("Scite — Smart citations: https://scite.ai/"),
              tags$li("Anthropic — Prompting best practices: https://docs.anthropic.com/claude/docs/prompt-engineering"),
              tags$li("Google Cloud — Prompt design best practices: https://cloud.google.com/vertex-ai/generative-ai/docs/text/prompt-design")
            ),
            p(em("Nota: ver la pestaña ‘LLM vs RAG’ para la tabla comparativa y la definición operativa utilizada en esta sesión."))
          )
        )
      )

    ) # /navset_tab
  )
}

session1_v3Server <- function(input, output, session) {

  ns <- session$ns

  # helper %||% (debe existir antes de usarse)
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  tool_mapping <- list(
    descubrimiento = list(
      titulo = "Descubrimiento",
      descripcion = c(
        "Explora el panorama con ResearchRabbit o Connected Papers para identificar clusters temáticos.",
        "Programa alertas en Litmaps y configura búsquedas conversacionales en Perplexity."),
      acciones = c(
        "Documenta papers promisorios y agrega etiquetas temáticas.",
        "Sincroniza tus referencias con Zotero/Mendeley para mantener bibliografía limpia." )
    ),
    curacion = list(
      titulo = "Curación",
      descripcion = c(
        "Evalúa calidad metodológica con Consensus y usa Scite para validar el contexto de las citas.",
        "Resume artículos clave en NotebookLM y guarda resúmenes disciplinarios."),
      acciones = c(
        "Crea carpetas por hipótesis y enlaza tus notas a los documentos originales.",
        "Etiqueta vacíos de información detectados para reutilizarlos en prompts." )
    ),
    consulta = list(
      titulo = "Consulta analítica",
      descripcion = c(
        "Carga tu colección curada en NotebookLM para responder preguntas con citas.",
        "Complementa con extracción estructurada en Elicit y tablas en Sheets/Excel."),
      acciones = c(
        "Guarda los prompts exitosos en un cuaderno compartido.",
        "Haz control cruzado con manuales/metodologías antes de incorporar hallazgos." )
    ),
    ideacion = list(
      titulo = "Ideación y escritura",
      descripcion = c(
        "Itera borradores de hipótesis, diseños experimentales y guiones de presentación con LLMs (Claude, Gemini).",
        "Utiliza modelos especializados (p.ej., GitHub Copilot para código R) para acelerar la implementación."),
      acciones = c(
        "Integra retroalimentación de pares y registra supuestos estadísticos.",
        "Genera checklists de validación antes de ejecutar experimentos o publicar." )
    )
  )

  # --- Reactive: Ensambla el prompt en tiempo real (robusto a NULL)
  prompt_r <- reactive({
    rol   <- input$pr_rol           %||% ""
    ctx   <- input$pr_contexto      %||% ""
    tarea <- input$pr_tarea         %||% ""
    forma <- input$pr_formato       %||% ""
    restr <- input$pr_restricciones %||% ""

    parts <- c(
      if (shiny::isTruthy(rol))   paste0("Actúa como: ", rol, ".") else NULL,
      if (shiny::isTruthy(ctx))   paste0("Contexto fundamentado:\n", ctx) else NULL,
      if (shiny::isTruthy(tarea)) paste0("Tu tarea:\n", tarea) else NULL,
      if (shiny::isTruthy(forma)) paste0("Formato de salida esperado:\n", forma) else NULL,
      if (shiny::isTruthy(restr)) paste0("Restricciones/criterios:\n", restr) else NULL
    )

    paste(parts, collapse = "\n\n")
  })

  # Actualiza la vista previa al cambiar entradas
  observeEvent(
    list(input$pr_rol, input$pr_contexto, input$pr_tarea, input$pr_formato, input$pr_restricciones),
    ignoreInit = TRUE,
    handlerExpr = {
      updateTextAreaInput(session, "pr_preview", value = prompt_r())
    }
  )

  # --- Reset
  observeEvent(input$pr_reset, {
    updateTextInput(session, "pr_rol", value = "")
    updateTextAreaInput(session, "pr_contexto", value = "")
    updateTextAreaInput(session, "pr_tarea", value = "")
    updateTextInput(session, "pr_formato", value = "")
    updateTextInput(session, "pr_restricciones", value = "")
    updateTextAreaInput(session, "pr_preview", value = "")
  })

  # --- Descargar .txt
  output$pr_descargar <- downloadHandler(
    filename = function() {
      paste0("prompt_sesion1_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    },
    content = function(file) {
      writeLines(prompt_r(), con = file, useBytes = TRUE)
    }
  )

  output$tool_recommendation <- renderUI({
    etapa <- input$tool_stage %||% "descubrimiento"
    info <- tool_mapping[[etapa]]

    if (is.null(info)) {
      return(NULL)
    }

    tagList(
      tags$h5(info$titulo),
      tags$ul(lapply(info$descripcion, tags$li)),
      tags$h6("Acciones sugeridas"),
      tags$ul(lapply(info$acciones, tags$li))
    )
  })

}
