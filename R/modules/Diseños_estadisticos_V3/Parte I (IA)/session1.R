# session1.R
# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA
# Dependencias: shiny, bslib
# ------------------------------------------------------------

session1_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 1: Flujo de Trabajo Moderno con Inteligencia Artificial")
    ),

    bslib::navset_tab(

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
              tags$li("Practicar ingeniería de prompts: rol, contexto y formato.")
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
      # 3) Flujo de 4 pasos
      # --------------------
      bslib::nav_panel(
        title = "Flujo de 4 pasos",
        bslib::navset_tab(
          id = ns("steps_tabs"),

          # 3.1 Descubrir
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
                "Herramientas: ",
                a(href = "https://www.researchrabbitapp.com/", target = "_blank", "ResearchRabbit"),
                " / ",
                a(href = "https://www.litmaps.com/", target = "_blank", "Litmaps")
              )
            )
          ),

          # 3.2 Curar
          bslib::nav_panel(
            title = "2) Curar",
            bslib::card(
              bslib::card_header("Colección deliberada (GIGO)"),
              p("Curar intencionalmente 15–30 artículos clave (revisiones, meta-análisis, métodos y experimentos seminales)."),
              tags$ul(
                tags$li("Define el enfoque temático y criterios de inclusión/exclusión."),
                tags$li("Prioriza calidad sobre cantidad; evita mezclar temas heterogéneos."),
                tags$li("Carga esta colección en NotebookLM para que sea tu ‘terreno de hechos’.")
              )
            )
          ),

          # 3.3 Consultar
          bslib::nav_panel(
            title = "3) Consultar",
            bslib::card(
              bslib::card_header("Consultas fundamentadas (RAG en acción)"),
              p("Ejemplos de prompts de consulta para NotebookLM:"),
              tags$ol(
                tags$li("“Con base en las fuentes cargadas, resume las metodologías usadas para medir fijación de N en leguminosas.”"),
                tags$li("“Construye una tabla comparativa de Autor A (2020), Autor B (2022) y Autor C (2023) sobre efectos de ", em("Trichoderma"), " en raíces (especie/dosis/resultados).”"),
                tags$li("“Extrae todas las métricas de rendimiento reportadas (kg/ha, biomasa seca, etc.) bajo estrés hídrico en cebada.”"),
                tags$li("“¿Qué vacíos de conocimiento declaran los autores en ‘Discusión’?”")
              )
            )
          ),

          # 3.4 Idear
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
      # 4) Práctica guiada (interactivo)
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
      # 5) Prompts efectivos (banco)
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
      # 6) Referencias (APA 7)
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

}
