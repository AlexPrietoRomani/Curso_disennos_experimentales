# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA (V3)
# Reorganizado en funciones UI por pestaña
# ------------------------------------------------------------

# ---------- UI helpers por pestaña (reciben ns) -----------------

pestanna1_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña1: Qué es la IA",
    icon = bs_icon("diagram-3"), # Icono para la pestaña

    # --- Fila 1: definición + mapa de círculos ---
    layout_column_wrap(
      width = 1/2,
      fill = FALSE, # Permite que las tarjetas tengan alturas diferentes si es necesario

      # --- Columna 1: Definición + contexto histórico ---
      card(
        card_header(
          class = "bg-primary text-white",
          "Definición operativa y contexto histórico"
        ),
        card_body(
          p(
            "En este curso seguiremos la definición de Russell y Norvig: ",
            "la Inteligencia Artificial (IA) es el estudio de ",
            strong("agentes que perciben su entorno y actúan sobre él"),
            " para alcanzar objetivos."
          ),
          p(
            "La idea de construir inteligencia artificial no es nueva: va desde los autómatas míticos y la lógica de ",
            "Ramon Llull y Leibniz, hasta la computación de Turing, la conferencia de Dartmouth y varios ",
            "‘inviernos de la IA’. Hoy la IA moderna se apoya en ",
            strong("aprendizaje automático, deep learning y modelos de lenguaje grandes (LLM)"),
            ", que veremos a lo largo de la sesión."
          ),
          hr(),

          # --- Pilares visuales ---
          h5("Los 3 pilares de un agente", class = "mb-3"),
          layout_column_wrap(
            width = 1/3, fill = FALSE, gap = 10,
            card(
              class = "text-center border-light shadow-sm h-100",
              card_body(
                bs_icon("eye", size = "2em", class = "text-primary mb-2"),
                h6("Agente", class = "mt-2 card-title"),
                p(
                  class = "small card-text text-muted",
                  "Algo que percibe y actúa (software, robot, servicio en la nube)."
                )
              )
            ),
            card(
              class = "text-center border-light shadow-sm h-100",
              card_body(
                bs_icon("globe-americas", size = "2em", class = "text-info mb-2"),
                h6("Entorno", class = "mt-2 card-title"),
                p(
                  class = "small card-text text-muted",
                  "Datos, sensores, texto, imágenes, registros agrícolas, etc."
                )
              )
            ),
            card(
              class = "text-center border-light shadow-sm h-100",
              card_body(
                bs_icon("bullseye", size = "2em", class = "text-success mb-2"),
                h6("Objetivo", class = "mt-2 card-title"),
                p(
                  class = "small card-text text-muted",
                  "Medida de desempeño (reducir error, aumentar rendimiento, gestionar riesgo)."
                )
              )
            )
          )
        )
      ),

      # --- Columna 2: Mapa interactivo ---
      card(
        card_header("Mapa interactivo: ¿dónde se ubican los LLM dentro de la IA?"),

        div(
          class = "ia-map-legend text-muted small mb-2",
          "Pasa el mouse por los círculos para ver el efecto visual y haz clic para fijar la explicación."
        ),

        # Contenedor de círculos
        div(
          class = "ia-map-wrapper",

          # Círculo grande: IA general (clicable)
          div(
            class = "ia-circle-main",
            actionButton(
              ns("ia_btn_general"),
              label = "Inteligencia\nArtificial",
              class  = "btn-light ia-circle-btn-main ia-circle-btn ia-selected"
            ),

            # Círculos internos: subcampos
            div(
              class = "ia-circle-sub ia-circle-simbolica",
              actionButton(
                ns("ia_btn_simb"),
                label = "IA\nsimbólica",
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
                label = "LLM y modelos\nfundacionales",
                class  = "btn-primary ia-circle-btn" # Destacado
              )
            )
          )
        ),

        hr(),
        # Aquí pintamos la ficha según el círculo seleccionado
        uiOutput(ns("ia_map_detail")),

        # Script para resaltar el círculo seleccionado
        tags$script(HTML(
          sprintf("
            $(document).on('click', '#%s .ia-map-wrapper .ia-circle-btn', function() {
              $('#%s .ia-map-wrapper .ia-circle-btn').removeClass('ia-selected');
              $(this).addClass('ia-selected');
            });
          ", ns(""), ns(""))
        ))
      )
    ),

    # --- Fila 2: Historia + ejemplos con tabs tipo “botón” ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Historia y ejemplos de IA"),
        navset_pill( # pills quedan como botones
          id = ns("ia_hist_tabs"),

          # --- Tab: Línea Histórica (resumen del reporte) ---
          nav_panel(
            "Línea histórica",
            icon = bs_icon("calendar3"),
            div(
              class = "timeline-wrapper",

              div(
                class = "timeline-item",
                div(class = "timeline-year", "Antigüedad – s. XVII"),
                div(
                  class = "timeline-content",
                  strong("Autómatas y lógica temprana: "),
                  "mitos de máquinas animadas, la lógica combinatoria de Ramon Llull ",
                  "y los proyectos de Leibniz de mecanizar el razonamiento."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1950"),
                div(
                  class = "timeline-content",
                  strong("Alan Turing: "),
                  "formaliza la computación y propone el test de Turing sobre comportamiento inteligente."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1956"),
                div(
                  class = "timeline-content",
                  strong("Conferencia de Dartmouth: "),
                  "nace la IA como campo académico y se inaugura la era de la IA simbólica."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1960–70s"),
                div(
                  class = "timeline-content",
                  strong("GOFAI y sistemas expertos: "),
                  "programas basados en reglas para demostración de teoremas, diagnóstico y planificación."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1970–80s"),
                div(
                  class = "timeline-content",
                  strong("Inviernos de la IA: "),
                  "promesas no cumplidas y limitaciones de hardware provocan recortes de financiación."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1990–00s"),
                div(
                  class = "timeline-content",
                  strong("Aprendizaje automático estadístico: "),
                  "árboles, SVM y redes neuronales poco profundas ganan terreno en tareas de predicción."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "1997"),
                div(
                  class = "timeline-content",
                  strong("Deep Blue: "),
                  "un sistema especializado vence al campeón mundial de ajedrez, mostrando el poder del cómputo masivo."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "2010–16"),
                div(
                  class = "timeline-content",
                  strong("Deep learning a gran escala: "),
                  "GPUs y grandes datasets impulsan avances en visión, voz y texto."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "2017"),
                div(
                  class = "timeline-content",
                  strong("Transformer: "),
                  "la arquitectura basada en atención permite entrenar modelos de lenguaje mucho más grandes."
                )
              ),

              div(
                class = "timeline-item",
                div(class = "timeline-year", "2018–hoy"),
                div(
                  class = "timeline-content",
                  strong("LLM, alucinaciones y grounding: "),
                  "modelos como GPT o LLaMA muestran capacidades emergentes, pero requieren técnicas como RAG ",
                  "y observabilidad para reducir alucinaciones y hacerlos utilizables en entornos críticos."
                )
              )
            )
          ),

          # --- Tab: Ejemplos (Mejorada con iconos y RAG) ---
          nav_panel(
            "Ejemplos en ciencia y agro",
            icon = bs_icon("lightbulb"),
            layout_column_wrap(
              width = 1/2,
              card(
                card_header(bs_icon("code-square"), "IA simbólica"),
                tags$ul(
                  tags$li("Sistemas expertos para diagnóstico de plagas según reglas IF–THEN."),
                  tags$li("Motores de reglas para decidir labores según umbrales climáticos.")
                )
              ),
              card(
                card_header(bs_icon("graph-up-arrow"), "Aprendizaje automático clásico"),
                tags$ul(
                  tags$li("Modelos de regresión / random forest para predecir rendimiento (kg/ha)."),
                  tags$li("Clasificación de lotes según riesgo fitosanitario a partir de clima + manejo.")
                )
              ),
              card(
                card_header(bs_icon("images"), "Deep learning / visión"),
                tags$ul(
                  tags$li("Redes convolucionales para conteo de plantas o frutos en imágenes de dron."),
                  tags$li("Segmentación de canopia para estimar cobertura y vigor.")
                )
              ),
              card(
                card_header(bs_icon("chat-square-text"), "NLP, LLM y RAG"),
                tags$ul(
                  tags$li("Modelos tipo GPT que resumen artículos, generan código en R/Python y proponen hipótesis/diseños experimentales."),
                  tags$li(
                    "Sistemas de ",
                    strong("Generación Aumentada por Recuperación (RAG)"),
                    " que conectan el LLM con tu biblioteca de documentos para reducir alucinaciones."
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

pestanna2_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña2: Qué es un LLM",
    icon  = bs_icon("chat-square-dots"),

    # --- Fila 1: definición + fortalezas/limitaciones ---
    layout_column_wrap(
      width = 1/2,
      fill  = FALSE,

      # Columna 1: definición y jerarquía dentro de la IA
      card(
        card_header(
          class = "bg-primary text-white",
          "¿Qué es un modelo de lenguaje grande (LLM)?"
        ),
        card_body(
          p(
            "En este curso llamamos ",
            strong("modelo de lenguaje grande (LLM)"),
            " a una red neuronal de propósito general entrenada con enormes corpus de texto ",
            "para predecir la siguiente palabra (token) en una secuencia.",
            " Modelos como GPT-3/4 o PaLM son ejemplos típicos ",
            "(Brown et al., 2020; Bommasani et al., 2021)."
          ),
          tags$ul(
            tags$li(
              strong("Modelo de lenguaje:"),
              " aprende patrones estadísticos del texto (probabilidades de secuencias)."
            ),
            tags$li(
              strong("Grande:"),
              " contiene miles de millones de parámetros, lo que le permite resolver muchas tareas ",
              "con el mismo modelo (traducción, resumen, código, etc.)."
            ),
            tags$li(
              strong("Generativo:"),
              " produce respuestas token a token, condicionadas por el contexto del prompt."
            )
          ),
          hr(),
          h6("¿Dónde vive un LLM dentro de la IA?"),
          div(
            class = "llm-hierarchy",
            span("IA",               class = "llm-level llm-level-ia"),
            span("ML",               class = "llm-level llm-level-ml"),
            span("Deep Learning",    class = "llm-level llm-level-dl"),
            span("NLP",              class = "llm-level llm-level-nlp"),
            span("LLM",              class = "llm-level llm-level-llm")
          ),
          p(
            class = "small text-muted mb-0",
            "Todos los LLM son modelos de procesamiento de lenguaje natural (NLP), ",
            "pero no todo modelo de NLP es un LLM: los LLM son la familia de ",
            strong("modelos de lenguaje de gran escala para tareas generativas"),
            " (Bommasani et al., 2021)."
          )
        )
      ),

      # Columna 2: fortalezas y límites (incluye alucinaciones de forma resumida)
      card(
        card_header("¿Qué hace bien un LLM y dónde se equivoca?"),
        card_body(
          layout_column_wrap(
            width = 1/2,
            fill  = FALSE,

            # Fortalezas
            div(
              h6(bs_icon("hand-thumbs-up"), " Fortalezas"),
              tags$ul(
                class = "small",
                tags$li(
                  "Produce texto coherente en muchos dominios con poco ejemplo ",
                  em("(few-shot / zero-shot)"),
                  " (Brown et al., 2020)."
                ),
                tags$li(
                  "Integra un contexto amplio (miles de tokens) y patrones complejos ",
                  "gracias a la arquitectura ",
                  em("Transformer"),
                  " (Vaswani et al., 2017)."
                ),
                tags$li(
                  "Puede seguir cadenas de razonamiento cuando lo guiamos paso a paso ",
                  em("(Chain-of-Thought prompting; Wei et al., 2022).")
                )
              )
            ),

            # Limitaciones
            div(
              h6(bs_icon("exclamation-triangle"), " Limitaciones estructurales"),
              tags$ul(
                class = "small",
                tags$li(
                  "No tiene una ‘base de conocimiento’ verificable: ",
                  "solo estima la siguiente palabra más probable ",
                  "a partir de patrones en los datos de entrenamiento."
                ),
                tags$li(
                  "Puede generar contenido plausible pero falso o no verificable ",
                  em("(alucinaciones/“confabulaciones”)"),
                  " (Huang et al., 2023; Rawte et al., 2023; Zhang et al., 2023)."
                ),
                tags$li(
                  "Su conocimiento está ‘congelado’ en la fecha de entrenamiento; ",
                  "no ve eventos posteriores salvo que lo conectemos a fuentes externas ",
                  "(RAG / grounding, ver pestañas 3 y 4)."
                )
              )
            )
          ),
          div(
            class = "alert alert-warning small mt-2",
            strong("Regla de uso en la sesión: "),
            "usa el LLM como generador de borradores e ideas; ",
            "la evidencia dura debe venir de artículos, datos o documentos externos ",
            "con citas explícitas."
          )
        )
      )
    ),

    # --- Fila 2: pipeline interno del LLM (interactivo) ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Cómo funciona un LLM por dentro (pipeline de generación)"),

        p(
          "Cuando envías un prompt, el modelo sigue siempre la misma tubería: ",
          "convierte tu texto en números, lo procesa con bloques ",
          em("Transformer"),
          " y vuelve a texto (Vaswani et al., 2017; Brown et al., 2020)."
        ),

        # Botones/etapas del flujo
        div(
          class = "llm-flow",
          actionButton(
            ns("llm_step_input"),
            label = "1. Texto de entrada",
            class = "btn-light llm-step-btn llm-step-selected"
          ),
          span(class = "llm-flow-arrow", bs_icon("arrow-right")),
          actionButton(
            ns("llm_step_tokens"),
            label = "2. Tokens",
            class = "btn-outline-primary llm-step-btn"
          ),
          span(class = "llm-flow-arrow", bs_icon("arrow-right")),
          actionButton(
            ns("llm_step_embed"),
            label = "3. Embeddings + posición",
            class = "btn-outline-primary llm-step-btn"
          ),
          span(class = "llm-flow-arrow", bs_icon("arrow-right")),
          actionButton(
            ns("llm_step_transformer"),
            label = "4. Bloques Transformer",
            class = "btn-outline-primary llm-step-btn"
          ),
          span(class = "llm-flow-arrow", bs_icon("arrow-right")),
          actionButton(
            ns("llm_step_output"),
            label = "5. Softmax y muestreo",
            class = "btn-outline-primary llm-step-btn"
          )
        ),

        # Detalle dinámico de la etapa seleccionada
        div(
          class = "llm-step-detail",
          uiOutput(ns("llm_step_detail"))
        ),

        tags$pre(
          class = "llm-flow-ascii mt-3",
"Texto → tokens → embeddings + posición → [Bloques Transformer con auto-atención] →
  logits → softmax → muestreo (temperatura/top-k) → texto generado"
        ),

        # Script JS para resaltar el botón seleccionado
        tags$script(HTML(
          sprintf("
            $(document).on('click', '#%s .llm-flow .llm-step-btn', function() {
              $('#%s .llm-flow .llm-step-btn').removeClass('llm-step-selected');
              $(this).addClass('llm-step-selected');
            });
          ", ns(""), ns(""))
        ))
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

    # “Tip” común al final de cada definición
    tip_card <- card(
      class = "border-secondary-subtle mt-2",
      card_body(
        class = "p-2 small",
        bs_icon("cursor", class = "me-1"),
        "Haz clic en otro círculo para cambiar la definición o vuelve a ‘Inteligencia Artificial’ para ver la vista general."
      )
    )

    switch(
      dom,

      # --- Vista general IA ---
      ia_general = tagList(
        h4(bs_icon("bounding-box"), " Inteligencia Artificial (vista general)"),
        p(
          "La IA es un campo amplio que reúne enfoques simbólicos, estadísticos y neuronales. ",
          "Su historia combina momentos de euforia (promesas de máquinas inteligentes generales) ",
          "con ‘inviernos de la IA’, cuando las limitaciones de hardware y datos frenaron el progreso."
        ),
        p(
          "Los modelos de lenguaje grandes que usamos hoy se apoyan en esa trayectoria: primero reglas simbólicas, ",
          "después aprendizaje automático y, finalmente, redes neuronales profundas basadas en la arquitectura ",
          em("Transformer"),
          "."
        ),
        tags$ul(
          tags$li(strong("Entrada:"), " datos de sensores, imágenes, texto, registros, señales, etc."),
          tags$li(strong("Salida:"),  " decisiones, predicciones, recomendaciones o textos generados."),
          tags$li(
            strong("Rol del humano:"),
            " formular objetivos, curar los datos, vigilar sesgos y validar resultados antes de usarlos en producción."
          )
        ),
        tip_card
      ),

      # --- IA simbólica / reglas ---
      ia_simb = tagList(
        h4(bs_icon("code-square"), " IA simbólica / basada en reglas"),
        p(
          "Esta corriente (a veces llamada ",
          em("Good Old-Fashioned AI"),
          ") intenta capturar la inteligencia como lógica explícita: ",
          "reglas IF–THEN, ontologías y motores de inferencia."
        ),
        tags$ul(
          tags$li("Ejemplos clásicos: demostradores de teoremas, sistemas expertos médicos o legales."),
          tags$li("Ventaja: alta explicabilidad; cada conclusión se puede rastrear a una regla concreta."),
          tags$li("Limitación: difícil de mantener y escalar cuando el dominio es amplio, ruidoso o cambia rápido.")
        ),
        tip_card
      ),

      # --- Machine Learning ---
      ia_ml = tagList(
        h4(bs_icon("graph-up-arrow"), " Aprendizaje automático (Machine Learning)"),
        p(
          "En el aprendizaje automático no escribimos reglas a mano: dejamos que el modelo las ‘aprenda’ ",
          "a partir de ejemplos etiquetados o de patrones en los datos."
        ),
        tags$ul(
          tags$li("Modelos de regresión, árboles de decisión, random forest, SVM, redes neuronales, etc."),
          tags$li("Muy eficaz para predicción de rendimiento, clasificación de lotes o detección de anomalías."),
          tags$li(
            "Dentro de ML, el ",
            strong("deep learning"),
            " usa redes con muchas capas y es la base de la visión por computador, el reconocimiento de voz ",
            "y los modelos de lenguaje grandes."
          )
        ),
        tip_card
      ),

      # --- Robótica y agentes físicos ---
      ia_robotica = tagList(
        h4(bs_icon("robot"), " Robótica y agentes físicos"),
        p(
          "La robótica combina percepción (sensores, cámaras), planificación y control para que un agente actúe ",
          "en el mundo físico. Puede usar tanto reglas simbólicas como aprendizaje automático o refuerzo."
        ),
        tags$ul(
          tags$li("Robots móviles o brazos robóticos que realizan labores agrícolas o de logística."),
          tags$li("Drones que inspeccionan cultivos, infraestructuras o animales."),
          tags$li("Sistemas que aprenden políticas de control mediante aprendizaje por refuerzo en simulación.")
        ),
        tip_card
      ),

      # --- LLM / modelos fundacionales ---
      ia_llm = tagList(
        h4(bs_icon("chat-square-text"), " Modelos de lenguaje grandes (LLM)"),
        p(
          "Los LLM son modelos de ",
          strong("procesamiento de lenguaje natural (NLP)"),
          " basados en deep learning y la arquitectura ",
          em("Transformer"),
          ". Se entrenan con enormes corpus de texto para predecir el siguiente token."
        ),
        tags$ul(
          tags$li(
            strong("Posición en el mapa:"),
            " son un subgrupo de NLP → deep learning → machine learning → IA."
          ),
          tags$li(
            strong("Arquitectura básica:"),
            " embeddings de tokens + bloques Transformer con auto-atención multi-cabeza + capa de salida ",
            "que produce una distribución de probabilidad sobre el vocabulario."
          ),
          tags$li(
            strong("Capacidades emergentes:"),
            " al escalar a miles de millones de parámetros aparecen habilidades como el razonamiento paso a paso ",
            em("(chain-of-thought)"),
            " y la adaptación a nuevas tareas sólo cambiando el ",
            em("prompt"),
            "."
          ),
          tags$li(
            strong("Limitación clave:"),
            " optimizan la fluidez estadística del texto, no la verdad. Por eso pueden ",
            strong("alucinar"),
            " datos o citas; más adelante veremos cómo técnicas de ",
            strong("grounding y RAG"),
            " conectan el LLM con fuentes verificables para reducir este problema."
          )
        ),
        tip_card
      )
    )
  })
}

# Pestaña 2: Qué es un LLM
pestanna2_session1_v3_server <- function(input, output, session) {

  # Etapa seleccionada en el flujo del LLM
  selected_step <- reactiveVal("step_input")

  observeEvent(input$llm_step_input,       { selected_step("step_input")       })
  observeEvent(input$llm_step_tokens,      { selected_step("step_tokens")      })
  observeEvent(input$llm_step_embed,       { selected_step("step_embed")       })
  observeEvent(input$llm_step_transformer, { selected_step("step_transformer") })
  observeEvent(input$llm_step_output,      { selected_step("step_output")      })

  output$llm_step_detail <- renderUI({
    step <- selected_step()

    switch(
      step,

      # 1) Texto de entrada
      step_input = tagList(
        h5(bs_icon("keyboard"), " 1. Texto de entrada"),
        p(
          "Escribes un prompt: una instrucción, una pregunta o un trozo de texto ",
          "sobre el que quieres trabajar."
        ),
        tags$ul(
          tags$li("El sistema puede añadir contexto oculto (rol, reglas, idioma, etc.)."),
          tags$li(
            "Se normaliza el texto (codificación, mayúsculas/minúsculas, caracteres especiales) ",
            "antes de pasar a la tokenización."
          )
        )
      ),

      # 2) Tokens
      step_tokens = tagList(
        h5(bs_icon("braces"), " 2. Tokens: el texto se trocea"),
        p(
          "El texto no se procesa palabra por palabra, sino como una secuencia de ",
          strong("tokens"),
          ": piezas más pequeñas (sub-palabras, símbolos, signos de puntuación)."
        ),
        tags$ul(
          tags$li(
            "La tokenización suele usar variantes de ",
            em("byte-pair encoding (BPE)"),
            " u otros esquemas descritos en la literatura reciente ",
            "(Li et al., 2024; artículo de Wikipedia sobre Large language models)."
          ),
          tags$li(
            "Cada token se mapea a un identificador numérico; el prompt completo se convierte ",
            "en una secuencia de IDs."
          ),
          tags$li(
            "Algunos tokens especiales marcan inicio/fin, saltos de línea, etc. ",
            "También existen ‘glitch tokens’ que pueden causar comportamientos raros."
          )
        )
      ),

      # 3) Embeddings + posición
      step_embed = tagList(
        h5(bs_icon("grid-3x3-gap"), " 3. Embeddings + codificación posicional"),
        p(
          "Cada ID de token se transforma en un vector denso de números llamado ",
          strong("embedding"),
          ", que captura relaciones semánticas (tokens similares → vectores cercanos)."
        ),
        tags$ul(
          tags$li(
            "Se suma una ",
            strong("codificación posicional"),
            " que indica en qué lugar de la secuencia está cada token ",
            "(Vaswani et al., 2017)."
          ),
          tags$li(
            "El resultado: una matriz de tamaño ",
            em("[número de tokens] × [dimensión del embedding]"),
            " que entra al bloque Transformer."
          )
        )
      ),

      # 4) Bloques Transformer
      step_transformer = tagList(
        h5(bs_icon("diagram-3"), " 4. Bloques Transformer: auto-atención"),
        p(
          "El corazón del LLM son los bloques ",
          em("Transformer"),
          " apilados (por ejemplo, 12, 24 o más capas).",
          " Cada bloque aplica ",
          strong("auto-atención multi-cabeza"),
          " y capas feed-forward (Vaswani et al., 2017)."
        ),
        tags$ul(
          tags$li(
            strong("Q, K, V (queries, keys, values):"),
            " para cada token se calculan tres proyecciones lineales; ",
            "las similitudes Q·K determinan qué tokens deben ‘mirarse’ entre sí."
          ),
          tags$li(
            "La auto-atención produce una versión ",
            strong("contextualizada"),
            " de cada token, que incorpora información del resto de la secuencia."
          ),
          tags$li(
            "En modelos generativos se usa atención enmascarada: cada posición sólo ve ",
            "tokens anteriores, preservando la naturaleza de predicción paso a paso."
          )
        )
      ),

      # 5) Softmax y muestreo
      step_output = tagList(
        h5(bs_icon("cursor-text"), " 5. Softmax y muestreo: elegir la siguiente palabra"),
        p(
          "Tras los bloques Transformer, cada posición produce un vector de ",
          em("logits"),
          " (un número por token posible del vocabulario)."
        ),
        tags$ul(
          tags$li(
            "Una capa lineal + función ",
            strong("softmax"),
            " convierten esos logits en una distribución de probabilidad ",
            "sobre el vocabulario."
          ),
          tags$li(
            "El modelo selecciona el siguiente token mediante ",
            strong("muestreo"),
            " (temperatura, top-k, top-p, etc.), ",
            "lo añade al texto y repite el proceso muchas veces."
          ),
          tags$li(
            "La prioridad es la ",
            strong("fluidez estadística"),
            " del texto, no la verdad fáctica; por eso puede alucinar ",
            "si no se conecta a fuentes externas (Lewis et al., 2020; Huang et al., 2023)."
          )
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
