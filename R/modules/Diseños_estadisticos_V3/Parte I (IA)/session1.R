# ------------------------------------------------------------
# Módulo: Sesión 1 — Flujo de Trabajo Moderno con IA (V3)
# Reorganizado en funciones UI por pestaña
# ------------------------------------------------------------

# ---------- UI helpers por pestaña (reciben ns) -----------------

# Pestaña 1: Que es la IA
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

# Pestaña 2: Qué es un LLM
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

# Pestaña 3: Qué es la alucinación de un LLM
pestanna3_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña3: Qué es la alucinación de un LLM",
    icon  = bs_icon("exclamation-octagon"),

    # --- Fila 1: definición + causas/impacto ---
    layout_column_wrap(
      width = 1/2,
      fill  = FALSE,

      # Columna 1: Definición y tipos
      card(
        card_header(
          class = "bg-danger text-white",
          "¿Qué es una alucinación de un LLM?"
        ),
        card_body(
          p(
            "Diremos que un modelo de lenguaje grande ",
            strong("(LLM) alucina"),
            " cuando genera una respuesta que suena coherente y bien redactada, ",
            "pero es ", strong("falsa, inventada o no está respaldada por ninguna fuente verificable"), "."
          ),
          p(
            "Es un efecto estructural de cómo se entrenan estos modelos: ",
            "optimizan la probabilidad de la siguiente palabra, no la verdad."
          ),
          hr(),
          h6("Tipos frecuentes de alucinación"),
          tags$ul(
            class = "hall-type-list",
            tags$li(
              strong("Fáctica: "),
              "inventa datos, números, citas o DOIs que no existen ",
              "(por ejemplo, un artículo inexistente con autores reales)."
            ),
            tags$li(
              strong("Semántica: "),
              "la respuesta es gramaticalmente correcta, pero no responde realmente la pregunta ",
              "o cambia de tema sin avisar."
            ),
            tags$li(
              strong("Gramatical/coherencia global: "),
              "frases individuales tienen sentido, pero el párrafo completo es inconsistente ",
              "o se contradice a sí mismo."
            )
          ),
          div(
            class = "alert alert-light small mt-2",
            bs_icon("info-circle"),
            "  En investigación suelen preocupar más las alucinaciones ",
            strong("fácticas"),
            " (datos o citas inventadas)."
          )
        )
      ),

      # Columna 2: Causas raíz y riesgo para la investigación
      card(
        card_header("¿Por qué alucina un LLM y por qué importa?"),
        card_body(
          h6("Tres causas estructurales"),
          tags$ul(
            class = "hall-type-list",
            tags$li(
              strong("Datos de entrenamiento: "),
              "si el corpus contiene errores, desinformación o lagunas, ",
              "el modelo puede amplificarlos en sus respuestas."
            ),
            tags$li(
              strong("Arquitectura y objetivo de entrenamiento: "),
              "el modelo aprende a optimizar fluidez y coherencia estadística, ",
              "no a chequear la realidad en una base de conocimiento."
            ),
            tags$li(
              strong("Contexto y prompting: "),
              "preguntas ambiguas, ventanas de contexto limitadas ",
              "o parámetros de muestreo muy creativos (temperatura alta) ",
              "incrementan la probabilidad de invención."
            )
          ),
          hr(),
          h6("Impacto en proyectos científicos"),
          tags$ul(
            class = "hall-type-list",
            tags$li(
              "Puede proponer diseños experimentales plausibles pero mal referenciados ",
              "o que nunca se han probado realmente."
            ),
            tags$li(
              "Puede generar tablas de resultados ‘bonitas’ que no provienen de ningún dataset."
            ),
            tags$li(
              "Riesgo de decisiones basadas en información inexistente ",
              "si no se exige siempre cita y verificación."
            )
          ),
          div(
            class = "hall-risk-callout small mt-2",
            strong("Mensaje clave: "),
            "un LLM siempre intentará responder. ",
            "La responsabilidad de decidir cuándo creerle recae en el equipo humano ",
            "y en la arquitectura (RAG, validación, observabilidad)."
          )
        )
      )
    ),

    # --- Fila 2: Cómo medimos la alucinación (tres lentes) ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Cómo medimos la alucinación: tres lentes complementarias"),
        card_body(
          p(
            "En la frontera de 2024 se usan varios benchmarks para medir cuán ‘alucinador’ es un modelo. ",
            "En la práctica, cada benchmark captura un ángulo distinto del riesgo factual."
          ),

          # Selector de benchmark
          div(
            class = "hall-bench-selector",
            actionButton(
              ns("hall_bench_truth"),
              label = "1. Veracidad del conocimiento (TruthfulQA, etc.)",
              class = "btn-outline-primary hall-bench-btn hall-bench-selected"
            ),
            actionButton(
              ns("hall_bench_consistency"),
              label = "2. Robustez del prompt (Consistency / personas)",
              class = "btn-outline-primary hall-bench-btn"
            ),
            actionButton(
              ns("hall_bench_rag"),
              label = "3. Fidelidad a documentos (FActScore / FCR)",
              class = "btn-outline-primary hall-bench-btn"
            )
          ),

          # Detalle dinámico del benchmark seleccionado
          div(
            class = "hall-bench-detail",
            uiOutput(ns("hall_bench_detail"))
          ),

          tags$hr(),
          p(
            class = "small text-muted mb-0",
            "Interpretación práctica: no existe el ‘modelo perfecto’ en todos los ejes. ",
            "Para un asistente de literatura, interesa más la veracidad general; ",
            "para un sistema RAG jurídico, importa ante todo la fidelidad a los documentos internos."
          ),

          # JS para resaltar el botón de benchmark seleccionado
          tags$script(HTML(
            sprintf("
              $(document).on('click', '#%s .hall-bench-selector .hall-bench-btn', function() {
                $('#%s .hall-bench-selector .hall-bench-btn').removeClass('hall-bench-selected');
                $(this).addClass('hall-bench-selected');
              });
            ", ns(""), ns(""))
          ))
        )
      )
    ),

    # --- Fila 3: Playbook para reducir alucinaciones en investigación ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Playbook para reducir alucinaciones en proyectos de investigación"),
        card_body(
          layout_column_wrap(
            width = 1/2,
            fill  = FALSE,

            # Columna izquierda: pipeline defensivo
            div(
              h6(bs_icon("shield-lock"), " Pipeline defensivo en 4 pasos"),
              tags$ol(
                class = "hall-type-list",
                tags$li(
                  strong("1. Formular bien la tarea: "),
                  "preguntas claras, acotadas y con contexto mínimo necesario."
                ),
                tags$li(
                  strong("2. Grounding/RAG: "),
                  "conectar el LLM a PDFs, bases internas o notas curadas ",
                  "y pedir explícitamente que cite fragmentos concretos."
                ),
                tags$li(
                  strong("3. Guardrails y prompting: "),
                  "incluir reglas del tipo ",
                  em("“si no tienes evidencia, responde que no sabes”"),
                  " y desactivar estilos demasiado creativos para análisis serios."
                ),
                tags$li(
                  strong("4. Observabilidad y revisión humana: "),
                  "log de prompts/respuestas, monitoreo de calidad y revisión por pares ",
                  "para los outputs que van a informes o decisiones."
                )
              )
            ),

            # Columna derecha: checklist práctico
            div(
              h6(bs_icon("list-check"), " Checklist rápido para la sesión / paper"),
              tags$ul(
                class = "hall-type-list",
                tags$li("¿El modelo dio ", strong("citas explícitas"), " (DOI, revista, año) para cada afirmación clave?"),
                tags$li("¿Contrastaste las afirmaciones con al menos un motor académico (Semantic Scholar, OpenAlex, etc.)?"),
                tags$li("¿Está documentado en el cuaderno qué párrafos vienen del LLM y cuáles son tuyos?"),
                tags$li("¿Se evitó usar el LLM para generar valores numéricos o resultados de experimentos ‘hipotéticos’?"),
                tags$li("¿El flujo RAG/NotebookLM que usas está restringido solo a documentos que tú mismo curaste?")
              ),
              div(
                class = "alert alert-info small mt-2",
                strong("Regla de oro: "),
                "en ciencia, el LLM es un coautor de borrador, nunca la única fuente de verdad."
              )
            )
          )
        )
      )
    )
  )
}

# Pestaña 4: Cómo usar RAG / NotebookLM
pestanna4_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña4: Cómo usar RAG / NotebookLM",
    icon  = bs_icon("journal-richtext"),

    # --- Fila 1: LLM general vs RAG + resumen de NotebookLM ---
    layout_column_wrap(
      width = 1/2,
      fill  = FALSE,

      # Columna 1: LLM general vs RAG
      card(
        card_header(
          class = "bg-primary text-white",
          "Por qué no basta un LLM general"
        ),
        card_body(
          p(
            "Incluso los modelos de frontera (GPT, Claude, etc.) están entrenados para ",
            strong("producir el texto más probable"),
            " a partir de sus datos de entrenamiento. ",
            "Eso los hace muy buenos redactando, pero también propensos a ",
            strong("alucinar"),
            " cuando no tienen información fiable."
          ),
          layout_column_wrap(
            width = 1/2,
            fill  = FALSE,
            # LLM general
            div(
              class = "rag-col-compare",
              h6(bs_icon("chat-dots"), " LLM general (sin grounding)"),
              tags$ul(
                class = "small",
                tags$li("Conocimiento congelado en la fecha de entrenamiento."),
                tags$li("No ve tus PDFs, protocolos o datos internos por defecto."),
                tags$li("Optimizado para fluidez lingüística, no para verdad fáctica."),
                tags$li("Alta probabilidad de citas inventadas o números inexistentes si el tema es específico.")
              )
            ),
            # RAG / NotebookLM
            div(
              class = "rag-col-compare",
              h6(bs_icon("book-half"), " RAG / NotebookLM (con grounding)"),
              tags$ul(
                class = "small",
                tags$li("Primero recupera fragmentos relevantes de tus fuentes (PDFs, Docs, Slides, notas)."),
                tags$li("Luego genera la respuesta condicionada a esos fragmentos, con ", strong("citas in-line"), "."),
                tags$li("Si los documentos no contienen la respuesta, es más fácil detectar el vacío."),
                tags$li("Mejor ", strong("trazabilidad"), " y auditoría para investigación y empresa.")
              )
            )
          ),
          div(
            class = "alert alert-info small mt-2",
            strong("Idea central: "),
            "RAG no hace al modelo ‘infalible’, pero cambia la pregunta de ",
            em("«¿es verdad lo que dice el modelo?»"),
            " a ",
            em("«¿está realmente apoyado en estas fuentes?»")
          )
        )
      ),

      # Columna 2: NotebookLM como cuaderno RAG
      card(
        card_header("NotebookLM como cuaderno con RAG"),
        card_body(
          p(
            "NotebookLM es un ",
            strong("cuaderno IA centrado en tus fuentes"),
            " (Google Docs, PDFs, Slides, notas). ",
            "Todo lo que responda el modelo se espera que esté fundamentado en esos documentos."
          ),
          tags$ul(
            class = "small",
            tags$li(
              strong("Carga de fuentes:"),
              " revisiones, métodos, ensayos, protocolos, reportes internos."
            ),
            tags$li(
              strong("Preguntas con citas:"),
              " cada respuesta incluye referencias clicables a los documentos de origen."
            ),
            tags$li(
              strong("Modos de síntesis:"),
              " resúmenes, guías de estudio, esquemas y ",
              em("audio overview"),
              " para explicar a no-técnicos."
            ),
            tags$li(
              strong("Rol del investigador:"),
              " decidir qué entra al cuaderno, revisar las citas y corregir omisiones."
            )
          ),
          hr(),
          p(
            class = "small text-muted mb-1",
            "En esta pestaña veremos cómo se conecta todo con las ideas de RAG vistas anteriormente."
          ),
          p(
            a(
              href   = "https://notebooklm.google/",
              target = "_blank",
              "Abrir NotebookLM en el navegador →"
            )
          )
        )
      )
    ),

    # --- Fila 2: Pipeline RAG de NotebookLM (interactivo) ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Pipeline de NotebookLM como sistema RAG"),
        card_body(
          p(
            "Cuando trabajas con un cuaderno de NotebookLM, el flujo típico combina ",
            strong("recuperación"),
            " + ",
            strong("generación"),
            " sobre tus fuentes. Usa este esquema para conectar la teoría con la práctica."
          ),

          # Botones de etapas
          div(
            class = "rag-flow",
            actionButton(
              ns("rag_step_sources"),
              label = "1. Subir y seleccionar fuentes",
              class = "btn-light rag-step-btn rag-step-selected"
            ),
            span(class = "rag-flow-arrow", bs_icon("arrow-right")),
            actionButton(
              ns("rag_step_index"),
              label = "2. Indexación y comprensión",
              class = "btn-outline-primary rag-step-btn"
            ),
            span(class = "rag-flow-arrow", bs_icon("arrow-right")),
            actionButton(
              ns("rag_step_retrieval"),
              label = "3. Pregunta + recuperación",
              class = "btn-outline-primary rag-step-btn"
            ),
            span(class = "rag-flow-arrow", bs_icon("arrow-right")),
            actionButton(
              ns("rag_step_generation"),
              label = "4. Respuesta con citas",
              class = "btn-outline-primary rag-step-btn"
            ),
            span(class = "rag-flow-arrow", bs_icon("arrow-right")),
            actionButton(
              ns("rag_step_sharing"),
              label = "5. Síntesis y compartición",
              class = "btn-outline-primary rag-step-btn"
            )
          ),

          # Detalle dinámico de la etapa
          div(
            class = "rag-step-detail",
            uiOutput(ns("rag_step_detail"))
          ),

          tags$pre(
            class = "rag-flow-ascii mt-3",
"Fuentes (PDFs, Docs, Slides, notas) 
   ↓ indexación semántica (embeddings)
   ↓ recuperación de pasajes relevantes (vector search / filtros)
   ↓ prompt aumentado (pregunta + contexto)
   ↓ generación con citas (LLM)
   ↓ síntesis: resúmenes, guías, audio overview, exportación"
          ),

          # Script JS para resaltar el botón seleccionado
          tags$script(HTML(
            sprintf("
              $(document).on('click', '#%s .rag-flow .rag-step-btn', function() {
                $('#%s .rag-flow .rag-step-btn').removeClass('rag-step-selected');
                $(this).addClass('rag-step-selected');
              });
            ", ns(""), ns(""))
          ))
        )
      )
    )
  )
}

# Pestaña 5: Flujo 4 pasos para búsquedas con IA
pestanna5_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña5: Flujo 4 pasos para búsquedas con IA",
    icon  = bs_icon("search"),

    navset_tab(
      id    = ns("steps_tabs"),
      class = "rag-steps-nav",

      # -------------------------
      # 1) DESCUBRIR
      # -------------------------
      nav_panel(
        title = tagList(
          span("1", class = "rag-step-number"),
          " Descubrir"
        ),
        card(
          card_header("1. Descubrir: de una idea dispersa a una búsqueda reproducible"),
          card_body(
            p(
              "En este paso defines cómo vas a ‘nombrar’ tu problema de investigación ",
              "para que los buscadores y bases de datos devuelvan literatura relevante."
            ),
            tags$ul(
              class = "small",
              tags$li(strong("Combina: "), "palabras clave + sinónimos + métodos + filtros por fuente/año."),
              tags$li(
                "Recuerda que cada base interpreta los operadores lógicos con ligeras diferencias; ",
                "por eso es útil tener una cadena booleana base y adaptarla."
              ),
              tags$li(
                "Tu objetivo aquí no es leer, sino construir una ",
                strong("estrategia de búsqueda documentada"),
                " que puedas reutilizar."
              )
            )
          )
        ),

        layout_column_wrap(
          width = 1/2,
          fill  = FALSE,

          # --- Columna A: Generador booleano (tus campos, reorganizados) ---
          card(
            card_header(bs_icon("braces-asterisk"), "Generador guiado de búsqueda booleana"),
            card_body(
              p(
                class = "small text-muted",
                "Completa los bloques; la app construirá una cadena booleana para pegar en Google Scholar, ",
                "Scopus, Web of Science, PubMed, etc."
              ),
              layout_column_wrap(
                width = 1/2,
                fill  = FALSE,
                textInput(
                  ns("kw_core"),
                  label       = "1) Término núcleo (obligatorio)",
                  placeholder = "blueberry OR \"Vaccinium corymbosum\""
                ),
                textInput(
                  ns("kw_syn"),
                  label       = "2) Sinónimos / variables relacionadas (separar con ; )",
                  placeholder = "yield; productivity; \"fruit set\""
                ),
                textInput(
                  ns("kw_method"),
                  label       = "3) Métodos / condiciones (AND)",
                  placeholder = "NDVI OR hyperspectral"
                ),
                textInput(
                  ns("kw_filter"),
                  label       = "4) Filtros (site:, filetype:, year:, etc.)",
                  placeholder = "site:gov OR site:nih.gov"
                )
              ),

              div(
                class = "kw-preview-wrapper mt-3",
                tags$label(
                  class = "form-label",
                  `for` = ns("kw_preview"),
                  "Vista previa de la cadena (para copiar/pegar)"
                ),
                textAreaInput(
                  ns("kw_preview"),
                  label = NULL,
                  rows  = 4
                )
              ),

              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("kw_build"), "Construir cadena", icon = bs_icon("magic")),
                downloadButton(ns("kw_download"), "Descargar (.txt)")
              ),

              p(
                class = "small text-muted mt-2 mb-0",
                strong("Sugerencia: "),
                "guarda la cadena final en tu protocolo o cuaderno NotebookLM como parte de la metodología."
              )
            )
          ),

          # --- Columna B: Guía rápida / heurísticas ---
          card(
            card_header(bs_icon("lightbulb"), "Guía rápida para una buena búsqueda"),
            card_body(
              tags$ol(
                class = "small rag-discover-list",
                tags$li(
                  strong("1. Define el ‘núcleo’ del tema: "),
                  "cultivo / especie + variable principal + contexto (zona, manejo, estrés)."
                ),
                tags$li(
                  strong("2. Lista sinónimos y términos relacionados: "),
                  "usa tesauros (MeSH, AGROVOC) o palabras de artículos clave."
                ),
                tags$li(
                  strong("3. Añade métodos y filtros: "),
                  "tipo de diseño, sensores, índices, años recientes, idioma, tipo de documento."
                ),
                tags$li(
                  strong("4. Prueba y ajusta: "),
                  "lanza la búsqueda y revisa si aparecen los trabajos ‘clásicos’ del tema; ",
                  "si no, reescribe el núcleo o los sinónimos."
                )
              ),
              div(
                class = "alert alert-info small mb-0",
                strong("Regla práctica: "),
                "si no encuentras los artículos seminales que conoces, la búsqueda está mal diseñada, ",
                "no el campo."
              )
            )
          )
        )
      ),

      # -------------------------
      # 2) CURAR
      # -------------------------
      nav_panel(
        title = tagList(
          span("2", class = "rag-step-number"),
          " Curar"
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("2. Curar: pasar de ‘todo lo que hay’ a ‘lo que realmente sirve’"),
            card_body(
              layout_column_wrap(
                width = 1/2,
                fill  = FALSE,
                card(
                  card_header(bs_icon("folder-check"), "Colección deliberada (GIGO)"),
                  tags$ul(
                    class = "small",
                    tags$li(
                      "Define criterios de ", strong("inclusión/exclusión"),
                      " (cultivo, rango de años, tipo de diseño, idioma) y escríbelos explícitamente."
                    ),
                    tags$li(
                      "Prioriza revisiones, meta-análisis y artículos muy citados como ‘esqueleto’ del campo."
                    ),
                    tags$li(
                      "Separa en cuadernos/bibliotecas diferentes temas que podrían mezclar conceptos ",
                      "(ej. estrés hídrico vs. nutrición)."
                    )
                  )
                ),
                card(
                  card_header(bs_icon("diagram-3"), "Checklist mínima antes de pasar a RAG/NotebookLM"),
                  tags$ul(
                    class = "small",
                    tags$li("¿Tienes claro qué variables dependientes e independientes aparecen en los estudios?"),
                    tags$li("¿Sabes qué rangos de dosis, zonas o variedades se repiten?"),
                    tags$li("¿Has eliminado duplicados y trabajos fuera de alcance (otros cultivos, otras especies)?"),
                    tags$li("¿Documentaste en 3–4 líneas cómo construiste la biblioteca? Esto será tu ‘Métodos’.")
                  )
                )
              )
            )
          )
        )
      ),

      # -------------------------
      # 3) CONSULTAR
      # -------------------------
      nav_panel(
        title = tagList(
          span("3", class = "rag-step-number"),
          " Consultar"
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("3. Consultar: hacer preguntas que el RAG pueda responder bien"),
            card_body(
              p(
                "Aquí ya no preguntas ‘a la web’, sino a tu colección curada (NotebookLM, RAG interno). ",
                "El objetivo es obtener síntesis con citas, tablas comparativas y vacíos claros."
              ),
              layout_column_wrap(
                width = 1/2,
                fill  = FALSE,
                card(
                  card_header(bs_icon("list-ol"), "Patrones de pregunta útiles"),
                  tags$ol(
                    class = "small",
                    tags$li("“Con base en estas fuentes, resume metodologías para X y limita por especie/dosis.”"),
                    tags$li("“Tabla comparativa Autor A/B/C (año) con variable, diseño y efecto.”"),
                    tags$li("“Extrae métricas de rendimiento (kg/ha, °Brix, firmeza) bajo condición Y.”"),
                    tags$li("“¿Qué vacíos de investigación declaran en la sección Discusión?”")
                  )
                ),
                card(
                  card_header(bs_icon("file-earmark-text"), "Buenas prácticas de consulta"),
                  tags$ul(
                    class = "small",
                    tags$li("Siempre exige ", strong("citas in-line"), " y, si es posible, número de página."),
                    tags$li("Haz varias preguntas cortas en vez de una sola pregunta enorme y ambigua."),
                    tags$li("Pide que distinga claramente entre ‘lo que dicen las fuentes’ y ‘interpretación del modelo’."),
                    tags$li("Guarda las mejores respuestas como notas base para la redacción de tu marco teórico.")
                  )
                )
              )
            )
          )
        )
      ),

      # -------------------------
      # 4) IDEAR
      # -------------------------
      nav_panel(
        title = tagList(
          span("4", class = "rag-step-number"),
          " Idear"
        ),
        layout_column_wrap(
          width = 1,
          card(
            card_header("4. Idear: de la evidencia sintetizada al diseño experimental"),
            card_body(
              p(
                "En este paso usas el LLM como ‘co-investigador’ para proponer hipótesis, diseños y planes ",
                "de análisis, pero siempre partiendo de la síntesis citada de los pasos anteriores."
              ),
              layout_column_wrap(
                width = 1/2,
                fill  = FALSE,
                card(
                  card_header(bs_icon("pen"), "Plantilla de prompt (esqueleto)"),
                  tags$pre(
"Rol: Eres un investigador en [cultivo/zona] con foco en diseños experimentales.

Contexto: Basado en el resumen adjunto (con citas) sobre [tema],
propón:
1) 2–3 hipótesis contrastables,
2) un diseño experimental adecuado (factorial, bloques, parcelas divididas, etc.),
3) variables a medir y frecuencia,
4) un plan de análisis estadístico.

Restricciones:
- Cita explícitamente los artículos que soportan cada decisión.
- Si falta evidencia, indícalo y marca la recomendación como tentativa."
                  )
                ),
                card(
                  card_header(bs_icon("shield-check"), "Reglas para idear con responsabilidad"),
                  tags$ul(
                    class = "small",
                    tags$li("Nunca aceptes parámetros numéricos (dosis, tamaños muestrales) sin rastrear la cita."),
                    tags$li("Usa el modelo para generar alternativas, no para decidir por ti."),
                    tags$li("Documenta qué partes del diseño surgieron de la IA y cuáles se basan en evidencia dura."),
                    tags$li("Repite el ciclo: si aparece una idea prometedora, vuelve a ‘Descubrir/Curar’ para ver si hay respaldo empírico.")
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

# Pestaña 6: Guías: ResearchRabbit, NotebookLM & Rayyan.ai
pestanna6_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña6: Guías: ResearchRabbit, NotebookLM & Rayyan.ai",
    icon  = bs_icon("collection"),

    layout_column_wrap(
      width = 1,

      # --- Card 1: introducción didáctica ---
      card(
        card_header(
          class = "bg-primary text-white",
          "Cómo encajan estas herramientas en tu flujo de revisión"
        ),
        card_body(
          div(
            class = "tool-catalog-intro",
            p(
              "En esta pestaña tienes un catálogo curado de herramientas que cubren ",
              strong("todo el flujo de trabajo con literatura científica"),
              ": desde descubrir papers (ResearchRabbit, Litmaps, Semantic Scholar), ",
              "pasando por consulta con citas y grounding tipo RAG (NotebookLM, scite, Consensus), ",
              "hasta la gestión de referencias (Zotero, Paperpile) y el ",
              strong("screening sistemático"),
              " para revisiones (Rayyan)."
            ),
            tags$ul(
              class = "small",
              tags$li(
                strong("1) Descubrir: "),
                "mapas de citas, búsqueda semántica y grafos de literatura."
              ),
              tags$li(
                strong("2) Curar / organizar: "),
                "seleccionar qué entra a tu biblioteca con criterios claros."
              ),
              tags$li(
                strong("3) Consultar con citas: "),
                "aplicaciones tipo RAG que responden ",
                "citando tus PDFs o bases abiertas (NotebookLM, scite, Consensus)."
              ),
              tags$li(
                strong("4) Idear / escribir: "),
                "a partir de tu síntesis verificada, apoyarte en LLMs para redactar borradores, ",
                "siempre con referencias trazables."
              )
            ),
            div(
              class = "alert alert-info mt-2 mb-0 small",
              strong("Idea de uso: "),
              "elige primero la etapa del flujo (1–4), luego usa el catálogo para decidir ",
              "qué herramienta probar y con qué objetivo concreto."
            )
          )
        )
      ),

      # --- Card 2: catálogo interactivo + filtro ---
      card(
        card_header("Catálogo de herramientas IA para literatura"),
        card_body(
          p(
            class = "mb-2 tool-catalog-intro",
            "Filtra por la etapa del flujo de 4 pasos y explora el catálogo. ",
            "Las categorías indican si una herramienta sirve principalmente para ",
            strong("descubrir literatura, consultar con citas/grounding, gestionar referencias"),
            " o realizar ",
            strong("screening de revisiones sistemáticas"),
            " como en Rayyan."
          ),

          # Controles de filtrado por flujo
          div(
            class = "tool-catalog-controls mb-3",
            selectInput(
              ns("tool_step_filter"),
              label   = "Filtrar por etapa del flujo 4 pasos:",
              choices = c(
                "Todos",
                "1) Descubrir",
                "2) Curar / organizar",
                "3) Consultar con citas",
                "4) Idear / escribir"
              ),
              selected = "Todos",
              width    = "100%"
            )
          ),

          # Tabla interactiva
          div(
            class = "tool-table-wrapper",
            DT::dataTableOutput(ns("tbl_tools"))
          ),

          div(
            class = "mt-3",
            downloadButton(ns("tools_download"), "Descargar catálogo completo (.csv)")
          )
        )
      )
    )
  )
}

# Pestaña 7: El prompt perfecto
pestanna7_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña7: El prompt perfecto",
    icon  = bs_icon("magic"),

    # --- Fila 0: Principios de un buen prompt ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Principios de un “prompt perfecto” con LLM"),
        card_body(
          p(
            "Los LLM responden mucho mejor cuando el prompt sigue una estructura clara: ",
            strong("Rol + Contexto delimitado + Tarea + Formato + Restricciones"),
            ". Esta estructura es consistente con las guías de ",
            "prompt engineering de OpenAI y Anthropic (uso de rol, ejemplos y formato explícito)."
          ),
          tags$ul(
            class = "small prompt-principles-list",
            tags$li(
              strong("Rol:"),
              " indica quién debe ‘simular’ ser el modelo (p.ej., ",
              em("Fitopatólogo + estadístico de campo"),
              ")."
            ),
            tags$li(
              strong("Contexto delimitado:"),
              " pega la síntesis de NotebookLM o RAG y delimítala con marcas claras ",
              "(p.ej., bloque ```markdown ... ```)."
            ),
            tags$li(
              strong("Tarea bien definida:"),
              " qué quieres exactamente (hipótesis, diseño, crítica, tabla comparativa, etc.)."
            ),
            tags$li(
              strong("Formato de salida:"),
              " lista, tabla, código R comentado, informe breve, etc."
            ),
            tags$li(
              strong("Restricciones:"),
              " no inventar datos ni referencias; si algo no está en el contexto, decirlo; ",
              "pedir citar siempre que sea posible."
            ),
            tags$li(
              strong("Opcional: ejemplos (few-shot)"),
              " y pedir razonamiento paso a paso cuando sea útil."
            )
          )
        )
      )
    ),

    # --- Fila 1: Constructor + vista previa ---
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Constructor de prompts estructurados"),
        card_body(
          layout_column_wrap(
            width = 1/2,
            fill  = FALSE,
            textInput(
              ns("pr_rol"),
              "Rol",
              placeholder = "p.ej., Fitopatólogo + estadístico de campo"
            ),
            selectInput(
              ns("pr_idioma"),
              "Idioma de respuesta",
              choices  = c("Español", "Inglés", "Bilingüe (ES-EN)"),
              selected = "Español"
            ),
            selectInput(
              ns("pr_publico"),
              "Público objetivo",
              choices = c(
                "Investigador experto",
                "Estudiante de posgrado",
                "Equipo técnico de campo",
                "Tomador de decisiones no técnico"
              ),
              selected = "Investigador experto"
            ),
            selectInput(
              ns("pr_razonamiento"),
              "Modo de razonamiento",
              choices = c(
                "Paso a paso (Chain-of-Thought)",
                "Respuesta directa y concisa",
                "Lista estructurada / tabla"
              ),
              selected = "Paso a paso (Chain-of-Thought)"
            )
          ),

          hr(),

          textAreaInput(
            ns("pr_contexto"),
            "Contexto (desde NotebookLM / RAG)",
            placeholder = "Pega aquí la síntesis con citas o fragmentos relevantes…",
            rows = 6
          ),
          div(
            class = "prompt-helper-chip small text-muted",
            bs_icon("info-circle"),
            " Delimita el contexto con ```markdown ... ``` en el prompt generado para evitar que el modelo mezcle información externa."
          ),

          textAreaInput(
            ns("pr_tarea"),
            "Tarea",
            placeholder = "p.ej., 3 hipótesis; diseño DBCA; plan de análisis en R con supuestos y post-hoc…",
            rows = 4
          ),
          textInput(
            ns("pr_formato"),
            "Formato esperado",
            placeholder = "Lista numerada; tabla Markdown; código R comentado; resumen en 300 palabras…"
          ),
          textInput(
            ns("pr_restricciones"),
            "Restricciones adicionales (opcional)",
            placeholder = "No inventar refs; α=0.05; usar sólo las fuentes indicadas; etc."
          ),

          hr(),
          actionButton(ns("pr_reset"), "Limpiar", icon = bs_icon("trash")),
          downloadButton(ns("pr_descargar"), "Descargar prompt (.txt)")
        )
      ),

      card(
        card_header("Vista previa de prompt (para pegar en el LLM)"),
        card_body(
          div(
            class = "prompt-preview-wrapper",
            textAreaInput(
              ns("pr_preview"),
              label = NULL,
              value = "",
              rows  = 14
            )
          ),
          p(
            class = "small text-muted mt-2",
            bs_icon("lightning"),
            " Copia este prompt en tu herramienta (ChatGPT, Claude, NotebookLM, etc.). ",
            "Puedes ajustarlo manually antes de guardarlo como plantilla."
          )
        )
      )
    ),

    # --- Fila 2: Banco de prompts efectivos (ejemplos) ---
    layout_column_wrap(
      width = 1,
      card(
        card_header("Banco de prompts efectivos (plantillas)"),
        layout_column_wrap(
          width = 1/2,
          card(
            card_header("Ideación disciplinar (hipótesis)"),
            tags$pre(
              class = "prompt-snippet",
"Actúa como: Agrónomo de suelos y especialista en teledetección.

Contexto (citado, no inventar nada fuera de este bloque):
```markdown
(PEGA AQUÍ la síntesis con citas desde NotebookLM / RAG)
Tu tarea:
Propón 5 hipótesis comprobables que vinculen reflectancia de dosel con actividad microbiana en rizosfera.
Cada hipótesis debe indicar:
- Métrica de reflectancia o índice.
- Variable de suelo o microbiológica.
- Tipo de cultivo y entorno experimental.
- Cómo podría probarse (diseño general).

Formato esperado:
Lista numerada. Cada ítem con 4 viñetas: métrica, variable, cultivo/entorno, idea de diseño.

Restricciones:
- No inventes datos ni valores numéricos concretos.
- Si el contexto no aporta evidencia suficiente, indícalo explícitamente."
            )
          ),
          card(
            card_header("Crítica metodológica (estilo revisor)"),
            tags$pre(
            class = "prompt-snippet",
"Actúa como: Revisor de Plant and Soil, muy estricto en diseño y análisis estadístico.

Contexto (citado):
```markdown
(RESUMEN METODOLÓGICO con citas de los artículos clave)
Tu tarea:
- Identifica debilidades en muestreo, aleatorización, control de confusores y análisis estadístico.
- Clasifica cada hallazgo por riesgo (bajo, medio, alto) sobre la validez de las conclusiones.
- Propón mejoras concretas (diseño o análisis) para cada debilidad.

Formato esperado:
Tabla Markdown con columnas:
Aspecto | Hallazgo | Riesgo | Recomendación

Restricciones:
- No inventes información no presente en el contexto.
- Si un aspecto no está descrito, señala que está ‘no reportado’ en lugar de asumir."
            )
          ),
          card(
            card_header("Extracción estructurada / tablas comparativas"),
            tags$pre(
          class = "prompt-snippet",
"Actúa como: Asistente de revisión sistemática.

Contexto (citado):
```markdown
(ARTÍCULOS sobre Trichoderma en raíces; fragmentos de Resultados/Materiales y Métodos)
Tu tarea:
Construye una tabla comparativa con las columnas:
Especie/cepa | Dosis | Cultivo | Condición | Métrica | Efecto | Fuente (cita).

Formato esperado:
Tabla Markdown. Una fila por experimento o tratamiento.

Restricciones:
- Solo extrae datos que estén explícitos en el contexto.
- Si falta información para una columna, deja ‘ND’ (no disponible) sin inventar."
            )
          ),
          card(
            card_header("Hipótesis + diseño + análisis en R"),
            tags$pre(
          class = "prompt-snippet",
"Actúa como: Fitopatólogo + estadístico aplicado.

Contexto (citado, con vacíos identificados):
```markdown
(SÍNTESIS de resultados previos y lagunas de conocimiento)
Tu tarea:
- Propón 3 hipótesis específicas y comprobables.
- Diseña un DBCA (4 tratamientos × 5 repeticiones) adecuado para probarlas.
- Escribe código R comentado para:
  - Ajustar el modelo ANOVA.
  - Evaluar supuestos (residuos, homogeneidad).
  - Realizar comparaciones post-hoc apropiadas.

Formato esperado:
Lista numerada para las hipótesis y el diseño, seguida de un bloque de código R.

Restricciones:
- No inventes valores de respuesta ni resultados.
- Usa nombres de variables genéricos (p.ej., rendimiento, tratamiento)."
            )
          )
        )
      )
    )
  )
}

pestanna8_session1_v3UI <- function(ns) {
  nav_panel(
    title = "Pestaña8: Referencias bibliográficas",
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
      pestanna8_session1_v3UI(ns)
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

# Pestaña 3: Qué son las alucinaciones de un LLM
pestanna3_session1_v3_server <- function(input, output, session) {

  # Benchmark seleccionado en la sección de métricas de alucinación
  selected_bench <- reactiveVal("bench_truth")

  observeEvent(input$hall_bench_truth, {
    selected_bench("bench_truth")
  })
  observeEvent(input$hall_bench_consistency, {
    selected_bench("bench_consistency")
  })
  observeEvent(input$hall_bench_rag, {
    selected_bench("bench_rag")
  })

  output$hall_bench_detail <- renderUI({
    bench <- selected_bench()

    switch(
      bench,

      # 1) Veracidad del conocimiento (TruthfulQA y similares)
      bench_truth = tagList(
        h5(bs_icon("patch-check"), " 1. Veracidad del conocimiento (TruthfulQA y similares)"),
        p(
          "Evalúan si el modelo responde con la verdad cuando se le hacen ",
          "preguntas de cultura general o conocimiento del mundo, ",
          "en lugar de repetir mitos o creencias populares."
        ),
        tags$ul(
          class = "hall-type-list",
          tags$li(
            "Suelen usar formato de opción múltiple o preguntas abiertas con respuestas conocidas."
          ),
          tags$li(
            "Detectan si el modelo imita desinformación frecuente en la web ",
            "o corrige esos errores."
          ),
          tags$li(
            "Útiles para asistentes generales de preguntas y respuestas, ",
            "o para evaluar qué tanto puedes confiar en el modelo sin RAG."
          )
        )
      ),

      # 2) Robustez del prompt / ConsistencyAI
      bench_consistency = tagList(
        h5(bs_icon("diagram-3"), " 2. Robustez del prompt (consistencia entre personas)"),
        p(
          "Benchmarks como los de ", strong("ConsistencyAI"),
          " formulan la misma pregunta muchas veces, ",
          "cambiando solo la ‘persona’ o el estilo del usuario (edad, país, tono, etc.)."
        ),
        tags$ul(
          class = "hall-type-list",
          tags$li(
            "Se mide qué tan similares son las respuestas: ",
            "si cambian mucho según quién pregunta, el modelo es menos confiable."
          ),
          tags$li(
            "Se usan métricas de similitud entre textos (embedding/similarity) para cuantificar esa estabilidad."
          ),
          tags$li(
            "Importante para asistentes abiertos al público, chatbots corporativos ",
            "y uso en temas sensibles donde no queremos respuestas contradictorias."
          )
        )
      ),

      # 3) Fidelidad a documentos / RAG (FActScore, FCR)
      bench_rag = tagList(
        h5(bs_icon("file-earmark-text"), " 3. Fidelidad a documentos (FActScore, FCR en RAG)"),
        p(
          "Cuando usamos ", strong("Retrieval-Augmented Generation (RAG)"),
          ", el foco ya no es si el modelo ‘sabe’ algo por sí mismo, ",
          "sino si respeta la información de los documentos que le damos."
        ),
        tags$ul(
          class = "hall-type-list",
          tags$li(
            strong("FActScore: "),
            "divide la respuesta en hechos atómicos y comprueba cuántos están respaldados ",
            "por las fuentes proporcionadas."
          ),
          tags$li(
            strong("Factual Consistency Rate (FCR): "),
            "mide el porcentaje de frases o hechos que son consistentes con el contexto ",
            "en un sistema RAG; el complemento es la tasa de alucinación."
          ),
          tags$li(
            "Son métricas clave en entornos legales, médicos o financieros, ",
            "donde el modelo debe ceñirse a contratos, guías clínicas o políticas internas."
          )
        ),
        div(
          class = "hall-risk-callout small mt-2",
          strong("Idea práctica: "),
          "si tu caso de uso es RAG (p.ej., chatbot sobre tus PDFs), ",
          "te interesa sobre todo que el modelo tenga un FCR alto, ",
          "más que su ‘cultura general’ sin grounding."
        )
      )
    )
  })
}

# Pestaña 4: Cómo usar RAG / NotebookLM
pestanna4_session1_v3_server <- function(input, output, session) {

  # Etapa seleccionada del pipeline RAG / NotebookLM
  selected_rag_step <- reactiveVal("step_sources")

  observeEvent(input$rag_step_sources,   { selected_rag_step("step_sources")   })
  observeEvent(input$rag_step_index,     { selected_rag_step("step_index")     })
  observeEvent(input$rag_step_retrieval, { selected_rag_step("step_retrieval") })
  observeEvent(input$rag_step_generation,{ selected_rag_step("step_generation")})
  observeEvent(input$rag_step_sharing,   { selected_rag_step("step_sharing")   })

  output$rag_step_detail <- renderUI({
    step <- selected_rag_step()

    switch(
      step,

      # 1) Subir y seleccionar fuentes
      step_sources = tagList(
        h5(bs_icon("cloud-arrow-up"), " 1. Subir y seleccionar fuentes"),
        p(
          "Creas un cuaderno y eliges qué documentos formarán parte del ‘mini-corpus’ ",
          "sobre el que quieres trabajar."
        ),
        tags$ul(
          class = "small",
          tags$li("Puedes combinar PDFs, Google Docs, Slides y notas propias."),
          tags$li(
            "Es clave curar bien la biblioteca: revisiones, métodos, ensayos clave, ",
            "protocolos y reportes que realmente quieras que el modelo cite."
          ),
          tags$li(
            "Todo lo que no esté aquí simplemente no podrá ser citado por NotebookLM."
          )
        )
      ),

      # 2) Indexación y comprensión
      step_index = tagList(
        h5(bs_icon("database-gear"), " 2. Indexación y comprensión de las fuentes"),
        p(
          "NotebookLM convierte los documentos en texto, extrae estructura y genera ",
          "representaciones numéricas (embeddings) para poder buscar a nivel de fragmentos."
        ),
        tags$ul(
          class = "small",
          tags$li(
            "Crea una especie de ‘mapa semántico’ de tus fuentes: conceptos cercanos ",
            "quedan cerca en el espacio vectorial."
          ),
          tags$li(
            "Puede generar resúmenes, glosarios o fichas por documento para ayudarte a ",
            "orientarte antes de hacer preguntas profundas."
          ),
          tags$li(
            "Si una fuente está mal escaneada o tiene poco texto legible, la calidad de la ",
            "indexación baja y aumentan las alucinaciones."
          )
        )
      ),

      # 3) Pregunta + recuperación
      step_retrieval = tagList(
        h5(bs_icon("search"), " 3. Pregunta + recuperación de fragmentos"),
        p(
          "Cuando haces una pregunta, NotebookLM la convierte en un embedding y ",
          "busca en el índice los pasajes más relevantes antes de llamar al LLM."
        ),
        tags$ul(
          class = "small",
          tags$li(
            "La recuperación puede usar búsqueda semántica (vector search) y filtros por fuente ",
            "o capítulo."
          ),
          tags$li(
            "Sólo los fragmentos recuperados se pasan al modelo como contexto; ",
            "esto es el núcleo del enfoque ",
            strong("Retrieval-Augmented Generation (RAG).")
          ),
          tags$li(
            "Si la pregunta está fuera de lo que cubren las fuentes, es esperable que ",
            "las respuestas sean vagas o que el sistema te invite a aportar más contexto."
          )
        )
      ),

      # 4) Respuesta con citas
      step_generation = tagList(
        h5(bs_icon("card-text"), " 4. Respuesta con citas y comprobación"),
        p(
          "El LLM genera la respuesta condicionada por los fragmentos recuperados ",
          "y añade ", strong("citas in-line"), " que apuntan a las fuentes originales."
        ),
        tags$ul(
          class = "small",
          tags$li(
            "Cada cita debería corresponder a un pasaje concreto del documento; ",
            "puedes abrirlo y verificar que el modelo no esté reinterpretando en exceso."
          ),
          tags$li(
            "En sistemas RAG bien configurados, la ",
            strong("tasa de consistencia factual (FCR)"),
            " entre respuesta y documentos supera habitualmente el 95 %. ",
            "Si ves muchas frases sin cita, trátalas como hipótesis, no como hechos."
          ),
          tags$li(
            "Tu rol es auditar: aceptar lo que está bien sustentado, corregir o descartar ",
            "lo que no esté respaldado o mezcle varias fuentes de forma dudosa."
          )
        )
      ),

      # 5) Síntesis y compartición
      step_sharing = tagList(
        h5(bs_icon("share"), " 5. Síntesis, comunicación y registro"),
        p(
          "Una vez que confías en las respuestas, puedes usar NotebookLM para generar ",
          "salidas adaptadas a distintos públicos."
        ),
        tags$ul(
          class = "small",
          tags$li(
            "Resúmenes ejecutivos, guías de estudio, listas de pasos o checklists, ",
            "respetando siempre las citas clave."
          ),
          tags$li(
            em("Audio overview"),
            " y explicaciones en lenguaje sencillo para direcciones o equipos no técnicos."
          ),
          tags$li(
            "Exportar o copiar los textos a tu gestor de referencias, cuaderno de laboratorio ",
            "o reporte estadístico, documentando qué partes vienen del modelo y qué partes ",
            "son tu análisis."
          )
        ),
        div(
          class = "alert alert-success small mt-2",
          strong("Buen hábito: "),
          "cuando algo pase de NotebookLM a un informe o artículo, asegúrate de que ",
          "al menos una cita apunte al documento original que respalda esa afirmación."
        )
      )
    )
  })
}

# Pestaña 5: Flujo 4 pasos para búsquedas con IA
pestanna5_session1_v3_server <- function(input, output, session) {
  ns <- session$ns

  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }

  # Construir cadena booleana
  observeEvent(input$kw_build, {
    core <- trimws(input$kw_core %||% "")
    syn  <- trimws(input$kw_syn %||% "")
    meth <- trimws(input$kw_method %||% "")
    filt <- trimws(input$kw_filter %||% "")

    syn_vec <- unlist(strsplit(syn, ";"))
    syn_vec <- trimws(syn_vec)

    syn_part <- if (length(syn_vec) && any(nzchar(syn_vec))) {
      paste0(
        "(",
        paste(syn_vec[nzchar(syn_vec)], collapse = " OR "),
        ")"
      )
    } else {
      ""
    }

    parts <- c(core, syn_part, meth)
    parts <- parts[nzchar(parts)]

    q <- paste(parts, collapse = " AND ")
    if (nzchar(filt)) {
      q <- paste(q, filt)
    }

    updateTextAreaInput(session, "kw_preview", value = q)
  })

  # Descargar cadena .txt
  output$kw_download <- downloadHandler(
    filename = function() {
      paste0("busqueda_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    },
    content = function(file) {
      writeLines(input$kw_preview %||% "", con = file, useBytes = TRUE)
    }
  )
}

# Pestaña 6: Guías: ResearchRabbit, NotebookLM & Rayyan.ai
pestanna6_session1_v3_server <- function(input, output, session) {

  # Catálogo base (para exportar y para filtrado)
  tools_df <- reactive({
    data.frame(
      Categoria = c(
        # Descubrir
        "Descubrir", "Descubrir", "Descubrir", "Descubrir", "Descubrir", "Descubrir",
        # Consultar con citas / grounding
        "Consultar con citas", "Consultar con citas", "Consultar con citas",
        # Gestión bibliográfica
        "Gestión bibliográfica", "Gestión bibliográfica",
        # Screening sistemático
        "Screening"
      ),
      Herramienta = c(
        "ResearchRabbit", "Litmaps", "Connected Papers",
        "Semantic Scholar", "OpenAlex", "Lens.org",
        "NotebookLM", "scite (smart citations)", "Consensus",
        "Zotero", "Paperpile", "Rayyan"
      ),
      Descripcion = c(
        # Descubrir
        "Mapas interactivos de artículos y autores para descubrir literatura relacionada y seguir nuevas publicaciones.",
        "Descubrimiento basado en citas: mapas temporales de literatura y alertas de nuevos trabajos en tu red de papers.",
        "Grafo de trabajos relacionados por similitud de citas; ayuda a ver familias de artículos alrededor de un paper semilla.",
        "Buscador semántico de artículos científicos con sugerencias de trabajos relacionados.",
        "Índice abierto de metadatos académicos (autores, revistas, citas) para análisis y descubrimiento a gran escala.",
        "Metabuscador de patentes y publicaciones con métricas de impacto e inteligencia tecnológica.",
        # Consultar con citas
        "Cuadernos tipo RAG: subes tus PDFs/notas y haces preguntas; responde usando esos documentos y entregando citas in-line.",
        "‘Smart citations’: clasifica si una cita apoya, contradice o solo menciona un paper; ideal para evaluar solidez de evidencia.",
        "Buscador IA que responde preguntas a partir de artículos científicos, devolviendo frases clave con cita al paper de origen.",
        # Gestión bibliográfica
        "Gestor de referencias libre: captura web, anotación de PDFs y generación de bibliografías.",
        "Gestor de referencias en la nube integrado con Google Docs/Drive; pensado para flujos 100%% web.",
        # Screening sistemático
        "Plataforma de cribado rápido para revisiones sistemáticas y scoping reviews; permite etiquetar, resolver conflictos y exportar decisiones."
      ),
      URL = c(
        "https://www.researchrabbitapp.com/",
        "https://www.litmaps.com/",
        "https://www.connectedpapers.com/",
        "https://www.semanticscholar.org/",
        "https://openalex.org/",
        "https://www.lens.org/",
        "https://notebooklm.google/",
        "https://scite.ai/",
        "https://consensus.app/",
        "https://www.zotero.org/",
        "https://paperpile.com/",
        "https://www.rayyan.ai/"
      ),
      # Etapa(s) principal(es) del flujo 4 pasos
      Flujo_4_pasos = c(
        # Descubrir
        "1) Descubrir",
        "1) Descubrir",
        "1) Descubrir",
        "1) Descubrir",
        "1) Descubrir",
        "1) Descubrir",
        # Consultar con citas / grounding
        "2) Curar / organizar; 3) Consultar con citas; 4) Idear / escribir",
        "3) Consultar con citas",
        "3) Consultar con citas",
        # Gestión bibliográfica
        "2) Curar / organizar; 4) Idear / escribir",
        "2) Curar / organizar; 4) Idear / escribir",
        # Screening sistemático
        "2) Curar / organizar"
      ),
      stringsAsFactors = FALSE
    )
  })

  # Filtrado por etapa del flujo 4 pasos
  tools_filtered <- reactive({
    df   <- tools_df()
    step <- input$tool_step_filter

    if (is.null(step) || identical(step, "Todos")) {
      return(df)
    }

    df[grepl(step, df$Flujo_4_pasos, fixed = TRUE), , drop = FALSE]
  })

  # Tabla interactiva
  output$tbl_tools <- DT::renderDataTable({
    df <- tools_filtered()

    # Columnas “bonitas” para mostrar (HTML)
    df_display <- df

    # Chips de categoría
    cat_key <- tolower(gsub("[ /]+", "-", df_display$Categoria))
    df_display$Categoria <- sprintf(
      "<span class='tool-cat tool-cat-%s'>%s</span>",
      cat_key,
      df$Categoria
    )

    # Herramienta como link clicable
    df_display$Herramienta <- sprintf(
      "<a href='%s' target='_blank' rel='noopener noreferrer'>%s</a>",
      df$URL,
      df$Herramienta
    )

    # Flujo como badge
    df_display$Flujo_4_pasos <- sprintf(
      "<span class='tool-step-badge'>%s</span>",
      df$Flujo_4_pasos
    )

    # No mostramos la URL en la tabla (ya va en el link)
    df_display$URL <- NULL

    DT::datatable(
      df_display,
      rownames = FALSE,
      filter   = "top",
      escape   = FALSE,
      options  = list(
        pageLength = 10,
        dom        = "ftip",
        autoWidth  = TRUE,
        columnDefs = list(
          list(className = "dt-left", targets = "_all")
        ),
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ filas",
          info = "Mostrando _START_–_END_ de _TOTAL_ herramientas",
          paginate = list(
            previous = "Anterior",
            `next`   = "Siguiente"
          )
        )
      )
    )
  })

  # Descarga CSV del catálogo completo (sin HTML)
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
        row.names    = FALSE,
        fileEncoding = "UTF-8"
      )
    }
  )
}

# Pestaña 7: El prompt perfecto
pestanna7_session1_v3_server <- function(input, output, session) {
  ns <- session$ns
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

  prompt_r <- reactive({
    rol         <- input$pr_rol           %||% ""
    ctx_raw     <- input$pr_contexto      %||% ""
    tarea       <- input$pr_tarea         %||% ""
    forma       <- input$pr_formato       %||% ""
    restr_extra <- input$pr_restricciones %||% ""
    idioma      <- input$pr_idioma        %||% ""
    publico     <- input$pr_publico       %||% ""
    razonamiento<- input$pr_razonamiento  %||% ""

    # Bloque de contexto delimitado
    ctx_block <- if (shiny::isTruthy(ctx_raw)) {
      paste0(
        "Contexto (información con citas; NO inventar nada fuera de este bloque):\n",
        "```markdown\n",
        ctx_raw,
        "\n```"
      )
    } else {
      NULL
    }

    # Línea de idioma, público y modo de razonamiento
    idioma_line <- if (shiny::isTruthy(idioma) || shiny::isTruthy(publico) || shiny::isTruthy(razonamiento)) {
      idioma_text <- switch(
        idioma,
        "Español"          = "Responde en español claro.",
        "Inglés"           = "Responde en inglés claro.",
        "Bilingüe (ES-EN)" = "Responde primero en español y luego en inglés."
      )

      razonamiento_text <- switch(
        razonamiento,
        "Paso a paso (Chain-of-Thought)" = "Expón tu razonamiento paso a paso antes de dar la respuesta final.",
        "Respuesta directa y concisa"    = "Prioriza una respuesta directa y concisa, sin pasos intermedios largos.",
        "Lista estructurada / tabla"     = "Organiza la respuesta como lista estructurada o tabla, según corresponda."
      )

      paste0(
        "Público objetivo: ", publico, ".\n",
        idioma_text, "\n",
        razonamiento_text
      )
    } else {
      NULL
    }

    # Restricciones base recomendadas
    restr_base <- c(
      "- Si la información solicitada no está en el contexto citado, indícalo explícitamente y NO la inventes.",
      "- No inventes datos numéricos, nombres de autores ni referencias bibliográficas.",
      "- Siempre que cites resultados o afirmaciones, intenta vincularlos a la fuente correspondiente dentro del contexto."
    )

    if (shiny::isTruthy(restr_extra)) {
      restr_all <- c(restr_base, paste0("- ", restr_extra))
    } else {
      restr_all <- restr_base
    }

    restr_block <- paste(
      c("Restricciones:", restr_all),
      collapse = "\n"
    )

    parts <- c(
      if (shiny::isTruthy(rol))   paste0("Actúa como: ", rol, ".") else NULL,
      ctx_block,
      if (shiny::isTruthy(tarea)) paste0("Tu tarea principal:\n", tarea) else NULL,
      if (shiny::isTruthy(forma)) paste0("Formato esperado de la respuesta:\n", forma) else NULL,
      idioma_line,
      restr_block,
      "Si algo no está claro en la instrucción, pide una aclaración antes de responder."
    )

    paste(parts, collapse = "\n\n")
  })

  # Actualizar vista previa cuando cambia cualquier campo relevante
  observeEvent(
    list(
      input$pr_rol,
      input$pr_contexto,
      input$pr_tarea,
      input$pr_formato,
      input$pr_restricciones,
      input$pr_idioma,
      input$pr_publico,
      input$pr_razonamiento
    ),
    ignoreInit = TRUE,
    handlerExpr = {
      updateTextAreaInput(session, "pr_preview", value = prompt_r())
    }
  )

  # Botón para limpiar todos los campos
  observeEvent(input$pr_reset, {
    updateTextInput(session, "pr_rol",           value = "")
    updateTextAreaInput(session, "pr_contexto",  value = "")
    updateTextAreaInput(session, "pr_tarea",     value = "")
    updateTextInput(session, "pr_formato",       value = "")
    updateTextInput(session, "pr_restricciones", value = "")
    updateSelectInput(session, "pr_idioma",      selected = "Español")
    updateSelectInput(session, "pr_publico",     selected = "Investigador experto")
    updateSelectInput(session, "pr_razonamiento",selected = "Paso a paso (Chain-of-Thought)")
    updateTextAreaInput(session, "pr_preview",   value = "")
  })

  # Descarga del prompt como .txt
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

# ---- Server principal del módulo ------------------------------

session1_v3Server <- function(input, output, session) {

  # Pestaña1 (Qué es la IA)
  pestanna1_session1_v3_server(input, output, session)

  # Pestaña2 (Qué es un LLM)
  pestanna2_session1_v3_server(input, output, session)

  # Pestaña3 (Alucinación)
  pestanna3_session1_v3_server(input, output, session)

  # Pestaña4 (RAG / NotebookLM)
  pestanna4_session1_v3_server(input, output, session)

  # Pestaña5: Flujo 4 pasos (subpestaña Descubrir)
  pestanna5_session1_v3_server(input, output, session)

  # Pestaña6: Catálogo herramientas
  pestanna6_session1_v3_server(input, output, session)

  # Pestaña7: Prompt perfecto
  pestanna7_session1_v3_server(input, output, session)
}
