# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session6.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto (Strip-Plot)
pestanna1_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto",
    
    # Título general
    h4(class = "section-header", "¿Dónde encaja el strip-plot en la historia del curso?"),
    
    # Bloque contextual: RCBD -> Split-plot -> Strip-plot
    div(
      class = "alert alert-info",
      tags$p(
        strong("Idea general: "),
        "en este bloque avanzado estamos refinando la forma de modelar ",
        em("la estructura de error"),
        " cuando el manejo de los factores se vuelve más complejo."
      ),
      tags$ul(
        tags$li(
          strong("Sesión 4 – RCBD + LMM: "),
          "un solo estrato de error residual (",
          code("Error residual"), ")."
        ),
        tags$li(
          strong("Sesión 5 – Split-plot: "),
          "dos unidades experimentales (parcela principal y subparcela) ",
          "→ dos errores (",
          code("Error A"),
          " y ",
          code("Error B"),
          ")."
        ),
        tags$li(
          strong("Sesión 6 – Strip-plot: "),
          "dos sistemas de franjas (filas y columnas) cruzados en cada bloque ",
          "→ tres estratos de error (",
          code("Error(a), Error(b), Error(c)"),
          ")."
        )
      ),
      tags$p(
        class = "text-muted small",
        "Mnemotecnia: RCBD → 1 error; Split-plot → 2 errores; Strip-plot → 3 errores."
      )
    ),
    
    # Resultados de aprendizaje específicos
    h4("Resultados de aprendizaje de esta sesión"),
    tags$ol(
      tags$li(
        "Ser capaz de describir el ",
        strong("layout de un strip-plot"),
        ": bloque, franjas A (verticales), franjas B (horizontales) y celdas A×B."
      ),
      tags$li(
        "Entender que la ",
        strong("interacción A×B"),
        " es la estimación más precisa y el foco central del diseño."
      ),
      tags$li(
        "Reconocer que el análisis correcto requiere ",
        strong("tres errores separados"),
        ": uno para A, otro para B y otro para A×B."
      )
    ),
    
    tags$hr(),
    
    # Dos columnas: puente conceptual + ejemplo ancla agronómico
    fluidRow(
      # Columna izquierda: puente y comparación con split-plot
      column(
        width = 6,
        h5("¿Cuándo tiene sentido hablar de strip-plot y por qué no es un split-plot \"al revés\"?"),
        tags$ul(
          tags$li(
            "En un ",
            strong("split-plot"),
            " hay una unidad grande (parcela principal) y dentro de ella subparcelas: ",
            "el factor de subparcela es el más preciso."
          ),
          tags$li(
            "En un ",
            strong("strip-plot"),
            " no hay jerarquía de parcelas; hay ",
            em("dos sistemas de franjas"),
            " que se cruzan en cada bloque (A en vertical, B en horizontal)."
          ),
          tags$li(
            "Eso genera ",
            strong("tres fuentes de variación aleatoria"),
            ": variación entre franjas A dentro de bloque (Error(a)), ",
            "variación entre franjas B dentro de bloque (Error(b)) y variación dentro de celdas A×B (Error(c))."
          ),
          tags$li(
            strong("Consecuencia clave: "),
            "la ",
            em("interacción A×B"),
            " se estima con el error más fino (Error(c)), por lo que suele ser la más precisa."
          )
        ),
        div(
          class = "note-cloud",
          tags$p(
            strong("Mensaje didáctico: "),
            "elegimos un diseño en franjas cuando ",
            strong("ambos factores son difíciles de cambiar"),
            " y la pregunta científica principal es ",
            em("cómo se comporta la combinación A×B"),
            " (la interacción), más que los efectos principales por separado."
          )
        )
      ),
      
      # Columna derecha: modelo y ejemplo ancla agronómico
      column(
        width = 6,
        h5("Ejemplo ancla agronómico: Labranza × Riego"),
        tags$ul(
          tags$li(
            strong("A = Labranza (franjas verticales): "),
            "p.ej., Cincel, Disco, Siembra directa."
          ),
          tags$li(
            strong("B = Riego (franjas horizontales): "),
            "p.ej., Goteo, Aspersión, Surco."
          ),
          tags$li(
            strong("Bloque (RCBD): "),
            "repetimos el sistema de franjas A×B en varios bloques del campo."
          ),
          tags$li(
            strong("Y = respuesta agronómica: "),
            "rendimiento, infiltración, humedad del suelo, etc., medida en cada celda A×B."
          )
        ),
        br(),
        h5("Modelo y estructura de error (visión matemática suave)"),
        withMathJax(
          helpText(
            "Un modelo típico (RCBD por bloques) se puede escribir como:",
            "$$Y_{ijk} = \\mu + R_k + \\alpha_i + \\gamma_{ik} + \\beta_j + \\Theta_{jk} + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
          ),
          tags$ul(
            tags$li(
              HTML(
                "<b>Error (a) / &gamma;<sub>ik</sub></b>: variación entre franjas A dentro del bloque ",
                "&rarr; se usa para probar el efecto de A (labranza)."
              )
            ),
            tags$li(
              HTML(
                "<b>Error (b) / &Theta;<sub>jk</sub></b>: variación entre franjas B dentro del bloque ",
                "&rarr; se usa para probar el efecto de B (riego)."
              )
            ),
            tags$li(
              HTML(
                "<b>Error (c) / &epsilon;<sub>ijk</sub></b>: variación dentro de las celdas A×B ",
                "&rarr; se usa para probar la interacción A&times;B."
              )
            )
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Croquis mental final (resumen visual en palabras)
    h5("Croquis mental de un strip-plot por bloque"),
    tags$p(
      "Imagina cada bloque como una malla: ",
      strong("franjas verticales (A)"),
      " × ",
      strong("franjas horizontales (B)"),
      " que se cruzan para formar celdas A×B. ",
      "En las siguientes pestañas veremos cómo el análisis de varianza separa esos tres errores: ",
      code("Error(a) para A"), ", ", code("Error(b) para B"), 
      " y ", code("Error(c) para A×B"), 
      ", y por qué la interacción es la más precisa."
    ),
    br()
  )
}

# Pestaña 2: Conceptos clave: franjas y tres errores
pestanna2_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave: franjas y errores",
    
    # Título general
    h4(class = "section-header", "Franjas A y B dentro de cada bloque → tres estratos de error"),
    
    # Bloque 1: Croquis mental por bloque
    div(
      class = "alert alert-info",
      h5("Croquis mental por bloque"),
      p(
        "Imagina un solo bloque. Dentro del bloque tenemos:"
      ),
      tags$ul(
        tags$li(
          strong("Filas / franjas verticales (Factor A): "),
          "cada franja tiene un nivel fijo de A (p.ej., tipo de labranza)."
        ),
        tags$li(
          strong("Columnas / franjas horizontales (Factor B): "),
          "cada franja tiene un nivel fijo de B (p.ej., sistema de riego)."
        ),
        tags$li(
          strong("Celdas A×B: "),
          "en cada intersección fila×columna medimos la respuesta ", code("Y"),
          " (rendimiento, infiltración, etc.)."
        )
      ),
      p(
        "Dentro de cada bloque se forma una grilla A×B. ",
        "Las franjas A comparten el mismo nivel de A a lo largo del bloque; ",
        "las franjas B comparten el mismo nivel de B. ",
        "Las celdas A×B son las unidades donde realmente observamos datos."
      )
    ),
    
    tags$hr(),
    
    # Bloque 2: Modelo y significado de cada término
    h4("Modelo con tres errores (visión simplificada)"),
    withMathJax(
      helpText(
        "Un modelo típico (RCBD por bloques) para strip-plot se puede escribir como:",
        "$$Y_{ijk} = \\mu + R_k + \\alpha_i + \\gamma_{ik} + \\beta_j + \\Theta_{jk} + (\\alpha\\beta)_{ij} + \\epsilon_{ijk}$$"
      )
    ),
    p("Interpretación de cada término en lenguaje de franjas:"),
    tags$ul(
      tags$li(
        strong("\\(R_k\\) – efecto de bloque: "),
        "captura diferencias globales entre bloques (p.ej., loma vs. bajo, gradiente de suelo). ",
        em("A menudo se trata como efecto aleatorio.")
      ),
      tags$li(
        strong("\\(\\alpha_i\\) – efecto del nivel i de A: "),
        "representa la diferencia sistemática entre franjas verticales de A ",
        "(p.ej., labranza Cincel vs. Disco vs. Directa)."
      ),
      tags$li(
        strong("\\(\\gamma_{ik}\\) – variación de la franja A dentro del bloque k: "),
        "mide cómo fluctúa la franja A alrededor de su promedio dentro de cada bloque. ",
        "Este término define el ", strong("Error(a)"), ": variación entre franjas A dentro de bloque."
      ),
      tags$li(
        strong("\\(\\beta_j\\) – efecto del nivel j de B: "),
        "representa la diferencia sistemática entre franjas horizontales de B ",
        "(p.ej., riego por Goteo vs. Aspersión)."
      ),
      tags$li(
        strong("\\(\\Theta_{jk}\\) – variación de la franja B dentro del bloque k: "),
        "mide cómo fluctúa la franja B alrededor de su promedio dentro de cada bloque. ",
        "Este término define el ", strong("Error(b)"), ": variación entre franjas B dentro de bloque."
      ),
      tags$li(
        strong("\\((\\alpha\\beta)_{ij}\\) – interacción A×B: "),
        "captura cómo cambia el efecto de A según el nivel de B (y viceversa). ",
        "En strip-plot, este suele ser el foco principal."
      ),
      tags$li(
        strong("\\(\\epsilon_{ijk}\\) – error dentro de la celda A×B: "),
        "variación residual entre observaciones dentro de la misma combinación A×B y bloque. ",
        "Este define el ", strong("Error(c)"), ": variación entre celdas A×B dentro de bloque."
      )
    ),
    
    tags$hr(),
    
    # Bloque 3: Tabla "quién usa qué error"
    h4("¿Qué error usamos para cada efecto?"),
    p(
      "En un ANOVA de franjas correcto, cada efecto se prueba con su estrato de error correspondiente. ",
      "La siguiente tabla resume la lógica:"
    ),
    tableOutput(ns("tabla_errores_strip")),
    
    tags$p(
      class = "text-muted small",
      "Recordatorio: los símbolos Ea, Eb y Ec se refieren a los cuadrados medios (MS) ",
      "de cada estrato de error; gl.a, gl.b y gl.c son sus grados de libertad."
    ),
    
    tags$hr(),
    
    # Bloque 4: Comparación explícita con split-plot
    h4("Comparación explícita: Split-Plot vs Strip-Plot"),
    tags$ul(
      tags$li(
        strong("Split-Plot (Sesión 5): "),
        "el factor de subparcela y la interacción usan el mismo error (Error B). ",
        "Tenemos dos estratos de error principales: Error A (whole-plot) y Error B (subparcela)."
      ),
      tags$li(
        strong("Strip-Plot (Sesión 6): "),
        "los tres efectos (A, B y A×B) tienen cada uno su propio error: ",
        strong("Error(a) para A"), ", ",
        strong("Error(b) para B"), " y ",
        strong("Error(c) para A×B"), ". ",
        "Eso significa ", em("tres precisiones distintas"), " en el mismo experimento."
      )
    ),
    div(
      class = "alert alert-secondary",
      p(
        strong("Mensaje didáctico: "),
        "En diseños en franjas, la interacción A×B se beneficia del error más pequeño (Error(c)), ",
        "por eso es típicamente el efecto más preciso. ",
        "Las pruebas de A y de B son menos precisas porque usan errores a nivel de franjas completas ",
        "(Error(a) y Error(b))."
      )
    )
  )
}

# Pestaña 3: Diseño & libro de campo
pestanna3_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & libro de campo",
    
    h4(class = "section-header",
       "Del diseño conceptual al libro de campo (strip-plot)"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Objetivo de esta pestaña:"),
        " pasar del croquis mental de franjas A (vertical) y franjas B (horizontal)",
        " a un ", strong("libro de campo aleatorizado"),
        " que puedes exportar y usar en campo."
      ),
      tags$ul(
        tags$li("Elegimos los niveles de ", strong("A (franjas verticales)"),
                " y ", strong("B (franjas horizontales"), ")."),
        tags$li("Definimos el número de ", strong("bloques (r)"),
                " (réplicas del diseño)."),
        tags$li("Generamos el diseño con ", code("agricolae::design.strip()"),
                " y visualizamos el fieldbook y el croquis por bloque.")
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("1) Definir factores y bloques"),
        helpText(
          "Piensa en A como franjas verticales (p.ej. distintos tipos de labranza) ",
          "y en B como franjas horizontales (p.ej. distintos sistemas de riego)."
        ),
        textInput(
          ns("niveles_A_txt"),
          "Niveles del Factor A (vertical, franjas):",
          value = "Cincel,Disco,Directa"
        ),
        textInput(
          ns("niveles_B_txt"),
          "Niveles del Factor B (horizontal, franjas):",
          value = "Goteo,Aspersion"
        ),
        numericInput(
          ns("r_bloques"), "Número de bloques (r):",
          value = 4, min = 2, step = 1
        ),
        checkboxInput(
          ns("usar_semilla"),
          "Usar semilla (para que el diseño sea reproducible)",
          value = TRUE
        ),
        numericInput(
          ns("semilla"), "Semilla:",
          value = 123, min = 1, step = 1
        ),
        actionButton(
          ns("btn_generar_diseno"),
          "Generar diseño strip-plot",
          class = "btn btn-primary w-100 mt-2"
        ),
        hr(),
        h5("2) Exportar"),
        helpText(
          "Cuando el diseño esté listo, podrás descargar el ",
          strong("fieldbook en CSV"),
          " para usarlo en Excel o en tu cuaderno de campo."
        ),
        uiOutput(ns("ui_descargar_book"))
      ),
      
      mainPanel(
        width = 8,
        
        h5("Resumen del diseño generado"),
        uiOutput(ns("resumen_diseno")),
        
        hr(),
        h5("Libro de campo (fieldbook)"),
        p(
          class = "text-muted small",
          "Esta tabla muestra, para cada observación, el ",
          strong("bloque"), ", el nivel de ",
          strong("A (franja vertical)"), " y el nivel de ",
          strong("B (franja horizontal)"),
          ". En otras palabras, cada fila es una ",
          strong("celda A×B dentro de un bloque"),
          " donde se medirá la respuesta."
        ),
        DT::dataTableOutput(ns("tabla_book")),
        
        hr(),
        h5("Croquis conceptual por bloque (A × B)"),
        p(
          class = "text-muted small",
          strong("Eje Y:"),
          " niveles del Factor A (franjas verticales). ",
          strong("Eje X:"),
          " niveles del Factor B (franjas horizontales). ",
          "Cada faceta representa un bloque (réplica completa del diseño)."
        ),
        plotOutput(ns("plot_croquis"), height = "420px")
      )
    )
  )
}

# Pestaña 4: ANOVA en franjas (strip-plot)
pestanna4_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) ANOVA en franjas",
    
    h4(class = "section-header", "ANOVA en franjas: tres tablas, tres errores"),
    
    p(
      "En un diseño en franjas (strip-plot) analizamos los datos con ",
      code("agricolae::strip.plot()"), ". El resultado clave es un ",
      strong("ANOVA con tres tablas"),
      ": una para A (franjas verticales), otra para B (franjas horizontales) ",
      "y otra para la interacción A×B, cada una con su propio estrato de error."
    ),
    
    div(
      class = "alert alert-info",
      tags$ul(
        tags$li(
          strong("Recordatorio conceptual (de la pestaña 2):")
        ),
        tags$li(
          HTML(
            "<b>A (franjas verticales):</b> se prueba con el ",
            "<b>Error(a)</b> → MS = Ea, gl = gl.a."
          )
        ),
        tags$li(
          HTML(
            "<b>B (franjas horizontales):</b> se prueba con el ",
            "<b>Error(b)</b> → MS = Eb, gl = gl.b."
          )
        ),
        tags$li(
          HTML(
            "<b>Interacción A×B (celdas):</b> se prueba con el ",
            "<b>Error(c)</b> → MS = Ec, gl = gl.c."
          )
        )
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        
        h5("1. Elegir fuente de datos"),
        radioButtons(
          ns("fuente_datos"),
          label = "Fuente de datos para el ANOVA:",
          choices = c(
            "Ejemplo de la librería: agricolae::plotted" = "ejemplo",
            "Simular datos a partir del diseño (P3)"      = "sim_diseno"
          )
        ),
        
        conditionalPanel(
          condition = sprintf("input['%s'] == 'sim_diseno'", ns("fuente_datos")),
          tags$hr(),
          h5("2. Opciones de simulación (caso didáctico)"),
          helpText(
            "Estas opciones solo se usan cuando eliges ",
            strong("Simular a partir del diseño"),
            ". Permiten controlar los efectos medios "
          ),
          numericInput(
            ns("media_base"),
            label = HTML("Media base (&mu;):"),
            value = 100, step = 1
          ),
          numericInput(
            ns("efecto_A_amp"),
            label = HTML("Amplitud efecto A (±):"),
            value = 8, step = 0.5
          ),
          numericInput(
            ns("efecto_B_amp"),
            label = HTML("Amplitud efecto B (±):"),
            value = 6, step = 0.5
          ),
          numericInput(
            ns("efecto_AB_amp"),
            label = HTML("Amplitud interacción A&times;B (±):"),
            value = 4, step = 0.5
          ),
          tags$hr(),
          h6("Desviaciones estándar por estrato de error"),
          numericInput(
            ns("sd_a"),
            label = HTML("Error(a) &sigma;<sub>A</sub> (franjas A dentro de bloque):"),
            value = 6, step = 0.5
          ),
          numericInput(
            ns("sd_b"),
            label = HTML("Error(b) &sigma;<sub>B</sub> (franjas B dentro de bloque):"),
            value = 5, step = 0.5
          ),
          numericInput(
            ns("sd_c"),
            label = HTML("Error(c) &sigma;<sub>C</sub> (celdas A&times;B):"),
            value = 4, step = 0.5
          )
        ),
        
        tags$hr(),
        h5("3. Ejecutar el ANOVA"),
        actionButton(
          ns("btn_correr_anova"),
          "Correr ANOVA de franjas (strip.plot)",
          class = "btn btn-success w-100 mt-2"
        ),
        
        helpText(
          class = "text-muted small",
          "Tip: en clase puedes empezar con el ejemplo ",
          code("agricolae::plotted"),
          " para ver la estructura, y luego cambiar a ",
          strong("Simular a partir del diseño"),
          " para conectar con el fieldbook de la pestaña 3."
        )
      ),
      
      mainPanel(
        width = 8,
        
        h5("4. Estructura de datos utilizada en el ANOVA"),
        p(
          class = "text-muted",
          "La tabla debe contener al menos las columnas: ",
          code("block"), ", ", code("col"), ", ", code("row"), " y ", code("Y"),
          ". Cada fila es una celda A×B en un bloque."
        ),
        DT::dataTableOutput(ns("tabla_datos_analisis")),
        
        hr(),
        h5("5. Resultado del ANOVA en franjas (tres tablas)"),
        uiOutput(ns("anova_resumen")),
        verbatimTextOutput(ns("anova_print")),
        
        hr(),
        h5("6. Mapa de calor de medias observadas A×B"),
        p(
          class = "text-muted",
          "Este mapa refuerza la idea de que la ",
          strong("interacción A×B"),
          " es el foco central del strip-plot: patrones no paralelos ",
          "entre filas y columnas indican interacción."
        ),
        plotOutput(ns("heat_means"), height = "360px")
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales & ejercicios guiados
pestanna5_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo & ejercicios",
    
    h4(class = "section-header",
       "Protocolo para analizar datos reales de strip-plot y explorar ejercicios"),
    
    p(
      "Esta pestaña resume el flujo de trabajo para analizar un diseño en franjas (strip-plot) ",
      "con tus propios datos (checklist Paso 0–4) y ofrece un simulador para practicar ",
      "cómo cambian las conclusiones cuando varían los componentes de error."
    ),
    
    bslib::navset_card_pill(
      
      # ---------------------------------------------------------------
      # 5.1 Protocolo con datos reales
      # ---------------------------------------------------------------
      bslib::nav_panel(
        "5.1 Protocolo datos reales",
        
        h5("Paso 0 – Entender el diseño strip-plot"),
        tags$ol(
          tags$li(
            "Verifica que tu archivo (Excel/CSV) contenga al menos las columnas: ",
            code("block"), " (bloque o repetición), ",
            code("col"), " (franjas verticales, factor A), ",
            code("row"), " (franjas horizontales, factor B) y ",
            code("Y"), " (variable respuesta)."
          ),
          tags$li(
            "Interpreta: ",
            strong("col"), " = niveles del factor A (franjas verticales) y ",
            strong("row"), " = niveles del factor B (franjas horizontales)."
          ),
          tags$li(
            "Pregunta de chequeo: ¿cada combinación A×B aparece al menos una vez en cada bloque?"
          )
        ),
        
        hr(),
        h5("Paso 1 – Explorar balance y faltantes"),
        p(
          "La tabla siguiente cuenta cuántas observaciones hay para cada combinación ",
          code("block × A (col) × B (row)"), ". ",
          "Una tabla sin ceros indica un diseño completamente balanceado."
        ),
        tableOutput(ns("tabla_balance")),
        p(class = "text-muted small",
          "Si observas muchas combinaciones con conteo 0, el diseño está desbalanceado ",
          "y el análisis clásico de franjas puede volverse frágil; en ese caso, en la pestaña 6 ",
          "se discute el uso de modelos mixtos (LMM)."),
        
        hr(),
        h5("Vista rápida de los datos usados en el ANOVA de franjas"),
        p(
          "Estos son los primeros registros del data.frame que se utiliza en la pestaña 4 ",
          "para correr ", code("agricolae::strip.plot()"), "."
        ),
        DT::dataTableOutput(ns("tabla_resumen_real")),
        p(class = "text-muted small",
          "Revisa que los niveles de ", code("col"), " y ", code("row"),
          " correspondan a los factores A y B definidos en el diseño.")
      ),
      
      # ---------------------------------------------------------------
      # 5.2 Post-hoc (LSD por estrato de error)
      # ---------------------------------------------------------------
      bslib::nav_panel(
        "5.2 Post-hoc (LSD por estrato)",
        
        h5("Paso 2 – ANOVA de franjas (recordatorio)"),
        p(
          "En la pestaña 4 ya corriste el ANOVA de franjas con ",
          code("strip.plot()"), ". De allí obtuviste tres errores: ",
          code("Ea, Eb, Ec"), " y sus grados de libertad ",
          code("gl.a, gl.b, gl.c"), " que se usan como denominadores F:"
        ),
        tags$ul(
          tags$li("Factor A (franjas verticales) → usa ", code("Error(a): Ea, gl.a")),
          tags$li("Factor B (franjas horizontales) → usa ", code("Error(b): Eb, gl.b")),
          tags$li("Interacción A×B → usa ", code("Error(c): Ec, gl.c"))
        ),
        
        hr(),
        h5("Paso 3 – Comparaciones múltiples (LSD) por estrato correcto"),
        p(
          "Aquí aplicamos ", code("agricolae::LSD.test()"), " usando explícitamente el ",
          strong("MS y gl del error adecuado"), " para cada efecto. ",
          "Primero corre el ANOVA de franjas en la pestaña 4; luego selecciona qué comparar."
        ),
        
        sidebarLayout(
          sidebarPanel(
            width = 4,
            helpText(
              "Requisito: haber ejecutado el ANOVA con strip.plot() en la pestaña 4 ",
              "para que el objeto de ajuste esté disponible."
            ),
            checkboxGroupInput(
              ns("posthoc_targets"),
              "¿Qué comparar?",
              choices = c("Factor A (franjas verticales)" = "A",
                          "Factor B (franjas horizontales)" = "B",
                          "Interacción A×B (combinaciones)" = "AB"),
              selected = c("A", "B", "AB")
            ),
            actionButton(
              ns("btn_correr_posthoc"),
              "Calcular comparaciones LSD",
              class = "btn btn-primary w-100"
            )
          ),
          mainPanel(
            width = 8,
            conditionalPanel(
              condition = sprintf("input['%s'].includes('A')", ns("posthoc_targets")),
              tags$h5("LSD para Factor A (usa MS = Ea, gl = gl.a)"),
              verbatimTextOutput(ns("out_lsd_A"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'].includes('B')", ns("posthoc_targets")),
              hr(),
              tags$h5("LSD para Factor B (usa MS = Eb, gl = gl.b)"),
              verbatimTextOutput(ns("out_lsd_B"))
            ),
            conditionalPanel(
              condition = sprintf("input['%s'].includes('AB')", ns("posthoc_targets")),
              hr(),
              tags$h5("LSD para Interacción A×B (usa MS = Ec, gl = gl.c)"),
              verbatimTextOutput(ns("out_lsd_AB"))
            ),
            hr(),
            h5("Paso 4 – Plantilla de redacción agronómica"),
            p(
              "Usa la siguiente plantilla como guía para redactar resultados interpretables ",
              "para un informe o tesis."
            ),
            verbatimTextOutput(ns("plantilla_reporte"))
          )
        )
      ),
      
      # ---------------------------------------------------------------
      # 5.3 Ejercicios interactivos (simulación)
      # ---------------------------------------------------------------
      bslib::nav_panel(
        "5.3 Ejercicios interactivos",
        
        h5("Simulador: juega con σₐ, σᵦ, σ꜀ y con la magnitud de los efectos"),
        p(
          "Este simulador genera datos de un strip-plot balanceado con niveles de A, B ",
          "y bloques r. Puedes cambiar las amplitudes de los efectos (A, B, A×B) y los ",
          "tres componentes de error ", HTML("&sigma;"), "ᴀ, ", HTML("&sigma;"), "ʙ, ",
          HTML("&sigma;"), "꜀ para ver cómo se afectan los valores F, los p-valores y ",
          "las comparaciones LSD."
        ),
        
        sidebarLayout(
          sidebarPanel(
            width = 4,
            tags$h6("Dimensiones del diseño"),
            numericInput(ns("ex_a_levels"), "Niveles de A (franjas verticales):",
                         value = 3, min = 2, max = 8, step = 1),
            numericInput(ns("ex_b_levels"), "Niveles de B (franjas horizontales):",
                         value = 3, min = 2, max = 8, step = 1),
            numericInput(ns("ex_r_blocks"), "Bloques (r):",
                         value = 4, min = 2, max = 20, step = 1),
            hr(),
            tags$h6("Medias y amplitud de efectos"),
            numericInput(ns("ex_mu"), "μ (media base):", value = 100),
            numericInput(ns("ex_amp_A"), "Amplitud efecto A (±):", value = 8, step = 0.5),
            numericInput(ns("ex_amp_B"), "Amplitud efecto B (±):", value = 6, step = 0.5),
            numericInput(ns("ex_amp_AB"), "Amplitud interacción A×B (±):", value = 5, step = 0.5),
            hr(),
            tags$h6("Estructura de error"),
            numericInput(ns("ex_sd_a"), HTML("&sigma;<sub>A</sub> (Error a):"),
                         value = 6, step = 0.5),
            numericInput(ns("ex_sd_b"), HTML("&sigma;<sub>B</sub> (Error b):"),
                         value = 5, step = 0.5),
            numericInput(ns("ex_sd_c"), HTML("&sigma;<sub>C</sub> (Error c):"),
                         value = 4, step = 0.5),
            actionButton(
              ns("btn_run_ex"),
              "Simular y analizar",
              class = "btn btn-success w-100 mt-2"
            ),
            hr(),
            uiOutput(ns("ui_ex_descarga"))
          ),
          mainPanel(
            width = 8,
            tags$h5("ANOVA de franjas (tres tablas)"),
            verbatimTextOutput(ns("ex_anova")),
            hr(),
            tags$h5("LSD (A, B y A×B con sus errores correctos)"),
            verbatimTextOutput(ns("ex_lsd")),
            hr(),
            tags$h5("Mapa de calor de medias simuladas A×B"),
            plotOutput(ns("ex_heat"), height = "360px"),
            p(class = "text-muted small",
              "Puedes descargar el CSV simulado y usarlo como práctica en una plantilla Quarto ",
              "o en un script R independiente.")
          )
        )
      )
    )
  )
}

# Pestaña 6: Notas avanzadas: LMM, desbalances y extensiones
pestanna6_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Notas avanzadas: LMM & desbalances",
    h4(class = "section-header", "Strip-plot como modelo mixto (LMM)"),
    withMathJax(
      div(
        class = "alert alert-info",
        p(
          strong("Idea central: "),
          "el ANOVA de franjas clásico (",
          code("strip.plot()"),
          ") se puede reescribir como un ",
          strong("modelo lineal mixto (LMM)"),
          " con varios componentes de varianza. ",
          "En el caso balanceado, ambos enfoques cuentan la misma historia; ",
          "cuando hay desbalances fuertes, el LMM suele ser más flexible."
        )
      ),
      h5("6.1 Estructura LMM equivalente en el caso balanceado"),
      p(
        "Si pensamos en un strip-plot balanceado con Bloques, franjas ",
        em("A (verticales)"),
        " y franjas ",
        em("B (horizontales)"),
        ", podemos escribir un modelo mixto tipo:"
      ),
      helpText("$$
Y_{ijk} = \\mu + A_i + B_j + (AB)_{ij} + u_k + u_{ik}^{(A)} + u_{jk}^{(B)} + \\varepsilon_{ijk}
      $$"),
      tags$ul(
        tags$li(HTML("<b>A<sub>i</sub></b>: efecto fijo del nivel i de A (franjas verticales).")),
        tags$li(HTML("<b>B<sub>j</sub></b>: efecto fijo del nivel j de B (franjas horizontales).")),
        tags$li(HTML("<b>(AB)<sub>ij</sub></b>: interacción fija A×B (combinaciones de labranza × riego, por ejemplo).")),
        tags$li(HTML("<b>u<sub>k</sub></b>: efecto aleatorio de bloque k.")),
        tags$li(HTML("<b>u<sub>ik</sub><sup>(A)</sup></b>: variación adicional de la franja A dentro del bloque k → aproximación a <b>Error(a)</b>.")),
        tags$li(HTML("<b>u<sub>jk</sub><sup>(B)</sup></b>: variación adicional de la franja B dentro del bloque k → aproximación a <b>Error(b)</b>.")),
        tags$li(HTML("<b>\\varepsilon_{ijk}</b>: variación dentro de la celda A×B del bloque k → <b>Error(c)</b> (residual)."))
      ),
      p(
        "En notación de ", code("lme4::lmer()"), " esto se suele escribir como:"
      ),
      pre(class = "r-code",
          "lmer(Y ~ A * B + (1 | block) + (1 | block:A) + (1 | block:B), data = datos)"),
      div(
        class = "alert alert-secondary",
        tags$strong("Correspondencia con el ANOVA de franjas (strip.plot):"),
        tags$ul(
          tags$li(HTML("Var(<code>block:A</code>) &approx; componente asociado a <b>Error(a)</b>.")),
          tags$li(HTML("Var(<code>block:B</code>) &approx; componente asociado a <b>Error(b)</b>.")),
          tags$li(HTML("Var(residual) &approx; componente asociado a <b>Error(c)</b>.")),
          tags$li(
            "En un strip-plot balanceado, las F y p-valores de ",
            code("lmerTest::anova(m, type = 3)"),
            " para A, B y A×B suelen coincidir (salvo pequeñas diferencias numéricas) ",
            "con las obtenidas vía ", code("agricolae::strip.plot()"), "."
          )
        )
      ),
      hr(),
      h4("6.2 ¿Cuándo pasar de strip.plot() a un LMM (lmer)?"),
      p(
        "El ANOVA clásico de franjas funciona muy bien como primera aproximación, ",
        "pero sufre cuando el diseño real se aleja del ideal balanceado."
      ),
      tags$ul(
        tags$li(
          strong("Muchos huecos en la tabla "),
          code("block × A × B"),
          ": celdas A×B perdidas en ciertos bloques."
        ),
        tags$li(
          strong("Bloques incompletos"),
          ": algunos bloques no tienen todas las combinaciones de A×B."
        ),
        tags$li(
          strong("Estructuras adicionales"),
          ": por ejemplo mediciones repetidas dentro de cada celda A×B ",
          "(varias fechas, varios años) o sub-subunidades dentro de cada celda."
        ),
        tags$li(
          strong("Necesidad de comparar modelos de fijos"),
          ": quieres evaluar covariables adicionales (p.ej., ",
          em("pendiente, textura del suelo, NDVI"),
          ") o simplificar interacciones."
        )
      ),
      div(
        class = "alert alert-warning",
        tags$strong("Mensaje clave:"),
        " cuando el diseño real está muy desbalanceado o es más complejo ",
        "que el strip-plot canónico, conviene salir del ANOVA rígido y ",
        "formulear el problema como un LMM con ",
        code("lmer()"),
        " o paquetes similares."
      ),
      hr(),
      h4("6.3 REML vs ML en strip-plot mixtos"),
      p(
        "El mismo principio de la Sesión 5 (split-plot) se aplica aquí "
        ,"cuando pasamos a modelos mixtos:"
      ),
      tags$ul(
        tags$li(
          strong("REML (Restricted Maximum Likelihood)"),
          ": recomendado para estimar componentes de varianza ",
          "cuando ya has decidido la estructura de efectos fijos ",
          code("Y ~ A * B"),
          "."
        ),
        tags$li(
          strong("ML (Maximum Likelihood, REML = FALSE)"),
          ": úsalo solo cuando quieres comparar modelos con ",
          strong("diferentes efectos fijos"),
          " (por ejemplo, ",
          code("Y ~ A * B"),
          " vs ",
          code("Y ~ A + B"),
          "), manteniendo ",
          em("exactamente igual"),
          " la parte aleatoria."
        )
      ),
      div(
        class = "alert alert-info",
        tags$strong("Regla práctica:"),
        tags$ul(
          tags$li("Define primero el diseño (strip-plot) y su estructura aleatoria razonable."),
          tags$li("Ajusta modelos con esa parte aleatoria usando REML para obtener varianzas e ICCs."),
          tags$li("Si necesitas comparar modelos de fijos, refitea esos modelos con REML = FALSE (ML) y compara AIC/LRT, siempre con la misma parte aleatoria.")
        )
      ),
      hr(),
      h4("6.4 Pseudorreplicación y n efectivo en strip-plot"),
      p(
        "Al igual que en el split-plot, el strip-plot tiene un riesgo fuerte de ",
        strong("pseudorreplicación"),
        " si se ignora la estructura de franjas."
      ),
      tags$ul(
        tags$li(
          "Si analizas las observaciones como si fueran un RCBD simple ",
          "y tratas cada celda A×B como réplica independiente de A y B, ",
          "estás inflando el tamaño de muestra efectivo para los efectos principales."
        ),
        tags$li(
          "Eso puede producir p-valores muy pequeños para A o B ",
          "que en realidad solo reflejan variación entre celdas dentro de la misma franja, ",
          "no evidencia real de efecto principal."
        ),
        tags$li(
          "El ANOVA de franjas (o el LMM equivalente) evita esta pseudorreplicación ",
          "al separar claramente ",
          strong("Error(a), Error(b) y Error(c)"),
          " y usar cada uno donde corresponde."
        )
      ),
      div(
        class = "alert alert-secondary",
        strong("Mensaje para llevar: "),
        "en strip-plot, la unidad experimental para A y B no es la celda individual, ",
        "sino la franja correspondiente dentro de cada bloque. ",
        "El modelo que no respeta esta estructura sobreestima el n y subestima el p-valor."
      ),
      hr(),
      h4("6.5 Extensiones naturales"),
      tags$ul(
        tags$li(
          strong("Mediciones repetidas dentro de cada celda A×B"),
          ": puedes añadir un término aleatorio adicional, por ejemplo ",
          code("(1 | block:A:B)"),
          " si hay varias observaciones por celda."
        ),
        tags$li(
          strong("Más de dos factores en franjas o sub-franjas"),
          ": combinaciones de strip-plot con split-plot o split-split-plot, ",
          "siempre respetando la jerarquía en la parte aleatoria."
        ),
        tags$li(
          strong("Hacia GLMM"),
          ": si la respuesta Y no es aproximadamente normal (conteos, proporciones), ",
          "es posible extender la idea a modelos lineales generalizados mixtos ",
          "(GLMM) con familia Poisson, binomial, etc., manteniendo la misma lógica ",
          "de componentes de varianza por estrato."
        )
      ),
      br(),
      p(
        class = "text-muted small",
        "Esta pestaña no se ejecuta en vivo, pero deja documentado el puente conceptual ",
        "entre el ANOVA de franjas clásico y los modelos mixtos modernos, ",
        "para que puedas avanzar hacia tesis o artículos usando ",
        code("lmer()"),
        " sin perder de vista la estructura experimental original."
      )
    )
  )
}

# Pestaña extra: Esquemas visuales y galería conceptual
pestanna_extra_session6_v3UI <- function(ns) {
  # Definir la ruta base para las imágenes de esta sesión
  base_path <- "images/sesiones/Disenos_estadisticos_V3/optimizada/"
  img_path  <- paste0(base_path, "session6/")
  
  bslib::nav_panel(
    title = "Extra: Esquemas Visuales",
    icon = icon("layer-group"), # Icono de capas/grupos
    
    tags$div(
      class = "container-fluid py-3",
      tags$h4(class = "text-primary mb-4", "Galería Conceptual: Diseños en Franjas (Strip-Plot)"),
      tags$p(
        class = "lead",
        "El diseño strip-plot introduce una estructura de aleatorización cruzada que genera tres errores experimentales. ",
        "Esta galería visualiza la disposición física, el modelo matemático y la comparación con otros diseños."
      ),
      tags$hr(),
      
      # Navegación interna con pestañas subrayadas
      bslib::navset_card_underline(
        
        # --- Sub-pestaña 1: Estructura Física ---
        bslib::nav_panel(
          title = "1. Estructura Física",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-8",
              tags$img(
                src = paste0(img_path, "strip_plot_mental_map.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Croquis mental de un diseño strip-plot por bloque",
                style = "width: 100%; object-fit: contain;"
              )
            ),
            tags$div(
              class = "col-md-4",
              tags$div(
                class = "alert alert-info mt-3 mt-md-0",
                tags$h5("El Cruce de Franjas"),
                tags$p(
                  "A diferencia del split-plot, aquí no hay una parcela 'principal' que contenga a la otra. ",
                  "El Factor A se aplica en franjas verticales y el Factor B en horizontales."
                ),
                tags$p(
                  "La unidad experimental para la interacción es la intersección, que es mucho más pequeña y homogénea que las franjas completas."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 2: El Modelo Matemático ---
        bslib::nav_panel(
          title = "2. Anatomía del Modelo",
          tags$div(
            class = "row justify-content-center",
            tags$div(
              class = "col-md-10",
              tags$img(
                src = paste0(img_path, "strip_plot_equation_anatomy.webp"),
                class = "img-fluid shadow border rounded mx-auto d-block",
                alt = "Desglose de la ecuación del modelo strip-plot",
                style = "width: 100%;"
              ),
              tags$div(
                class = "mt-3 p-3 bg-light border rounded text-center",
                tags$h6("Tres Fuentes de Ruido"),
                tags$p(
                  "Observa los términos griegos en color. Cada uno representa un error aleatorio distinto que debe usarse para probar su respectivo efecto fijo. ",
                  "Usar un solo error residual (como en un factorial simple) invalidaría el análisis."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 3: De Campo a Datos ---
        bslib::nav_panel(
          title = "3. Del Campo a la Tabla",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-12 mb-3",
              tags$img(
                src = paste0(img_path, "fieldbook_structure_strip.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Relación entre diseño físico y fieldbook",
                style = "width: 100%;"
              )
            )
          )
        ),
        
        # --- Sub-pestaña 4: Matriz de Errores ---
        bslib::nav_panel(
          title = "4. Quién prueba a Quién",
          tags$div(
            class = "row justify-content-center",
            tags$div(
              class = "col-md-8",
              tags$img(
                src = paste0(img_path, "anova_error_matrix_strip.webp"),
                class = "img-fluid shadow border rounded",
                alt = "Tabla de errores y precisión por efecto",
                style = "width: 100%;"
              )
            ),
            tags$div(
              class = "col-md-4",
              tags$div(
                class = "card border-success",
                tags$div(class = "card-header bg-success text-white", "La Joya del Strip-Plot"),
                tags$div(
                  class = "card-body",
                  tags$p(
                    "Fíjate en la precisión. El diseño sacrifica precisión en los efectos principales (A y B) para maximizar la precisión en la interacción (A×B)."
                  ),
                  tags$small("Úsalo cuando la interacción es tu prioridad científica.")
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 5: Flujo en R ---
        bslib::nav_panel(
          title = "5. Flujo en R",
          tags$div(
            class = "text-center",
            tags$img(
              src = paste0(img_path, "agricolae_workflow_strip.webp"),
              class = "img-fluid shadow-sm rounded",
              alt = "Flujo de trabajo con agricolae::strip.plot",
              style = "max-height: 500px;"
            )
          )
        ),
        
        # --- Sub-pestaña 6: Comparativa de Diseños ---
        bslib::nav_panel(
          title = "6. Comparativa de Diseños",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-12",
              tags$img(
                src = paste0(img_path, "design_comparison_factorial_split_strip.webp"),
                class = "img-fluid shadow border rounded",
                alt = "Comparación visual: Factorial vs Split-Plot vs Strip-Plot",
                style = "width: 100%;"
              ),
              tags$div(
                class = "mt-4 p-3 bg-white border-top",
                tags$h5("Resumen Visual"),
                tags$ul(
                  tags$li(strong("Factorial RCBD:"), " Aleatorización total. Máxima precisión para efectos principales, pero difícil de implementar si los factores son grandes (ej. riego)."),
                  tags$li(strong("Split-Plot:"), " Jerarquía. Un factor grande contiene a uno pequeño. Ideal cuando un factor es difícil de mover."),
                  tags$li(strong("Strip-Plot:"), " Cruce. Dos factores difíciles de mover se cruzan. Sacrifica efectos principales por una interacción ultra-precisa.")
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
pestanna7_session6_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    
    h4(class = "section-header", "Referencias y recursos recomendados"),
    tags$p(
      class = "text-muted",
      "Lecturas para profundizar en diseños en franjas (strip-plot), ",
      "modelos mixtos con varios estratos de error y la implementación en R."
    ),
    
    tags$ul(
      # 1) Libro de modelos mixtos (marco general multi-estrato)
      tags$li(
        "Stroup, W. W. (2013). ",
        em("Generalized Linear Mixed Models: Modern Concepts, Methods and Applications."),
        " CRC Press. Capítulos sobre diseños con múltiples estratos de error ",
        "(incluyendo strip-plot / split-block) y su análisis vía modelos mixtos."
      ),
      
      # 2) Tutorial agricolae (uso práctico en R)
      tags$li(
        "De Mendiburu, F. (2017). ",
        em("Tutorial del paquete agricolae."),
        " Documento en línea que ilustra la generación y análisis de diseños ",
        "agrícolas en R, incluyendo funciones como ",
        code("design.strip()"), " y ", code("strip.plot()"), ". ",
        tags$a(
          href   = "https://tarwi.lamolina.edu.pe/~fmendiburu/index-filer/download/tutorial.pdf",
          target = "_blank",
          "Enlace al tutorial agricolae"
        )
      ),
      
      # 3) Documentación CRAN agricolae: diseño y ANOVA en franjas
      tags$li(
        "Paquete ", code("agricolae"), " (CRAN). Documentación de ayuda para:",
        tags$ul(
          tags$li(
            tags$a(
              href   = "https://search.r-project.org/CRAN/refmans/agricolae/html/design.strip.html",
              target = "_blank",
              "design.strip() – generación de diseños en franjas (strip-plot) en RCBD"
            )
          ),
          tags$li(
            tags$a(
              href   = "https://search.r-project.org/CRAN/refmans/agricolae/html/strip.plot.html",
              target = "_blank",
              "strip.plot() – ANOVA en franjas con tres errores (a, b, c)"
            )
          )
        )
      ),
      
      # 4) Notas de curso sobre strip-plot y expected mean squares
      tags$li(
        "Penn State University, STAT 503. ",
        em("Lesson 14.5 – Strip-Plot Designs."),
        " Notas de curso que presentan el strip-plot (split-block), la ",
        "estructura de tres errores, los expected mean squares y la elección ",
        "de denominadores F en ANOVA multi-estrato. ",
        tags$a(
          href   = "https://online.stat.psu.edu/stat503/book/export/html/691",
          target = "_blank",
          "Notas STAT 503 sobre Strip-Plot"
        )
      ),
      
      # 5) Recursos generales sobre diseños avanzados / multi-estrato
      tags$li(
        "Textos de diseños experimentales y modelos mixtos que tratan ",
        "diseños avanzados (split-plot, strip-plot / split-block, split-split): ",
        "por ejemplo, capítulos dedicados a estructuras de bloques y ",
        "estratos de error en libros de diseño agronómico o de modelos mixtos.",
        tags$br(),
        tags$span(
          class = "text-muted small",
          "Útiles para conectar la lógica de strip-plot con otros diseños multi-estrato ",
          "vistos en la Parte III del curso."
        )
      )
    ),
    
    tags$hr(),
    tags$p(
      class = "text-muted small",
      "Sugerencia pedagógica: vincula cada referencia con una parte concreta de la sesión ",
      "(diseño y fieldbook, ANOVA en franjas, modelos mixtos/LMM) al preparar tu informe, ",
      "trabajo de curso o tesis."
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session6_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 6: Diseños en Franjas (Strip-Plot)")
    ),
    navset_tab(
      pestanna1_session6_v3UI(ns),
      pestanna2_session6_v3UI(ns),
      pestanna3_session6_v3UI(ns),
      pestanna4_session6_v3UI(ns),
      pestanna5_session6_v3UI(ns),
      pestanna6_session6_v3UI(ns),
      pestanna_extra_session6_v3UI(ns),
      pestanna7_session6_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto
pestanna1_session6_v3_server <- function(input, output, session) {
  # No server logic needed for this introductory tab
}

# Pestaña 2: Conceptos clave: franjas y tres errores
pestanna2_session6_v3_server <- function(input, output, session, design_obj, has_pkg) {
  # Esta pestaña es principalmente conceptual: solo generamos una tabla pedagógica.
  
  output$tabla_errores_strip <- renderTable({
    data.frame(
      `Efecto que se prueba` = c(
        "Factor A (franjas verticales)",
        "Factor B (franjas horizontales)",
        "Interacción A×B (celdas)"
      ),
      `Contraste (intuición)` = c(
        "Diferencias entre medias de filas (niveles de A)",
        "Diferencias entre medias de columnas (niveles de B)",
        "Diferencias entre combinaciones A×B"
      ),
      `Estrato de error adecuado` = c(
        "Error(a): variación entre franjas A dentro de bloque",
        "Error(b): variación entre franjas B dentro de bloque",
        "Error(c): variación entre celdas A×B dentro de bloque"
      ),
      `MS y gl usados en ANOVA` = c(
        "MS = Ea, gl = gl.a",
        "MS = Eb, gl = gl.b",
        "MS = Ec, gl = gl.c"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  },
  striped  = TRUE,
  bordered = TRUE,
  align    = "c")
}

# Pestaña 3: Diseño & libro de campo
pestanna3_session6_v3_server <- function(
  input, output, session,
  design_obj, datos_para_analisis, strip_fit, has_pkg
) {
  ns <- session$ns
  
  # -------------------------------------------------------------------
  # 1) Generar el diseño con agricolae::design.strip()
  # -------------------------------------------------------------------
  observeEvent(input$btn_generar_diseno, {
    # Verificar que el paquete agricolae está disponible
    shiny::req(has_pkg("agricolae"))
    
    # Parsear niveles de A y B desde los textInput (separados por coma)
    A <- strsplit(input$niveles_A_txt, "\\s*,\\s*")[[1]] |> unique()
    B <- strsplit(input$niveles_B_txt, "\\s*,\\s*")[[1]] |> unique()
    r <- input$r_bloques
    
    # Validaciones básicas didácticas
    shiny::validate(
      need(length(A) >= 2,
           "Debe especificar al menos 2 niveles para el Factor A (franjas verticales)."),
      need(length(B) >= 2,
           "Debe especificar al menos 2 niveles para el Factor B (franjas horizontales)."),
      need(r >= 2,
           "Se requieren al menos 2 bloques (réplicas) para hacer inferencia válida.")
    )
    
    # Semilla para reproducibilidad (opcional)
    if (isTRUE(input$usar_semilla)) {
      set.seed(input$semilla)
    }
    
    # Llamada a agricolae::design.strip
    des <- agricolae::design.strip(
      trt1  = A,
      trt2  = B,
      r     = r,

    )
    
    # Guardar el objeto de diseño en el reactiveVal compartido
    design_obj(des)
    
    showNotification(
      "Diseño strip-plot generado con agricolae::design.strip().",
      type = "message", duration = 3
    )
  })
  
  # -------------------------------------------------------------------
  # 2) Resumen del diseño: r × a × b celdas
  # -------------------------------------------------------------------
  output$resumen_diseno <- renderUI({
    des <- design_obj(); shiny::req(des)
    
    A <- des$parameters$trt1
    B <- des$parameters$trt2
    r <- des$parameters$replications
    n_celdas <- length(A) * length(B) * r
    
    # Detectar los nombres de columna que usan agricolae para A y B
    book <- des$book
    nm_A <- names(book)[which(names(book) %in% c("trt1", "labranza", "col"))[1]]
    nm_B <- names(book)[which(names(book) %in% c("trt2", "riego", "row"))[1]]
    
    tagList(
      div(
        class = "alert alert-success",
        HTML(paste0(
          "<b>Estructura del diseño strip-plot:</b><br>",
          "&bull; Bloques (r): <b>", r, "</b><br>",
          "&bull; Niveles de A (franjas verticales): <b>", length(A), "</b><br>",
          "&bull; Niveles de B (franjas horizontales): <b>", length(B), "</b><br>",
          "&bull; Total de celdas por diseño: <b>", n_celdas, "</b> observaciones esperadas.<br><br>",
          "<b>Mapeo a columnas del fieldbook:</b><br>",
          "Factor A (vertical) &rarr; columna <code>", nm_A, "</code>.<br>",
          "Factor B (horizontal) &rarr; columna <code>", nm_B, "</code>.<br>"
        ))
      ),
      p(
        class = "text-muted small",
        "Más adelante, cuando analicemos estos datos, usaremos estas columnas para",
        " definir el modelo ANOVA de franjas con sus tres errores: ",
        strong("Error(a) para A, Error(b) para B y Error(c) para A×B"), "."
      )
    )
  })
  
  # -------------------------------------------------------------------
  # 3) Mostrar el fieldbook (tabla)
  # -------------------------------------------------------------------
  output$tabla_book <- DT::renderDataTable({
    des <- design_obj(); shiny::req(des)
    book <- des$book
    
    DT::datatable(
      book,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX    = TRUE
      )
    )
  })
  
  # -------------------------------------------------------------------
  # 4) Botón para descargar el fieldbook en CSV
  # -------------------------------------------------------------------
  output$ui_descargar_book <- renderUI({
    shiny::req(design_obj())
    downloadButton(
      ns("dl_book"),
      "Descargar fieldbook (CSV)",
      class = "btn btn-secondary w-100"
    )
  })
  
  output$dl_book <- downloadHandler(
    filename = function() {
      paste0("stripplot_fieldbook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      des <- design_obj()
      utils::write.csv(des$book, file, row.names = FALSE)
    }
  )
  
  # -------------------------------------------------------------------
  # 5) Croquis conceptual por bloque (A × B)
  # -------------------------------------------------------------------
  output$plot_croquis <- renderPlot({
    des <- design_obj(); shiny::req(des)
    book <- des$book
    
    # Detectar niveles de A y B desde los parámetros
    A_levels <- des$parameters$trt1
    B_levels <- des$parameters$trt2
    
    # Construir una grilla completa A × B × bloque
    grid <- expand.grid(
      block = sort(unique(book$block)),
      A     = factor(A_levels, levels = A_levels),
      B     = factor(B_levels, levels = B_levels)
    )
    
    # Detectar nombres reales de columnas para A y B en el book
    # Detectar nombres reales de columnas para A y B en el book por eliminación
    # Las columnas estándar son: plots, block. Las que sobran son los factores.
    std_cols <- c("plots", "block", "r") 
    factor_cols <- setdiff(names(book), std_cols)
    
    # Asumimos que la primera es A y la segunda es B (o por orden de aparición en design.strip)
    # design.strip(trt1=A, trt2=B) -> suele poner A primero, luego B.
    if (length(factor_cols) >= 2) {
      nm_A <- factor_cols[1]
      nm_B <- factor_cols[2]
    } else {
      # Fallback si falla la detección
      nm_A <- names(book)[1] 
      nm_B <- names(book)[2]
    }
    
    # Unir con el book para saber qué celdas están asignadas
    grid <- grid |>
      dplyr::left_join(
        book |>
          dplyr::rename(
            A = !!dplyr::sym(nm_A),
            B = !!dplyr::sym(nm_B)
          ) |>
          dplyr::mutate(
            A = factor(A, levels = A_levels),
            B = factor(B, levels = B_levels)
          ) |>
          dplyr::select(block, A, B, plots),
        by = c("block", "A", "B")
      )
    
    ggplot2::ggplot(
      grid,
      ggplot2::aes(x = B, y = A, fill = !is.na(plots))
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = ifelse(is.na(plots), "", plots)),
        size = 3
      ) +
      ggplot2::facet_wrap(~ block, nrow = 1) +
      ggplot2::labs(
        x     = "Factor B (franjas horizontales)",
        y     = "Factor A (franjas verticales)",
        fill  = "Parcela asignada",
        title = "Croquis conceptual por bloque (A × B en un diseño strip-plot)"
      ) +
      ggplot2::scale_fill_manual(values = c("TRUE" = "#9ecae1", "FALSE" = "#f0f0f0")) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(legend.position = "none")
  })
}

# Pestaña 4: ANOVA en franjas (strip-plot)
pestanna4_session6_v3_server <- function(input, output, session,
                                         datos_para_analisis,
                                         strip_fit,
                                         has_pkg
                                        ) {
  
  ns <- session$ns
  
  # 1) Vista previa de los datos que se van a analizar
  output$tabla_datos_analisis <- DT::renderDataTable({
    df <- datos_para_analisis()   # definido en el server principal
    shiny::req(df)
    
    DT::datatable(
      head(df, 20),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # 2) Ejecutar agricolae::strip.plot cuando el usuario hace clic
  observeEvent(input$btn_correr_anova, {
    shiny::req(has_pkg("agricolae"))
    
    df <- datos_para_analisis()
    shiny::req(df)
    
    # Validar estructura mínima
    shiny::validate(
      need(
        all(c("block", "col", "row", "Y") %in% names(df)),
        "El data.frame debe contener las columnas: block, col, row, Y."
      )
    )
    
    # Llamada al ANOVA de franjas
    fit <- agricolae::strip.plot(
      BLOCK = df$block,
      COL   = df$col,
      ROW   = df$row,
      Y     = df$Y
    )
    
    strip_fit(fit)
    
    showNotification(
      "ANOVA en franjas completado (strip.plot). Revisa las tres tablas y los errores Ea, Eb, Ec.",
      type = "message",
      duration = 4
    )
  })
  
  # 3) Resumen pedagógico de los tres errores
  output$anova_resumen <- renderUI({
    fit <- strip_fit()
    shiny::req(fit)
    
    tagList(
      div(
        class = "alert alert-secondary",
        HTML(paste0(
          "<b>Resumen de estratos de error (conexión con la teoría):</b><br>",
          "<ul>",
          "<li><b>Error(a):</b> variación entre franjas A dentro de bloque → ",
          "MS = Ea, gl = gl.a → se usa para probar el efecto de <b>A (franjas verticales)</b>.</li>",
          "<li><b>Error(b):</b> variación entre franjas B dentro de bloque → ",
          "MS = Eb, gl = gl.b → se usa para probar el efecto de <b>B (franjas horizontales)</b>.</li>",
          "<li><b>Error(c):</b> variación entre celdas A×B dentro de bloque → ",
          "MS = Ec, gl = gl.c → se usa para probar la <b>interacción A×B</b>.</li>",
          "</ul>",
          "En la salida de ", code("strip.plot"), 
          " verás tres tablas ANOVA: una para A, otra para B y otra para A×B. ",
          "Cada una utiliza su estrato de error correspondiente."
        ))
      )
    )
  })
  
  # 4) Impresión detallada de las tres tablas ANOVA y de Ea/Eb/Ec
  output$anova_print <- renderPrint({
    fit <- strip_fit()
    shiny::req(fit)
    
    cat("== ANOVA A (franjas verticales) ==\n")
    print(fit$ANOVA[[1]])
    
    cat("\n== ANOVA B (franjas horizontales) ==\n")
    print(fit$ANOVA[[2]])
    
    cat("\n== ANOVA A × B (interacción) ==\n")
    print(fit$ANOVA[[3]])
    
    cat("\n--- Estratos de error estimados (para conectar con la teoría) ---\n")
    cat("Ea (Error a, franjas A dentro de bloque):", fit$Ea, " | gl.a:", fit$gl.a, "\n")
    cat("Eb (Error b, franjas B dentro de bloque):", fit$Eb, " | gl.b:", fit$gl.b, "\n")
    cat("Ec (Error c, celdas A×B):", fit$Ec, " | gl.c:", fit$gl.c, "\n")
  })
  
  # 5) Mapa de calor de las medias observadas por combinación A×B
  output$heat_means <- renderPlot({
    df <- datos_para_analisis()
    shiny::req(df)
    
    means <- df |>
      dplyr::group_by(col, row) |>
      dplyr::summarise(meanY = mean(Y), .groups = "drop")
    
    ggplot2::ggplot(means, ggplot2::aes(x = row, y = col, fill = meanY)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.1f", meanY)),
        size = 3
      ) +
      ggplot2::scale_fill_continuous(name = "Media") +
      ggplot2::labs(
        x = "Factor B (horizontal)",
        y = "Factor A (vertical)",
        title = "Medias observadas por combinación A×B"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
}

# Pestaña 5: Protocolo para datos reales & ejercicios guiados
pestanna5_session6_v3_server <- function(input, output, session,
                                          datos_para_analisis,
                                          strip_fit
                                        ) {
  ns <- session$ns
  
  # -------------------------------------------------------------
  # 5.1 Protocolo datos reales: balance y vista rápida
  # -------------------------------------------------------------
  output$tabla_resumen_real <- DT::renderDataTable({
    df <- datos_para_analisis(); shiny::req(df)
    DT::datatable(
      head(df, 20),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$tabla_balance <- renderTable({
    df <- datos_para_analisis(); shiny::req(df)
    
    # Conteo por combinación block × A × B
    df |>
      dplyr::count(block, col, row, name = "n") |>
      dplyr::arrange(block, col, row)
    
  }, striped = TRUE, bordered = TRUE, align = "c", digits = 0)
  
  # -------------------------------------------------------------
  # 5.2 Post-hoc (LSD) usando Ea, Eb, Ec y sus gl
  # -------------------------------------------------------------
  compute_LSDs <- eventReactive(input$btn_correr_posthoc, {
    shiny::req(strip_fit())
    fit <- strip_fit()
    df  <- datos_para_analisis(); shiny::req(df)
    
    res <- list()
    
    # Factor A: usa Error(a) → Ea, gl.a
    if ("A" %in% input$posthoc_targets) {
      res$A <- agricolae::LSD.test(
        y       = df$Y,
        trt     = df$col,
        DFerror = fit$gl.a,
        MSerror = fit$Ea,
        console = TRUE
      )
    }
    
    # Factor B: usa Error(b) → Eb, gl.b
    if ("B" %in% input$posthoc_targets) {
      res$B <- agricolae::LSD.test(
        y       = df$Y,
        trt     = df$row,
        DFerror = fit$gl.b,
        MSerror = fit$Eb,
        console = TRUE
      )
    }
    
    # Interacción A×B: usa Error(c) → Ec, gl.c
    if ("AB" %in% input$posthoc_targets) {
      df$AB <- interaction(df$col, df$row, drop = TRUE)
      res$AB <- agricolae::LSD.test(
        y       = df$Y,
        trt     = df$AB,
        DFerror = fit$gl.c,
        MSerror = fit$Ec,
        console = TRUE
      )
    }
    
    res
  })
  
  output$out_lsd_A <- renderPrint({
    x <- compute_LSDs(); shiny::req(x$A)
    print(x$A)
  })
  
  output$out_lsd_B <- renderPrint({
    x <- compute_LSDs(); shiny::req(x$B)
    print(x$B)
  })
  
  output$out_lsd_AB <- renderPrint({
    x <- compute_LSDs(); shiny::req(x$AB)
    print(x$AB)
  })
  
  # Plantilla de redacción agronómica (texto guía)
  output$plantilla_reporte <- renderText({
    # Intento de obtener p-valor de la interacción, si existe
    p_ab <- NA
    gl_ab <- NA
    fit <- strip_fit()
    
    if (!is.null(fit)) {
      # fit$ANOVA[[3]] suele contener la tabla de A×B con F y Pr>F
      tab_ab <- try(fit$ANOVA[[3]], silent = TRUE)
      if (!inherits(tab_ab, "try-error")) {
        # Buscamos una columna que parezca p-valor
        posibles_p <- c("Pr(>F)", "Pr>F", "Pr.F.")
        col_p <- intersect(posibles_p, colnames(tab_ab))
        if (length(col_p) > 0) {
          p_ab <- tab_ab[1, col_p[1]]
        }
        # Buscamos gl si está disponible (suele llamarse 'Df' o similar)
        if ("Df" %in% colnames(tab_ab)) {
          gl_ab <- tab_ab[1, "Df"]
        }
      }
    }
    
    linea_p <- if (!is.na(p_ab)) {
      paste0("p = ", signif(as.numeric(p_ab), 3))
    } else {
      "p = ... (tomado de la tabla de ANOVA de la interacción A×B)"
    }
    
    linea_gl <- if (!is.na(gl_ab)) {
      paste0("gl = ", gl_ab)
    } else {
      "gl = ... (grados de libertad de la fila A×B en la tabla de ANOVA)"
    }
    
    paste(
      "Ejemplo de texto para informe/tesis (ajusta valores y contexto):",
      "",
      "El diseño en franjas (strip-plot) mostró un efecto [significativo/no significativo]",
      "de la interacción entre el factor A (franjas verticales) y el factor B",
      "(franjas horizontales) sobre la respuesta Y, evaluado con el Error(c).",
      "",
      paste0("En el ANOVA de franjas, la interacción A×B presentó ", linea_gl,
             " y ", linea_p, ", utilizando como denominador el MS del Error(c) (Ec)."),
      "",
      "Los efectos principales de A y B se evaluaron con los estratos Error(a) y Error(b),",
      "respectivamente; dado que estos errores suelen tener menor precisión que el Error(c),",
      "la interpretación se centra en cómo cambian las combinaciones A×B.",
      "",
      "Para la comunicación agronómica se recomienda:",
      "  • Reportar las medias de Y para cada combinación A×B (tabla o gráfico).",
      "  • Destacar las combinaciones que son agronómicamente superiores o más estables.",
      "  • Explicar las implicancias prácticas (por ejemplo, la mejor combinación de",
      "    labranza × riego en términos de rendimiento o infiltración).",
      sep = "\n"
    )
  })
  
  # -------------------------------------------------------------
  # 5.3 Ejercicios interactivos (simulación)
  # -------------------------------------------------------------
  ex_data <- eventReactive(input$btn_run_ex, {
    a   <- input$ex_a_levels
    b   <- input$ex_b_levels
    r   <- input$ex_r_blocks
    mu  <- input$ex_mu
    aamp  <- input$ex_amp_A
    bamp  <- input$ex_amp_B
    abamp <- input$ex_amp_AB
    sda <- input$ex_sd_a
    sdb <- input$ex_sd_b
    sdc <- input$ex_sd_c
    
    # Etiquetas de niveles
    A_levels <- paste0("A", seq_len(a))
    B_levels <- paste0("B", seq_len(b))
    blocks   <- paste0("Bloque", seq_len(r))
    
    # Efectos fijos de A, B e interacción
    set.seed(777)
    eff_A  <- stats::runif(a, -aamp, aamp); names(eff_A) <- A_levels
    eff_B  <- stats::runif(b, -bamp, bamp); names(eff_B) <- B_levels
    eff_AB <- matrix(
      stats::runif(a * b, -abamp, abamp),
      nrow = a,
      dimnames = list(A_levels, B_levels)
    )
    
    # Simulación por bloque
    sim <- lapply(blocks, function(k) {
      # Efectos de franjas A y B específicos del bloque (errores a y b)
      ga <- stats::rnorm(a, 0, sda); names(ga) <- A_levels
      tb <- stats::rnorm(b, 0, sdb); names(tb) <- B_levels
      
      expand.grid(col = A_levels, row = B_levels) |>
        dplyr::mutate(
          block = k,
          Y = mu +
            eff_A[col] +
            eff_B[row] +
            purrr::map2_dbl(col, row, ~ eff_AB[.x, .y]) +
            ga[col] +      # Error(a): franjas A dentro de bloque
            tb[row] +      # Error(b): franjas B dentro de bloque
            stats::rnorm(dplyr::n(), 0, sdc)  # Error(c): dentro de celda
        )
    }) |>
      dplyr::bind_rows()
    
    sim$block <- factor(sim$block)
    sim$col   <- factor(sim$col)
    sim$row   <- factor(sim$row)
    sim
  })
  
  ex_fit <- reactive({
    df <- ex_data(); shiny::req(df)
    agricolae::strip.plot(
      BLOCK = df$block,
      COL   = df$col,
      ROW   = df$row,
      Y     = df$Y
    )
  })
  
  output$ex_anova <- renderPrint({
    fit <- ex_fit(); shiny::req(fit)
    cat("== ANOVA A (vertical) ==\n")
    print(fit$ANOVA[[1]])
    cat("\n== ANOVA B (horizontal) ==\n")
    print(fit$ANOVA[[2]])
    cat("\n== ANOVA A × B (interacción) ==\n")
    print(fit$ANOVA[[3]])
    cat("\nMS/gl por estrato de error:\n")
    cat("  Ea =", fit$Ea, " | gl.a =", fit$gl.a, "\n")
    cat("  Eb =", fit$Eb, " | gl.b =", fit$gl.b, "\n")
    cat("  Ec =", fit$Ec, " | gl.c =", fit$gl.c, "\n")
  })
  
  output$ex_lsd <- renderPrint({
    fit <- ex_fit()
    df  <- ex_data()
    shiny::req(fit, df)
    
    df$AB <- interaction(df$col, df$row, drop = TRUE)
    
    cat("--- LSD para A (MS = Ea, gl = gl.a) ---\n")
    print(
      agricolae::LSD.test(
        df$Y, df$col,
        DFerror = fit$gl.a,
        MSerror = fit$Ea,
        console = TRUE
      )
    )
    
    cat("\n--- LSD para B (MS = Eb, gl = gl.b) ---\n")
    print(
      agricolae::LSD.test(
        df$Y, df$row,
        DFerror = fit$gl.b,
        MSerror = fit$Eb,
        console = TRUE
      )
    )
    
    cat("\n--- LSD para A×B (MS = Ec, gl = gl.c) ---\n")
    print(
      agricolae::LSD.test(
        df$Y, df$AB,
        DFerror = fit$gl.c,
        MSerror = fit$Ec,
        console = TRUE
      )
    )
  })
  
  output$ex_heat <- renderPlot({
    df <- ex_data(); shiny::req(df)
    means <- df |>
      dplyr::group_by(col, row) |>
      dplyr::summarise(meanY = mean(Y), .groups = "drop")
    
    ggplot2::ggplot(means, ggplot2::aes(x = row, y = col, fill = meanY)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(
        ggplot2::aes(label = sprintf("%.1f", meanY)),
        size = 3
      ) +
      ggplot2::labs(
        x = "Factor B (horizontal)",
        y = "Factor A (vertical)",
        fill = "Media",
        title = "Medias simuladas por combinación A×B"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  # Descarga de datos simulados
  output$ui_ex_descarga <- renderUI({
    shiny::req(ex_data())
    downloadButton(
      ns("ex_dl_csv"),
      "Descargar datos simulados (CSV)",
      class = "btn btn-outline-secondary w-100"
    )
  })
  
  output$ex_dl_csv <- downloadHandler(
    filename = function() {
      paste0("stripplot_simulado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      utils::write.csv(ex_data(), file, row.names = FALSE)
    }
  )
}

# Pestaña 6: Notas avanzadas
pestanna6_session6_v3_server <- function(input, output, session) {
  # Pestaña puramente conceptual / estática: no se requiere lógica de servidor.
}

# Pestaña Extra: Conexiones con otros diseños multi-estrato
pestanna_extra_session6_v3_server <- function(input, output, session) {
  # Esta pestaña es estática y educativa.
  return(NULL)
}

# Pestaña 7: Referencias
pestanna7_session6_v3_server <- function(input, output, session) {
  # Pestaña puramente informativa: no se requiere lógica de servidor.
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session6_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  has_pkg <- function(pkg) {
    requireNamespace(pkg, quietly = TRUE)
  }
  need_pkgs <- c("agricolae", "ggplot2", "dplyr", "tidyr", "DT", "stringr", "purrr")
  miss <- need_pkgs[!vapply(need_pkgs, has_pkg, logical(1))]
  if (length(miss)) {
    showModal(modalDialog(
      title = "Paquetes faltantes",
      HTML(paste0(
        "Faltan paquetes: <b>", paste(miss, collapse = ", "), "</b>.",
        "<br>Instálalos con: <code>install.packages(c(\"",
        paste(miss, collapse="\",\""), "\"))</code>"
      )),
      easyClose = TRUE
    ))
  }

  # Reactives
  design_obj <- reactiveVal(NULL)
  strip_fit <- reactiveVal(NULL)

  datos_para_analisis <- eventReactive(input$btn_correr_anova, {
    src <- input$fuente_datos

    if (identical(src, "ejemplo")) {
      # Usar un dataset manual en lugar de 'plotted' que parece no existir en CRAN agricolae actual
      # Generamos un dataset balanceado simple inspirado en yield ~ A * B
      
      # Estructura: 3 bloques, 3 niveles A (a1, a2, a3), 3 niveles B (b1, b2, b3)
      df_ex <- expand.grid(
        block = factor(1:3),
        col   = factor(c("a1", "a2", "a3")),
        row   = factor(c("b1", "b2", "b3"))
      )
      
      # Simular respuesta Y con efectos
      set.seed(999)
      # Efectos
      eff_col <- c(a1=10, a2=12, a3=15)
      eff_row <- c(b1=5, b2=5, b3=8)
      
      df_ex$Y <- 100 + 
                 eff_col[df_ex$col] + 
                 eff_row[df_ex$row] + 
                 rnorm(nrow(df_ex), 0, 2) # Ruido

      df <- df_ex
      df
    } else {
      des <- design_obj(); shiny::req(des)
      book <- des$book
      # Detección robusta de nombres
      std_cols <- c("plots", "block", "r") 
      factor_cols <- setdiff(names(book), std_cols)
      if (length(factor_cols) >= 2) {
        nm_A <- factor_cols[1]
        nm_B <- factor_cols[2]
      } else {
        nm_A <- names(book)[1]
        nm_B <- names(book)[2]
      }

      mu  <- input$media_base
      aamp <- input$efecto_A_amp
      bamp <- input$efecto_B_amp
      abamp <- input$efecto_AB_amp
      sda <- input$sd_a; sdb <- input$sd_b; sdc <- input$sd_c

      A_levels <- unique(book[[nm_A]])
      B_levels <- unique(book[[nm_B]])
      r_blocks <- sort(unique(book$block))

      set.seed(1234)
      eff_A  <- stats::runif(length(A_levels), -aamp, aamp); names(eff_A) <- A_levels
      eff_B  <- stats::runif(length(B_levels), -bamp, bamp); names(eff_B) <- B_levels
      eff_AB <- matrix(stats::runif(length(A_levels)*length(B_levels), -abamp, abamp),
                       nrow = length(A_levels), dimnames = list(A_levels, B_levels))

      sim_list <- lapply(r_blocks, function(k) {
        ga <- stats::rnorm(length(A_levels), 0, sda); names(ga) <- A_levels
        tb <- stats::rnorm(length(B_levels), 0, sdb); names(tb) <- B_levels

        blk <- subset(book, block == k)
        with(blk, {
          y <- mu +
            eff_A[book[[nm_A]]] +
            eff_B[book[[nm_B]]] +
            mapply(function(a,b) eff_AB[a,b], book[[nm_A]], book[[nm_B]]) +
            ga[book[[nm_A]]] + tb[book[[nm_B]]] +
            stats::rnorm(nrow(blk), 0, sdc)

          data.frame(
            block = factor(k),
            col   = factor(book[[nm_A]], levels = A_levels),
            row   = factor(book[[nm_B]], levels = B_levels),
            Y     = y
          )
        })
      })
      dplyr::bind_rows(sim_list)
    }
  })

  # Call tab servers
  pestanna1_session6_v3_server(input, output, session)
  pestanna2_session6_v3_server(input, output, session, design_obj, has_pkg)
  pestanna3_session6_v3_server(
    input, output, session,
    design_obj, datos_para_analisis, strip_fit, has_pkg
  )
  pestanna4_session6_v3_server(input, output, session, datos_para_analisis, strip_fit, has_pkg)
  pestanna5_session6_v3_server(
    input, output, session,
    datos_para_analisis = datos_para_analisis,
    strip_fit          = strip_fit
  )
  pestanna6_session6_v3_server(input, output, session)
  pestanna_extra_session6_v3_server(input, output, session)
  pestanna7_session6_v3_server(input, output, session)
}
