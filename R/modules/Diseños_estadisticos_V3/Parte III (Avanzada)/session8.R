# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session8.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto
pestanna1_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto",
    
    # Título general de la sesión
    h4(class = "section-header", "¿Dónde encaja el Diseño de Bloques Aumentados (DBA) en la historia del curso?"),
    
    # Bloque introductorio: problema agronómico real
    div(
      class = "alert alert-info",
      p(
        strong("Contexto típico: "),
        "etapas tempranas de mejoramiento, con ",
        em("muchas líneas nuevas"), " pero ",
        em("muy poca semilla por línea"),
        ". Solo algunos pocos genotipos (", strong("testigos o checks"),
        ") tienen suficiente semilla para repetirse en todos los bloques."
      ),
      p(
        "Un ", strong("Diseño de Bloques Aumentados (DBA)"),
        " permite evaluar decenas o cientos de líneas nuevas ",
        "replicando únicamente a los testigos. Es, en esencia, un ",
        strong("RCBD 'aumentado'"),
        " con muchos tratamientos sin réplica directa."
      )
    ),
    
    fluidRow(
      # Columna izquierda: problema práctico y roles de checks/entries
      column(
        width = 6,
        
        h5("¿Cuándo aparece el problema DBA en la práctica?"),
        tags$ul(
          tags$li(
            "Ensayos de ", strong("mejoramiento genético temprano"),
            ": cientos de líneas candidatas, pero poca semilla por línea."
          ),
          tags$li(
            "Se dispone de algunos ",
            strong("testigos (checks) robustos"),
            " (variedades comerciales, materiales de referencia) ",
            "que sí pueden repetirse en todos o casi todos los bloques."
          ),
          tags$li(
            "No es posible armar un RCBD clásico donde ",
            strong("todos los tratamientos estén replicados"),
            " en cada bloque."
          )
        ),
        
        h5("Roles dentro de un DBA"),
        tags$ul(
          tags$li(
            strong("Testigos (checks): "),
            "se repiten en todos (o casi todos) los bloques. ",
            "Sirven para estimar el ",
            strong("efecto de bloque y el error experimental"),
            ", y para calibrar el nivel general del ensayo."
          ),
          tags$li(
            strong("Nuevos (entries): "),
            "cada genotipo nuevo aparece ",
            strong("una sola vez"),
            " en todo el ensayo. Sus respuestas se ajustan ",
            "usando la información de bloques y checks."
          )
        ),
        
        div(
          class = "note-cloud",
          p(
            strong("Idea clave: "),
            "en un DBA no puedes confiar en una 'media por tratamiento' basada en réplicas, ",
            "porque los nuevos no están replicados. ",
            "Necesitas un ", strong("modelo mixto (LMM)"),
            " que combine la información de ",
            em("bloques + testigos + nuevos"),
            " para obtener estimaciones comparables."
          )
        )
      ),
      
      # Columna derecha: timeline del curso + resultados de aprendizaje
      column(
        width = 6,
        
        h5("DBA dentro de la familia de diseños del curso"),
        tags$ol(
          tags$li(
            strong("Sesión 4 – RCBD + LMM: "),
            "todos los tratamientos están replicados en cada bloque. ",
            "Un solo estrato de error (residual) y un factor de bloqueo."
          ),
          tags$li(
            strong("Sesión 5 – Split-plot: "),
            "dos unidades experimentales (whole-plot y subparcela) ",
            "→ dos estratos de error (Error A y Error B)."
          ),
          tags$li(
            strong("Sesión 6 – Strip-plot: "),
            "franjas cruzadas A×B dentro de bloque ",
            "→ tres estratos de error (a, b, c)."
          ),
          tags$li(
            strong("Sesión 7 – Fila-Columna (Row-Column): "),
            "bloqueo espacial en dos direcciones (fila y columna) ",
            "→ modelo mixto con dos componentes de varianza de bloqueo."
          ),
          tags$li(
            strong("Sesión 8 – DBA: "),
            "un RCBD 'aumentado' donde ",
            strong("solo los testigos están replicados"),
            " y los nuevos aparecen una sola vez. ",
            "Los modelos mixtos permiten extraer información razonable sobre los nuevos ",
            "a pesar de no tener réplicas directas."
          )
        ),
        
        h5("Resultados de aprendizaje de esta pestaña"),
        tags$ul(
          tags$li(
            "Ser capaz de explicar, en lenguaje sencillo, ",
            strong("qué es un Diseño de Bloques Aumentados (DBA)"),
            " y en qué contexto se usa."
          ),
          tags$li(
            "Distinguir con claridad entre ",
            strong("testigos (checks)"),
            " y ",
            strong("nuevos (entries)"),
            " y entender por qué solo los testigos se replican."
          ),
          tags$li(
            "Reconocer que el ",
            strong("análisis moderno de DBA se basa en modelos mixtos (LMM)"),
            ", donde los bloques se tratan como efectos aleatorios ",
            "y los genotipos (especialmente los nuevos) pueden modelarse como fijos o aleatorios ",
            "(BLUPs) según el objetivo de selección."
          )
        ),
        
        p(
          class = "text-muted small",
          "En las siguientes pestañas verás cómo generar un DBA con ",
          code("agricolae::design.dau"),
          " y cómo simular/analisar estos datos usando ",
          strong("augmentedRCBD"),
          " y ",
          strong("lmerTest"),
          " para obtener inferencias útiles sobre los nuevos genotipos."
        )
      )
    )
  )
}

# Pestaña 2: Conceptos clave: DBA & modelo mixto
pestanna2_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave: DBA & modelo mixto",
    
    # Título principal de la sección
    h4(class = "section-header", "Estructura de un Diseño de Bloques Aumentados (DBA)"),
    
    # Bloque de contexto breve
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "en un DBA combinamos ",
        strong("pocos testigos bien replicados"),
        " con ",
        strong("muchos genotipos nuevos sin réplica"),
        ". Los testigos permiten estimar el efecto de bloque y el error, ",
        "para poder comparar de forma justa a los nuevos."
      )
    ),
    
    fluidRow(
      # -------------------------------------------------------------------
      # Columna izquierda: estructura y columnas típicas del dataset
      # -------------------------------------------------------------------
      column(
        width = 6,
        h5("Estructura básica del DBA"),
        tags$ul(
          tags$li(
            strong("Testigos (Checks): "),
            "son tratamientos de referencia (variedades comerciales, líneas élite). ",
            "Se repiten en todos o en casi todos los bloques. ",
            "Por estar replicados, permiten estimar el ",
            em("error experimental"),
            " y el ",
            em("efecto de bloque")
          ),
          tags$li(
            strong("Nuevos (Entries): "),
            "son las líneas en evaluación temprana. ",
            "Por limitación de semilla, normalmente aparecen ",
            strong("una sola vez"),
            " en todo el ensayo."
          ),
          tags$li(
            strong("Bloques: "),
            "agru pan parcelas que comparten condiciones similares de suelo, manejo, etc. ",
            "En análisis moderno se modelan como ",
            strong("efectos aleatorios")
          )
        ),
        
        hr(),
        h5("Columnas mínimas en el archivo de datos"),
        p("Un archivo típico de DBA (por ejemplo, un Excel) debería tener al menos:"),
        tags$table(
          class = "table table-sm table-bordered",
          tags$thead(
            tags$tr(
              tags$th("Columna"),
              tags$th("Ejemplo"),
              tags$th("Rol en el análisis")
            )
          ),
          tags$tbody(
            tags$tr(
              tags$td(code("block")),
              tags$td("1, 2, 3, 4"),
              tags$td("Identifica el bloque (o repetición) en el campo")
            ),
            tags$tr(
              tags$td(code("trt")),
              tags$td("C1, C2, N1, N2, ..."),
              tags$td("Nombre del tratamiento (check o nuevo)")
            ),
            tags$tr(
              tags$td(code("tipo")),
              tags$td("Check, New"),
              tags$td("Clasifica si el tratamiento es testigo o nuevo")
            ),
            tags$tr(
              tags$td(code("Y")),
              tags$td("kg/ha, ºBrix, firmeza, ..."),
              tags$td("Variable respuesta que queremos comparar")
            )
          )
        ),
        p(
          class = "text-muted small",
          "En la práctica pueden existir más columnas (parcela, sector, coordenadas, etc.), ",
          "pero estas son las esenciales para el análisis estadístico."
        )
      ),
      
      # -------------------------------------------------------------------
      # Columna derecha: modelo mixto y lógica de análisis
      # -------------------------------------------------------------------
      column(
        width = 6,
        h5("Modelo mixto (LMM) típico para un DBA"),
        p(
          "Una forma sencilla de escribir el modelo (ignorando otras covariables) es:"
        ),
        withMathJax(
          helpText("$$Y_{ij} = \\mu + B_i + G_j + \\epsilon_{ij}$$")
        ),
        tags$ul(
          tags$li(
            HTML("<b>\\(Y_{ij}\\)</b>: respuesta observada en el bloque <i>i</i> y genotipo (tratamiento) <i>j</i>.")
          ),
          tags$li(
            HTML("<b>\\(\\mu\\)</b>: media general del ensayo.")
          ),
          tags$li(
            HTML("<b>\\(B_i\\)</b>: efecto del bloque <i>i</i> (normalmente tratado como <b>aleatorio</b>).")
          ),
          tags$li(
            HTML("<b>\\(G_j\\)</b>: efecto del genotipo <i>j</i>. "),
            "En mejoramiento temprano suele modelarse como ",
            strong("aleatorio"),
            " para obtener ",
            strong("BLUPs"),
            " y hacer ranking de materiales. ",
            "En otros contextos puede tratarse como fijo."
          ),
          tags$li(
            HTML("<b>\\(\\epsilon_{ij}\\)</b>: error residual (variación no explicada).")
          )
        ),
        
        div(
          class = "note-cloud",
          p(
            strong("Papel de los testigos: "),
            "al estar replicados en varios bloques, los testigos suministran la información necesaria ",
            "para estimar la variabilidad entre bloques (", em("varianza de bloque"), ") y la ",
            "varianza residual. Con esas estimaciones, el modelo puede 'corregir' las observaciones ",
            "de los genotipos nuevos y producir comparaciones más justas."
          )
        ),
        br(),
        h5("De los ajustes intra-bloque al enfoque moderno"),
        p(
          "En los trabajos clásicos de Federer, el DBA se analizaba mediante ajustes ",
          em("intra-bloque"),
          " manuales: se corregía cada parcela por la media de testigos en su bloque. "
        ),
        p(
          "Hoy en día, el enfoque recomendado es usar un ",
          strong("modelo mixto (LMM)"),
          " que reproduce esa lógica de forma más general y flexible. ",
          "Ventajas del LMM:"
        ),
        tags$ul(
          tags$li("Permite estimar componentes de varianza (bloque, genotipo, residual)."),
          tags$li("Permite obtener BLUPs de genotipos nuevos, útiles para ranking y selección."),
          tags$li("Tolera estructuras más complejas (bloques incompletos, covariables, etc.).")
        ),
        p(
          class = "text-muted small",
          "En las siguientes pestañas verás cómo se genera el diseño (", code("design.dau"),
          ") y cómo se simula/analiza un DBA realista con modelos mixtos."
        )
      )
    )
  )
}

# Pestaña 3: Diseño & fieldbook DBA
pestanna3_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & fieldbook DBA",
    
    h4(class = "section-header", "¿Cómo luce un Diseño de Bloques Aumentados en el fieldbook?"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Objetivo de esta pestaña: "),
        "ver en concreto cómo se materializa un DBA en el libro de campo: ",
        em("qué filas son testigos, cuáles son nuevos, y cómo se reparten por bloque.")
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros del diseño"),
        textInput(
          ns("checks_txt"),
          "Testigos (separados por coma):",
          value = "CheckA, CheckB, CheckC"
        ),
        numericInput(
          ns("n_new"),
          "Número de Nuevos Tratamientos (entries):",
          value = 90,
          min = 10,
          step = 1
        ),
        numericInput(
          ns("n_blocks"),
          "Número de Bloques:",
          value = 5,
          min = 2,
          step = 1
        ),
        actionButton(
          ns("btn_gen_dba"),
          "Generar diseño DBA",
          class = "btn btn-primary w-100 mt-2"
        ),
        tags$hr(),
        uiOutput(ns("ui_dl_dba"))
      ),
      
      mainPanel(
        width = 8,
        
        tags$h5("Resumen del diseño (lectura pedagógica)"),
        verbatimTextOutput(ns("out_dba_summary")),
        
        tags$hr(),
        tags$h5("Fieldbook (primeras filas)"),
        DT::dataTableOutput(ns("tbl_dba_book")),
        
        tags$hr(),
        tags$h5("Distribución en bloques: checks vs nuevos"),
        plotOutput(ns("plot_dba_map"), height = "360px"),
        helpText(
          "En el eje X se muestran los bloques; en el eje Y, la posición dentro de cada bloque. ",
          "El color distingue entre parcelas de testigos (checks) y de nuevos (entries)."
        )
      )
    )
  )
}

# Pestaña 4: Simulación & análisis: clásico vs LMM (BLUPs)
pestanna4_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & análisis",
    
    # Texto introductorio
    p(
      "En esta pestaña vas a ",
      strong("simular un ensayo de bloques aumentados (DBA)"),
      " y comparar dos enfoques de análisis:",
      br(),
      "1) Un análisis clásico tipo ", code("augmentedRCBD"), " (ajuste intra-bloque).", br(),
      "2) Un modelo mixto (LMM) con bloques y genotipos aleatorios, que entrega ",
      strong("BLUPs"),
      " para apoyar la selección."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros de simulación"),
        helpText(
          "Estos parámetros controlan la señal genética y la variabilidad de bloque y residual ",
          "sobre el diseño generado en la pestaña 3."
        ),
        numericInput(ns("sim_mu"), "Media general (μ):", value = 100),
        numericInput(ns("sim_sigma_g"), "SD Genotipos (σ_G: variación genética):", value = 10),
        numericInput(ns("sim_sigma_b"), "SD Bloques (σ_B: gradiente entre bloques):", value = 15),
        numericInput(ns("sim_sigma_e"), "SD Residual (σ_e: ruido dentro de bloque):", value = 5),
        br(),
        actionButton(
          ns("btn_sim_dba"),
          "Simular y analizar DBA",
          class = "btn btn-success w-100"
        ),
        helpText(
          "Paso recomendado: primero genera el diseño en la pestaña 3, ",
          "luego simula aquí con distintos niveles de σ_G, σ_B y σ_e."
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # ----------------------------------------------------------
          # Panel 4.1: Visualización del ensayo simulado
          # ----------------------------------------------------------
          bslib::nav_panel(
            "4.1 Visualización del ensayo",
            tags$h5("Mapa del ensayo DBA simulado"),
            plotOutput(ns("plot_sim_layout"), height = "360px"),
            helpText(
              "Cada celda representa una parcela en un bloque. El color indica el valor observado de Y.",
              "Las facetas distinguen ",
              strong("testigos (Checks)"),
              " de ",
              strong("nuevos (New)"),
              ". Así puedes ver cómo se combinan los efectos de bloque y genotipo."
            )
          ),
          
          # ----------------------------------------------------------
          # Panel 4.2: Análisis clásico
          # ----------------------------------------------------------
          bslib::nav_panel(
            "4.2 Análisis clásico (augmentedRCBD)",
            tags$h5("Enfoque tipo Federer / augmentedRCBD"),
            p(
              "Si el paquete ", code("augmentedRCBD"), " está instalado, se usa su función ",
              code("augmentedRCBD()"),
              " para reproducir el ajuste intra-bloque clásico de los DBA:",
              " los ", strong("checks"), " sirven como base para ajustar las medias de los ",
              strong("nuevos"), " y estimar el error residual."
            ),
            p(
              "Si el paquete no está disponible, se muestra un ",
              strong("ANOVA simple con ", code("lm()")),
              " solo como referencia pedagógica (no es el análisis ideal para DBA)."
            ),
            verbatimTextOutput(ns("out_aug_classic"))
          ),
          
          # ----------------------------------------------------------
          # Panel 4.3: Análisis mixto (LMM) y BLUPs
          # ----------------------------------------------------------
          bslib::nav_panel(
            "4.3 Análisis mixto (LMM) & BLUPs",
            tags$h5("Modelo mixto: bloques y genotipos aleatorios"),
            p(
              "Aquí ajustamos un modelo mixto del tipo:",
              br(),
              code("Y ~ 1 + (1|block) + (1|trt)"),
              br(),
              "donde tanto ", strong("bloques"), " como ", strong("genotipos"),
              " se tratan como efectos aleatorios."
            ),
            tags$div(
              class = "alert alert-secondary",
              "Interpretación didáctica:",
              tags$ul(
                tags$li("Los bloques capturan variación ambiental entre bloques (σ²_B)."),
                tags$li("Los genotipos capturan variación genética (σ²_G)."),
                tags$li(
                  "Los ",
                  strong("BLUPs"),
                  " resultantes 'encogen' las estimaciones de genotipos con pocos datos ",
                  "hacia la media, lo cual es deseable en etapas tempranas de mejoramiento."
                )
              )
            ),
            tags$h5("Resumen del modelo mixto"),
            verbatimTextOutput(ns("out_aug_lmm")),
            tags$hr(),
            tags$h5("BLUPs de genotipos (Top 20)"),
            plotOutput(ns("plot_blups"), height = "320px"),
            helpText(
              "Las barras muestran los BLUPs de cada genotipo. Observa cómo se rankean ",
              "los nuevos en relación con los testigos."
            )
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales (DBA)
pestanna5_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo (datos reales)",
    
    h4(class = "section-header",
       "Protocolo para analizar un Diseño de Bloques Aumentados (DBA) con datos reales"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Idea central: "),
        "esta pestaña resume, en modo checklist, cómo pasar de un ",
        em("Excel real"),
        " con un DBA a un análisis correcto usando ",
        strong("augmentedRCBD"),
        " y/o ",
        strong("modelos mixtos (LMM)"),
        "."
      ),
      p(
        "La estructura de columnas que se describe aquí es la misma que ya viste en la pestaña ",
        strong("3) Diseño & fieldbook"),
        " (simulación del DBA) y el tipo de análisis coincide con lo mostrado en la pestaña ",
        strong("4) Simulación & análisis: clásico vs LMM (BLUPs)"),
        "."
      )
    ),
    
    # -------------------------------------------------------------------
    # Paso 0 – Entender el diseño
    # -------------------------------------------------------------------
    h4("Paso 0 – Entender el diseño (revisar el Excel)"),
    p(
      "Antes de correr ningún modelo, necesitas tener claro cómo está armado el ensayo y cómo está ",
      strong("estructurado tu archivo de datos"),
      ". Estas son las columnas mínimas recomendadas en tu Excel:"
    ),
    
    tags$table(
      class = "table table-sm table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Columna"),
          tags$th("Ejemplo"),
          tags$th("Rol en el DBA")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td(code("block")),
          tags$td("1, 2, 3, 4, …"),
          tags$td("Identificador de bloque (o repetición). Captura gradientes de suelo / manejo.")
        ),
        tags$tr(
          tags$td(code("trt")),
          tags$td("C1, C2, N1, N2, …"),
          tags$td("Identificador del genotipo o tratamiento (testigos y nuevos).")
        ),
        tags$tr(
          tags$td(code("tipo")),
          tags$td("\"Check\" / \"New\""),
          tags$td("Categoría del tratamiento: testigo replicado vs. nuevo con 1 parcela.")
        ),
        tags$tr(
          tags$td(code("Y")),
          tags$td("kg/ha, ºBrix, firmeza, etc."),
          tags$td("Respuesta principal a analizar.")
        ),
        tags$tr(
          tags$td(code("parcela"), style = "font-style: italic;"),
          tags$td("opcional"),
          tags$td("ID único de parcela, útil para trazabilidad, no obligatorio.")
        )
      )
    ),
    
    p(
      strong("Preguntas de chequeo (diseño DBA real):")
    ),
    tags$ul(
      tags$li(
        "¿Cuántos ",
        strong("testigos (checks)"),
        " distintos tienes? ¿Cuántas veces se replica cada uno en el ensayo?"
      ),
      tags$li(
        "¿Cada ",
        strong("nuevo (entry)"),
        " aparece solo una vez en todo el ensayo (una parcela por genotipo)?"
      ),
      tags$li(
        "¿Los bloques tienen un tamaño razonablemente similar (número de parcelas por bloque)? ",
        "Si hay bloques muy pequeños o muy grandes, tu precisión puede cambiar entre bloques."
      ),
      tags$li(
        "¿La estructura de tu Excel es compatible con el fieldbook que viste en la pestaña ",
        strong("3) Diseño & fieldbook DBA"),
        " (columnas ",
        code("block"), ", ", code("trt"), ", ", code("Y"),
        ", y eventualmente ", code("tipo"),
        ")?"
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------
    # Paso 1 – Explorar balance
    # -------------------------------------------------------------------
    h4("Paso 1 – Explorar el balance y la distribución de tratamientos"),
    p(
      "El siguiente paso es evaluar si el diseño está razonablemente balanceado. ",
      "En R (fuera de la app), podrías usar comandos tipo:"
    ),
    tags$pre(
      class = "code-block",
      "library(dplyr)\n\n",
      "# 1) ¿Cuántos checks y nuevos por bloque?\n",
      "datos %>%\n",
      "  count(block, tipo)\n\n",
      "# 2) Verificar que cada nuevo aparece una sola vez\n",
      "datos %>%\n",
      "  count(trt) %>%\n",
      "  arrange(n)   # los \"New\" deberían tener n = 1\n\n",
      "# 3) Tabla de distribución por bloque y tipo\n",
      "with(datos, table(block, tipo))"
    ),
    p(
      strong("Interpretación didáctica:"),
      " si ves que cada testigo aparece en todos (o casi todos) los bloques, ",
      "y que los nuevos aparecen una sola vez, entonces tu ensayo está ",
      em("alineado con la estructura DBA estándar"),
      "."
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------
    # Paso 2 – Modelos candidatos
    # -------------------------------------------------------------------
    h4("Paso 2 – Modelos candidatos para el análisis"),
    p(
      "En esta sesión vimos dos enfoques complementarios (reproducidos en la pestaña ",
      strong("4) Simulación & análisis"),
      "):"
    ),
    
    tags$h5("2.1 Modelo clásico (ajuste tipo Federer / augmentedRCBD)"),
    p(
      "Si tienes instalado el paquete ",
      code("augmentedRCBD"),
      ", puedes seguir la lógica clásica de los diseños aumentados. ",
      "La idea es ajustar las medias de los nuevos usando los checks dentro de cada bloque ",
      "y usar solo los testigos para estimar el error residual."
    ),
    tags$pre(
      class = "code-block",
      "library(augmentedRCBD)\n\n",
      "res_classic <- augmentedRCBD(\n",
      "  block = datos$block,\n",
      "  trt   = datos$trt,\n",
      "  y     = datos$Y,\n",
      "  method.comp = \"lsd\",\n",
      "  alpha = 0.05,\n",
      "  group = FALSE,\n",
      "  console = TRUE\n",
      ")"
    ),
    p(
      "Este enfoque reproduce el ajuste intra-bloque descrito por ",
      strong("Federer (1956)"),
      ": las diferencias entre nuevos se comparan a través de su comportamiento frente a los testigos ",
      "dentro de cada bloque."
    ),
    
    tags$h5("2.2 Modelo mixto recomendado (LMM)"),
    p(
      "El análisis moderno en mejoramiento suele usar ",
      strong("modelos mixtos (LMM)"),
      " donde tanto los bloques como los genotipos se tratan como efectos aleatorios, ",
      "obteniendo ",
      strong("BLUPs"),
      " que combinan la observación individual con la estructura del ensayo."
    ),
    p("Un modelo base típico es:"),
    tags$pre(
      class = "code-block",
      "library(lme4)\n\n",
      "m_lmm <- lmer(\n",
      "  Y ~ 1 + (1 | block) + (1 | trt),\n",
      "  data = datos\n",
      ")"
    ),
    tags$ul(
      tags$li(
        code("(1 | block)"), ": bloque aleatorio → captura diferencias sistemáticas entre bloques."
      ),
      tags$li(
        code("(1 | trt)"), ": genotipo aleatorio → produce BLUPs para cada trt, ",
        "especialmente útil cuando la mayoría de los genotipos tiene una sola parcela."
      )
    ),
    p(
      "En fases más avanzadas podrías considerar modelos con ",
      em("checks fijos"),
      " y ",
      em("nuevos aleatorios"),
      " (por ejemplo, usando una variable ",
      code("tipo"),
      "), pero para la enseñanza de esta sesión mantenemos el modelo base completo aleatorio para los genotipos."
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------
    # Paso 3 – Outputs clave
    # -------------------------------------------------------------------
    h4("Paso 3 – Outputs clave a revisar"),
    
    tags$h5("3.1 Desde el análisis clásico (augmentedRCBD)"),
    tags$ul(
      tags$li(
        strong("Tabla ANOVA: "),
        "permite ver si hay diferencias globales significativas entre tratamientos."
      ),
      tags$li(
        strong("Medias ajustadas (checks y nuevos): "),
        "muestran el rendimiento corregido por bloque."
      ),
      tags$li(
        strong("Comparaciones post-hoc (LSD u otras): "),
        "para saber qué nuevos superan a los testigos."
      )
    ),
    
    tags$h5("3.2 Desde el modelo mixto (LMM)"),
    p(
      "En la pestaña ",
      strong("4) Simulación & análisis: clásico vs LMM"),
      " ya viste cómo inspeccionar estos elementos:"
    ),
    tags$ul(
      tags$li(
        strong("Componentes de varianza (VarCorr(m)): "),
        "valores de ",
        code("σ²_block"), ", ", code("σ²_trt"), " y ", code("σ²_residual"),
        ". ",
        "Te dicen qué proporción de la variabilidad total está en bloque, en genotipos, y en el residuo."
      ),
      tags$li(
        strong("BLUPs de genotipos (ranef(m)$trt): "),
        "efectos aleatorios estimados para cada genotipo. ",
        "En la app se grafican los ",
        em("Top 20"),
        " para ilustrar el ranking."
      ),
      tags$li(
        "Opcionalmente puedes obtener ",
        strong("EMMs / medias ajustadas"),
        " con paquetes como ",
        code("emmeans"),
        ", si quieres reportar medias en la escala original con intervalos de confianza."
      )
    ),
    
    tags$hr(),
    
    # -------------------------------------------------------------------
    # Paso 4 – Redacción agronómica
    # -------------------------------------------------------------------
    h4("Paso 4 – Redacción agronómica del informe"),
    p(
      "Finalmente, toda la estadística debe traducirse en un ",
      strong("mensaje agronómico interpretable"),
      ". ",
      "Una plantilla simple que pueden usar en sus reportes (por ejemplo, en Quarto/RMarkdown) es:"
    ),
    tags$pre(
      class = "code-block",
      "Ejemplo de texto para un informe:\n\n",
      "  \"Se evaluaron N líneas nuevas y k testigos en un diseño de bloques\n",
      "   aumentados con b bloques. Los testigos se usaron para estimar el error\n",
      "   y corregir efectos de bloque mediante un modelo mixto\n",
      "   Y ~ (1|block) + (1|trt).\n\n",
      "   Los BLUPs de genotipo indicaron que las líneas L1, L2 y L3 superan\n",
      "   consistentemente a los testigos en rendimiento (kg/ha), con diferencias\n",
      "   medias ajustadas del orden de +Δ ± SE. Estos resultados sustentan la\n",
      "   selección de dichas líneas para etapas posteriores del programa de\n",
      "   mejoramiento.\""
    ),
    p(
      "Puedes adaptar este texto para incluir otras variables (firmeza, ºBrix, etc.) ",
      "y para incorporar decisiones específicas de tu programa (por ejemplo, ",
      "criterios de corte para avanzar líneas a la siguiente etapa)."
    )
    
    # Nota: si en el futuro quisieras añadir una plantilla Quarto descargable,
    # podrías agregar aquí un downloadButton y manejarlo en el server.
  )
}

# Pestaña 6: Ejercicios prácticos & tareas
pestanna6_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios prácticos & tareas",
    
    # Breve introducción
    p(
      "En esta pestaña se proponen ejercicios para conectar las distintas partes de la sesión: ",
      strong("3) Diseño & fieldbook DBA"), ", ",
      strong("4) Simulación & análisis"), " y ",
      strong("5) Protocolo para datos reales"),
      ". La idea es que puedas practicar el flujo completo: ",
      em("diseñar → simular → analizar → redactar conclusiones agronómicas.")
    ),
    
    tags$ol(
      tags$li(
        strong("Ejercicio 1 – Construir el diseño DBA en la app:"),
        tags$ul(
          tags$li(
            "Ve a la pestaña ", strong("3) Diseño & fieldbook DBA"),
            " y genera un diseño con: ",
            code("200 líneas nuevas"), " y ", code("4 testigos"),
            " distribuidos en ", code("10 bloques"), "."
          ),
          tags$li(
            "Registra en tu cuaderno: ",
            tags$ul(
              tags$li("Número total de parcelas (filas del fieldbook)."),
              tags$li("Cuántas parcelas de ", em("checks"), " por bloque."),
              tags$li("Cuántas parcelas de ", em("nuevos (entries)"), " por bloque.")
            )
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 2 – Simular un ensayo con alta variabilidad de bloque:"),
        tags$ul(
          tags$li(
            "Con el diseño generado en el Ejercicio 1, ve a la pestaña ",
            strong("4) Simulación & análisis: clásico vs LMM"), "."
          ),
          tags$li(
            "Fija los parámetros de simulación (panel izquierdo) en algo como:",
            tags$ul(
              tags$li(HTML("&sigma;<sub>block</sub> = 20 (variabilidad alta entre bloques).")),
              tags$li(HTML("&sigma;<sub>genotipo</sub> = 10.")),
              tags$li(HTML("&sigma;<sub>residual</sub> = 5."))
            )
          ),
          tags$li(
            "Presiona ", strong("Simular y Analizar"),
            " y observa el mapa/heatmap de Y en el panel de visualización."
          ),
          tags$li(
            "Pregunta guía: ¿Se ve el efecto de bloque en el patrón de colores? ",
            "¿Hay bloques claramente más altos o más bajos que otros?"
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 3 – Comparar análisis clásico vs LMM:"),
        tags$ul(
          tags$li(
            "En la pestaña ", strong("4) Simulación & análisis"), ", revisa primero el panel ",
            strong("Análisis clásico (augmentedRCBD)"),
            ". Si el paquete está instalado, verás la salida de ",
            code("augmentedRCBD::augmentedRCBD()"),
            "; de lo contrario, verás un ANOVA aproximado con ", code("lm()"), "."
          ),
          tags$li(
            "Luego revisa el panel ", strong("Análisis mixto (LMM): BLUPs de genotipos"),
            " donde se ajusta un modelo del tipo ",
            code("Y ~ (1|block) + (1|trt)"),
            " y se obtienen BLUPs para cada genotipo."
          ),
          tags$li(
            "Preguntas guía:",
            tags$ul(
              tags$li("¿Coinciden, al menos en parte, los genotipos destacados en ambos enfoques?"),
              tags$li("¿Cambia el orden de los mejores nuevos cuando miras los BLUPs del LMM?"),
              tags$li("Si hay genotipos con una sola observación muy alta, ¿el LMM los ‘modera’ (shrinkage) frente al análisis clásico?")
            )
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 4 – Aplicar el protocolo a modo ‘datos reales’:"),
        tags$ul(
          tags$li(
            "Usa la pestaña ", strong("5) Protocolo para datos reales (DBA"),
            ") como guía. Imagina que los datos simulados fueran un ensayo real de mejoramiento."
          ),
          tags$li(
            "Fuera de la app (en tu cuaderno o en un archivo aparte), escribe un mini-reporte de ",
            strong("3–4 líneas"),
            " que responda, por ejemplo:"
          ),
          tags$li(
            tags$ul(
              tags$li("¿Cuántas líneas nuevas y cuántos testigos se evaluaron?"),
              tags$li("¿Qué modelo usaste para analizar (clásico / LMM) y por qué?"),
              tags$li("¿Qué genotipos parecen superiores a los testigos en rendimiento medio?"),
              tags$li("¿Qué tan confiable consideras esa conclusión, dado que los nuevos solo tienen una observación?")
            )
          )
        )
      ),
      
      tags$li(
        strong("Ejercicio 5 – Extensión / tarea (fuera de la app):"),
        tags$ul(
          tags$li(
            "Exporta el ", strong("fieldbook"), " desde la pestaña ",
            strong("3) Diseño & fieldbook DBA"),
            " usando el botón de descarga."
          ),
          tags$li(
            "Si tu flujo incluye exportar BLUPs desde el modelo LMM, guarda también esa tabla",
            " (o reconstruye los BLUPs con código en un script de R)."
          ),
          tags$li(
            "Repite el análisis en otro entorno (por ejemplo, un script en R, ASReml, SAS, etc.) ",
            "siguiendo los mismos pasos del protocolo, y verifica que obtienes conclusiones similares."
          )
        )
      )
    ),
    
    hr(),
    tags$h5("Descargar datos simulados de ejemplo (opcional)"),
    helpText(
      "Si ya corriste una simulación en la pestaña 4) con el botón ",
      "\"Simular y Analizar\", puedes descargar el data set simulado actual ",
      "para practicar fuera de la app (por ejemplo en un script de R o en una plantilla Quarto)."
    ),
    downloadButton(
      ns("dl_dba_example"),
      "Descargar datos simulados (CSV)",
      class = "btn btn-secondary"
    )
  )
}

# Pestaña 7: Referencias (DBA)
pestanna7_session8_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    
    h4(class = "section-header", "Referencias para Diseños de Bloques Aumentados (DBA)"),
    
    p(
      "Esta pestaña resume referencias clave para: ",
      em("diseño"), " de DBA, ",
      em("análisis clásico"), " tipo Federer/augmentedRCBD y ",
      em("análisis moderno"), " con modelos mixtos y BLUPs en mejoramiento."
    ),
    
    # --------------------------------------------------------------------
    # 1) Diseño DBA clásico
    # --------------------------------------------------------------------
    h5("1) Diseño DBA clásico"),
    tags$ul(
      tags$li(
        strong("Federer, W. T. (1956). "),
        em("Augmented (or Hoonuiaku) Designs."),
        " Artículo fundacional donde se introduce formalmente la idea ",
        "de bloques aumentados: testigos replicados + muchos tratamientos sin réplica."
      ),
      tags$li(
        "Capítulos sobre diseños para ensayos de mejoramiento temprano en libros de diseño experimental ",
        "y biometría agrícola (por ejemplo, textos clásicos de diseño de experimentos en agricultura)."
      )
    ),
    
    # --------------------------------------------------------------------
    # 2) Implementación y análisis clásico en R
    # --------------------------------------------------------------------
    h5("2) Implementación y análisis clásico en R"),
    tags$ul(
      tags$li(
        strong("Paquete "),
        code("agricolae"),
        ": función ",
        code("design.dau()"),
        " para construir diseños de bloques aumentados. Documentación en CRAN y tutoriales del autor ",
        "explican la parametrización de testigos (", code("trt1"), ") y nuevos (", code("trt2"), ")."
      ),
      tags$li(
        strong("Tutoriales de agricolae (De Mendiburu)"),
        ": manuales en PDF donde se muestran ejemplos de ",
        code("design.dau"), " y otros diseños agrícolas avanzados."
      ),
      tags$li(
        strong("Paquete "),
        code("augmentedRCBD"),
        ": análisis clásico de diseños de bloques aumentados. ",
        "Incluye funciones para:",
        tags$ul(
          tags$li("Estimar ANOVA basada en testigos."),
          tags$li("Ajustar medias de nuevos (entries) usando el efecto de bloque."),
          tags$li("Realizar comparaciones tipo LSD entre nuevos y testigos.")
        ),
        "Documentación y vignette disponibles en CRAN: ",
        tags$a(
          href = "https://cran.r-project.org/package=augmentedRCBD",
          target = "_blank",
          "https://cran.r-project.org/package=augmentedRCBD"
        )
      )
    ),
    
    # --------------------------------------------------------------------
    # 3) Modelos mixtos y BLUPs en mejoramiento
    # --------------------------------------------------------------------
    h5("3) Modelos mixtos y BLUPs en mejoramiento"),
    tags$ul(
      tags$li(
        strong("Stroup, W. W. (2013). "),
        em("Generalized Linear Mixed Models: Modern Concepts, Methods and Applications."),
        " Libro de referencia para entender cómo formular diseños agrícolas ",
        "como modelos mixtos (REML, componentes de varianza, efectos aleatorios de bloque y genotipo)."
      ),
      tags$li(
        strong("Textos de mejoramiento vegetal y análisis de ensayos"),
        ": capítulos dedicados al uso de BLUPs para ranking de genotipos, ",
        "tratando los genotipos como efectos aleatorios y los bloques como efectos aleatorios de ambiente."
      ),
      tags$li(
        strong("Documentación de "),
        code("lme4"),
        " y ",
        code("lmerTest"),
        ": explicación de la sintaxis ",
        code("lmer(Y ~ 1 + (1|block) + (1|trt), data = ...)"),
        " y cómo obtener componentes de varianza y efectos aleatorios (BLUPs)."
      )
    ),
    
    # --------------------------------------------------------------------
    # 4) Notas de cursos y recursos prácticos
    # --------------------------------------------------------------------
    h5("4) Notas de cursos y recursos prácticos"),
    tags$ul(
      tags$li(
        "Notas de cursos de diseño de experimentos y mejoramiento de universidades agrícolas ",
        "(por ejemplo, módulos donde se tratan ensayos con tratamientos no replicados ",
        "y el uso de testigos para estimar el error)."
      ),
      tags$li(
        "Vignettes y ejemplos reproducibles incluidos en los paquetes ",
        code("agricolae"), " y ",
        code("augmentedRCBD"),
        ", que muestran flujos completos desde el diseño hasta el análisis."
      ),
      tags$li(
        "Informes técnicos y manuales de programas de mejoramiento (p. ej. instituciones públicas o centros internacionales) ",
        "que aplican DBA en etapas tempranas y discuten su integración con análisis multiambiente y selección genética."
      )
    ),
    
    hr(),
    div(
      class = "alert alert-info",
      p(
        strong("Sugerencia de uso: "),
        "usa estas referencias como complemento de la app. La app te guía en el ",
        em("cómo hacer"),
        " (diseñar, simular, ajustar modelos), y las referencias te dan el ",
        em("por qué"),
        " estadístico y agronómico de los DBA."
      )
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session8_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 8: Diseños de Bloques Aumentados (DBA)")
    ),
    navset_tab(
      pestanna1_session8_v3UI(ns),
      pestanna2_session8_v3UI(ns),
      pestanna3_session8_v3UI(ns),
      pestanna4_session8_v3UI(ns),
      pestanna5_session8_v3UI(ns),
      pestanna6_session8_v3UI(ns),
      pestanna7_session8_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto
pestanna1_session8_v3_server <- function(input, output, session) {
  # Pestaña conceptual: no se requiere lógica de servidor.
}

# Pestaña 2: Conceptos clave
pestanna2_session8_v3_server <- function(input, output, session) {
  # Pestaña conceptual: no requiere lógica de servidor.
}

# Pestaña 3: Diseño & fieldbook DBA
pestanna3_session8_v3_server <- function(input, output, session, dba_design, has_pkg) {
  ns <- session$ns
  
  # -------------------------------------------------------------------
  # 1) Generar diseño DBA con agricolae::design.dau
  # -------------------------------------------------------------------
  observeEvent(input$btn_gen_dba, {
    if (!has_pkg("agricolae")) {
      showNotification("Instale 'agricolae' para usar esta función (design.dau).", type = "error")
      return()
    }
    
    # Parseo de testigos
    checks <- strsplit(input$checks_txt, "\\s*,\\s*")[[1]]
    checks <- checks[checks != ""]
    n_new  <- input$n_new
    n_blocks <- input$n_blocks
    
    shiny::validate(
      shiny::need(length(checks) >= 1, "Debe ingresar al menos 1 testigo."),
      shiny::need(length(checks) >= 2, "Se recomiendan al menos 2 testigos para estimar el error correctamente (DF = (b-1)*(c-1))."),
      shiny::need(n_new >= 1,          "Debe haber al menos 1 tratamiento nuevo."),
      shiny::need(n_blocks >= 2,       "Se requieren al menos 2 bloques.")
    )
    
    # Pequeña advertencia didáctica (no detiene el proceso)
    if (n_new < n_blocks) {
      showNotification(
        "Advertencia: hay muy pocos tratamientos nuevos en relación con el número de bloques; revise si esto es razonable.",
        type = "warning",
        duration = 6
      )
    }
    
    # Nombres de nuevos
    new_trts <- paste0("New", seq_len(n_new))
    
    # Intento de generación del diseño
    tryCatch({
      des <- agricolae::design.dau(
        trt1  = checks,      # testigos
        trt2  = new_trts,    # nuevos
        r     = n_blocks,    # número de bloques
        serie = 2,
        seed  = 123
      )
      dba_design(des)
      showNotification("Diseño DBA generado correctamente (agricolae::design.dau).", type = "message")
      
    }, error = function(e) {
      showNotification(
        paste("Error generando el diseño DBA con design.dau:", e$message),
        type = "error",
        duration = 8
      )
    })
  })
  
  # -------------------------------------------------------------------
  # 2) Resumen pedagógico del diseño
  # -------------------------------------------------------------------
  output$out_dba_summary <- renderPrint({
    des <- dba_design(); shiny::req(des)
    
    params <- des$parameters
    book   <- des$book
    
    # Extraer listas de tratamientos desde 'parameters' si existen;
    # si no, inferir desde el fieldbook.
    checks <- if (!is.null(params$trt1)) params$trt1 else {
      # fallback: nombres que aparezcan más de una vez (proxy de checks)
      tbl <- table(book$trt)
      names(tbl)[tbl > 1]
    }
    news <- if (!is.null(params$trt2)) params$trt2 else {
      setdiff(unique(book$trt), checks)
    }
    
    k_checks  <- length(checks)
    n_new     <- length(news)
    bloques   <- sort(unique(book$block))
    b         <- length(bloques)
    n_parc    <- nrow(book)
    
    cat("=== Resumen del Diseño de Bloques Aumentados (DBA) ===\n\n")
    cat("Número de bloques (b):       ", b, " (bloques: ", paste(bloques, collapse = ", "), ")\n", sep = "")
    cat("Nº de testigos (checks):     ", k_checks, "\n", sep = "")
    cat("   Checks: ", paste(checks, collapse = ", "), "\n", sep = "")
    cat("Nº de nuevos (entries):      ", n_new, "\n", sep = "")
    cat("Total de parcelas (nrow):    ", n_parc, "\n\n", sep = "")
    
    # Réplicas por tratamiento
    tbl_trt <- table(book$trt)
    rep_checks <- tbl_trt[names(tbl_trt) %in% checks]
    rep_news   <- tbl_trt[names(tbl_trt) %in% news]
    
    if (length(rep_checks)) {
      cat("Réplicas por check (mín–máx): ",
          min(rep_checks), " – ", max(rep_checks), "\n", sep = "")
    }
    if (length(rep_news)) {
      cat("Réplicas por nuevos (entries): ",
          paste(unique(rep_news), collapse = ", "), "\n", sep = "")
    }
    
    cat("\nLectura didáctica:\n")
    cat("- Los testigos (checks) se repiten en varios bloques y permiten estimar el efecto de bloque\n")
    cat("  y la variabilidad residual.\n")
    cat("- Los nuevos (entries) suelen aparecer una sola vez en todo el ensayo y se ajustan usando\n")
    cat("  la información de los checks.\n")
  })
  
  # -------------------------------------------------------------------
  # 3) Fieldbook (primeras filas)
  # -------------------------------------------------------------------
  output$tbl_dba_book <- DT::renderDataTable({
    des <- dba_design(); shiny::req(des)
    DT::datatable(
      head(des$book, 30),
      options  = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -------------------------------------------------------------------
  # 4) Mapa por bloque: checks vs nuevos
  # -------------------------------------------------------------------
  output$plot_dba_map <- renderPlot({
    des <- dba_design(); shiny::req(des)
    fb <- des$book
    
    # Necesitamos dplyr y ggplot2
    fb <- fb |>
      dplyr::group_by(block) |>
      dplyr::mutate(pos = dplyr::row_number()) |>
      dplyr::ungroup()
    
    checks <- if (!is.null(des$parameters$trt1)) des$parameters$trt1 else {
      # fallback: cualquier tratamiento con más de 1 réplica lo tratamos como check
      tbl <- table(fb$trt)
      names(tbl)[tbl > 1]
    }
    
    fb$Type <- ifelse(fb$trt %in% checks, "Check", "New")
    
    ggplot2::ggplot(
      fb,
      ggplot2::aes(
        x     = factor(.data[["block"]]),
        y     = .data[["pos"]],
        fill  = .data[["Type"]],
        label = .data[["trt"]]
      )
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(size = 3) +
      ggplot2::scale_fill_manual(
        values = c("Check" = "#ff7f0e", "New" = "#1f77b4"),
        name   = "Tipo de tratamiento"
      ) +
      ggplot2::labs(
        x     = "Bloque",
        y     = "Posición dentro del bloque",
        title = "Mapa del diseño DBA: checks vs nuevos por bloque"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  # -------------------------------------------------------------------
  # 5) Descarga del fieldbook
  # -------------------------------------------------------------------
  output$ui_dl_dba <- renderUI({
    shiny::req(dba_design())
    downloadButton(
      ns("dl_dba_csv"),
      "Descargar fieldbook (CSV)",
      class = "btn btn-secondary w-100"
    )
  })
  
  output$dl_dba_csv <- downloadHandler(
    filename = function() {
      paste0("dba_fieldbook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      des <- dba_design(); shiny::req(des)
      utils::write.csv(des$book, file, row.names = FALSE)
    }
  )
}

# Pestaña 4: Simulación & análisis
pestanna4_session8_v3_server <- function(input, output, session,
                                         dba_design,
                                         sim_dba_data,
                                         has_pkg
                                        ) {
  
  # ------------------------------------------------------------
  # 4.1 Visualización del ensayo simulado
  # ------------------------------------------------------------
  output$plot_sim_layout <- renderPlot({
    df <- sim_dba_data()
    shiny::req(df)
    
    # Detectar checks vs nuevos en base al número de repeticiones por tratamiento:
    # - En un DBA típico: los checks se repiten varias veces.
    # - Los nuevos (entries) aparecen solo una vez.
    tbl_trt   <- table(df$trt)
    checks_id <- names(tbl_trt)[tbl_trt > 1]
    
    df$Type <- ifelse(df$trt %in% checks_id, "Check", "New")
    
    # Posición dentro de cada bloque (solo para ordenar en el eje Y)
    df <- df |>
      dplyr::group_by(block) |>
      dplyr::arrange(trt, .by_group = TRUE) |>
      dplyr::mutate(pos = dplyr::row_number()) |>
      dplyr::ungroup()
    
    ggplot2::ggplot(
      df,
      ggplot2::aes(x = factor(.data[["block"]]),
                   y = .data[["pos"]],
                   fill = .data[["Y"]],
                   label = .data[["trt"]])
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(size = 2.5) +
      ggplot2::scale_y_reverse() +
      ggplot2::scale_fill_viridis_c(name = "Y (respuesta)") +
      ggplot2::facet_wrap(~ Type, nrow = 1) +
      ggplot2::labs(
        x = "Bloque",
        y = "Posición en el bloque",
        title = "Ensayo DBA simulado: distribución de Y por bloque y tipo de tratamiento"
      ) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        legend.position = "right",
        strip.text = ggplot2::element_text(face = "bold")
      )
  })
  
  # ------------------------------------------------------------
  # 4.2 Análisis clásico (augmentedRCBD)
  # ------------------------------------------------------------
  output$out_aug_classic <- renderPrint({
    df <- sim_dba_data()
    shiny::req(df)
    
    if (has_pkg("augmentedRCBD")) {
      # Detectar check vs nuevos por nº de repeticiones
      tbl <- table(df$trt)
      checks <- names(tbl)[tbl > 1]
      cat("Checks detectados (aparecen en más de un bloque):\n  ",
          paste(checks, collapse = ", "), "\n\n")
      
      cat("Análisis clásico tipo Federer con ",
          "augmentedRCBD::augmentedRCBD(...)\n\n")
      
      res <- augmentedRCBD::augmentedRCBD(
        block = df$block,
        treatment = df$trt,
        y = df$Y,
        method.comp = "lsd",
        alpha = 0.05,
        group = FALSE,
        console = TRUE
      )
      
      cat("\n--- Tabla ANOVA (augmentedRCBD) ---\n")
      print(res$ANOVA)
      
      cat("\n--- Resumen de medias ajustadas (tratamientos) ---\n")
      print(head(res$treatmentMeans))
      
    } else {
      cat("Paquete 'augmentedRCBD' no instalado.\n",
          "Se muestra un ANOVA simple usando lm(Y ~ block + trt) ",
          "solo con fines ilustrativos (no es el análisis ideal para DBA).\n\n")
      
      m <- stats::lm(Y ~ factor(block) + factor(trt), data = df)
      print(anova(m))
    }
  })
  
  # ------------------------------------------------------------
  # 4.3 Análisis mixto (LMM) & BLUPs
  # ------------------------------------------------------------
  output$out_aug_lmm <- renderPrint({
    df <- sim_dba_data()
    shiny::req(df)
    
    # Modelo mixto: bloques y genotipos como aleatorios
    # Y ~ 1 + (1|block) + (1|trt)
    # En mejoramiento temprano, este enfoque permite obtener BLUPs
    # más robustos para selección.
    library(lmerTest)
    
    m <- tryCatch({
       lmerTest::lmer(Y ~ 1 + (1 | block) + (1 | trt), data = df)
    }, error = function(e) {
       cat("Error al ajustar el modelo mixto: ", e$message, "\n")
       cat("Posible causa: diseño mal conectado o falta de grados de libertad.\n")
       NULL
    })
    
    if (is.null(m)) return()
    
    cat("Modelo mixto ajustado:\n")
    cat("  Fórmula: Y ~ 1 + (1 | block) + (1 | trt)\n\n")
    
    print(summary(m))
    
    cat("\n--- Componentes de varianza (VarCorr) ---\n")
    vc <- lme4::VarCorr(m)
    print(vc)
    
    vc_df <- as.data.frame(vc)
    cat("\nResumen numérico de σ² por componente:\n")
    print(vc_df[, c("grp", "vcov")])
    
    # Guardar el modelo en la sesión para reutilizar en el gráfico de BLUPs
    session$userData$dba_lmm <- m
  })
  
  # Gráfico de BLUPs (Top 20 genotipos)
  output$plot_blups <- renderPlot({
    m <- session$userData$dba_lmm
    if (is.null(m)) {
      # Si falló el ajuste, mostramos un plot vacío con mensaje
      plot(1, type="n", axes=FALSE, xlab="", ylab="")
      text(1, 1, "No se pudo ajustar el modelo LMM.", cex=1.5)
      return()
    }
    
    ran <- lme4::ranef(m)$trt
    ran$Trt <- rownames(ran)
    names(ran)[1] <- "BLUP"
    
    # Ordenar por BLUP (de mayor a menor) y tomar los 20 mejores
    top <- ran |>
      dplyr::arrange(dplyr::desc(BLUP)) |>
      dplyr::slice(1:20)
    
    ggplot2::ggplot(
      top,
      ggplot2::aes(x = stats::reorder(.data[["Trt"]], .data[["BLUP"]]), y = .data[["BLUP"]])
    ) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Genotipo",
        y = "BLUP (efecto aleatorio estimado)",
        title = "Top 20 genotipos según BLUP (modelo mixto DBA)"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
}

# Pestaña 5: Protocolo para datos reales (DBA)
pestanna5_session8_v3_server <- function(input, output, session) {
  # Pestaña solo informativa: no se requiere lógica de servidor.
}

# Pestaña 6: Ejercicios prácticos & tareas
pestanna6_session8_v3_server <- function(input, output, session, sim_dba_data = NULL) {
  
  # Descarga de un dataset simulado de ejemplo
  output$dl_dba_example <- downloadHandler(
    filename = function() {
      paste0("DBA_simulado_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Si la app principal pasó sim_dba_data (reactive), lo usamos.
      if (!is.null(sim_dba_data)) {
        df <- sim_dba_data()
        # Por seguridad, requerimos que tenga al menos columnas básicas
        req_cols <- c("block", "trt", "Y")
        if (!all(req_cols %in% names(df))) {
          warning("El objeto sim_dba_data() no tiene las columnas esperadas; generando dataset simple de respaldo.")
          df <- .generar_dba_respaldo()
        }
      } else {
        # Si no hay sim_dba_data, generamos un dataset sencillo de respaldo
        df <- .generar_dba_respaldo()
      }
      
      utils::write.csv(df, file, row.names = FALSE)
    }
  )
}

# Pestaña 7: Referencias (DBA)
pestanna7_session8_v3_server <- function(input, output, session) {
  # Pestaña puramente estática: no se requiere lógica de servidor.
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session8_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  has_pkg <- function(p) requireNamespace(p, quietly=TRUE)

  # Reactives
  dba_design <- reactiveVal(NULL)

  sim_dba_data <- eventReactive(input$btn_sim_dba, {
    des <- dba_design()
    if (is.null(des)) {
      checks <- c("C1", "C2")
      new_trts <- paste0("N", 1:20)
      des <- agricolae::design.dau(checks, new_trts, r=4, serie=2, seed=123)
    }
    fb <- des$book

    mu <- input$sim_mu
    s_g <- input$sim_sigma_g
    s_b <- input$sim_sigma_b
    s_e <- input$sim_sigma_e

    trts <- unique(fb$trt)
    eff_t <- rnorm(length(trts), 0, s_g); names(eff_t) <- trts

    blks <- unique(fb$block)
    eff_b <- rnorm(length(blks), 0, s_b); names(eff_b) <- blks

    fb$Y <- NA
    for(i in 1:nrow(fb)) {
      t_val <- as.character(fb$trt[i])
      b_val <- as.character(fb$block[i])
      fb$Y[i] <- mu + eff_t[t_val] + eff_b[b_val] + rnorm(1, 0, s_e)
    }
    fb
  })

  .generar_dba_respaldo <- function() {
    # Pequeño DBA ficticio para que el botón siempre entregue algo útil
    set.seed(123)
    bloques  <- paste0("B", 1:4)
    checks   <- c("C1", "C2")
    nuevos   <- paste0("N", 1:20)
    
    # Construimos un diseño muy sencillo: todos los checks en todos los bloques
    fb_checks <- expand.grid(
      block = bloques,
      trt   = checks,
      stringsAsFactors = FALSE
    )
    # 5 nuevos por bloque, asignados al azar
    fb_news <- do.call(
      rbind,
      lapply(bloques, function(b) {
        data.frame(
          block = b,
          trt   = sample(nuevos, size = 5),
          stringsAsFactors = FALSE
        )
      })
    )
    fb <- rbind(fb_checks, fb_news)
    fb$block <- factor(fb$block)
    fb$trt   <- factor(fb$trt)
    
    # Simulación simple de respuesta
    mu   <- 100
    s_g  <- 10
    s_b  <- 15
    s_e  <- 5
    
    eff_t <- stats::rnorm(length(levels(fb$trt)), 0, s_g)
    names(eff_t) <- levels(fb$trt)
    
    eff_b <- stats::rnorm(length(levels(fb$block)), 0, s_b)
    names(eff_b) <- levels(fb$block)
    
    fb$Y <- mu + eff_t[fb$trt] + eff_b[fb$block] + stats::rnorm(nrow(fb), 0, s_e)
    
    fb
  }

  # Call tab servers
  pestanna1_session8_v3_server(input, output, session)
  pestanna2_session8_v3_server(input, output, session)
  pestanna3_session8_v3_server(input, output, session, dba_design, has_pkg)
  pestanna4_session8_v3_server(input, output, session, dba_design, sim_dba_data, has_pkg)
  pestanna5_session8_v3_server(input, output, session)
  pestanna6_session8_v3_server(input, output, session)
  pestanna7_session8_v3_server(input, output, session)
}
