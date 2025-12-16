# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session7.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto
pestanna1_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Plan & contexto",
    
    # Título general
    h4(class = "section-header",
       "¿Dónde encajan los diseños Fila–Columna en la historia del curso?"),
    
    # Bloque contextual: de RCBD a Row-Column
    div(
      class = "alert alert-info",
      p(
        strong("Idea general: "), 
        "en esta sesión pasamos de ",
        em("bloquear en una sola dirección"),
        " (RCBD) o usar estructuras jerárquicas (split-plot, strip-plot), ",
        "a ", strong("bloquear en dos direcciones espaciales simultáneas"),
        " (Fila y Columna) para mejorar la precisión en ensayos grandes de campo."
      )
    ),
    
    # Mapa de la Parte III
    h5("Mapa de las sesiones de la Parte III (Avanzada)"),
    tags$ul(
      tags$li(
        strong("Sesión 4 – RCBD + LMM: "),
        "un solo factor de bloqueo (Bloque) → un único estrato de error residual."
      ),
      tags$li(
        strong("Sesión 5 – Split-plot: "),
        "dos unidades experimentales (parcela principal y subparcela) → ",
        "dos estratos de error (Error A y Error B)."
      ),
      tags$li(
        strong("Sesión 6 – Strip-plot: "),
        "franjas cruzadas de dos factores (A y B) dentro de bloque → ",
        "tres estratos de error: Error(a), Error(b) y Error(c)."
      ),
      tags$li(
        strong("Sesión 7 – Fila–Columna: "),
        "conjunto de tratamientos dispuesto en una grilla (Filas × Columnas), ",
        "con bloqueo en ambas direcciones → modelo mixto con efectos aleatorios de ",
        code("Row"), " y ", code("Col"), "."
      )
    ),
    
    hr(),
    
    # Resultados de aprendizaje de la sesión
    h5("Resultados de aprendizaje de la sesión"),
    tags$ul(
      tags$li(
        "Describir de forma clara un layout Fila–Columna: tratamientos en una grilla, ",
        "indexados por ", code("Row"), " y ", code("Col"), 
        " y, opcionalmente, por una repetición ", code("Rep"), "."
      ),
      tags$li(
        "Reconocer cuándo un diseño Fila–Columna puede mejorar a un RCBD: ",
        "presencia de gradientes fuertes en dos direcciones del campo ",
        "(por ejemplo, norte–sur y este–oeste)."
      ),
      tags$li(
        "Identificar la forma básica del modelo mixto asociado: ",
        code("y ~ Trat + (1|Row) + (1|Col)"),
        " (con o sin ", code("Rep"), "), e interpretar Fila y Columna como ",
        em("bloqueos espaciales aleatorios"), "."
      )
    ),
    
    hr(),
    
    # Dos columnas: ejemplo agronómico + mensaje clave
    fluidRow(
      column(
        width = 6,
        h5("Ejemplo agronómico ancla"),
        p(
          "Imagina un ensayo de ", strong("20–100 genotipos de arándano"), 
          " en una parcela grande. Por la fisiografía del terreno:"
        ),
        tags$ul(
          tags$li(
            "Existe un gradiente de ", strong("fertilidad"), 
            " que aumenta de sur a norte."
          ),
          tags$li(
            "Existe un gradiente de ", strong("humedad"),
            " que aumenta de oeste a este."
          ),
          tags$li(
            "Cada genotipo ocupa una celda de la grilla definida por ",
            code("Fila"), " × ", code("Columna"), "."
          )
        ),
        p(
          "Un RCBD tradicional solo bloquearía en una dirección (por ejemplo, sur–norte). ",
          "El diseño Fila–Columna permite ",
          strong("bloquear simultáneamente en ambas direcciones"),
          " y así reducir el ruido espacial en la estimación de las diferencias entre genotipos."
        )
      ),
      column(
        width = 6,
        h5("Mensaje que queremos que te lleves"),
        div(
          class = "note-cloud",
          p(
            strong("Resumen: "),
            "usa un diseño Fila–Columna cuando el experimento es grande y el campo presenta ",
            em("gradientes bidireccionales"),
            " que no se pueden capturar bien con un único bloque. ",
            "El análisis natural es un ",
            strong("modelo mixto"),
            " con filas y columnas como efectos aleatorios."
          )
        ),
        p(
          class = "text-muted small",
          "En las siguientes pestañas veremos cómo generar estos diseños (",
          code("FielDHub::row_column"), "), cómo simular gradientes espaciales ",
          "y cómo comparar RCBD vs Fila–Columna vía modelos lineales mixtos (LMM)."
        )
      )
    )
  )
}

# Pestaña 2: Conceptos clave: doble bloqueo y modelo LMM
pestanna2_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Conceptos clave",
    
    h4(class = "section-header", "Conceptos clave: diseño Fila–Columna"),
    
    # Bloque definitorio
    div(
      class = "alert alert-info",
      p(
        strong("¿Qué es un diseño Fila–Columna? "),
        "Es un diseño de campo donde los tratamientos se disponen en una ",
        strong("grilla de filas y columnas"),
        " y se incorpora simultáneamente el bloqueo en ambas direcciones para ",
        "controlar gradientes espaciales complejos que no se capturan con un solo bloque."
      )
    ),
    
    # Croquis mental
    h5("Croquis mental: Filas × Columnas"),
    tags$p(
      "Imagina una parcela de ensayo dividida en una grilla de ",
      code("R filas") , " por ", code("C columnas"), ".",
      "Cada celda de esa grilla contiene un tratamiento (por ejemplo, un genotipo). ",
      "La razón de tener filas y columnas es capturar variación espacial en dos direcciones:",
      tags$ul(
        tags$li(
          strong("Filas: "),
          "pueden representar gradientes de la parcela en dirección norte–sur "
          , "(por ejemplo, pendiente, humedad)."
        ),
        tags$li(
          strong("Columnas: "),
          "pueden representar gradientes en dirección este–oeste "
          , "(por ejemplo, fertilidad, textura del suelo)."
        )
      )
    ),
    
    tags$hr(),
    
    # Comparación con Cuadrado Latino
    h5("Cuadrado Latino vs Row‑Column general"),
    tags$ul(
      tags$li(
        strong("Cuadrado Latino: "),
        "estructura clásica donde ",
        code("# tratamientos = # filas = # columnas"),
        ", lo que impone una restricción fuerte (específico para ciertos tamaños)."
      ),
      tags$li(
        strong("Row‑Column (General): "),
        "estructura más flexible que permite que el número de tratamientos ",
        "sea distinto al número de filas y/o columnas, y soporta diseños resolubles ",
        "o incompletos adecuados a las dimensiones del campo real."
      )
    ),
    tags$p(
      "En la práctica de mejoramiento y de ensayos agronómicos grandes, ",
      "es común usar diseños Row‑Column generalizados (alfa‑latinizados, resolubles, etc.) ",
      "que mantienen bloqueo bidimensional sin requerir igualdad de dimensiones."
    ),
    
    tags$hr(),
    
    # Modelo LMM típico
    h5("Modelo mixto estándar para Row‑Column"),
    withMathJax(
      helpText(
        "Una formulación habitual de un modelo mixto para un diseño Fila–Columna es:",
        "$$Y_{ijk} = \\mu + \\tau_k + \\text{Fila}_i + \\text{Columna}_j + \\varepsilon_{ijk}$$"
      )
    ),
    tags$ul(
      tags$li(
        HTML("<b>\\(\\mu\\)</b>: media general del experimento.")
      ),
      tags$li(
        HTML("<b>\\(\\tau_k\\)</b>: efecto del tratamiento <code>k</code> (fijo o aleatorio)."),
        "En ensayos de mejores genotipos suele tratarse como efecto fijo para comparar niveles."
      ),
      tags$li(
        HTML("<b>\\(\\text{Fila}_i\\)</b>: efecto de la fila <code>i</code> (bloque espacial en una dirección)."),
        "Se modela típicamente como efecto aleatorio para estimar varianza de gradiente."
      ),
      tags$li(
        HTML("<b>\\(\\text{Columna}_j\\)</b>: efecto de la columna <code>j</code> (bloque espacial en la otra dirección)."),
        "También se trata como aleatorio por la misma razón."
      ),
      tags$li(
        HTML("<b>\\(\\varepsilon_{ijk}\\)</b>: error residual."),
        "Representa variación no explicada dentro de cada combinación fila–columna–tratamiento."
      )
    ),
    tags$p(
      "Una implementación concreta en R con el paquete ", code("lme4"), " sería:",
      pre(class = "r-code",
          "lmer(Y ~ Trat + (1 | Row) + (1 | Col), data = datos)")
    ),
    tags$p(
      "Si también existe un factor de repetición (por ejemplo, bloques adicionales), ",
      "la fórmula puede extenderse a:",
      pre(class = "r-code",
          "lmer(Y ~ Trat + (1 | Rep) + (1 | Rep:Row) + (1 | Rep:Col), data = datos)")
    ),
    
    tags$hr(),
    
    # Explicación de aleatorización y REML
    h5("Interpretación del bloqueo aleatorio"),
    tags$p(
      "Al tratar Filas y Columnas como efectos aleatorios, lo que realmente estás ",
      "estimando son componentes de varianza asociados a gradientes espaciales en cada ",
      "dirección. Esto mejora la precisión en la estimación de los efectos de los tratamientos ",
      "de manera análoga a cómo lo hace un bloque en un RCBD, pero ahora en dos direcciones."
    ),
    tags$p(
      "Este enfoque de estructuras de bloque aleatorias es estándar en la literatura de ",
      "modelos mixtos y diseños experimentales complejos (p.ej., Stroup, John & Williams, Piepho)."
    ),
    
    tags$hr(),
    
    # Mensaje comparativo con RCBD
    h5("Comparación con RCBD (bloque simple)"),
    tags$p(
      "En un RCBD tradicional solo hay un factor de bloqueo (Bloque) que captura variación ",
      "a lo largo de una sola dimensión del campo. Si existe heterogeneidad bidireccional (p.ej., ",
      "pendiente y fertilidad cruzadas), un diseño Fila–Columna con efectos aleatorios de ",
      "fila y columna suele entregar estimaciones más precisas y menores errores estándar."
    ),
    
    tags$hr(),
    
    # Cierre de la pestaña
    tags$p(
      class = "text-muted small",
      "En la siguiente pestaña veremos cómo generar concretamente un diseño Row–Column ",
      "en R y cómo visualizar el fieldbook resultante."
    )
  )
}

# Pestaña 3: Diseño & fieldbook Row-Column
pestanna3_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) Diseño & fieldbook",
    
    h4(class = "section-header", "Del esquema teórico al libro de campo (Row-Column)"),
    p(
      "En esta pestaña conectamos la idea de doble bloqueo (filas y columnas) con un ",
      strong("fieldbook concreto"),
      " tal como se usaría en un ensayo de mejoramiento."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h5("Parámetros del diseño fila-columna"),
        numericInput(
          ns("n_trts"),
          "Número de tratamientos (genotipos):",
          value = 20, min = 4, step = 1
        ),
        helpText(
          "Se buscará ubicar estos tratamientos en una grilla de ",
          em("Filas × Columnas"),
          ". Si hay más celdas que tratamientos, se rellenará con 'Blank'."
        ),
        numericInput(
          ns("n_rows"),
          "Número de Filas (Row):",
          value = 4, min = 2, step = 1
        ),
        numericInput(
          ns("n_cols"),
          "Número de Columnas (Col):",
          value = 5, min = 2, step = 1
        ),
        numericInput(
          ns("n_reps"),
          "Número de repeticiones (copias del diseño):",
          value = 1, min = 1, step = 1
        ),
        helpText(
          "Cada repetición puede pensarse como un 'mini-campo' con la misma estructura ",
          "Fila × Columna."
        ),
        hr(),
        actionButton(
          ns("btn_gen_rc"),
          "Generar diseño fila-columna",
          class = "btn btn-primary w-100"
        ),
        hr(),
        uiOutput(ns("ui_dl_rc"))
      ),
      
      mainPanel(
        width = 8,
        
        h5("Resumen del diseño generado"),
        uiOutput(ns("rc_resumen_diseno")),
        tags$div(
          class = "text-muted small",
          "Interpretación: filas y columnas actúan como ",
          strong("dos factores de bloqueo"),
          " que controlan gradientes espaciales (ej. pendiente y fertilidad)."
        ),
        hr(),
        
        h5("Mapa del diseño (por repetición)"),
        plotOutput(ns("plot_rc_map"), height = "420px"),
        tags$div(
          class = "text-muted small",
          "Eje X = columna (posible gradiente Este–Oeste); ",
          "Eje Y = fila (posible gradiente Norte–Sur). Cada faceta corresponde a una repetición."
        ),
        hr(),
        
        h5("Fieldbook (primeras filas)"),
        DT::dataTableOutput(ns("tbl_rc_book")),
        tags$div(
          class = "text-muted small",
          "En un análisis con modelos mixtos, típicamente usarás columnas del tipo ",
          code("Row"), ", ", code("Col"), ", ", code("Rep"),
          " y ", code("Tratamiento"),
          " junto con la variable respuesta ", code("Y"),
          " que se medirá en campo."
        )
      )
    )
  )
}

# Pestaña 4: Simulación & análisis (RCBD vs Row-Column)
pestanna4_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) Simulación & análisis",
    
    h4(class = "section-header",
       "RCBD vs Row-Column (LMM) con gradientes en fila y columna"),
    
    p(
      "En esta pestaña simulamos datos con gradientes espaciales en fila y columna ",
      "para comparar un modelo 'ingenuo' (RCBD/CRD) con un modelo mixto Row-Column ",
      "que incluye filas y columnas como efectos aleatorios."
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Parámetros de simulación (campo hipotético)"),
        helpText(
          "Interpretación recomendada:\n",
          "- ", strong("σ Genotipo"), ": variación verdadera entre tratamientos.\n",
          "- ", strong("σ Fila / σ Columna"), ": gradientes espaciales (pendiente, fertilidad).\n",
          "- ", strong("σ Residual"), ": ruido dentro de parcela."
        ),
        numericInput(ns("sim_mu"), "Media general (μ):", value = 100),
        numericInput(ns("sim_sigma_g"), "SD Genotipo (señal entre tratamientos):", value = 5),
        numericInput(ns("sim_sigma_row"), "SD Fila (gradiente norte–sur):", value = 8),
        numericInput(ns("sim_sigma_col"), "SD Columna (gradiente este–oeste):", value = 8),
        numericInput(ns("sim_sigma_e"), "SD Residual (ruido dentro de parcela):", value = 4),
        tags$hr(),
        helpText(
          "La simulación usa el diseño fila–columna generado en la pestaña 3 (si existe). ",
          "Si no hay diseño, se genera una grilla simple por defecto."
        ),
        actionButton(
          ns("btn_sim_run"),
          "Simular y analizar",
          class = "btn btn-success w-100"
        )
      ),
      
      mainPanel(
        width = 8,
        bslib::navset_card_pill(
          
          # 4.1 Visualización espacial
          bslib::nav_panel(
            title = "4.1 Visualización espacial",
            h5("Mapa de la respuesta simulada (Y) en la grilla Fila × Columna"),
            plotOutput(ns("plot_sim_spatial"), height = "360px"),
            p(class = "text-muted small",
              "La escala de color refleja la respuesta observada. Si los gradientes de fila/columna ",
              "son grandes (σ Fila, σ Columna altos), deberías ver bandas o patrones sistemáticos.")
          ),
          
          # 4.2 Comparación de modelos
          bslib::nav_panel(
            title = "4.2 Comparación de modelos",
            
            h5("Modelo 1: RCBD / CRD (ignora filas y columnas)"),
            p(
              "Si solo hay una repetición (REP = 1) se ajusta un ",
              code("CRD:  Y ~ Tratamiento"),
              ".\n",
              "Si hay varias repeticiones, se ajusta un modelo tipo ",
              code("RCBD: Y ~ Tratamiento + (1|REP)"),
              " donde REP es el bloque clásico."
            ),
            verbatimTextOutput(ns("out_rcbd")),
            tags$hr(),
            
            h5("Modelo 2: Row-Column LMM (Filas y Columnas como aleatorios)"),
            p(
              "Modelo mixto recomendado para diseños fila–columna:\n",
              code("Y ~ Tratamiento + (1|REP) + (1|REP:Row) + (1|REP:Col)"),
              " (si existe REP) o\n",
              code("Y ~ Tratamiento + (1|Row) + (1|Col)"),
              " si no hay repeticiones.\n\n",
              "Este modelo captura explícitamente los gradientes de fila y columna ",
              "como componentes de varianza adicionales."
            ),
            verbatimTextOutput(ns("out_rclmm")),
            div(
              class = "alert alert-warning",
              strong("Lectura sugerida: "),
              "compara los valores de AIC, los F para tratamientos y el tamaño del ",
              "término residual entre ambos modelos. En general, si hay gradiente fuerte, ",
              "el modelo Row-Column reduce la varianza residual y mejora la precisión ",
              "de los efectos de tratamientos."
            )
          ),
          
          # 4.3 VarCorr & heredabilidad
          bslib::nav_panel(
            title = "4.3 VarCorr & heredabilidad",
            h5("Componentes de varianza (modelo Row-Column)"),
            verbatimTextOutput(ns("out_vc")),
            p(
              "En esta salida se observan las varianzas estimadas para:\n",
              "- Fila (y REP:Fila si aplica)\n",
              "- Columna (y REP:Col si aplica)\n",
              "- Residual\n\n",
              "Si las varianzas de fila y columna son apreciables frente al residual, ",
              "el diseño fila–columna está capturando con éxito la heterogeneidad espacial."
            ),
            p(class = "text-muted small",
              "En aplicaciones de mejoramiento genético, a partir de estos componentes ",
              "se suele calcular una heredabilidad en sentido amplio para los efectos ",
              "de tratamiento (genotipos).")
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales (Row-Column)
pestanna5_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo: datos reales",
    
    h4(class = "section-header", "Checklist para analizar un diseño Fila-Columna (Row-Column)"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Objetivo de esta pestaña: "),
        "dar una receta paso a paso para pasar de un ",
        em("Excel real"),
        " con filas/columnas a un ",
        em("modelo mixto (LMM) correctamente especificado"),
        " para un diseño fila-columna."
      ),
      p(
        "La idea es que puedas tomar tus propios datos de campo y replicar el flujo: ",
        code("leer datos → explorar diseño → ajustar LMM → obtener medias ajustadas y reporte agronómico.")
      )
    ),
    
    # --------------------------------------------------------------
    # Paso 0 – Entender el diseño fila-columna
    # --------------------------------------------------------------
    h4("Paso 0 – Entender el diseño fila-columna"),
    p(
      "Antes de ajustar modelos, verifica que tu archivo de datos tiene las columnas mínimas ",
      "y que realmente corresponde a un diseño fila-columna (bloqueo en dos direcciones)."
    ),
    
    tags$ul(
      tags$li(
        strong("Columnas mínimas recomendadas:"),
        tags$ul(
          tags$li(code("Row"), " (fila física en el campo; p.ej. hilera de plantas)."),
          tags$li(code("Col"), " (columna física en el campo; p.ej. posición este-oeste)."),
          tags$li(code("Trt"), " (tratamiento/genotipo/línea)."),
          tags$li(code("Y"), " (respuesta: rendimiento, firmeza, Brix, etc.)."),
          tags$li(code("Rep") , " o ", code("Block"), " (si hay más de una grilla repetida).")
        )
      ),
      tags$li(
        strong("Preguntas de chequeo:"),
        tags$ul(
          tags$li("¿Cada tratamiento aparece una vez por repetición (o según lo definido en el diseño)?"),
          tags$li("¿Las filas y columnas representan bandas físicas reales en el campo (no solo índices arbitrarios)?"),
          tags$li("¿La grilla Row × Col cubre el campo de forma razonablemente regular (sin huecos extremos)?")
        )
      )
    ),
    
    div(
      class = "alert alert-secondary",
      strong("Intuición: "),
      "Row y Col funcionan como dos factores de bloqueo espaciales que capturan gradientes ",
      "en dos direcciones (por ejemplo, pendiente norte-sur y fertilidad este-oeste)."
    ),
    
    # --------------------------------------------------------------
    # Paso 1 – Explorar balance y gradiente
    # --------------------------------------------------------------
    h4("Paso 1 – Explorar balance y gradiente"),
    p(
      "Con los datos ya leídos en R (por ejemplo en un objeto ", code("datos"), "), ",
      "el primer paso es revisar el layout y la presencia de gradientes."
    ),
    
    tags$ul(
      tags$li(
        strong("1.1) Verificar celdas ocupadas (balance):"),
        tags$ul(
          tags$li(
            "Conteos por fila y columna, para detectar huecos o desbalances grandes."
          )
        ),
        tags$pre(
          class = "r-code",
          "dplyr::count(datos, Row, Col)\nxtabs(~ Row + Col, data = datos)"
        )
      ),
      tags$li(
        strong("1.2) Visualizar la respuesta en el plano fila × columna:"),
        p("Esto ayuda a detectar gradientes fuertes (p.ej., rendimientos mayores en un lado del campo)."),
        tags$pre(
          class = "r-code",
          "library(ggplot2)\n\nggplot(datos, aes(x = Col, y = Row, fill = Y)) +\n  geom_tile(color = \"white\") +\n  scale_y_reverse() +\n  scale_fill_viridis_c() +\n  theme_minimal() +\n  labs(title = \"Mapa espacial de Y\",\n       x = \"Columna (este-oeste)\", y = \"Fila (norte-sur)\")"
        )
      ),
      tags$li(
        strong("Interpretación:"),
        tags$ul(
          tags$li("Si Y cambia suavemente a lo largo de filas/columnas, el diseño fila-columna tiene sentido."),
          tags$li("Si no hay gradiente aparente, el beneficio frente a un RCBD simple puede ser menor, pero igual es válido usar Row-Column.")
        )
      )
    ),
    
    # --------------------------------------------------------------
    # Paso 2 – Modelos candidatos
    # --------------------------------------------------------------
    h4("Paso 2 – Modelos candidatos"),
    p(
      "Comparamos un modelo más simple (CRD/RCBD) con un modelo que reconoce la estructura fila-columna. ",
      "Ambos se formulan como modelos lineales (mixtos) en R."
    ),
    
    tags$ul(
      tags$li(
        strong("2.1) Modelo simple (CRD / RCBD):"),
        p(
          "Si no consideras filas ni columnas, el modelo típico sería:"
        ),
        tags$pre(
          class = "r-code",
          "# Conversión a factor recomendada\n\ndatos <- datos |>\n  dplyr::mutate(\n    Row = factor(Row),\n    Col = factor(Col),\n    Trt = factor(Trt),\n    Rep = factor(Rep)\n  )\n\n# Si tienes solo 1 repetición (sin Rep):\nmod_simple <- lm(Y ~ Trt, data = datos)\n\n# Si tienes varias repeticiones (RCBD):\n# library(lmerTest)\nmod_rcbd <- lmerTest::lmer(Y ~ Trt + (1 | Rep), data = datos)"
        ),
        p(
          "Este modelo ignora el patrón espacial dentro de cada repetición (no tiene Row ni Col)."
        )
      ),
      tags$li(
        strong("2.2) Modelo recomendado fila-columna (Row-Column LMM):"),
        p(
          "Aquí tratamos ", code("Row"), " y ", code("Col"), " como ",
          em("efectos aleatorios"),
          " que capturan los gradientes espaciales."
        ),
        tags$pre(
          class = "r-code",
          "# Sin Rep (una sola grilla):\nmod_rc <- lmerTest::lmer(\n  Y ~ Trt + (1 | Row) + (1 | Col),\n  data = datos\n)\n\n# Con Rep (varias grillas):\nmod_rc <- lmerTest::lmer(\n  Y ~ Trt + (1 | Rep) + (1 | Rep:Row) + (1 | Rep:Col),\n  data = datos\n)"
        ),
        p(
          "La estructura aleatoria se lee como: dentro de cada repetición, hay variación adicional ",
          "asociada a filas y columnas, más allá de la variación residual."
        )
      )
    ),
    
    div(
      class = "alert alert-secondary",
      strong("Idea clave: "),
      "Tratar filas y columnas como aleatorias permite ",
      em("limpiar el gradiente de campo"),
      " y obtener estimaciones más precisas de los efectos de tratamiento."
    ),
    
    # --------------------------------------------------------------
    # Paso 3 – Outputs clave del LMM
    # --------------------------------------------------------------
    h4("Paso 3 – Outputs clave del modelo fila-columna"),
    p(
      "Una vez ajustado el modelo mixto ", code("mod_rc"),
      ", nos interesan tres tipos de resultados."
    ),
    
    tags$ul(
      tags$li(
        strong("3.1) Prueba de tratamientos (efectos fijos):"),
        tags$pre(
          class = "r-code",
          "anova(mod_rc, type = 3)   # requiere lmerTest cargado\nsummary(mod_rc)$coefficients"
        ),
        p(
          "Aquí ves si hay diferencias globales entre tratamientos y la magnitud de los efectos estimados."
        )
      ),
      tags$li(
        strong("3.2) Componentes de varianza (VarCorr):"),
        tags$pre(
          class = "r-code",
          "lme4::VarCorr(mod_rc)"
        ),
        p(
          "Si la varianza asociada a ", code("Row"), " y/o ", code("Col"), 
          " es apreciable, significa que el doble bloqueo está capturando heterogeneidad espacial relevante."
        )
      ),
      tags$li(
        strong("3.3) Medias ajustadas y errores estándar (EMMs):"),
        tags$pre(
          class = "r-code",
          "# library(emmeans)\nemm_trt <- emmeans::emmeans(mod_rc, ~ Trt)\nsummary(emm_trt)\n\n# Comparaciones múltiples (Tukey, LSD, etc.)\ncontrast(emm_trt, method = \"pairwise\", adjust = \"tukey\")"
        ),
        p(
          "Estas medias ajustadas son la base para el reporte agronómico: ",
          "incluyen el efecto de tratamientos, habiendo corregido por filas/columnas."
        )
      )
    ),
    
    # --------------------------------------------------------------
    # Paso 4 – Post-hoc & reporte agronómico
    # --------------------------------------------------------------
    h4("Paso 4 – Post-hoc y reporte agronómico"),
    p(
      "El último paso es traducir los resultados estadísticos a un lenguaje agronómico claro y honesto."
    ),
    
    tags$ul(
      tags$li(
        strong("4.1) Post-hoc / Comparaciones de tratamientos:"),
        p(
          "A partir de ", code("emm_trt"), " puedes construir tablas de medias ajustadas con letras o ",
          "intervalos de confianza, dependiendo de la tradición de tu grupo."
        )
      ),
      tags$li(
        strong("4.2) Elementos mínimos del reporte:"),
        tags$ul(
          tags$li(
            strong("Descripción del diseño:"),
            " número de tratamientos, tamaño de la grilla (R×C), número de repeticiones."
          ),
          tags$li(
            strong("Breve discusión espacial:"),
            " menciona si hubo gradiente de campo y si filas/columnas capturaron varianza relevante ",
            "(a partir de ", code("VarCorr(mod_rc)"), ")."
          ),
          tags$li(
            strong("Resultados principales:"),
            " tabla de medias ajustadas por tratamiento (con SE o IC) y resumen de significancia global ",
            "y por pares (si aplica)."
          ),
          tags$li(
            strong("Mensaje agronómico final:"),
            " sintetiza qué tratamientos se destacan y en qué magnitud, ",
            "teniendo en cuenta que el análisis corrigió por la heterogeneidad espacial."
          )
        )
      )
    ),
    
    div(
      class = "alert alert-light",
      h5("Plantilla tipo Quarto (idea general)"),
      p(
        "Puedes empaquetar este flujo en un reporte reproducible. La estructura típica sería:"
      ),
      tags$ol(
        tags$li("Leer el CSV con el fieldbook (Row, Col, Trt, Y, Rep)."),
        tags$li("Explorar el layout y el gradiente (mapa espacial de Y)."),
        tags$li("Ajustar el modelo fila-columna ", code("mod_rc"), " con ", code("lmerTest"), "."),
        tags$li("Extraer VarCorr, ANOVA de tratamientos y EMMs."),
        tags$li("Generar tablas y gráficos listos para informes o tesis.")
      ),
      p(
        "Esta pestaña sirve como guía conceptual; el código se puede copiar y adaptar a cada ensayo."
      )
    )
  )
}

# Pestaña 6: Ejercicios prácticos & tareas
pestanna6_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Ejercicios prácticos & tareas",
    
    h4(class = "section-header",
       "Guía de práctica: del diseño Row-Column al reporte agronómico"),
    
    div(
      class = "alert alert-info",
      p(
        strong("Objetivo de esta pestaña: "),
        "proponer una secuencia de ejercicios que conecte las pestañas ",
        strong("3) Diseño & fieldbook"), ", ",
        strong("4) Simulación & análisis"), " y ",
        strong("5) Protocolo para datos reales"),
        " para que puedas practicar con tus propios datos o con un dataset simulado."
      )
    ),
    
    h5("Ruta sugerida de trabajo"),
    tags$ol(
      tags$li(
        strong("Ejercicio 1 – Generar un diseño fila-columna 4×6 (24 tratamientos)"),
        tags$ul(
          tags$li("Ir a la pestaña ", strong("3) Diseño & fieldbook Row-Column"), "."),
          tags$li("Configurar: 24 tratamientos, 4 filas, 6 columnas, 1 repetición."),
          tags$li("Visualizar el mapa del diseño y verificar que cada tratamiento aparece exactamente una vez.")
        )
      ),
      tags$li(
        strong("Ejercicio 2 – Simular gradientes espaciales y comparar modelos"),
        tags$ul(
          tags$li("En la pestaña ", strong("4) Simulación & análisis"), " elegir parámetros, por ejemplo:"),
          tags$li(HTML("Media general &mu; ≈ 100; SD Genotipo (señal) ≈ 5.")),
          tags$li(HTML("Gradiente fuerte de columna: SD Columna ≈ 15; gradiente de fila más débil: SD Fila ≈ 3.")),
          tags$li("Simular y observar el mapa espacial: ¿se ve el gradiente vertical/horizontal?"),
          tags$li("Comparar el modelo RCBD simplificado vs. el modelo Row-Column (LMM) y comentar:"),
          tags$li("¿Cambian los p-valores de tratamientos?"),
          tags$li("¿Disminuye la varianza residual o el AIC en el modelo Row-Column?")
        )
      ),
      tags$li(
        strong("Ejercicio 3 – Aplicar el protocolo a un Excel real (o simulado)"),
        tags$ul(
          tags$li("Ir a la pestaña ", strong("5) Protocolo para datos reales"), " y seguir los pasos:"),
          tags$li(strong("Paso 0:"), " identificar columnas mínimas: Row, Col, Trt, Y, Rep (si existe)."),
          tags$li(strong("Paso 1:"), " explorar balance y posibles gradientes espaciales."),
          tags$li(strong("Paso 2:"), " ajustar modelos candidato: RCBD simple vs. Row-Column con filas/columnas aleatorias."),
          tags$li(strong("Paso 3:"), " revisar efectos fijos (Trt) y componentes de varianza (VarCorr).")
        )
      ),
      tags$li(
        strong("Ejercicio 4 – Post-hoc y reporte agronómico"),
        tags$ul(
          tags$li("Calcular medias ajustadas de tratamientos (por ejemplo con ", code("emmeans"), ")."),
          tags$li("Extraer errores estándar o intervalos de confianza de esas medias."),
          tags$li("Redactar un breve reporte (3–5 líneas) que responda:"),
          tags$li("¿Qué tratamientos destacan en rendimiento / variable de interés?"),
          tags$li("¿Las filas y columnas captan una fracción relevante de la variación (var(Row), var(Col))?"),
          tags$li("¿Por qué el diseño fila-columna fue apropiado para este ensayo?")
        )
      ),
      tags$li(
        strong("Ejercicio 5 – Extensión opcional: múltiples repeticiones"),
        tags$ul(
          tags$li("Volver a la pestaña ", strong("3) Diseño & fieldbook"), " y generar un diseño con varias repeticiones (Rep > 1)."),
          tags$li("Simular datos en la pestaña ", strong("4) Simulación & análisis"), " con ese diseño."),
          tags$li("Ajustar el modelo:"),
          tags$li(code("Y ~ Trt + (1|REP) + (1|REP:Row) + (1|REP:Col)")),
          tags$li("Comparar VarCorr y la precisión de tratamientos frente al caso de una sola repetición.")
        )
      )
    ),
    
    hr(),
    h5("Dataset de ejemplo para practicar fuera de la app"),
    p(
      "Si todavía no tienes tu propio archivo Excel con un diseño fila-columna, ",
      "puedes descargar un dataset simulado. Este dataset corresponde a un diseño 4×6 ",
      "con 24 genotipos (G01–G24), un gradiente fuerte en columnas y uno más suave en filas."
    ),
    tags$ul(
      tags$li("Columnas clave: ", code("Row"), ", ", code("Col"), ", ", code("Trt"), ", ", code("Rep"), ", ", code("Y"), "."),
      tags$li("Puedes usarlo como base para reproducir el protocolo de la pestaña 5 ",
              "en R, en otro software estadístico o en un documento Quarto.")
    ),
    
    # Botón de descarga (se habilita desde el server)
    uiOutput(ns("ui_dl_ejemplo_rc")),
    
    br(), br(),
    p(class = "text-muted small",
      "Sugerencia: si estás trabajando en tu tesis o proyecto de investigación, ",
      "adapta estos ejercicios a tu propio diseño de campo (dimensiones, número de tratamientos, ",
      "estructura de repeticiones) y documenta explícitamente por qué un diseño fila-columna ",
      "es adecuado para controlar la heterogeneidad espacial.")
  )
}

# Pestaña extra: Esquemas visuales y galería conceptual
pestanna_extra_session7_v3UI <- function(ns) {

  # Base común por proyecto (tu estándar)
  base_path <- "images/sesiones/Diseños_estadisticos_V3/"
  # Carpeta específica de la sesión
  img_path  <- paste0(base_path, "session7/")

  bslib::nav_panel(
    title = "Extra: Esquemas Visuales",
    icon  = icon("layer-group"),

    tags$div(
      class = "container-fluid py-3",

      tags$h4(class = "text-primary mb-3",
              "Galería Conceptual: Diseño Fila–Columna (Row-Column)"),

      tags$p(
        class = "lead",
        "Cuando el campo tiene variación en dos direcciones (por ejemplo, pendiente y fertilidad), ",
        "un bloqueo simple (RCBD) controla solo una parte del ruido. El diseño fila–columna introduce ",
        "bloqueo bidireccional y se analiza naturalmente con modelos mixtos (LMM)."
      ),

      tags$hr(),

      bslib::navset_card_underline(

        # --- Sub-pestaña A: Comparativa de gradientes ---
        bslib::nav_panel(
          title = "A. Comparativa Espacial",

          tags$div(
            class = "row align-items-center",

            tags$div(
              class = "col-md-8",
              tags$img(
                src   = paste0(img_path, "spatial_gradient_comparison.png"),
                class = "img-fluid shadow-sm border rounded",
                alt   = "Comparación de RCBD versus Row-Column bajo un gradiente espacial diagonal",
                style = "width: 100%; object-fit: contain;"
              ),
              tags$div(
                class = "mt-2 text-muted small",
                "Archivo: spatial_gradient_comparison.png"
              )
            ),

            tags$div(
              class = "col-md-4",
              tags$div(
                class = "alert alert-info mt-3 mt-md-0",
                tags$h5("¿Qué idea está transmitiendo?"),
                tags$p(
                  "En el panel RCBD, cada bloque es largo en una sola dirección: dentro del mismo bloque ",
                  "todavía puede existir un cambio fuerte del gradiente (ruido no controlado)."
                ),
                tags$p(
                  "En Row-Column, la grilla (filas y columnas) captura variación en dos ejes: ",
                  "la comparación visual busca que se “vea” más homogeneidad local dentro de cada celda."
                ),
                tags$ul(
                  tags$li(strong("Lectura rápida:"), " si el gradiente es cruzado/diagonal, piensa en bloqueo doble."),
                  tags$li(strong("Mensaje estadístico:"), " menos varianza residual esperada; más precisión en medias ajustadas.")
                )
              )
            )
          )
        ),

        # --- Sub-pestaña B: Modelo LMM ---
        bslib::nav_panel(
          title = "B. El Modelo LMM",

          tags$div(
            class = "row justify-content-center",

            tags$div(
              class = "col-md-10",
              tags$img(
                src   = paste0(img_path, "lmm_formula_breakdown.png"),
                class = "img-fluid shadow border rounded mx-auto d-block",
                alt   = "Desglose visual de la ecuación del modelo mixto para diseño fila–columna",
                style = "width: 100%;"
              ),

              tags$div(
                class = "mt-3 p-3 bg-light border rounded",
                tags$h6("Lectura guiada de la ecuación"),
                tags$ul(
                  tags$li(strong("Efecto fijo (τk):"), " es el “señal” (tratamientos/genotipos) que quieres comparar."),
                  tags$li(strong("Efectos aleatorios (Ri, Cj):"), " representan variación espacial por fila y columna (bloqueo bidireccional)."),
                  tags$li(strong("Residuo (εijk):"), " queda como ruido “más puro” tras filtrar estructura espacial.")
                ),
                tags$div(
                  class = "mt-2",
                  tags$span(class = "text-muted small",
                            "Tip docente: acompaña esta imagen mostrando VarCorr() y cómo cae σ² residual al incorporar (1|Row) y (1|Col).")
                )
              )
            )
          )
        ),

        # --- Sub-pestaña C: Flujo de trabajo ---
        bslib::nav_panel(
          title = "C. Flujo de Trabajo",

          tags$div(
            class = "row align-items-center",

            tags$div(
              class = "col-md-8",
              tags$img(
                src   = paste0(img_path, "row_col_analysis_pipeline.png"),
                class = "img-fluid shadow-sm border rounded",
                alt   = "Pipeline de análisis en R para diseño fila–columna: de la tabla al reporte",
                style = "width: 100%; object-fit: contain;"
              ),
              tags$div(
                class = "mt-2 text-muted small",
                "Archivo: row_col_analysis_pipeline.png"
              )
            ),

            tags$div(
              class = "col-md-4",
              tags$div(
                class = "card border-secondary mt-3 mt-md-0",
                tags$div(class = "card-header bg-secondary text-white",
                         "Checklist del pipeline"),
                tags$div(
                  class = "card-body",
                  tags$ol(
                    tags$li(strong("Estructura de datos:"), " columnas Row, Col, Trt y respuesta (Yield, etc.)."),
                    tags$li(strong("EDA espacial:"), " heatmap/tiles por Row×Col para detectar gradientes."),
                    tags$li(strong("Ajuste LMM:"), " modelo con fila y columna como efectos aleatorios."),
                    tags$li(strong("Salida agronómica:"), " ANOVA/contrastes + VarCorr + medias ajustadas + ranking.")
                  ),
                  tags$div(
                    class = "mt-2 p-2 bg-light border rounded",
                    tags$div(class = "small text-muted", "Ejemplo de fórmula (referencial):"),
                    tags$code("lmer(Y ~ Trt + (1|Row) + (1|Col), data = df)")
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

# Pestaña 7: Referencias
pestanna7_session7_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    
    h4(class = "section-header", "Referencias para diseños Fila-Columna y modelos mixtos"),
    
    p(
      "Esta sección reúne bibliografía clave organizada por tema: ",
      strong("generación de diseños Row-Column"), ", ",
      strong("análisis con modelos mixtos (LMM)"),
      " y ",
      strong("aplicaciones en mejoramiento genético"),
      ". La idea es que el alumno sepa a qué texto acudir según la fase del flujo de trabajo."
    ),
    
    # ---------------------------------------------------------------
    # 1) Generación de diseños Row-Column
    # ---------------------------------------------------------------
    h5("1) Generación de diseños Row-Column (diseño experimental)"),
    tags$ul(
      tags$li(
        strong("FielDHub – Shiny App para diseño de experimentos en agricultura. "),
        "Paquete en R pensado para generar diseños complejos, incluyendo ",
        em("row-column designs"),
        ", alfa-latice, resolvables, etc. ",
        "Es la base computacional que usamos en la pestaña de ‘Diseño & fieldbook’. ",
        br(),
        "CRAN: ",
        tags$a(
          href   = "https://cran.r-project.org/package=FielDHub",
          target = "_blank",
          "https://cran.r-project.org/package=FielDHub"
        )
      ),
      tags$li(
        strong("John, J. A. & Williams, E. R. (1995). "),
        em("Cyclic and Computer Generated Designs."),
        " Oxford Science Publications. ",
        "Libro clásico sobre construcción de diseños resolubles, cíclicos y estructuras ",
        "de filas/columnas. Útil para entender la lógica matemática detrás de los ",
        "diseños que hoy generamos con software."
      )
    ),
    
    tags$hr(),
    
    # ---------------------------------------------------------------
    # 2) Análisis con modelos mixtos (LMM)
    # ---------------------------------------------------------------
    h5("2) Análisis con modelos mixtos: bloque simple vs. doble bloqueo"),
    tags$ul(
      tags$li(
        strong("Stroup, W. W. (2013). "),
        em("Generalized Linear Mixed Models: Modern Concepts, Methods and Applications."),
        " CRC Press. ",
        "Texto de referencia para interpretar diseños con múltiples estratos de bloqueo ",
        "(RCBD, split-plot, strip-plot, fila-columna, etc.) bajo el marco LMM. ",
        "Los capítulos sobre estructuras de bloque y ‘block structure’ enlazan directamente ",
        "con lo que hacemos en esta Parte III del curso."
      ),
      tags$li(
        "Notas y manuales de modelos mixtos en R (por ejemplo, documentación de ",
        code("lme4"), " y ", code("lmerTest"),
        "). Aunque no son un libro formal, aportan ejemplos prácticos de fórmulas del tipo ",
        code("Y ~ Trt + (1|Row) + (1|Col)"),
        " y extensiones con réplicas.",
        br(),
        span(class = "text-muted small",
             "Recomendación didáctica: contrastar siempre la fórmula RCBD simple ",
             code("Y ~ Trt + (1|Rep)"),
             " con la versión fila-columna ",
             code("Y ~ Trt + (1|Rep) + (1|Rep:Row) + (1|Rep:Col)"),
             " para ver el impacto de los términos de bloqueo adicionales."
        )
      )
    ),
    
    tags$hr(),
    
    # ---------------------------------------------------------------
    # 3) Aplicaciones en mejoramiento genético y row-column
    # ---------------------------------------------------------------
    h5("3) Aplicaciones en mejoramiento genético y diseños fila-columna"),
    tags$ul(
      tags$li(
        strong("Piepho, H. P. et al. (2003). "),
        "Trabajo clásico en ",
        em("Theoretical and Applied Genetics"),
        " donde se comparan resultados de diseños fila-columna resolubles en ensayos de ",
        "mejoramiento. Ilustra cómo el bloqueo bidimensional mejora la precisión de ",
        "las comparaciones entre genotipos frente a un RCBD estándar."
      ),
      tags$li(
        "Artículos de evaluación de ensayos multi-ambientales y multi-locación que utilizan ",
        em("row-column designs"),
        " como base del layout de campo. Suelen combinar: ",
        "diseño fila-columna + LMM (REM/L) + EMMs (medias ajustadas) para genotipos.",
        br(),
        span(
          class = "text-muted small",
          "Mensaje clave: el diseño fila-columna no es solo una curiosidad teórica, ",
          "sino una herramienta estándar en programas de mejoramiento cuando el campo ",
          "presenta gradientes fuertes en dos direcciones."
        )
      )
    ),
    
    tags$hr(),
    
    # ---------------------------------------------------------------
    # 4) ANOVA multi-estrato y expected mean squares
    # ---------------------------------------------------------------
    h5("4) ANOVA multi-estrato y expected mean squares (lecturas de apoyo)"),
    tags$ul(
      tags$li(
        "Apuntes y notas de cursos de diseño experimental (por ejemplo, ",
        em("STAT 502/503"), " u otros cursos universitarios disponibles en línea) ",
        "que discuten explícitamente ANOVA multi-estrato, expected mean squares y ",
        "denominadores F en diseños con múltiples bloques (RCBD, split-plot, strip-plot, ",
        "fila-columna)."
      ),
      tags$li(
        "En el contexto de esta sesión, estos recursos ayudan a conectar la notación de ",
        em("expected mean squares"),
        " con lo que el alumno ve empíricamente en ",
        code("VarCorr()"),
        " y en las fórmulas LMM: cómo cada componente de varianza está ligado a un ",
        "estrato de bloqueo (fila, columna, residual)."
      )
    ),
    
    tags$hr(),
    
    p(
      class = "text-muted small",
      "Sugerencia práctica: si estás trabajando en una tesis o proyecto aplicado, ",
      "elige al menos una referencia de cada bloque (diseño, LMM y aplicaciones) ",
      "y úsala como marco teórico para justificar el uso de diseños fila-columna ",
      "y modelos mixtos en tu análisis."
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session7_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 7: Diseños Fila-Columna (Row-Column Designs)")
    ),
    navset_tab(
      pestanna1_session7_v3UI(ns),
      pestanna2_session7_v3UI(ns),
      pestanna3_session7_v3UI(ns),
      pestanna4_session7_v3UI(ns),
      pestanna5_session7_v3UI(ns),
      pestanna6_session7_v3UI(ns),
      pestanna_extra_session7_v3UI(ns),
      pestanna7_session7_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Plan & contexto
pestanna1_session7_v3_server <- function(input, output, session) {
  # Pestaña puramente explicativa: no requiere lógica de servidor.
}

# Pestaña 2: Conceptos clave
pestanna2_session7_v3_server <- function(input, output, session) {
  # Esta pestaña es puramente explicativa: sin lógica de servidor.
}

# Pestaña 3: Diseño & fieldbook Row-Column
pestanna3_session7_v3_server <- function(input, output, session, rc_design, has_pkg) {
  ns <- session$ns
  
  # -------------------------------------------------------------------
  # 1) Generar diseño con FielDHub (o fallback simple)
  # -------------------------------------------------------------------
  observeEvent(input$btn_gen_rc, {
    
    nt   <- input$n_trts
    nr   <- input$n_rows
    nc   <- input$n_cols
    reps <- input$n_reps
    
    # Validaciones básicas pedagógicas
    # Validaciones básicas pedagógicas
    shiny::validate(
      shiny::need(nt >= 3, "Usa al menos 3 tratamientos para que el ejemplo sea interesante."),
      shiny::need(nr >= 2, "Se requieren al menos 2 filas."),
      shiny::need(nc >= 2, "Se requieren al menos 2 columnas."),
      shiny::need(reps >= 1, "El número de repeticiones debe ser ≥ 1."),
      shiny::need(nt <= nr * nc,
           paste0("N° tratamientos (", nt, ") no puede exceder N° celdas (",
                  nr, " × ", nc, " = ", nr * nc, "). Aumenta filas/columnas o reduce tratamientos.")
      )
    )
    
    # Intentar usar FielDHub si está disponible
    if (!has_pkg("FielDHub")) {
      showNotification(
        "El paquete 'FielDHub' no está instalado. Se usará un generador simple de respaldo.",
        type = "warning", duration = 7
      )
    }
    
    if (has_pkg("FielDHub")) {
      # Intento 1: diseño usando FielDHub
      tryCatch({
        # Nota: se usa el valor por defecto de nrep dentro de FielDHub::row_column().
        # La estructura de filas/columnas sigue siendo válida para fines pedagógicos.
        des <- FielDHub::row_column(
          t          = nt,
          nrows      = nr,
          ncols      = nc,
          plotNumber = 101
        )
        rc_design(des)
        showNotification("Diseño fila-columna generado con FielDHub.", type = "message", duration = 4)
        return()
        
      }, error = function(e) {
        showNotification(
          paste(
            "Hubo un problema al llamar FielDHub::row_column():",
            e$message,
            "Se usará un generador simple de respaldo."
          ),
          type = "warning", duration = 7
        )
      })
    }
    
    # Si no hay FielDHub o falló: generador simple de respaldo
    trts     <- paste0("T", seq_len(nt))
    n_cells  <- nr * nc
    trts_full <- c(trts, rep("Blank", n_cells - nt))
    
    # Diseño base de una repetición
    base_book <- expand.grid(Row = seq_len(nr), Col = seq_len(nc))
    base_book$Trt <- sample(trts_full)
    
    # Replicamos el diseño 'reps' veces
    full_book <- do.call(
      rbind,
      lapply(seq_len(reps), function(rp) {
        b <- base_book
        b$Rep <- rp
        b$Trt <- sample(trts_full)  # nueva aleatorización por repetición
        b
      })
    )
    
    rc_design(list(fieldBook = full_book))
    showNotification("Diseño fila-columna generado con el generador simple.", type = "message", duration = 4)
  })
  
  # -------------------------------------------------------------------
  # 2) Resumen textual del diseño (para interpretación didáctica)
  # -------------------------------------------------------------------
  output$rc_resumen_diseno <- renderUI({
    des <- rc_design(); shiny::req(des)
    
    fb <- des$fieldBook
    # Homogeneizar nombres a mayúsculas para detección
    names(fb) <- toupper(names(fb))
    
    c_row <- grep("ROW", names(fb), value = TRUE)[1]
    c_col <- grep("COL", names(fb), value = TRUE)[1]
    c_trt <- grep("TRT|TREAT|ENTRY", names(fb), value = TRUE)[1]
    c_rep <- grep("REP", names(fb), value = TRUE)
    
    shiny::req(c_row, c_col, c_trt)
    
    n_rows <- length(unique(fb[[c_row]]))
    n_cols <- length(unique(fb[[c_col]]))
    n_trts <- length(unique(fb[[c_trt]]))
    n_cells <- nrow(fb)
    
    if (length(c_rep) == 0) {
      n_reps <- 1
      rep_text <- "No se encontró una columna 'REP'; se asume una sola repetición."
    } else {
      c_rep   <- c_rep[1]
      n_reps  <- length(unique(fb[[c_rep]]))
      rep_text <- paste0("La columna ", code(c_rep), " identifica las repeticiones del diseño.")
    }
    
    n_blanks <- sum(fb[[c_trt]] %in% c("Blank", "BLANK"))
    
    tagList(
      div(
        class = "alert alert-success",
        HTML(paste0(
          "<b>Estructura:</b> ", n_rows, " filas × ", n_cols, " columnas × ",
          n_reps, " repeticiones = ", n_cells, " celdas de parcela.<br>",
          "<b>N° de tratamientos distintos en el fieldbook:</b> ", n_trts, "<br>",
          "<b>Celdas marcadas como 'Blank' (si las hay):</b> ", n_blanks
        ))
      ),
      tags$p(
        class = "text-muted small",
        "En un análisis mixto típico, las filas y columnas se modelan como efectos aleatorios (",
        code("(1|Fila) + (1|Columna)"),
        "), y los tratamientos como efectos fijos de interés científico."
      ),
      tags$p(class = "text-muted small", rep_text)
    )
  })
  
  # -------------------------------------------------------------------
  # 3) Mapa del diseño (por repetición)
  # -------------------------------------------------------------------
  output$plot_rc_map <- renderPlot({
    des <- rc_design(); shiny::req(des)
    fb <- des$fieldBook
    
    names(fb) <- toupper(names(fb))
    c_row <- grep("ROW", names(fb), value = TRUE)[1]
    c_col <- grep("COL", names(fb), value = TRUE)[1]
    c_trt <- grep("TRT|TREAT|ENTRY", names(fb), value = TRUE)[1]
    c_rep <- grep("REP",  names(fb), value = TRUE)
    
    shiny::req(c_row, c_col, c_trt)
    
    # Asegurar numéricos para scale_y_reverse (usando as.factor para evitar NAs si vienen como texto tipo 'Row1')
    fb[[c_row]] <- as.numeric(as.factor(fb[[c_row]]))
    fb[[c_col]] <- as.numeric(as.factor(fb[[c_col]]))
    
    if (length(c_rep) == 0) {
      fb$REP <- factor(1)
    } else {
      fb$REP <- factor(fb[[c_rep[1]]])
    }
    
    ggplot2::ggplot(
      fb,
      ggplot2::aes(x = .data[[c_col]], y = .data[[c_row]], fill = .data[[c_trt]], label = .data[[c_trt]])
    ) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(size = 3) +
      ggplot2::scale_y_reverse() +
      ggplot2::facet_wrap(~ REP) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::guides(fill = "none") +
      ggplot2::labs(
        title = "Mapa del diseño fila-columna por repetición",
        x = "Columna (posible gradiente Este–Oeste)",
        y = "Fila (posible gradiente Norte–Sur)"
      )
  })
  
  # -------------------------------------------------------------------
  # 4) Tabla del fieldbook (primeras filas)
  # -------------------------------------------------------------------
  output$tbl_rc_book <- DT::renderDataTable({
    des <- rc_design(); shiny::req(des)
    fb  <- des$fieldBook
    DT::datatable(
      head(fb, 20),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  # -------------------------------------------------------------------
  # 5) Descarga del fieldbook en CSV
  # -------------------------------------------------------------------
  output$ui_dl_rc <- renderUI({
    shiny::req(rc_design())
    downloadButton(
      ns("dl_rc_csv"),
      "Descargar fieldbook (CSV)",
      class = "btn btn-secondary w-100"
    )
  })
  
  output$dl_rc_csv <- downloadHandler(
    filename = function() {
      paste0("row_column_fieldbook_", Sys.Date(), ".csv")
    },
    content = function(file) {
      des <- rc_design(); shiny::req(des)
      utils::write.csv(des$fieldBook, file, row.names = FALSE)
    }
  )
}

# Pestaña 4: Simulación & análisis (RCBD vs Row-Column)
pestanna4_session7_v3_server <- function(input, output, session, rc_design, sim_data) {
  
  # 4.1 Visualización espacial ---------------------------------------------
  output$plot_sim_spatial <- renderPlot({
    sim <- sim_data(); shiny::req(sim)
    df   <- sim$data
    c_row <- sim$cols$row
    c_col <- sim$cols$col
    
    # Asegurar numéricos para scale_y_reverse (defensa contra 'Row1' o factores)
    df[[c_row]] <- as.numeric(as.factor(df[[c_row]]))
    df[[c_col]] <- as.numeric(as.factor(df[[c_col]]))
    
    ggplot2::ggplot(df, ggplot2::aes(x = .data[[c_col]], y = .data[[c_row]], fill = .data[["Y"]])) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_viridis_c(name = "Y simulada") +
      ggplot2::scale_y_reverse() +
      ggplot2::labs(
        title = "Mapa espacial de la respuesta simulada",
        x     = "Columna (posible gradiente este–oeste)",
        y     = "Fila (posible gradiente norte–sur)"
      ) +
      ggplot2::theme_minimal(base_size = 12)
  })
  
  
  # 4.2 Comparación de modelos: RCBD / CRD vs Row-Column LMM --------------
  
  # Modelo 1: RCBD o CRD según nº de REP
  output$out_rcbd <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df   <- sim$data
    c_trt <- sim$cols$trt
    
    # Aseguramos que existe una columna REP (aunque sea única)
    if (!"REP" %in% names(df)) {
      df$REP <- factor("1")
    } else {
      df$REP <- factor(df$REP)
    }
    
    n_rep <- length(unique(df$REP))
    
    if (n_rep < 2) {
      cat("Modelo 1: CRD (sin bloque explícito)\n")
      cat("Fórmula ajustada: Y ~ Tratamiento\n\n")
      fmla <- stats::as.formula(paste("Y ~", c_trt))
      m <- stats::lm(fmla, data = df)
      print(anova(m))
    } else {
      cat("Modelo 1: RCBD (REP como bloque)\n")
      cat("Fórmula ajustada (LMM): Y ~ Tratamiento + (1|REP)\n\n")
      suppressPackageStartupMessages({
        library(lmerTest)
      })
      fmla <- stats::as.formula(paste("Y ~", c_trt, "+ (1|REP)"))
      m <- lmerTest::lmer(fmla, data = df)
      print(anova(m))
      cat("\nAIC del modelo RCBD:", stats::AIC(m), "\n")
    }
  })
  
  
  # Modelo 2: Row-Column LMM
  output$out_rclmm <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df    <- sim$data
    c_trt <- sim$cols$trt
    c_row <- sim$cols$row
    c_col <- sim$cols$col
    
    # Aseguramos REP
    if (!"REP" %in% names(df)) {
      df$REP <- factor("1")
    } else {
      df$REP <- factor(df$REP)
    }
    df[[c_row]] <- factor(df[[c_row]])
    df[[c_col]] <- factor(df[[c_col]])
    
    n_rep <- length(unique(df$REP))
    
    suppressPackageStartupMessages({
      library(lmerTest)
    })
    
    if (n_rep < 2) {
      cat("Modelo 2: Row-Column LMM sin REP\n")
      cat("Fórmula ajustada: Y ~ Tratamiento + (1|Row) + (1|Col)\n\n")
      fmla <- stats::as.formula(
        paste("Y ~", c_trt, "+ (1|", c_row, ") + (1|", c_col, ")")
      )
    } else {
      cat("Modelo 2: Row-Column LMM con REP\n")
      cat("Fórmula ajustada: Y ~ Tratamiento + (1|REP) + (1|REP:Row) + (1|REP:Col)\n\n")
      fmla <- stats::as.formula(
        paste("Y ~", c_trt,
              "+ (1|REP) + (1|REP:", c_row, ") + (1|REP:", c_col, ")")
      )
    }
    
    # Validación de grados de libertad para evitar crash
    n_obs <- nrow(df)
    n_trt_levels <- length(unique(df[[c_trt]]))
    
    if (n_obs <= n_trt_levels && n_rep < 2) {
      cat("ADVERTENCIA: El diseño está saturado (Nº Obs = Nº Tratamientos) y no hay repeticiones externas.\n")
      cat("No quedan grados de libertad para estimar el error tras ajustar los Tratamientos como fijos.\n")
      cat("El modelo mixto no puede converger. Por favor, asegúrate de tener más observaciones que tratamientos (repeticiones).\n")
      return()
    }
    
    m <- lmerTest::lmer(fmla, data = df)
    print(anova(m))
    cat("\n--- Componentes de varianza (VarCorr) ---\n")
    print(lme4::VarCorr(m), comp = c("Variance", "Std.Dev."))
    cat("\nAIC del modelo Row-Column:", stats::AIC(m), "\n")
    
    cat("\nComentario didáctico:\n",
        "- Compara este AIC con el del modelo RCBD/CRD.\n",
        "- Observa si la varianza residual disminuye al incluir Fila y Columna.\n",
        "- Si hay gradiente espacial, el modelo Row-Column suele ajustar mejor.\n")
  })
  
  
  # 4.3 Componentes de varianza & heredabilidad (si se quiere avanzar) ----
  output$out_vc <- renderPrint({
    sim <- sim_data(); shiny::req(sim)
    df    <- sim$data
    c_trt <- sim$cols$trt
    c_row <- sim$cols$row
    c_col <- sim$cols$col
    
    if (!"REP" %in% names(df)) {
      df$REP <- factor("1")
    } else {
      df$REP <- factor(df$REP)
    }
    df[[c_row]] <- factor(df[[c_row]])
    df[[c_col]] <- factor(df[[c_col]])
    
    n_rep <- length(unique(df$REP))
    
    suppressPackageStartupMessages({
      library(lmerTest)
    })
    
    if (n_rep < 2) {
      fmla <- stats::as.formula(
        paste("Y ~", c_trt, "+ (1|", c_row, ") + (1|", c_col, ")")
      )
    } else {
      fmla <- stats::as.formula(
        paste("Y ~", c_trt,
              "+ (1|REP) + (1|REP:", c_row, ") + (1|REP:", c_col, ")")
      )
    }
    
    # Validación de grados de libertad
    n_obs <- nrow(df)
    n_trt_levels <- length(unique(df[[c_trt]]))
    if (n_obs <= n_trt_levels && n_rep < 2) {
      cat("No se puede ajustar el modelo (diseño saturado) para mostrar componentes de varianza.\n")
      cat("Aumenta el número de repeticiones o reduce el número de tratamientos.")
      return()
    }
    
    m <- lmerTest::lmer(fmla, data = df)
    
    cat("Componentes de varianza (modelo Row-Column):\n")
    print(lme4::VarCorr(m), comp = c("Variance", "Std.Dev."))
    
    # Opcional: bosquejo de heredabilidad (muy simplificado, para mejoramiento)
    vc <- lme4::VarCorr(m)
    sigma_g <- as.numeric(vc[[c_trt]])[1] %||% NA  # si Trt es aleatorio; aquí usualmente es fijo
    sigma_e <- attr(vc, "sc")^2
    
    cat("\nNota: si los tratamientos se modelaran como aleatorios, ",
        "a partir de estos componentes podríamos aproximar una heredabilidad.\n")
  })
}

# Pestaña 5: Protocolo para datos reales (Row-Column)
pestanna5_session7_v3_server <- function(input, output, session) {
  # Pestaña solo textual / pedagógica: no se requiere lógica de servidor.
}

# Pestaña 6: Ejercicios prácticos & tareas
pestanna6_session7_v3_server <- function(input, output, session) {
  ns <- session$ns
  
  # UI del botón de descarga
  output$ui_dl_ejemplo_rc <- renderUI({
    downloadButton(
      outputId = ns("dl_ejemplo_rc"),
      label    = "Descargar dataset ejemplo (CSV)",
      class    = "btn btn-outline-secondary w-100"
    )
  })
  
  # Handler de descarga: genera un diseño 4×6 con 24 tratamientos y gradiente espacial
  output$dl_ejemplo_rc <- downloadHandler(
    filename = function() {
      paste0("row_column_ejemplo_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Parámetros del diseño ejemplo
      nr  <- 4   # nº de filas
      nc  <- 6   # nº de columnas
      nt  <- 24  # nº de tratamientos (uno por celda)
      mu  <- 100 # media general
      
      # Desviaciones estándar para la simulación (consistente con los ejercicios)
      sd_trt  <- 5   # variabilidad entre tratamientos (señal)
      sd_row  <- 3   # gradiente de fila (suave)
      sd_col  <- 15  # gradiente de columna (fuerte)
      sd_eps  <- 4   # error residual
      
      # Grid fila-columna
      grid <- expand.grid(
        Row = 1:nr,
        Col = 1:nc
      )
      
      # Etiquetas de tratamientos: G01, G02, ..., G24
      trt_labels <- paste0("G", sprintf("%02d", 1:nt))
      
      # Asignar cada tratamiento exactamente una vez
      set.seed(123)
      grid$Trt <- sample(trt_labels, size = nrow(grid), replace = FALSE)
      
      # Una sola repetición (Rep = 1); se podría extender a más
      grid$Rep <- 1L
      
      # Efectos simulados
      eff_trt <- stats::rnorm(nt, mean = 0, sd = sd_trt)
      names(eff_trt) <- trt_labels
      
      eff_row <- stats::rnorm(nr, mean = 0, sd = sd_row)
      names(eff_row) <- as.character(1:nr)
      
      eff_col <- stats::rnorm(nc, mean = 0, sd = sd_col)
      names(eff_col) <- as.character(1:nc)
      
      # Construir la respuesta Y con gradiente espacial
      grid$Y <- mu +
        eff_trt[grid$Trt] +
        eff_row[as.character(grid$Row)] +
        eff_col[as.character(grid$Col)] +
        stats::rnorm(nrow(grid), mean = 0, sd = sd_eps)
      
      # Exportar a CSV
      utils::write.csv(grid, file, row.names = FALSE)
    }
  )
}

# Pestaña 7: Referencias
pestanna7_session7_v3_server <- function(input, output, session) {
  # Pestaña puramente explicativa: no requiere lógica de servidor.
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session7_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers
  has_pkg <- function(p) requireNamespace(p, quietly=TRUE)

  # Reactives
  rc_design <- reactiveVal(NULL)

  sim_data <- eventReactive(input$btn_sim_run, {
    des <- rc_design()
    if (is.null(des)) {
      # Default fallback: 4x5 grid, 10 tratamientos (para tener 2 reps/tratamiento)
      nr=4; nc=5; nt=10
      book <- expand.grid(Row=1:nr, Col=1:nc)
      # Repetir tratamientos para tener grados de libertad del error
      trt_seq <- rep(paste0("T", 1:nt), length.out = nr*nc)
      book$Trt <- sample(trt_seq)
      book$Rep <- 1
      fb <- book
    } else {
      fb <- des$fieldBook
    }

    names(fb) <- toupper(names(fb))
    c_row <- grep("ROW", names(fb), value=TRUE)[1]
    c_col <- grep("COL", names(fb), value=TRUE)[1]
    c_trt <- grep("TRT|TREAT|ENTRY", names(fb), value=TRUE)[1]

    mu <- input$sim_mu
    s_g <- input$sim_sigma_g
    s_r <- input$sim_sigma_row
    s_c <- input$sim_sigma_col
    s_e <- input$sim_sigma_e

    trts <- unique(fb[[c_trt]])
    eff_t <- rnorm(length(trts), 0, s_g); names(eff_t) <- trts

    rows <- unique(fb[[c_row]])
    eff_r <- rnorm(length(rows), 0, s_r); names(eff_r) <- rows

    cols <- unique(fb[[c_col]])
    eff_c <- rnorm(length(cols), 0, s_c); names(eff_c) <- cols

    fb$Y <- NA
    for(i in 1:nrow(fb)) {
      t_val <- as.character(fb[[c_trt]][i])
      r_val <- as.character(fb[[c_row]][i])
      c_val <- as.character(fb[[c_col]][i])

      et <- if(t_val %in% names(eff_t)) eff_t[t_val] else 0

      fb$Y[i] <- mu + et + eff_r[r_val] + eff_c[c_val] + rnorm(1, 0, s_e)
    }

    list(data = fb, cols = list(row=c_row, col=c_col, trt=c_trt))
  })

  # Call tab servers
  pestanna1_session7_v3_server(input, output, session)
  pestanna2_session7_v3_server(input, output, session)
  pestanna3_session7_v3_server(input, output, session, rc_design, has_pkg)
  pestanna4_session7_v3_server(input, output, session, rc_design, sim_data)
  pestanna5_session7_v3_server(input, output, session)
  pestanna6_session7_v3_server(input, output, session)
  pestanna7_session7_v3_server(input, output, session)
}
