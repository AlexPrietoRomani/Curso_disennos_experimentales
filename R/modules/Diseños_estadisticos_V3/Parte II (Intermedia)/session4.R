# R/modules/Diseños_estadisticos_V3/Parte II (Intermedia)/session4.R

# -------------------------------------------------------------------------
# UI Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Contexto, objetivos y limitaciones del ANOVA
pestanna1_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "1) Contexto, objetivos y limitaciones",
    
    # Contenedor general
    tags$div(
      class = "mb-3",
      
      # Encabezado
      tags$div(
        class = "lead",
        tags$h4("De ANOVA clásico a Modelos Lineales Mixtos (LMM)"),
        tags$p(
          "En la Sesión 2 trabajaste con diseños en bloques completos al azar (RCBD) y ",
          "ANOVA clásico usando ", code("aov()"), ". El modelo típico era algo como:",
          tags$br(),
          strong("y ~ tratamiento + bloque"),
          " con todos los efectos tratados como fijos y datos balanceados ",
          "(mismo número de parcelas por tratamiento y por bloque)."
        ),
        tags$p(
          "Esa configuración funciona bien como laboratorio de ideas, pero en campo ",
          "las condiciones reales son más complejas: parcelas perdidas, bloques incompletos, ",
          "ensayos repetidos por año o sitio, etc. En ese contexto, los ",
          strong("Modelos Lineales Mixtos (LMM)"),
          " son la extensión natural del ANOVA."
        )
      ),
      
      # Bloque 1: Repaso Sesión 2 (RCBD + ANOVA)
      bslib::card(
        bslib::card_header("1. Repaso rápido: ¿qué hacíamos en la Sesión 2 con RCBD?"),
        tags$ul(
          tags$li(
            "Diseño base: RCBD completo con todos los tratamientos dentro de cada bloque."
          ),
          tags$li(
            "Modelo ajustado con ANOVA clásico: ",
            code("y ~ tratamiento + bloque"),
            " usando ", code("aov()"), "."
          ),
          tags$li(
            "Interpretación: interés principal en el efecto de ", strong("tratamiento"),
            " (comparar medias); el factor ", strong("bloque"),
            " se incorporaba para reducir la variabilidad residual."
          ),
          tags$li(
            "Supuesto silencioso: el diseño está ",
            strong("balanceado"),
            " (mismas repeticiones por tratamiento y bloque)."
          ),
          tags$li(
            "Otro supuesto silencioso: tratábamos al bloque como un efecto ",
            strong("fijo"),
            " (no pensábamos explícitamente en bloques como una muestra de una población mayor)."
          )
        ),
        tags$p(
          class = "small text-muted",
          "El ANOVA clásico se puede ver como un caso particular de modelo lineal ",
          "(GLM gaussiano) con estructura relativamente simple y datos bien comportados."
        )
      ),
      
      # Bloque 2: Problemas del mundo real
      bslib::card(
        bslib::card_header("2. Problemas del mundo real: desbalance y factores aleatorios"),
        tags$p(
          "En experimentos agronómicos reales rara vez se cumplen al 100% los supuestos anteriores. ",
          "Algunas situaciones frecuentes:"
        ),
        tags$ul(
          tags$li(
            strong("Datos desbalanceados:"),
            " parcelas perdidas por problemas de manejo, daño por plagas, ",
            "bloques incompletos o cambios en el número de parcelas entre años."
          ),
          tags$li(
            strong("Bloques como muestra de una heterogeneidad mayor:"),
            " los bloques representan franjas del campo, camas de cultivo, potreros o ",
            "sub-lotes que podríamos ver como una muestra de muchas posibles unidades similares."
          ),
          tags$li(
            strong("Años y sitios múltiples:"),
            " el mismo tratamiento se evalúa en distintos años, localidades o fundos. ",
            "En ese contexto, año y sitio se interpretan mejor como efectos ",
            strong("aleatorios"),
            " (muestras de un universo de condiciones), no como factores fijos específicos."
          ),
          tags$li(
            strong("Dependencia entre observaciones:"),
            " unidades dentro de un mismo bloque/año/sitio tienden a parecerse más entre sí; ",
            "si ignoramos esa estructura, un modelo lineal simple puede subestimar la variabilidad ",
            "y producir p-valores demasiado optimistas."
          )
        ),
        tags$div(
          class = "alert alert-warning",
          strong("Limitación clave del ANOVA clásico: "),
          "cuando hay desbalance o factores que en realidad son aleatorios, ",
          "las Sumas de Cuadrados Tipo I en ", code("aov()"),
          " dependen del orden de los términos y la inferencia puede cambiar ",
          "aun usando la misma base de datos."
        )
      ),
      
      # Bloque 3: Solución general – Modelos Lineales Mixtos
      bslib::card(
        bslib::card_header("3. Solución general: Modelos Lineales Mixtos (LMM)"),
        tags$p(
          "Los ", strong("Modelos Lineales Mixtos (LMM)"),
          " extienden el ANOVA/GLM para manejar explícitamente efectos ",
          strong("fijos"), " y ", strong("aleatorios"),
          " en un mismo modelo. Algunas ideas centrales:"
        ),
        tags$ul(
          tags$li(
            strong("Separación de componentes de varianza:"),
            " además de la varianza residual, se estima la varianza atribuible a bloques, ",
            "años, sitios, etc. Esto permite cuantificar cuánta heterogeneidad explica cada fuente."
          ),
          tags$li(
            strong("Robustez ante desbalance:"),
            " el ajuste por ", em("máxima verosimilitud restringida (REML)"),
            " permite trabajar con células vacías o números desiguales de observaciones ",
            "por combinación de factores, sin redefinir toda la inferencia."
          ),
          tags$li(
            strong("Efectos fijos vs aleatorios bien definidos:"),
            " tratamientos quedan como ", strong("fijos"),
            " (interés en estimar y comparar sus medias), mientras que bloques/años/sitios ",
            "pueden ser ", strong("aleatorios"),
            " (interés en la varianza entre ellos y en generalizar a ambientes similares)."
          ),
          tags$li(
            strong("Mismo lenguaje que en RCBD:"),
            " el RCBD clásico puede reescribirse como:",
            tags$br(),
            code("rend ~ tratamiento + (1 | bloque)"),
            " donde ", code("(1 | bloque)"), " indica un intercepto aleatorio por bloque."
          )
        ),
        tags$div(
          class = "alert alert-info",
          strong("Idea central para esta sesión: "),
          "en un RCBD perfectamente balanceado, el contraste de tratamientos suele ser muy similar ",
          "entre ANOVA clásico y LMM. La verdadera ventaja de los LMM aparece cuando ",
          strong("hay desbalance o queremos inferir más allá de los bloques/años específicos"),
          " observados."
        ),
        tags$p(
          class = "small text-muted",
          "Lecturas recomendadas para profundizar: Yang (2010, mixed-model analysis en agricultura), ",
          "Bates et al. (2015, ", code("lme4"), "), Kuznetsova et al. (2017, ", code("lmerTest"), 
          ") y Gbur et al. (2012, GLMM en ciencias agrarias)."
        )
      ),
      
      # Bloque 4: Resultados de aprendizaje
      bslib::card(
        bslib::card_header("4. ¿Qué podrás hacer al final de la sesión?"),
        tags$p(
          "Esta sesión está pensada como un puente entre el ANOVA clásico y los modelos mixtos ",
          "que se usan en artículos y reportes agronómicos actuales."
        ),
        tags$ul(
          tags$li(
            strong("Distinguir efectos fijos y aleatorios"),
            " en un ensayo de campo (p. ej. tratamientos vs bloques/años/sitios) ",
            "a partir de la pregunta de inferencia."
          ),
          tags$li(
            strong("Reconocer cuándo el ANOVA clásico empieza a fallar"),
            " o volverse incómodo: desbalance, celdas vacías, estructuras complejas de bloque."
          ),
          tags$li(
            strong("Ajustar un modelo mixto simple en RCBD"),
            " usando ", code("lmer()"), " / ", code("lmerTest"),
            " para comparar tratamientos con bloque como efecto aleatorio."
          ),
          tags$li(
            strong("Interpretar VarCorr e ICC"),
            " como medidas de cuánta variación procede de los bloques ",
            "y qué tan fuerte es la correlación intra-bloque (pseudorreplicación)."
          )
        ),
        tags$p(
          class = "small text-muted",
          "Estos objetivos conectan directamente con las pestañas siguientes: ",
          strong("P2"), " (decisión fijo/aleatorio), ",
          strong("P3"), " (comparar aov() vs lmer() en un RCBD simulado), y ",
          strong("P4"), " (ejercicios prácticos y reporte reproducible)."
        )
      )
    )
  )
}

# Pestaña 2: Fijos vs Aleatorios
pestanna2_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "2) Fijos vs Aleatorios",
    
    # Título / framing
    tags$div(class = "mb-3",
      h4("Decisión conceptual: ¿qué quiero inferir?"),
      tags$p(
        "El paso clave antes de ajustar un modelo mixto es decidir qué factores tratar como ",
        strong("efectos fijos"), " y cuáles como ", strong("efectos aleatorios"), ". ",
        "Esa decisión no es matemática, sino de ", strong("pregunta científica"),
        " y de diseño del experimento."
      ),
      tags$p(
        class = "small text-muted",
        "Meta de esta pestaña: que puedas responder rápido “esto va a la izquierda de la fórmula (fijo) ",
        "o dentro de ", code('(1| )'), " (aleatorio)” cuando armes un modelo con ", code("lmer()"), "."
      )
    ),
    
    # Dos tarjetas: Fijos vs Aleatorios
    fluidRow(
      column(
        width = 6,
        tags$div(class = "card h-100 shadow-sm",
          tags$div(class = "card-header bg-light",
            strong("Efectos Fijos (Fixed Effects)")
          ),
          tags$div(class = "card-body",
            tags$p(
              "Se usan cuando te interesan ", strong("específicamente"),
              " los niveles observados (no una población más grande)."
            ),
            tags$ul(
              tags$li("Niveles concretos y etiquetados: 0, 50, 100 kg N/ha; Variedad A, B, C."),
              tags$li("Objetivo: comparar medias entre niveles (contrastes T1 vs T2, etc.)."),
              tags$li("Los niveles se interpretan uno por uno (cada coeficiente β tiene interés directo)."),
              tags$li(
                "En R: aparecen ", strong("a la izquierda"), " de la fórmula, por ej.: ",
                code("rend ~ tratamiento + (1|bloque)")
              )
            ),
            tags$hr(),
            tags$strong("Ejemplos agro (efectos fijos):"),
            tags$ul(
              tags$li(
                "Ensayo de dosis de N en una sola finca, un año: ",
                em("dosis de N"), " como fijo (0, 50, 100 kg N/ha)."
              ),
              tags$li(
                "Ensayo de variedades de arándano en una estación experimental: ",
                em("variedad"), " como fija (Var. A, B, C)."
              ),
              tags$li(
                "Comparación de sistemas de labranza (convencional vs. mínima vs. cero): ",
                em("labranza"), " como fija."
              )
            )
          )
        )
      ),
      column(
        width = 6,
        tags$div(class = "card h-100 shadow-sm",
          tags$div(class = "card-header bg-light",
            strong("Efectos Aleatorios (Random Effects)")
          ),
          tags$div(class = "card-body",
            tags$p(
              "Se usan cuando los niveles observados son una ",
              strong("muestra de una población mayor"),
              " (bloques, años, sitios) y el interés es la ", strong("varianza"),
              " entre ellos, no cada nivel en particular."
            ),
            tags$ul(
              tags$li("Niveles numerosos o intercambiables (Bloque1, Bloque2, …; Año1, Año2, …)."),
              tags$li("Objetivo: cuantificar cuánta variación aporta ese factor (heterogeneidad)."),
              tags$li(
                "En R (", code("lme4 / lmerTest"), "): se especifican dentro de ",
                code("(1 | factor_aleatorio)"),
                " como interceptos aleatorios."
              ),
              tags$li(
                "Ejemplo típico en RCBD: ",
                code("rend ~ tratamiento + (1|bloque)")
              )
            ),
            tags$hr(),
            tags$strong("Ejemplos agro (efectos aleatorios):"),
            tags$ul(
              tags$li(
                "Ensayo de dosis de N en una sola finca, un año: ",
                em("bloque"), " como aleatorio (muestra de la variabilidad espacial en esa finca)."
              ),
              tags$li(
                "Ensayo multi-sitio (5 fundos): ",
                em("tratamiento fijo, sitio aleatorio"),
                " si quieres generalizar a “fundos similares”, no solo a esos 5."
              ),
              tags$li(
                "Serie multi-año en la misma finca: ",
                em("año aleatorio"),
                " si el interés es “años típicos” y no comparar Año 1 vs Año 2 en particular."
              )
            )
          )
        )
      )
    ),
    
    tags$hr(),
    
    # Matriz de decisión
    h5("Matriz de decisión rápida"),
    tags$table(
      class = "table table-sm table-bordered",
      tags$thead(
        tags$tr(
          tags$th("Característica"),
          tags$th("Efecto Fijo"),
          tags$th("Efecto Aleatorio")
        )
      ),
      tags$tbody(
        tags$tr(
          tags$td("Inferencia principal"),
          tags$td("Entre niveles específicos (T1 vs T2 vs T3)."),
          tags$td("A una población de niveles (sitios, años, bloques similares).")
        ),
        tags$tr(
          tags$td("Tipo de niveles"),
          tags$td("Pocos, etiquetados y de interés directo."),
          tags$td("Muchos, intercambiables o vistos como muestra.")
        ),
        tags$tr(
          tags$td("Ejemplos agro"),
          tags$td("Dosis, variedad puntual, sistema de labranza."),
          tags$td("Bloque, año, localidad, potrero, finca.")
        ),
        tags$tr(
          tags$td("Sintaxis típica en R"),
          tags$td(code("rend ~ tratamiento")),
          tags$td(code("rend ~ tratamiento + (1|bloque)"))
        ),
        tags$tr(
          tags$td("Pregunta típica"),
          tags$td("“¿A ≠ B?” (diferencias de medias)."),
          tags$td("“¿Cuánta varianza aporta g?” (ICC, VarCorr).")
        )
      )
    ),
    
    # Regla de dedo destacada
    tags$div(
      class = "alert alert-info mt-3",
      tags$strong("Regla de dedo:"),
      tags$ul(
        tags$li(
          strong("Efecto fijo"),
          ": cuando las categorías son pocas, conocidas, y te interesa cada nivel ",
          "(p. ej., cada dosis o variedad)."
        ),
        tags$li(
          strong("Efecto aleatorio"),
          ": cuando las categorías son muchas o son una muestra de una población mayor ",
          "(bloques, sitios, años) y te interesa la ",
          strong("variabilidad"),
          " que introducen, no cada nivel por separado."
        )
      )
    ),
    
    tags$p(
      class = "small text-muted",
      "En el resto de la sesión trabajaremos principalmente con ",
      strong("bloque como aleatorio en RCBD"),
      " (", code("rend ~ tratamiento + (1|bloque)"), "), ",
      "pero la misma lógica se extiende a ", strong("año"), " y ", strong("sitio"),
      " en ensayos multi-año o multi-sitio."
    )
  )
}

# Pestaña 3: RCBD balanceado — aov() vs lmer()
pestanna3_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "3) RCBD balanceado: aov() vs lmer()",
    
    tags$div(
      class = "mb-3",
      tags$p(
        strong("Objetivo de esta pestaña:"),
        " ver qué sucede en el ", strong("caso ideal (RCBD balanceado, sin celdas perdidas)"),
        " cuando comparamos:"
      ),
      tags$ul(
        tags$li(code("m1: aov(rend ~ tratamiento + bloque)"), " → bloque tratado como ", strong("fijo")),
        tags$li(code("m2: aov(rend ~ tratamiento)"), " → bloque ignorado"),
        tags$li(code("m3: lmer(rend ~ tratamiento + (1|bloque))"), " → bloque como ", strong("aleatorio"), " (LMM, REML)")
      ),
      tags$p(
        "La idea es que veas que, con un diseño bien comportado, ",
        strong("aov() y lmer() cuentan la misma historia para tratamientos"),
        " y que el LMM aporta además la ",
        strong("varianza de bloque e ICC"),
        " (cuánta heterogeneidad capturan los bloques)."
      )
    ),
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
        tags$h5("Simulador RCBD balanceado"),
        tags$p(
          class = "text-muted small",
          "Usamos un Diseño de Bloques Completos al Azar (RCBD) con todas las celdas presentes ",
          "(sin datos perdidos). Cada combinación tratamiento × bloque tiene una parcela."
        ),
        numericInput(
          ns("n_trt"), "N° tratamientos:",
          value = 4, min = 2, max = 10, step = 1
        ),
        numericInput(
          ns("n_blk"), "N° bloques:",
          value = 6, min = 2, max = 30, step = 1
        ),
        numericInput(
          ns("mu_base"), "Media base (μ):",
          value = 100, min = -1e4, max = 1e4, step = 1
        ),
        sliderInput(
          ns("efecto_lineal"),
          "Efecto lineal de tratamiento (incremento por nivel):",
          min = -50, max = 50, value = 10, step = 1
        ),
        sliderInput(
          ns("sd_blk"),
          "SD bloque (σ_block): heterogeneidad entre bloques",
          min = 0, max = 50, value = 12, step = 1
        ),
        sliderInput(
          ns("sd_res"),
          "SD residual (σ_res): variabilidad dentro de bloque",
          min = 1, max = 60, value = 15, step = 1
        ),
        actionButton(
          ns("run_sim_bal"),
          "Simular RCBD balanceado y ajustar modelos",
          class = "btn btn-primary w-100 mt-2"
        ),
        tags$p(
          class = "small text-muted mt-2",
          "Recomendación didáctica: prueba valores de σ_block altos (p. ej. 25–30) para ver ",
          "cómo crece la importancia del bloqueo (ICC) y cómo cambia el modelo sin bloque."
        )
      ),
      
      mainPanel(
        width = 8,
        
        # Bloque 1: Resumen del diseño y balance
        bslib::card(
          bslib::card_header("Balance del diseño (RCBD completo)"),
          fluidRow(
            column(
              width = 6,
              tags$p(
                strong("Tabla de conteos tratamiento × bloque"),
                " — en un RCBD balanceado, todos los conteos deben ser 1."
              ),
              DT::dataTableOutput(ns("s4_tab_design_counts"))
            ),
            column(
              width = 6,
              tags$p(
                class = "mb-1",
                strong("Mensaje sobre el balance:")
              ),
              verbatimTextOutput(ns("s4_msg_balance")),
              tags$p(
                class = "small text-muted mt-3",
                "También mostramos las primeras filas del dataset simulado para que veas la estructura ",
                "típica: columnas ", code("tratamiento"), ", ", code("bloque"), " y ", code("rend"), "."
              ),
              DT::dataTableOutput(ns("s4_tab_data_head"))
            )
          )
        ),
        
        tags$hr(),
        
        # Bloque 2: Comparación de modelos
        bslib::card(
          bslib::card_header("Comparación de modelos para el efecto de tratamiento"),
          tags$p(
            "Ajustamos tres modelos sobre el mismo RCBD balanceado y resumimos el p-valor del ",
            strong("efecto tratamiento"),
            " en cada uno:"
          ),
          DT::dataTableOutput(ns("s4_tab_modelos_resumen")),
          tags$p(
            class = "small text-muted",
            "En un diseño balanceado, el mensaje sobre tratamientos suele ser muy similar entre ",
            code("aov(rend ~ tratamiento + bloque)"), " y ",
            code("lmer(rend ~ tratamiento + (1|bloque))"),
            ". El modelo que ignora bloque (", code("aov(rend ~ tratamiento)"), ") ",
            "puede subestimar la incertidumbre si la variabilidad entre bloques es grande."
          )
        ),
        
        tags$hr(),
        
        # Bloque 3: Detalle de cada modelo
        bslib::card(
          bslib::card_header("Detalles de cada ajuste (salidas completas)"),
          fluidRow(
            column(
              width = 4,
              tags$h6("m1: aov(rend ~ tratamiento + bloque)"),
              verbatimTextOutput(ns("s4_out_aov_blk"))
            ),
            column(
              width = 4,
              tags$h6("m2: aov(rend ~ tratamiento)  [sin bloque]"),
              verbatimTextOutput(ns("s4_out_aov_noblk"))
            ),
            column(
              width = 4,
              tags$h6("m3: lmer(rend ~ tratamiento + (1|bloque))"),
              verbatimTextOutput(ns("s4_out_lmer"))
            )
          )
        ),
        
        tags$hr(),
        
        # Bloque 4: Varianza de bloque e ICC
        bslib::card(
          bslib::card_header("Varianza de bloque e ICC (Modelo mixto)"),
          fluidRow(
            column(
              width = 6,
              tags$h6("Componentes de varianza (VarCorr)"),
              verbatimTextOutput(ns("s4_out_varcorr"))
            ),
            column(
              width = 6,
              tags$h6("ICC y mensaje interpretativo"),
              verbatimTextOutput(ns("s4_out_icc")),
              tags$p(
                class = "small text-muted",
                "ICC (Intraclass Correlation Coefficient) ≈ proporción de la varianza total ",
                "atribuible a diferencias entre bloques. ",
                "Un ICC alto significa que el bloqueo está capturando mucha heterogeneidad del campo."
              )
            )
          )
        ),
        
        tags$hr(),
        
        # Mensaje didáctico final
        tags$div(
          class = "alert alert-info",
          tags$strong("Mensaje clave de esta pestaña: "),
          "en un RCBD bien balanceado, ",
          strong("aov() y lmer() cuentan una historia muy similar para tratamientos"),
          ". El LMM agrega la descomposición de varianza de bloque e ICC, ",
          "que ayudan a cuantificar la relevancia del bloqueo. ",
          "El beneficio fuerte de los modelos mixtos aparece cuando el diseño se rompe ",
          "(datos perdidos, bloques incompletos, multi-año/multi-sitio), lo que exploraremos en la siguiente pestaña."
        )
      )
    )
  )
}

# Pestaña 4: RCBD desbalanceado – sensibilidad de aov() vs robustez de lmer()
pestanna4_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "4) RCBD desbalanceado",
    withMathJax(
      tags$div(
        class = "mb-3",
        tags$h4("RCBD con celdas perdidas: ¿qué pasa con el ANOVA?"),
        tags$p(
          "En un Diseño de Bloques Completos al Azar (RCBD) ideal, todas las combinaciones ",
          code("tratamiento × bloque"), " están presentes. En la práctica se pierden parcelas (plantas enfermas, ",
          "errores de registro, etc.), lo que genera un ", strong("diseño desbalanceado"), "."
        ),
        tags$p(
          "En esta pestaña comparamos tres enfoques sobre los mismos datos simulados:"
        ),
        tags$ul(
          tags$li(code("m1: aov(rend ~ tratamiento + bloque)"), " — bloque tratado como fijo."),
          tags$li(code("m2: aov(rend ~ tratamiento)"), " — ignora bloque (modelo \"naïve\")."),
          tags$li(code("m3: lmer(rend ~ tratamiento + (1|bloque))"), " — bloque como aleatorio (LMM, REML).")
        ),
        tags$div(
          class = "alert alert-info small",
          tags$strong("Mensaje clave: "),
          "en presencia de desbalance, el ",
          strong("ANOVA clásico secuencial (Type I)"),
          " puede dar conclusiones distintas según el orden de los términos; ",
          "en un modelo mixto bien especificado, la prueba de tratamientos no depende de ese orden."
        )
      ),

      sidebarLayout(
        sidebarPanel(
          width = 4,
          tags$h5("Simulador RCBD desbalanceado"),
          tags$p(
            class = "small text-muted",
            "1 fila = 1 parcela (combinación tratamiento × bloque). Simulamos pérdida aleatoria de celdas ",
            "para crear desbalance y estudiar su efecto sobre las pruebas F."
          ),
          numericInput(
            ns("s4_n_trt_unbal"),
            "N° tratamientos:",
            value = 4, min = 2, max = 10, step = 1
          ),
          numericInput(
            ns("s4_n_blk_unbal"),
            "N° bloques:",
            value = 5, min = 2, max = 30, step = 1
          ),
          numericInput(
            ns("s4_mu_base_unbal"),
            "Media base (μ):",
            value = 100, min = -1e4, max = 1e4, step = 1
          ),
          sliderInput(
            ns("s4_efecto_lineal_unbal"),
            "Efecto lineal de tratamiento (incremento por nivel):",
            min = -50, max = 50, value = 8, step = 1
          ),
          sliderInput(
            ns("s4_sd_blk_unbal"),
            "SD de bloque (σ_block):",
            min = 0, max = 50, value = 12, step = 1
          ),
          sliderInput(
            ns("s4_sd_res_unbal"),
            "SD residual (σ_res):",
            min = 1, max = 60, value = 15, step = 1
          ),
          tags$hr(),
          sliderInput(
            ns("s4_perc_missing_unbal"),
            "% celdas perdidas (desbalance):",
            min = 5, max = 60, value = 20, step = 5
          ),
          tags$small(
            class = "text-muted d-block mb-2",
            "Sugerencia: usa valores ≥ 15–20% para ver claramente el efecto del desbalance ",
            "en las pruebas F de ", code("aov()"), "."
          ),
          actionButton(
            ns("s4_run_sim_unbal"),
            "Simular RCBD desbalanceado y ajustar modelos",
            class = "btn btn-primary w-100"
          ),
          tags$hr(),
          downloadButton(
            ns("s4_dl_csv_unbal"),
            "Descargar datos simulados (CSV)",
            class = "btn btn-success w-100"
          )
        ),

        mainPanel(
          width = 8,

          # 1) Resumen del diseño y del desbalance
          bslib::card(
            bslib::card_header("Resumen del diseño y del desbalance"),
            tags$p(
              class = "small",
              "Primero verificamos cuántas celdas tratamiento × bloque quedaron observadas ",
              "después de eliminar parcelas (desbalance)."
            ),
            fluidRow(
              column(
                width = 6,
                tags$strong("Vista rápida de los datos (primeras filas)"),
                DT::dataTableOutput(ns("s4_tab_datos_unbal"))
              ),
              column(
                width = 6,
                tags$strong("Conteos por combinación tratamiento × bloque"),
                tags$small(
                  class = "text-muted d-block mb-1",
                  "Las celdas con conteo 0 son combinaciones que faltan (parcelas perdidas)."
                ),
                DT::dataTableOutput(ns("s4_tab_counts_unbal"))
              )
            )
          ),

          tags$hr(),

          # 2) ANOVA clásico con y sin bloque
          bslib::card(
            bslib::card_header("ANOVA clásico (aov) con desbalance"),
            tags$p(
              class = "small",
              "Comparamos dos modelos clásicos: (i) con bloque como fijo; (ii) ignorando bloque. ",
              "Ambos ajustados con ", code("aov()"), " (sumas de cuadrados Type I / secuenciales)."
            ),
            fluidRow(
              column(
                width = 6,
                h6("m1: aov(rend ~ tratamiento + bloque)"),
                verbatimTextOutput(ns("s4_out_aov_trt_blk")),
                tags$p(
                  class = "small text-muted",
                  textOutput(ns("s4_msg_aov_trt_blk"), inline = FALSE)
                )
              ),
              column(
                width = 6,
                h6("m2: aov(rend ~ tratamiento)  (bloque ignorado)"),
                verbatimTextOutput(ns("s4_out_aov_trt_only")),
                tags$p(
                  class = "small text-muted",
                  textOutput(ns("s4_msg_aov_trt_only"), inline = FALSE)
                )
              )
            )
          ),

          tags$hr(),

          # 3) Modelo mixto: tratamiento fijo + bloque aleatorio
          bslib::card(
            bslib::card_header("Modelo Lineal Mixto: lmer(rend ~ tratamiento + (1|bloque))"),
            tags$p(
              class = "small",
              "En el LMM, bloque entra como intercepto aleatorio. Usamos ",
              code("lmerTest::lmer"), " con método REML y tabla ANOVA Type III ",
              "con d.f. de Satterthwaite (p-valores robustos al orden de los términos)."
            ),
            verbatimTextOutput(ns("s4_out_lmer_anova_unbal")),
            fluidRow(
              column(
                width = 6,
                h6("Componentes de varianza (VarCorr)"),
                verbatimTextOutput(ns("s4_out_varcorr_unbal"))
              ),
              column(
                width = 6,
                h6("ICC = σ²_bloque / (σ²_bloque + σ²_residual)"),
                verbatimTextOutput(ns("s4_out_icc_unbal")),
                tags$p(
                  class = "small text-muted",
                  "ICC alto + desbalance + modelo sin bloque ⇒ riesgo de F inflado ",
                  "(falsos positivos por pseudorreplicación intra-bloque)."
                )
              )
            )
          ),

          tags$hr(),

          # 4) Diagnósticos y BLUPs
          bslib::card(
            bslib::card_header("Diagnóstico del LMM y BLUPs de bloque"),
            fluidRow(
              column(
                width = 6,
                plotOutput(ns("s4_plot_resid_fitted_unbal"), height = "260px")
              ),
              column(
                width = 6,
                plotOutput(ns("s4_plot_qq_unbal"), height = "260px")
              )
            ),
            tags$small(
              class = "text-muted d-block mb-2",
              "Como en regresión simple, buscamos residuos sin patrón fuerte y aproximadamente normales; ",
              "las varianzas se interpretan en el contexto del RCBD."
            ),
            plotOutput(ns("s4_plot_ranef_unbal"), height = "260px"),
            tags$small(
              class = "text-muted",
              "Los BLUPs por bloque muestran qué bloques rindieron relativamente por encima o por debajo ",
              "de la media, después de ajustar por tratamientos."
            )
          )
        )
      )
    )
  )
}

# Pestaña 5: Protocolo para datos reales & ejercicios guiados
pestanna5_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "5) Protocolo para datos reales",
    
    # Encabezado
    tags$div(
      class = "mb-3",
      tags$h4("Checklist aplicado a ensayos reales (RCBD con bloque aleatorio)"),
      tags$p(
        class = "text-muted",
        "Objetivo: convertir lo visto en la sesión en un flujo reproducible que puedas aplicar ",
        "a tus propios ensayos (RCBD con tratamiento fijo y bloque/año/sitio aleatorios)."
      )
    ),
    
    # Paso 0 – Entender el diseño
    bslib::card(
      bslib::card_header("Paso 0 – Entender el diseño"),
      tags$p(
        "Antes de tocar el teclado, clarificar el diseño experimental y la pregunta de interés. ",
        "En esta sesión asumimos un ", strong("RCBD (Diseño de Bloques Completos al Azar)"),
        " donde cada bloque contiene todos los tratamientos una vez."
      ),
      tags$ul(
        tags$li(
          strong("¿Cuál es la unidad experimental? "),
          "Parcela, cama, maceta, animal, planta, etc."
        ),
        tags$li(
          strong("Tratamientos (fijos): "),
          "dosis, variedades, manejos que deseas comparar explícitamente ",
          "(niveles específicos de interés)."
        ),
        tags$li(
          strong("Bloque / Año / Sitio (aleatorios): "),
          "fuentes de variación que se interpretan como muestra de una población mayor ",
          "(camas, potreros, fundos, años)."
        )
      ),
      tags$div(
        class = "alert alert-info small",
        strong("Regla práctica: "),
        "si te interesan las categorías una por una (pocas, conocidas) → ",
        em("efecto fijo"), ". ",
        "Si las categorías son muchas o representan una población más grande ",
        "(bloques, sitios, años) → ", em("efecto aleatorio"), "."
      ),
      tags$p(
        class = "small text-muted mb-0",
        "En esta sesión trabajamos explícitamente con ",
        code("bloque"), " como efecto aleatorio en un RCBD, ",
        "pero la misma lógica se extiende a ", code("año"), " y ", code("sitio"), "."
      )
    ),
    
    # Paso 1 – Explorar desbalance
    bslib::card(
      bslib::card_header("Paso 1 – Explorar desbalance (tamaños de celda)"),
      fluidRow(
        column(
          width = 6,
          tags$p(
            "Usamos la tabla de conteos ", code("count(tratamiento, bloque)"),
            " para revisar si el diseño está balanceado. ",
            "En un RCBD ideal, todas las celdas tienen el mismo número de observaciones."
          ),
          tags$ul(
            tags$li("Balanceado: todas las combinaciones tratamiento–bloque tienen el mismo n."),
            tags$li("Desbalance leve: algunas celdas tienen 1 observación menos."),
            tags$li("Desbalance fuerte: celdas vacías o tamaños muy disparejos.")
          ),
          tags$div(
            class = "alert alert-warning small",
            strong("Interpretación pedagógica: "),
            "si ves muchas celdas vacías o tamaños muy distintos, ",
            "la inferencia basada en ", code("aov()"), " (Sumas de Cuadrados Tipo I) ",
            "se vuelve muy sensible al orden de los términos. En esos casos, ",
            strong("LMM con bloque aleatorio es la opción principal.")
          )
        ),
        column(
          width = 6,
          bslib::card(
            bslib::card_header("Conteo tratamiento × bloque (datos actuales)"),
            tags$small(
              "Si has simulado datos en las pestañas anteriores, aquí verás el conteo ",
              "por tratamiento y bloque del objeto en memoria."
            ),
            DT::dataTableOutput(ns("p5_tab_count"))
          )
        )
      )
    ),
    
    # Paso 2 – Modelo candidato
    bslib::card(
      bslib::card_header("Paso 2 – Especificar el modelo candidato"),
      tags$p(
        "Para un RCBD clásico con una sola respuesta continua (por ejemplo, rendimiento) "
        ,"y un solo factor de tratamiento fijo, el modelo LMM base es:"
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        "mL <- lmer(rend ~ tratamiento + (1 | bloque), data = d, REML = TRUE)"
      ),
      tags$ul(
        tags$li(
          code("tratamiento"), " como efecto fijo → comparamos medias entre niveles."
        ),
        tags$li(
          code("(1 | bloque)"), " como intercepto aleatorio → captura la variación entre bloques ",
          "y ajusta la correlación entre parcelas del mismo bloque."
        )
      ),
      tags$div(
        class = "alert alert-secondary small",
        strong("Comparación con modelos alternativos:"),
        tags$ul(
          tags$li(
            code("rend ~ tratamiento + bloque"),
            " (bloque fijo) sirve como punto de comparación, ",
            "pero no cuantifica la varianza entre bloques."
          ),
          tags$li(
            code("rend ~ tratamiento"),
            " (sin bloque) ignora la estructura del diseño; cuando el ICC es alto, ",
            "tiende a subestimar la variabilidad y producir p-valores demasiado optimistas."
          )
        )
      )
    ),
    
    # Paso 3 – Revisar outputs clave
    bslib::card(
      bslib::card_header("Paso 3 – Revisar outputs del LMM"),
      fluidRow(
        column(
          width = 6,
          tags$h5("3.1 Tabla ANOVA (efecto tratamiento)"),
          tags$p(
            "Con ", code("lmerTest"), " usamos típicamente:",
            tags$pre(
              class = "r-code",
              style = "white-space: pre-wrap; margin-bottom: 4px;",
              "anova(mL, type = 3, ddf = \"Satterthwaite\")"
            ),
            "La fila de ", code("tratamiento"), " entrega el F y el p-valor que responden ",
            "a la pregunta: “¿hay diferencias globales entre tratamientos?”. ",
            "A diferencia de ", code("aov()"), ", este resultado no depende del orden "
            ,"en que escribas tratamiento y bloque en la fórmula."
          )
        ),
        column(
          width = 6,
          tags$h5("3.2 VarCorr e ICC"),
          tags$p(
            "La descomposición de varianzas se obtiene con ", code("VarCorr(mL)"), 
            " e indica cuánta variabilidad está entre bloques y cuánta es residual."
          ),
          tags$pre(
            class = "r-code",
            style = "white-space: pre-wrap; margin-bottom: 4px;",
"vc   <- as.data.frame(lme4::VarCorr(mL))
sig_b <- vc$vcov[vc$grp == \"bloque\" & vc$var1 == \"(Intercept)\"]
sig_e <- vc$vcov[vc$grp == \"Residual\"]
ICC  <- sig_b / (sig_b + sig_e)"
          ),
          tags$p(
            strong("ICC (Intraclass Correlation Coefficient): "),
            "proporción de la varianza total atribuible a diferencias entre bloques."
          ),
          tags$ul(
            tags$li("ICC cercano a 0 → bloque casi irrelevante."),
            tags$li("ICC medio/alto → parcelas del mismo bloque están fuertemente correlacionadas; ignorar el bloque puede inflar falsos positivos.")
          )
        )
      ),
      tags$hr(),
      tags$h5("3.3 Diagnóstico gráfico"),
      tags$p(
        "Como en regresión lineal, revisamos residuos vs. ajustados y Q–Q normal sobre los residuales del LMM ",
        "(típicamente residuales marginales).",
        "El objetivo es detectar heterocedasticidad marcada, colas pesadas o outliers severos."
      ),
      tags$ul(
        tags$li("Nube aleatoria de puntos alrededor de 0 → varianza aproximadamente constante."),
        tags$li("Puntos cerca de la línea en Q–Q → residuos aproximadamente normales."),
        tags$li("Patrones sistemáticos → considerar transformaciones, modelos más complejos o interpretar como estructura biológica del sistema.")
      )
    ),
    
    # Paso 4 – Reporte agronómico y plantilla Quarto
    bslib::card(
      bslib::card_header("Paso 4 – Armar el reporte agronómico (Quarto/R Markdown)"),
      tags$p(
        "Finalmente, el análisis debe aterrizar en un reporte claro para agrónomos, jefes de campo o clientes. ",
        "Una estructura mínima recomendable:"
      ),
      tags$ol(
        tags$li("Descripción del ensayo: diseño, unidad experimental, tratamientos, bloque/año/sitio."),
        tags$li("Exploración del desbalance: tabla tratamiento × bloque, n por celda."),
        tags$li("Modelo usado: ", code("rend ~ tratamiento + (1|bloque)"), 
                " y justificación (bloque como aleatorio)."),
        tags$li("Resultados clave: tabla ANOVA (tratamiento), VarCorr, ICC y diagnóstico gráfico."),
        tags$li("Medias de tratamientos (con sus IC o letras de comparación, si corresponde)."),
        tags$li("Conclusión agronómica: qué tratamiento recomendarías y bajo qué condiciones.")
      ),
      tags$hr(),
      tags$h5("Plantilla Quarto (para copiar y adaptar)"),
      tags$small(
        "Suponiendo que has guardado los datos en ", code("datos_rcbd.csv"),
        " con columnas ", code("tratamiento"), ", ", code("bloque"), ", ", code("rend"), "."
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        HTML(
'```{r}
# Paquetes clave para LMM en ensayos agrícolas
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)  # p-valores tipo III (Satterthwaite)
library(car)       # pruebas adicionales (ej. Levene)

# Paso 0: leer y entender el diseño
d <- read.csv("datos_rcbd.csv")
str(d)
d %>% count(tratamiento, bloque)  # Paso 1: desbalance

# Paso 2: modelos candidatos
m_trt_blk_fix <- aov(rend ~ tratamiento + bloque, data = d)
m_trt_only    <- aov(rend ~ tratamiento, data = d)
m_LMM         <- lmer(rend ~ tratamiento + (1|bloque), data = d, REML = TRUE)

# Paso 3a: contraste global de tratamientos (LMM)
anova(m_LMM, type = 3, ddf = "Satterthwaite")

# Paso 3b: VarCorr e ICC
vc   <- as.data.frame(VarCorr(m_LMM))
sig_b <- vc$vcov[vc$grp == "bloque" & vc$var1 == "(Intercept)"]
sig_e <- vc$vcov[vc$grp == "Residual"]
ICC   <- sig_b / (sig_b + sig_e)
ICC

# Paso 3c: diagnósticos
par(mfrow = c(1, 2))
plot(fitted(m_LMM), resid(m_LMM),
     xlab = "Valores ajustados", ylab = "Residuos")
abline(h = 0, lty = 2, col = 2)
qqnorm(resid(m_LMM)); qqline(resid(m_LMM), col = 2)

# (Opcional) Levene sobre un modelo simple, solo pedagógico
car::leveneTest(rend ~ tratamiento, data = d)

# Paso 4: resumen de medias por tratamiento
d %>%
  group_by(tratamiento) %>%
  summarise(
    n   = n(),
    media = mean(rend, na.rm = TRUE),
    sd    = sd(rend, na.rm = TRUE)
  )
```'
        )
      ),
      tags$div(
        class = "alert alert-success small mt-2",
        strong("Idea final: "),
        "este mismo flujo se puede adaptar a datos reales de tu empresa o tesis. ",
        "Lo que cambia son las columnas (por ejemplo, incluir año o sitio) y la fórmula del modelo, ",
        "pero la lógica RCBD + LMM + ICC + diagnóstico se mantiene."
      )
    )
  )
}

# Pestaña 6: Notas avanzadas & advertencias
pestanna6_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "6) Notas avanzadas & advertencias",
    
    tags$div(
      class = "mb-3",
      tags$h4("Notas avanzadas para usar LMM con criterio"),
      tags$p(
        "Esta pestaña resume advertencias y buenas prácticas que suelen generar dudas ",
        "al trabajar con modelos lineales mixtos (LMM) en agricultura: uso de AIC, ",
        "REML vs ML, pruebas para efectos aleatorios y el concepto de pseudorreplicación."
      ),
      tags$p(
        class = "small text-muted",
        "La idea no es que memorices teoría asintótica, sino que tengas un mapa mental ",
        "claro de qué se puede hacer con cierta confianza y qué decisiones requieren ",
        "cautela e interpretación contextual."
      )
    ),
    
    # 1) AIC, REML y comparación de modelos
    bslib::card(
      bslib::card_header("1) AIC, REML y comparación de modelos"),
      tags$p(
        "El criterio de información de Akaike (AIC) se usa para comparar modelos que compiten ",
        "entre sí: menor AIC ⇒ modelo más parsimonioso, siempre que se cumplan ciertas condiciones."
      ),
      tags$ul(
        tags$li(
          strong("Regla 1 – Comparar siempre sobre la misma respuesta y los mismos datos:"),
          " nada de comparar AIC entre modelos ajustados a bases distintas o después de filtrar ",
          "de forma diferente."
        ),
        tags$li(
          strong("Regla 2 – En modelos mixtos:"),
          " para comparar estructuras de ", strong("efectos fijos"),
          " (por ejemplo, incluir o no una interacción) se recomienda ajustar los modelos ",
          "con ", code("REML = FALSE"), " (máxima verosimilitud, ML) y mantener la ",
          strong("misma estructura de efectos aleatorios"),
          "."
        ),
        tags$li(
          strong("Regla 3 – Comparar AIC de LMM sólo entre modelos compatibles:"),
          " no tiene sentido comparar el AIC de un ", code("lm/aov"),
          " con el de un ", code("lmer()"), " porque la estructura de varianzas es distinta ",
          "y la función de verosimilitud no es la misma."
        ),
        tags$li(
          strong("Regla 4 – Para elegir estructura de aleatorios:"),
          " es habitual comparar modelos anidados con distinta estructura de ", code("(1|...)"),
          " usando AIC o pruebas de razón de verosimilitud; sin embargo, ",
          "las decisiones deben apoyarse también en el diseño experimental y en la pregunta científica."
        )
      ),
      tags$div(
        class = "alert alert-secondary small",
        strong("Resumen práctico: "),
        "si vas a decidir entre varios modelos LMM, define primero la estructura de aleatorios ",
        "que tiene sentido por diseño; luego, para afinar los efectos fijos, usa ",
        code("REML = FALSE"), " y compara AIC entre modelos con los mismos aleatorios. ",
        "Para reportar resultados finales (parámetros, VarCorr, ICC), vuelve a ajustar ",
        "el modelo elegido con ", code("REML = TRUE"), "."
      )
    ),
    
    # 2) Tests para efectos aleatorios
    bslib::card(
      bslib::card_header("2) Pruebas para efectos aleatorios (bloque, año, sitio)"),
      tags$p(
        "Una pregunta frecuente es: “¿realmente necesito el efecto aleatorio de bloque/año/sitio?”. ",
        "Técnicamente se puede usar una prueba de razón de verosimilitud (LRT) entre modelos anidados:"
      ),
      tags$pre(
        class = "r-code",
        style = "white-space: pre-wrap;",
        "m_full  <- lmer(rend ~ tratamiento + (1|bloque), data = d, REML = FALSE)\n",
        "m_red   <- lmer(rend ~ tratamiento,              data = d, REML = FALSE)\n",
        "anova(m_red, m_full)  # LRT: ¿aporta algo (1|bloque)?"
      ),
      tags$ul(
        tags$li(
          "La hipótesis nula es que la varianza de bloque es 0 (bloque irrelevante)."
        ),
        tags$li(
          "Bajo la nula, la varianza está en la frontera del espacio de parámetros (≥ 0), ",
          "por lo que la distribución teórica de la LRT no es exactamente una χ² estándar; ",
          "los p-valores deben leerse como ", strong("guía aproximada"),
          ", no como verdad absoluta."
        ),
        tags$li(
          "En práctica aplicada, la decisión de incluir bloque/año/sitio se basa en: ",
          "diseño del experimento, lógica agronómica, tamaño de la varianza estimada ",
          "e impacto sobre las conclusiones de tratamiento."
        )
      ),
      tags$div(
        class = "alert alert-info small",
        strong("Regla de trabajo: "),
        "si el diseño fue pensado con bloques/años/sitios, por defecto inclúyelos como aleatorios ",
        "en el modelo. Usa LRT/AIC como confirmación, no como único criterio para “apagar” un efecto ",
        "aleatorio que hace sentido por diseño."
      )
    ),
    
    # 3) Pseudorreplicación e ICC
    bslib::card(
      bslib::card_header("3) Pseudorreplicación e ICC"),
      tags$p(
        "La pseudorreplicación (Hurlbert, 1984) ocurre cuando se tratan observaciones no independientes ",
        "como si fueran réplicas independientes. En contextos agrícolas, esto pasa cuando se ignora ",
        "el bloque/año/sitio que estructura la dependencia."
      ),
      tags$ul(
        tags$li(
          "Si hay múltiples parcelas dentro del mismo bloque, año o sitio, las respuestas tienden a ser ",
          "más parecidas entre sí que con parcelas de otros bloques/años/sitios."
        ),
        tags$li(
          "Un modelo ", code("lm/aov"), " sin bloque/año/sitio asume independencia total; ",
          "cuando en realidad existe correlación, los errores estándar de tratamiento se subestiman ",
          "y los p-valores pueden ser demasiado optimistas (riesgo de falsos positivos)."
        ),
        tags$li(
          "En un LMM, el ", strong("ICC (Intraclass Correlation Coefficient)"),
          " cuantifica qué proporción de la varianza total se debe a diferencias ",
          "entre bloques/años/sitios. Un ICC alto indica que hay fuerte correlación intragrupo ",
          "y que sería peligroso ignorar ese efecto aleatorio."
        )
      ),
      tags$div(
        class = "alert alert-warning small",
        strong("Lectura rápida del ICC: "),
        tags$ul(
          tags$li("ICC ≈ 0 → el factor de agrupamiento casi no aporta varianza adicional."),
          tags$li("ICC intermedio (0.05–0.20) → hay correlación intra-bloque relevante."),
          tags$li("ICC alto (> 0.20) → fuerte correlación: ignorar el bloque/año/sitio es, en la práctica, pseudorreplicación.")
        )
      )
    ),
    
    # 4) Notas de implementación y referencias cruzadas
    bslib::card(
      bslib::card_header("4) Notas de implementación en R y lecturas para profundizar"),
      tags$ul(
        tags$li(
          "En RCBD simples, una especificación base recomendada es ",
          code("rend ~ tratamiento + (1|bloque)"),
          ", siguiendo la literatura de modelos mixtos en agricultura."
        ),
        tags$li(
          "Evita mezclar estructuras muy complejas sin respaldo en el diseño (interceptos y pendientes aleatorias) ",
          "si el número de bloques/sitios es bajo."
        ),
        tags$li(
          "Para reportar, combina resultados del LMM (tabla ANOVA, VarCorr, ICC) con gráficos ",
          "claros de medias de tratamientos y, si es pertinente, comparaciones múltiples ajustadas."
        )
      ),
      tags$p(
        class = "small text-muted",
        "La pestaña de ", strong("Referencias"), " al final de la sesión lista algunos textos clave ",
        "para profundizar (Yang, Bates et al., Kuznetsova et al., Gbur et al., Hurlbert, entre otros)."
      )
    )
  )
}

# Pestaña Extra: Conceptos Visuales
pestanna_extra_session4_v3UI <- function(ns) {
  # Definir la ruta base para las imágenes de esta sesión
  base_path <- "images/sesiones/Disenos_estadisticos_V3/optimizada/"
  img_path  <- paste0(base_path, "session4/")
  
  bslib::nav_panel(
    title = "Extra: Conceptos Visuales",
    icon = icon("lightbulb"), # Icono para ideas/conceptos
    
    tags$div(
      class = "container-fluid py-3",
      tags$h4(class = "text-primary mb-4", "Galería Conceptual: De ANOVA a LMM"),
      tags$p(
        class = "lead",
        "Esta pestaña consolida los conceptos más abstractos de la sesión mediante esquemas visuales. ",
        "Úsala como referencia rápida para recordar por qué usamos modelos mixtos y cómo interpretar sus resultados."
      ),
      tags$hr(),
      
      # Navegación interna con pestañas subrayadas
      bslib::navset_card_underline(
        
        # --- Sub-pestaña 1: El problema del desbalance ---
        bslib::nav_panel(
          title = "1. El problema del Desbalance",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-8",
              tags$img(
                src = paste0(img_path, "imbalance_anova_problem.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Esquema comparativo de diseño balanceado vs desbalanceado",
                style = "width: 100%; object-fit: contain;"
              )
            ),
            tags$div(
              class = "col-md-4",
              tags$div(
                class = "alert alert-warning border-warning mt-3 mt-md-0",
                tags$h5("¿Por qué falla el ANOVA clásico aquí?"),
                tags$p(
                  "Cuando la matriz de datos tiene 'huecos' (celdas perdidas), los predictores dejan de ser ortogonales (independientes)."
                ),
                tags$p(
                  "Esto hace que las Sumas de Cuadrados secuenciales (Tipo I, las que usa ", code("aov()"), ") ",
                  "dependan del orden en que escribes las variables. El modelo mixto (usando REML) es robusto a este problema."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 2: Decisión Fijo vs Aleatorio ---
        bslib::nav_panel(
          title = "2. Decisión: ¿Fijo o Aleatorio?",
          tags$div(
            class = "row justify-content-center",
            tags$div(
              class = "col-md-10",
              tags$img(
                src = paste0(img_path, "decision_tree_fixed_random.webp"),
                class = "img-fluid shadow border rounded mx-auto d-block",
                alt = "Árbol de decisión para efectos fijos vs aleatorios",
                style = "max-height: 600px;"
              ),
              tags$div(
                class = "mt-3 text-center text-muted",
                tags$small(
                  "Esta es la decisión conceptual más importante. No depende de la estadística, sino de tu pregunta de investigación y tu diseño."
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 3: Anatomía de la Fórmula ---
        bslib::nav_panel(
          title = "3. Anatomía de la Fórmula LMM",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-12 mb-3",
              tags$img(
                src = paste0(img_path, "lmm_formula_anatomy.webp"),
                class = "img-fluid shadow-sm border rounded",
                alt = "Desglose de la sintaxis de lmer() en R",
                style = "width: 100%;"
              )
            ),
            tags$div(
              class = "col-md-12",
              tags$div(
                class = "card bg-light",
                tags$div(
                  class = "card-body",
                  tags$h6("Traducción rápida:"),
                  tags$ul(
                    tags$li(strong("Tratamiento (Fijo):"), " 'Quiero saber si la media de T1 es distinta a la de T2'."),
                    tags$li(strong("(1 | Bloque) (Aleatorio):"), " 'Sé que hay ruido, y sé que ese ruido se agrupa por bloques. Quiero modelar esa estructura de varianza para no engañarme con los p-valores'.")
                  )
                )
              )
            )
          )
        ),
        
        # --- Sub-pestaña 4: Varianza e ICC ---
        bslib::nav_panel(
          title = "4. Entendiendo el ICC",
          tags$div(
            class = "row align-items-center",
            tags$div(
              class = "col-md-7",
              tags$img(
                src = paste0(img_path, "variance_partition_icc.webp"),
                class = "img-fluid shadow border rounded",
                alt = "Gráfico de pastel mostrando la partición de varianza y el ICC",
                style = "width: 100%;"
              )
            ),
            tags$div(
              class = "col-md-5",
              tags$div(
                class = "alert alert-info border-info mt-3 mt-md-0",
                tags$h5("El Coeficiente de Correlación Intraclase (ICC)"),
                tags$p(
                  "El LMM separa la 'torta' de la varianza total en partes. El ICC te dice qué tan grande es la rebanada del factor de agrupamiento (bloque, año, sitio)."
                ),
                tags$hr(),
                tags$strong("Interpretación práctica:"),
                tags$ul(
                  tags$li("ICC alto = Las observaciones dentro del mismo bloque son muy parecidas entre sí."),
                  tags$li("Si ignoras un ICC alto (usando un modelo lineal simple), cometes ", strong("pseudorreplicación"), " y tus p-valores serán demasiado optimistas.")
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
pestanna7_session4_v3UI <- function(ns) {
  bslib::nav_panel(
    title = "7) Referencias",
    tags$ul(
      tags$li(
        "Yang, R.-C. (2010). Towards understanding and use of mixed-model analysis of agricultural experiments. ",
        em("Canadian Journal of Plant Science"), " 90(5), 605–627. ",
        a(href = "https://cdnsciencepub.com/doi/10.4141/CJPS10049",
          "doi:10.4141/CJPS10049", target = "_blank")
      ),
      tags$li(
        "Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting Linear Mixed-Effects Models Using lme4. ",
        em("Journal of Statistical Software, 67(1)"), ". ",
        a(href = "https://www.jstatsoft.org/v67/i01/",
          "JSS", target = "_blank")
      ),
      tags$li(
        "Kuznetsova, A., Brockhoff, P. B., & Christensen, R. H. B. (2017). lmerTest Package: Tests in Linear Mixed Effects Models. ",
        em("Journal of Statistical Software, 82(13)"), ". ",
        a(href = "https://www.jstatsoft.org/v82/i13/",
          "JSS", target = "_blank")
      ),
      tags$li(
        "Gbur, E. E., Stroup, W. W., et al. (2012). ",
        em("Analysis of Generalized Linear Mixed Models in the Agricultural and Natural Resources Sciences"),
        ". ASA/SSSA/CSSA. ",
        a(href = "https://www.ars.usda.gov/ARSUserFiles/3122/GburEtAl2012.pdf",
          "PDF", target = "_blank")
      ),
      tags$li(
        "Hurlbert, S. (1984). Pseudoreplication and the Design of Ecological Field Experiments. ",
        em("Ecological Monographs, 54(2)"), " 187–211. ",
        a(href = "http://www.jstor.org/stable/1942661",
          "PDF / JSTOR", target = "_blank")
      ),
      tags$li(
        "Fox, J., & Weisberg, S. (Manual del paquete ", code("car"), "). ",
        a(href = "https://cran.r-project.org/package=car/car.pdf",
          "car.pdf", target = "_blank")
      )
    ),
    tags$hr(),
    tags$p(
      class = "small text-muted",
      "Esta sesión enfatiza el uso de modelos mixtos para robustez ante desbalance y correcta ",
      "cuantificación de varianza de bloqueo, siguiendo las recomendaciones de estos autores."
    )
  )
}

# -------------------------------------------------------------------------
# Main UI
# -------------------------------------------------------------------------

session4_v3UI <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "session-title",
        h3("Sesión 4: De ANOVA a Modelos Lineales Mixtos (LMM)")
    ),
    withMathJax(),
    bslib::navset_tab(
      pestanna1_session4_v3UI(ns),
      pestanna2_session4_v3UI(ns),
      pestanna3_session4_v3UI(ns),
      pestanna4_session4_v3UI(ns),
      pestanna5_session4_v3UI(ns),
      pestanna6_session4_v3UI(ns),
      pestanna_extra_session4_v3UI(ns),
      pestanna7_session4_v3UI(ns)
    )
  )
}

# -------------------------------------------------------------------------
# Server Functions per Tab
# -------------------------------------------------------------------------

# Pestaña 1: Contexto, objetivos y limitaciones
pestanna1_session4_v3_server <- function(input, output, session) {
  # No se requiere lógica de servidor para esta pestaña.
  # Solo muestra contenido estático de contexto y objetivos.
}

# Pestaña 2: Fijos vs Aleatorios
pestanna2_session4_v3_server <- function(input, output, session) {
  # No server logic needed (pestaña puramente conceptual/teórica)
}

# Pestaña 3: RCBD balanceado — aov() vs lmer()
pestanna3_session4_v3_server <- function(input, output, session, datos_rcbd) {
  # -------------------------------------------------------------------
  # 1) Simulador RCBD balanceado (sin celdas perdidas)
  # -------------------------------------------------------------------
  sim_rcbd_bal <- function(n_trt, n_blk, mu, eff_lin, sd_blk, sd_res) {
    stopifnot(n_trt >= 2, n_blk >= 2)
    
    # Niveles de tratamiento y bloque
    trt_levels <- factor(
      paste0("T", seq_len(n_trt)),
      levels = paste0("T", seq_len(n_trt))
    )
    blk_levels <- factor(
      paste0("B", seq_len(n_blk)),
      levels = paste0("B", seq_len(n_blk))
    )
    
    # Grid completo tratamiento × bloque (RCBD balanceado)
    grid <- tidyr::expand_grid(
      tratamiento = trt_levels,
      bloque      = blk_levels
    )
    
    # Efecto lineal de tratamiento (T1 = 0, T2 = eff_lin, T3 = 2*eff_lin, ...)
    trt_idx <- as.integer(grid$tratamiento)
    eff_trt <- (trt_idx - 1L) * eff_lin
    
    # Efectos aleatorios de bloque ~ N(0, sd_blk^2)
    # Fijamos semilla para reproducibilidad del ejemplo
    set.seed(123)
    b_eff  <- stats::rnorm(n_blk, mean = 0, sd = sd_blk)
    blk_map <- stats::setNames(b_eff, levels(blk_levels))
    
    # Error residual ~ N(0, sd_res^2)
    e <- stats::rnorm(nrow(grid), mean = 0, sd = sd_res)
    
    rend <- mu + eff_trt + blk_map[as.character(grid$bloque)] + e
    
    dplyr::mutate(grid, rend = as.numeric(rend))
  }
  
  # Dataset simulado (RCBD balanceado)
  datos_bal <- eventReactive(input$run_sim_bal, {
    sim_rcbd_bal(
      n_trt   = input$n_trt,
      n_blk   = input$n_blk,
      mu      = input$mu_base,
      eff_lin = input$efecto_lineal,
      sd_blk  = input$sd_blk,
      sd_res  = input$sd_res
    )
  })
  
  # -------------------------------------------------------------------
  # 2) Resumen del diseño: conteos tratamiento × bloque + muestra de datos
  # -------------------------------------------------------------------
  output$s4_tab_design_counts <- DT::renderDataTable({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    tab_counts <- d |>
      dplyr::count(tratamiento, bloque, name = "n_parcelas") |>
      tidyr::pivot_wider(
        names_from  = bloque,
        values_from = n_parcelas
      )
    
    DT::datatable(
      tab_counts,
      rownames = FALSE,
      options = list(pageLength = min(10, nrow(tab_counts)))
    )
  })
  
  output$s4_msg_balance <- renderPrint({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    tab_counts <- d |>
      dplyr::count(tratamiento, bloque, name = "n_parcelas")
    
    # En RCBD balanceado, todas las celdas deben tener el mismo n (usualmente 1)
    n_vals <- unique(tab_counts$n_parcelas)
    if (length(n_vals) == 1L) {
      cat(sprintf(
        "Diseño completamente balanceado:\n- N tratamientos = %d\n- N bloques = %d\n- N parcelas por celda (tratamiento × bloque) = %d\n- Total de observaciones = %d\n",
        dplyr::n_distinct(d$tratamiento),
        dplyr::n_distinct(d$bloque),
        n_vals,
        nrow(d)
      ))
    } else {
      cat(
        "Advertencia: los conteos tratamiento × bloque no son todos iguales.\n",
        "Este simulador debería producir un RCBD balanceado; revise parámetros.",
        sep = ""
      )
    }
  })
  
  output$s4_tab_data_head <- DT::renderDataTable({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    DT::datatable(
      head(d, 8),
      rownames = FALSE,
      options = list(dom = "tp", pageLength = 8)
    )
  })
  
  # -------------------------------------------------------------------
  # 3) Ajuste de modelos: m1 (aov con bloque), m2 (aov sin bloque), m3 (lmer)
  # -------------------------------------------------------------------
  mod_aov_blk <- reactive({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    stats::aov(rend ~ tratamiento + bloque, data = d)
  })
  
  mod_aov_noblk <- reactive({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    stats::aov(rend ~ tratamiento, data = d)
  })
  
  mod_lmer <- reactive({
    d <- datos_bal(); shiny::req(nrow(d) > 0)
    lmerTest::lmer(
      rend ~ tratamiento + (1 | bloque),
      data = d,
      REML = TRUE
    )
  })
  
  # Tabla resumen de p-valores del efecto "tratamiento" en cada modelo
  output$s4_tab_modelos_resumen <- DT::renderDataTable({
    m1 <- mod_aov_blk()
    m2 <- mod_aov_noblk()
    m3 <- mod_lmer()
    
    # Para m1 y m2, usamos broom::tidy sobre el objeto aov
    aov1_tbl <- broom::tidy(m1)
    aov2_tbl <- broom::tidy(m2)
    
    p1_vec <- aov1_tbl |>
      dplyr::filter(term == "tratamiento") |>
      dplyr::pull(p.value)
    p1 <- p1_vec[1]
    
    p2_vec <- aov2_tbl |>
      dplyr::filter(term == "tratamiento") |>
      dplyr::pull(p.value)
    p2 <- p2_vec[1]
    
    # Para m3 (LMM), usamos ANOVA tipo III (lmerTest)
    an_lmer <- suppressMessages(
      anova(m3, type = 3, ddf = "Satterthwaite")
    )
    # as.data.frame() para manipular
    an_lmer_df <- as.data.frame(an_lmer)
    
    # Buscamos fila de "tratamiento"
    if ("tratamiento" %in% rownames(an_lmer_df)) {
      p3 <- an_lmer_df["tratamiento", "Pr(>F)"]
    } else {
      p3 <- NA_real_
    }
    
    res <- tibble::tibble(
      Modelo   = c("m1: aov(rend ~ tratamiento + bloque)",
                   "m2: aov(rend ~ tratamiento)",
                   "m3: lmer(rend ~ tratamiento + (1|bloque))"),
      Bloque   = c("Incluido (fijo)", "Ignorado", "Incluido (aleatorio)"),
      Método   = c("ANOVA clásico (GLM)",
                   "ANOVA clásico (GLM)",
                   "Modelo lineal mixto (REML)"),
      `p(tratamiento)` = c(p1, p2, p3)
    )
    
    # Resaltar el modelo "recomendado" en este contexto (m3)
    res$`← Recomendación` <- c("", "", "★")
    
    DT::datatable(
      res,
      rownames = FALSE,
      options = list(pageLength = 3, dom = "t")
    )
  })
  
  # -------------------------------------------------------------------
  # 4) Salidas completas de cada modelo (para quien quiera ver el detalle)
  # -------------------------------------------------------------------
  output$s4_out_aov_blk <- renderPrint({
    m1 <- mod_aov_blk(); shiny::req(m1)
    summary(m1)
  })
  
  output$s4_out_aov_noblk <- renderPrint({
    m2 <- mod_aov_noblk(); shiny::req(m2)
    summary(m2)
  })
  
  output$s4_out_lmer <- renderPrint({
    m3 <- mod_lmer(); shiny::req(m3)
    summary(m3)
  })
  
  # -------------------------------------------------------------------
  # 5) Varianza de bloque e ICC (LMM)
  # -------------------------------------------------------------------
  output$s4_out_varcorr <- renderPrint({
    m3 <- mod_lmer(); shiny::req(m3)
    print(lme4::VarCorr(m3), comp = c("Variance", "Std.Dev."))
  })
  
  output$s4_out_icc <- renderPrint({
    m3 <- mod_lmer(); shiny::req(m3)
    vc <- as.data.frame(lme4::VarCorr(m3))
    
    sig_b <- vc$vcov[vc$grp == "bloque"   & vc$var1 == "(Intercept)"]
    sig_e <- vc$vcov[vc$grp == "Residual"]
    
    if (length(sig_b) == 0 || length(sig_e) == 0) {
      cat("No se pudo extraer σ²_bloque y/o σ²_residual desde VarCorr.\n")
      return(invisible(NULL))
    }
    
    ICC <- sig_b / (sig_b + sig_e)
    
    cat(sprintf(
      "ICC = %.3f → %.1f%% de la varianza total atribuible a diferencias entre bloques.\n",
      ICC, 100 * ICC
    ))
    
    msg <- if (!is.finite(ICC)) {
      "ICC no finito (verificar componentes de varianza)."
    } else if (ICC < 0.05) {
      "Interpretación: la variación entre bloques es muy pequeña; el bloqueo apenas contribuye a explicar la variabilidad."
    } else if (ICC < 0.20) {
      "Interpretación: la variación entre bloques es moderada; el bloqueo ayuda, pero el componente residual sigue dominando."
    } else {
      "Interpretación: la variación entre bloques es grande; ignorar el bloque puede llevar a inferencias demasiado optimistas sobre tratamientos."
    }
    
    cat(msg, "\n")
  })
}

# Pestaña 4: RCBD desbalanceado – server
pestanna4_session4_v3_server <- function(input, output, session, datos_rcbd) {

  # -------------------------------------------------------------------
  # 1) Simulación de RCBD desbalanceado (local a la pestaña)
  # -------------------------------------------------------------------
  sim_rcbd_unbal <- function(n_trt, n_blk, mu, eff_lin, sd_blk, sd_res, perc_missing) {
    stopifnot(n_trt >= 2, n_blk >= 2)

    trt_levels <- factor(
      paste0("T", seq_len(n_trt)),
      levels = paste0("T", seq_len(n_trt))
    )
    blk_levels <- factor(
      paste0("B", seq_len(n_blk)),
      levels = paste0("B", seq_len(n_blk))
    )

    # Grid completo tratamiento × bloque
    grid <- tidyr::expand_grid(
      tratamiento = trt_levels,
      bloque      = blk_levels
    )

    # Efecto lineal de tratamiento (T1, T2, T3, ... con incremento eff_lin)
    trt_idx  <- as.integer(grid$tratamiento)
    eff_trt  <- (trt_idx - 1) * eff_lin

    # Efecto aleatorio de bloque ~ N(0, sd_blk^2)
    set.seed(123)
    b_eff   <- rnorm(n_blk, mean = 0, sd = sd_blk)
    blk_map <- stats::setNames(b_eff, levels(blk_levels))

    # Error residual
    e <- rnorm(nrow(grid), mean = 0, sd = sd_res)

    rend <- mu + eff_trt + blk_map[as.character(grid$bloque)] + e

    d <- dplyr::mutate(grid, rend = as.numeric(rend))

    # Introducir desbalance: eliminar aleatoriamente un % de celdas
    perc_missing <- max(0, min(100, perc_missing))
    if (perc_missing > 0) {
      n_drop <- floor(nrow(d) * perc_missing / 100)
      if (n_drop > 0 && n_drop < nrow(d)) {
        drop_idx <- sample(seq_len(nrow(d)), size = n_drop, replace = FALSE)
        d <- d[-drop_idx, , drop = FALSE]
      }
    }
    d
  }

  # Reactive que contiene los datos simulados desbalanceados
  s4_datos_unbal <- eventReactive(input$s4_run_sim_unbal, {
    sim_rcbd_unbal(
      n_trt       = input$s4_n_trt_unbal,
      n_blk       = input$s4_n_blk_unbal,
      mu          = input$s4_mu_base_unbal,
      eff_lin     = input$s4_efecto_lineal_unbal,
      sd_blk      = input$s4_sd_blk_unbal,
      sd_res      = input$s4_sd_res_unbal,
      perc_missing = input$s4_perc_missing_unbal
    )
  }, ignoreInit = TRUE)

  # -------------------------------------------------------------------
  # 2) Resumen de datos y desbalance
  # -------------------------------------------------------------------
  output$s4_tab_datos_unbal <- DT::renderDataTable({
    d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
    DT::datatable(
      head(d, 10),
      options = list(pageLength = 10, dom = "tip"),
      rownames = FALSE
    )
  })

  output$s4_tab_counts_unbal <- DT::renderDataTable({
    d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
    counts <- d |>
      dplyr::count(tratamiento, bloque, name = "n_celdas") |>
      tidyr::pivot_wider(
        names_from  = bloque,
        values_from = n_celdas,
        values_fill = 0
      )
    DT::datatable(
      counts,
      options = list(pageLength = 10, dom = "tip"),
      rownames = FALSE
    )
  })

  # -------------------------------------------------------------------
  # 3) Modelos aov() con y sin bloque
  # -------------------------------------------------------------------
  ajuste_aov_trt_blk <- reactive({
    d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
    stats::aov(rend ~ tratamiento + bloque, data = d)
  })

  ajuste_aov_trt_only <- reactive({
    d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
    stats::aov(rend ~ tratamiento, data = d)
  })

  output$s4_out_aov_trt_blk <- renderPrint({
    m1 <- ajuste_aov_trt_blk(); shiny::req(m1)
    summary(m1)
  })

  output$s4_out_aov_trt_only <- renderPrint({
    m2 <- ajuste_aov_trt_only(); shiny::req(m2)
    summary(m2)
  })

  # Mensajes con p-valor(tratamiento) de cada modelo
  output$s4_msg_aov_trt_blk <- renderText({
    m1 <- ajuste_aov_trt_blk(); shiny::req(m1)
    an <- summary(m1)[[1]]
    p_trat <- an["tratamiento", "Pr(>F)"]
    if (is.na(p_trat)) return("No se pudo extraer p-valor de tratamiento en este ajuste.")

    paste0(
      "En el modelo con bloque como fijo, p(tratamiento) ≈ ",
      formatC(p_trat, format = "f", digits = 4),
      ". Este valor es sensible al desbalance pero no al orden de los términos ",
      "en la fórmula (aquí sólo hay un orden posible: tratamiento + bloque)."
    )
  })

  output$s4_msg_aov_trt_only <- renderText({
    m2 <- ajuste_aov_trt_only(); shiny::req(m2)
    an <- summary(m2)[[1]]
    p_trat <- an["tratamiento", "Pr(>F)"]
    if (is.na(p_trat)) return("No se pudo extraer p-valor de tratamiento en este ajuste.")

    paste0(
      "En el modelo que ignora el bloque, p(tratamiento) ≈ ",
      formatC(p_trat, format = "f", digits = 4),
      ". Si la varianza entre bloques es grande (ICC alto), este modelo ",
      "tiende a subestimar la incertidumbre (riesgo de F inflado y falsos positivos)."
    )
  })

  # -------------------------------------------------------------------
  # 4) Modelo mixto: lmer(rend ~ tratamiento + (1|bloque))
  # -------------------------------------------------------------------
  ajuste_lmer_unbal <- reactive({
    d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
    # lmerTest agrega pruebas para efectos fijos con Satterthwaite
    lmerTest::lmer(rend ~ tratamiento + (1 | bloque), data = d, REML = TRUE)
  })

  output$s4_out_lmer_anova_unbal <- renderPrint({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    # ANOVA Type III con ddf de Satterthwaite
    anova(m3, type = 3, ddf = "Satterthwaite")
  })

  output$s4_out_varcorr_unbal <- renderPrint({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    print(lme4::VarCorr(m3), comp = c("Variance", "Std.Dev."))
  })

  output$s4_out_icc_unbal <- renderPrint({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    vc <- as.data.frame(lme4::VarCorr(m3))
    sig_b <- vc$vcov[vc$grp == "bloque"   & vc$var1 == "(Intercept)"]
    sig_e <- vc$vcov[vc$grp == "Residual"]

    if (!is.finite(sig_b) || !is.finite(sig_e)) {
      cat("No fue posible calcular el ICC (componentes de varianza no finitos).\n")
    } else {
      ICC <- sig_b / (sig_b + sig_e)
      cat(sprintf(
        "ICC ≈ %.3f (≈ %.1f%% de la varianza total atribuible a diferencias entre bloques).\n",
        ICC, 100 * ICC
      ))
      cat(
        "Lectura: ICC cercano a 0 implica bloques poco relevantes; ICC moderado/alto indica ",
        "fuerte correlación intra-bloque.\n",
        "Ignorar el bloque en este contexto puede inflar la evidencia a favor de diferencias ",
        "entre tratamientos.\n"
      )
    }
  })

  # -------------------------------------------------------------------
  # 5) Diagnósticos y BLUPs
  # -------------------------------------------------------------------
  output$s4_plot_resid_fitted_unbal <- renderPlot({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    df_plot <- data.frame(
      fitted = stats::fitted(m3),
      resid  = stats::resid(m3)
    )
    ggplot2::ggplot(df_plot, ggplot2::aes(x = fitted, y = resid)) +
      ggplot2::geom_point(alpha = 0.7) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      ggplot2::labs(
        x = "Valores ajustados",
        y = "Residuos",
        title = "Residuos vs ajustados (LMM desbalanceado)"
      ) +
      ggplot2::theme_bw()
  })

  output$s4_plot_qq_unbal <- renderPlot({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    df <- data.frame(resid = stats::resid(m3))
    ggplot2::ggplot(df, ggplot2::aes(sample = resid)) +
      ggplot2::stat_qq() +
      ggplot2::stat_qq_line(color = "red") +
      ggplot2::labs(
        title = "Q-Q Normal de residuos (LMM)",
        x = "Cuantiles teóricos",
        y = "Cuantiles muestrales"
      ) +
      ggplot2::theme_bw()
  })

  output$s4_plot_ranef_unbal <- renderPlot({
    m3 <- ajuste_lmer_unbal(); shiny::req(m3)
    re <- lme4::ranef(m3, condVar = TRUE)
    rb <- as.data.frame(re$bloque)
    rb$bloque <- rownames(rb)
    colnames(rb)[1] <- "ranef"
    ggplot2::ggplot(rb, ggplot2::aes(x = reorder(bloque, ranef), y = ranef)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_point(size = 3) +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Bloque",
        y = "Intercepto aleatorio (BLUP)",
        title = "BLUPs por bloque (LMM)"
      ) +
      ggplot2::theme_bw()
  })

  # -------------------------------------------------------------------
  # 6) Descargar datos simulados
  # -------------------------------------------------------------------
  output$s4_dl_csv_unbal <- downloadHandler(
    filename = function() {
      paste0(
        "datos_rcbd_desbalanceado_nT", input$s4_n_trt_unbal,
        "_nB", input$s4_n_blk_unbal,
        "_miss", input$s4_perc_missing_unbal,
        "_", Sys.Date(), ".csv"
      )
    },
    content = function(file) {
      d <- s4_datos_unbal(); shiny::req(nrow(d) > 0)
      utils::write.csv(d, file, row.names = FALSE)
    }
  )
}

# Pestaña 5: Guía paso a paso para análisis de LMM en RCBD
pestanna5_session4_v3_server <- function(input, output, session, datos_rcbd) {
  
  # Paso 1: tabla tratamiento × bloque
  output$p5_tab_count <- DT::renderDataTable({
    d <- datos_rcbd()
    shiny::req(d, nrow(d) > 0)
    
    # Asegurarnos de que existan las columnas esperadas
    if (!all(c("tratamiento", "bloque") %in% names(d))) {
      return(DT::datatable(
        data.frame(
          mensaje = "El objeto de datos actual no tiene columnas 'tratamiento' y 'bloque'.",
          check.names = FALSE
        ),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }
    
    tab <- d |>
      dplyr::count(tratamiento, bloque, name = "n") |>
      tidyr::pivot_wider(
        names_from  = bloque,
        values_from = n,
        values_fill = 0
      )
    
    DT::datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        dom        = "tip",
        scrollX    = TRUE
      )
    )
  })
  
  # No se requiere más lógica: el resto de la pestaña es contenido estático / guía
}

# Pestaña 6: Referencias
pestanna6_session4_v3_server <- function(input, output, session) {
  # Pestaña completamente teórica; no necesita lógica de servidor.
}

# Pestaña Extra: Recursos Visuales
pestanna_extra_session4_v3_server <- function(input, output, session) {
  # Esta pestaña contiene solo imágenes y texto estático explicativo.
  # No se requiere lógica de servidor activa, pero mantenemos la función
  # para consistencia con la estructura modular.
  return(NULL)
}

# Pestaña 7: Referencias
pestanna7_session4_v3_server <- function(input, output, session) {
  # Pestaña únicamente informativa; no requiere lógica de servidor.
}

# -------------------------------------------------------------------------
# Main Server
# -------------------------------------------------------------------------

session4_v3Server <- function(input, output, session) {
  ns <- session$ns

  # Helpers (sim_rcbd, etc.) y reactivos (datos_rcbd) se quedan igual
  sim_rcbd <- function(n_trt, n_blk, mu, eff_lin, sd_blk, sd_res, perc_missing = 0) {
    stopifnot(n_trt >= 2, n_blk >= 2)
    trt_levels <- factor(paste0("T", seq_len(n_trt)), levels = paste0("T", seq_len(n_trt)))
    blk_levels <- factor(paste0("B", seq_len(n_blk)), levels = paste0("B", seq_len(n_blk)))

    grid <- tidyr::expand_grid(tratamiento = trt_levels, bloque = blk_levels)
    trt_idx <- as.integer(grid$tratamiento)
    eff_trt <- (trt_idx - 1) * eff_lin

    set.seed(123)
    b_eff <- rnorm(n_blk, mean = 0, sd = sd_blk)
    blk_map <- setNames(b_eff, levels(blk_levels))
    e <- rnorm(nrow(grid), mean = 0, sd = sd_res)

    rend <- mu + eff_trt + blk_map[as.character(grid$bloque)] + e
    d <- dplyr::mutate(grid, rend = as.numeric(rend))

    if (perc_missing > 0) {
      n_drop <- floor(nrow(d) * perc_missing / 100)
      if (n_drop > 0 && n_drop < nrow(d)) {
        drop_idx <- sample(seq_len(nrow(d)), size = n_drop, replace = FALSE)
        d <- d[-drop_idx, , drop = FALSE]
      }
    }
    d
  }

  # Reactives
  datos_rcbd <- eventReactive(input$run_sim, {
    sim_rcbd(
      n_trt       = input$n_trt,
      n_blk       = input$n_blk,
      mu          = input$mu_base,
      eff_lin     = input$efecto_lineal,
      sd_blk      = input$sd_blk,
      sd_res      = input$sd_res,
      perc_missing = input$perc_missing
    )
  })

  # Call tab servers
  pestanna1_session4_v3_server(input, output, session)
  pestanna2_session4_v3_server(input, output, session)
  pestanna3_session4_v3_server(input, output, session, datos_rcbd)
  pestanna4_session4_v3_server(input, output, session, datos_rcbd)
  pestanna5_session4_v3_server(input, output, session, datos_rcbd)
  pestanna6_session4_v3_server(input, output, session)
  pestanna_extra_session4_v3_server(input, output, session)
  pestanna7_session4_v3_server(input, output, session)
}
