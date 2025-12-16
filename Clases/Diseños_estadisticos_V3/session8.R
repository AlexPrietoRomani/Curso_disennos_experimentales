############################################################
# Sesión 8 - Diseños de Bloques Aumentados (DBA)
# Script pedagógico de acompañamiento a la app Shiny
#
# Objetivos didácticos de la sesión:
#   1) Construir un diseño de bloques aumentados (DBA)
#      - Muchos genotipos nuevos con 1 sola observación
#      - Pocos testigos (checks) replicados en todos los bloques
#   2) Simular una variable respuesta Y con:
#        - efecto de genotipo (σ_G: variación genética verdadera)
#        - efecto de bloque (σ_B: gradiente ambiental entre bloques)
#        - error residual (σ_e: variación dentro de bloque)
#   3) Explorar el diseño y la simulación con gráficos:
#        - distribución global
#        - efecto de bloques
#        - checks vs nuevos
#        - “mapa” del ensayo
#   4) Comparar dos enfoques de análisis:
#        a) Análisis clásico tipo Federer (augmentedRCBD)
#           - ANOVA ajustado
#           - medias ajustadas por tratamiento
#        b) Modelo mixto (LMM) con BLUPs de genotipos
#           - componentes de varianza (block, trt, residual)
#           - ranking de genotipos (checks y nuevos)
############################################################

# ----------------------------------------------------------
# 0) Paquetes y configuración inicial
# ----------------------------------------------------------
# Aquí verificamos que los paquetes “base” estén instalados.
# Si falta alguno, detenemos el script con un mensaje claro,
# porque sin ellos no se puede correr nada de la sesión.

pkgs_base <- c("dplyr", "ggplot2", "lme4", "lmerTest")
pkgs_missing <- pkgs_base[!sapply(pkgs_base, requireNamespace, quietly = TRUE)]

if (length(pkgs_missing) > 0) {
  stop(
    "Faltan paquetes base para este script. Instala primero:\n  ",
    paste(pkgs_missing, collapse = ", ")
  )
}

# Cargamos paquetes principales (manipulación, gráficos, LMM)
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# Paquetes específicos para DBA:
#   - agricolae: genera el diseño DBA clásico (design.dau)
#   - augmentedRCBD: análisis clásico tipo Federer
has_agricolae <- requireNamespace("agricolae", quietly = TRUE)
has_augRCBD   <- requireNamespace("augmentedRCBD", quietly = TRUE)

if (!has_agricolae) {
  message(
    "Aviso: 'agricolae' NO está instalado. ",
    "Se usará un generador simple de DBA como respaldo.\n",
    "Recomendado: install.packages('agricolae') para reproducir la sesión exactamente."
  )
}

if (!has_augRCBD) {
  message(
    "Aviso: 'augmentedRCBD' NO está instalado.\n",
    "El análisis clásico tipo Federer se ilustrará con un ANOVA aproximado (lm), ",
    "pero no será el flujo completo de augmentedRCBD.\n",
    "Recomendado: install.packages('augmentedRCBD')."
  )
}

set.seed(123)  # para reproducibilidad de toda la simulación

# ----------------------------------------------------------
# 1) Contexto didáctico del DBA (versión en consola)
# ----------------------------------------------------------
# Esta sección imprime en consola el contexto agronómico donde
# típicamente se usan los DBA: etapas tempranas de breeding.

cat("====================================================\n")
cat("SESIÓN 8 – DISEÑOS DE BLOQUES AUMENTADOS (DBA)\n")
cat("====================================================\n\n")

cat("Contexto típico:\n")
cat("- Etapas tempranas de mejoramiento: muchas líneas nuevas, poca semilla.\n")
cat("- Solo algunos genotipos (testigos o checks) se pueden replicar en todos los bloques.\n")
cat("- Los nuevos (entries) aparecen una sola vez.\n")
cat("- El diseño de bloques aumentados combina:\n")
cat("    * Pocos testigos bien replicados (para estimar error y bloque)\n")
cat("    * Muchos nuevos no replicados, ajustados usando la info de los testigos.\n\n")

# ----------------------------------------------------------
# 2) Generar un diseño de Bloques Aumentados (DBA)
# ----------------------------------------------------------
# En esta parte construimos el “fieldbook” (libro de campo) del DBA:
#   - columna block: bloque donde está la parcela
#   - columna trt: tratamiento (genotipo: check o nuevo)
#   - columna tipo: etiqueta pedagógica (Check / New)
#   - columna plots: identificador de parcela (cuando usamos el respaldo)

# Parámetros pedagógicos del ensayo
checks        <- c("CheckA", "CheckB", "CheckC")  # testigos
n_new         <- 60                               # nº de líneas nuevas
n_blocks      <- 6                                # nº de bloques
new_trt_names <- paste0("N", sprintf("%02d", 1:n_new))

cat("----------------------------------------------------\n")
cat("PASO 1: Construcción del diseño DBA\n")
cat("----------------------------------------------------\n\n")

cat("Configuración del ensayo:\n")
cat("  - Nº de testigos (checks): ", length(checks), " -> ",
    paste(checks, collapse = ", "), "\n", sep = "")
cat("  - Nº de nuevos (entries): ", n_new, "\n", sep = "")
cat("  - Nº de bloques (b):      ", n_blocks, "\n\n", sep = "")

if (has_agricolae) {
  # Caso ideal (recomendado): usamos design.dau de agricolae.
  # Esto construye un DBA “clásico” donde:
  #   - los checks están replicados en todos los bloques
  #   - los nuevos se distribuyen entre bloques sin replicación
  dba_design <- agricolae::design.dau(
    trt1  = checks,
    trt2  = new_trt_names,
    r     = n_blocks,   # número de bloques
    serie = 2,
    seed  = 123
  )
  
  fieldbook <- dba_design$book
  
} else {
  # Respaldo simple: construimos un DBA “a mano”.
  # Didácticamente sirve para entender la estructura, pero
  # no sigue exactamente el mismo algoritmo que design.dau.
  
  cat("Usando generador simple de respaldo (sin agricolae)...\n")
  
  bloques <- paste0("B", seq_len(n_blocks))
  
  # Todos los checks aparecen en todos los bloques (replicados).
  fb_checks <- expand.grid(
    block = bloques,
    trt   = checks,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Repartimos los nuevos de forma más o menos equitativa
  # entre los bloques (1 réplica por nuevo).
  fb_news <- lapply(bloques, function(b) {
    data.frame(
      block = b,
      trt   = sample(new_trt_names, size = ceiling(n_new / n_blocks)),
      stringsAsFactors = FALSE
    )
  }) |> bind_rows()
  
  fieldbook <- bind_rows(fb_checks, fb_news)
  fieldbook$plots <- seq_len(nrow(fieldbook))  # id de parcela ficticia
}

# Aseguramos tipos factor para block y trt (necesario para modelos)
fieldbook <- fieldbook |>
  as.data.frame() |>
  dplyr::mutate(
    block = factor(block),
    trt   = factor(trt)
  )

# Creamos una columna "tipo" que clasifica tratamientos:
#   - "Check": testigo, replicado en varios bloques
#   - "New":   línea nueva, usualmente con 1 réplica
fieldbook <- fieldbook |>
  mutate(
    tipo = ifelse(trt %in% checks, "Check", "New")
  )

# Resumen rápido del diseño construido
cat("Resumen del fieldbook inicial:\n")
cat("  - Nº de filas (parcelas): ", nrow(fieldbook), "\n", sep = "")
cat("  - Nº de bloques:          ", n_distinct(fieldbook$block), "\n", sep = "")
cat("  - Nº de tratamientos:     ", n_distinct(fieldbook$trt), "\n\n", sep = "")

table_trt <- table(fieldbook$trt)
cat("Réplicas por tratamiento (primeros 10):\n")
print(head(table_trt, 10))
cat("\nNota: en un DBA típico, los checks aparecen varias veces,\n",
    "mientras que los nuevos deberían aparecer una sola vez.\n",
    "Eso se observa en la tabla de réplicas: los checks tienen r > 1\n",
    "y los nuevos generalmente r = 1.\n\n", sep = "")

# ----------------------------------------------------------
# 3) Simulación de la variable respuesta Y
# ----------------------------------------------------------
# Ahora definimos un “modelo verdadero” de simulación:
#   Y_ij = mu + G_i + B_j + e_ij
#     - mu: media general del ensayo
#     - G_i ~ N(0, σ_G^2): efecto de genotipo i (trt)
#     - B_j ~ N(0, σ_B^2): efecto de bloque j
#     - e_ij ~ N(0, σ_E^2): error residual
# Esto corresponde a un modelo mixto con genotipos y bloques
# como efectos aleatorios.

cat("----------------------------------------------------\n")
cat("PASO 2: Simulación de la variable respuesta Y\n")
cat("----------------------------------------------------\n\n")

# Parámetros de simulación (puedes jugar con ellos en clase)
mu   <- 100   # media general (nivel del ensayo)
sd_G <- 10    # SD genotipos (variación genética verdadera)
sd_B <- 15    # SD bloques (gradiente entre bloques)
sd_E <- 5     # SD residual (ruido dentro de bloque)

cat("Parámetros de simulación:\n")
cat("  - Media general (mu):       ", mu,  "\n", sep = "")
cat("  - SD genotipos (sigma_G):   ", sd_G, "\n", sep = "")
cat("  - SD bloques (sigma_B):     ", sd_B, "\n", sep = "")
cat("  - SD residual (sigma_e):    ", sd_E, "\n\n", sep = "")

# Niveles de genotipos y bloques
trt_levels <- levels(fieldbook$trt)
blk_levels <- levels(fieldbook$block)

# Efectos aleatorios de genotipo y bloque simulados desde N(0, σ^2)
eff_trt <- rnorm(length(trt_levels), mean = 0, sd = sd_G)
names(eff_trt) <- trt_levels

eff_blk <- rnorm(length(blk_levels), mean = 0, sd = sd_B)
names(eff_blk) <- blk_levels

# Construimos Y para cada fila (parcela) del fieldbook
fieldbook <- fieldbook |>
  mutate(
    eff_trt = eff_trt[trt],    # G_i
    eff_blk = eff_blk[block],  # B_j
    Y       = mu + eff_trt + eff_blk + rnorm(n(), mean = 0, sd = sd_E)
  )

cat("Vista de las primeras filas del fieldbook simulado:\n")
print(head(fieldbook, 10))
cat("\n")
# Interpretación de las columnas impresas:
#   - block: bloque del ensayo
#   - trt:   genotipo en esa parcela
#   - tipo:  Check / New
#   - eff_trt: efecto aleatorio verdadero del genotipo (G_i)
#   - eff_blk: efecto aleatorio verdadero del bloque (B_j)
#   - Y:        respuesta simulada (lo que veríamos en campo)

# ----------------------------------------------------------
# 4) Exploración gráfica: diseño y respuesta simulada
# ----------------------------------------------------------
# Aquí no ajustamos modelos todavía; solo miramos el patrón
# de la respuesta Y:
#   - ¿es aproximadamente normal?
#   - ¿hay bloques más altos/bajos?
#   - ¿los checks se comportan distinto a los nuevos?

cat("----------------------------------------------------\n")
cat("PASO 3: Exploración gráfica del diseño y de Y\n")
cat("----------------------------------------------------\n\n")

# 4.1 Distribución global de Y
#     Sirve para verificar la forma de la distribución
#     (asimetría, colas, outliers).
p_hist <- ggplot(fieldbook, aes(x = Y)) +
  geom_histogram(bins = 30, color = "white") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución de la respuesta simulada (Y)",
    x     = "Y",
    y     = "Frecuencia"
  )

# 4.2 Boxplot por bloque
#     Aquí buscamos diferencias sistemáticas entre bloques:
#       - Si la mediana de un bloque es muy distinta a otra,
#         hay un gradiente ambiental fuerte.
p_box_block <- ggplot(fieldbook, aes(x = block, y = Y)) +
  geom_boxplot() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Boxplot de Y por bloque (gradiente entre bloques)",
    x     = "Bloque",
    y     = "Y"
  )

# 4.3 Boxplot por tipo (Check vs New)
#     Comparamos la distribución de Y entre:
#       - Checks: genotipos conocidos, replicados
#       - New:   materiales nuevos
#     Útil para ver si, en promedio, los nuevos superan a los checks,
#     o si hay desplazamiento en la distribución.
p_box_tipo <- ggplot(fieldbook, aes(x = tipo, y = Y, fill = tipo)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal(base_size = 12) +
  guides(fill = "none") +
  labs(
    title = "Y por tipo de tratamiento (Check vs New)",
    x     = "Tipo",
    y     = "Y"
  )

# 4.4 “Mapa” por bloque (posición dentro del bloque)
#     Construimos una variable pos para ordenar los tratamientos
#     dentro de cada bloque y visualizar un “heatmap” simplificado.
fieldbook_mapa <- fieldbook |>
  group_by(block) |>
  arrange(trt, .by_group = TRUE) |>
  mutate(pos = row_number()) |>
  ungroup()

p_mapa <- ggplot(
  fieldbook_mapa,
  aes(x = block, y = pos, fill = Y, label = trt)
) +
  geom_tile(color = "white") +   # cada celda = parcela
  geom_text(size = 2.8) +        # etiqueta = nombre de genotipo
  scale_y_reverse() +            # posición 1 arriba
  scale_fill_viridis_c(name = "Y") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Ensayo DBA simulado: Y por bloque y posición",
    x     = "Bloque",
    y     = "Posición en el bloque"
  )

# Mostramos los plots en pantalla
print(p_hist)
print(p_box_block)
print(p_box_tipo)
print(p_mapa)

cat("Gráficos generados:\n")
cat("  - Histograma global de Y (forma de la distribución)\n")
cat("  - Boxplot de Y por bloque (gradiente entre bloques)\n")
cat("  - Boxplot de Y por tipo (Check vs New)\n")
cat("  - Mapa tipo heatmap del ensayo por bloque (patrón espacial por bloque)\n\n")

# ----------------------------------------------------------
# 5) Análisis clásico tipo Federer (augmentedRCBD)
# ----------------------------------------------------------
# Aquí aplicamos el enfoque clásico de Federer para DBA.
# Cuando augmentedRCBD está disponible:
#   - La función augmentedRCBD imprime en la consola:
#       * ANOVA Treatment Adjusted
#       * ANOVA Block Adjusted
#       * Coefficient of Variation (CV%)
#       * Overall Adjusted Mean
#       * Standard Errors & Critical Difference (CD)
#       * Medias ajustadas por tratamiento (Adjusted Means)
#   - Los F-value y p-value de las filas:
#       * Block (ignoring Treatments)
#           => Test de gradiente entre bloques
#       * Treatment (eliminating Blocks)
#           => Diferencias globales entre tratamientos
#       * Treatment: Check / Test / Test vs Check
#           => Sub-descomposición en checks y nuevos

cat("----------------------------------------------------\n")
cat("PASO 4: Análisis clásico tipo Federer\n")
cat("----------------------------------------------------\n\n")

if (has_augRCBD) {
  cat("Usando 'augmentedRCBD::augmentedRCBD' para el análisis clásico DBA...\n\n")
  
  # Ajuste clásico: los checks se usan para estimar el error
  # y calibrar/ajustar las medias de los nuevos.
  res_classic <- augmentedRCBD::augmentedRCBD(
    block       = fieldbook$block,
    treatment   = fieldbook$trt,
    y           = fieldbook$Y,
    checks      = checks,
    method.comp = "lsd",   # comparación de medias para checks
    alpha       = 0.05,
    group       = FALSE,
    console     = TRUE     # IMP: imprime en consola los resúmenes
  )
  
  # NOTA IMPORTANTE:
  #   Dependiendo de la versión del paquete, los elementos
  #   res_classic$ANOVA y res_classic$treatmentMeans pueden no
  #   venir guardados y ser impresos solo en la consola.
  #   En la salida que muestras, ambos aparecen como NULL; esto
  #   significa que la información relevante está en el bloque
  #   de texto que augmentedRCBD ya imprimió arriba.
  
  cat("\n--- Tabla ANOVA (augmentedRCBD) ---\n")
  print(res_classic$ANOVA)
  
  cat("\n--- Medias ajustadas por tratamiento (primeras filas) ---\n")
  print(head(res_classic$treatmentMeans))
  
} else {
  # Si no tenemos el paquete, mostramos un ANOVA aproximado:
  #   Y ~ block + trt
  # que NO reproduce exactamente la metodología de Federer,
  # pero sirve para entender la comparación global.
  
  cat("El paquete 'augmentedRCBD' no está disponible.\n")
  cat("Se muestra un ANOVA aproximado usando lm(Y ~ block + trt),\n")
  cat("solo como referencia pedagógica (NO es el análisis exacto DBA clásico).\n\n")
  
  mod_lm <- lm(Y ~ block + trt, data = fieldbook)
  print(anova(mod_lm))
}

cat("\nComentario didáctico (augmentedRCBD):\n")
cat("- La fila 'Block (ignoring Treatments)' en el ANOVA Treatment Adjusted\n")
cat("  evalúa si hay diferencias significativas entre bloques (gradiente ambiental).\n")
cat("- La fila 'Treatment (eliminating Blocks)' evalúa si, una vez ajustado por bloque,\n")
cat("  hay diferencias globales entre tratamientos.\n")
cat("- La descomposición 'Treatment: Check', 'Treatment: Test', 'Treatment: Test vs. Check'\n")
cat("  ayuda a separar la variabilidad de los testigos, de los nuevos, y del contraste\n")
cat("  global entre nuevos y checks.\n")
cat("- El Coefficient of Variation (CV%) mide la variabilidad relativa del ensayo;\n")
cat("  valores más bajos indican mayor precisión experimental.\n")
cat("- Las 'Adjusted Means' son las medias de genotipos ajustadas por bloque y por el\n")
cat("  patrón de checks: son la base para seleccionar materiales en etapas tempranas.\n\n")

# ----------------------------------------------------------
# 6) Modelo mixto (LMM) con bloques y genotipos aleatorios
#    y obtención de BLUPs
# ----------------------------------------------------------
# Ahora cambiamos al enfoque de modelos mixtos:
#   Y ~ 1 + (1 | block) + (1 | trt)
#
# Interpretación:
#   - (Intercept): media general estimada (mu_hat)
#   - Random effect block: captura σ²_B (variación entre bloques)
#   - Random effect trt:   captura σ²_G (variación genética)
#   - Residual:            captura σ²_E (ruido dentro de bloque)
#
# De este ajuste extraemos:
#   - Componentes de varianza (VarCorr)
#   - BLUPs de genotipos:
#       * Positivos: genotipos por encima de la media ajustada
#       * Negativos: genotipos por debajo de la media ajustada

cat("----------------------------------------------------\n")
cat("PASO 5: Modelo mixto (LMM) y BLUPs de genotipos\n")
cat("----------------------------------------------------\n\n")

cat("Ajustando el modelo mixto:\n")
cat("  Y ~ 1 + (1 | block) + (1 | trt)\n\n")

mod_lmm <- lmer(Y ~ 1 + (1 | block) + (1 | trt), data = fieldbook)

cat("Resumen del modelo mixto:\n")
print(summary(mod_lmm))
cat("\n")
# Cómo leer esta salida:
#   - “Random effects”:
#       * trt (Intercept): varianza genética entre genotipos (σ²_G_hat)
#       * block (Intercept): varianza ambiental entre bloques (σ²_B_hat)
#       * Residual: varianza residual (σ²_E_hat)
#   - “Fixed effects”:
#       * (Intercept): media general estimada (mu_hat).
#         El t-value y Pr(>|t|) solo indican si mu_hat es distinto de 0,
#         no es lo más interesante agronómicamente, pero es parte del resumen.

cat("--- Componentes de varianza (VarCorr) ---\n")
vc <- VarCorr(mod_lmm)
print(vc)

vc_df <- as.data.frame(vc)[, c("grp", "vcov")]
names(vc_df) <- c("Componente", "Varianza")
cat("\nResumen de varianzas estimadas:\n")
print(vc_df)

cat("\nInterpretación rápida de las varianzas:\n")
cat("- 'block': σ²_B_hat (variación ambiental entre bloques).\n")
cat("- 'trt':   σ²_G_hat (variación genética entre genotipos).\n")
cat("- 'Residual': σ²_E_hat (variación dentro de bloque, no explicada).\n")
cat("  Proporciones relativas ayudan a entender si el ensayo está\n")
cat("  dominado por ruido ambiental, variación genética o residual.\n\n")

# 6.1 BLUPs de genotipos
#     ranef(mod_lmm)$trt devuelve los efectos aleatorios de cada genotipo.
#     Estos son los BLUPs (Best Linear Unbiased Predictions).
#     La media general ajustada de cada genotipo sería:
#       mu_hat + BLUP_trt

ran_trt <- ranef(mod_lmm)$trt
ran_trt <- tibble::rownames_to_column(as.data.frame(ran_trt), var = "trt")
names(ran_trt)[2] <- "BLUP"  # columna de efecto aleatorio

# Añadimos tipo de tratamiento para distinguir checks y nuevos
ran_trt <- ran_trt |>
  mutate(
    tipo = ifelse(trt %in% checks, "Check", "New")
  )

cat("BLUPs de genotipos (primeras 10 filas):\n")
print(head(ran_trt, 10))
cat("\n")
# Interpretación:
#   - Un BLUP positivo indica un genotipo que, en promedio,
#     está por encima de la media general ajustada.
#   - Un BLUP negativo indica un genotipo por debajo de la media.
#   - Los BLUPs de checks y nuevos se comparan en la misma escala,
#     pero los nuevos tienden a “encogerse” (shrinkage) hacia 0
#     cuando hay poca información (solo 1 réplica).

# 6.2 Gráfico de los Top 20 genotipos según BLUP
#     Mostramos un ranking rápido de los mejores 20 materiales
#     considerando BLUP (efecto genético ajustado).
top_n <- 20
top_blups <- ran_trt |>
  arrange(desc(BLUP)) |>
  slice(1:top_n)

p_blups <- ggplot(
  top_blups,
  aes(x = reorder(trt, BLUP), y = BLUP, fill = tipo)
) +
  geom_col() +
  coord_flip() +
  theme_minimal(base_size = 12) +
  labs(
    title = paste0("Top ", top_n, " genotipos según BLUP (modelo mixto DBA)"),
    x     = "Genotipo",
    y     = "BLUP (efecto aleatorio estimado)"
  ) +
  scale_fill_manual(
    values = c("Check" = "#ff7f0e", "New" = "#1f77b4"),
    name   = "Tipo"
  )

print(p_blups)

cat("\nComentario didáctico sobre BLUPs:\n")
cat("- Los BLUPs reflejan el desempeño esperado de cada genotipo,\n")
cat("  habiendo corregido por el efecto de bloque y el error residual.\n")
cat("- En etapas tempranas de mejoramiento, donde los nuevos tienen\n")
cat("  una sola observación, el uso de BLUPs es preferible a trabajar\n")
cat("  con medias crudas por bloque.\n")
cat("- El “shrinkage” (encogimiento) hace que genotipos con poca\n")
cat("  información no aparezcan artificialmente extremos; sus BLUPs\n")
cat("  se acercan más a 0 (la media), mientras que genotipos con más\n")
cat("  información (por ejemplo, checks) pueden sostener efectos más\n")
cat("  alejados de la media.\n")
cat("- En la práctica, el ranking por BLUPs es una herramienta clave\n")
cat("  para decidir qué genotipos avanzar a la siguiente etapa del\n")
cat("  programa de mejoramiento.\n\n")
cat("====================================================\n")