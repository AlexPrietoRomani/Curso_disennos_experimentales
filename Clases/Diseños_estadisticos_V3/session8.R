############################################################
# Sesión 8 - Diseños de Bloques Aumentados (DBA)
# Script pedagógico de acompañamiento a la app Shiny
#
# Objetivo:
#   1) Construir un diseño de bloques aumentados (DBA)
#   2) Simular una variable respuesta Y con:
#        - efecto de genotipo (σ_G)
#        - efecto de bloque (σ_B)
#        - error residual (σ_e)
#   3) Explorar el diseño y la simulación con gráficos
#   4) Comparar:
#        a) Análisis clásico tipo Federer (augmentedRCBD)
#        b) Modelo mixto (LMM) con BLUPs para genotipos
############################################################

# ----------------------------------------------------------
# 0) Paquetes y configuración inicial
# ----------------------------------------------------------

pkgs_base <- c("dplyr", "ggplot2", "lme4", "lmerTest")
pkgs_missing <- pkgs_base[!sapply(pkgs_base, requireNamespace, quietly = TRUE)]

if (length(pkgs_missing) > 0) {
  stop(
    "Faltan paquetes base para este script. Instala primero:\n  ",
    paste(pkgs_missing, collapse = ", ")
  )
}

# Cargamos paquetes principales
library(dplyr)
library(ggplot2)
library(lme4)
library(lmerTest)

# Paquetes específicos (si están instalados)
has_agricolae    <- requireNamespace("agricolae", quietly = TRUE)
has_augRCBD      <- requireNamespace("augmentedRCBD", quietly = TRUE)

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

set.seed(123)  # para reproducibilidad

# ----------------------------------------------------------
# 1) Contexto didáctico del DBA (versión en consola)
# ----------------------------------------------------------

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
  # Usamos design.dau de agricolae (diseño clásico DBA)
  dba_design <- agricolae::design.dau(
    trt1  = checks,
    trt2  = new_trt_names,
    r     = n_blocks,   # número de bloques
    serie = 2,
    seed  = 123
  )
  
  fieldbook <- dba_design$book
  
} else {
  # Respaldo simple: construimos un DBA "a mano"
  cat("Usando generador simple de respaldo (sin agricolae)...\n")
  
  bloques <- paste0("B", seq_len(n_blocks))
  
  # Todos los checks en todos los bloques
  fb_checks <- expand.grid(
    block = bloques,
    trt   = checks,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  
  # Repartimos nuevos más o menos equitativamente
  # (aquí asumimos tamaño de bloque razonable)
  fb_news <- lapply(bloques, function(b) {
    data.frame(
      block = b,
      trt   = sample(new_trt_names, size = ceiling(n_new / n_blocks)),
      stringsAsFactors = FALSE
    )
  }) |> bind_rows()
  
  fieldbook <- bind_rows(fb_checks, fb_news)
  fieldbook$plots <- seq_len(nrow(fieldbook))  # columna de parcela ficticia
}

# Aseguramos tipos
fieldbook <- fieldbook |>
  as.data.frame() |>
  dplyr::mutate(
    block = factor(block),
    trt   = factor(trt)
  )

# Creamos una columna "tipo" (Check / New)
fieldbook <- fieldbook |>
  mutate(
    tipo = ifelse(trt %in% checks, "Check", "New")
  )

# Resumen rápido
cat("Resumen del fieldbook inicial:\n")
cat("  - Nº de filas (parcelas): ", nrow(fieldbook), "\n", sep = "")
cat("  - Nº de bloques:          ", n_distinct(fieldbook$block), "\n", sep = "")
cat("  - Nº de tratamientos:     ", n_distinct(fieldbook$trt), "\n\n", sep = "")

table_trt <- table(fieldbook$trt)
cat("Réplicas por tratamiento (primeros 10):\n")
print(head(table_trt, 10))
cat("\nNota: en un DBA típico, los checks aparecen varias veces,\n",
    "mientras que los nuevos deberían aparecer una sola vez.\n\n", sep = "")

# ----------------------------------------------------------
# 3) Simulación de la variable respuesta Y
# ----------------------------------------------------------

cat("----------------------------------------------------\n")
cat("PASO 2: Simulación de la variable respuesta Y\n")
cat("----------------------------------------------------\n\n")

# Parámetros de simulación (puedes jugar con ellos)
mu   <- 100   # media general
sd_G <- 10    # SD genotipos (variación genética verdadera)
sd_B <- 15    # SD bloques (gradiente entre bloques)
sd_E <- 5     # SD residual (ruido dentro de bloque)

cat("Parámetros de simulación:\n")
cat("  - Media general (mu):       ", mu,  "\n", sep = "")
cat("  - SD genotipos (sigma_G):   ", sd_G, "\n", sep = "")
cat("  - SD bloques (sigma_B):     ", sd_B, "\n", sep = "")
cat("  - SD residual (sigma_e):    ", sd_E, "\n\n", sep = "")

# Efectos aleatorios de genotipo y bloque
trt_levels <- levels(fieldbook$trt)
blk_levels <- levels(fieldbook$block)

eff_trt <- rnorm(length(trt_levels), mean = 0, sd = sd_G)
names(eff_trt) <- trt_levels

eff_blk <- rnorm(length(blk_levels), mean = 0, sd = sd_B)
names(eff_blk) <- blk_levels

# Construimos Y
fieldbook <- fieldbook |>
  mutate(
    eff_trt = eff_trt[trt],
    eff_blk = eff_blk[block],
    Y       = mu + eff_trt + eff_blk + rnorm(n(), mean = 0, sd = sd_E)
  )

cat("Vista de las primeras filas del fieldbook simulado:\n")
print(head(fieldbook, 10))
cat("\n")

# ----------------------------------------------------------
# 4) Exploración gráfica: diseño y respuesta simulada
# ----------------------------------------------------------

cat("----------------------------------------------------\n")
cat("PASO 3: Exploración gráfica del diseño y de Y\n")
cat("----------------------------------------------------\n\n")

# 4.1 Distribución global de Y
p_hist <- ggplot(fieldbook, aes(x = Y)) +
  geom_histogram(bins = 30, color = "white") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribución de la respuesta simulada (Y)",
    x     = "Y",
    y     = "Frecuencia"
  )

# 4.2 Boxplot por bloque
p_box_block <- ggplot(fieldbook, aes(x = block, y = Y)) +
  geom_boxplot() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Boxplot de Y por bloque (gradiente entre bloques)",
    x     = "Bloque",
    y     = "Y"
  )

# 4.3 Boxplot por tipo (Check vs New)
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
fieldbook_mapa <- fieldbook |>
  group_by(block) |>
  arrange(trt, .by_group = TRUE) |>
  mutate(pos = row_number()) |>
  ungroup()

p_mapa <- ggplot(
  fieldbook_mapa,
  aes(x = block, y = pos, fill = Y, label = trt)
) +
  geom_tile(color = "white") +
  geom_text(size = 2.8) +
  scale_y_reverse() +
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
cat("  - Histograma global de Y\n")
cat("  - Boxplot de Y por bloque\n")
cat("  - Boxplot de Y por tipo (Check vs New)\n")
cat("  - Mapa tipo heatmap del ensayo por bloque\n\n")

# ----------------------------------------------------------
# 5) Análisis clásico tipo Federer (augmentedRCBD)
# ----------------------------------------------------------

cat("----------------------------------------------------\n")
cat("PASO 4: Análisis clásico tipo Federer\n")
cat("----------------------------------------------------\n\n")

if (has_augRCBD) {
  cat("Usando 'augmentedRCBD::augmentedRCBD' para el análisis clásico DBA...\n\n")
  
  # Ajuste clásico: checks usados para estimar error y ajustar nuevos
  res_classic <- augmentedRCBD::augmentedRCBD(
    block       = fieldbook$block,
    treatment   = fieldbook$trt,
    y           = fieldbook$Y,
    checks      = checks,
    method.comp = "lsd",
    alpha       = 0.05,
    group       = FALSE,
    console     = TRUE
  )
  
  cat("\n--- Tabla ANOVA (augmentedRCBD) ---\n")
  print(res_classic$ANOVA)
  
  cat("\n--- Medias ajustadas por tratamiento (primeras filas) ---\n")
  print(head(res_classic$treatmentMeans))
  
} else {
  cat("El paquete 'augmentedRCBD' no está disponible.\n")
  cat("Se muestra un ANOVA aproximado usando lm(Y ~ block + trt),\n")
  cat("solo como referencia pedagógica (NO es el análisis exacto DBA clásico).\n\n")
  
  mod_lm <- lm(Y ~ block + trt, data = fieldbook)
  print(anova(mod_lm))
}

cat("\nComentario didáctico:\n")
cat("- En el enfoque clásico, los testigos (checks) se usan para estimar el error\n")
cat("  y corregir las medias de los nuevos dentro de cada bloque.\n")
cat("- La tabla ANOVA permite ver si hay diferencias globales entre tratamientos.\n\n")

# ----------------------------------------------------------
# 6) Modelo mixto (LMM) con bloques y genotipos aleatorios
#    y obtención de BLUPs
# ----------------------------------------------------------

cat("----------------------------------------------------\n")
cat("PASO 5: Modelo mixto (LMM) y BLUPs de genotipos\n")
cat("----------------------------------------------------\n\n")

cat("Ajustando el modelo mixto:\n")
cat("  Y ~ 1 + (1 | block) + (1 | trt)\n\n")

mod_lmm <- lmer(Y ~ 1 + (1 | block) + (1 | trt), data = fieldbook)

cat("Resumen del modelo mixto:\n")
print(summary(mod_lmm))

cat("\n--- Componentes de varianza (VarCorr) ---\n")
vc <- VarCorr(mod_lmm)
print(vc)

vc_df <- as.data.frame(vc)[, c("grp", "vcov")]
names(vc_df) <- c("Componente", "Varianza")
cat("\nResumen de varianzas estimadas:\n")
print(vc_df)

cat("\nInterpretación rápida:\n")
cat("- 'block' captura variación ambiental entre bloques (σ²_B).\n")
cat("- 'trt' captura variación genética entre genotipos (σ²_G).\n")
cat("- 'Residual' es la varianza residual (σ²_e).\n\n")

# 6.1 BLUPs de genotipos
ran_trt <- ranef(mod_lmm)$trt
ran_trt <- tibble::rownames_to_column(as.data.frame(ran_trt), var = "trt")
names(ran_trt)[2] <- "BLUP"  # columna de efecto aleatorio

# Añadimos tipo de tratamiento
ran_trt <- ran_trt |>
  mutate(
    tipo = ifelse(trt %in% checks, "Check", "New")
  )

cat("BLUPs de genotipos (primeras 10 filas):\n")
print(head(ran_trt, 10))

# 6.2 Gráfico de los Top 20 genotipos según BLUP
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
  scale_fill_manual(values = c("Check" = "#ff7f0e", "New" = "#1f77b4"),
                    name = "Tipo")

print(p_blups)

cat("\nComentario didáctico sobre BLUPs:\n")
cat("- Los BLUPs reflejan el desempeño esperado de cada genotipo,\n")
cat("  habiendo corregido por el efecto de bloque y el error.\n")
cat("- En etapas tempranas de mejoramiento, donde los nuevos tienen\n")
cat("  una sola observación, el uso de BLUPs es preferible a trabajar\n")
cat("  con medias crudas.\n")
cat("- Los BLUPs tienden a 'encoger' (shrink) estimaciones extremas de\n")
cat("  genotipos con poca información hacia la media general.\n\n")