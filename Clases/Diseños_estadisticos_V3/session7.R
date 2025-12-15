# ============================================================
# Sesión 7 – Diseños Fila–Columna (Row-Column Designs)
# Script de clase: simulación, EDA y modelos mixtos
# ============================================================

# ------------------------------------------------------------
# 0) Paquetes y opciones generales
# ------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  dplyr,
  tidyr,
  ggplot2,
  lme4,        # modelos mixtos
  lmerTest,    # p-valores y ANOVA tipo III
  emmeans      # medias marginales estimadas
)

theme_set(theme_bw())

set.seed(123)  # para reproducibilidad de la simulación

# ------------------------------------------------------------
# 1) Simular un diseño Fila–Columna (fieldbook)
# ------------------------------------------------------------
# Idea didáctica:
#   - Un solo “campo” organizado como grilla Row × Col.
#   - Cada celda es una parcela con un tratamiento (genotipo).
#   - Simulamos gradientes espaciales:
#       * Fila (Norte–Sur)
#       * Columna (Este–Oeste)
#   - Sobre eso, agregamos diferencias entre tratamientos + ruido residual.

# Parámetros del diseño (puedes cambiar para jugar en clase)
n_rows <- 4      # nº de filas (Row)
n_cols <- 6      # nº de columnas (Col)
n_trts <- 24     # nº de tratamientos (uno por celda)
mu     <- 100    # media general del ensayo

# Comprobación: n_rows * n_cols debe ser ≥ n_trts
n_cells <- n_rows * n_cols
stopifnot(n_cells >= n_trts)

# Etiquetas de tratamientos: G01, G02, ..., G24
trt_labels <- paste0("G", sprintf("%02d", seq_len(n_trts)))

# Grilla base Row × Col
fieldbook <- expand.grid(
  Row = seq_len(n_rows),
  Col = seq_len(n_cols)
) |>
  arrange(Row, Col)

# Asignar tratamientos:
# - Aquí, 1 tratamiento por celda.
# - Si n_cells > n_trts, se podrían repetir tratamientos o usar “Blank”.
fieldbook$Trt <- sample(
  x = trt_labels,
  size = nrow(fieldbook),
  replace = FALSE
)

# (Opcional) columna de repetición:
#   En este ejemplo usamos una sola repetición (Rep = 1).
fieldbook$Rep <- 1L

# Estructura del fieldbook
dplyr::glimpse(fieldbook)
head(fieldbook)

# ------------------------------------------------------------
# 2) Simular gradientes espaciales y respuesta Y
# ------------------------------------------------------------
# Componentes de la simulación:
#   - Efecto de tratamiento (Trt): variación verdadera entre genotipos.
#   - Efecto de fila (Row): gradiente Norte–Sur.
#   - Efecto de columna (Col): gradiente Este–Oeste.
#   - Error residual (ε): ruido dentro de parcela.

sd_trt <- 5    # SD entre tratamientos (señal genética)
sd_row <- 3    # SD gradiente en filas (suave)
sd_col <- 10   # SD gradiente en columnas (más fuerte)
sd_eps <- 4    # SD residual

# Efectos aleatorios simulados
eff_trt <- rnorm(n_trts, mean = 0, sd = sd_trt)
names(eff_trt) <- trt_labels

eff_row <- rnorm(n_rows, mean = 0, sd = sd_row)
names(eff_row) <- as.character(seq_len(n_rows))

eff_col <- rnorm(n_cols, mean = 0, sd = sd_col)
names(eff_col) <- as.character(seq_len(n_cols))

# Construir Y para cada celda Row × Col
fieldbook <- fieldbook |>
  mutate(
    eff_Trt = eff_trt[Trt],
    eff_Row = eff_row[as.character(Row)],
    eff_Col = eff_col[as.character(Col)],
    eps     = rnorm(n(), mean = 0, sd = sd_eps),
    Y       = mu + eff_Trt + eff_Row + eff_Col + eps
  )

dplyr::glimpse(fieldbook)
summary(fieldbook$Y)

# Comentario didáctico:
#   - Si sd_row y sd_col son grandes, vemos bandas claras en el mapa espacial.
#   - Si sd_trt es grande, hay diferencias fuertes entre tratamientos.
#   - sd_eps controla cuán ruidoso es el ensayo dentro de cada celda.

# ------------------------------------------------------------
# 3) EDA – Exploración del diseño y del gradiente espacial
# ------------------------------------------------------------

# 3.1 Estructura básica
dplyr::count(fieldbook, Row, Col)  # debería ser 1 parcela por celda
dplyr::count(fieldbook, Trt)       # cada tratamiento una vez (en este diseño)

# 3.2 Histograma global de Y
g_hist <- ggplot(fieldbook, aes(x = Y)) +
  geom_histogram(bins = 15, colour = "white") +
  labs(
    title = "Distribución global de la respuesta (Y)",
    x = "Y (respuesta simulada)",
    y = "Frecuencia"
  )

print(g_hist)

# 3.3 Mapa espacial Row × Col
g_spatial <- ggplot(
  fieldbook,
  aes(x = Col, y = Row, fill = Y)
) +
  geom_tile(color = "white") +
  scale_y_reverse() +  # fila 1 arriba
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = "Mapa espacial de Y en la grilla Fila × Columna",
    x = "Columna (Este–Oeste)",
    y = "Fila (Norte–Sur)"
  ) +
  theme_minimal(base_size = 12)

print(g_spatial)

# 3.4 Gráfico simple por tratamiento
g_trt_box <- ggplot(fieldbook, aes(x = Trt, y = Y)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Respuesta por tratamiento (Y ~ Trt)",
    x = "Tratamiento",
    y = "Y"
  )

print(g_trt_box)

# ------------------------------------------------------------
# 4) Modelo ingenuo vs. Modelo Row–Column (LMM)
# ------------------------------------------------------------
# Modelo 1: ingenuo (CRD), ignora filas y columnas
#   Y ~ Trt
#   Útil como punto de comparación: trata el campo como homogéneo.
# Modelo 2: Row–Column LMM (filas y columnas como bloqueos espaciales)
#   Y ~ Trt + (1|Row) + (1|Col)
#   Captura la variación de gradiente en dos direcciones.

# Aseguramos factores
datos <- fieldbook |>
  mutate(
    Row = factor(Row),
    Col = factor(Col),
    Trt = factor(Trt),
    Rep = factor(Rep)
  )

# ---------- Modelo 1: CRD / “naive” ----------
mod_crd <- lm(Y ~ Trt, data = datos)
anova(mod_crd)

cat("\nResumen modelo CRD (ingenuo, sin filas/columnas):\n")
print(summary(mod_crd))

# ---------- Modelo 2: Row–Column LMM ----------
mod_rc <- lmer(
  Y ~ Trt + (1 | Row) + (1 | Col),
  data = datos,
  REML = TRUE
)

cat("\nResumen modelo Row–Column (LMM):\n")
print(summary(mod_rc))

cat("\nANOVA tipo III para tratamientos (lmerTest):\n")
print(anova(mod_rc, type = 3))

# Comparación informal vía AIC
cat("\nComparación AIC (CRD vs Row–Column):\n")
print(AIC(mod_crd, mod_rc))

# Comentario agronómico que puedes usar en clase:
#   - Si el modelo Row–Column tiene AIC menor y varianzas de Row/Col apreciables,
#     significa que el diseño está capturando heterogeneidad espacial real.
#   - Ignorar filas/columnas (modelo CRD) puede inflar la varianza residual
#     y reducir la precisión de las comparaciones entre tratamientos.

# ------------------------------------------------------------
# 5) Componentes de varianza (VarCorr) – interpretación
# ------------------------------------------------------------

cat("\nComponentes de varianza (modelo Row–Column):\n")
vc <- VarCorr(mod_rc)
print(vc, comp = c("Variance", "Std.Dev."))

# Lectura en clase:
#   - Var(Row): variabilidad asociada al gradiente Norte–Sur.
#   - Var(Col): variabilidad asociada al gradiente Este–Oeste.
#   - Var(Residual): variación dentro de celda Row × Col × Trt,
#                    después de descontar filas, columnas y tratamientos.

# ------------------------------------------------------------
# 6) Medias ajustadas por tratamiento (EMMs) y gráficos
# ------------------------------------------------------------

emm_trt <- emmeans(mod_rc, ~ Trt)
emm_trt_df <- as.data.frame(emm_trt)

cat("\nMedias marginales estimadas por tratamiento (EMMs):\n")
print(emm_trt)

cat("\nComparaciones múltiples (Tukey) entre tratamientos:\n")
compar_trt <- contrast(emm_trt, method = "tukey")
print(compar_trt)

# 6.1 Ranking de tratamientos (medias ajustadas)
g_rank_trt <- ggplot(
  emm_trt_df,
  aes(x = reorder(Trt, emmean), y = emmean)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    title = "Ranking de tratamientos (medias ajustadas, Row–Column LMM)",
    x = "Tratamiento",
    y = "Y ajustado"
  )

print(g_rank_trt)

# 6.2 Comparar observados vs ajustados (scatter)
datos_pred <- datos |>
  mutate(
    fitted_rc = fitted(mod_rc)
  )

g_obs_fit <- ggplot(datos_pred, aes(x = fitted_rc, y = Y)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  labs(
    title = "Observados vs ajustados (modelo Row–Column)",
    x = "Y ajustado (fitted)",
    y = "Y observado"
  )

print(g_obs_fit)

# ------------------------------------------------------------
# 7) Exportar tablas clave para usar en informes / Shiny
# ------------------------------------------------------------

if (!dir.exists("resultados_session7_rowcolumn")) {
  dir.create("resultados_session7_rowcolumn", recursive = TRUE)
}

# Fieldbook simulado completo
write.csv(
  fieldbook,
  file = "resultados_session7_rowcolumn/fieldbook_rowcolumn_simulado.csv",
  row.names = FALSE
)

# Medias ajustadas por tratamiento
write.csv(
  emm_trt_df,
  file = "resultados_session7_rowcolumn/emm_tratamientos_rowcolumn.csv",
  row.names = FALSE
)

# Comparaciones múltiples
compar_trt_df <- as.data.frame(compar_trt)
write.csv(
  compar_trt_df,
  file = "resultados_session7_rowcolumn/comparaciones_tratamientos_tukey.csv",
  row.names = FALSE
)

cat("\nArchivos exportados en 'resultados_session7_rowcolumn/'.\n")