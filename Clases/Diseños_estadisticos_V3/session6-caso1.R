# ============================================================
# session6-caso1.R
# Strip-plot Variedad × Corte en 4 bloques (CASO 1):
# ANCOVA mixto con covariable (Dias_Antesis).
#
# Diseño experimental:
#   - Experimento de campo con diseño strip-plot:
#       * Factor A: Variedad (10 niveles, V1–V10), tiras en una dirección.
#       * Factor B: Corte (4 momentos, niveles 1–4), tiras en la otra dirección.
#       * Bloques: 4 (I, II, III, IV), dispuestos en el campo.
#   - En cada bloque se cruzan todas las variedades con todos los cortes:
#       4 bloques × 10 variedades × 4 cortes = 160 unidades experimentales.
#   - Estructura de error:
#       * Estrato 1: Bloque (variación global entre bloques).
#       * Estrato 2: Bloque:Variedad (tiras de variedades dentro de bloque).
#       * Estrato 3: Bloque:Corte (tiras de cortes dentro de bloque).
#       * Residual: intersecciones Variedad × Corte dentro de cada bloque
#         (unidad experimental donde se mide el rendimiento).
#
# Covariable:
#   - Dias_Antesis: número de días desde un evento fenológico (p.ej., antesis)
#     hasta el corte, medida por unidad experimental.
#   - Objetivo: ajustar el rendimiento por variación en Dias_Antesis para
#     reducir el error residual y obtener comparaciones de variedades/cortes
#     "a igual nivel" medio de la covariable (ANCOVA).
#
# Modelo lineal mixto sin covariable (referencia):
#   y_ijk = μ + α_i + β_j + (αβ)_ij
#           + b_k + (ba)_ik + (bb)_jk + ε_ijk
#
#   donde:
#     y_ijk   : rendimiento en la variedad i, corte j, bloque k
#     α_i     : efecto fijo de la Variedad i
#     β_j     : efecto fijo del Corte j
#     (αβ)_ij : interacción Variedad × Corte
#     b_k     : efecto aleatorio del Bloque k
#     (ba)_ik : efecto aleatorio Bloque:Variedad (tira de Variedad en bloque)
#     (bb)_jk : efecto aleatorio Bloque:Corte (tira de Corte en bloque)
#     ε_ijk   : error residual (unidad experimental)
#
# Modelo ANCOVA mixto (covariable centrada z_ijk):
#   Definimos z_ijk = Dias_Antesis_ijk - mean(Dias_Antesis)
#
#   y_ijk = μ + α_i + β_j + (αβ)_ij + γ z_ijk
#           + b_k + (ba)_ik + (bb)_jk + ε_ijk
#
#   - γ es la pendiente común (ajuste por Dias_Antesis).
#   - Los efectos α_i, β_j y (αβ)_ij se interpretan como diferencias de medias
#     ajustadas al valor medio de Dias_Antesis (z = 0).
#
# Supuestos clave de la ANCOVA:
#   1) La covariable es medida antes o de forma independiente a los tratamientos.
#   2) Relación aproximadamente lineal entre y y Dias_Antesis.
#   3) Homogeneidad de pendientes: la pendiente de y vs Dias_Antesis es similar
#      entre niveles de Variedad y Corte (si no se modelan pendientes distintas).
#   4) Supuestos habituales del LMM:
#        - Residuos ~ N(0, σ^2) (normalidad).
#        - Homogeneidad de varianza (sin patrones fuertes en residuos vs ajustados).
#        - Aleatorización respetada (independencia condicional al modelo).
#
# Flujo:
#   0) Paquetes y opciones
#   1) Importar y preparar datos
#   2) EDA (Y y covariable)
#   3) Modelos candidatos:
#        - LMM sin covariable (referencia)
#        - LMM con covariable (ANCOVA)
#        - (Opcional) prueba de homogeneidad de pendientes
#   4) Diagnóstico de supuestos
#   5) Post hoc (medias ajustadas)
#   6) Gráficos de resultados
#   7) Exportar tablas
# ============================================================

# ------------------------------------------------------------
# 0) Paquetes y opciones
# ------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  readxl,
  dplyr, tidyr,
  ggplot2,
  lme4,
  lmerTest,
  emmeans,
  broom.mixed
)

theme_set(theme_bw())

# ------------------------------------------------------------
# 1) Importar y preparar datos
# ------------------------------------------------------------

ruta_archivo <- "C:/Users/ALEX/OneDrive/Asesorias/Escuela CEDEPA/2da capacitación/Curso_disennos_experimentales/Clases/Diseños_estadisticos_V3/Libro1 (1).xlsx"

datos_raw <- readxl::read_excel(
  path  = ruta_archivo,
  sheet = "CASO 1"
)

# Ajusta aquí los nombres reales de columnas si difieren:
datos <- datos_raw |>
  dplyr::rename(
    bloque       = Bloque,
    variedad     = Variedad,
    corte_num    = Corte,          # 30, 45, 60, 75, etc.
    dias_antesis = Dias_Antesis,   # nombre asumido
    y            = Rendimiento
  ) |>
  dplyr::mutate(
    bloque       = factor(bloque),
    variedad     = factor(variedad),
    corte        = factor(corte_num),
    dias_antesis = as.numeric(dias_antesis)
  )

# Comprobaciones básicas de estructura
print(dim(datos))
dplyr::glimpse(datos)

# Chequeo de 1 observación por celda Bloque:Variedad:Corte
tabla_n <- datos |>
  dplyr::count(bloque, variedad, corte)

stopifnot(all(tabla_n$n == 1))

# ------------------------------------------------------------
# 2) EDA (Y y covariable)
# ------------------------------------------------------------

# 2.1 Resumen numérico
summary(datos[, c("y", "dias_antesis")])

# 2.2 Correlación simple entre covariable y rendimiento
cor_y_cov <- cor(datos$y, datos$dias_antesis)
cat("Correlación Rendimiento ~ Dias_Antesis:", cor_y_cov, "\n")

# 2.3 Histogramas
g_hist_y <- ggplot(datos, aes(x = y)) +
  geom_histogram(bins = 15, colour = "white") +
  labs(
    title = "Distribución de rendimiento (CASO 1)",
    x = "Rendimiento",
    y = "Frecuencia"
  )

g_hist_cov <- ggplot(datos, aes(x = dias_antesis)) +
  geom_histogram(bins = 15, colour = "white") +
  labs(
    title = "Distribución de la covariable (Días a antesis)",
    x = "Días a antesis",
    y = "Frecuencia"
  )

# 2.4 Boxplots por Variedad y Corte (Y)
g_box_var <- ggplot(datos, aes(x = variedad, y = y)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento por variedad",
    x = "Variedad",
    y = "Rendimiento"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

g_box_corte <- ggplot(datos, aes(x = corte, y = y)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento por corte",
    x = "Corte",
    y = "Rendimiento"
  )

# 2.5 Relación Y ~ covariable (global)
g_scatter_cov <- ggplot(datos, aes(x = dias_antesis, y = y)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relación rendimiento ~ días a antesis (global)",
    x = "Días a antesis",
    y = "Rendimiento"
  )

# 2.6 Relación Y ~ covariable por variedad
g_scatter_cov_var <- ggplot(
  datos,
  aes(x = dias_antesis, y = y, colour = variedad)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Rendimiento ~ días a antesis por variedad",
    x = "Días a antesis",
    y = "Rendimiento"
  )

# 2.7 Relación Y ~ covariable por corte
g_scatter_cov_corte <- ggplot(
  datos,
  aes(x = dias_antesis, y = y, colour = corte)
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Rendimiento ~ días a antesis por corte",
    x = "Días a antesis",
    y = "Rendimiento"
  )

print(g_hist_y)
print(g_hist_cov)
print(g_box_var)
print(g_box_corte)
print(g_scatter_cov)
print(g_scatter_cov_var)
print(g_scatter_cov_corte)

# Comentario didáctico:
#   En estos gráficos el objetivo es ver si la covariable se asocia
#   negativamente o positivamente con Y y si la "pendiente" parece similar
#   entre variedades y cortes (paralelismo de regresiones).

# ------------------------------------------------------------
# 3) Modelos candidatos: LMM sin y con covariable
# ------------------------------------------------------------

# 3.1 LMM base sin covariable (solo para comparar)
mod_lmm_base <- lmer(
  y ~ variedad * corte +
    (1 | bloque) +
    (1 | bloque:variedad) +
    (1 | bloque:corte),
  data = datos,
  REML = TRUE
)

summary(mod_lmm_base)
anova(mod_lmm_base, type = 3)

# 3.2 LMM con covariable (ANCOVA) con pendiente común
#     Hipótesis: el efecto de dias_antesis es lineal y similar en
#     todas las variedades y cortes (una sola pendiente global).
mod_lmm_cov <- lmer(
  y ~ dias_antesis + variedad * corte +
    (1 | bloque) +
    (1 | bloque:variedad) +
    (1 | bloque:corte),
  data = datos,
  REML = TRUE
)

summary(mod_lmm_cov)
anova(mod_lmm_cov, type = 3)

# 3.3 Test formal: ¿la covariable mejora el ajuste frente al modelo base?
anova(mod_lmm_base, mod_lmm_cov)  # comparación vía LRT (mismo REML: usar ML si quieres ser 100% ortodoxo)

# 3.4 (Opcional) Probar pendiente distinta por variedad (interacción con covariable)
#     Esto evalúa si Variedades responden distinto a la fenología.
mod_lmm_cov_var <- lmer(
  y ~ dias_antesis * variedad + corte +
    (1 | bloque) +
    (1 | bloque:variedad) +
    (1 | bloque:corte),
  data = datos,
  REML = TRUE
)

anova(mod_lmm_cov, mod_lmm_cov_var)

#   En docencia:
#     - Si la interacción dias_antesis:variedad NO es significativa y
#       las pendientes parecen paralelas, nos quedamos con mod_lmm_cov.
#     - Si sí es significativa, se puede discutir un modelo con pendientes
#       específicas por variedad (pero complica la interpretación en clase).

# ------------------------------------------------------------
# 4) Diagnóstico de supuestos (usando mod_lmm_cov)
# ------------------------------------------------------------

datos_diag <- broom.mixed::augment(mod_lmm_cov)

g_res_fit <- ggplot(datos_diag, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    title = "Residuos vs valores ajustados (modelo con covariable)",
    x = "Ajustados",
    y = "Residuos"
  )

g_qq <- ggplot(datos_diag, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot de residuos (modelo con covariable)",
    x = "Cuantiles teóricos",
    y = "Cuantiles residuos"
  )

g_res_trat <- ggplot(
  datos_diag,
  aes(x = interaction(variedad, corte), y = .resid)
) +
  geom_boxplot() +
  labs(
    title = "Residuos por Variedad × Corte (modelo con covariable)",
    x = "Variedad:Corte",
    y = "Residuos"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(g_res_fit)
print(g_qq)
print(g_res_trat)

# ------------------------------------------------------------
# 5) Post hoc (medias ajustadas por la covariable)
# ------------------------------------------------------------

# Para ANCOVA, las EMMs se calculan típicamente en un valor de referencia
# de la covariable (por defecto, la media de dias_antesis en el dataset).
cov_prom <- mean(datos$dias_antesis, na.rm = TRUE)
cat("Valor de referencia de la covariable (media):", cov_prom, "\n")

# 5.1 Medias ajustadas por variedad (promediando cortes)
emm_var <- emmeans(
  mod_lmm_cov,
  ~ variedad,
  cov.reduce = list(dias_antesis = cov_prom)
)

pairs_var <- pairs(emm_var, adjust = "tukey")

print(emm_var)
print(pairs_var)

# 5.2 Medias ajustadas por corte
emm_corte <- emmeans(
  mod_lmm_cov,
  ~ corte,
  cov.reduce = list(dias_antesis = cov_prom)
)

pairs_corte <- pairs(emm_corte, adjust = "tukey")

print(emm_corte)
print(pairs_corte)

# 5.3 Interacción Variedad × Corte (ajustada)
emm_var_corte <- emmeans(
  mod_lmm_cov,
  ~ variedad | corte,
  cov.reduce = list(dias_antesis = cov_prom)
)

compar_var_dentro_corte <- contrast(emm_var_corte, method = "tukey")

print(emm_var_corte)
print(compar_var_dentro_corte)

# ------------------------------------------------------------
# 6) Gráficos de resultados (EMMs ajustadas)
# ------------------------------------------------------------

# 6.1 Interacción Variedad × Corte (medias ajustadas)
emm_df <- as.data.frame(emm_var_corte)

g_emm_inter <- ggplot(
  emm_df,
  aes(x = corte, y = emmean, colour = variedad, group = variedad)
) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = emmean - SE, ymax = emmean + SE),
    width = 0.1
  ) +
  labs(
    title = "Interacción Variedad × Corte (ajustada por días a antesis)",
    x = "Corte",
    y = "Rendimiento ajustado"
  ) +
  theme(legend.position = "bottom")

print(g_emm_inter)

# 6.2 Ranking de variedades (rendimiento ajustado)
emm_var_df <- as.data.frame(emm_var)

g_rank_var <- ggplot(
  emm_var_df,
  aes(x = reorder(variedad, emmean), y = emmean)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2
  ) +
  coord_flip() +
  labs(
    title = "Ranking de variedades (ajustado por días a antesis)",
    x = "Variedad",
    y = "Rendimiento ajustado"
  )

print(g_rank_var)

# 6.3 Efecto del corte (ajustado)
emm_corte_df <- as.data.frame(emm_corte)

g_corte <- ggplot(
  emm_corte_df,
  aes(x = corte, y = emmean)
) +
  geom_col() +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2
  ) +
  labs(
    title = "Efecto del momento de corte (ajustado por días a antesis)",
    x = "Corte",
    y = "Rendimiento ajustado"
  )

print(g_corte)

# 6.4 Gráfico de la relación Y ~ covariable usando el modelo
#     Curva ajustada global y por variedad.
#     Predicciones en un rango de la covariable:
nuevo_cov <- tibble::tibble(
  dias_antesis = seq(
    from = min(datos$dias_antesis, na.rm = TRUE),
    to   = max(datos$dias_antesis, na.rm = TRUE),
    length.out = 50
  )
) |>
  tidyr::crossing(
    variedad = levels(datos$variedad),
    corte    = levels(datos$corte),
    bloque   = NA  # bloque no afecta el valor medio esperado
  )

pred_cov <- broom.mixed::augment(
  mod_lmm_cov,
  newdata = nuevo_cov,
  re.form = NA
)

g_pred_cov_var <- ggplot(
  pred_cov,
  aes(x = dias_antesis, y = .fitted, colour = variedad)
) +
  geom_line() +
  facet_wrap(~ corte) +
  labs(
    title = "Curvas ajustadas Rendimiento ~ días a antesis",
    x = "Días a antesis",
    y = "Rendimiento ajustado"
  )

print(g_pred_cov_var)

# ------------------------------------------------------------
# 7) Exportar tablas clave
# ------------------------------------------------------------

if (!dir.exists("resultados_caso1")) dir.create("resultados_caso1")

write.csv(
  emm_var_df,
  file = "resultados_caso1/emm_variedades_ajustadas.csv",
  row.names = FALSE
)

write.csv(
  emm_corte_df,
  file = "resultados_caso1/emm_cortes_ajustadas.csv",
  row.names = FALSE
)

write.csv(
  as.data.frame(emm_var_corte),
  file = "resultados_caso1/emm_variedad_por_corte_ajustadas.csv",
  row.names = FALSE
)
