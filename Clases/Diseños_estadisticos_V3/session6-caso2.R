# ============================================================
# session6-caso2.R
# Strip-plot Variedad × Corte en 4 bloques (CASO 2):
# Modelo lineal mixto (LMM) sin covariable.
#
# Contexto del CASO 2 (hoja “CASO 2” del Excel):
#   Variables en el archivo:
#     - Bloque
#     - Variedad
#     - Corte   (tiempos: 30, 45, 60, 75 días, por ejemplo)
#     - Rendimiento
#
# Diseño experimental:
#   - Experimento de campo con diseño strip-plot:
#       * Factor A: Variedad (10 niveles, V1–V10), tiras en una dirección.
#       * Factor B: Corte (4 momentos de cosecha), tiras en la otra dirección.
#       * Bloques: 4 (I, II, III, IV), dispuestos en el campo.
#   - En cada bloque se cruzan todas las variedades con todos los cortes:
#       4 bloques × 10 variedades × 4 cortes = 160 unidades experimentales.
#
# Estructura de error en strip-plot:
#   - Estrato 1: Bloque (variación global entre bloques).
#   - Estrato 2: Bloque:Variedad  (tiras de variedades dentro de bloque).
#   - Estrato 3: Bloque:Corte     (tiras de cortes dentro de bloque).
#   - Residual: intersecciones Variedad × Corte dentro de cada bloque
#               (unidad experimental donde se mide el rendimiento).
#
# Modelo lineal mixto (LMM) para CASO 2 (sin covariable):
#
#   y_ijk = μ + α_i + β_j + (αβ)_ij
#           + b_k + (ba)_ik + (bb)_jk + ε_ijk
#
#   donde:
#     y_ijk   : rendimiento en la variedad i, corte j, bloque k
#     α_i     : efecto fijo de la Variedad i
#     β_j     : efecto fijo del Corte j
#     (αβ)_ij : interacción fija Variedad × Corte
#     b_k     : efecto aleatorio del Bloque k
#     (ba)_ik : efecto aleatorio Bloque:Variedad
#     (bb)_jk : efecto aleatorio Bloque:Corte
#     ε_ijk   : error residual (unidad experimental)
#
#   - Se estima por REML usando lmer (lme4 / lmerTest).
#   - El modelo respeta la estructura jerárquica del strip-plot al incluir
#     los tres estratos de error como efectos aleatorios.
#
# Objetivos del análisis:
#   1) Explorar la variabilidad del rendimiento por Variedad, Corte y Bloque.
#   2) Ajustar modelos:
#        - ANOVA “naive” (bloque fijo, un solo error) como comparación.
#        - LMM strip-plot con Bloque, Bloque:Variedad y Bloque:Corte aleatorios.
#   3) Evaluar supuestos (normalidad, homogeneidad, outliers).
#   4) Obtener medias marginales estimadas (EMMs) de Variedad, Corte e
#      interacción Variedad × Corte, con comparaciones post hoc (Tukey).
#   5) Construir gráficos (interacción, ranking de variedades, efecto de cortes)
#      para comunicar resultados a estudiantes o equipo técnico.
#
# Flujo del script:
#   0) Paquetes y opciones
#   1) Importar y preparar datos
#   2) EDA (exploración)
#   3) Modelos candidatos: ANOVA vs LMM strip-plot
#   4) Diagnóstico de supuestos
#   5) Post hoc detallado con emmeans
#   6) Gráficos de resultados (EMMs)
#   7) Exportar tablas
# ============================================================

# ------------------------------------------------------------
# 0) Paquetes y opciones
# ------------------------------------------------------------

if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}

pacman::p_load(
  readxl,      # lectura de Excel
  dplyr, tidyr,
  ggplot2,     # gráficos
  lme4,        # modelos mixtos
  lmerTest,    # p-valores en lmer
  emmeans,     # medias marginales estimadas
  broom.mixed  # diagnósticos de LMM
)

theme_set(theme_bw())

# ------------------------------------------------------------
# 1) Importar y preparar datos (CASO 2)
# ------------------------------------------------------------

# ruta_archivo <- "C:/Users/ALEX/OneDrive/Asesorias/Escuela CEDEPA/2da capacitación/Curso_disennos_experimentales/Clases/Diseños_estadisticos_V3/Libro1 (1).xlsx"
ruta_archivo <- "Libro1 (1).xlsx" # Ruta relativa para fines didácticos

datos_raw <- readxl::read_excel(
  path  = ruta_archivo,
  sheet = "CASO 2"
)

# Estandarizamos nombres y tipos
datos <- datos_raw |>
  dplyr::rename(
    bloque    = Bloque,
    variedad  = Variedad,
    corte_num = Corte,        # 30, 45, 60, 75 (numérico, útil para gráficos)
    y         = Rendimiento
  ) |>
  dplyr::mutate(
    bloque   = factor(bloque),
    variedad = factor(variedad),
    corte    = factor(corte_num)  # corte como factor (efecto fijo categórico)
  )

# Comprobar balance: 4 bloques × 10 variedades × 4 cortes = 160 observaciones
print(dim(datos))
# Esperado en la consola:
# [1] 160   5  → 160 filas, 5 columnas

print(dplyr::count(datos, bloque, variedad, corte))
# Esperado:
# - Una tabla con 160 filas (todas las combinaciones) y n = 1 en cada celda.
# - Si algún n ≠ 1, hay celdas con más de una observación o alguna falta.

# Chequeo rápido de que haya 1 observación por celda Bloque:Variedad:Corte
tabla_n <- datos |>
  dplyr::count(bloque, variedad, corte)

stopifnot(all(tabla_n$n == 1))
# Si hay alguna combinación con n ≠ 1, aquí R lanza error.
# Esto obliga a revisar el diseño antes de modelar.

# ------------------------------------------------------------
# 2) EDA (exploración)
#    Inspirado en el protocolo de Zuur et al. (2010):
#    - estructura
#    - outliers en Y
#    - distribuciones
#    - relaciones básicas Variedad/Corte/Bloque
# ------------------------------------------------------------

# 2.1 Estructura general
dplyr::glimpse(datos)
summary(datos)

# Interpretación típica de la salida:
# - glimpse: confirma tipos:
#     bloque: factor con niveles I, II, III, IV
#     variedad: factor V1…V10
#     corte_num: numérico (30, 45, 60, 75)
#     y: numérico (rendimiento)
#     corte: factor (30, 45, 60, 75)
# - summary:
#     corte_num: Min 30, Max 75, distrib uniforme (cada corte 40 obs)
#     y: medias y cuartiles cercanos a lo esperado del diseño simulado
#     bloque: 40 obs por bloque
#     corte: 40 obs por nivel

# 2.2 Resumen por tratamiento (Variedad × Corte)
resumen_trat <- datos |>
  dplyr::group_by(variedad, corte) |>
  dplyr::summarise(
    n     = dplyr::n(),   # n esperado = 4 (4 bloques)
    media = mean(y),
    sd    = sd(y),
    .groups = "drop"
  )

print(resumen_trat)
# Interpretación:
# - Deberías ver 40 filas (10 variedades × 4 cortes).
# - n = 4 para todos → diseño balanceado.
# - media: promedios por tratamiento (Variedad×Corte).
# - sd muy pequeños (≈ 0.4) en este dataset de ejemplo → datos “limpios”.
#   En datos reales, sd más grandes indicarían mayor variabilidad entre bloques.

# 2.3 Distribución global del rendimiento
g_hist <- ggplot(datos, aes(x = y)) +
  geom_histogram(bins = 15, colour = "white") +
  labs(
    title = "Distribución de rendimiento (global)",
    x = "Rendimiento",
    y = "Frecuencia"
  )

# Esperado:
# - Histograma unimodal, relativamente “suave”.
# - No se esperan colas extremas ni outliers evidentes en este ejemplo.
# - Nos sirve para ver si una transformación (log, sqrt) podría ser necesaria.

# 2.4 Boxplots por factor
g_box_var <- ggplot(datos, aes(x = variedad, y = y)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento por variedad",
    x = "Variedad",
    y = "Rendimiento"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Interpretación:
# - Compara distribuciones de rendimiento por variedad (promedio sobre cortes).
# - Esperado: variedades V1 < V2 < … < V10, porque el dataset está construido
#   con un patrón creciente de rendimiento por variedad.
# - En datos reales, buscarías patrones de ranking, posibles variedades outliers, etc.

g_box_corte <- ggplot(datos, aes(x = corte, y = y)) +
  geom_boxplot() +
  labs(
    title = "Rendimiento por momento de corte",
    x = "Corte",
    y = "Rendimiento"
  )

# Interpretación:
# - Muestra el efecto de cortar antes o después (30 vs 75).
# - Esperado: corte 30 > 45 > 60 > 75 (rendimiento decrece al retrasar el corte).
# - Esto conecta con la biología del cultivo: pérdida de rendimiento si se cosecha tarde.

# 2.5 Trayectorias Variedad × Corte por bloque
g_lineas_bloque <- ggplot(
  datos,
  aes(x = corte_num, y = y, colour = variedad, group = variedad)
) +
  geom_line() +
  geom_point() +
  facet_wrap(~ bloque) +
  labs(
    title = "Trayectorias Variedad × Corte por bloque",
    x = "Momento de corte",
    y = "Rendimiento"
  )

# Interpretación:
# - Cada panel es un bloque, cada línea una variedad.
# - Valida visualmente:
#     * coherencia del efecto de corte dentro de cada bloque
#     * estabilidad del ranking de variedades entre bloques.
# - En datos reales buscarías:
#     * si el ranking de variedades cambia por bloque (interacción fuerte)
#     * si hay bloques “raros” con patrón muy distinto (problemas en campo).

print(g_hist)
print(g_box_var)
print(g_box_corte)
print(g_lineas_bloque)

# ------------------------------------------------------------
# 3) Modelos candidatos: ANOVA vs LMM strip-plot
# ------------------------------------------------------------
# Idea:
# - ANOVA "naive": trata bloque como fijo y usa un único error residual.
#   Ignora que hay dos tiras (Variedad y Corte) y sus errores asociados.
# - LMM strip-plot:
#   * Variedad, Corte e interacción como fijos (lo que queremos comparar).
#   * Efectos aleatorios:
#       (1 | bloque)          → variación entre bloques
#       (1 | bloque:variedad) → tiras de Variedad dentro de bloque (filas)
#       (1 | bloque:corte)    → tiras de Corte dentro de bloque (columnas)
#   * Residual → intersecciones Variedad × Corte dentro de cada bloque.

# 3.1 ANOVA "naive" (bloque fijo, un solo error)
mod_aov_naive <- aov(y ~ bloque + variedad * corte, data = datos)
summary(mod_aov_naive)

# Interpretación típica del summary.aov:
# - Tabla con:
#     bloque, variedad, corte, variedad:corte, Residuals
# - En este dataset todos los efectos salen altamente significativos
#   (p-values prácticamente 0), pero OJO:
#   * Los F se calculan con un único MSE (“Residuals”).
#   * No respeta los diversos estratos de error del strip-plot.
# - Mensaje conceptual:
#   “Esto es lo que obtendrías si no respetas el diseño”.

# 3.2 LMM para strip-plot (respeta la estructura de error)
mod_lmm <- lmer(
  y ~ variedad * corte +
    (1 | bloque) +
    (1 | bloque:variedad) +
    (1 | bloque:corte),
  data = datos,
  REML = TRUE
)

summary(mod_lmm)

# Interpretación de la salida summary(mod_lmm):
# - REML criterion at convergence: valor del log-likelihood (no se compara entre modelos con distinta estructura fija).
# - Random effects (componentes de varianza):
#     Groups          Name        Variance   Std.Dev.
#     bloque:variedad (Intercept) ~2.8e-04  (muy pequeño)
#     bloque:corte    (Intercept) ~9.6e-05  (aún más pequeño)
#     bloque          (Intercept) ~1.8e-01  (dominante entre los aleatorios)
#     Residual                    ~1.8e-03  (error en intersecciones)
#   En este ejemplo “de libro”, la variabilidad entre bloques es mucho mayor
#   que la variación a nivel de filas/columnas o residual.
# - Fixed effects:
#     (Intercept): media de referencia (Variedad V1, Corte 30 en Bloque “pivot”)
#     variedadV2…V10: diferencias respecto a V1 (en promedio sobre cortes)
#     corte45, corte60, corte75: diferencias respecto al corte 30
#     interacción variedad:corte: ajustes que permiten que la trayectoria
#     Variedad × Corte no sea perfectamente paralela.
#   Todos salen súper significativos porque los datos son muy “limpios”.

# Tabla ANOVA con df y F de Satterthwaite (lmerTest)
anova(mod_lmm, type = 3)  # tipo III para interpretar interacción

# Interpretación:
# - “Type III Analysis of Variance Table with Satterthwaite's method”
# - Filas:
#     variedad       (NumDF=9,  DenDF≈27)  → efecto principal
#     corte          (NumDF=3,  DenDF≈9)
#     variedad:corte (NumDF=27, DenDF≈81)
# - Los grados de libertad de denominador ahora dependen de la estructura
#   de random effects (Satterthwaite) y son más realistas que usar un único MSE.
# - En este ejemplo, todos los F son enormes y p-valores < 2.2e-16.

# 3.3 Comparación informal AIC (ANOVA vs LMM)
AIC(mod_aov_naive, mod_lmm)

# Interpretación:
# - Ambas filas tienen df = 44 (mismo nº de parámetros efectivos).
# - AIC mod_aov_naive < AIC mod_lmm en este dataset de juguete.
# - En diseños reales, NO usarías AIC para decir que la ANOVA “es mejor”,
#   porque el LMM está estructuralmente alineado con el diseño experimental.
# - Mensaje didáctico:
#   “Aunque el AIC sea menor en el modelo mal especificado, preferimos el
#    modelo que respeta el diseño y la estructura de error.”

# Comentario:
#   - El LMM respeta el diseño (múltiples estratos de error) y permite
#     generalizar conclusiones más allá de estos 4 bloques (bloque aleatorio).
#   - La ANOVA naive subestima varianzas de error para algunos efectos,
#     lo que puede inflar tests de significancia en diseños strip-plot.

# ------------------------------------------------------------
# 4) Diagnóstico de supuestos (sobre el LMM)
#    - Normalidad de residuos
#    - Homogeneidad de varianza
#    - Estructura de residuos vs ajustados
# ------------------------------------------------------------
# Objetivo:
# - Validar que el modelo mixto ajusta “razonablemente bien”.
# - En este CASO 2 sintético, todo se ve casi perfecto; en datos reales,
#   aquí discutirías transformaciones o modelos alternativos.

datos_diag <- broom.mixed::augment(mod_lmm)
# datos_diag contiene:
# - .fitted: valores ajustados
# - .resid: residuos
# - columnas originales (bloque, variedad, corte, etc.)

# 4.1 Residuos vs ajustados
g_res_fit <- ggplot(datos_diag, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(
    title = "Residuos vs valores ajustados",
    x = "Ajustados",
    y = "Residuos"
  )

# Interpretación:
# - En este ejemplo, verás una nube de puntos muy pegada a 0.
# - Buscamos:
#     * ausencia de patrón en abanico (heterocedasticidad)
#     * ausencia de curvaturas (falta de término no lineal)
# - En datos reales, si se observa que la varianza crece con el nivel
#   de rendimiento, podríamos considerar transformaciones o varianza
#   heterogénea (modelos más avanzados).

# 4.2 QQ-plot de residuos
g_qq <- ggplot(datos_diag, aes(sample = .resid)) +
  stat_qq() +
  stat_qq_line() +
  labs(
    title = "QQ-plot de residuos",
    x = "Cuantiles teóricos",
    y = "Cuantiles residuos"
  )

# Interpretación:
# - En estos datos sintéticos, los puntos prácticamente caen sobre la línea.
# - En datos reales, desviaciones fuertes en colas (S o C) indicarían
#   posibles problemas de normalidad o outliers.

# 4.3 Homogeneidad por tratamiento (Variedad:Corte)
g_res_trat <- ggplot(
  datos_diag,
  aes(x = interaction(variedad, corte), y = .resid)
) +
  geom_boxplot() +
  labs(
    title = "Residuos por combinación Variedad × Corte",
    x = "Variedad:Corte",
    y = "Residuos"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Interpretación:
# - Cada caja corresponde a un tratamiento Variedad × Corte.
# - En este ejemplo, todas las cajas son muy pequeñas (residuos ~0).
# - En datos reales, tratamientos con cajas muy anchas podrían sugerir
#   heterogeneidad de varianza entre tratamientos.

print(g_res_fit)
print(g_qq)
print(g_res_trat)

# ------------------------------------------------------------
# 5) Post hoc detallado con emmeans
#    En strip-plot, normalmente:
#      - evaluar interacción Variedad × Corte
#      - comparar variedades dentro de cada corte
#      - opcional: cortes dentro de cada variedad
# ------------------------------------------------------------
# Clave didáctica:
# - Primero mirar ANOVA (significancia de efectos).
# - Si la interacción Variedad × Corte es significativa (como aquí),
#   interpretamos principalmente esa interacción:
#     * Variedades dentro de cada corte
#     * Cortes dentro de cada variedad

# 5.1 Medias marginales por variedad (promedio sobre todos los cortes)
emm_var <- emmeans(mod_lmm, ~ variedad)
pairs_var <- pairs(emm_var, adjust = "tukey")

print(emm_var)
print(pairs_var)

# Interpretación (emm_var):
# - Columna emmean: rendimiento ajustado promedio de cada variedad
#   (promedio sobre los 4 cortes).
# - En este ejemplo:
#     V1 ≈ 23.4, V2 ≈ 25.2, …, V10 ≈ 40.8
#   creciente conforme aumenta el índice de la variedad.
# - lower.CL y upper.CL: intervalos de confianza al 95%.
#
# Interpretación (pairs_var):
# - Comparaciones todas-vs-todas entre variedades (Tukey).
# - Todas las diferencias son altamente significativas (p < 0.0001),
#   lo que refleja un gradiente muy marcado entre variedades.
# - Nota de la salida:
#   “NOTE: Results may be misleading due to involvement in interactions”
#   → recuerda que hay interacción Variedad×Corte, por lo que no debemos
#   quedarnos solo con este ranking marginal; hay que mirar por corte.

# 5.2 Medias marginales por corte (promedio sobre todas las variedades)
emm_corte <- emmeans(mod_lmm, ~ corte)
pairs_corte <- pairs(emm_corte, adjust = "tukey")

print(emm_corte)
print(pairs_corte)

# Interpretación (emm_corte):
# - EMMs de cada momento de corte, promediando sobre variedades.
# - En este ejemplo:
#     30 ≈ 35.1,
#     45 ≈ 32.9,
#     60 ≈ 30.6,
#     75 ≈ 28.1.
# - Muestra el efecto global de atrasar el corte (rendimiento decrece).
#
# Interpretación (pairs_corte):
# - Todas las diferencias entre cortes son significativas (p < 0.0001).
# - De nuevo, son promedios sobre variedades; más fino es ver corte dentro
#   de cada variedad.

# 5.3 Interacción: variedades dentro de cada corte
emm_var_corte <- emmeans(mod_lmm, ~ variedad | corte)
compar_var_dentro_corte <- contrast(emm_var_corte, method = "tukey")

print(emm_var_corte)
print(compar_var_dentro_corte)

# Interpretación (emm_var_corte):
# - Para cada corte (30, 45, 60, 75) lista las EMMs de las 10 variedades.
# - Se aprecia, por ejemplo:
#     corte 30: V1 ≈ 26.3, V10 ≈ 44.6
#     corte 75: V1 ≈ 20.5, V10 ≈ 36.9
# - El patrón: todas las variedades bajan su rendimiento al retrasar el corte,
#   pero V10 siempre está en la parte superior.
#
# Interpretación (compar_var_dentro_corte):
# - Comparaciones Tukey entre variedades, separadas por corte.
# - En este ejemplo, casi todas las parejas son significativas en todos los cortes
#   (diseño muy “didáctico”).
# - En datos reales, esta tabla te permitiría decir:
#   “En el corte 45, V8 y V9 no difieren significativamente, pero ambas
#    superan a V3 y V4”, etc.

# 5.4 Interacción: cortes dentro de cada variedad (opcional)
emm_corte_var <- emmeans(mod_lmm, ~ corte | variedad)
compar_corte_dentro_var <- contrast(emm_corte_var, method = "tukey")

print(emm_corte_var)
print(compar_corte_dentro_var)

# Interpretación (emm_corte_var):
# - Para cada variedad, da las EMMs de los 4 cortes.
# - Ejemplo para V1:
#     30 ≈ 26.3, 45 ≈ 24.7, 60 ≈ 22.0, 75 ≈ 20.5
# - Para V10:
#     30 ≈ 44.6, 45 ≈ 42.1, 60 ≈ 39.6, 75 ≈ 36.9
#
# Interpretación (compar_corte_dentro_var):
# - Compara los 4 cortes dentro de cada variedad.
# - En el dataset de ejemplo, todas las comparaciones corte30–corte75
#   son fuertemente significativas (p < 0.0001), lo que formaliza que
#   retrasar la cosecha reduce el rendimiento para todas las variedades.

# ------------------------------------------------------------
# 6) Gráficos de resultados a partir de EMMs
# ------------------------------------------------------------
# Ahora convertimos las EMMs a data.frame y construimos gráficos más
# “publicables” para comunicar resultados a no estadísticos.

# 6.1 Gráfico de interacción Variedad × Corte (medias ajustadas)
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
    title = "Interacción Variedad × Corte (medias ajustadas)",
    x = "Corte",
    y = "Rendimiento ajustado"
  ) +
  theme(legend.position = "bottom")

print(g_emm_inter)

# Interpretación:
# - Cada línea: una variedad, con su trayectoria de rendimiento según corte.
# - Las barras de error representan ±1 SE (no son IC al 95%, pero dan idea
#   de la incertidumbre).
# - En este ejemplo:
#     * todas las curvas decrecen con el corte.
#     * el orden de las curvas se mantiene (V10 siempre arriba, V1 abajo).
# - En datos reales, buscarías también si hay cruzamientos (interacciones
#   cualitativas: una variedad gana en corte temprano pero pierde en cortes tardíos).

# 6.2 Ranking de variedades por rendimiento medio (EMMs)
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
    title = "Ranking de variedades (medias ajustadas)",
    x = "Variedad",
    y = "Rendimiento ajustado"
  )

print(g_rank_var)

# Interpretación:
# - Barras horizontales ordenadas de menor a mayor rendimiento ajustado.
# - IC al 95% como barras de error.
# - Permite decir:
#   “V10, V9 y V8 son el grupo de mayor rendimiento, claramente separados
#    de V1–V3”, etc.
# - Recuerda que este ranking es promediado sobre cortes; si la interacción
#   fuera fuerte (curvas que se cruzan), sería prudente mostrar también
#   rankings por corte.

# 6.3 Efecto del momento de corte (EMMs)
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
    title = "Efecto del momento de corte (medias ajustadas)",
    x = "Corte",
    y = "Rendimiento ajustado"
  )

print(g_corte)

# Interpretación:
# - Barras de EMM por corte, promediando sobre variedades.
# - Visualiza claramente la pérdida de rendimiento al retrasar el corte.
# - En informes agronómicos, este gráfico es muy intuitivo para discutir
#   ventanas de cosecha óptimas.

# ------------------------------------------------------------
# 7) Exportar tablas clave (opcional)
# ------------------------------------------------------------
# Finalmente, guardamos tablas resumen en CSV para:
# - usarlas en informes, presentaciones o Shiny.
# - Compartir con colegas que usen Excel.

if (!dir.exists("resultados")) dir.create("resultados")

write.csv(
  resumen_trat,
  file = "resumen_tratamientos.csv",
  row.names = FALSE
)
# Contenido:
# - Variedad, Corte, n, media, sd
# - Base para tablas clásicas de informe (Cuadro 1).

write.csv(
  emm_var_df,
  file = "resultados/emm_variedades.csv",
  row.names = FALSE
)
# Contenido:
# - Variedad, emmean, SE, df, lower.CL, upper.CL
# - Ranking de variedades con precisión estadística.

write.csv(
  emm_corte_df,
  file = "resultados/emm_cortes.csv",
  row.names = FALSE
)
# Contenido:
# - Corte, emmean, SE, df, lower.CL, upper.CL
# - Efecto de momento de corte sobre rendimiento promedio.