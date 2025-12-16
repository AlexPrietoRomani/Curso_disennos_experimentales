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
  lme4,        # modelos mixtos lineales (función lmer)
  lmerTest,    # p-valores y ANOVA para lmer (type III, etc.)
  emmeans      # medias marginales estimadas (EMMs) y comparaciones
)

theme_set(theme_bw())   # tema base para ggplot en todo el script
set.seed(123)           # fija la semilla para que la simulación sea reproducible

# ------------------------------------------------------------
# 1) Simular un diseño Fila–Columna (fieldbook)
# ------------------------------------------------------------
# Idea didáctica:
#   - Un solo “campo” organizado como grilla Row × Col.
#   - Cada celda es una parcela con un tratamiento (genotipo).
#   - Vamos a tener REPLICACIÓN de tratamientos (cada genotipo aparece varias veces
#     en diferentes posiciones de la grilla).
#   - Esto nos permite:
#       * Ajustar un modelo CRD (Y ~ Trt).
#       * Ajustar un modelo Row–Column (Y ~ Trt + (1|Row) + (1|Col)).
#       * Comparar ambos (AIC, varianzas, EMMs, etc.).

# Parámetros del diseño
n_rows  <- 4      # nº de filas (Row) en la grilla
n_cols  <- 6      # nº de columnas (Col)
n_cells <- n_rows * n_cols  # nº total de parcelas (24)

n_trts  <- 6      # nº de tratamientos (p.ej. 6 genotipos)
mu      <- 100    # media general del ensayo (nivel base de Y)

# Comprobación: n_cells debe ser múltiplo de n_trts para que el diseño sea balanceado
stopifnot(n_cells %% n_trts == 0)
n_rep_per_trt <- n_cells / n_trts  # aquí será 24 / 6 = 4 repeticiones por tratamiento

# Etiquetas de tratamientos: G01, G02, ..., G06
trt_labels <- paste0("G", sprintf("%02d", seq_len(n_trts)))

# Grilla base Row × Col (estructura espacial)
fieldbook <- expand.grid(
  Row = seq_len(n_rows),
  Col = seq_len(n_cols)
) |>
  arrange(Row, Col)   # ordenamos por fila y columna para ver la grilla ordenada

# Asignar tratamientos con replicación balanceada
# - Cada tratamiento aparece exactamente n_rep_per_trt veces.
# - Luego barajamos aleatoriamente para simular una aleatorización.
fieldbook$Trt <- rep(trt_labels, each = n_rep_per_trt) |>
  sample(size = n_cells, replace = FALSE)

# (Opcional) columna de repetición, aquí solo una "campaña"
fieldbook$Rep <- 1L

# Estructura del fieldbook (lo que verías al correr glimpse y head)
# - 24 filas (una por parcela).
# - Columnas: Row, Col, Trt, Rep.
dplyr::glimpse(fieldbook)
head(fieldbook)

# Comentario para clase:
#   - Row y Col representan la posición física en el campo.
#   - Trt es el genotipo / tratamiento asignado a cada celda.
#   - Rep es una etiqueta de “campaña” o repetición global (aquí siempre 1).

# ------------------------------------------------------------
# 2) Simular gradientes espaciales y respuesta Y
# ------------------------------------------------------------
# Componentes de la simulación de Y:
#   - Efecto de tratamiento (eff_trt): variación verdadera entre genotipos.
#   - Efecto de fila (eff_row): gradiente Norte–Sur (p.ej. suelo más fértil arriba).
#   - Efecto de columna (eff_col): gradiente Este–Oeste (p.ej. riego, pendiente).
#   - Error residual (eps): ruido dentro de parcela (variabilidad no explicada).

sd_trt <- 5    # SD entre tratamientos (señal genética)
sd_row <- 3    # SD gradiente en filas (suave)
sd_col <- 10   # SD gradiente en columnas (más fuerte)
sd_eps <- 4    # SD residual (ruido de parcela)

# Efectos aleatorios simulados para cada tratamiento
eff_trt <- rnorm(n_trts, mean = 0, sd = sd_trt)
names(eff_trt) <- trt_labels   # nombramos cada elemento con su G01, G02, etc.

# Efectos de fila (Row) – gradiente Norte–Sur
eff_row <- rnorm(n_rows, mean = 0, sd = sd_row)
names(eff_row) <- as.character(seq_len(n_rows))

# Efectos de columna (Col) – gradiente Este–Oeste
eff_col <- rnorm(n_cols, mean = 0, sd = sd_col)
names(eff_col) <- as.character(seq_len(n_cols))

# Construir Y para cada celda Row × Col
fieldbook <- fieldbook |>
  mutate(
    eff_Trt = eff_trt[Trt],                # efecto del tratamiento Gxx
    eff_Row = eff_row[as.character(Row)],  # efecto de la fila
    eff_Col = eff_col[as.character(Col)],  # efecto de la columna
    eps     = rnorm(n(), mean = 0, sd = sd_eps),  # ruido residual
    Y       = mu + eff_Trt + eff_Row + eff_Col + eps
  )

dplyr::glimpse(fieldbook)
summary(fieldbook$Y)

# Interpretación típica de summary(Y):
#   Min.     -> el valor mínimo de la respuesta simulada.
#   1st Qu.  -> cuartil 25% (inferior).
#   Median   -> mediana (50% de las parcelas están por debajo).
#   Mean     -> media muestral (debería estar cerca de mu = 100).
#   3rd Qu.  -> cuartil 75% (superior).
#   Max.     -> valor máximo de la respuesta simulada.
#
#   En clase, puedes comentar:
#     - Si la variabilidad es alta o baja en relación a la media.
#     - Cómo esto se relaciona con las SD simuladas (sd_trt, sd_row, sd_col, sd_eps).

# ------------------------------------------------------------
# 3) EDA – Exploración del diseño y del gradiente espacial
# ------------------------------------------------------------

# 3.1 Estructura básica
dplyr::count(fieldbook, Row, Col)  # debería ser 1 parcela por celda
dplyr::count(fieldbook, Trt)       # cada tratamiento debería tener 4 repeticiones

# Interpretación:
#   - count(Row, Col): confirma que no hay celdas duplicadas ni vacías.
#   - count(Trt): revisa que el diseño esté balanceado (mismo n por tratamiento).

# 3.2 Histograma global de Y
g_hist <- ggplot(fieldbook, aes(x = Y)) +
  geom_histogram(bins = 15, colour = "white") +
  labs(
    title = "Distribución global de la respuesta (Y)",
    x = "Y (respuesta simulada)",
    y = "Frecuencia"
  )

print(g_hist)

# Para explicar:
#   - Es un histograma de todos los rendimientos (o variable agronómica Y).
#   - Comentar si la distribución parece aproximadamente normal o si hay sesgos.
#   - Relacionar con el supuesto de normalidad de los residuos en modelos lineales.

# 3.3 Mapa espacial Row × Col
g_spatial <- ggplot(
  fieldbook,
  aes(x = Col, y = Row, fill = Y)
) +
  geom_tile(color = "white") +
  scale_y_reverse() +  # fila 1 arriba (formato “mapa”)
  scale_fill_viridis_c() +
  coord_equal() +
  labs(
    title = "Mapa espacial de Y en la grilla Fila × Columna",
    x = "Columna (Este–Oeste)",
    y = "Fila (Norte–Sur)"
  ) +
  theme_minimal(base_size = 12)

print(g_spatial)

# Interpretación del mapa:
#   - Cada “tile” es una parcela.
#   - Colores más claros/oscursos reflejan valores mayores/menores de Y.
#   - Permite visualizar gradientes espaciales: por ejemplo,
#       * columnas con valores sistemáticamente mayores (efecto de Col),
#       * filas con valores diferentes (efecto de Row).
#   - Esta visualización motiva el uso de efectos aleatorios Row y Col en el LMM.

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

# Cómo usar este boxplot en clase:
#   - Comparar medianas y rangos intercuartílicos entre tratamientos.
#   - Comentar que, en un CRD clásico, el modelo Y ~ Trt asumiría que
#     cualquier patrón espacial es “ruido aleatorio” (parte del error residual).
#   - Esto es precisamente lo que mejoramos con el modelo Row–Column.

# ------------------------------------------------------------
# 4) Modelo ingenuo vs. Modelo Row–Column (LMM)
# ------------------------------------------------------------
# Modelo 1: ingenuo (CRD), ignora filas y columnas
#   Y ~ Trt
#   Útil como punto de comparación: trata el campo como homogéneo.
#
# Modelo 2: Row–Column LMM (filas y columnas como bloqueos espaciales)
#   Y ~ Trt + (1|Row) + (1|Col)
#   Captura la variación de gradiente en dos direcciones:
#     - efecto aleatorio de fila (Row),
#     - efecto aleatorio de columna (Col).

# Aseguramos que las variables de diseño sean factores
datos <- fieldbook |>
  mutate(
    Row = factor(Row),
    Col = factor(Col),
    Trt = factor(Trt),
    Rep = factor(Rep)
  )

# ---------- Modelo 1: CRD / “naive” ----------
mod_crd <- lm(Y ~ Trt, data = datos)

cat("\nANOVA modelo CRD (ingenuo, sin filas/columnas):\n")
print(anova(mod_crd))

# Interpretación ANOVA CRD:
#   - Fila “Trt”: prueba H0: todas las medias de tratamientos son iguales
#     vs H1: al menos un tratamiento difiere.
#   - Columnas:
#       Df      -> grados de libertad de Trt y del residual.
#       Sum Sq  -> suma de cuadrados explicada por Trt y residual.
#       Mean Sq -> Sum Sq / Df.
#       F value -> razón F = MS(Trt) / MS(Residual).
#       Pr(>F)  -> p-valor del test. Si es bajo (p < 0.05), hay evidencia
#                 de diferencias entre tratamientos en este modelo ingenuo.

cat("\nResumen modelo CRD:\n")
print(summary(mod_crd))

# Puntos clave del summary(mod_crd):
#   - Residuals: distribución de los residuos (mínimo, cuartiles, máximo).
#   - Coefficients:
#       (Intercept) -> media estimada del tratamiento de referencia (primer nivel de Trt).
#       TrtG02, TrtG03, ... -> diferencia estimada respecto al tratamiento de referencia.
#       Std. Error -> error estándar de cada estimador.
#       t value    -> estadístico t (Estimate / Std. Error).
#       Pr(>|t|)   -> p-valor de cada contraste vs tratamiento de referencia.
#   - R-squared: proporción de variabilidad explicada por el factor Trt
#                bajo el modelo CRD.
#   - Residual standard error: desviación estándar de los residuos (σ̂ en este modelo).

# ---------- Modelo 2: Row–Column LMM ----------
mod_rc <- lme4::lmer(
  Y ~ Trt + (1 | Row) + (1 | Col),
  data    = datos,
  REML    = TRUE,
  control = lme4::lmerControl(
    optimizer = "bobyqa",
    optCtrl   = list(maxfun = 1e5)
  )
)

cat("\nResumen modelo Row–Column (LMM):\n")
print(summary(mod_rc))

# Cómo leer summary(mod_rc):
#   - "Random effects":
#       Groups   Name        Variance Std.Dev.
#       Col      (Intercept) 70.33    8.39   -> Varianza entre columnas (efecto Este–Oeste).
#       Row      (Intercept)  6.95    2.64   -> Varianza entre filas (efecto Norte–Sur).
#       Residual             31.49    5.61   -> Varianza residual (dentro de celda).
#     Interpretación:
#       * La variación por columnas es la mayor en este ejemplo (más fuerte gradiente EN–EO).
#       * La variación entre filas también existe, pero menor.
#       * El residual es lo que queda después de descontar Trt, Row y Col.
#
#   - "Fixed effects":
#       (Intercept) -> media esperada del tratamiento de referencia (G01) en la grilla,
#                     después de ajustar por filas y columnas.
#       TrtG02, TrtG03, ... -> diferencias promedio (ajustadas) respecto a G01.
#       t value -> cuántas veces el efecto está por encima de su error estándar;
#                  valores grandes en módulo sugieren diferencias importantes.

cat("\nANOVA tipo III para tratamientos (via lmerTest):\n")
print(anova(mod_rc, type = 3))

# Nota:
#   - anova(mod_rc, type = 3) (de lmerTest) da una tabla de ANOVA para los efectos fijos.
#   - Aquí vemos una fila “Trt” con:
#       npar   -> número de parámetros asociados a Trt.
#       Sum Sq -> suma de cuadrados asociada a Trt en el modelo mixto.
#       Mean Sq-> Sum Sq / npar.
#       F value-> estadístico F para el efecto global de Trt.
#   - Es un test global del efecto de tratamientos, ahora
#     *ajustando por la estructura Row–Column*.

# Comparación informal vía AIC
cat("\nComparación AIC (CRD vs Row–Column):\n")
print(AIC(mod_crd, mod_rc))

# Interpretación del AIC:
#   - AIC más bajo indica mejor ajuste “penalizado” por complejidad.
#   - Si AIC(mod_rc) << AIC(mod_crd), el modelo Row–Column se ajusta
#     mejor a los datos que el CRD ingenuo, justificando el uso
#     de los efectos aleatorios de Row y Col.
#   - Aquí se observa que el AIC del modelo Row–Column es claramente
#     menor que el del CRD, lo cual respalda el modelo con bloqueo espacial.

# ------------------------------------------------------------
# 5) Componentes de varianza (VarCorr) – interpretación
# ------------------------------------------------------------

cat("\nComponentes de varianza (modelo Row–Column):\n")
vc <- VarCorr(mod_rc)
print(vc, comp = c("Variance", "Std.Dev."))

# Comentario para clase:
#   - Var(Col): magnitud de la variación entre columnas.
#   - Var(Row): magnitud de la variación entre filas.
#   - Var(Residual): variación dentro de celda Row × Col × Trt.
#   - Si Var(Col) y/o Var(Row) son grandes comparadas con Var(Residual),
#     significa que hay heterogeneidad espacial importante y que el
#     modelado Row–Column es muy relevante.
#   - Esto conecta con el mapa espacial: patrones “en bandas” que se ven
#     reflejados en las varianzas de Row y Col.

# ------------------------------------------------------------
# 6) Medias ajustadas por tratamiento (EMMs) y gráficos
# ------------------------------------------------------------
# Aquí pasamos a la parte de interpretación agronómica:
#   - Medias ajustadas por tratamiento (EMMs) bajo el modelo Row–Column.
#   - Comparaciones múltiples (Tukey).
#   - Gráfico de ranking de tratamientos.
#   - Gráfico Observado vs Ajustado para ver calidad de ajuste.

emm_trt <- emmeans(mod_rc, ~ Trt)
emm_trt_df <- as.data.frame(emm_trt)

cat("\nMedias marginales estimadas por tratamiento (EMMs):\n")
print(emm_trt)

# Lectura de la tabla de EMMs:
#   - Trt      -> nombre del tratamiento (G01, G02, …).
#   - emmean   -> media ajustada (esperanza de Y) para ese tratamiento,
#                 ajustada por los efectos aleatorios Row y Col.
#   - SE       -> error estándar de la media ajustada.
#   - df       -> grados de libertad (Kenward–Roger en este ejemplo).
#   - lower.CL, upper.CL -> intervalo de confianza al 95% para la media ajustada.
#
#   Ejemplo de interpretación:
#     “G01 presenta un emmean de ~108.5, con IC95% [97.2, 119.7],
#      mientras que G03 tiene un emmean de ~83.5, claramente menor;
#      esto sugiere que G01 supera a G03 en la respuesta Y
#      después de ajustar por gradientes espaciales”.

cat("\nComparaciones múltiples (Tukey) entre tratamientos:\n")
compar_trt <- contrast(emm_trt, method = "tukey")
print(compar_trt)

# Cómo leer las comparaciones Tukey:
#   - Cada fila “G01 - G02”, “G01 - G03”, etc. es una comparación par a par.
#   - estimate -> diferencia de medias ajustadas (emmean_G01 - emmean_G02).
#   - SE       -> error estándar de esa diferencia.
#   - df       -> grados de libertad.
#   - t.ratio  -> estadístico t = estimate / SE.
#   - p.value  -> p-valor ajustado por método de Tukey (control de familia).
#
#   Ejemplo:
#     “G01 - G03 estimate = 24.95, p.value = 0.0021”:
#       G01 está ~25 unidades por encima de G03, y la diferencia es significativa
#       incluso tras corrección por comparaciones múltiples.
#     Esto te permite decir, en lenguaje agronómico:
#       “El genotipo G01 supera claramente a G03 en rendimiento ajustado”.

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

# Uso del gráfico de ranking:
#   - Ordena los tratamientos de menor a mayor media ajustada.
#   - Las barras muestran emmean; las “T” verticales son los IC95%.
#   - Permite discutir:
#       * qué tratamientos están claramente por encima o por debajo de otros,
#       * si hay solapamiento de intervalos (diferencias menos claras).

# 6.2 Comparar observados vs ajustados (scatter)
datos_pred <- datos |>
  mutate(
    fitted_rc = fitted(mod_rc)  # valores ajustados por el modelo Row–Column
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

# Interpretación del gráfico Observado vs Ajustado:
#   - Si los puntos se alinean cercanos a la diagonal (línea punteada),
#     el modelo está capturando bien la estructura de los datos.
#   - Grandes desviaciones sistemáticas de la diagonal podrían indicar
#     falta de ajuste o estructura no capturada.
#   - Es una forma visual de ver la “calidad de predicción” del modelo
#     Row–Column sobre las parcelas observadas.

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