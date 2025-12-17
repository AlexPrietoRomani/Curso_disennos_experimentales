############################################################
# Sesión 9 – ANCOVA (script pedagógico)
# 
# Objetivo:
#   - Entender qué hace la ANCOVA en comparación con un ANOVA simple.
#   - Ver en datos simulados cómo la covariable reduce el ruido
#     y ajusta las medias de tratamiento.
#   - Practicar el chequeo de homogeneidad de pendientes (Trat×X).
#   - Extender la idea a un RCBD con covariable.
#
# Nota:
#   - anova(lm) en R entrega SS secuenciales (Type I): el orden de términos
#     en la fórmula puede cambiar las SS de cada factor (diseños no ortogonales).
#   - En este script mantenemos tu orden original para no cambiar nada del flujo.
#   - En comentarios te indico cómo “leer” la tabla bajo ese comportamiento.
############################################################

# ----------------------------------------------------------
# 0. Paquetes y opciones generales
# ----------------------------------------------------------

# Instalar (si hace falta) y cargar paquetes básicos
pkgs <- c("ggplot2", "dplyr", "broom")
to_install <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(to_install) > 0) {
  install.packages(to_install)
}

library(ggplot2)
library(dplyr)
library(broom)

set.seed(123)  # para reproducibilidad en toda la sesión

# ----------------------------------------------------------
# 1. Introducción didáctica (salida en consola)
# ----------------------------------------------------------

cat("============================================================\n")
cat("Sesión 9 – ANCOVA: introducción práctica\n")
cat("============================================================\n\n")

cat(
  "Idea central:\n",
  "  · La ANCOVA combina un factor de tratamiento (ANOVA)\n",
  "    con una covariable continua (regresión) para explicar Y.\n",
  "  · Modelo típico (sin bloque):  Y_ij = μ + τ_i + β (X_ij - X̄..) + ε_ij.\n",
  "  · β ajusta la respuesta por diferencias en la covariable X.\n\n",
  sep = ""
)

cat(
  "Preguntas guía:\n",
  "  1) ¿Cambia la conclusión sobre Trat si ajusto por X?\n",
  "  2) ¿Cuánto se reduce el MS_residual al incluir X?\n",
  "  3) ¿Las pendientes Y–X son similares entre tratamientos\n",
  "     (homogeneidad de pendientes) o no?\n\n",
  sep = ""
)

# ----------------------------------------------------------
# 2. EJEMPLO 1: ANCOVA en un diseño completamente al azar (CRD)
# ----------------------------------------------------------
# Diseño:
#   - k tratamientos
#   - n_per unidades por tratamiento
#   - Covariable X continua (ej: vigor inicial)
#
# Modelo de simulación:
#   Y = μ + τ(trat) + β*(X - mean(X)) + error
#
# Intuición agronómica/experimental:
#   - X captura una fuente de variación “pre-existente” (vigor, tamaño inicial, etc.)
#   - Si X explica parte de Y, entonces ajustar por X:
#       (i) reduce varianza residual,
#      (ii) mejora precisión (SE más pequeños),
#     (iii) aumenta potencia para detectar diferencias de tratamientos.
# ----------------------------------------------------------

# Parámetros "verdaderos" de la simulación
k_trat      <- 3      # Nº tratamientos
n_per_trat  <- 25     # Nº unidades por tratamiento
mu_true     <- 100    # media general
tau_amp     <- 8      # amplitud ± para efectos de tratamiento
beta_true   <- 1.5    # pendiente verdadera de la covariable
mu_x_true   <- 50     # media de X
sd_x_true   <- 5      # desviación estándar de X
sigma_e     <- 5      # desviación estándar residual

# 2.1 Definimos niveles de tratamiento y efectos verdaderos
trat_levels <- paste0("T", seq_len(k_trat))

tau_vec <- runif(k_trat, -tau_amp, tau_amp)  # efectos de tratamiento
names(tau_vec) <- trat_levels

tau_vec
# Interpretación (tau_vec):
#   - Son diferencias “reales” de tratamiento alrededor de la media mu_true.
#   - En un ANOVA/ANCOVA, esto es lo que intentamos detectar con evidencia estadística.

# 2.2 Construimos el data frame simulado
sim_CRD <- lapply(trat_levels, function(tr_i) {
  # Covariable X ~ Normal(mu_x_true, sd_x_true)
  X_i <- rnorm(n_per_trat, mean = mu_x_true, sd = sd_x_true)
  data.frame(
    trat = tr_i,
    X    = X_i,
    stringsAsFactors = FALSE
  )
}) |>
  bind_rows()

sim_CRD$trat <- factor(sim_CRD$trat, levels = trat_levels)

# Centramos la covariable (X - X̄..)
# Nota docente:
#   - Centrar X ayuda a interpretar el intercepto (la media ajustada a X = X̄),
#     pero NO cambia la pendiente estimada ni las comparaciones entre tratamientos.
X_centered <- sim_CRD$X - mean(sim_CRD$X)

# Error aleatorio
error_vec <- rnorm(nrow(sim_CRD), mean = 0, sd = sigma_e)

# Generamos Y según el modelo verdadero
sim_CRD$Y <- mu_true +
  tau_vec[as.character(sim_CRD$trat)] +
  beta_true * X_centered +
  error_vec

# Resumen rápido
cat("------------------------------------------------------------\n")
cat("EJEMPLO 1 – Datos simulados (CRD con covariable)\n")
cat("------------------------------------------------------------\n")
cat("Nº tratamientos =", k_trat, "\n")
cat("Nº observaciones totales =", nrow(sim_CRD), "\n\n")

head(sim_CRD)

# ----------------------------------------------------------
# 2.3 Gráfico: Y vs X coloreado por tratamiento
# ----------------------------------------------------------
# Qué mirar en este scatter:
#   1) Tendencia global creciente/decreciente: evidencia visual de β ≠ 0.
#   2) Si las “nubes” de puntos de cada tratamiento parecen paralelas:
#        - sugiere homogeneidad de pendientes (pendiente común).
#   3) Desplazamientos verticales entre tratamientos (a igual X):
#        - sugiere efecto de tratamiento después de ajustar por X.
#   4) Solapamiento fuerte entre colores:
#        - puede indicar señal de tratamiento débil vs ruido.
p_scatter_CRD <- ggplot(sim_CRD, aes(x = X, y = Y, colour = trat)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "CRD simulado: Y vs X por tratamiento",
    x = "Covariable X (ej. vigor inicial)",
    y = "Respuesta Y",
    colour = "Trat"
  ) +
  theme_minimal(base_size = 12)

print(p_scatter_CRD)

# ----------------------------------------------------------
# 2.4 ANOVA vs ANCOVA (modelo aditivo)
# ----------------------------------------------------------
# Modelo 1 (ANOVA):     Y ~ trat
# Modelo 2 (ANCOVA):    Y ~ trat + X
#
# Cómo leer la tabla de ANOVA (columnas):
#   - Df: grados de libertad del término.
#   - Sum Sq: suma de cuadrados asociada al término (según Type I en R).
#   - Mean Sq: Sum Sq / Df.
#   - F value: Mean Sq (término) / Mean Sq (residual).
#   - Pr(>F): p-valor del test H0: “ese término no aporta” (en el sentido del modelo).
#
# OJO docente (Type I):
#   - En anova(mod_anc_CRD) con fórmula Y ~ trat + X:
#       * "trat" se evalúa ANTES que X (no ajustado por X en SS del numerador).
#       * "X" se evalúa DESPUÉS de trat (X ajustado por trat).
#   - Aun así, el MSE residual del modelo con X suele bajar, y eso cambia F.
#   - Si tu objetivo inferencial es “trat ajustado por X” en SS parciales,
#     típicamente se usa Type II/III (p.ej. car::Anova) o se reordena (Y ~ X + trat).
#     (Aquí NO lo cambiamos, solo lo explicamos.)
#
# Modelo 1: ANOVA simple (sin covariable)
mod_aov_CRD <- lm(Y ~ trat, data = sim_CRD)

# Modelo 2: ANCOVA aditiva (trat + X)
mod_anc_CRD <- lm(Y ~ trat + X, data = sim_CRD)

# MS_residual de cada modelo
# Interpretación:
#   - MS_residual (MSE) ≈ estimación de σ² (variabilidad “no explicada” por el modelo).
#   - ANCOVA suele reducir MSE si X explica una fracción importante de Y.
ms_resid_aov_CRD <- sum(residuals(mod_aov_CRD)^2) / df.residual(mod_aov_CRD)
ms_resid_anc_CRD <- sum(residuals(mod_anc_CRD)^2) / df.residual(mod_anc_CRD)

cat("============================================================\n")
cat("EJEMPLO 1 – Comparación ANOVA vs ANCOVA (CRD)\n")
cat("============================================================\n\n")

cat("Modelo 1: Y ~ trat  (ANOVA simple)\n")
print(anova(mod_aov_CRD))
# Cómo interpretar tu salida observada (ejemplo con set.seed(123)):
#   - trat: p ~ 0.036 -> evidencia de diferencias entre tratamientos (sin ajustar por X).
#   - Residuals: MSE ~ 74.1 -> ruido relativamente alto.
cat("\nMS_residual (modelo 1) =", round(ms_resid_aov_CRD, 3), "\n\n")

cat("Modelo 2: Y ~ trat + X  (ANCOVA aditiva)\n")
print(anova(mod_anc_CRD))
# Cómo interpretar tu salida observada:
#   - X: p < 2e-16 -> X explica MUCHA variación de Y (β claramente ≠ 0).
#   - Residuals: MSE ~ 19.8 (bajó desde ~74.1) -> el “ruido” bajó mucho.
#     Esto implica:
#       * mayor precisión,
#       * intervalos más estrechos,
#       * tests de tratamiento potencialmente más potentes.
cat("\nMS_residual (modelo 2) =", round(ms_resid_anc_CRD, 3), "\n")
cat(
  "Reducción relativa de MS_residual:",
  round(100 * (ms_resid_aov_CRD - ms_resid_anc_CRD) / ms_resid_aov_CRD, 1),
  "%\n\n"
)

# Pendiente estimada β y su IC
coefs_anc <- coef(mod_anc_CRD)
beta_hat_CRD <- coefs_anc["X"]
ci_beta_CRD  <- confint(mod_anc_CRD, "X", level = 0.95)

cat("Pendiente verdadera β =", beta_true, "\n")
cat(
  "β_hat (modelo 2)      =", round(beta_hat_CRD, 3),
  " | IC 95% = [", round(ci_beta_CRD[1], 3), ",", round(ci_beta_CRD[2], 3), "]\n\n"
)
# Interpretación de β y su IC:
#   - β_hat es el cambio esperado en Y por 1 unidad de X (manteniendo constante trat).
#   - Si el IC 95% NO incluye 0 -> evidencia de asociación lineal entre Y y X.
#   - En tu salida: β_hat ~ 1.45 y el IC cubre ~1.5 -> recupera bien el valor verdadero.

# ----------------------------------------------------------
# 2.5 Test de homogeneidad de pendientes (Trat × X)
# ----------------------------------------------------------
# Objetivo:
#   - Verificar si es razonable suponer una pendiente común β para todos los tratamientos.
#
# Modelos:
#   - Aditivo (pendiente común):          Y ~ trat + X
#   - Interacción (pendientes distintas): Y ~ trat * X  (incluye trat:X)
#
# Test:
#   - H0: NO interacción -> pendientes iguales entre tratamientos.
#   - Si p grande: no hay evidencia fuerte contra H0 -> usar modelo aditivo es razonable.
#   - Si p pequeño: pendientes difieren -> interpretar interacción (ANCOVA con pendientes distintas).
mod_add_CRD <- lm(Y ~ trat + X,        data = sim_CRD)  # pendiente común
mod_int_CRD <- lm(Y ~ trat * X,        data = sim_CRD)  # pendientes específicas

cat("------------------------------------------------------------\n")
cat("Chequeo de homogeneidad de pendientes (CRD)\n")
cat("------------------------------------------------------------\n\n")
anova_slopes <- anova(mod_add_CRD, mod_int_CRD)
print(anova_slopes)

cat(
  "\nInterpretación guía:\n",
  "  - H0: las pendientes Y–X son iguales en todos los tratamientos\n",
  "  - Si p-valor (fila 2) es grande → NO hay evidencia fuerte contra H0\n",
  "    → podemos usar ANCOVA con pendiente común.\n",
  "  - Si p-valor es pequeño → las pendientes difieren por tratamiento, conviene\n",
  "    interpretar el modelo con interacción (Trat×X).\n\n",
  sep = ""
)
# En tu salida: p ~ 0.56 -> claramente grande -> asumimos pendientes paralelas (OK).

# ----------------------------------------------------------
# 2.6 Gráfico de rectas de ANCOVA (pendiente común por tratamiento)
# ----------------------------------------------------------
# Qué mirar:
#   1) Rectas aproximadamente paralelas (consistente con test de interacción no significativo).
#   2) Distancias verticales entre rectas: representan diferencias de tratamientos “ajustadas por X”.
#   3) Si una recta está consistentemente arriba: tratamiento con mayor Y esperado para cualquier X.
#
# Generamos predicciones con el modelo aditivo (pendiente común)
x_seq <- seq(min(sim_CRD$X), max(sim_CRD$X), length.out = 50)

pred_CRD <- expand.grid(
  trat = levels(sim_CRD$trat),
  X    = x_seq
)

pred_CRD$Y_hat <- predict(mod_add_CRD, newdata = pred_CRD)

p_rectas_CRD <- ggplot() +
  geom_point(
    data = sim_CRD,
    aes(x = X, y = Y, colour = trat),
    alpha = 0.6
  ) +
  geom_line(
    data = pred_CRD,
    aes(x = X, y = Y_hat, colour = trat),
    linewidth = 1
  ) +
  labs(
    title = "CRD: rectas de ANCOVA (pendiente común) por tratamiento",
    x = "Covariable X",
    y = "Respuesta Y",
    colour = "Trat"
  ) +
  theme_minimal(base_size = 12)

print(p_rectas_CRD)

# ----------------------------------------------------------
# 3. EJEMPLO 2: ANCOVA en un RCBD con covariable
# ----------------------------------------------------------
# Contexto:
#   - 3 tratamientos de riego
#   - 4 bloques
#   - Covariable X_cov (vigor inicial)
#   - Respuesta Y (rendimiento kg/ha)
#
# Modelo “conceptual”:
#   Y = μ + τ(trat) + Block(block) + β*(X_cov - mean(X_cov)) + error
#
# Nota docente:
#   - En RCBD, el bloque captura heterogeneidad espacial/ambiental “grande”.
#   - X_cov captura variabilidad continua (dentro de bloque y/o entre bloques).
#   - ANCOVA en RCBD suele mejorar precisión si X_cov explica rendimiento.
# ----------------------------------------------------------

bloques <- factor(1:4)
trat_riego <- factor(
  c("Riego_bajo", "Riego_medio", "Riego_alto"),
  levels = c("Riego_bajo", "Riego_medio", "Riego_alto")
)

# Diseño RCBD: todas las combinaciones block × trat
diseno_RCBD <- expand.grid(
  block = bloques,
  trat  = trat_riego
)

n_RCBD <- nrow(diseno_RCBD)

# Efecto de bloque sobre la covariable X (por ejemplo, leves diferencias de vigor medio entre bloques)
ef_bloque_X <- rnorm(length(bloques), mean = 0, sd = 2)
names(ef_bloque_X) <- levels(bloques)

# Generamos X_cov por parcela
X_cov_RCBD <- 50 +
  ef_bloque_X[as.character(diseno_RCBD$block)] +
  rnorm(n_RCBD, mean = 0, sd = 3)

# Efectos de tratamiento sobre Y (rendimiento)
ef_trat_Y <- c(
  "Riego_bajo"  = -600,  # rinde algo menos
  "Riego_medio" =    0,  # referencia
  "Riego_alto"  =  500   # rinde más
)

# Efecto de bloque sobre Y (gradiente de suelo, etc.)
ef_bloque_Y <- rnorm(length(bloques), mean = 0, sd = 300)
names(ef_bloque_Y) <- levels(bloques)

beta_RCBD      <- 80    # kg/ha por unidad de X_cov
media_global_Y <- 9000  # kg/ha

Y_RCBD <- media_global_Y +
  ef_trat_Y[as.character(diseno_RCBD$trat)] +
  beta_RCBD * (X_cov_RCBD - mean(X_cov_RCBD)) +
  ef_bloque_Y[as.character(diseno_RCBD$block)] +
  rnorm(n_RCBD, mean = 0, sd = 250)

datos_RCBD <- data.frame(
  block = diseno_RCBD$block,
  trat  = diseno_RCBD$trat,
  X_cov = X_cov_RCBD,
  Y     = Y_RCBD
)

cat("============================================================\n")
cat("EJEMPLO 2 – RCBD con covariable (riego en arándanos)\n")
cat("============================================================\n\n")

head(datos_RCBD)

# ----------------------------------------------------------
# 3.1 Gráficos exploratorios (RCBD)
# ----------------------------------------------------------

# Dispersión Y vs X_cov por tratamiento y bloque
# Qué mirar:
#   1) Pendiente positiva global: sugiere β > 0 (vigor inicial ayuda al rendimiento).
#   2) Diferencias verticales entre colores: sugiere efecto de riego a igual vigor.
#   3) Patrones por forma (bloque): bloques con puntos sistemáticamente arriba/abajo
#      indican efecto de bloque importante (heterogeneidad ambiental).
p_scatter_RCBD <- ggplot(datos_RCBD, aes(x = X_cov, y = Y, colour = trat, shape = block)) +
  geom_point(size = 3) +
  labs(
    title = "RCBD simulado: Y vs X_cov por tratamiento y bloque",
    x = "Covariable X_cov (vigor inicial)",
    y = "Y (rendimiento kg/ha)",
    colour = "Tratamiento",
    shape = "Bloque"
  ) +
  theme_minimal(base_size = 12)

print(p_scatter_RCBD)

# Distribución de X_cov por tratamiento
# Por qué importa:
#   - Idealmente, X_cov debería estar “razonablemente balanceada” entre tratamientos
#     (producto de la aleatorización).
#   - Si un tratamiento tiene X_cov sistemáticamente mayor/menor, el ANOVA sin ANCOVA
#     puede atribuir a tratamiento lo que en realidad es covariable.
p_box_RCBD <- ggplot(datos_RCBD, aes(x = trat, y = X_cov, fill = trat)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.7, size = 2, colour = "black") +
  labs(
    title = "RCBD: distribución de la covariable X_cov por tratamiento",
    x = "Tratamiento",
    y = "X_cov (vigor inicial)",
    fill = "Tratamiento"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

print(p_box_RCBD)

# ----------------------------------------------------------
# 3.2 ANOVA clásico (con bloque) vs ANCOVA (con bloque + covariable)
# ----------------------------------------------------------
# Modelo A (ANOVA RCBD):     Y ~ block + trat
# Modelo B (ANCOVA RCBD):    Y ~ block + trat + X_cov
#
# Lectura de la tabla:
#   - "block": compara variación entre bloques vs residual (¿hay heterogeneidad por bloque?).
#   - "trat":  evidencia de diferencias de tratamientos (bajo la estructura del modelo).
#   - "X_cov": evidencia de relación lineal entre vigor y rendimiento, controlando block y trat.
#
# En tu salida observada:
#   - En Modelo A: trat no fue significativo (p ~ 0.12) y MSE alto (~305k).
#   - En Modelo B: al incluir X_cov:
#       * MSE cae fuerte (~101k): gran parte del “ruido” era explicado por vigor.
#       * trat se vuelve significativo (p ~ 0.021): aumenta potencia al bajar el MSE.
#
# Nota docente (Type I):
#   - En anova(mod_anc_RCBD) con fórmula Y ~ block + trat + X_cov:
#       * block entra primero (SS de block “sin ajustar” por trat ni X_cov).
#       * trat entra segundo (SS de trat ajustada por block, pero no por X_cov en SS del numerador).
#       * X_cov entra tercero (SS de X_cov ajustada por block + trat).
#   - El MSE residual corresponde al modelo completo (incluyendo X_cov).
mod_aov_RCBD <- lm(Y ~ block + trat, data = datos_RCBD)
mod_anc_RCBD <- lm(Y ~ block + trat + X_cov, data = datos_RCBD)

ms_resid_aov_RCBD <- sum(residuals(mod_aov_RCBD)^2) / df.residual(mod_aov_RCBD)
ms_resid_anc_RCBD <- sum(residuals(mod_anc_RCBD)^2) / df.residual(mod_anc_RCBD)

cat("------------------------------------------------------------\n")
cat("RCBD – Comparación ANOVA vs ANCOVA (bloque + covariable)\n")
cat("------------------------------------------------------------\n\n")

cat("Modelo A: Y ~ block + trat  (ANOVA RCBD clásico)\n")
print(anova(mod_aov_RCBD))
cat("\nMS_residual (Modelo A) =", round(ms_resid_aov_RCBD, 1), "\n\n")

cat("Modelo B: Y ~ block + trat + X_cov  (ANCOVA)\n")
print(anova(mod_anc_RCBD))
cat("\nMS_residual (Modelo B) =", round(ms_resid_anc_RCBD, 1), "\n")
cat(
  "Reducción relativa de MS_residual:",
  round(100 * (ms_resid_aov_RCBD - ms_resid_anc_RCBD) / ms_resid_aov_RCBD, 1),
  "%\n\n"
)

# Pendiente estimada β en el RCBD
coefs_RCBD <- coef(mod_anc_RCBD)
beta_hat_RCBD <- coefs_RCBD["X_cov"]
ci_beta_RCBD  <- confint(mod_anc_RCBD, "X_cov", level = 0.95)

cat("Pendiente verdadera β (RCBD) =", beta_RCBD, "kg/ha por unidad de X_cov\n")
cat(
  "β_hat (modelo B)           =", round(beta_hat_RCBD, 2),
  " | IC 95% = [", round(ci_beta_RCBD[1], 2), ",", round(ci_beta_RCBD[2], 2), "]\n\n"
)
# Interpretación del β_hat en RCBD:
#   - Es el incremento esperado en rendimiento por 1 unidad de vigor,
#     manteniendo constante el bloque y el tratamiento.
#   - En tu salida: β_hat ~ 120.5, IC amplio (n pequeño: solo 12 parcelas totales),
#     pero el IC no incluye 0 => evidencia de relación vigor-rendimiento.

# ----------------------------------------------------------
# 3.3 Mini-diagnóstico de residuos en el RCBD (modelo B)
# ----------------------------------------------------------
# Qué buscamos con estos gráficos:
#   1) Residuos vs Ajustados: ausencia de patrón (nube aleatoria alrededor de 0)
#      sugiere varianza aproximadamente constante y forma funcional razonable.
#   2) Residuos vs X_cov: si queda patrón, puede indicar no linealidad (faltó curvatura),
#      o que la relación no es bien capturada por un término lineal.
resid_RCBD <- augment(mod_anc_RCBD)

p_resid_fitted <- ggplot(resid_RCBD, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "RCBD – Residuos vs Predicciones (modelo ANCOVA)",
    x = "Valores ajustados",
    y = "Residuos"
  ) +
  theme_minimal(base_size = 12)

print(p_resid_fitted)

p_resid_X <- ggplot(resid_RCBD, aes(x = X_cov, y = .resid)) +
  geom_point(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "RCBD – Residuos vs X_cov (modelo ANCOVA)",
    x = "X_cov",
    y = "Residuos"
  ) +
  theme_minimal(base_size = 12)

print(p_resid_X)

