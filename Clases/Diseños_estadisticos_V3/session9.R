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
#   - k tratamientos
#   - n_per unidades por tratamiento
#   - Una covariable X continua (ex: vigor inicial)
#   - Y generada según: Y = μ + τ_i + β (X - X̄..) + ε
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

# Modelo 1: ANOVA simple (sin covariable)
mod_aov_CRD <- lm(Y ~ trat, data = sim_CRD)

# Modelo 2: ANCOVA aditiva (trat + X)
mod_anc_CRD <- lm(Y ~ trat + X, data = sim_CRD)

# MS_residual de cada modelo
ms_resid_aov_CRD <- sum(residuals(mod_aov_CRD)^2) / df.residual(mod_aov_CRD)
ms_resid_anc_CRD <- sum(residuals(mod_anc_CRD)^2) / df.residual(mod_anc_CRD)

cat("============================================================\n")
cat("EJEMPLO 1 – Comparación ANOVA vs ANCOVA (CRD)\n")
cat("============================================================\n\n")

cat("Modelo 1: Y ~ trat  (ANOVA simple)\n")
print(anova(mod_aov_CRD))
cat("\nMS_residual (modelo 1) =", round(ms_resid_aov_CRD, 3), "\n\n")

cat("Modelo 2: Y ~ trat + X  (ANCOVA aditiva)\n")
print(anova(mod_anc_CRD))
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

# ----------------------------------------------------------
# 2.5 Test de homogeneidad de pendientes (Trat × X)
# ----------------------------------------------------------

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

# ----------------------------------------------------------
# 2.6 Gráfico de rectas de ANCOVA (pendiente común por tratamiento)
# ----------------------------------------------------------

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
#   - 3 tratamientos de riego: Riego_bajo, Riego_medio, Riego_alto
#   - 4 bloques (block = 1, 2, 3, 4)
#   - Covariable X_cov = vigor inicial
#   - Respuesta Y = rendimiento (kg/ha)
#   - Modelo verdadero: Y = μ + τ_trat + β (X_cov - X̄..) + B_block + ε
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

# Modelo A: ANOVA clásico RCBD (sin covariable)
mod_aov_RCBD <- lm(Y ~ block + trat, data = datos_RCBD)

# Modelo B: ANCOVA fijo con bloque + covariable
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

# ----------------------------------------------------------
# 3.3 Mini-diagnóstico de residuos en el RCBD (modelo B)
# ----------------------------------------------------------

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

