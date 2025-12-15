# R/scripts/Diseños_estadisticos_V3/Parte_III/session11_MANCOVA_demo.R
# Sesión 11 (MANCOVA) – Script didáctico independiente
# ------------------------------------------------------
# Objetivo:
#   - Simular un experimento con:
#       * g tratamientos (variedades)
#       * una covariable continua X (ej. vigor inicial)
#       * 3 respuestas continuas (Y1, Y2, Y3) correlacionadas
#   - Comparar:
#       * MANOVA sin covariable:           cbind(Y1,Y2,Y3) ~ trat
#       * MANCOVA aditiva (Trat + X):      cbind(Y1,Y2,Y3) ~ trat + X
#       * MANCOVA con interacción Trat*X:  cbind(Y1,Y2,Y3) ~ trat * X
#   - Mostrar:
#       * Correlaciones entre respuestas y con X
#       * Resultados de MANOVA/MANCOVA (Pillai)
#       * Gráfico Y1 ~ X por tratamiento (pendientes aproxim. paralelas)

# ------------------------------------------------------
# 0) Paquetes necesarios
# ------------------------------------------------------
has_pkg <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message("Instala el paquete '", p, "' para aprovechar todas las funciones.")
    return(FALSE)
  }
  TRUE
}

has_pkg("ggplot2")

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Este script requiere el paquete 'ggplot2'. Instálalo e inténtalo de nuevo.")
}

library(ggplot2)

# ------------------------------------------------------
# 1) Introducción didáctica (imprimimos en consola)
# ------------------------------------------------------
cat("============================================================\n")
cat("Sesión 11 – MANCOVA (script didáctico en R)\n")
cat("============================================================\n\n")

cat("Idea general:\n")
cat("- La MANCOVA combina la lógica de:\n")
cat("    * MANOVA  -> múltiples respuestas continuas (Y1, Y2, Y3, ...)\n")
cat("    * ANCOVA  -> una o más covariables continuas (X) que ajustan el análisis\n\n")

cat("En este ejemplo simularemos un ensayo con:\n")
cat("  - 3 tratamientos (variedades): T1, T2, T3\n")
cat("  - 1 covariable continua X (ej. vigor inicial)\n")
cat("  - 3 respuestas continuas correlacionadas:\n")
cat("      Y1 = rendimiento (kg/ha)\n")
cat("      Y2 = ºBrix (sólidos solubles)\n")
cat("      Y3 = firmeza del fruto\n\n")

cat("Compararemos tres modelos:\n")
cat("  (M0) MANOVA sin covariable:           cbind(Y1, Y2, Y3) ~ trat\n")
cat("  (M1) MANCOVA aditiva (Trat + X):      cbind(Y1, Y2, Y3) ~ trat + X\n")
cat("  (M2) MANCOVA con interacción Trat*X:  cbind(Y1, Y2, Y3) ~ trat * X\n\n")

cat("La idea didáctica es ver cómo la covariable X ayuda a explicar\n")
cat("variación conjunta en (Y1,Y2,Y3) y cómo cambia el efecto global\n")
cat("del tratamiento cuando se ajusta por X.\n\n")

# ------------------------------------------------------
# 2) Parámetros de simulación (puedes modificarlos)
# ------------------------------------------------------
set.seed(123)  # reproducibilidad

g_trat      <- 3     # número de tratamientos (T1, T2, T3)
n_por_trat  <- 30    # observaciones por tratamiento
sigma_res   <- 8     # desvío estándar residual (para cada respuesta)
rho         <- 0.5   # correlación residual base entre Y1, Y2, Y3

# Pendientes verdaderas de la covariable para cada respuesta
beta_Y1 <- 2.0   # relación X -> Y1 (rendimiento)
beta_Y2 <- 0.8   # relación X -> Y2 (ºBrix)
beta_Y3 <- 1.2   # relación X -> Y3 (firmeza)

# Medias base (cuando X=0 y trat = referencia)
mu_Y1 <- 9000
mu_Y2 <- 12
mu_Y3 <- 280

cat("Parámetros de simulación:\n")
cat("  - g_trat     =", g_trat, "\n")
cat("  - n_por_trat =", n_por_trat, " (obs por tratamiento)\n")
cat("  - sigma_res  =", sigma_res, " (desvío estándar residual por Y)\n")
cat("  - rho        =", rho, " (correlación residual entre Y1, Y2, Y3)\n")
cat("  - Pendientes verdaderas de X:\n")
cat("       beta_Y1 =", beta_Y1, "\n")
cat("       beta_Y2 =", beta_Y2, "\n")
cat("       beta_Y3 =", beta_Y3, "\n\n")

# ------------------------------------------------------
# 3) Simulación del experimento MANCOVA
# ------------------------------------------------------

# 3.1) Tratamientos y covariable X
N <- g_trat * n_por_trat
trat <- factor(rep(paste0("T", 1:g_trat), each = n_por_trat))

# Covariable X ~ Normal(0,1), centrada (ej. vigor inicial estandarizado)
X_raw <- rnorm(N, mean = 0, sd = 1)
X     <- X_raw - mean(X_raw)

# 3.2) Efectos de tratamiento sobre cada respuesta
#      (aquí simulamos diferencias multivariadas entre tratamientos)
#      eff_trat es una matriz g_trat x 3 (Y1, Y2, Y3)
eff_trat <- matrix(
  rnorm(g_trat * 3, mean = 0, sd = 400),  # puedes cambiar la SD si quieres
  nrow = g_trat, ncol = 3
)
colnames(eff_trat) <- c("Y1", "Y2", "Y3")
rownames(eff_trat) <- paste0("T", 1:g_trat)

# 3.3) Matriz de covarianza residual Σ con correlación rho
p <- 3  # nº de respuestas
Sigma <- matrix(rho * sigma_res^2, nrow = p, ncol = p)
diag(Sigma) <- sigma_res^2

# Descomposición de Cholesky para generar errores multivariantes correlacionados
chol_S <- chol(Sigma)

# 3.4) Medias lineales por unidad experimental
mu_base_vec <- c(mu_Y1, mu_Y2, mu_Y3)
beta_vec    <- c(beta_Y1, beta_Y2, beta_Y3)

mu_mat <- matrix(NA_real_, nrow = N, ncol = p)

for (k in 1:g_trat) {
  idx_k <- which(trat == paste0("T", k))
  # media = mu_base + efecto_trat_k + beta*X
  for (j in 1:p) {
    mu_mat[idx_k, j] <- mu_base_vec[j] +
      eff_trat[k, j] +
      beta_vec[j] * X[idx_k]
  }
}

# 3.5) Errores residuales y respuestas finales
Z <- matrix(rnorm(N * p), nrow = N, ncol = p)
E <- Z %*% chol_S
Y_mat <- mu_mat + E
colnames(Y_mat) <- c("Y1", "Y2", "Y3")

# 3.6) Data frame final
datos <- data.frame(
  trat = trat,
  X    = X,
  Y1   = Y_mat[, "Y1"],
  Y2   = Y_mat[, "Y2"],
  Y3   = Y_mat[, "Y3"]
)

cat("Primeras filas del dataset simulado (MANCOVA):\n")
print(head(datos, 6))
cat("\n")

# ------------------------------------------------------
# 4) Estructura básica y correlaciones
# ------------------------------------------------------
cat("Dimensiones del dataset:\n")
print(dim(datos))
cat("\nNiveles de trat:\n")
print(levels(datos$trat))
cat("\n")

cat("Correlaciones empíricas entre respuestas (Y1, Y2, Y3):\n")
print(cor(datos[, c("Y1", "Y2", "Y3")]))
cat("\n")

cat("Correlaciones entre X y las respuestas:\n")
print(cor(datos[, c("X", "Y1", "Y2", "Y3")]))
cat("\n")

cat("Comentario:\n")
cat("- Las correlaciones entre Y1, Y2 y Y3 reflejan tanto la estructura residual\n")
cat("  como el efecto compartido de la covariable X sobre las tres respuestas.\n")
cat("- Las correlaciones entre X y Yk muestran cuánto 'impacta' la covariable\n")
cat("  en cada respuesta (slopes beta_Yk seleccionados arriba).\n\n")

# ------------------------------------------------------
# 5) Ajuste de modelos MANOVA / MANCOVA
# ------------------------------------------------------

# 5.1) MANOVA sin covariable (modelo base)
fit_MANOVA <- manova(cbind(Y1, Y2, Y3) ~ trat, data = datos)

# 5.2) MANCOVA aditiva: Trat + X
fit_MANCOVA_add <- manova(cbind(Y1, Y2, Y3) ~ trat + X, data = datos)

# 5.3) MANCOVA con interacción Trat*X (para revisar homogeneidad de pendientes)
fit_MANCOVA_int <- manova(cbind(Y1, Y2, Y3) ~ trat * X, data = datos)

cat("============================================================\n")
cat("Resultados MANOVA vs MANCOVA (test Pillai)\n")
cat("============================================================\n\n")

cat("--- Modelo (M0): MANOVA sin covariable ---\n")
print(summary(fit_MANOVA, test = "Pillai"))
cat("\n")

cat("--- Modelo (M1): MANCOVA aditiva (trat + X) ---\n")
print(summary(fit_MANCOVA_add, test = "Pillai"))
cat("\n")

cat("--- Modelo (M2): MANCOVA con interacción (trat * X) ---\n")
print(summary(fit_MANCOVA_int, test = "Pillai"))
cat("\n")

cat("Lectura didáctica sugerida:\n")
cat("- Compara el efecto global de 'trat' en M0 y M1: Pillai y p-valor.\n")
cat("- Si X explica parte importante de la variación en (Y1,Y2,Y3),\n")
cat("  el modelo M1 suele tener residuos más pequeños y puede ganar potencia.\n")
cat("- El término 'trat:X' en M2 permite evaluar si las pendientes de X\n")
cat("  son aproximadamente comunes entre tratamientos (homogeneidad de pendientes).\n\n")

# ------------------------------------------------------
# 6) AN(C)OVA univariada de seguimiento (opcional)
# ------------------------------------------------------
cat("============================================================\n")
cat("AN(C)OVA univariada de seguimiento (ejemplo con Y1)\n")
cat("============================================================\n\n")

# Modelo sin X (ANOVA)
lm_Y1_noX <- lm(Y1 ~ trat, data = datos)
# Modelo con X (ANCOVA)
lm_Y1_X   <- lm(Y1 ~ trat + X, data = datos)

cat("--- ANOVA univariante (Y1 ~ trat) ---\n")
print(anova(lm_Y1_noX))
cat("\n")

cat("--- ANCOVA univariante (Y1 ~ trat + X) ---\n")
print(anova(lm_Y1_X))
cat("\n")

cat("Resumen ajustado (modelo con X):\n")
print(summary(lm_Y1_X))
cat("\n")

cat("Comentario:\n")
cat("- Observa cómo cambian los residuos y el R² al incluir X en el modelo de Y1.\n")
cat("- En ANÁLISIS multivariado (MANCOVA) hacemos algo análogo, pero para el\n")
cat("  vector completo (Y1,Y2,Y3) de forma simultánea.\n\n")

# ------------------------------------------------------
# 7) Visualización: relación Y1 ~ X por tratamiento
# ------------------------------------------------------

cat("Generando gráfico: Y1 ~ X por tratamiento (con rectas ajustadas)...\n")
cat("El gráfico se mostrará en la ventana de gráficos de R.\n\n")

g1 <- ggplot(datos, aes(x = X, y = Y1, colour = trat)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    x = "Covariable X (centrada; p.ej. vigor inicial)",
    y = "Y1: Rendimiento (kg/ha)",
    colour = "Tratamiento",
    title = "Relación Y1 ~ X por tratamiento (MANCOVA simulada)"
  ) +
  theme_minimal(base_size = 12)

print(g1)

# ------------------------------------------------------
# 8) Heatmap de medias por tratamiento y respuesta
#     (no ajustadas por X, pero útil para ver el patrón multivariado)
# ------------------------------------------------------
cat("Generando heatmap simple de medias por tratamiento×respuesta (sin ajustar por X)...\n\n")


means_long <- datos %>%
  group_by(trat) %>%
  summarise(
    Y1 = mean(Y1),
    Y2 = mean(Y2),
    Y3 = mean(Y3),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols      = starts_with("Y"),
    names_to  = "respuesta",
    values_to = "media"
  )

g2 <- ggplot(means_long, aes(x = respuesta, y = trat, fill = media)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f", media)), size = 3) +
  scale_fill_viridis_c(name = "Media") +
  labs(
    x = "Respuesta",
    y = "Tratamiento",
    title = "Medias simuladas por tratamiento y respuesta (sin ajuste por X)"
  ) +
  theme_minimal(base_size = 12)

print(g2)