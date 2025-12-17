# ======================================================================
# R/scripts/Diseños_estadisticos_V3/Parte_III/session11_MANCOVA_demo.R
# Sesión 11 (MANCOVA) – Script didáctico independiente (versión comentada)
# ======================================================================
#
# OBJETIVO DIDÁCTICO
#   1) Simular un experimento con:
#        - g tratamientos (variedades)
#        - una covariable continua X (p. ej., vigor inicial estandarizado)
#        - 3 respuestas continuas (Y1, Y2, Y3) correlacionadas
#   2) Comparar:
#        (M0) MANOVA sin covariable:           cbind(Y1,Y2,Y3) ~ trat
#        (M1) MANCOVA aditiva (Trat + X):      cbind(Y1,Y2,Y3) ~ trat + X
#        (M2) MANCOVA con interacción Trat*X:  cbind(Y1,Y2,Y3) ~ trat * X
#   3) Aprender a interpretar:
#        - Correlaciones entre respuestas y con X
#        - La tabla multivariada (Pillai): Df, Pillai, approx F, num Df, den Df, p-value
#        - Qué significa que X sea (o no) significativa en el contexto multivariado
#        - Qué significa que trat:X sea (o no) significativa (homogeneidad de pendientes)
#
# IDEA CLAVE
#   - MANOVA: “¿Cambian las respuestas (Y1,Y2,Y3) en conjunto entre tratamientos?”
#   - MANCOVA: “¿Cambian (Y1,Y2,Y3) entre tratamientos DESPUÉS de ajustar por X?”
#   - Interacción trat:X: “¿La relación entre X y las respuestas es la misma en todos los tratamientos?”
#
# NOTA :
#   Este script usa una misma magnitud de efecto de tratamiento (SD=400) para Y1,Y2,Y3.
#   Eso puede producir escalas poco realistas (por ejemplo Y2 con valores muy grandes o negativos),
#   y también puede hacer que el efecto de tratamiento sea “demasiado” dominante (Pillai ~ máximo).
#   Para escenarios agronómicos más realistas, ajusta:
#     - la variabilidad residual por respuesta (sigma_res) o usa un sigma por respuesta
#     - la magnitud de eff_trat por respuesta (no un único SD=400 para todas)
#     - las pendientes beta_Yk si quieres que X tenga mayor impacto detectable
#
# ======================================================================

# ------------------------------------------------------
# 0) Paquetes necesarios
# ------------------------------------------------------
require_or_stop <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Falta el paquete '", pkg, "'. Instálalo y vuelve a ejecutar el script.", call. = FALSE)
  }
}

require_or_stop("ggplot2")
require_or_stop("dplyr")
require_or_stop("tidyr")

library(ggplot2)
library(dplyr)
library(tidyr)

# ------------------------------------------------------
# 1) Parámetros de simulación (puedes modificarlos)
# ------------------------------------------------------
set.seed(123)  # reproducibilidad

g_trat      <- 3     # número de tratamientos (T1, T2, T3)
n_por_trat  <- 30    # observaciones por tratamiento
sigma_res   <- 8     # desvío estándar residual (se usa igual para cada respuesta)
rho         <- 0.5   # correlación residual “base” entre Y1, Y2, Y3

# Pendientes verdaderas de la covariable para cada respuesta
beta_Y1 <- 2.0   # relación X -> Y1 (rendimiento)
beta_Y2 <- 0.8   # relación X -> Y2 (°Brix)
beta_Y3 <- 1.2   # relación X -> Y3 (firmeza)

# Medias base (cuando X=0 y trat = referencia)
mu_Y1 <- 9000
mu_Y2 <- 12
mu_Y3 <- 280

# ------------------------------------------------------
# 2) Simulación del experimento MANCOVA
# ------------------------------------------------------

# 2.1) Tratamientos y covariable X
# - trat: factor con g_trat niveles
# - X: covariable continua centrada (media ~ 0)
N <- g_trat * n_por_trat
trat <- factor(rep(paste0("T", 1:g_trat), each = n_por_trat))

X_raw <- rnorm(N, mean = 0, sd = 1)
X     <- X_raw - mean(X_raw)  # centrado: facilita interpretación del intercepto y efectos de trat

# 2.2) Efectos de tratamiento sobre cada respuesta
# - eff_trat es una matriz g_trat x 3 (Y1, Y2, Y3)
# - Conceptualmente: “desplaza” el vector de medias (Y1,Y2,Y3) por tratamiento
eff_trat <- matrix(
  rnorm(g_trat * 3, mean = 0, sd = 400),  # NOTA: SD=400 para todas las respuestas (ver nota de escalas)
  nrow = g_trat, ncol = 3
)
colnames(eff_trat) <- c("Y1", "Y2", "Y3")
rownames(eff_trat) <- paste0("T", 1:g_trat)

# 2.3) Matriz de covarianza residual Σ (misma varianza para cada Y y covarianza controlada por rho)
# - p = número de respuestas
# - Sigma modela “ruido conjunto”: si rho > 0, los errores tienden a moverse juntos
p <- 3
Sigma <- matrix(rho * sigma_res^2, nrow = p, ncol = p)
diag(Sigma) <- sigma_res^2

# Generación de errores multivariantes correlacionados: E = Z %*% chol(Sigma)
chol_S <- chol(Sigma)

# 2.4) Medias lineales por unidad experimental
# - media = mu_base + efecto_trat + beta*X  (por cada respuesta)
mu_base_vec <- c(mu_Y1, mu_Y2, mu_Y3)
beta_vec    <- c(beta_Y1, beta_Y2, beta_Y3)

mu_mat <- matrix(NA_real_, nrow = N, ncol = p)

for (k in 1:g_trat) {
  idx_k <- which(trat == paste0("T", k))
  for (j in 1:p) {
    mu_mat[idx_k, j] <- mu_base_vec[j] +
      eff_trat[k, j] +
      beta_vec[j] * X[idx_k]
  }
}

# 2.5) Errores residuales y respuestas finales
Z <- matrix(rnorm(N * p), nrow = N, ncol = p)
E <- Z %*% chol_S

Y_mat <- mu_mat + E
colnames(Y_mat) <- c("Y1", "Y2", "Y3")

# 2.6) Dataset final
datos <- data.frame(
  trat = trat,
  X    = X,
  Y1   = Y_mat[, "Y1"],
  Y2   = Y_mat[, "Y2"],
  Y3   = Y_mat[, "Y3"]
)

# Sugerencia de uso interactivo:
#   View(head(datos))
#   summary(datos)

# ------------------------------------------------------
# 3) Estructura básica y correlaciones (para “leer” el problema)
# ------------------------------------------------------

# 3.1) Correlación entre respuestas (Y1,Y2,Y3)
# Interpretación:
#   - Valores altos positivos: respuestas “suben/bajan juntas”
#   - Valores cercanos a 0: respuestas casi independientes
#   - Valores negativos: trade-off (una sube cuando la otra baja)
cor_Y <- cor(datos[, c("Y1", "Y2", "Y3")])

# 3.2) Correlación de X con respuestas
# Interpretación:
#   - Si |cor(X, Yk)| es alto, X tiene potencial de “explicar” bastante de Yk
#   - Si es bajo, la contribución de X puede ser pequeña (o quedar opacada por efectos de trat)
cor_XY <- cor(datos[, c("X", "Y1", "Y2", "Y3")])

# Para ver en consola (modo interactivo):
cor_Y
cor_XY

# ------------------------------------------------------
# 4) Ajuste de modelos MANOVA / MANCOVA
# ------------------------------------------------------
# NOTA (qué hace manova en R):
#   - manova(...) construye un objeto basado en aov con múltiples respuestas.
#   - La “magia” está en summary(..., test=...) que aplica estadísticas multivariadas.
#
# MODELOS:
#   (M0) MANOVA:   diferencias entre tratamientos ignorando X
#   (M1) MANCOVA:  diferencias entre tratamientos ajustando por X (efecto aditivo)
#   (M2) MANCOVA:  agrega trat:X para evaluar si la “pendiente” de X cambia por tratamiento

fit_MANOVA      <- manova(cbind(Y1, Y2, Y3) ~ trat,     data = datos)
fit_MANCOVA_add <- manova(cbind(Y1, Y2, Y3) ~ trat + X, data = datos)
fit_MANCOVA_int <- manova(cbind(Y1, Y2, Y3) ~ trat * X, data = datos)

# ------------------------------------------------------
# 5) Salida principal: resumen multivariado (Pillai)
# ------------------------------------------------------
# IMPORTANTE: cómo leer summary(..., test="Pillai")
# La tabla típica incluye columnas:
#   - Df: grados de libertad del término (p. ej., trat tiene g_trat-1)
#   - Pillai: estadístico de Pillai (más grande = mayor separación multivariada de medias)
#   - approx F: transformación aproximada a una F (para prueba de hipótesis)
#   - num Df / den Df: grados de libertad de la F aproximada
#   - Pr(>F): p-value. Si es pequeño (ej. <0.05), evidencia contra H0
#
# HIPÓTESIS (interpretación conceptual):
#   - Para 'trat':
#       H0: los vectores de medias (Y1,Y2,Y3) son iguales en todos los tratamientos
#       H1: al menos un tratamiento difiere en el vector de medias
#   - Para 'X' en M1:
#       H0: X NO explica variación conjunta en (Y1,Y2,Y3) una vez considerado trat
#       H1: X sí explica parte de la variación conjunta
#   - Para 'trat:X' en M2:
#       H0: la relación entre X y las respuestas es la misma en todos los tratamientos
#           (homogeneidad de pendientes en el sentido multivariado)
#       H1: al menos un tratamiento tiene una pendiente distinta (interacción)

sum_M0_pillai <- summary(fit_MANOVA,      test = "Pillai")
sum_M1_pillai <- summary(fit_MANCOVA_add, test = "Pillai")
sum_M2_pillai <- summary(fit_MANCOVA_int, test = "Pillai")

# Para ver en consola (modo interactivo):
sum_M0_pillai
sum_M1_pillai
sum_M2_pillai

# GUÍA DE INTERPRETACIÓN “MANOVA vs MANCOVA” (lo más importante de la sesión):
#
# (A) Comparación del efecto de TRAT entre M0 y M1:
#   - Si 'trat' es significativo en M0 y también en M1:
#       el tratamiento afecta (Y1,Y2,Y3) incluso después de ajustar por X.
#   - Si 'trat' es significativo en M0 pero deja de serlo en M1:
#       parte (o casi toda) la diferencia entre tratamientos podría estar explicada por X
#       (es decir, diferencias iniciales en vigor estaban “sesgando” la comparación).
#   - Si 'trat' no es significativo en M0 y aparece significativo en M1:
#       ajustar por X redujo el “ruido multivariado” y aumentó potencia (mejor precisión).
#
# (B) Lectura del término X en M1:
#   - X significativo: la covariable explica variación conjunta relevante en el vector Y.
#   - X no significativo: (i) X realmente no aporta, o (ii) su aporte es pequeño frente a otros efectos,
#     o (iii) el diseño/simulación no generó suficiente señal de X a nivel multivariado.
#
# (C) Lectura del término trat:X en M2 (homogeneidad de pendientes):
#   - trat:X NO significativo: se puede preferir el modelo aditivo (M1) por parsimonia.
#   - trat:X significativo: no es correcto asumir pendientes comunes; interpreta M2 y/o analiza
#     efectos simples (pendientes por tratamiento) para entender el patrón.

# También puedes comparar con otros estadísticos (Wilks, Hotelling-Lawley, Roy)
sum_M1_wilks <- summary(fit_MANCOVA_add, test = "Wilks")

# ------------------------------------------------------
# 6) “ANOVA vs ANCOVA” como puente conceptual (seguimiento univariado)
# ------------------------------------------------------
# Aunque MANCOVA es multivariada, es muy útil hacer seguimiento univariado para:
#   - Identificar en cuál(es) respuesta(s) se concentra el efecto
#   - Detectar trade-offs (p. ej., sube Y1 pero baja Y2)
#
# Aquí repetimos la idea “ANOVA vs ANCOVA” para Y1:
#   - ANOVA:  Y1 ~ trat
#   - ANCOVA: Y1 ~ trat + X
#
# Cómo leer anova(lm):
#   - Df: grados de libertad del término
#   - Sum Sq: suma de cuadrados explicada por el término
#   - Mean Sq: Sum Sq / Df
#   - F value: (Mean Sq término) / (Mean Sq residual)
#   - Pr(>F): p-value del test del término
#
# NOTA:
#   - anova(lm) en R usa sumas de cuadrados SECUENCIALES (Type I).
#   - En diseños balanceados suele ser aceptable; en desbalanceados puede cambiar la interpretación
#     según el orden de términos. (Para Type II/III se suele usar car::Anova.)

lm_Y1_noX <- lm(Y1 ~ trat,     data = datos)  # ANOVA (sin X)
lm_Y1_X   <- lm(Y1 ~ trat + X, data = datos)  # ANCOVA (con X)

anova_Y1_noX <- anova(lm_Y1_noX)
anova_Y1_X   <- anova(lm_Y1_X)

sum_Y1_X <- summary(lm_Y1_X)

# Para ver en consola (modo interactivo):
anova_Y1_noX
anova_Y1_X
sum_Y1_X

# Qué mirar específicamente al comparar anova_Y1_noX vs anova_Y1_X:
#   1) ¿El p-value de 'trat' cambia al incluir X?
#      - Si cambia mucho, X estaba asociado a Y1 y también “mezclado” con trat.
#   2) ¿El Residual Sum Sq baja al incluir X?
#      - Si baja, X está explicando parte del error/residuo (mejor ajuste).
#   3) En summary(lm_Y1_X), mira:
#      - Coeficiente de X (Estimate): pendiente estimada (β̂) para Y1
#      - Pr(>|t|) de X: evidencia de que β ≠ 0
#      - Residual standard error: tamaño típico del error en Y1 (en unidades de Y1)
#      - R²: proporción de varianza explicada (ojo: puede inflarse con efectos enormes simulados)

# ------------------------------------------------------
# 7) Seguimiento univariado “automático” desde el objeto manova
# ------------------------------------------------------
# summary.aov(manova_obj) entrega tablas ANOVA/ANCOVA por respuesta como seguimiento.
# Es útil después de una prueba global significativa (MANOVA/MANCOVA):
uni_M0 <- summary.aov(fit_MANOVA)        # ANOVAs separados por respuesta (sin X)
uni_M1 <- summary.aov(fit_MANCOVA_add)   # ANCOVAs separados por respuesta (con X)
uni_M2 <- summary.aov(fit_MANCOVA_int)   # ANCOVAs con interacción por respuesta

# Para ver en consola (modo interactivo):
uni_M0
uni_M1
uni_M2

# Interpretación práctica de uni_M1:
#   - Si X es significativa en algunas respuestas (Yk) pero no en otras, eso es normal:
#     la covariable puede afectar fuerte a un rasgo (p. ej., rendimiento) y poco a otro (p. ej., °Brix).
#   - Si trat es significativo en MAN(C)OVA global pero en univariado no aparece tan claro,
#     puede ser señal de “efecto pequeño distribuido” en varias respuestas (la ventaja de MANOVA/MANCOVA).

# ------------------------------------------------------
# 8) Visualización: relación Y1 ~ X por tratamiento
# ------------------------------------------------------
# Objetivo del gráfico:
#   - Ver la relación entre X e Y1 (tendencia)
#   - Comparar si las rectas por tratamiento son aproximadamente paralelas
#     (si NO lo son, sugiere interacción trat:X)
#
# Lectura rápida:
#   - Diferencias verticales entre rectas: diferencias de intercepto (trat) ajustadas por X
#   - Diferencias de pendiente: evidencia visual de interacción (homogeneidad de pendientes se rompe)

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
# 9) Heatmap de medias por tratamiento × respuesta (no ajustadas por X)
# ------------------------------------------------------
# Este heatmap NO ajusta por X: muestra el patrón “crudo” promedio por tratamiento.
# Utilidad:
#   - Comparar rápidamente el perfil multivariado de cada tratamiento
#   - Detectar trade-offs: un tratamiento alto en Y1 pero bajo en Y2, etc.
#
# Si el objetivo es “medias ajustadas por X”, eso ya es un paso posterior (p. ej., emmeans con modelos
# univariados, o aproximaciones multivariadas más avanzadas).

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
    title = "Medias por tratamiento y respuesta (sin ajuste por X)"
  ) +
  theme_minimal(base_size = 12)

print(g2)
