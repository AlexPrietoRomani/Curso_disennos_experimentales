# ======================================================================
# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session10_MANOVA_demo.R
# Sesión 10 (MANOVA) - Script didáctico autónomo
#
# Objetivo:
#   - Mostrar, con datos simulados, cuándo tiene sentido usar MANOVA
#     en lugar de hacer solo ANOVAs por separado.
#   - Ilustrar la estructura típica de datos y la comparación
#     ANOVA univariada vs MANOVA global.
#
# Contexto agronómico:
#   Ensayo de variedades de arándano (trat = variedad) donde medimos,
#   en la misma parcela:
#     * Y1_rend  = rendimiento (kg/ha)
#     * Y2_brix  = °Brix (contenido de azúcares)
#     * Y3_firm  = firmeza del fruto
# ======================================================================

# ----------------------------------------------------------------------
# 0) Paquetes necesarios
# ----------------------------------------------------------------------
# Si algún paquete falta, descomentar e instalar:
# install.packages(c("MASS", "ggplot2", "dplyr", "tidyr"))

library(MASS)     # mvrnorm: simulación normal multivariante
library(ggplot2)
library(dplyr)
library(tidyr)

# ----------------------------------------------------------------------
# 1) Introducción didáctica (impresa en consola)
# ----------------------------------------------------------------------
cat("\n",
    "============================================================\n",
    "Sesión 10 (MANOVA) - Introducción didáctica con datos simulados\n",
    "============================================================\n\n",
    "Situación típica:\n",
    "  - Ensayo de variedades de arándano (trat = variedad).\n",
    "  - En cada parcela medimos varias respuestas:\n",
    "      Y1 = rendimiento (kg/ha)\n",
    "      Y2 = °Brix (contenido de azúcares)\n",
    "      Y3 = firmeza (N o g/mm)\n\n",
    "Idea central de MANOVA:\n",
    "  - En vez de hacer una ANOVA separada para cada Y,\n",
    "    probamos el efecto de trat sobre el VECTOR (Y1, Y2, Y3).\n",
    "  - Aprovechamos la correlación entre respuestas, en lugar de ignorarla.\n\n",
    sep = ""
)

# ----------------------------------------------------------------------
# 2) Parámetros de simulación: experimento simple tipo CRD
# ----------------------------------------------------------------------
set.seed(123)      # reproducibilidad

G     <- 3         # número de tratamientos (variedades: T1, T2, T3)
n_rep <- 15        # n observaciones por tratamiento
rho   <- 0.6       # correlación entre respuestas
delta <- 0.8       # “tamaño” del efecto de tratamiento (≈ 0.8 SD)

cat("Parámetros de simulación:\n",
    "  - Nº tratamientos (variedades):", G, "\n",
    "  - Nº observaciones por tratamiento:", n_rep, "\n",
    "  - Correlación esperada entre respuestas (rho):", rho, "\n",
    "  - Magnitud del efecto de tratamiento (delta):", delta, "\n\n"
)

# Medias “base” para cada respuesta (orden de magnitud agronómico)
mu_base <- c(
  Y1_rend = 10000,  # rendimiento (kg/ha)
  Y2_brix = 12,     # ºBrix
  Y3_firm = 280     # firmeza
)

# Escalas de diferencia entre tratamientos para cada respuesta (en unidades)
offset_scale <- c(
  Y1_rend = 800,    # ≈ 0.8 ton/ha por paso en la secuencia centrada
  Y2_brix = 0.5,
  Y3_firm = 15
)

# Secuencia centrada para generar medias por tratamiento:
#   para G = 3 → -1, 0, 1
idx        <- seq_len(G)
center_seq <- idx - mean(idx)

mu_list <- lapply(seq_len(G), function(g) {
  mu_base + center_seq[g] * delta * offset_scale
})
names(mu_list) <- paste0("T", 1:G)

# Desvíos estándar residuales para cada respuesta (ruido intra-tratamiento)
sd_vec <- c(
  Y1_rend = 1000,  # SD residual en rendimiento
  Y2_brix = 1,     # SD residual en ºBrix
  Y3_firm = 20     # SD residual en firmeza
)

# Matriz de correlaciones: asumimos la misma rho para todas las parejas
Rmat <- matrix(rho, nrow = 3, ncol = 3)
diag(Rmat) <- 1

# Matriz de covarianza Σ = D * R * D
Sigma <- diag(sd_vec) %*% Rmat %*% diag(sd_vec)

cat("Medias base aproximadas por respuesta:\n")
print(mu_base)
cat("\nDesvíos estándar residuales por respuesta:\n")
print(sd_vec)
cat("\nMatriz de correlación objetivo (R):\n")
print(round(Rmat, 2))
cat("\nMatriz de covarianza Σ (aprox.):\n")
print(round(Sigma, 1))
cat("\n")

# ----------------------------------------------------------------------
# 3) Simulación de datos multivariantes por tratamiento
# ----------------------------------------------------------------------
datos_list <- lapply(seq_len(G), function(g) {
  mu_g <- mu_list[[g]]
  Yg   <- MASS::mvrnorm(
    n     = n_rep,
    mu    = mu_g,
    Sigma = Sigma
  )
  data.frame(
    trat    = factor(paste0("T", g)),
    Y1_rend = Yg[, 1],
    Y2_brix = Yg[, 2],
    Y3_firm = Yg[, 3]
  )
})

datos <- do.call(rbind, datos_list)
rownames(datos) <- NULL

cat("Vista rápida de los datos simulados:\n")
print(head(datos, 10))
cat("\nDimensiones del dataset:", nrow(datos), "x", ncol(datos), "\n\n")

# ----------------------------------------------------------------------
# 4) Estructura MANOVA: correlación entre respuestas
# ----------------------------------------------------------------------
cat("Correlación empírica entre las respuestas (Y1, Y2, Y3):\n")
print(round(cor(datos[, c("Y1_rend", "Y2_brix", "Y3_firm")]), 3))
cat("\nComentario:\n",
    "  - En MANOVA, esta correlación se utiliza para evaluar\n",
    "    el efecto global del tratamiento sobre el vector (Y1, Y2, Y3).\n\n", sep = "")

# ----------------------------------------------------------------------
# 5) ANOVA univariada: una respuesta a la vez
# ----------------------------------------------------------------------
cat("============================================================\n",
    "ANOVA univariados por respuesta\n",
    "============================================================\n\n")

aov_y1 <- aov(Y1_rend ~ trat, data = datos)
aov_y2 <- aov(Y2_brix ~ trat, data = datos)
aov_y3 <- aov(Y3_firm ~ trat, data = datos)

cat("Respuesta Y1 = rendimiento (kg/ha)\n")
print(summary(aov_y1))
cat("\n--------------------------------------------------------\n\n")

cat("Respuesta Y2 = ºBrix\n")
print(summary(aov_y2))
cat("\n--------------------------------------------------------\n\n")

cat("Respuesta Y3 = firmeza\n")
print(summary(aov_y3))
cat("\n--------------------------------------------------------\n\n")

cat("Comentario didáctico:\n",
    "  - Aquí cada ANOVA ignora las otras respuestas.\n",
    "  - El control del error tipo I por múltiples pruebas no se ve aún.\n\n",
    sep = "")

# ----------------------------------------------------------------------
# 6) MANOVA global: todas las respuestas juntas
# ----------------------------------------------------------------------
cat("============================================================\n",
    "MANOVA global: cbind(Y1, Y2, Y3) ~ trat\n",
    "============================================================\n\n")

fit_manova <- manova(
  cbind(Y1_rend, Y2_brix, Y3_firm) ~ trat,
  data = datos
)

cat("Prueba global usando traza de Pillai:\n")
print(summary(fit_manova, test = "Pillai"))
cat("\n--------------------------------------------------------\n\n")

cat("Prueba global usando Wilks' Lambda:\n")
print(summary(fit_manova, test = "Wilks"))
cat("\n--------------------------------------------------------\n\n")

cat("Interpretación didáctica (orientativa):\n",
    "  - Si las pruebas globales (Pillai, Wilks) son significativas,\n",
    "    concluimos que la variedad (trat) afecta el VECTOR de respuestas\n",
    "    (rendimiento, ºBrix, firmeza) en conjunto.\n",
    "  - Luego revisamos los ANOVA univariados para ver en cuáles Y\n",
    "    se concentra la diferencia (o si hay trade-offs entre respuestas).\n\n",
    sep = "")

# ----------------------------------------------------------------------
# 7) Visualización 1: dispersión Y1 vs Y2 por tratamiento
# ----------------------------------------------------------------------
cat("Generando gráfico 1: Dispersión Y1 vs Y2 por tratamiento...\n")

g1 <- ggplot(datos, aes(x = Y1_rend, y = Y2_brix, color = trat)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(
    x = "Y1: Rendimiento (kg/ha)",
    y = "Y2: ºBrix",
    color = "Tratamiento",
    title = "Dispersión Y1 vs Y2 por tratamiento"
  ) +
  theme_minimal(base_size = 13)

print(g1)

cat("Comentario didáctico del gráfico:\n",
    "  - Cada punto es una parcela.\n",
    "  - Vemos la relación entre rendimiento y ºBrix,\n",
    "    coloreada por variedad.\n",
    "  - Si los grupos se separan en el plano (Y1,Y2), esto apoya\n",
    "    la idea de un efecto multivariante de trat.\n\n",
    sep = "")

# ----------------------------------------------------------------------
# 8) Visualización 2: heatmap de medias por tratamiento × respuesta
# ----------------------------------------------------------------------
cat("Generando gráfico 2: Heatmap de medias por tratamiento y respuesta...\n")

means_long <- datos %>%
  group_by(trat) %>%
  summarise(
    Y1_rend = mean(Y1_rend),
    Y2_brix = mean(Y2_brix),
    Y3_firm = mean(Y3_firm),
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
    title = "Medias simuladas por tratamiento y respuesta (heatmap)"
  ) +
  theme_minimal(base_size = 13)

print(g2)

cat("Comentario didáctico del heatmap:\n",
    "  - Cada celda muestra la media de una respuesta para un tratamiento.\n",
    "  - Permite ver el patrón multivariado de forma compacta:\n",
    "    qué variedades son altas en rendimiento, ºBrix o firmeza,\n",
    "    y si hay compromisos (trade-offs) entre respuestas.\n\n",
    sep = "")
