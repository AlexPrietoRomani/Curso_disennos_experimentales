# ======================================================================
# R/modules/Diseños_estadisticos_V3/Parte III (Avanzada)/session10_MANOVA_demo.R
# Sesión 10 (MANOVA) - Script didáctico autónomo
#
# Objetivo (didáctico):
#   1) Simular un experimento CRD con 1 factor (trat = variedad) y 3 respuestas
#      medidas en la MISMA unidad experimental (parcela).
#   2) Comparar el enfoque "ANOVA por separado" vs "MANOVA global":
#        - ANOVA univariada: analiza cada Y por separado (Y1, luego Y2, luego Y3).
#        - MANOVA: prueba el efecto de trat sobre el VECTOR (Y1, Y2, Y3) a la vez.
#   3) Visualizar patrones multivariados (scatter bivariado y heatmap de medias).
#
# Contexto agronómico:
#   Ensayo de variedades de arándano (trat = variedad) donde medimos, en cada parcela:
#     * Y1_rend  = rendimiento (kg/ha)
#     * Y2_brix  = °Brix (contenido de azúcares)
#     * Y3_firm  = firmeza del fruto
#
# ¿Cuándo MANOVA suele ser pertinente?
#   - Cuando hay VARIAS respuestas por unidad experimental y esas respuestas
#     están correlacionadas (p.ej., rendimiento y firmeza).
#   - Cuando el "efecto de tratamiento" puede manifestarse como un patrón conjunto
#     (trade-offs) que no se entiende bien mirando cada respuesta en aislamiento.
#
# Idea clave (ANOVA vs MANOVA):
#   - ANOVA univariada responde: "¿hay diferencias entre tratamientos para ESTA Y?"
#   - MANOVA responde: "¿hay diferencias entre tratamientos para el VECTOR (Y1,Y2,Y3)?"
#   - MANOVA aprovecha la covarianza entre respuestas (no asume independencia entre Y's).
# ======================================================================


# ----------------------------------------------------------------------
# 0) Paquetes necesarios
# ----------------------------------------------------------------------
# Si algún paquete falta, instalar (una vez) con:
# install.packages(c("MASS", "ggplot2", "dplyr", "tidyr"))

library(MASS)     # mvrnorm: simulación normal multivariante
library(ggplot2)
library(dplyr)
library(tidyr)


# ----------------------------------------------------------------------
# 1) Parámetros de simulación: experimento simple tipo CRD
# ----------------------------------------------------------------------
set.seed(123)      # reproducibilidad

G     <- 3         # número de tratamientos (variedades: T1, T2, T3)
n_rep <- 15        # nº observaciones por tratamiento
rho   <- 0.6       # correlación "objetivo" entre respuestas
delta <- 0.8       # tamaño del efecto de tratamiento (≈ 0.8 SD, en escala relativa)

# Lectura didáctica de estos parámetros:
#   - G y n_rep definen el tamaño muestral total: N = G * n_rep.
#   - rho controla cuánto "se mueven juntas" las respuestas:
#       rho alto => respuestas muy correlacionadas (MANOVA puede ganar eficiencia).
#   - delta controla la separación (en promedio) entre tratamientos, en el vector de Y's.


# Medias base para cada respuesta (orden de magnitud agronómico)
mu_base <- c(
  Y1_rend = 10000,  # rendimiento (kg/ha)
  Y2_brix = 12,     # ºBrix
  Y3_firm = 280     # firmeza
)

# Escalas de diferencia entre tratamientos por respuesta (en unidades reales)
offset_scale <- c(
  Y1_rend = 800,    # ≈ 0.8 ton/ha por paso de "center_seq"
  Y2_brix = 0.5,
  Y3_firm = 15
)

# Secuencia centrada para generar medias por tratamiento:
#   para G = 3 => (-1, 0, 1), con lo que "T2" queda centrado alrededor de mu_base
idx        <- seq_len(G)
center_seq <- idx - mean(idx)

# Medias por tratamiento (vector de medias de 3 respuestas para cada T)
mu_list <- lapply(seq_len(G), function(g) {
  mu_base + center_seq[g] * delta * offset_scale
})
names(mu_list) <- paste0("T", 1:G)

# Desvíos estándar residuales por respuesta (ruido intra-tratamiento)
sd_vec <- c(
  Y1_rend = 1000,  # SD residual en rendimiento
  Y2_brix = 1,     # SD residual en ºBrix
  Y3_firm = 20     # SD residual en firmeza
)

# Matriz de correlaciones (misma rho para todas las parejas)
Rmat <- matrix(rho, nrow = 3, ncol = 3)
diag(Rmat) <- 1

# Matriz de covarianza Σ = D * R * D
#   - La diagonal de Σ son varianzas (sd^2) de cada respuesta.
#   - Fuera de la diagonal son covarianzas (capturan asociación lineal).
Sigma <- diag(sd_vec) %*% Rmat %*% diag(sd_vec)

# Notas de interpretación:
#   - Si rho = 0, Σ queda diagonal: respuestas independientes (MANOVA se parece más a "3 ANOVAs").
#   - Si rho es alto, Σ tiene covarianzas grandes: MANOVA puede detectar patrones conjuntos
#     incluso si en alguna respuesta el efecto univariado no es tan evidente.


# ----------------------------------------------------------------------
# 2) Simulación de datos multivariantes por tratamiento
# ----------------------------------------------------------------------
# Cada tratamiento g genera una nube multivariante (Y1,Y2,Y3) con media mu_list[[g]]
# y covarianza Sigma. Esto emula 3 mediciones en la misma parcela.

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

# Vista rápida (para clase):
head(datos, 10)
dim(datos)


# ----------------------------------------------------------------------
# 3) EDA multivariado: correlación empírica entre respuestas
# ----------------------------------------------------------------------
# Esta correlación estimada (a partir de los datos simulados) suele aproximar
# la correlación objetivo rho. La idea didáctica es evidenciar que las Y's
# no son independientes, lo que motiva MANOVA.

round(cor(datos[, c("Y1_rend", "Y2_brix", "Y3_firm")]), 3)


# ----------------------------------------------------------------------
# 4) ANOVA univariada: una respuesta a la vez
# ----------------------------------------------------------------------
# Aquí hacemos 3 ANOVAs separados:
#   - Y1 ~ trat
#   - Y2 ~ trat
#   - Y3 ~ trat
#
# Cómo leer un ANOVA (tabla típica):
#   Df:
#     - Df(trat) = G - 1  (nº parámetros de tratamientos, menos 1)
#     - Df(Residuals) = N - G
#   Sum Sq (Suma de cuadrados):
#     - Variabilidad explicada por trat vs variabilidad que queda como error (residual).
#   Mean Sq (Cuadrado medio):
#     - Mean Sq = Sum Sq / Df
#   F value:
#     - F = MS_trat / MS_residual
#   Pr(>F) (p-valor):
#     - Probabilidad (bajo H0: no diferencias entre tratamientos) de obtener un F tan grande o mayor.
#
# Advertencia didáctica:
#   - Al hacer 3 pruebas separadas (una por respuesta), aumenta el riesgo de falsos positivos
#     si no controlamos el error por múltiples pruebas. MANOVA se usa a menudo como “prueba global”
#     antes de mirar los univariados.

aov_y1 <- aov(Y1_rend ~ trat, data = datos)
aov_y2 <- aov(Y2_brix ~ trat, data = datos)
aov_y3 <- aov(Y3_firm ~ trat, data = datos)

# Salidas ANOVA univariadas:
summary(aov_y1)
summary(aov_y2)
summary(aov_y3)

# Lectura pedagógica recomendada:
#   1) Mirar p-valor de "trat":
#        - Si es pequeño: evidencia de diferencias entre variedades para ESA respuesta.
#        - Si es grande: no hay evidencia suficiente para esa Y (con ese tamaño muestral/ruido).
#   2) Mirar MS_residual:
#        - MS_residual alto indica mucho ruido intra-tratamiento.
#        - Comparar MS_residual entre respuestas NO es directo (están en unidades distintas),
#          pero sirve para ver cuán “ruidosa” es cada variable en su propia escala.


# ----------------------------------------------------------------------
# 5) MANOVA global: todas las respuestas juntas
# ----------------------------------------------------------------------
# MANOVA ajusta el mismo factor "trat", pero el resultado es una prueba global.
#
# En R:
#   fit_manova <- manova(cbind(Y1, Y2, Y3) ~ trat, data = datos)
#
# summary.manova permite elegir el estadístico de prueba:
#   test = c("Pillai", "Wilks", "Hotelling-Lawley", "Roy")
# (por defecto, R recomienda Pillai y entrega una transformación ~ F aproximada). 
#
# En términos conceptuales:
#   - H0 (global): los vectores de medias (μ1, μ2, μ3) son iguales entre tratamientos.
#   - H1: al menos un tratamiento difiere en el vector de medias (en algún patrón multivariado).
#
# Importante:
#   - Un MANOVA significativo dice “hay diferencia multivariante”.
#   - NO dice automáticamente “en cuál respuesta” ni “en qué dirección”.
#   - Por eso el flujo clásico es:
#        (1) MANOVA global
#        (2) Follow-up: ANOVAs univariadas (o contrastes/EMMs), interpretando con criterio.

fit_manova <- manova(
  cbind(Y1_rend, Y2_brix, Y3_firm) ~ trat,
  data = datos
)

# -----------------------
# 5.1) Salidas MANOVA
# -----------------------
# La tabla MANOVA (ej. Pillai/Wilks) incluye típicamente:
#   - Df: grados de libertad del efecto (trat) y residual.
#   - Estadístico multivariante (Pillai o Wilks, etc.).
#   - approx F: una transformación que se aproxima a una F.
#   - num Df / den Df: grados de libertad del numerador y denominador para esa F aproximada.
#   - Pr(>F): p-valor global.
#
# Nota didáctica:
#   - Pillai: suele considerarse más robusto y es el default recomendado por la doc de R. 
#   - Wilks: es muy popular en la literatura; también se reporta con F aproximada en R. 
#   - Hotelling-Lawley y Roy: alternativas; Roy actúa como “cota superior” en algunos casos. 

# MANOVA (Pillai)
summary(fit_manova, test = "Pillai")

# MANOVA (Wilks)
summary(fit_manova, test = "Wilks")

# Otras pruebas multivariantes
summary(fit_manova, test = "Hotelling-Lawley")
summary(fit_manova, test = "Roy")

# -----------------------
# 5.2) Follow-up recomendado: ANOVAs univariadas desde el objeto MANOVA
# -----------------------
# R permite obtener “tablas ANOVA por respuesta” directamente desde fit_manova:
#   summary.aov(fit_manova)
# Esto es útil didácticamente porque deja explícito el flujo:
#   - Primero el test global (MANOVA)
#   - Luego la descomposición univariada (qué pasa en cada Y)
# (La propia documentación de summary.manova muestra este patrón). 
#
# Interpretación:
#   - Si MANOVA es significativo, es coherente mirar estos univariados para entender
#     qué variables contribuyen más a la diferencia.
#   - Si MANOVA NO es significativo, muchos cursos recomiendan prudencia al “buscar”
#     significancia en univariados, salvo hipótesis a priori.

summary.aov(fit_manova)


# ----------------------------------------------------------------------
# 6) Visualización 1: dispersión Y1 vs Y2 por tratamiento
# ----------------------------------------------------------------------
# Propósito del gráfico:
#   - Ver si los tratamientos se “separan” en el plano (Y1, Y2).
#   - Si hay separación clara por color, suele apoyar una señal multivariada.
#   - También permite ver trade-offs: por ejemplo, tratamientos con alto rendimiento
#     pero bajo ºBrix (o viceversa).
#
# Interpretación “tipo clase”:
#   - Cada punto = 1 parcela.
#   - Grupos (colores) más separados => mayor evidencia de efecto de trat.
#   - Mucha superposición => más difícil detectar efecto (o efecto pequeño / ruido alto).

g1 <- ggplot(datos, aes(x = Y1_rend, y = Y2_brix, color = trat)) +
  geom_point(alpha = 0.8, size = 2) +
  labs(
    x = "Y1: Rendimiento (kg/ha)",
    y = "Y2: ºBrix",
    color = "Tratamiento",
    title = "Dispersión Y1 vs Y2 por tratamiento"
  ) +
  theme_minimal(base_size = 13)

# Para visualizar:
g1


# ----------------------------------------------------------------------
# 7) Visualización 2: heatmap de medias por tratamiento × respuesta
# ----------------------------------------------------------------------
# Propósito del heatmap:
#   - Resumir el “perfil multivariado” de cada tratamiento en una tabla visual.
#   - Ver de forma compacta en qué respuestas un tratamiento es alto/bajo.
#   - Identificar patrones: un tratamiento puede destacar en Y1 pero no en Y2/Y3, etc.
#
# Nota didáctica:
#   - Aquí se muestran medias "crudas" (sin ajuste adicional). En diseños con bloque,
#     se usarían medias ajustadas (EMMs) o BLUPs según corresponda.

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

# Para visualizar:
g2


# ----------------------------------------------------------------------
# 8) Cierre didáctico (en comentarios)
# ----------------------------------------------------------------------
# Lectura recomendada en clase (secuencia lógica):
#   A) Confirmar correlación entre respuestas (EDA) -> motiva MANOVA.
#   B) Revisar ANOVAs univariados -> qué pasa “por variable”.
#   C) Revisar MANOVA global (Pillai / Wilks) -> decisión global sobre el vector.
#   D) Si MANOVA es significativo:
#        - usar summary.aov(fit_manova) como follow-up,
#        - y luego (si aplica) comparaciones múltiples/contrastes por respuesta,
#          idealmente controlando el error por multiplicidad.
#
# Asunciones a recordar (nivel curso):
#   - Independencia entre unidades experimentales.
#   - Normalidad multivariante de residuos (aprox.) y ausencia de outliers severos.
#   - Homogeneidad de matrices de covarianza entre grupos (aprox.).
#     (En cursos avanzados se discute Box's M / enfoques robustos.)
#
# Nota técnica:
#   - manova() en R es esencialmente un aov() con múltiples respuestas y un
#     método summary multivariante. 
