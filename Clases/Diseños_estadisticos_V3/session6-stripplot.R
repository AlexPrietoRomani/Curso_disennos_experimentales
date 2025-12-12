# ==============================================================================
# SESIÓN 6 (PRÁCTICA EN R): DISEÑOS EN FRANJAS (STRIP-PLOT)
# Curso: Diseños Estadísticos Avanzados en Agricultura
# Autor: Alex + equipo
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Instalar una vez (si hace falta):
# install.packages(c("agricolae", "tidyverse", "lme4", "lmerTest"))

library(agricolae)   # Diseño y ANOVA en franjas: design.strip, strip.plot
library(tidyverse)   # dplyr + ggplot2 para manipulación y gráficos
library(lme4)        # Modelos lineales mixtos (LMM) tipo lmer()
library(lmerTest)    # P-valores y ANOVA tipo III para lmer()

set.seed(123)        # Semilla para reproducibilidad de la simulación

# ------------------------------------------------------------------------------
# 2. DISEÑO STRIP-PLOT BALANCEADO (LABRANZA × RIEGO)
# ------------------------------------------------------------------------------
# Objetivo:
#   - Construir el diseño físico (bloques, franjas A y franjas B).
#   - Entender qué representa cada columna del "fieldbook".
#
# Notación de esta sesión:
#   - Factor A: franjas verticales (p.ej. Labranza).
#   - Factor B: franjas horizontales (p.ej. Riego).
#   - Bloque: RCBD que se repite en el campo.

# Definimos niveles
labranza <- c("Cincel", "Disco", "Directa")   # Factor A (franjas verticales)
riego    <- c("Goteo", "Aspersion")          # Factor B (franjas horizontales)
r_bloques <- 4                               # Número de bloques (réplicas)

# Generamos el diseño strip-plot con agricolae
des_strip <- design.strip(
  trt1 = labranza,   # franjas verticales (A)
  trt2 = riego,      # franjas horizontales (B)
  r    = r_bloques,
  serie = 2          # solo para numerar parcelas (plots)
)

# Fieldbook: libro de campo resultante
book <- des_strip$book

# Vistazo rápido al libro de campo
head(book)

# Comentarios:
#   - "block": bloque (RCBD).
#   - "trt1": niveles de A (labranza) por franja vertical.
#   - "trt2": niveles de B (riego) por franja horizontal.
#   - Cada fila = una celda A×B dentro de un bloque, donde mediremos Y.


# ------------------------------------------------------------------------------
# 3. SIMULACIÓN DE RESPUESTA CON TRES ERRORES: Ea, Eb, Ec
# ------------------------------------------------------------------------------
# Objetivo:
#   - Simular una variable respuesta Y (p.ej. rendimiento).
#   - Incorporar tres componentes de error:
#       Error(a): variación entre franjas A dentro de bloque (block:trt1)
#       Error(b): variación entre franjas B dentro de bloque (block:trt2)
#       Error(c): ruido dentro de celdas A×B (residual)
#
#   - Esta estructura es la esencia del strip-plot.

# Parámetros de simulación (puedes jugar con estos valores)
mu    <- 5000  # media base (kg/ha)
amp_A <- 400   # amplitud del efecto de A (± amp_A)
amp_B <- 300   # amplitud del efecto de B (± amp_B)
amp_AB <- 250  # amplitud de la interacción A×B (± amp_AB)

sd_a <- 200    # Desv. estándar Error(a) - franjas A dentro de bloque
sd_b <- 180    # Desv. estándar Error(b) - franjas B dentro de bloque
sd_c <- 150    # Desv. estándar Error(c) - residuo dentro de celda A×B

# Niveles (coinciden con el diseño)
A_levels <- labranza
B_levels <- riego
blocks   <- sort(unique(book$block))

# 3.1 Efectos fijos biológicos (verdad del experimento)
set.seed(123)
ef_A <- runif(length(A_levels), -amp_A, amp_A)     # efecto medio de cada labranza
names(ef_A) <- A_levels

ef_B <- runif(length(B_levels), -amp_B, amp_B)     # efecto medio de cada riego
names(ef_B) <- B_levels

# Interacción A×B (matriz)
ef_AB <- matrix(
  runif(length(A_levels) * length(B_levels), -amp_AB, amp_AB),
  nrow = length(A_levels),
  dimnames = list(A_levels, B_levels)
)

ef_A
ef_B
ef_AB

# 3.2 Incorporamos los tres errores por bloque
# Creamos un data.frame de trabajo con columnas más intuitivas
datos <- book %>%
  rename(
    A = trt1,    # labranza
    B = trt2     # riego
  ) %>%
  mutate(
    A = factor(A, levels = A_levels),
    B = factor(B, levels = B_levels),
    block = factor(block)
  )

# Para cada bloque, generamos efectos aleatorios de franjas A y B
set.seed(2024)
efectos_error <- list()

for (k in blocks) {
  # Efectos aleatorios de franjas A dentro del bloque k (Error a)
  ga <- rnorm(length(A_levels), mean = 0, sd = sd_a)
  names(ga) <- A_levels
  
  # Efectos aleatorios de franjas B dentro del bloque k (Error b)
  gb <- rnorm(length(B_levels), mean = 0, sd = sd_b)
  names(gb) <- B_levels
  
  efectos_error[[as.character(k)]] <- list(ga = ga, gb = gb)
}

# Finalmente, construimos Y
set.seed(2025)
datos <- datos %>%
  rowwise() %>%
  mutate(
    # Efectos fijos
    Y_fija = mu +
      ef_A[as.character(A)] +
      ef_B[as.character(B)] +
      ef_AB[as.character(A), as.character(B)],
    
    # Efectos de franjas dentro de bloque (Errores a y b)
    err_a = efectos_error[[as.character(block)]]$ga[as.character(A)],
    err_b = efectos_error[[as.character(block)]]$gb[as.character(B)],
    
    # Error(c) residual dentro de celda A×B
    err_c = rnorm(1, mean = 0, sd = sd_c),
    
    # Respuesta simulada
    Y = Y_fija + err_a + err_b + err_c
  ) %>%
  ungroup()

# Vistazo a los datos simulados
head(datos)

# ------------------------------------------------------------------------------
# 4. ANÁLISIS EN FRANJAS CON agricolae::strip.plot
# ------------------------------------------------------------------------------
# Objetivo:
#   - Aplicar el ANOVA de franjas (tres tablas, tres errores).
#   - Ver los cuadrados medios Ea, Eb, Ec y sus g.l.
#
# Requisitos de strip.plot():
#   - BLOCK: factor de bloques.
#   - COL:   factor de franjas verticales (A).
#   - ROW:   factor de franjas horizontales (B).
#   - Y:     respuesta.

fit_strip <- strip.plot(
  BLOCK = datos$block,
  COL   = datos$A,
  ROW   = datos$B,
  Y     = datos$Y
)

# El objeto fit_strip contiene 3 ANOVAs (A, B, A×B) y los MS/gl de cada error.
names(fit_strip)

# 4.1 Tablas ANOVA para A, B e interacción A×B
cat("=== ANOVA A (franjas verticales: Labranza) ===\n")
print(fit_strip$ANOVA[[1]])

cat("\n=== ANOVA B (franjas horizontales: Riego) ===\n")
print(fit_strip$ANOVA[[2]])

cat("\n=== ANOVA A×B (interacción Labranza×Riego) ===\n")
print(fit_strip$ANOVA[[3]])

# 4.2 Estratos de error estimados
cat("\n--- Estratos de error (cuadrados medios y g.l.) ---\n")
cat("Ea (Error a, franjas A dentro de bloque): MS =", fit_strip$Ea,
    " | gl.a =", fit_strip$gl.a, "\n")
cat("Eb (Error b, franjas B dentro de bloque): MS =", fit_strip$Eb,
    " | gl.b =", fit_strip$gl.b, "\n")
cat("Ec (Error c, celdas A×B): MS =", fit_strip$Ec,
    " | gl.c =", fit_strip$gl.c, "\n")

# Comentario didáctico:
#   - El efecto de A se prueba con Ea y gl.a.
#   - El efecto de B se prueba con Eb y gl.b.
#   - La interacción A×B se prueba con Ec y gl.c (el error más fino).


# ------------------------------------------------------------------------------
# 5. TABLA DE VARIANZA POR ESTRATO (INTERPRETACIÓN DE PRECISIÓN)
# ------------------------------------------------------------------------------
# Objetivo:
#   - Traducir Ea, Eb y Ec a componentes de varianza aproximados.
#   - Entender qué proporción de variación está en cada nivel.

var_A_est <- fit_strip$Ea   # Aproximación de varianza a nivel de franjas A
var_B_est <- fit_strip$Eb   # Aproximación de varianza a nivel de franjas B
var_C_est <- fit_strip$Ec   # Varianza residual (celdas A×B)

var_total_est <- var_A_est + var_B_est + var_C_est

tabla_var <- tibble(
  Estrato      = c("Error(a): franjas A dentro de bloque",
                   "Error(b): franjas B dentro de bloque",
                   "Error(c): celdas A×B"),
  MS_aprox     = c(var_A_est, var_B_est, var_C_est),
  Porcentaje   = 100 * c(var_A_est, var_B_est, var_C_est) / var_total_est
)

tabla_var

# Lectura:
#   - Un porcentaje alto en Error(c) implica mucha variabilidad dentro de celdas.
#   - Si Error(c) es pequeño, la interacción A×B suele ser muy precisa.


# ------------------------------------------------------------------------------
# 6. COMPARACIONES MÚLTIPLES (LSD) USANDO CADA ESTRATO DE ERROR
# ------------------------------------------------------------------------------
# Objetivo:
#   - Aplicar LSD para A, B y A×B, usando el MS y gl correcto:
#       A  → Ea, gl.a
#       B  → Eb, gl.b
#       A×B → Ec, gl.c
#
#   - Conectar con la salida típica de LSD.test en trabajos agronómicos.

# 6.1 LSD para A (labranza, franjas verticales) usando Ea
lsd_A <- LSD.test(
  y       = datos$Y,
  trt     = datos$A,
  DFerror = fit_strip$gl.a,
  MSerror = fit_strip$Ea,
  console = TRUE
)

# 6.2 LSD para B (riego, franjas horizontales) usando Eb
lsd_B <- LSD.test(
  y       = datos$Y,
  trt     = datos$B,
  DFerror = fit_strip$gl.b,
  MSerror = fit_strip$Eb,
  console = TRUE
)

# 6.3 LSD para la interacción A×B usando Ec
datos <- datos %>%
  mutate(AB = interaction(A, B, drop = TRUE))

lsd_AB <- LSD.test(
  y       = datos$Y,
  trt     = datos$AB,
  DFerror = fit_strip$gl.c,
  MSerror = fit_strip$Ec,
  console = TRUE
)

# Resultados en objetos
lsd_A$groups
lsd_B$groups
lsd_AB$groups

# Comentario:
#   - Los alumnos pueden revisar las tablas $groups para ver las letras
#     de significancia por labranza, por riego y por combinación A×B.


# ------------------------------------------------------------------------------
# 7. GRÁFICOS DE RESULTADOS: INTERACCIÓN Y MAPA DE CALOR
# ------------------------------------------------------------------------------
# Objetivo:
#   - Ver gráficamente la interacción Labranza×Riego.
#   - Mostrar un mapa de calor de medias A×B (más “diseño de campo”).

# 7.1 Medias observadas Y por A×B
medias_AB <- datos %>%
  group_by(A, B) %>%
  summarise(
    mean_Y = mean(Y),
    .groups = "drop"
  )

medias_AB

# 7.2 Gráfico de interacción (líneas)
ggplot(medias_AB, aes(x = B, y = mean_Y, color = A, group = A)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title = "Interacción Labranza × Riego (medias observadas)",
    x     = "Sistema de riego (B)",
    y     = "Rendimiento medio (kg/ha)",
    color = "Labranza (A)"
  ) +
  theme_bw()

# 7.3 Mapa de calor (heatmap) de medias A×B
ggplot(medias_AB, aes(x = B, y = A, fill = mean_Y)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(mean_Y, 1)), size = 4) +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Mapa de calor de medias A×B",
    x     = "Factor B (Riego)",
    y     = "Factor A (Labranza)",
    fill  = "Rendimiento\nmedio"
  ) +
  theme_minimal(base_size = 12)

# Lectura:
#   - Líneas no paralelas en el gráfico de interacción = evidencia de interacción.
#   - El “patrón de manchas” en el heatmap ayuda a identificar combinaciones
#     A×B particularmente altas o bajas (útil para discusión agronómica).


# ------------------------------------------------------------------------------
# 8. MODELO MIXTO EQUIVALENTE (LMM) CON lmer()
# ------------------------------------------------------------------------------
# Objetivo:
#   - Mostrar cómo el strip-plot se puede modelar como un LMM:
#       Y ~ A * B + (1 | block) + (1 | block:A) + (1 | block:B)
#   - Comparar la interpretación con el ANOVA de franjas.

# Ajustamos el modelo mixto
m_lmm <- lmer(
  Y ~ A * B + (1 | block) + (1 | block:A) + (1 | block:B),
  data = datos,
  REML = TRUE
)

summary(m_lmm)

# ANOVA tipo III para A, B y A×B
anova(m_lmm, type = 3)

# Componentes de varianza (comparables con Ea, Eb, Ec)
VarCorr(m_lmm)

# Comentario:
#   - Var(block:A) ≈ componente asociado a Error(a).
#   - Var(block:B) ≈ componente asociado a Error(b).
#   - Var(residual) ≈ componente asociado a Error(c).
#   - En diseño balanceado, los resultados de strip.plot() y lmer()
#     suelen contar la misma historia (pequeñas diferencias numéricas).
