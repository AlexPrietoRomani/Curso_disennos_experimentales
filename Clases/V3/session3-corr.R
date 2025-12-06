# ==============================================================================
# TALLER: Análisis de Correlación (Pearson vs Spearman)
# Contexto: Variables Agronómicas (Simulación)
# Objetivo: Entender la relación entre variables y detectar trampas estadísticas.
# ==============================================================================

# 1. LIBRERÍAS
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("corrplot")) install.packages("corrplot") # Para matrices visuales

library(ggplot2)
library(dplyr)
library(corrplot)

# ==============================================================================
# PARTE 1: GENERACIÓN DE DATOS (Escenario Agronómico)
# ==============================================================================
set.seed(42)
n <- 50

# Simulamos variables comunes en un ensayo de suelos/cultivos
# - Nitrogeno: Variable independiente
# - Rendimiento: Depende linealmente del Nitrógeno
# - pH: Variable independiente (rango ácido a neutro)
# - Materia_Organica: Relacionada positivamente con N (colinealidad)
# - Enfermedad: Relación NO LINEAL con humedad (para probar Spearman)

df_agro <- data.frame(
  Nitrogeno = runif(n, 50, 200),
  pH = rnorm(n, 6.5, 0.5)
)

# Relaciones:
# 1. Lineal fuerte: Rendimiento aumenta con N
df_agro$Rendimiento <- 20 + 0.5 * df_agro$Nitrogeno + rnorm(n, 0, 10)

# 2. Relación Curva (Monótona): Severidad de enfermedad vs Humedad
# (Simulamos humedad y una respuesta exponencial)
df_agro$Humedad <- runif(n, 30, 90)
df_agro$Severidad <- exp(0.05 * df_agro$Humedad) + rnorm(n, 0, 5)

# 3. Outlier artificial (El error de digitación)
# Introducimos un dato que arruina la correlación lineal
df_agro[1, "Nitrogeno"] <- 500  # Valor extremo
df_agro[1, "Rendimiento"] <- 10 # Valor bajo (contradictorio)

cat(">>> Resumen de Datos Simulados <<<\n")
View(df_agro)

# ==============================================================================
# PARTE 2: CORRELACIÓN DE PEARSON (La estándar)
# Supuesto Crítico: Relación LINEAL y Normalidad.
# Sensible a outliers.
# ==============================================================================

cat("\n--- 1. ANÁLISIS DE PEARSON (N vs Rendimiento) ---\n")

# Gráfico previo (Siempre graficar antes de calcular)
p1 <- ggplot(df_agro, aes(x = Nitrogeno, y = Rendimiento)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Pearson: Sensibilidad a Outliers",
       subtitle = "Observe el punto extremo a la derecha. ¿Cómo afecta a la recta?") +
  theme_bw()

print(p1)

# Cálculo del coeficiente y prueba de hipótesis
test_pearson <- cor.test(df_agro$Nitrogeno, df_agro$Rendimiento, method = "pearson")

cat("Coeficiente de Pearson (r):", round(test_pearson$estimate, 3), "\n")
cat("P-valor:", format.pval(test_pearson$p.value, digits = 4), "\n")
cat("Nota Crítica: Si hay outliers, Pearson subestima o sobreestima la relación real.\n")

# ==============================================================================
# PARTE 3: CORRELACIÓN DE SPEARMAN (La robusta)
# Basada en RANGOS (Orden). No asume linealidad, solo monotonicidad.
# ==============================================================================

cat("\n--- 2. ANÁLISIS DE SPEARMAN (Humedad vs Severidad) ---\n")

# Gráfico de una relación Curva
p2 <- ggplot(df_agro, aes(x = Humedad, y = Severidad)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "orange") +
  labs(title = "Spearman: Relaciones No Lineales",
       subtitle = "La relación es curva (exponencial). Pearson fallaría en medir la fuerza real.") +
  theme_bw()

print(p2)

# Comparación Crítica
r_pearson_curva <- cor(df_agro$Humedad, df_agro$Severidad, method = "pearson")
r_spearman_curva <- cor(df_agro$Humedad, df_agro$Severidad, method = "spearman")

cat("Comparación en relación curva:\n")
cat("Pearson (r):", round(r_pearson_curva, 3), "(Intenta ajustar una recta)\n")
cat("Spearman (rho):", round(r_spearman_curva, 3), "(Captura el orden creciente)\n")
cat("Conclusión: Spearman es superior cuando la relación es curva pero monótona.\n")

# ==============================================================================
# PARTE 4: MATRIZ DE CORRELACIÓN (Todas contra todas)
# ==============================================================================

cat("\n--- 3. MATRIZ DE CORRELACIÓN VISUAL ---\n")

# Calculamos la matriz completa (usamos Spearman para ser conservadores)
M <- cor(df_agro, method = "pearson")

# Visualización con corrplot (Muy usado en publicaciones)
# Tipos: "circle", "square", "ellipse", "number"
corrplot(M, method = "ellipse", type = "upper", 
         order = "hclust", # Agrupa variables similares (cluster)
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", # Agrega los números
         title = "Matriz de Correlación de Spearman", mar = c(0,0,2,0))

# ==============================================================================
# PARTE 5: CHECKLIST DE INTERPRETACIÓN
# ==============================================================================
cat("\n>>> GUÍA DE INTERPRETACIÓN <<<\n")
cat("1. Magnitud: \n")
cat("   0.0 - 0.3: Despreciable\n")
cat("   0.3 - 0.5: Baja\n")
cat("   0.5 - 0.7: Moderada\n")
cat("   0.7 - 0.9: Alta\n")
cat("   0.9 - 1.0: Muy alta (Cuidado con colinealidad)\n")
cat("2. Signo: (+) Aumentan juntas, (-) Una sube, la otra baja.\n")
cat("3. P-valor: Si p < 0.05, la correlación es significativamente distinta de cero.\n")
cat("4. Causalidad: CORRELACIÓN NO IMPLICA CAUSALIDAD. \n   (Ej: N y Rendimiento correlacionan, pero el N *causa* el rendimiento).\n")