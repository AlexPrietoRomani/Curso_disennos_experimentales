# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 5: Análisis de un DBCA con Datos Agronómicos
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# install.packages("tidyverse")
# install.packages("car")
# install.packages("agricolae")
# install.packages("rstatix")

library(tidyverse)
library(car)
library(agricolae)
library(rstatix)

# --- 1. Carga e Inspección de Datos ---

cat("--- Paso 1: Carga e Inspección Inicial de los Datos ---\n\n")
cat("1.1. Copia la tabla de datos de Excel e ingrésala usando read.delim('clipboard')\n")

datos <- read.delim("clipboard")

cat("1.2. Inspección básica\n")
cat("Primeras 6 filas:\n")
print(head(datos))
cat("\nDimensiones del dataframe:\n")
print(dim(datos))
cat("\nNombres de columnas:\n")
print(colnames(datos))

cat("1.3. Estructura y tipos de datos:\n")
str(datos)
cat("\nVistazo rápido con glimpse():\n")
glimpse(datos)

cat("1.4. Conversión de variables a factor:\n")
# En DBCA, suele haber una variable de bloque ("REP", "BLOQUE", etc.) y el tratamiento.
datos$REP <- as.factor(datos$REP)
datos$INSTN <- as.factor(datos$INSTN) # Si usas INSTN como tratamiento principal
# Si tienes otra columna como tratamiento principal, cambia aquí
cat("Columnas REP (bloque) e INSTN (tratamiento) convertidas a factor.\n")

cat("1.5. Verificación de NAs:\n")
print(colSums(is.na(datos)))

# --- 2. Resumen Estadístico Descriptivo ---

cat("\n--- Paso 2: Estadística Descriptiva por Tratamiento ---\n")
resumen_dbca <- datos %>%
  group_by(INSTN) %>%
  summarise(
    N = n(),
    Media = mean(TWTP, na.rm = TRUE),
    Mediana = median(TWTP, na.rm = TRUE),
    DE = sd(TWTP, na.rm = TRUE),
    CV = sd(TWTP, na.rm = TRUE) / mean(TWTP, na.rm = TRUE) * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print(as.data.frame(resumen_dbca))

cat("Interpretación: La media y la mediana te muestran tendencia central; la DE y el CV, dispersión.\n")

# --- 3. Visualización Exploratoria ---

cat("\n--- Paso 3: Visualización Exploratoria ---\n")

cat("3.1. Histograma general de TWTP\n")
hist_plot <- ggplot(datos, aes(x = TWTP)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(title = "Histograma general de TWTP", x = "TWTP", y = "Frecuencia") +
  theme_minimal()
print(hist_plot)

cat("3.2. Boxplot comparativo por tratamiento\n")
box_plot <- ggplot(datos, aes(x = INSTN, y = TWTP, fill = INSTN)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
  labs(title = "Distribución de TWTP por Tratamiento", x = "Tratamiento (INSTN)", y = "TWTP") +
  theme_minimal() + theme(legend.position = "none")
print(box_plot)

cat("3.3. Boxplot por bloque\n")
box_plot_rep <- ggplot(datos, aes(x = REP, y = TWTP, fill = REP)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Distribución de TWTP por Bloque", x = "Bloque (REP)", y = "TWTP") +
  theme_minimal() + theme(legend.position = "none")
print(box_plot_rep)

# --- 4. ANOVA de DBCA y verificación de supuestos ---

cat("\n--- Paso 4: ANOVA de DBCA y Diagnóstico de Supuestos ---\n")

cat("4.1. Ajuste del modelo ANOVA DBCA: TWTP ~ INSTN + REP\n")
modelo_dbca <- aov(TWTP ~ INSTN + REP, data = datos)
cat("¡Modelo ajustado!\n")

cat("4.2. Diagnóstico gráfico de supuestos\n")
par(mfrow = c(2,2))
plot(modelo_dbca)
par(mfrow = c(1,1))

cat("Interpretación: Observa la normalidad y homogeneidad en los gráficos de residuos.\n")

cat("4.3. Prueba formal de normalidad (Shapiro-Wilk) de los residuos\n")
print(shapiro.test(residuals(modelo_dbca)))

cat("4.4. Prueba formal de homogeneidad de varianzas (Levene)\n")
print(leveneTest(TWTP ~ INSTN, data = datos))

cat("4.5. Tabla ANOVA DBCA\n")
print(summary(modelo_dbca))

# --- 5. Pruebas Post-Hoc (si es significativo) ---

cat("\n--- Paso 5: Prueba Post-Hoc de comparaciones múltiples ---\n")

# Si el p-valor para INSTN en la tabla ANOVA es < 0.05, puedes hacer pruebas post-hoc:
cat("Usando test de Tukey HSD (agricolae)\n")
tukey <- HSD.test(modelo_dbca, "INSTN", group=TRUE)
print(tukey$groups)
print(tukey$statistics)

cat("Interpretación: Las letras distintas indican diferencias estadísticas.\n")

# --- 6. Alternativa No Paramétrica (si no se cumplen los supuestos) ---

cat("\n--- Paso 6: Alternativa No Paramétrica (Kruskal-Wallis por bloque) ---\n")
kruskal <- kruskal(TWTP, INSTN, REP, data = datos, group=TRUE, console=TRUE)
print(kruskal$groups)
cat("Interpretación: Las letras distintas indican diferencias estadísticas por el método no paramétrico.\n")

cat("\n--- Fin del Script de Práctica DBCA ---\n")
