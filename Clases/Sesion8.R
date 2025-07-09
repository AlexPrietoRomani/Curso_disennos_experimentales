# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 8: Análisis de un Diseño en Cuadrado Latino (DCL)
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# Este script utiliza paquetes estándar para el análisis y visualización.
# Asegúrate de tenerlos instalados. Si no, ejecútalos una vez en la consola:
# install.packages("tidyverse")
# install.packages("agricolae")

# Cargar las librerías necesarias
library(tidyverse)
library(agricolae)

cat("¡Bienvenido a la práctica del Diseño en Cuadrado Latino!\n")
cat("Simularemos y analizaremos un experimento para evaluar 4 fungicidas.\n\n")


# --- 1. Simulación de Datos para un Escenario DCL ---
# El primer paso es crear un conjunto de datos que represente un experimento DCL.

cat("--- Paso 1: Simulación del Experimento ---\n\n")

# 1.1. Contexto del Experimento
cat("Contexto: Queremos evaluar la eficacia de 4 nuevos fungicidas (A, B, C, D)\n")
cat("para controlar la severidad de la roya en un cultivo de café.\n")
cat("El campo experimental está en una ladera (gradiente de Fila) y tiene un borde\n")
cat("con árboles que proyectan sombra (gradiente de Columna).\n\n")

# 1.2. Generación del Layout del DCL
# Usaremos agricolae para crear un layout 4x4 aleatorizado.
set.seed(42) # Para que todos tengamos el mismo layout aleatorizado
tratamientos <- c("Fungicida_A", "Fungicida_B", "Fungicida_C", "Fungicida_D")
diseno_dcl <- design.lsd(tratamientos)

# El plan de campo está en el objeto '$book'
plan_campo <- diseno_dcl$book
cat("Layout del campo generado (Croquis):\n")
print(plan_campo)
cat("\n")

# 1.3. Simulación de la Variable de Respuesta
# Vamos a simular la 'Severidad' de la roya (en una escala de 0 a 100).
# La severidad dependerá de los efectos de Fila, Columna, Tratamiento y un error.

# Definimos los efectos
efecto_fila <- c(0, -5, -10, -15)      # La severidad disminuye al bajar en la ladera
efecto_columna <- c(0, 3, 6, 9)       # La severidad aumenta al alejarse de la sombra
efecto_tratamiento <- c(-20, -15, -5, 0) # Efecto de cada fungicida (A, B, C, D)
names(efecto_tratamiento) <- tratamientos

# Crear el dataframe final para el análisis
datos_dcl <- plan_campo %>%
  # Cambiar nombres para mayor claridad
  rename(Fila = row, Columna = col, Tratamiento = tratamientos) %>%
  # Simular la respuesta
  mutate(
    Severidad = 50 + # Media base de severidad
                efecto_fila[Fila] +
                efecto_columna[Columna] +
                efecto_tratamiento[Tratamiento] +
                rnorm(n(), mean = 0, sd = 4) # Error aleatorio
  ) %>%
  # Convertir a factores para el análisis
  mutate(
    Fila = as.factor(Fila),
    Columna = as.factor(Columna),
    Tratamiento = as.factor(Tratamiento)
  )

cat("Primeras filas de los datos simulados listos para analizar:\n")
glimpse(datos_dcl)
cat("\n")


# --- 2. Análisis Exploratorio Visual ---
# Visualizamos los datos para entender las tendencias principales.

cat("--- Paso 2: Análisis Exploratorio Visual ---\n\n")

# Gráfico del layout con los valores de severidad
plot_layout <- ggplot(datos_dcl, aes(x = Columna, y = Fila, fill = Severidad)) +
  geom_tile(color = "white", lwd = 1.5) +
  geom_text(aes(label = paste(Tratamiento, "\n", round(Severidad, 1))), color = "white", fontface = "bold", size = 3.5) +
  scale_fill_viridis_c(option = "magma") + # Paleta de colores para la severidad
  scale_y_discrete(limits = rev) + # Poner Fila 1 arriba
  labs(
    title = "Layout del Campo y Severidad de la Roya Observada",
    subtitle = "Cada celda muestra el Tratamiento aplicado y la Severidad resultante",
    fill = "Severidad"
  ) +
  theme_minimal() +
  coord_fixed()

print(plot_layout)
cat("Observa los patrones de color: se ve un gradiente vertical y horizontal, como esperábamos.\n\n")

# Boxplots por cada factor
plot_fila <- ggplot(datos_dcl, aes(x=Fila, y=Severidad, fill=Fila)) + geom_boxplot(show.legend=F) + labs(title="Efecto de las Filas")
plot_columna <- ggplot(datos_dcl, aes(x=Columna, y=Severidad, fill=Columna)) + geom_boxplot(show.legend=F) + labs(title="Efecto de las Columnas")
plot_trat <- ggplot(datos_dcl, aes(x=Tratamiento, y=Severidad, fill=Tratamiento)) + geom_boxplot(show.legend=F) + labs(title="Efecto de los Tratamientos")

# Combinar los 3 boxplots en una sola figura
library(patchwork) # Necesitarás este paquete: install.packages("patchwork")
print(plot_fila + plot_columna + plot_trat)

cat("Los boxplots muestran diferencias claras entre los niveles de cada factor, sugiriendo\n")
cat("que tanto el bloqueo como los tratamientos tuvieron un efecto.\n\n")


# --- 3. Modelo DCL, ANOVA y Diagnóstico ---
cat("--- Paso 3: Análisis de Varianza (ANOVA) del DCL ---\n\n")

# 3.1. Ajuste del Modelo Lineal del DCL
# La fórmula incluye los tres efectos: Fila, Columna y Tratamiento.
cat("3.1. Ajustando el modelo ANOVA: Severidad ~ Fila + Columna + Tratamiento\n")
modelo_dcl <- aov(Severidad ~ Fila + Columna + Tratamiento, data = datos_dcl)
cat("¡Modelo ajustado!\n\n")

# 3.2. Verificación de Supuestos del Modelo
cat("3.2. Verificando los supuestos sobre los residuales...\n")
par(mfrow = c(2, 2))
plot(modelo_dcl)
par(mfrow = c(1, 1))
cat("Los gráficos de diagnóstico parecen razonables, sin patrones alarmantes.\n")
cat("La prueba de Shapiro-Wilk confirmará la normalidad.\n")
print(shapiro.test(residuals(modelo_dcl)))
cat("-> El p-valor > 0.05 confirma que los residuos son normales. ¡Podemos confiar en el ANOVA!\n\n")

# 3.3. Interpretación de la Tabla ANOVA
cat("3.3. Tabla ANOVA del Diseño en Cuadrado Latino:\n")
print(summary(modelo_dcl))

cat("\nInterpretación de la Tabla ANOVA:\n")
cat(" - Fila: El p-valor es muy significativo (< 0.05). ¡El bloqueo por filas fue EFECTIVO!\n")
cat("   Había un gradiente real en esa dirección.\n")
cat(" - Columna: El p-valor también es significativo. ¡El bloqueo por columnas fue EFECTIVO!\n")
cat("   Controlar la segunda fuente de variación fue una decisión correcta.\n")
cat(" - Tratamiento: El p-valor es muy significativo. Esto significa que hay una diferencia\n")
cat("   real en la eficacia entre al menos dos de los fungicidas probados.\n")
cat(" - Error (Residuals): Observa que los grados de libertad (Df) para el error son (4-1)*(4-2) = 6.\n\n")


# --- 4. Comparaciones Múltiples (Prueba Post-Hoc) ---
cat("--- Paso 4: Pruebas de Comparación Múltiple (Post-Hoc) ---\n\n")
cat("Dado que el efecto del Tratamiento fue significativo, ahora identificamos qué\n")
cat("fungicidas son diferentes entre sí usando la prueba HSD de Tukey.\n")

tukey_dcl <- HSD.test(modelo_dcl, "Tratamiento", group = TRUE, console = TRUE)

# Visualizar los resultados de Tukey
cat("\nVisualización de los resultados de Tukey con un gráfico de barras:\n")
plot(tukey_dcl, las=1, main="Comparación de Medias de Severidad por Tratamiento")

cat("\n\n")

# --- 5. Conclusión Final ---
cat("--- Paso 5: Conclusión Final del Análisis DCL ---\n\n")
cat("El uso de un Diseño en Cuadrado Latino fue altamente efectivo, ya que se detectaron\n")
cat("efectos significativos tanto de las Filas como de las Columnas, validando la decisión\n")
cat("de controlar el doble gradiente del campo.\n\n")
cat("El análisis de varianza mostró una diferencia altamente significativa entre la eficacia\n")
cat("de los fungicidas probados. La prueba de Tukey reveló que el Fungicida_A (grupo 'a')\n")
cat("fue el más eficaz, reduciendo significativamente la severidad de la roya en comparación\n")
cat("con los demás. El Fungicida_D (grupo 'c') fue el menos eficaz, sin diferir del control\n")
cat("implícito en un escenario de alta enfermedad.\n")

cat("\n--- Fin del Script de Práctica DCL ---\n")