# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 6: Diseños Factoriales y el Poder de la Interacción
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# Este script utiliza paquetes estándar para el análisis factorial.
# Asegúrate de tenerlos instalados. Si no, ejecútalos una vez en la consola:
# install.packages("tidyverse")
# install.packages("MASS")      # Contiene el dataset 'oats'
# install.packages("emmeans")   # Para análisis post-hoc moderno
# install.packages("car")       # Para la prueba de Levene

# Cargar las librerías necesarias
library(tidyverse)
library(MASS)
library(emmeans)
library(car)
library(agricolae)
library(rstatix)

cat("¡Bienvenido a la práctica de la Sesión 6!\n")
cat("Exploraremos los diseños factoriales usando el dataset 'oats'.\n\n")

# --- 1. ¿Por qué Diseños Factoriales? El Dataset 'oats' ---
# Los diseños factoriales nos permiten estudiar múltiples factores y, crucialmente,
# sus interacciones, algo imposible con un enfoque de "un factor a la vez".

# 1.1. Carga y Preparación del Dataset 'oats'
cat("--- Paso 1: Carga y Preparación de los Datos ---\n\n")

# Cargamos el dataset 'oats' de la librería MASS
datos_avena <- MASS::oats

# Vamos a limpiar y renombrar las variables para que sean más intuitivas.
# Este es un paso fundamental en cualquier análisis.
datos_avena_limpios <- datos_avena %>%
  mutate(
    bloque = B,
    variedad = V,
    nitrogeno = N,
    rendimiento = Y
  ) %>%
  # La variable 'N' (nitrógeno) es numérica, pero en este experimento se usó
  # en niveles discretos. La convertiremos a factor para el ANOVA.
  mutate(
    nitrogeno = factor(nitrogeno, labels = c("0 cwt/acre", "0.2 cwt/acre", "0.4 cwt/acre", "0.6 cwt/acre"))
  ) %>%
  dplyr::select(bloque, variedad, nitrogeno, rendimiento) # Seleccionamos las columnas de interés

cat("Estructura de los datos limpios (con glimpse()):\n")
glimpse(datos_avena_limpios)
cat("\nResumen del diseño:\n")
cat(" - Bloques (B):", length(levels(datos_avena_limpios$bloque)), "niveles\n")
cat(" - Variedades (V):", length(levels(datos_avena_limpios$variedad)), "niveles\n")
cat(" - Dosis de Nitrógeno (N):", length(levels(datos_avena_limpios$nitrogeno)), "niveles\n")
cat(paste0("-> Este es un diseño factorial ", length(levels(datos_avena_limpios$variedad)), 
           "x", length(levels(datos_avena_limpios$nitrogeno)), " en un DBCA.\n\n"))


# --- 2. Visualización Exploratoria: La Búsqueda de la Interacción ---

cat("--- Paso 2: Visualización Exploratoria ---\n")
cat("El gráfico de interacción es la herramienta más poderosa para visualizar un factorial.\n")
cat("Buscamos líneas que NO sean paralelas. Si se cruzan, la interacción es fuerte.\n\n")

plot_interaccion <- ggplot(datos_avena_limpios, aes(x = nitrogeno, y = rendimiento, group = variedad, color = variedad)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) + # Línea que une las medias
  stat_summary(fun = mean, geom = "point", size = 3) +   # Puntos en cada media
  labs(
    title = "Gráfico de Interacción: Rendimiento de Avena",
    subtitle = "Efecto de la Dosis de Nitrógeno para cada Variedad",
    x = "Nivel de Nitrógeno Aplicado",
    y = "Rendimiento Promedio (bushels/acre)",
    color = "Variedad de Avena"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

print(plot_interaccion)

cat("\nInterpretación Visual Preliminar:\n")
cat(" - Las líneas no son paralelas. La línea de 'Golden Rain' tiene una pendiente más pronunciada\n")
cat("   que las otras, especialmente al pasar de 0 a 0.2 cwt/acre.\n")
cat(" - La variedad 'Marvellous' parece tener un rendimiento consistentemente bajo.\n")
cat(" - Esta falta de paralelismo es una pista visual de que podría existir una\n")
cat("   interacción significativa entre 'variedad' y 'nitrogeno'.\n\n")


# --- 3. Modelo Factorial en DBCA y Diagnóstico ---

cat("--- Paso 3: Modelo Lineal del Factorial en DBCA y Diagnóstico de Supuestos ---\n\n")

# 3.1. Ajuste del Modelo Lineal
# La fórmula incluye el bloque, y los efectos principales e interacción de los factores.
# 'bloque + variedad * nitrogeno' es un atajo para 'bloque + variedad + nitrogeno + variedad:nitrogeno'
cat("3.1. Ajustando el modelo ANOVA: rendimiento ~ bloque + variedad * nitrogeno\n")
modelo_factorial <- aov(rendimiento ~ bloque + variedad * nitrogeno, data = datos_avena_limpios)
cat("¡Modelo ajustado!\n\n")

# 3.2. Verificación de Supuestos del Modelo
cat("3.2. Verificando los supuestos sobre los residuales del modelo...\n")
# Gráficos de diagnóstico
par(mfrow = c(2, 2))
plot(modelo_factorial)
par(mfrow = c(1, 1))

# Pruebas formales
cat("\nPrueba de Shapiro-Wilk para Normalidad de Residuos (H0: son normales):\n")
print(shapiro.test(residuals(modelo_factorial)))
cat("\nPrueba de Levene para Homogeneidad de Varianzas (H0: son homogéneas):\n")
print(leveneTest(rendimiento ~ variedad * nitrogeno, data = datos_avena_limpios))

cat("\nConclusión de Supuestos:\n")
cat(" - Ambas pruebas formales tienen p-valores > 0.05. No hay evidencia para rechazar\n")
cat("   los supuestos de normalidad y homocedasticidad. Podemos proceder con confianza.\n\n")


# --- 4. Interpretación de la Tabla ANOVA y Análisis Post-Hoc ---

cat("--- Paso 4: Interpretación de la Tabla ANOVA y Análisis Post-Hoc ---\n\n")

# 4.1. Interpretación de la Tabla ANOVA
cat("4.1. Tabla ANOVA del modelo factorial:\n")
print(summary(modelo_factorial))

cat("\nInterpretación Estratégica de la Tabla ANOVA:\n")
cat(" 1. ¡MIRA LA INTERACCIÓN PRIMERO! El término 'variedad:nitrogeno' tiene un p-valor de 0.015.\n")
cat("    Como es < 0.05, la interacción es ESTADÍSTICAMENTE SIGNIFICATIVA.\n")
cat(" 2. ¡IGNORA LOS EFECTOS PRINCIPALES! Dado que la interacción es significativa, interpretar\n")
cat("    los efectos principales de 'variedad' y 'nitrogeno' por separado sería engañoso.\n")
cat("    El efecto de una variedad DEPENDE del nivel de nitrógeno, y viceversa.\n")
cat(" 3. La conclusión principal es: necesitamos desglosar y entender la interacción.\n\n")


# 4.2. Desglose de la Interacción con `emmeans` (Análisis Post-Hoc)
cat("4.2. Desglosando la interacción con el paquete 'emmeans'...\n")
cat("   Esto se conoce como analizar los 'efectos simples'.\n\n")

# Pregunta 1: ¿Cómo se comportan las variedades DENTRO de cada nivel de nitrógeno?
cat("--- Comparación de Variedades para cada Nivel de Nitrógeno ---\n")
emm_variedad_en_N <- emmeans(modelo_factorial, pairwise ~ variedad | nitrogeno)
print(emm_variedad_en_N$contrasts) # Usar $contrasts para ver solo la tabla de comparaciones

cat("\nInterpretación:\n")
cat(" - Para N='0 cwt/acre': No hay diferencias significativas entre las variedades (todos los p > 0.05).\n")
cat(" - Para N='0.2 cwt/acre': 'Golden Rain' es significativamente superior a 'Marvellous'.\n")
cat(" - Para N='0.4 cwt/acre' y '0.6 cwt/acre': Las diferencias se magnifican.\n")
cat("   Esta es la interacción en acción: el efecto de la variedad depende del nitrógeno.\n\n")

# Visualizar estas comparaciones
cat("Visualizando los efectos simples...\n")
plot_emm_variedad_en_N <- plot(emm_variedad_en_N, comparisons = TRUE) + 
  labs(title = "Comparación de Variedades en cada Nivel de N")
print(plot_emm_variedad_en_N)
cat("\n-> Las flechas rojas indican diferencias significativas.\n\n")


# Pregunta 2: ¿Cómo responde cada variedad a los diferentes niveles de nitrógeno?
cat("--- Comparación de Niveles de Nitrógeno para cada Variedad ---\n")
emm_N_en_variedad <- emmeans(modelo_factorial, pairwise ~ nitrogeno | variedad)
print(emm_N_en_variedad$contrasts)

cat("\nInterpretación:\n")
cat(" - Para 'Golden Rain': Todos los niveles de N son significativamente mejores que el control (0 cwt/acre).\n")
cat(" - Para 'Marvellous': La respuesta al nitrógeno es mucho menor, casi insignificante.\n")
cat("   ¡Otra vez, la interacción! La respuesta al fertilizante depende de la variedad.\n\n")


# --- 5. Visualización Avanzada: Enfoque del "Super-Factor" ---

cat("--- Paso 5: Visualización con el Enfoque del 'Super-Factor' ---\n")
cat("Cuando una interacción es significativa, una técnica poderosa es combinar\n")
cat("los factores en una sola variable para realizar una única prueba de Tukey\n")
cat("sobre todas las combinaciones de tratamiento.\n\n")

# -----------------------------------------------------------------------------
# Paso 5.1: Creación de la Variable de Combinación (el "Super-Factor")
# -----------------------------------------------------------------------------
cat("5.1. Creando una nueva variable que une 'variedad' y 'nitrogeno'...\n")

# Usamos `unite()` de `tidyr` para combinar las dos columnas en una nueva.
# 'remove = FALSE' mantiene las columnas originales.
datos_con_superfactor <- datos_avena_limpios %>%
  unite(
    col = "combinacion",          # Nombre de la nueva columna
    c(variedad, nitrogeno),       # Columnas a unir
    sep = " - ",                  # Separador a usar
    remove = FALSE                # Mantener las columnas originales
  ) %>%
  mutate(combinacion = as.factor(combinacion)) # Asegurarse de que sea un factor

cat("Vistazo a los datos con la nueva columna 'combinacion':\n")
glimpse(datos_con_superfactor)
cat("\n")


# -----------------------------------------------------------------------------
# Paso 5.2: ANOVA de una vía sobre el "Super-Factor" y Prueba de Tukey
# -----------------------------------------------------------------------------
cat("5.2. Realizando un ANOVA de una vía sobre la nueva variable 'combinacion'...\n")
# Ahora tratamos el problema como un simple DBCA donde 'combinacion' es el tratamiento principal.
# El modelo es: respuesta ~ bloque + super_factor
modelo_superfactor <- aov(rendimiento ~ bloque + combinacion, data = datos_con_superfactor)

# La tabla ANOVA debería dar un resultado muy significativo para 'combinacion',
# confirmando que hay diferencias entre las 12 combinaciones.
cat("Tabla ANOVA del modelo con el 'Super-Factor':\n")
print(summary(modelo_superfactor))
cat("\n")

# Ahora realizamos una única prueba de Tukey sobre este modelo.
cat("Realizando la prueba HSD de Tukey sobre todas las combinaciones...\n")
tukey_superfactor <- HSD.test(modelo_superfactor, "combinacion", group = TRUE, console = FALSE)

# Extraer los datos limpios para graficar
tukey_data_plot <- as.data.frame(tukey_superfactor$groups)
tukey_data_plot <- tukey_data_plot %>%
  rownames_to_column(var = "combinacion") %>%
  rename(Media = rendimiento, Grupos = groups)

cat("Tabla de medias y letras de significancia para cada combinación:\n")
print(tukey_data_plot)
cat("\n")


# -----------------------------------------------------------------------------
# Paso 5.3: Visualización de los Resultados del "Super-Factor"
# -----------------------------------------------------------------------------
cat("5.3. Creando gráficos para visualizar las comparaciones de todas las combinaciones...\n\n")

# Gráfico 1: Gráfico de Barras Ordenado
# Este es el gráfico más claro para este enfoque.
plot_barras_superfactor <- ggplot(tukey_data_plot, aes(x = reorder(combinacion, -Media), y = Media)) +
  geom_bar(stat = "identity", fill = "darkcyan", color = "black", alpha = 0.8) +
  geom_text(
    aes(label = Grupos),
    vjust = -0.5,
    fontface = "bold",
    color = "black",
    size = 4
  ) +
  labs(
    title = "Comparación de Todas las Combinaciones de Tratamiento (Variedad x Nitrógeno)",
    subtitle = "Prueba HSD de Tukey (α = 0.05). Letras distintas indican diferencias significativas.",
    x = "Combinación de Tratamiento (Variedad - Nivel de Nitrógeno)",
    y = "Media del Rendimiento (TWTP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(tukey_data_plot$Media) * 1.15)

print(plot_barras_superfactor)

