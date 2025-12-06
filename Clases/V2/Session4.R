# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 4: Análisis de un DCA con Datos de Plagas
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# Este script asume que los paquetes ya están instalados.
# Si no, ejecútalos una vez en la consola:
# install.packages("tidyverse")
# install.packages("car")
# install.packages("agricolae") # Muy útil para agronomía
# install.packages("rstatix")

# Cargar las librerías necesarias
library(tidyverse) # Para manipulación de datos (dplyr) y gráficos (ggplot2)
library(car)       # Para la prueba de Levene
library(agricolae) # Para pruebas post-hoc agronómicas como Duncan o LSD
library(rstatix)   # Para la prueba de Dunn (post-hoc no paramétrica)

# --- 1. Creación e Inspección de Datos ---
# El primer paso en cualquier análisis es cargar y entender la estructura básica de tus datos.

cat("--- Paso 1: Carga, Inspección y Descripción Inicial de los Datos ---\n\n")

# 1.1. Carga de Datos
# Para esta práctica, usaremos `read.delim("clipboard")`. Esto es muy útil para
# copiar y pegar rápidamente datos desde Excel, una página web o un documento.
# INSTRUCCIONES: Selecciona y copia (Ctrl+C) toda la tabla de datos que te proporcioné.
# Luego, ejecuta la siguiente línea en R.
cat("1.1. Cargando datos desde el portapapeles...\n")
cat("     Asegúrate de haber copiado la tabla de datos completa antes de ejecutar.\n")
datos <- read.delim("clipboard")


# 1.2. Inspección Inicial de la Estructura
cat("1.2. Inspección Inicial de la Estructura del Dataframe\n\n")

# `head()` y `tail()`: Muestran las primeras y últimas 6 filas.
# Esencial para verificar que los datos se cargaron correctamente y ver el formato.
cat("Primeras 6 filas del dataframe (con head()):\n")
print(head(datos))
cat("\nÚltimas 6 filas del dataframe (con tail()):\n")
print(tail(datos))


# `dim()`: Devuelve las dimensiones del dataframe (filas, columnas).
# Útil para saber el tamaño de tu dataset de un vistazo.
cat("Dimensiones del dataframe (filas, columnas):\n")
print(dim(datos))


# `colnames()` y `names()`: Muestran los nombres de las columnas.
# Clave para verificar que los encabezados se leyeron bien y para referenciar columnas.
cat("Nombres de las columnas (con colnames()):\n")
print(colnames(datos))


# 1.3. Exploración Detallada de Tipos de Datos y Contenido
cat("1.3. Exploración Detallada de los Tipos de Variables\n\n")

# `str()`: Muestra la "estructura" del objeto. Es una de las funciones más útiles.
# Te dice el tipo de cada columna (int, num, chr, Factor) y muestra algunos valores de ejemplo.
cat("Estructura detallada del dataframe (con str()):\n")
str(datos)


# `glimpse()`: Una versión mejorada de `str()` del paquete `dplyr` (parte de `tidyverse`).
# Es más compacta y a menudo más fácil de leer.
cat("Vistazo rápido y limpio con glimpse() de dplyr:\n")
glimpse(datos)


# 1.4. Corrección de Tipos de Datos
# A menudo, R no interpreta los tipos de datos como queremos. `glimpse()` y `str()`
# nos mostraron que TRATAMIENTO es un 'chr' (caracter), pero para nuestro análisis,
# debe ser una variable categórica (un 'Factor').
cat("1.4. Corrección de Tipos de Datos\n\n")
cat("La columna 'TRATAMIENTO' es de tipo 'character'. La convertiremos a 'factor'...\n")
datos$TRATAMIENTO <- as.factor(datos$TRATAMIENTO)

glimpse(datos[, "TRATAMIENTO", drop = FALSE]) # Muestra solo la columna corregida


# 1.5. Resumen Estadístico y Verificación de Calidad
cat("1.5. Resumen Estadístico y Verificación de Calidad\n\n")

# a) Resumen General con `summary()`
# `summary()`: Proporciona un resumen estadístico rápido para cada columna.
# Para columnas numéricas: Mínimo, 1er Cuartil, Mediana, Media, 3er Cuartil, Máximo.
# Para columnas de factores: Conteo de observaciones por cada nivel (repeticiones).
cat("a) Resumen general con summary():\n")
print(summary(datos))
cat("\nInterpretación del Resumen:\n")
cat(" - Para 'TRATAMIENTO', confirma que nuestro diseño está balanceado, con 40 repeticiones por cada tratamiento.\n")
cat(" - Para 'ADULTO', la media (Mean) es 0.65, pero la mediana (Median) es 0.00. Esta gran\n")
cat("   diferencia entre la media y la mediana es un fuerte indicador de una distribución asimétrica\n")
cat("   a la derecha (la media es 'arrastrada' hacia arriba por unos pocos valores altos).\n\n")

# b) Cálculo de Medidas Descriptivas por Tratamiento
# Para un análisis más profundo, es útil calcular las medidas descriptivas para nuestra
# variable de interés ('ADULTO') para cada nivel del factor ('TRATAMIENTO').
# Usaremos `dplyr` para esto, que es muy potente para operaciones "agrupar y resumir".
cat("b) Medidas descriptivas para 'ADULTO' por cada tratamiento:\n")

resumen_numerico <- datos %>%
  group_by(TRATAMIENTO) %>%
  summarise(
    N_Repeticiones = n(),
    Media = mean(ADULTO, na.rm = TRUE),
    Mediana = median(ADULTO, na.rm = TRUE),
    Desviacion_Estandar = sd(ADULTO, na.rm = TRUE),
    Coef_Variacion_CV_porc = (sd(ADULTO, na.rm = TRUE) / mean(ADULTO, na.rm = TRUE)) * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) # Redondear todos los resultados numéricos a 2 decimales

print(as.data.frame(resumen_numerico)) # Imprimir como dataframe para mejor formato en consola
cat("\n")

# c) Interpretación Agronómica de las Medidas Descriptivas
cat("c) Interpretación Agronómica de las Medidas Descriptivas:\n\n")

cat(" - MEDIA vs. MEDIANA:\n")
cat("   La media representa el promedio aritmético, mientras que la mediana es el valor central.\n")
cat("   En T1, la media (1.45) es mayor que la mediana (1.00), lo que sugiere asimetría.\n")
cat("   En los demás tratamientos, la media es muy baja pero la mediana es 0, indicando que\n")
cat("   al menos la mitad de las observaciones en esos grupos fueron cero.\n\n")

cat(" - DESVIACIÓN ESTÁNDAR (DE):\n")
cat("   Mide la dispersión o variabilidad promedio de los datos alrededor de su media.\n")
cat("   Una DE alta significa que los datos están muy dispersos; una DE baja, que están agrupados.\n")
cat("   Observamos que T1 tiene la mayor DE (1.26), lo que significa que el conteo de adultos\n")
cat("   en ese tratamiento fue el más variable o inconsistente.\n\n")

cat(" - COEFICIENTE DE VARIACIÓN (CV):\n")
cat("   El CV es una medida de dispersión RELATIVA. Se expresa como un porcentaje y se calcula\n")
cat("   como (DE / Media) * 100. Es extremadamente útil en agronomía para comparar la\n")
cat("   variabilidad de diferentes variables o tratamientos, incluso si tienen medias muy distintas.\n")
cat("   Un CV alto indica una alta variabilidad en relación con la media.\n\n")

cat("   Interpretación del CV en este caso:\n")
cat("   * CVs > 30-35% en experimentos de campo a menudo se consideran altos, indicando una\n")
cat("     variabilidad considerable que puede dificultar la detección de diferencias entre tratamientos.\n")
cat("   * Los tratamientos T2 a T6 tienen CVs extremadamente altos (¡incluso por encima del 200%!).\n")
cat("     Esto se debe a que sus medias son muy cercanas a cero. Cuando la media es muy pequeña,\n")
cat("     incluso una pequeña desviación estándar puede resultar en un CV enorme.\n")
cat("   * Este hallazgo del CV alto refuerza la idea de que los datos son heterogéneos y que\n")
cat("     la variabilidad no es la misma en todos los tratamientos, otra señal de alerta para el ANOVA.\n\n")


# d) Verificación de Valores Faltantes (NAs)
# Es crucial saber si tenemos datos faltantes antes de cualquier análisis.
cat("d) Verificación de valores faltantes (NAs) en el dataset:\n")
cat("   ¿Hay algún NA en todo el dataframe? ->", any(is.na(datos)), "\n")
cat("   Número total de NAs por columna:\n")
print(colSums(is.na(datos)))
cat("   -> Conclusión: No hay valores faltantes en este conjunto de datos, lo cual es ideal.\n\n")

# --- 2. Análisis Exploratorio Visual ---
# Antes de cualquier prueba formal, es CRUCIAL visualizar los datos.
# Esto nos da una intuición sobre su distribución y posibles diferencias.

cat("--- Paso 2: Análisis Exploratorio Visual ---\n\n")

# 2.1 Histograma General de la Variable de Respuesta
# Primero, veamos la distribución de TODOS los conteos de adultos, sin importar el tratamiento.
cat("2.1. Visualizando el Histograma General de 'ADULTO'...\n")

plot_hist_general <- ggplot(datos, aes(x = ADULTO)) +
  geom_histogram(
    binwidth = 1, # Ancho de barra de 1 unidad, ya que son conteos
    fill = "steelblue", 
    color = "black",
    alpha = 0.8
  ) +
  labs(
    title = "Histograma General del Conteo de Adultos",
    subtitle = "Distribución de todos los datos combinados",
    x = "Número de Adultos",
    y = "Frecuencia (Número de Observaciones)"
  ) +
  theme_minimal()

print(plot_hist_general)

cat("Interpretación del Histograma General:\n")
cat(" - La distribución está fuertemente sesgada a la derecha.\n")
cat(" - La gran mayoría de las observaciones son 0 o 1.\n")
cat(" - Claramente, esto no se parece en nada a una campana de Gauss (distribución normal).\n")
cat("   Esta es nuestra primera gran señal de alerta para el supuesto de normalidad.\n\n")


# 2.2 Boxplot Comparativo por Tratamiento
# Ahora, comparemos las distribuciones entre los diferentes tratamientos.
cat("2.2. Visualizando el Boxplot Comparativo por Tratamiento...\n")

plot_boxplot_comparativo <- ggplot(datos, aes(x = TRATAMIENTO, y = ADULTO, fill = TRATAMIENTO)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.1, alpha = 0.5, color = "black") + # Añade puntos para ver la distribución real
  labs(
    title = "Distribución del Conteo de Adultos por Tratamiento",
    subtitle = "Boxplot con puntos de datos individuales (jitter)",
    x = "Tratamiento",
    y = "Número de Adultos"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(plot_boxplot_comparativo)

cat("Interpretación del Boxplot Comparativo:\n")
cat(" - Se observa que la mayoría de los datos son ceros o valores muy bajos para los tratamientos T2 a T6.\n")
cat(" - El tratamiento T1 parece tener valores más altos y una dispersión (variabilidad) mucho mayor que los demás.\n")
cat(" - La diferencia en la dispersión (altura de las cajas y rango de los bigotes) sugiere que\n")
cat("   el supuesto de homogeneidad de varianzas también podría no cumplirse.\n\n")


# 2.3 Histogramas por Tratamiento (Facetado)
# Para una vista más detallada, podemos crear un histograma para cada tratamiento por separado.
cat("2.3. Visualizando Histogramas por Tratamiento (Facetado)...\n")

plot_hist_facetado <- ggplot(datos, aes(x = ADULTO)) +
  geom_histogram(
    binwidth = 1,
    fill = "skyblue",
    color = "black"
  ) +
  facet_wrap(~ TRATAMIENTO, ncol = 3) + # Crea una "parrilla" de gráficos, uno por cada tratamiento
  labs(
    title = "Histograma del Conteo de Adultos para Cada Tratamiento",
    subtitle = "Distribuciones individuales por grupo",
    x = "Número de Adultos",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(face = "bold")) # Pone en negrita los títulos T1, T2, etc.

print(plot_hist_facetado)

cat("Interpretación de los Histogramas Facetados:\n")
cat(" - Esta vista confirma nuestras sospechas del boxplot de manera aún más clara.\n")
cat(" - La distribución para T1 está más dispersa y contiene valores más altos (hasta 6).\n")
cat(" - Las distribuciones para T2 a T6 están fuertemente concentradas en 0 y 1.\n")
cat(" - Ninguna de estas distribuciones individuales se parece a una distribución normal.\n\n")

# --- 3. Análisis de Varianza (ANOVA) y Verificación de Supuestos ---

cat("--- Paso 3: Flujo de Trabajo del ANOVA y Diagnóstico del Modelo ---\n\n")

# 3.1. Ajustar el modelo ANOVA
# El modelo lineal que queremos probar es que la variable respuesta (ADULTO)
# se explica o varía en función del factor (TRATAMIENTO).
# Este es el modelo para un Diseño Completamente al Azar (DCA).
cat("3.1. Ajustando el modelo ANOVA con aov()...\n")
modelo_aov <- aov(ADULTO ~ TRATAMIENTO, data = datos)
cat("¡Modelo ajustado!\n\n")


# 3.2. Diagnóstico Gráfico Integral de los Supuestos
# ¡Este es el paso más importante! Si los supuestos no se cumplen, las conclusiones del ANOVA no son válidas.
# La función `plot()` aplicada a un objeto `aov` genera 4 gráficos de diagnóstico clave.
# Los analizaremos en una parrilla de 2x2.

cat("3.2. Generando los 4 gráficos de diagnóstico del modelo...\n")

# Configuramos R para mostrar los 4 gráficos en una sola ventana (parrilla 2x2)
par(mfrow = c(2, 2)) 
plot(modelo_aov)
par(mfrow = c(1, 1)) # Restauramos la configuración a un solo gráfico por ventana

cat("¡Gráficos generados! Ahora, interpretemos cada uno de ellos:\n\n")

# Interpretación detallada de cada gráfico
cat("--- Interpretación de los Gráficos de Diagnóstico ---\n\n")

cat("a) Gráfico 1: 'Residuals vs Fitted' (Arriba a la Izquierda)\n")
cat("   - ¿QUÉ MUESTRA?: Este gráfico representa los residuos (errores) del modelo en el eje Y\n")
cat("     frente a los valores ajustados (predichos) por el modelo en el eje X.\n")
cat("   - ¿QUÉ BUSCAMOS?: Una nube de puntos dispersa aleatoriamente alrededor de la línea horizontal\n")
cat("     en cero, sin ningún patrón discernible. La línea roja (una línea suavizada de los residuos)\n")
cat("     debería ser aproximadamente horizontal y plana.\n")
cat("   - ¿QUÉ VEMOS AQUÍ?: Observamos una forma de 'embudo' o 'megáfono', donde la dispersión\n")
cat("     de los residuos aumenta a medida que los valores ajustados crecen. La línea roja no es\n")
cat("     plana. Esto es una señal CLARA de HETEROCEDASTICIDAD (varianzas no homogéneas).\n")
cat("   - CONCLUSIÓN: El supuesto de homogeneidad de varianzas está VIOLADO.\n\n")

cat("b) Gráfico 2: 'Normal Q-Q' (Arriba a la Derecha)\n")
cat("   - ¿QUÉ MUESTRA?: Compara los cuantiles de los residuos estandarizados (eje Y) con los\n")
cat("     cuantiles teóricos de una distribución normal estándar (eje X).\n")
cat("   - ¿QUÉ BUSCAMOS?: Que los puntos caigan muy cerca de la línea diagonal de referencia (la línea de 45°).\n")
cat("   - ¿QUÉ VEMOS AQUÍ?: Los puntos se desvían sistemáticamente de la línea, especialmente en las colas\n")
cat("     (los extremos). Tienen una forma de 'S' alargada, lo que indica que la distribución de\n")
cat("     los residuos es más 'pesada' en las colas y más 'puntiaguda' en el centro que una normal.\n")
cat("   - CONCLUSIÓN: El supuesto de normalidad de los residuos está VIOLADO.\n\n")

cat("c) Gráfico 3: 'Scale-Location' (Abajo a la Izquierda)\n")
cat("   - ¿QUÉ MUESTRA?: La raíz cuadrada de los residuos estandarizados en el eje Y frente a los\n")
cat("     valores ajustados en el eje X. Es similar al primer gráfico, pero ayuda a visualizar\n")
cat("     mejor la homocedasticidad.\n")
cat("   - ¿QUÉ BUSCAMOS?: Una línea roja suavizada que sea aproximadamente horizontal y plana. Esto\n")
cat("     indicaría que la varianza de los residuos es constante a lo largo de los valores ajustados.\n")
cat("   - ¿QUÉ VEMOS AQUÍ?: La línea roja tiene una pendiente positiva clara, lo que confirma que\n")
cat("     la varianza de los residuos aumenta con el valor ajustado.\n")
cat("   - CONCLUSIÓN: Confirma la violación del supuesto de homogeneidad de varianzas.\n\n")

cat("d) Gráfico 4: 'Residuals vs Leverage' (Abajo a la Derecha)\n")
cat("   - ¿QUÉ MUESTRA?: Los residuos estandarizados (eje Y) frente al 'apalancamiento' (leverage) de\n")
cat("     cada observación (eje X). El apalancamiento mide cuán influyente es una observación en la\n")
cat("     predicción del modelo. Este gráfico ayuda a identificar outliers influyentes.\n")
cat("   - ¿QUÉ BUSCAMOS?: Observaciones que estén fuera de las líneas punteadas rojas (la 'distancia de Cook').\n")
cat("     Un punto con alto apalancamiento Y alto residuo es particularmente problemático.\n")
cat("   - ¿QUÉ VEMOS AQUÍ?: Vemos varias observaciones (identificadas por su número de fila, ej., 29, 33)\n")
cat("     que tienen residuos estandarizados grandes. La observación 29, en particular, tiene un\n")
cat("     residuo alto y podría ser un outlier influyente.\n")
cat("   - CONCLUSIÓN: Hay presencia de outliers que pueden estar afectando desproporcionadamente al modelo.\n\n")

# 3.3. Pruebas Estadísticas Formales para los Supuestos
cat("3.3. Pruebas Estadísticas Formales para confirmar el diagnóstico gráfico...\n")

# Prueba formal de Shapiro-Wilk para Normalidad
# H0: Los residuos provienen de una distribución normal.
cat("    Prueba de Shapiro-Wilk para Normalidad:\n")
prueba_shapiro <- shapiro.test(residuals(modelo_aov))
print(prueba_shapiro)
cat("    -> Conclusión Shapiro-Wilk: El p-valor es muy pequeño (< 2.2e-16), por lo que\n")
cat("       rechazamos H0 con alta confianza. Los residuos NO son normales.\n\n")

# Prueba formal de Levene para Homocedasticidad
# H0: Las varianzas de los grupos son iguales.
cat("    Prueba de Levene para Homogeneidad de Varianzas:\n")
prueba_levene <- leveneTest(ADULTO ~ TRATAMIENTO, data = datos)
print(prueba_levene)
cat("    -> Conclusión Levene: El p-valor es muy pequeño (< 2.2e-16), por lo que\n")
cat("       rechazamos H0 con alta confianza. Las varianzas NO son homogéneas.\n\n")

# 3.4. Interpretación de la Tabla ANOVA
cat("3.4. Tabla ANOVA (¡Recordatorio: los resultados no son fiables!):\n")
cat("A PESAR de la clara violación de los supuestos, mostraremos la tabla ANOVA para fines didácticos.\n")
print(summary(modelo_aov))
cat("\n   -> Interpretación Condicional: Si los supuestos se hubieran cumplido, el Pr(>F) de 1.8e-09\n")
cat("      indicaría una diferencia altamente significativa entre los tratamientos. Sin embargo,\n")
cat("      dado que los supuestos no se cumplen, no podemos confiar en la validez de este p-valor.\n\n")

# --- 4. Decisión y Análisis Alternativo (No Paramétrico) ---

cat("--- Paso 4: Decisión y Análisis Alternativo ---\n\n")
cat("Conclusión Intermedia: Dado que los supuestos de Normalidad y Homocedasticidad\n")
cat("fueron violados de manera clara, el ANOVA paramétrico no es la prueba adecuada\n")
cat("para este conjunto de datos. La prueba correcta a utilizar es su alternativa\n")
cat("no paramétrica: la Prueba de Kruskal-Wallis.\n\n")

# 4.1. Prueba de Kruskal-Wallis
# H0: Las medianas de todos los tratamientos son iguales.
# H1: Al menos una mediana es diferente.
cat("4.1. Realizando la Prueba de Kruskal-Wallis...\n")
prueba_kruskal <- kruskal.test(ADULTO ~ TRATAMIENTO, data = datos)
print(prueba_kruskal)
cat("\nInterpretación de Kruskal-Wallis:\n")
cat(" - El p-valor es", round(prueba_kruskal$p.value, 5), ".\n")
if(prueba_kruskal$p.value < 0.05) {
  cat(" - Como el p-valor es < 0.05, rechazamos la hipótesis nula.\n")
  cat(" - Concluimos que hay una diferencia estadísticamente significativa en el número\n")
  cat("   mediano de adultos entre los tratamientos.\n\n")
} else {
  cat(" - Como el p-valor es > 0.05, no podemos rechazar la hipótesis nula.\n")
  cat(" - No hay evidencia suficiente para afirmar que hay diferencias entre las medianas.\n\n")
}

# 4.2. Pruebas Post-Hoc No Paramétricas (Prueba de Dunn)
# Como Kruskal-Wallis fue significativo, necesitamos saber QUÉ grupos son diferentes.
# Usamos la prueba de Dunn.
if(prueba_kruskal$p.value < 0.05) {
  cat("4.2. Realizando la Prueba Post-Hoc de Dunn para identificar diferencias...\n")
  # Usamos rstatix para una salida clara y ordenada
  prueba_dunn <- dunn_test(ADULTO ~ TRATAMIENTO, data = datos, p.adjust.method = "bonferroni")
  
  print(prueba_dunn)
  
  cat("\nInterpretación de la Prueba de Dunn:\n")
  cat(" - Buscamos en la columna 'p.adj' (p-valor ajustado).\n")
  cat(" - Cualquier comparación con 'p.adj' < 0.05 indica una diferencia significativa.\n")
  cat(" - En este caso, podemos ver que el Tratamiento T1 es significativamente\n")
  cat("   diferente de T2, T3, T4, T5 y T6.\n")
  cat(" - No hay diferencias significativas entre los tratamientos T2, T3, T4, T5 y T6.\n")
  
  # Generar una tabla de medianas para ayudar a la interpretación
  medianas_por_trat <- datos %>%
    group_by(TRATAMIENTO) %>%
    summarise(Mediana_Adultos = median(ADULTO), .groups = 'drop')
  
  cat("\nMedianas por tratamiento para contextualizar los resultados:\n")
  print(medianas_por_trat)
  
} else {
  cat("4.2. La prueba de Kruskal-Wallis no fue significativa, por lo tanto, no se realizan\n")
  cat("    pruebas post-hoc.\n")
}

# --- 5. Conclusión Final ---

cat("\n--- Paso 5: Conclusión Final del Análisis ---\n\n")
cat("El análisis exploratorio inicial sugirió que los datos de conteo de adultos no seguían\n")
cat("una distribución normal y tenían varianzas desiguales. Las pruebas formales de Shapiro-Wilk\n")
cat("y Levene confirmaron la violación de los supuestos del ANOVA.\n\n")
cat("Por lo tanto, se optó por la prueba no paramétrica de Kruskal-Wallis, la cual resultó\n")
cat("estadísticamente significativa (p < 0.05), indicando que el tipo de tratamiento\n")
cat("tiene un efecto sobre el número mediano de adultos observados.\n\n")
cat("La prueba post-hoc de Dunn reveló que el Tratamiento T1 produce un número mediano de\n")
cat("adultos significativamente mayor que todos los demás tratamientos (T2, T3, T4, T5 y T6),\n")
cat("mientras que no se encontraron diferencias significativas entre estos últimos cinco.\n\n")
cat("Fin del script de práctica.\n")

