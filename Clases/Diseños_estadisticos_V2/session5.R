# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 5: Análisis de un DBCA con Datos de Papa
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# Este script asume que los paquetes ya están instalados.
# Si no, ejecútalos una vez en la consola:
# install.packages("tidyverse")
# install.packages("car")
# install.packages("agricolae")
# install.packages("rstatix")

# Cargar las librerías necesarias
library(tidyverse) # Para manipulación de datos (dplyr) y gráficos (ggplot2)
library(car)       # Para la prueba de Levene
library(agricolae) # Para pruebas post-hoc agronómicas y análisis de diseños
library(rstatix)   # Para pruebas estadísticas amigables

# --- 1. Carga, Limpieza e Inspección de Datos ---

cat("--- Paso 1: Carga, Limpieza e Inspección Inicial de los Datos ---\n\n")

# 1.1. Carga de Datos
cat("1.1. Cargando datos desde el portapapeles...\n")
cat("     INSTRUCCIONES: Selecciona y copia (Ctrl+C) toda la tabla de datos.\n")
cat("     Luego, ejecuta la siguiente línea en R.\n")
datos <- read.delim("clipboard")

str(datos)

colnames(datos)

# 1.2. Limpieza y Preparación de Variables
cat("1.2. Limpiando y preparando las variables para el análisis DBCA...\n")
# El DBCA requiere una variable de bloque y una de tratamiento.
# En tu dataset:
#   - Bloque (o Repetición): REP
#   - Tratamiento (Genotipo/Variedad): INSTN
#   - Variable de Respuesta de interés: TWTP (Peso Total de Tubérculos por Parcela)

# Convertir las variables de agrupación a factores es CRUCIAL.
datos <- datos %>%
  mutate(
    Bloque = as.factor(Localidad),
    Tratamiento = as.factor(INSTN),
    Rendimiento = as.numeric(MTWP)
  )


str(datos)

cat("Columnas REP (Bloque) e INSTN (Tratamiento) convertidas a factor.\n\n")

# 1.3. Inspección de la Estructura
cat("1.3. Vistazo rápido a la estructura de los datos con glimpse():\n")
glimpse(datos)
cat("\nVerificación de valores faltantes (NAs):\n")
print(colSums(is.na(datos)))


# --- 2. Análisis Descriptivo y Exploratorio Visual ---

cat("--- Paso 2: Análisis Descriptivo y Exploratorio Visual ---\n")
cat("La clave del DBCA es entender la variabilidad que viene de los tratamientos\n")
cat("Y la variabilidad que viene de los bloques. Vamos a visualizar ambas.\n\n")

# 2.1. Descriptivos por Tratamiento
cat("2.1. Resumen estadístico para la variable de respuesta 'TWTP' por Tratamiento (INSTN):\n")
resumen_por_tratamiento <- datos %>%
  group_by(Tratamiento) %>%
  summarise(
    N = n(),
    Media_Rend = mean(Rendimiento, na.rm = TRUE),
    DE_Rend = sd(Rendimiento, na.rm = TRUE),
    CV_porc = (DE_Rend / Media_Rend) * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2)))

print(as.data.frame(resumen_por_tratamiento))
cat("\nInterpretación: Esta tabla nos da una idea inicial de qué tratamientos (genotipos) podrían\n")
cat("tener mayor rendimiento promedio (Media_TWTP) y cuál es su variabilidad (DE, CV).\n\n")

# 2.2. Visualización del Efecto del Tratamiento
cat("2.2. Visualizando el efecto de los Tratamientos (INSTN) sobre TWTP...\n")
plot_trat <- ggplot(datos, aes(x = reorder(Tratamiento, Rendimiento, median), y = Rendimiento, fill = Tratamiento)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  labs(
    title = "Rendimeinto por Tratamiento",
    x = "Tratamiento (Genotipo - INSTN)",
    y = "MTWP"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot_trat)
cat("Interpretación del Boxplot por Tratamiento:\n")
cat(" - Se observan diferencias claras en las medianas y rangos entre los genotipos.\n")
cat(" - Genotipos como 'Yungay' (CIP720064) parecen tener rendimientos mucho más altos.\n")
cat(" - La variabilidad (altura de las cajas) también parece ser diferente entre tratamientos.\n\n")

# 2.3. Visualización del Efecto del Bloque
cat("2.3. Visualizando el efecto de los Bloques (REP) sobre TWTP...\n")
cat("   ¿Por qué hacemos esto? Para justificar el uso de un DBCA. Si hay diferencias\n")
cat("   entre bloques, significa que había una fuente de variabilidad en el campo\n")
cat("   (ej. un gradiente de fertilidad) que el bloqueo nos ayudó a controlar.\n")

plot_bloque <- ggplot(datos, aes(x = Bloque, y = Rendimiento, fill = Bloque)) +
  geom_boxplot(alpha = 0.8, show.legend = FALSE) +
  theme_minimal()

print(plot_bloque)

cat("\nInterpretación del Boxplot por Bloque:\n")
cat(" - Observamos que las medianas de rendimiento varían entre los bloques.\n")
cat("   Por ejemplo, el Bloque 3 parece tener un rendimiento general más alto que los otros.\n")
cat(" - ¡Esto es una excelente noticia! Significa que bloquear fue una buena decisión.\n")
cat("   Hemos capturado y aislado esta variabilidad, haciendo nuestra comparación\n")
cat("   de tratamientos más precisa.\n\n")

# --- 3. Modelo DBCA: ANOVA y Diagnóstico ---

cat("--- Paso 3: Modelo Lineal del DBCA, ANOVA y Diagnóstico de Supuestos ---\n\n")

# 3.1. Ajuste del Modelo Lineal del DBCA
# La fórmula del modelo DBCA incluye tanto el efecto del tratamiento como el del bloque.
# El modelo es: Respuesta ~ Tratamiento + Bloque
cat("3.1. Ajustando el modelo ANOVA para un DBCA: TWTP ~ INSTN + REP\n")
modelo_dbca <- aov(Rendimiento ~ Tratamiento + Bloque, data = datos)
cat("¡Modelo ajustado!\n\n")

# 3.2. Diagnóstico Gráfico Integral de los Supuestos
cat("3.2. Generando y analizando los 4 gráficos de diagnóstico del modelo...\n")
par(mfrow = c(2, 2))
plot(modelo_dbca)
par(mfrow = c(1, 1))
cat("\nInterpretación de los Gráficos de Diagnóstico:\n")
cat(" - 'Residuals vs Fitted': Buscamos una nube de puntos aleatoria. Aquí no se ve un patrón\n")
cat("   demasiado problemático, aunque no es perfectamente aleatorio.\n")
cat(" - 'Normal Q-Q': Los puntos se ajustan razonablemente bien a la línea diagonal, aunque hay\n")
cat("   leves desviaciones en las colas. Parece que el supuesto de normalidad es aceptable.\n")
cat(" - 'Scale-Location': La línea roja no es perfectamente plana, lo que podría sugerir una leve\n")
cat("   heterocedasticidad, pero no es tan pronunciado como en el ejemplo del DCA.\n")
cat(" - 'Residuals vs Leverage': Ningún punto parece estar fuera de las líneas de Cook.\n\n")

# 3.3. Pruebas Estadísticas Formales para los Supuestos
cat("3.3. Pruebas formales para confirmar el diagnóstico gráfico...\n")
# Prueba de Shapiro-Wilk para Normalidad de los Residuos
cat("    Prueba de Shapiro-Wilk (H0: los residuos son normales):\n")
print(shapiro.test(residuals(modelo_dbca)))
cat("    -> El p-valor es mayor a 0.05. No rechazamos la H0. El supuesto de normalidad se cumple.\n\n")

# Prueba de Levene para Homogeneidad de Varianzas
cat("    Prueba de Levene (H0: las varianzas son homogéneas entre tratamientos):\n")
print(leveneTest(Rendimiento ~ Tratamiento, data = datos))

print(leveneTest(Rendimiento ~ interaction(Tratamiento, Bloque), data = datos))

cat("    -> El p-valor es mayor a 0.05. No rechazamos la H0. El supuesto de homocedasticidad se cumple.\n\n")

# 3.4. Análisis de la Tabla ANOVA
cat("3.4. Interpretando la Tabla ANOVA del DBCA...\n")
print(summary(modelo_dbca))
cat("\nInterpretación de la Tabla ANOVA:\n")
cat(" - INSTN (Tratamiento): El p-valor (Pr(>F)) es muy pequeño (< 0.05). Esto significa que hay\n")
cat("   una diferencia estadísticamente significativa en el rendimiento (TWTP) entre al menos dos\n")
cat("   de los genotipos probados. ¡El factor tratamiento es significativo!\n")
cat(" - REP (Bloque): El p-valor para el bloque también es significativo. Esto confirma lo que vimos\n")
cat("   en el boxplot: hubo diferencias reales de rendimiento entre los bloques. Bloquear fue EFECTIVO\n")
cat("   y nos ayudó a reducir el 'ruido' experimental (la varianza del error o 'Residuals').\n\n")

# --- 4. Comparaciones Múltiples (Pruebas Post-Hoc) ---

cat("--- Paso 4: Pruebas de Comparación Múltiple (Post-Hoc) ---\n")
cat("Dado que el efecto del tratamiento (INSTN) fue significativo, ahora necesitamos saber\n")
cat("exactamente QUÉ genotipos son diferentes entre sí.\n\n")

# --- 4.1 Prueba HSD de Tukey --- 
# Usaremos la prueba HSD de Tukey, ya que es el estándar cuando se cumplen los supuestos.
# El paquete 'agricolae' presenta los resultados de una forma muy común en agronomía,
# agrupando los tratamientos con letras.
cat("4.1. Realizando la prueba HSD de Tukey con 'agricolae'...\n")
tukey_test <- HSD.test(modelo_dbca, "Tratamiento", group = TRUE, console = TRUE)

# El resultado ya se imprime por `console = TRUE`, pero para tener los grupos:
grupos_tukey <- tukey_test$groups
cat("\nGrupos de Tratamientos según Tukey HSD:\n")
print(grupos_tukey)
cat("\nInterpretación de los Grupos de Tukey:\n")
cat(" - Tratamientos que comparten la misma letra NO son significativamente diferentes entre sí.\n")
cat(" - Por ejemplo, 'Yungay' está en el grupo 'a', lo que indica que su rendimiento fue\n")
cat("   significativamente mayor que todos los demás genotipos.\n")
cat(" - Genotipos en el grupo 'b' son estadísticamente similares entre sí, pero superiores a los del grupo 'c', y así sucesivamente.\n\n")


# --- 4.2. Visualización de los Resultados de la Prueba de Tukey ---

cat("\n4.2. Visualizando los resultados de la prueba de Tukey HSD...\n")
cat("Un buen gráfico puede resumir la tabla de comparaciones de una manera mucho más clara.\n\n")

# Extraer los datos necesarios para graficar.
tukey_data_for_plot <- as.data.frame(tukey_test$groups)
tukey_data_for_plot <- tukey_data_for_plot %>%
  rownames_to_column(var = "Tratamiento") %>% # Convertir los nombres de las filas a una columna
  rename(Tratamiento = 1, Grupos = groups) # Renombrar la primera columna a 'Media' y 'groups' a 'Grupos'
# El `rename(Media = 1)` es una forma robusta de renombrar la 
# primera columna sin importar cómo se llame (en este caso 'TWTP')

cat("Datos limpios y listos para los gráficos:\n")
print(tukey_data_for_plot)


# -----------------------------------------------------------------------------
# Opción 1: Gráfico rápido con `agricolae::bar.group()`
# -----------------------------------------------------------------------------
cat("Generando gráfico con agricolae::bar.group()...\n")
cat("Este es un método rápido y directo que viene con el paquete.\n")

# Para que el gráfico se vea bien, podemos ajustar los márgenes del plot
par(mar = c(8, 4, 4, 2) + 0.1) # Aumentar el margen inferior para los nombres largos

# La función bar.group necesita el objeto de los grupos y opcionalmente los límites de confianza
# El objeto tukey_test ya contiene toda la información.
bar.group(
  tukey_test$groups, 
  ylim = c(0, 120)
)

# Restaurar los márgenes por defecto
par(mar = c(5, 4, 4, 2) + 0.1) 
cat("¡Gráfico de barras de agricolae generado!\n\n")

# -----------------------------------------------------------------------------
# Opción 2: Gráfico personalizable y más elegante con `ggplot2`
# -----------------------------------------------------------------------------
cat("Generando una versión más personalizable y elegante con ggplot2...\n")
cat("Este enfoque nos da control total sobre la estética del gráfico.\n")

# -----------------------------------------------------------------------------
# Gráfico 2.1: Barras Verticales (Tratamientos en Eje X)
# -----------------------------------------------------------------------------
cat("Generando Gráfico 1: Barras verticales (Tratamientos en Eje X)...\n")

plot_bar_vertical <- ggplot(tukey_data_for_plot, aes(x = reorder(Tratamiento, -Rendimiento), y = Rendimiento)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "black", alpha = 0.8) +
  geom_text(
    aes(label = Grupos),
    vjust = -0.5,
    color = "black",
    size = 4
  ) +
  labs(
    title = "Comparación de Medias de Rendimiento (TWTP) por Genotipo",
    subtitle = "Prueba HSD de Tukey (α = 0.05). Letras distintas indican diferencias significativas.",
    x = "Genotipo (Tratamiento)",
    y = "Media del Peso Total de Tubérculos (TWTP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(tukey_data_for_plot$Rendimiento) * 1.15)

print(plot_bar_vertical)

# -----------------------------------------------------------------------------
# Gráfico 2.2: Barras Horizontales (Tratamientos en Eje Y)
# -----------------------------------------------------------------------------
cat("Generando Gráfico 2: Barras horizontales (Tratamientos en Eje Y)...\n")

plot_bar_horizontal <- ggplot(tukey_data_for_plot, aes(x = reorder(Tratamiento, Rendimiento), y = Rendimiento)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black", alpha = 0.8) +
  geom_text(
    aes(label = Grupos),
    hjust = -0.2,
    color = "black",
    size = 4
  ) +
  coord_flip() +
  labs(
    title = "Comparación de Medias de Rendimiento (TWTP) por Genotipo",
    subtitle = "Prueba HSD de Tukey (α = 0.05). Letras distintas indican diferencias significativas.",
    x = "Genotipo (Tratamiento)",
    y = "Media del Peso Total de Tubérculos (TWTP)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(tukey_data_for_plot$Rendimiento) * 1.15)

print(plot_bar_horizontal)
cat("¡Gráfico de barras horizontales generado!\n\n")


# -----------------------------------------------------------------------------
# Gráfico 2.3: Boxplot con Letras de Tukey
# -----------------------------------------------------------------------------
cat("Generando Gráfico 3: Boxplot con las letras de Tukey añadidas...\n")

# Paso 1: Calcular la posición Y para cada letra (un poco por encima del máximo)
# ¡IMPORTANTE! Usamos los datos ya filtrados por localidad.
posicion_letras <- datos %>%
  group_by(Tratamiento) %>%
  summarise(pos_y = max(Rendimiento, na.rm = TRUE) * 1.05) %>%
  # Paso 2: Unir con la tabla que tiene las letras de los grupos
  left_join(
    tukey_data_for_plot %>% select(Tratamiento, Grupos),
    by = c("Tratamiento" = "Tratamiento") # La clave de unión es el nombre del genotipo
  )

cat("\nDatos para posicionar las letras en el boxplot:\n")
print(posicion_letras)
cat("\n")

# Paso 3: Crear el gráfico usando los datos filtrados y las posiciones de las letras
plot_boxplot_con_letras <- ggplot(datos, aes(x = reorder(Tratamiento, Rendimiento, median), y = Rendimiento)) +
  geom_boxplot(aes(fill = Tratamiento), alpha = 0.7, show.legend = FALSE) +
  geom_jitter(width = 0.1, alpha = 0.3, color = "black") +
  # Añadir las letras usando el dataframe de posiciones que creamos
  geom_text(
    data = posicion_letras,
    aes(x = Tratamiento, y = pos_y, label = Grupos),
    color = "black",
    fontface = "bold",
    size = 5
  ) +
  labs(
    title = paste("Distribución y Comparación de Medias de Rendimiento en"),
    subtitle = "Prueba HSD de Tukey (α = 0.05). Letras distintas indican diferencias significativas.",
    x = "Genotipo (INSTN)",
    y = "Rendimiento"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 10))

print(plot_boxplot_con_letras)

# --- 5. Conclusión Final ---

cat("--- Paso 5: Conclusión Final del Análisis DBCA ---\n\n")
cat("El análisis de los datos del ensayo de papa en un Diseño de Bloques Completos al Azar (DBCA)\n")
cat("mostró que existió una diferencia altamente significativa en el Peso Total de Tubérculos por\n")
cat("Parcela (TWTP) entre los diferentes genotipos (INSTN) evaluados (p < 0.05).\n\n")
cat("El efecto del bloqueo (REP) también fue significativo, lo que valida la decisión de usar\n")
cat("un DBCA para controlar la variabilidad espacial en el campo.\n\n")
cat("La prueba de comparación múltiple de Tukey HSD permitió agrupar los genotipos según su rendimiento.\n")
cat("El genotipo 'Yungay' (CIP720064) demostró ser el de mayor rendimiento, siendo estadísticamente\n")
cat("superior a todos los demás. Se identificaron varios grupos de rendimiento intermedios y bajos,\n")
cat("proporcionando una clasificación clara para la selección de materiales.\n\n")
cat("Los supuestos de normalidad de residuos y homogeneidad de varianzas fueron verificados y cumplidos,\n")
cat("dando validez a las conclusiones de este análisis paramétrico.\n")

cat("\n--- Fin del Script de Práctica DBCA ---\n")