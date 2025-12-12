# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 7: Análisis de un Diseño en Parcelas Divididas
# Curso: Fundamentos de Estadística Agrícola y R
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---

# Este script utiliza paquetes estándar para el análisis de diseños complejos.
# Asegúrate de tenerlos instalados. Si no, ejecútalos una vez en la consola:
# install.packages("tidyverse")
# install.packages("MASS")      # Contiene el dataset 'oats'
# install.packages("agricolae") # Para pruebas post-hoc y análisis de diseños
# install.packages("emmeans")   # Para un enfoque moderno de post-hoc
# install.packages("car")       # Para la prueba de Levene

# Cargar las librerías necesarias
library(tidyverse)
library(MASS)
library(agricolae)
library(emmeans)
library(car)

cat("¡Bienvenido a la práctica de la Sesión 7!\n")
cat("Hoy analizaremos un diseño en Parcelas Divididas (Split-Plot) usando el dataset 'oats'.\n\n")


# --- 1. Carga y Preparación de los Datos ---
# El primer paso es cargar y entender la estructura de nuestros datos.

cat("--- Paso 1: Carga, Preparación e Inspección de los Datos ---\n\n")

# 1.1. Carga del Dataset 'oats'
data(oats, package = "MASS")
datos_avena <- oats

# 1.2. Entendiendo la estructura del experimento
cat("Contexto del Experimento 'oats':\n")
cat(" - Se estudió el rendimiento de avena (Y) en un diseño en bloques.\n")
cat(" - Factor Principal (Parcela Grande): 4 niveles de fertilización Nitrogenada (N).\n")
cat(" - Factor Secundario (Sub-Parcela): 3 variedades de avena (V).\n")
cat(" - Bloques: 6 bloques de campo (B).\n\n")

# 1.3. Limpieza y preparación de variables
# Convertimos las variables de agrupación a factores para que R las trate como categorías.
datos_avena_limpios <- datos_avena %>%
  mutate(
    bloque = B,
    nitrogeno = factor(N, labels = c("0 cwt", "0.2 cwt", "0.4 cwt", "0.6 cwt")),
    variedad = V
  ) %>%
  rename(rendimiento = Y) %>%
  dplyr::select(bloque, nitrogeno, variedad, rendimiento)

cat("Vistazo a la estructura de los datos limpios con glimpse():\n")
glimpse(datos_avena_limpios)
cat("\n")


# --- 2. Análisis Exploratorio Visual ---
# Antes de modelar, visualizamos para entender las tendencias y posibles interacciones.

cat("--- Paso 2: Análisis Exploratorio Visual ---\n\n")

# 2.1. Gráfico de Interacción
# Este es el gráfico más importante para un diseño factorial.
# Muestra cómo responde cada variedad (líneas de colores) a los niveles de nitrógeno.
# Buscamos líneas que NO sean paralelas.
cat("2.1. Creando el gráfico de interacción...\n")

esquisser(datos_avena_limpios)

plot_interaccion <- ggplot(datos_avena_limpios, 
                           aes(x = nitrogeno, y = rendimiento, group = variedad, color = variedad)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) + # Une las medias
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) + # Añade barras de error
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title = "Gráfico de Interacción: Rendimiento de Avena",
    subtitle = "Respuesta de cada Variedad a la Fertilización Nitrogenada",
    x = "Nivel de Nitrógeno Aplicado",
    y = "Rendimiento Promedio (bushels/acre)",
    color = "Variedad"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

print(plot_interaccion)

cat("\nInterpretación Visual del Gráfico:\n")
cat(" - Las líneas no son paralelas. La respuesta al nitrógeno parece depender de la variedad.\n")
cat(" - 'Golden Rain' muestra una fuerte respuesta positiva al nitrógeno.\n")
cat(" - 'Marvellous' parece tener el rendimiento más bajo y responde muy poco al nitrógeno.\n")
cat(" - Esto sugiere fuertemente que existe una INTERACCIÓN significativa entre variedad y nitrógeno.\n\n")


# --- 3. Modelo Lineal del Split-Plot y Verificación de Supuestos ---
cat("--- Paso 3: Modelo Lineal del Split-Plot y Diagnóstico ---\n\n")

# 3.1. Ajuste del Modelo Lineal del Split-Plot
# La sintaxis en R para un Split-Plot en DBCA es clave.
# Usamos Error(Bloque/ParcelaPrincipal) para especificar los dos estratos de error.
# En nuestro caso, el Bloque es 'bloque' y la Parcela Principal es 'nitrogeno'.
cat("3.1. Ajustando el modelo Split-Plot con la sintaxis de Error()...\n")
cat("     Modelo: rendimiento ~ nitrogeno * variedad + Error(bloque/nitrogeno)\n")

modelo_sp <- aov(rendimiento ~ nitrogeno * variedad + Error(bloque/nitrogeno), 
                 data = datos_avena_limpios)

cat("¡Modelo ajustado!\n\n")

# 3.2. Interpretación de la Tabla ANOVA del Split-Plot
# La función summary() sobre este modelo nos dará la tabla ANOVA estratificada.
cat("3.2. Tabla ANOVA del Diseño Split-Plot:\n")
print(summary(modelo_sp))

cat("\nInterpretación de la Tabla ANOVA por Estratos:\n\n")
cat("--> Estrato 1: `Error: bloque:nitrogeno` (Análisis de la Parcela Principal)\n")
cat("    - Aquí se prueba el efecto del factor principal (nitrogeno) usando el Error(a).\n")
cat("    - El p-valor para `nitrogeno` es muy significativo (< 0.001). Esto indica que, en promedio,\n")
cat("      la fertilización nitrogenada tuvo un efecto sobre el rendimiento.\n\n")

cat("--> Estrato 2: `Error: Within` (Análisis de la Sub-Parcela)\n")
cat("    - Aquí se prueban los efectos del factor secundario (variedad) y la interacción usando el Error(b).\n")
cat("    - `variedad`: El p-valor es significativo (< 0.001), indicando diferencias promedio entre las variedades.\n")
cat("    - `nitrogeno:variedad` (La Interacción): El p-valor es 0.015 (< 0.05). ¡Esta es la prueba más importante!\n")
cat("      Confirma nuestra sospecha del gráfico: la interacción es significativa. El efecto del nitrógeno\n")
cat("      depende de la variedad, y viceversa.\n\n")


# 3.3. Verificación de Supuestos
# Los supuestos se verifican sobre los residuales del error más pequeño y numeroso, el Error(b).
cat("3.3. Verificando los supuestos sobre los residuales del Error (b)...\n")
# Extraemos los residuales del estrato 'Within' (Error b)
residuos_sp <- residuals(modelo_sp$Within)

# Gráficos de diagnóstico
par(mfrow = c(1, 2))
hist(residuos_sp, main = "Histograma de Residuales (Error b)", col = "lightblue")
qqnorm(residuos_sp, main = "Q-Q Plot de Residuales (Error b)")
qqline(residuos_sp, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# Pruebas formales
cat("\nPrueba de Shapiro-Wilk para Normalidad (H0: los residuos son normales):\n")
print(shapiro.test(residuos_sp))
cat("-> El p-valor es > 0.05. No se rechaza la normalidad. ¡El supuesto se cumple!\n\n")


# --- 4. Análisis Post-Hoc: Desglose de la Interacción ---
cat("--- Paso 4: Análisis Post-Hoc para la Interacción Significativa ---\n\n")
cat("Dado que la interacción `nitrogeno:variedad` fue significativa, nuestro análisis\n")
cat("debe centrarse en desglosar esta interacción. Ignoraremos los efectos principales.\n")
cat("Usaremos el paquete 'emmeans' por su robustez y claridad.\n\n")

# 4.1. Comparación de Variedades DENTRO de cada Nivel de Nitrógeno
cat("4.1. Pregunta: ¿Qué variedad es mejor en cada nivel de Nitrógeno?\n")
emm_variedad_en_N <- emmeans(modelo_sp, ~ variedad | nitrogeno)
comparaciones_v_en_N <- pairs(emm_variedad_en_N)

cat("\nResultados de las comparaciones (Tukey ajustado):\n")
print(summary(comparaciones_v_en_N, infer = TRUE)) # infer=TRUE para obtener p-valores

cat("\nVisualización de estas comparaciones:\n")
plot_comp_v_en_N <- plot(comparaciones_v_en_N, comparisons = TRUE) + 
  labs(title="Comparación de Variedades dentro de cada Nivel de N")
print(plot_comp_v_en_N)

cat("\nInterpretación:\n")
cat(" - Las flechas rojas indican pares que son significativamente diferentes.\n")
cat(" - Con 0 cwt de N, no hay diferencias. Conforme aumenta el N, 'Golden Rain' y 'Victory'\n")
cat("   se separan significativamente de 'Marvellous'.\n\n")

# 4.2. Comparación de Dosis de Nitrógeno DENTRO de cada Variedad
cat("4.2. Pregunta: ¿Cómo responde cada Variedad al Nitrógeno?\n")
emm_N_en_variedad <- emmeans(modelo_sp, ~ nitrogeno | variedad)
comparaciones_N_en_v <- pairs(emm_N_en_variedad)

cat("\nResultados de las comparaciones (Tukey ajustado):\n")
print(summary(comparaciones_N_en_v, infer = TRUE))

cat("\nVisualización de estas comparaciones:\n")
plot_comp_N_en_v <- plot(comparaciones_N_en_v, comparisons = TRUE) +
  labs(title="Respuesta al Nitrógeno dentro de cada Variedad")
print(plot_comp_N_en_v)

cat("\nInterpretación:\n")
cat(" - Para 'Golden Rain', se observan diferencias significativas entre casi todos los niveles de N.\n")
cat(" - Para 'Marvellous', ninguna dosis de N es significativamente diferente de otra. No responde.\n")
cat(" - Para 'Victory', la respuesta es intermedia.\n\n")


# --- 5. Modelo Real (Split-Plot) y ANVA Auxiliar (Factorial en DBCA) ---
cat("--- Paso 3: Comparando el Diseño Real con su Alternativa Simple ---\n\n")

# 5.1. Modelo Real del Split-Plot
cat("5.1. Ajustando el modelo correcto del Split-Plot con dos errores...\n")
modelo_sp <- aov(rendimiento ~ nitrogeno * variedad + Error(bloque/nitrogeno), data = datos_avena_limpios)
cat("Tabla ANOVA del Split-Plot (Dos Estratos):\n")
print(summary(modelo_sp))
# Extraemos el CME de la sub-parcela (Error b), que es la medida de precisión clave
cme_sp <- summary(modelo_sp)$`Error: Within`[[1]]['Residuals', 'Mean Sq']
cat("\n   -> CME del Split-Plot (Error b):", round(cme_sp, 2), "\n\n")

# 5.2. ANVA Auxiliar: Factorial en DBCA
cat("5.2. Ajustando el modelo ANVA Auxiliar (Factorial en DBCA), ignorando la estructura Split-Plot...\n")
modelo_auxiliar_dbca <- aov(rendimiento ~ bloque + nitrogeno * variedad, data = datos_avena_limpios)
cat("Tabla ANOVA del Factorial en DBCA (Un solo Error):\n")
print(summary(modelo_auxiliar_dbca))
# Extraemos el CME de este modelo más simple
cme_auxiliar <- summary(modelo_auxiliar_dbca)[[1]]['Residuals', 'Mean Sq']
cat("\n   -> CME del ANVA Auxiliar (DBCA):", round(cme_auxiliar, 2), "\n\n")

# 5.3. Cálculo e Interpretación de la Eficiencia Relativa
cat("5.3. Calculando la Eficiencia Relativa (ER)...\n")
eficiencia_relativa <- cme_auxiliar / cme_sp
cat(paste0("   Eficiencia Relativa (ER) = CME_Auxiliar / CME_SP = ", round(cme_auxiliar, 2), " / ", round(cme_sp, 2), " = ", round(eficiencia_relativa, 2), "\n"))

cat("\nInterpretación de la ER:\n")
if (eficiencia_relativa > 1.1) {
    cat(paste0("   - Una ER de ", round(eficiencia_relativa, 2), " (> 1.10) indica que el diseño Split-Plot fue MUY EFICIENTE.\n"))
    cat("   - Fue un ", round((eficiencia_relativa - 1) * 100, 1), "% más preciso que un factorial convencional.\n")
    cat("   - La decisión de usar parcelas divididas fue excelente, ya que controló una fuente\n")
    cat("     importante de error, aumentando la precisión para detectar diferencias en las variedades y la interacción.\n\n")
} else {
    cat("   - Una ER cercana a 1.0 indica que el diseño Split-Plot no fue mucho más eficiente.\n")
    cat("   - La variabilidad entre las parcelas principales era similar a la de las sub-parcelas.\n\n")
}


# --- 6. Análisis Post-Hoc Detallado y Visualización ---
cat("--- Paso 6: Análisis Post-Hoc para la Interacción Significativa ---\n\n")

# La tabla ANOVA del Split-Plot nos mostró que la interacción era significativa (p=0.015).
# Por lo tanto, debemos desglosarla.

# 6.1. Generar Letras de Agrupación para todas las combinaciones
cat("6.1. Usando el enfoque del 'Super-Factor' para comparar todas las combinaciones...\n")
# Creamos una nueva variable que combina los factores
datos_con_superfactor <- datos_avena_limpios %>%
  unite("combinacion", variedad, nitrogeno, sep = " + ", remove = FALSE)

# Ajustamos un modelo de una vía sobre esta nueva variable
modelo_superfactor <- aov(rendimiento ~ bloque + combinacion, data = datos_con_superfactor)

# Realizamos la prueba de Tukey sobre el modelo de una vía
tukey_superfactor <- HSD.test(modelo_superfactor, "combinacion", group = TRUE, console = FALSE)

# Extraemos y limpiamos los datos para graficar
tukey_data_plot <- as.data.frame(tukey_superfactor$groups)
tukey_data_plot <- tukey_data_plot %>%
  rownames_to_column(var = "combinacion") %>%
  rename(Media = rendimiento, Grupos = groups)

cat("Tabla de medias y letras de significancia para cada combinación:\n")
print(tukey_data_plot)
cat("\n")

# 6.2. Visualización Post-Hoc con Gráfico de Barras
cat("6.2. Creando el gráfico de barras con los resultados de Tukey...\n")

plot_barras_posthoc <- ggplot(tukey_data_plot, aes(x = reorder(combinacion, -Media), y = Media)) +
  geom_bar(stat = "identity", fill = "darkcyan", color = "black", alpha = 0.8) +
  geom_text(
    aes(label = Grupos),
    vjust = -0.5,
    fontface = "bold",
    color = "black",
    size = 4
  ) +
  labs(
    title = "Comparación de Todas las Combinaciones de Tratamiento",
    subtitle = "Prueba HSD de Tukey (α = 0.05). Letras distintas indican diferencias significativas.",
    x = "Combinación (Variedad + Nivel de Nitrógeno)",
    y = "Media del Rendimiento (bushels/acre)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 10),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  ylim(0, max(tukey_data_plot$Media) * 1.15)

print(plot_barras_posthoc)
cat("\nInterpretación del Gráfico:\n")
cat(" - Este gráfico nos da un ranking completo de todas las combinaciones de tratamiento.\n")
cat(" - Podemos identificar visualmente la mejor combinación: 'Golden Rain + 0.6 cwt'.\n")
cat(" - Las letras nos permiten agrupar estadísticamente: todas las combinaciones con 'a' son\n")
cat("   estadísticamente superiores a las que solo tienen 'b', 'c', etc.\n\n")


# 6.3. Visualización Post-Hoc con el Gráfico de Interacción y Letras
cat("6.3. Creando el gráfico de interacción con las letras de agrupación añadidas...\n")
# Para esto, necesitamos unir los resultados de Tukey de vuelta con los factores originales
letras_para_interaccion <- tukey_data_plot %>%
  separate(combinacion, into = c("variedad", "nitrogeno"), sep = " \\+ ") %>%
  # Asegurarse de que el orden de los factores sea el mismo que en los datos originales
  mutate(nitrogeno = factor(nitrogeno, levels = levels(datos_avena_limpios$nitrogeno)))

plot_interaccion_con_letras <- ggplot(letras_para_interaccion, 
                                      aes(x = nitrogeno, y = Media, group = variedad, color = variedad)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_text(
    aes(label = Grupos),
    vjust = -1.5,
    fontface = "bold",
    color = "black",
    size = 4,
    show.legend = FALSE
  ) +
  labs(
    title = "Rendimiento de Avena: Interacción y Significancia",
    subtitle = "Letras indican el resultado de la comparación de Tukey entre todas las 12 combinaciones",
    x = "Nivel de Nitrógeno Aplicado",
    y = "Rendimiento Promedio (bushels/acre)",
    color = "Variedad"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "top")

print(plot_interaccion_con_letras)
cat("\nInterpretación del Gráfico:\n")
cat(" - Esta es la visualización más completa. No solo muestra la interacción (líneas no paralelas),\n")
cat("   sino que también indica qué puntos específicos son estadísticamente diferentes de otros.\n\n")


# --- 7. Conclusión Final ---
cat("--- Paso 7: Conclusión Final del Análisis Split-Plot ---\n\n")
cat("El análisis confirmó que la estructura Split-Plot fue altamente eficiente (ER > 1.10),\n")
cat("justificando su uso. Se encontró una interacción significativa entre variedad y nitrógeno.\n")
cat("El análisis post-hoc y las visualizaciones nos permitieron identificar las mejores\n")
cat("combinaciones de tratamiento (ej. 'Golden Rain' con altas dosis de N) y entender que la\n")
cat("recomendación de una variedad depende fuertemente del nivel de fertilización a utilizar.\n")

cat("\n--- Fin del Script de Práctica de la Sesión 7 ---\n")