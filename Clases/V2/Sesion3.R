# -----------------------------------------------------------------------------
# Script de Práctica - Sesión 3: Probabilidad y Distribuciones Esenciales
# -----------------------------------------------------------------------------

# --- 0. Configuración Inicial y Paquetes ---
# Asegúrate de tener estos paquetes instalados. Si no, ejecútalos una vez:
# install.packages("tidyverse") # Incluye ggplot2
# install.packages("MASS")      # Para fitdistr

library(tidyverse)
library(MASS) # Para la función fitdistr, aunque en este script nos enfocaremos en simulación y visualización.

# Establecer una semilla para reproducibilidad (opcional, pero útil para obtener los mismos resultados)
set.seed(123)

cat("¡Bienvenido a la práctica de la Sesión 3!\n")
cat("Este script te guiará a través de conceptos de probabilidad y distribuciones.\n\n")

# --- 1. Teoría de Probabilidad (Revisión Conceptual) ---
# Esta sección es principalmente conceptual y se discute en clase.
# Aquí solo recordaremos algunos puntos clave con ejemplos simples.

cat("--- 1. Teoría de Probabilidad ---\n")

# 1.1 Experimento Aleatorio y Espacio Muestral (Omega)
# Ejemplo: Evaluar la germinación de 3 semillas de maíz.
# Un resultado posible podría ser (Germina, No Germina, Germina)
# Omega contendría todas las 2^3 = 8 combinaciones posibles.
cat("1.1 Espacio Muestral:\n")
cat("   Si evaluamos la germinación de 1 semilla: Omega = {Germina, No Germina}\n")
cat("   P(Germina) podría ser 0.8, P(No Germina) = 0.2.  P(Omega) = 0.8 + 0.2 = 1\n\n")

# 1.2 Eventos
# Evento A: "Exactamente 2 semillas germinan de 3".
# Evento B: "Al menos 1 semilla germina de 3".

# 1.3 Probabilidad Condicional
# P(Planta enferma | Síntoma X presente)
# Ejemplo: Probabilidad de tener roya (R) dado que la hoja tiene pústulas amarillas (PA)
# P(R | PA) = P(R y PA) / P(PA)
cat("1.3 Probabilidad Condicional:\n")
cat("   P(Enfermedad | Humedad Alta) nos dice la probabilidad de enfermedad si ya sabemos que hay humedad alta.\n\n")

# 1.4 Eventos Independientes
# Ejemplo: La germinación de una semilla en una maceta no afecta la germinación de otra semilla en OTRA maceta (asumiendo condiciones idénticas e independientes).
# Si P(Germina Semilla1) = 0.8 y P(Germina Semilla2) = 0.8 (independientes)
# Entonces P(Ambas Germinan) = P(Semilla1 Germina) * P(Semilla2 Germina) = 0.8 * 0.8 = 0.64
cat("1.4 Eventos Independientes:\n")
cat("   Si la probabilidad de lluvia hoy es 0.3 y la de mañana es 0.4 (y son independientes),\n")
cat("   la probabilidad de que llueva ambos días es 0.3 * 0.4 = 0.12\n\n")


# --- 2. Definición y Simulación de Distribuciones ---
cat("--- 2. Simulación de Distribuciones ---\n")

# 2.1 Distribución Binomial: Infestación de Plagas
# Supongamos que inspeccionamos lotes de 20 plantas de tomate.
# La probabilidad de que una planta individual esté infestada por mosca blanca es p = 0.15.
# Queremos simular el número de plantas infestadas en 100 de estos lotes.

n_plantas_por_lote <- 20
prob_infestacion_individual <- 0.15
numero_lotes_simulados <- 100

# Usamos rbinom() para simular:
# n: número de simulaciones (lotes)
# size: número de ensayos en cada simulación (plantas por lote)
# prob: probabilidad de "éxito" (planta infestada) en cada ensayo
plantas_infestadas_por_lote <- rbinom(n = numero_lotes_simulados,
                                      size = n_plantas_por_lote,
                                      prob = prob_infestacion_individual)

cat("2.1 Distribución Binomial (Infestación de Plagas):\n")
cat("   Simulación del número de plantas infestadas en", numero_lotes_simulados, "lotes de", n_plantas_por_lote, "plantas (p_infestación =", prob_infestacion_individual, "):\n")
print(head(plantas_infestadas_por_lote, 10)) # Muestra los primeros 10 resultados
cat("   Media observada de plantas infestadas por lote:", mean(plantas_infestadas_por_lote), "\n")
cat("   Media teórica (np):", n_plantas_por_lote * prob_infestacion_individual, "\n")
cat("   Varianza observada:", var(plantas_infestadas_por_lote), "\n")
cat("   Varianza teórica (np(1-p)):", n_plantas_por_lote * prob_infestacion_individual * (1 - prob_infestacion_individual), "\n\n")

# Calculando la probabilidad de observar exactamente k infestaciones, ej. P(X=3)
# Usamos dbinom() (función de masa de probabilidad)
prob_exactamente_3_infestadas <- dbinom(x = 3, size = n_plantas_por_lote, prob = prob_infestacion_individual)
cat("   Probabilidad de observar EXACTAMENTE 3 plantas infestadas en un lote:", round(prob_exactamente_3_infestadas, 4), "\n")

# Calculando la probabilidad de observar k o menos infestaciones, ej. P(X <= 2)
# Usamos pbinom() (función de distribución acumulada)
prob_2_o_menos_infestadas <- pbinom(q = 2, size = n_plantas_por_lote, prob = prob_infestacion_individual)
cat("   Probabilidad de observar 2 O MENOS plantas infestadas en un lote:", round(prob_2_o_menos_infestadas, 4), "\n\n")


# 2.2 Distribución de Poisson: Conteo de Lesiones Foliares
# Supongamos que contamos el número de manchas de mildiu por hoja de vid.
# En promedio (tasa lambda), encontramos 2.5 manchas por hoja.
# Queremos simular el número de manchas en 200 hojas inspeccionadas.

lambda_manchas_por_hoja <- 2.5
numero_hojas_simuladas <- 200

# Usamos rpois() para simular:
# n: número de simulaciones (hojas)
# lambda: tasa media de ocurrencia del evento
manchas_por_hoja <- rpois(n = numero_hojas_simuladas,
                          lambda = lambda_manchas_por_hoja)

cat("2.2 Distribución de Poisson (Lesiones Foliares):\n")
cat("   Simulación del número de manchas de mildiu en", numero_hojas_simuladas, "hojas (lambda =", lambda_manchas_por_hoja, "manchas/hoja):\n")
print(head(manchas_por_hoja, 10))
cat("   Media observada de manchas por hoja:", mean(manchas_por_hoja), "\n")
cat("   Media teórica (lambda):", lambda_manchas_por_hoja, "\n")
cat("   Varianza observada:", var(manchas_por_hoja), "\n") # Debería ser cercana a la media para Poisson
cat("   Varianza teórica (lambda):", lambda_manchas_por_hoja, "\n\n")

# Calculando la probabilidad de observar exactamente k manchas, ej. P(X=1)
# Usamos dpois()
prob_exactamente_1_mancha <- dpois(x = 1, lambda = lambda_manchas_por_hoja)
cat("   Probabilidad de observar EXACTAMENTE 1 mancha en una hoja:", round(prob_exactamente_1_mancha, 4), "\n")

# Calculando la probabilidad de observar más de k manchas, ej. P(X > 4) = 1 - P(X <= 4)
# Usamos ppois()
prob_mas_de_4_manchas <- 1 - ppois(q = 4, lambda = lambda_manchas_por_hoja)
cat("   Probabilidad de observar MÁS DE 4 manchas en una hoja:", round(prob_mas_de_4_manchas, 4), "\n\n")


# 2.3 Distribución Normal: Variabilidad de Rendimiento
# Supongamos que el rendimiento del maíz en una región sigue una distribución normal.
# La media del rendimiento es mu = 8.5 toneladas/hectárea.
# La desviación estándar es sigma = 1.2 toneladas/hectárea.
# Queremos simular el rendimiento de 150 parcelas.

media_rendimiento_maiz <- 8.5 # t/ha
sd_rendimiento_maiz <- 1.2    # t/ha
numero_parcelas_simuladas <- 150

# Usamos rnorm() para simular:
# n: número de simulaciones (parcelas)
# mean: media de la distribución
# sd: desviación estándar de la distribución
rendimiento_parcelas <- rnorm(n = numero_parcelas_simuladas,
                              mean = media_rendimiento_maiz,
                              sd = sd_rendimiento_maiz)

cat("2.3 Distribución Normal (Variabilidad de Rendimiento):\n")
cat("   Simulación del rendimiento de maíz en", numero_parcelas_simuladas, "parcelas (mu =", media_rendimiento_maiz, ", sigma =", sd_rendimiento_maiz, "t/ha):\n")
print(head(rendimiento_parcelas, 10))
cat("   Media observada de rendimiento:", mean(rendimiento_parcelas), "\n")
cat("   Desviación estándar observada:", sd(rendimiento_parcelas), "\n\n")

# Calculando la probabilidad de que el rendimiento sea menor a un valor, ej. P(X < 7 t/ha)
# Usamos pnorm() (función de distribución acumulada)
prob_menor_a_7 <- pnorm(q = 7, mean = media_rendimiento_maiz, sd = sd_rendimiento_maiz)
cat("   Probabilidad de que el rendimiento sea MENOR a 7 t/ha:", round(prob_menor_a_7, 4), "\n")

# Calculando la probabilidad de que el rendimiento esté entre dos valores, ej. P(8 < X < 10 t/ha)
# P(8 < X < 10) = P(X < 10) - P(X < 8)
prob_entre_8_y_10 <- pnorm(q = 10, mean = media_rendimiento_maiz, sd = sd_rendimiento_maiz) -
                     pnorm(q = 8, mean = media_rendimiento_maiz, sd = sd_rendimiento_maiz)
cat("   Probabilidad de que el rendimiento esté ENTRE 8 y 10 t/ha:", round(prob_entre_8_y_10, 4), "\n")

# Encontrando el valor de rendimiento que es superado solo por el 5% de las parcelas (cuantil 0.95)
# Usamos qnorm() (función cuantil)
cuantil_95_rendimiento <- qnorm(p = 0.95, mean = media_rendimiento_maiz, sd = sd_rendimiento_maiz)
cat("   El 5% de las parcelas con mayor rendimiento superan aproximadamente:", round(cuantil_95_rendimiento, 2), "t/ha\n\n")


# --- 3. Graficar Distribuciones con ggplot2 ---
cat("--- 3. Graficar Distribuciones ---\n")
cat("   Ahora visualizaremos las distribuciones simuladas usando ggplot2.\n")
cat("   Asegúrate de ejecutar la ventana de Gráficos (Plots) en RStudio para verlos.\n\n")

# Preparar dataframes para ggplot
df_binomial <- data.frame(Infestaciones = plantas_infestadas_por_lote)
df_poisson <- data.frame(Manchas = manchas_por_hoja)
df_normal <- data.frame(Rendimiento = rendimiento_parcelas)

# Gráfico para la Distribución Binomial (Histograma de frecuencias)
# Como es discreta, un gráfico de barras es más apropiado que un histograma continuo
# Contamos las ocurrencias de cada número de infestaciones
conteo_infestaciones <- df_binomial %>%
  count(Infestaciones, name = "Frecuencia")

plot_binomial <- ggplot(conteo_infestaciones, aes(x = factor(Infestaciones), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black", alpha=0.7) +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 3) + # Añadir etiquetas de frecuencia
  labs(title = "Distribución Binomial Simulada: Plantas Infestadas",
       x = "Número de Plantas Infestadas por Lote (de 20)",
       y = "Frecuencia Absoluta (de 100 lotes)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_binomial)
cat("   GRÁFICO: Se ha generado el gráfico de barras para la distribución Binomial.\n\n")

# Gráfico para la Distribución de Poisson (Histograma de frecuencias)
conteo_manchas <- df_poisson %>%
  count(Manchas, name = "Frecuencia")

plot_poisson <- ggplot(conteo_manchas, aes(x = factor(Manchas), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black", alpha=0.7) +
  geom_text(aes(label = Frecuencia), vjust = -0.5, size = 3) +
  labs(title = "Distribución de Poisson Simulada: Lesiones Foliares",
       x = "Número de Manchas por Hoja",
       y = "Frecuencia Absoluta (de 200 hojas)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_poisson)
cat("   GRÁFICO: Se ha generado el gráfico de barras para la distribución de Poisson.\n\n")

# Gráfico para la Distribución Normal (Histograma con curva de densidad)
plot_normal <- ggplot(df_normal, aes(x = Rendimiento)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(color = "darkblue", linewidth = 1) + # Superponer curva de densidad estimada
  stat_function(fun = dnorm, args = list(mean = media_rendimiento_maiz, sd = sd_rendimiento_maiz),
                color = "red", linetype = "dashed", linewidth = 1) + # Superponer curva teórica
  labs(title = "Distribución Normal Simulada: Rendimiento de Maíz",
       x = "Rendimiento (toneladas/hectárea)",
       y = "Densidad") +
  annotate("text", x = media_rendimiento_maiz + 2*sd_rendimiento_maiz, y = 0.05,
           label = "Curva teórica (roja)\nDensidad estimada (azul)", hjust=0, size=3, color="darkred") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
print(plot_normal)
cat("   GRÁFICO: Se ha generado el histograma con curva de densidad para la Normal.\n\n")

# --- 4. Discusión sobre la Elección de Distribución (Conceptual) ---
cat("--- 4. Elección de Distribución ---\n")
cat("La elección de qué distribución usar depende de la naturaleza de tus datos:\n")
cat(" - BINOMIAL: Número de 'éxitos' en un número FIJO de ensayos independientes (ej. plantas enfermas de N muestreadas).\n")
cat(" - POISSON: Conteo de eventos 'raros' en un intervalo/unidad FIJA (ej. número de insectos por trampa, defectos por metro cuadrado).\n")
cat("            A menudo se usa cuando n es grande y p es pequeña en la binomial (lambda = np).\n")
cat(" - NORMAL: Variables continuas que resultan de la suma de muchos efectos pequeños (Teorema Central del Límite).\n")
cat("           (ej. rendimiento, altura de planta, peso de fruto). Siempre verificar supuestos de normalidad (QQ-plot, Shapiro-Wilk).\n\n")

# Ejemplo: ¿Cuándo usar Poisson en lugar de Binomial?
# Si inspeccionamos 1000 semillas (n=1000) y la probabilidad de un defecto genético raro es p=0.002.
# Podríamos usar Binomial(n=1000, p=0.002).
# Media Binomial = np = 1000 * 0.002 = 2.
# Varianza Binomial = np(1-p) = 2 * (1-0.002) = 1.996.
# Como n es grande y p pequeña, Poisson con lambda = np = 2 sería una buena aproximación.
# Varianza Poisson = lambda = 2.
# Las medias y varianzas son muy similares.
cat("   Ejemplo de aproximación Binomial a Poisson:\n")
cat("   Si n es grande y p pequeña, Binomial(n,p) ~ Poisson(lambda=np).\n")
cat("   n=1000, p=0.002 => Binomial(mean=2, var=1.996)\n")
cat("   lambda=np=2    => Poisson(mean=2, var=2)\n\n")


# --- 5. Scripts de simulación y gráficos interpretados (Resultado Esperado) ---
# El resultado de esta sesión es que puedas:
# 1. Entender los parámetros clave de estas tres distribuciones.
# 2. Simular datos usando rbinom(), rpois(), rnorm().
# 3. Calcular probabilidades usando dbinom/dpois/dnorm y pbinom/ppois/pnorm.
# 4. Generar gráficos básicos (histogramas, barras, densidades) para visualizar estas distribuciones.
# 5. Tener una idea inicial de cuándo aplicar cada distribución en contextos agronómicos.

cat("--- Fin de la Práctica de la Sesión 3 ---\n")
cat("¡Buen trabajo! Revisa los gráficos generados y experimenta cambiando los parámetros.\n")

# ---- EXTRA: Verificación de Normalidad para los datos de rendimiento simulados ----
cat("\n--- EXTRA: Verificación de Normalidad para Datos de Rendimiento ---\n")
if (length(rendimiento_parcelas) >= 3 && length(rendimiento_parcelas) <= 5000) { # Shapiro-Wilk es para n entre 3 y 5000
  shapiro_test_rendimiento <- shapiro.test(rendimiento_parcelas)
  cat("Prueba de Shapiro-Wilk para la normalidad de los rendimientos simulados:\n")
  print(shapiro_test_rendimiento)
  if(shapiro_test_rendimiento$p.value > 0.05){
    cat("   Conclusión: El p-valor es > 0.05, por lo tanto, no hay evidencia para rechazar la hipótesis nula.\n")
    cat("               Los datos simulados parecen seguir una distribución normal, como se esperaba.\n\n")
  } else {
    cat("   Conclusión: El p-valor es <= 0.05. Se rechaza la hipótesis nula de normalidad.\n")
    cat("               Esto sería inesperado para datos simulados de rnorm(), revisar la simulación o el tamaño de muestra.\n\n")
  }
} else {
  cat("   No se realizó la prueba de Shapiro-Wilk porque el número de observaciones está fuera del rango (3-5000).\n\n")
}

# Gráfico Q-Q para los rendimientos
# Para ver este gráfico, necesitarías ejecutarlo interactivamente en RStudio
# o guardarlo en un archivo.
# quartz() # Para macOS, para abrir una nueva ventana gráfica si no estás en RStudio
# windows() # Para Windows
# x11() # Para Linux
cat("   Para el QQ-plot, ejecute manualmente en su consola:\n")
cat("   qqnorm(rendimiento_parcelas, main='QQ-plot para Rendimiento Simulado')\n")
cat("   qqline(rendimiento_parcelas, col='red', lwd=2)\n")

# Para que el script no abra una ventana gráfica automáticamente si se ejecuta en un entorno no interactivo:
# if (interactive()) {
#   qqnorm(rendimiento_parcelas, main="QQ-plot para Rendimiento Simulado")
#   qqline(rendimiento_parcelas, col="red", lwd=2)
# }