# ==============================================================================
# TALLER PRÁCTICO: DE ANOVA A MODELOS LINEALES MIXTOS (LMM)
# Sesión 4 - Diseños Estadísticos en Agricultura
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Si no tienes alguna librería, instálala con install.packages("nombre_paquete")

library(tidyverse)  # Manipulación de datos y gráficos (ggplot2, dplyr)
library(lme4)       # Ajuste de Modelos Lineales Mixtos (lmer)
library(lmerTest)   # P-valores para LMM (tablas ANOVA tipo III)
library(broom)      # Para limpiar salidas de modelos
library(emmeans)    # Comparaciones múltiples (post-hoc) ajustadas
library(car)        # Diagnósticos adicionales

# Semilla para reproducibilidad (que a todos nos salgan los mismos números)
set.seed(123)

# ------------------------------------------------------------------------------
# 2. GENERACIÓN DE DATOS SIMULADOS (RCBD)
# ------------------------------------------------------------------------------
# Creamos una función para simular un ensayo agrícola con Bloques (RCBD).
# Esto nos permite tener la "verdad" del campo (sabemos cuánto rinde cada trt).

simular_rcbd <- function(n_trt = 4, n_bloques = 6, media_gen = 100, 
                         efecto_trt = 10, sd_bloque = 15, sd_error = 10) {
  
  # 1. Crear la estructura del diseño (Grid completo)
  tratamientos <- paste0("T", 1:n_trt)
  bloques      <- paste0("B", 1:n_bloques)
  
  datos <- expand.grid(tratamiento = tratamientos, bloque = bloques)
  
  # 2. Definir efectos verdaderos
  # Efecto Tratamiento (Fijo): T1=0, T2=10, T3=20...
  betas <- seq(0, by = efecto_trt, length.out = n_trt)
  names(betas) <- tratamientos
  
  # Efecto Bloque (Aleatorio): Muestreado de una Normal(0, sd_bloque)
  random_bloques <- rnorm(n_bloques, mean = 0, sd = sd_bloque)
  names(random_bloques) <- bloques
  
  # 3. Generar la variable respuesta (Rendimiento)
  # Modelo: y = mu + beta_trt + b_bloque + error
  datos$rendimiento <- media_gen + 
                       betas[datos$tratamiento] + 
                       random_bloques[datos$bloque] + 
                       rnorm(nrow(datos), mean = 0, sd = sd_error)
  
  return(datos)
}

# --- GENERAMOS EL DATASET "PERFECTO" (BALANCEADO) ---
datos_bal <- simular_rcbd(n_trt = 4, n_bloques = 5, sd_bloque = 20)

# Verificamos balance (todas las celdas deben tener 1 observación)
table(datos_bal$tratamiento, datos_bal$bloque)

# ------------------------------------------------------------------------------
# 3. ESCENARIO A: EL MUNDO IDEAL (DISEÑO BALANCEADO)
# ------------------------------------------------------------------------------
# Objetivo: Demostrar que cuando todo está perfecto, aov() y lmer() coinciden.

# Modelo 1: ANOVA Clásico (Bloque como Fijo)
m_aov <- aov(rendimiento ~ tratamiento + bloque, data = datos_bal)

# Modelo 2: LMM (Bloque como Aleatorio)
# Sintaxis: (1 | bloque) indica intercepto aleatorio por bloque
m_lmm <- lmer(rendimiento ~ tratamiento + (1 | bloque), data = datos_bal)

cat("=== COMPARACIÓN EN DISEÑO BALANCEADO ===\n")
print(summary(m_aov)) # Ver p-valor de tratamiento
cat("\n--- ANOVA del Modelo Mixto ---\n")
print(anova(m_lmm))   # Ver p-valor de tratamiento

# NOTA: Los p-valores para "tratamiento" deberían ser prácticamente idénticos.

# ------------------------------------------------------------------------------
# 4. ESCENARIO B: EL MUNDO REAL (DISEÑO DESBALANCEADO)
# ------------------------------------------------------------------------------
# 
# Vamos a "perder" datos aleatoriamente (plantas muertas, error de cosecha).

filas_a_borrar <- sample(1:nrow(datos_bal), 4) # Borramos 4 parcelas al azar
datos_desbal   <- datos_bal[-filas_a_borrar, ]

# Verificamos desbalance (verás ceros en la tabla)
print(table(datos_desbal$tratamiento, datos_desbal$bloque))

# --- EL PROBLEMA DEL ANOVA CLÁSICO (TYPE I SS) ---
# El orden importa en aov() cuando hay desbalance.

m_aov_bad1 <- aov(rendimiento ~ tratamiento + bloque, data = datos_desbal)
m_aov_bad2 <- aov(rendimiento ~ bloque + tratamiento, data = datos_desbal)

cat("\n=== PROBLEMA DEL ANOVA CLÁSICO CON DESBALANCE ===\n")
cat("Orden 1 (Trat + Bloque) - SS Tratamiento:\n")
print(anova(m_aov_bad1)[1, "Sum Sq"]) # Suma de cuadrados
cat("Orden 2 (Bloque + Trat) - SS Tratamiento:\n")
print(anova(m_aov_bad2)[2, "Sum Sq"]) # Suma de cuadrados cambia

# --- LA SOLUCIÓN ROBUSTA: LMM (REML) ---
# lmer usa REML y lmerTest usa Sumas de Cuadrados Tipo III por defecto.
# El resultado NO depende del orden.

m_lmm_unbal <- lmer(rendimiento ~ tratamiento + (1 | bloque), data = datos_desbal)

cat("\n--- Resultado LMM (Robusto al desbalance) ---\n")
print(anova(m_lmm_unbal))

# ------------------------------------------------------------------------------
# 5. INTERPRETACIÓN DE VARIABILIDAD (ICC)
# ------------------------------------------------------------------------------
# 
# ¿Vale la pena bloquear? ¿Cuánta varianza explican los bloques?

# Extraemos componentes de varianza
vc <- as.data.frame(VarCorr(m_lmm_unbal))
print(vc)

sigma2_bloque <- vc[vc$grp == "bloque", "vcov"]
sigma2_resid  <- vc[vc$grp == "Residual", "vcov"]

# Cálculo del ICC (Coeficiente de Correlación Intraclase)
ICC <- sigma2_bloque / (sigma2_bloque + sigma2_resid)

cat(sprintf("\nEl ICC es: %.3f (%.1f%% de la varianza total se debe a los bloques)\n", 
            ICC, ICC * 100))

# REGLA DE DEDO:
# ICC > 0.05 : El bloqueo fue útil.
# ICC > 0.20 : El bloqueo fue CRÍTICO. Ignorarlo sería un error grave (pseudorreplicación).

# ------------------------------------------------------------------------------
# 6. DIAGNÓSTICOS DEL MODELO
# ------------------------------------------------------------------------------
# Al igual que en regresión, debemos validar supuestos sobre los RESIDUOS.

par(mfrow = c(1, 2)) # Dividir pantalla gráfico

# 1. Homocedasticidad (Residuos vs Ajustados)
plot(fitted(m_lmm_unbal), resid(m_lmm_unbal), 
     main = "Residuos vs Ajustados", xlab = "Predichos", ylab = "Residuos")
abline(h = 0, col = "red", lty = 2)

# 2. Normalidad (Q-Q Plot)
qqnorm(resid(m_lmm_unbal))
qqline(resid(m_lmm_unbal), col = "red")

# Restablecer pantalla
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 7. REPORTE FINAL Y COMPARACIÓN DE MEDIAS (POST-HOC)
# ------------------------------------------------------------------------------
# Usamos emmeans (Estimated Marginal Means) que es el estándar de oro para LMM.

cat("\n=== MEDIAS ESTIMADAS Y COMPARACIONES ===\n")
medias_estimadas <- emmeans(m_lmm_unbal, ~ tratamiento)

# Mostrar medias ajustadas (tienen en cuenta el efecto del bloque)
print(medias_estimadas)

# Comparación de pares (Tukey adjustment por defecto)
comparaciones <- pairs(medias_estimadas)
print(comparaciones)

# Visualización rápida de resultados
plot(medias_estimadas, comparisons = TRUE) + 
  theme_bw() + 
  labs(title = "Medias de Tratamientos ajustadas por LMM", 
       subtitle = "Barras rojas son intervalos de comparación")