# ==============================================================================
# TALLER: Modelos de Crecimiento No Lineal (Gompertz vs Logístico)
# Contexto: Acumulación de Biomasa en Maíz (Simulación)
# Referencia: Archontoulis & Miguez (2015). Agronomy Journal.
# ==============================================================================

# 1. LIBRERÍAS
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
if (!require("minpack.lm")) install.packages("minpack.lm") # Algoritmo Levenberg-Marquardt (más robusto que nls base)

library(ggplot2)
library(dplyr)
library(minpack.lm)

# ==============================================================================
# PARTE 1: LA "VERDAD" BIOLÓGICA (Simulación de Datos)
# ==============================================================================

# Definimos la función Gompertz con parámetros biológicos interpretables:
# y = Asym * exp(-exp(-k * (t - Ti)))
# Asym: Biomasa máxima (techo)
# k: Tasa relativa de crecimiento
# Ti: Tiempo (días) en el punto de inflexión (donde la velocidad es máxima)

gompertz_fun <- function(t, Asym, k, Ti) {
  Asym * exp(-exp(-k * (t - Ti)))
}

# --- CONFIGURACIÓN DEL EXPERIMENTO VIRTUAL ---
set.seed(123)
dias <- seq(0, 120, by = 5)  # Muestreos cada 5 días hasta madurez

# Parámetros "Verdaderos" (DGP - Data Generating Process)
verdadero_Asym <- 25000  # kg MS/ha (Biomasa final)
verdadero_k    <- 0.08   # Tasa de crecimiento
verdadero_Ti   <- 40     # El cultivo acelera máximo a los 40 días (temprano)

# Generamos la respuesta perfecta
y_verdad <- gompertz_fun(dias, verdadero_Asym, verdadero_k, verdadero_Ti)

# Añadimos Ruido (Error experimental, variabilidad de campo)
# El ruido suele ser heterocedástico (mayor biomasa = mayor error), pero usaremos normal simple por didáctica.
ruido <- rnorm(length(dias), mean = 0, sd = 800)
y_observado <- pmax(0, y_verdad + ruido) # pmax evita biomasa negativa

df_cultivo <- data.frame(tiempo = dias, biomasa = y_observado)

# Visualización Previa
plot(df_cultivo$tiempo, df_cultivo$biomasa, pch=19, col="darkgreen",
     main = "Datos Simulados: Crecimiento de Cultivo",
     xlab = "Días después de siembra (DDS)", ylab = "Biomasa (kg/ha)")

# ==============================================================================
# PARTE 2: EL DUELO DE MODELOS (Ajuste NLS)
# Hipótesis: Como los datos vienen de un proceso Gompertz (asimétrico),
# el modelo Logístico (simétrico) debería fallar en capturar la forma exacta.
# ==============================================================================

# --- A) Ajuste Modelo GOMPERTZ ---
# Usamos nlsLM porque es más robusto a valores iniciales malos que nls()
fit_gompertz <- nlsLM(biomasa ~ Asym * exp(-exp(-k * (tiempo - Ti))),
                      data = df_cultivo,
                      start = list(Asym = 24000, k = 0.05, Ti = 50))

# --- B) Ajuste Modelo LOGÍSTICO (Para comparar) ---
# Ecuación: y = Asym / (1 + exp((Ti - t)/scale)) 
# Nota: La parametrización puede variar, R usa SSlogis (Asym, xmid, scal)
fit_logistic <- nlsLM(biomasa ~ Asym / (1 + exp(-k * (tiempo - Ti))),
                      data = df_cultivo,
                      start = list(Asym = 24000, k = 0.05, Ti = 60))

# ==============================================================================
# PARTE 3: DIAGNÓSTICO Y COMPARACIÓN CRÍTICA
# ==============================================================================

cat("\n>>> COMPARACIÓN DE MODELOS (AIC) <<<\n")
# Calculamos AIC (Menor es mejor)
aic_gomp <- AIC(fit_gompertz)
aic_log  <- AIC(fit_logistic)

resumen_modelos <- data.frame(
  Modelo = c("Gompertz", "Logístico"),
  AIC = c(aic_gomp, aic_log),
  Delta_AIC = c(aic_gomp - min(aic_gomp, aic_log), 
                aic_log - min(aic_gomp, aic_log))
)
print(resumen_modelos)

cat("\nInterpretación Crítica:\n")
if(aic_gomp < aic_log - 2) {
  cat("El modelo GOMPERTZ es estadísticamente superior. \nEsto confirma que el crecimiento es ASIMÉTRICO (crece rápido temprano).\n")
} else {
  cat("Los modelos son similares. La asimetría no es lo suficientemente fuerte para distinguirla con este ruido.\n")
}

# ==============================================================================
# PARTE 4: VISUALIZACIÓN DE ALTA CALIDAD
# ==============================================================================

# Generar curvas suaves para graficar
tiempo_grid <- seq(0, 120, length.out = 200)

pred_gomp <- predict(fit_gompertz, newdata = data.frame(tiempo = tiempo_grid))
pred_log  <- predict(fit_logistic, newdata = data.frame(tiempo = tiempo_grid))

df_pred <- data.frame(
  tiempo = rep(tiempo_grid, 2),
  prediccion = c(pred_gomp, pred_log),
  Modelo = rep(c("Gompertz (Asimétrico)", "Logístico (Simétrico)"), each = 200)
)

# Puntos de inflexión estimados
coef_g <- coef(fit_gompertz)
inflex_gomp_x <- coef_g["Ti"]
inflex_gomp_y <- coef_g["Asym"] * exp(-1) # En Gompertz, inflexión es al ~36.8% de Asym

coef_l <- coef(fit_logistic)
inflex_log_x  <- coef_l["Ti"]
inflex_log_y  <- coef_l["Asym"] * 0.5   # En Logístico, inflexión es al 50% de Asym

# GRÁFICO FINAL
p <- ggplot() +
  # 1. Datos observados
  geom_point(data = df_cultivo, aes(x = tiempo, y = biomasa), 
             alpha = 0.6, size = 3, color = "gray30") +
  # 2. Curvas de los modelos
  geom_line(data = df_pred, aes(x = tiempo, y = prediccion, color = Modelo), 
            linewidth = 1.2) +
  # 3. Marcar Puntos de Inflexión (La diferencia clave)
  annotate("point", x = inflex_gomp_x, y = inflex_gomp_y, color = "blue", size=4, shape=18) +
  annotate("text", x = inflex_gomp_x + 5, y = inflex_gomp_y, 
           label = "Inflexión Gompertz\n(~37% Max)", color = "blue", size=3, hjust=0) +
  
  annotate("point", x = inflex_log_x, y = inflex_log_y, color = "red", size=4, shape=18) +
  annotate("text", x = inflex_log_x - 5, y = inflex_log_y, 
           label = "Inflexión Logística\n(50% Max)", color = "red", size=3, hjust=1) +
  
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  labs(title = "Batalla de Modelos: Gompertz vs Logístico",
       subtitle = "Note cómo el Gompertz captura mejor el arranque rápido del cultivo",
       x = "Tiempo (Días)", y = "Biomasa (kg/ha)") +
  theme(legend.position = "top")

print(p)

# ==============================================================================
# PARTE 5: REPORTE DE PARÁMETROS BIOLÓGICOS
# ==============================================================================
cat("\n>>> PARÁMETROS ESTIMADOS (Gompertz) <<<\n")
print(coef(fit_gompertz))
cat("\nInterpretación:\n")
cat(sprintf("- Asym (Techo): %.0f kg/ha (Estimación de rendimiento potencial)\n", coef(fit_gompertz)["Asym"]))
cat(sprintf("- Ti (Inflexión): %.1f días. Momento de máxima velocidad de crecimiento.\n", coef(fit_gompertz)["Ti"]))
cat("- k (Tasa): Velocidad relativa de aproximación a la asíntota.\n")