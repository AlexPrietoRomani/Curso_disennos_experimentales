# ==============================================================================
# TALLER PRÁCTICO: Modelos de Regresión (Lineal y No Lineal) en R
# Asignatura: Diseños Estadísticos - Sesión 3
# ==============================================================================
# OBJETIVOS:
# 1. Ajustar un Modelo de Regresión Lineal Simple (MRLS) e interpretar coeficientes.
# 2. Realizar el diagnóstico de supuestos (la "prueba de la verdad").
# 3. Comparar modelos lineales vs. no lineales (Cuadrático, Logarítmico) usando AIC.
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Usaremos 'ggplot2' para gráficos elegantes y 'car' para diagnósticos robustos.
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("car")) install.packages("car")     # Para Levene y diagnósticos
if (!require("dplyr")) install.packages("dplyr") # Para manipulación de datos

library(ggplot2)
library(car)
library(dplyr)

# ==============================================================================
# PARTE I: El Modelo Lineal Simple (MRLS)
# Datos: 'trees' (Dataset clásico de R: Girth (diametro) vs Volume (volumen))
# Hipótesis: A mayor diámetro del tronco, mayor volumen de madera.
# ==============================================================================

cat("\n--- INICIO PARTE I: MRLS ---\n")

# 1. Carga y Exploración Visual (EDA)
data("trees")

df_madera <- trees %>% 
  rename(x = Girth, y = Volume) # Renombramos para generalizar (x=insumo/predictor, y=respuesta)

# Visualización previa: ¿Tiene sentido ajustar una recta?
p1 <- ggplot(df_madera, aes(x = x, y = y)) +
  geom_point(color = "darkblue", alpha = 0.6, size = 3) +
  labs(title = "Exploración: Volumen vs Diámetro", 
       subtitle = "Buscamos una relación monótona creciente",
       x = "Diámetro (pulgadas)", y = "Volumen (pies cúbicos)") +
  theme_bw()

print(p1)

# 2. Ajuste del Modelo (OLS - Mínimos Cuadrados Ordinarios)
# Modelo: y = beta0 + beta1*x + error
modelo_lin <- lm(y ~ x, data = df_madera)

# 3. Interpretación de Resultados (Summary)
cat("\n>>> Resumen del Modelo Lineal:\n")
summary(modelo_lin)

# CRÍTICA DE RESULTADOS:
# - Estimate (Intercept/beta0): Valor de y cuando x=0. (A veces no tiene sentido biológico).
# - Estimate (x/beta1): Pendiente. ¿Cuánto aumenta 'y' por cada unidad extra de 'x'?
# - Pr(>|t|): Si es < 0.05, rechazamos que beta1 sea 0. Existe asociación significativa.
# - R-squared: ¿Qué % de la variabilidad de los datos explica mi recta?

# 4. Visualización del Ajuste con Intervalos de Confianza
p2 <- ggplot(df_madera, aes(x = x, y = y)) +
  geom_point(color = "darkblue", alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", fill = "pink") +
  labs(title = "Ajuste MRLS con IC 95%", 
       subtitle = "La sombra rosa es la zona de confianza de la media") +
  theme_bw()

print(p2)

# ==============================================================================
# PARTE II: Diagnóstico de Supuestos (El detector de mentiras)
# No confíes en el p-valor si los residuos no cumplen las reglas.
# ==============================================================================

cat("\n--- INICIO PARTE II: DIAGNÓSTICO ---\n")

# Extraemos residuos y valores ajustados
df_diag <- data.frame(
  residuos = residuals(modelo_lin),
  ajustados = fitted(modelo_lin)
)

# 1. Homocedasticidad (Varianza constante)
# Gráfico: Residuos vs Ajustados.
# BUSCAMOS: Una "nube sin forma" alrededor del 0.
# EVITAMOS: Formas de "megáfono" (fanning) o curvas (U).
p_resid <- ggplot(df_diag, aes(x = ajustados, y = residuos)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Diagnóstico 1: Homocedasticidad y Linealidad",
       x = "Valores Ajustados (Predichos)", 
       y = "Residuos (Errores)") +
  theme_bw()

print(p_resid)

# Prueba formal (Breusch-Pagan implícito con ncvTest del paquete car)
cat("\n>>> Test de Varianza Constante (Non-Constant Error Variance):\n")
print(car::ncvTest(modelo_lin)) 

# Si p < 0.05, tenemos heterocedasticidad (problema de varianza).

# 2. Normalidad de los Residuos
# Gráfico Q-Q Plot: Los puntos deben seguir la línea diagonal.
p_qq <- ggplot(df_diag, aes(sample = residuos)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Diagnóstico 2: Normalidad (Q-Q Plot)") +
  theme_bw()

print(p_qq)

# Prueba formal: Shapiro-Wilk
cat("\n>>> Test de Normalidad (Shapiro-Wilk):\n")
print(shapiro.test(residuals(modelo_lin)))
# Si p < 0.05, los residuos NO son normales (cuidado con muestras pequeñas).

# ==============================================================================
# PARTE III: Modelos No Lineales y Selección (Caso Agronómico)
# Situación: Dosis de Nitrógeno vs Rendimiento.
# Esperamos rendimientos decrecientes o un óptimo (curva), no una recta infinita.
# ==============================================================================

cat("\n--- INICIO PARTE III: MODELOS NO LINEALES ---\n")

# 1. Generamos datos simulados con curvatura (Parábola)
set.seed(123)
n <- 60
nitrogeno <- sort(runif(n, 0, 200)) # Dosis 0 a 200 kgN/ha
# Verdad biológica: 50 base + 2*N - 0.01*N^2 (Rendimiento marginal decreciente) + Ruido
rendimiento <- 50 + 2 * nitrogeno - 0.01 * nitrogeno^2 + rnorm(n, sd = 15)

df_agro <- data.frame(x = nitrogeno, y = rendimiento)

# Visualización inicial
ggplot(df_agro, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  labs(title = "Datos Simulados: Dosis de Nitrógeno vs Rendimiento",
       x = "Dosis de Nitrógeno (kg/ha)", y = "Rendimiento (q/ha)") +
  theme_bw()

# 2. Ajustamos tres modelos competidores
# A) Lineal: y ~ x (Asume crecimiento infinito)
mod_lin <- lm(y ~ x, data = df_agro)

# B) Cuadrático: y ~ x + x^2 (Permite un óptimo/pico)
mod_quad <- lm(y ~ x + I(x^2), data = df_agro)

# C) Logarítmico: y ~ log(x) (Crecimiento rápido al inicio, luego se estanca)
# Nota: log(0) es infinito, sumamos una pequeña constante o filtramos x>0
mod_log <- lm(y ~ log(x), data = subset(df_agro, x > 0))

# 3. Comparación Estadística: AIC y R2 Ajustado
# Referencia: Burnham & Anderson (2002). El AIC penaliza la complejidad.
# Menor AIC = Mejor equilibrio entre ajuste y parsimonia.

metricas <- data.frame(
  Modelo = c("Lineal", "Cuadrático", "Logarítmico"),
  AIC = c(AIC(mod_lin), AIC(mod_quad), AIC(mod_log)),
  R2_Adj = c(summary(mod_lin)$adj.r.squared, 
             summary(mod_quad)$adj.r.squared, 
             summary(mod_log)$adj.r.squared)
)

# Calculamos Delta AIC (Diferencia respecto al mejor)
min_aic <- min(metricas$AIC)
metricas$Delta_AIC <- metricas$AIC - min_aic

cat("\n>>> Tabla de Selección de Modelos:\n")
print(metricas %>% arrange(AIC))

# NOTA CRÍTICA:
# Si Delta_AIC > 10, el modelo perdedor prácticamente no tiene soporte empírico.
# Si Delta_AIC < 2, los modelos son casi equivalentes estadísticamente (usar criterio biológico).

# 4. Visualización Comparativa
# Predecimos valores para graficar las curvas suaves
grid_x <- seq(min(df_agro$x), max(df_agro$x), length.out = 100)
pred_lin <- predict(mod_lin, newdata = data.frame(x = grid_x))
pred_quad <- predict(mod_quad, newdata = data.frame(x = grid_x))
# Para log, evitamos log(0) si x empieza en 0
grid_x_log <- grid_x[grid_x > 0]
pred_log <- predict(mod_log, newdata = data.frame(x = grid_x_log))

# Base del gráfico
p_comp <- ggplot(df_agro, aes(x = x, y = y)) +
  geom_point(alpha = 0.5) +
  # Curva Lineal
  geom_line(data = data.frame(x = grid_x, y = pred_lin), 
            aes(x, y), color = "blue", size = 1, linetype = "dashed") +
  # Curva Cuadrática
  geom_line(data = data.frame(x = grid_x, y = pred_quad), 
            aes(x, y), color = "red", size = 1.2) +
  # Curva Logarítmica
  geom_line(data = data.frame(x = grid_x_log, y = pred_log), 
            aes(x, y), color = "green", size = 1) +
  labs(title = "Comparación Visual de Modelos",
       subtitle = "Rojo = Cuadrático | Azul = Lineal | Verde = Logarítmico",
       x = "Dosis de Nitrógeno (kg/ha)", y = "Rendimiento (q/ha)") +
  theme_bw()

print(p_comp)

# ==============================================================================
# PARTE IV: Cálculo del Óptimo Económico/Biológico (Solo si gana el Cuadrático)
# Si y = b0 + b1*x + b2*x^2 (y b2 es negativo), el máximo está en x = -b1 / (2*b2)
# ==============================================================================

if (metricas$Modelo[which.min(metricas$AIC)] == "Cuadrático") {
  coefs <- coef(mod_quad)
  b1 <- coefs["x"]
  b2 <- coefs["I(x^2)"]
  
  if(b2 < 0) {
    x_optimo <- -b1 / (2 * b2)
    cat("\n>>> CONCLUSIÓN AGRONÓMICA:\n")
    cat(sprintf("El modelo cuadrático sugiere una dosis óptima biológica de: %.2f kg N/ha\n", x_optimo))
    
    # Verificamos si el óptimo está dentro del rango estudiado (interpolación vs extrapolación)
    en_rango <- (x_optimo >= min(df_agro$x) & x_optimo <= max(df_agro$x))
    cat(sprintf("¿El óptimo cae dentro del rango de datos experimentales? %s\n", en_rango))
  }
}

cat("\n--- FIN DEL TALLER ---\n")