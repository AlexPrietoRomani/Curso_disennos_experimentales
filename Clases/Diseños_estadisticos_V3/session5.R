# ==============================================================================
# TALLER PRÁCTICO: DISEÑOS DE PARCELAS DIVIDIDAS (SPLIT-PLOT) CON LMM
# Sesión 5 - Diseños Estadísticos Avanzados en Agricultura
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. CARGA DE LIBRERÍAS
# ------------------------------------------------------------------------------
# Instala las librerías si no las tienes: 
# install.packages(c("tidyverse", "lme4", "lmerTest",
#                    "emmeans", "multcomp", "multcompView"))

library(tidyverse)    # Manipulación de datos y gráficos (ggplot2, dplyr, etc.)
library(lme4)         # Modelos lineales mixtos (lmer) [Bates et al., 2015]
library(lmerTest)     # P-valores y ANOVA tipo III para lmer
library(emmeans)      # Medias marginales estimadas y comparaciones múltiples
library(multcomp)     # Herramientas de comparaciones múltiples (cld, etc.)
library(multcompView) # Construcción de letras de significancia

set.seed(123) # Fijamos semilla para reproducibilidad

# ------------------------------------------------------------------------------
# 2. SIMULACIÓN DEL ESCENARIO AGRÍCOLA (Split-Plot)
# ------------------------------------------------------------------------------
# CONTEXTO: Ensayo de Riego (Parcela Principal) x Nitrógeno (Subparcela)
# - Factor A (difícil de cambiar):     Riego (3 niveles)
# - Factor B (más fácil de cambiar):   Nitrógeno (4 niveles)
# - Bloques:                           4 repeticiones
#
# Objetivo didáctico:
#   - Generar un conjunto de datos "perfecto" (balanceado)  -> datos_completos
#   - Generar una versión "de campo" (pérdidas, desbalance) -> datos_sucios
#   - Generar dataset explícito con outliers                -> datos_eda
#
# Ver Yang (2010), Piepho (2003) y Stroup (2013) para la transición desde ANOVA
# clásico a modelos mixtos en experimentos agropecuarios y split-plot. 

generar_split_plot <- function(n_bloques = 4) {
  # 1. Estructura del diseño
  Bloque <- factor(paste0("B", 1:n_bloques))
  Riego  <- factor(c("R_Bajo", "R_Medio", "R_Alto"),
                   levels = c("R_Bajo", "R_Medio", "R_Alto"))
  Nitro  <- factor(c("N0", "N50", "N100", "N150"),
                   levels = c("N0", "N50", "N100", "N150"))
  
  # Diseño factorial completo Bloque × Riego (A) × Nitro (B)
  datos <- expand.grid(Bloque = Bloque, Riego = Riego, Nitro = Nitro)
  
  # 2. Efectos biológicos reales ("la verdad" del experimento)
  mu <- 5000 
  efecto_riego <- c(R_Bajo = -800, R_Medio = 200, R_Alto = 600)
  efecto_nitro <- c(N0 = -400, N50 = 100, N100 = 200, N150 = 100)
  
  # 3. Componentes de error (LA CLAVE DEL SPLIT-PLOT)
  #    - sd_parcela_A: variabilidad entre parcelas principales (Bloque:Riego)
  #    - sd_residual : variabilidad entre subparcelas (dentro de Bloque:Riego)
  sd_parcela_A <- 300  # Error grande (Parcela principal)
  sd_residual  <- 150  # Error pequeño (Subparcela)
  
  # Identificador de parcela principal (unidad experimental de A)
  datos$Parcela_Principal_ID <- interaction(datos$Bloque, datos$Riego)
  n_pp <- length(unique(datos$Parcela_Principal_ID))
  
  # Efectos aleatorios de parcela principal ~ N(0, sd_parcela_A^2)
  error_A_vals <- rnorm(n_pp, 0, sd_parcela_A)
  names(error_A_vals) <- unique(datos$Parcela_Principal_ID)
  
  # 4. Generar respuesta simulada
  datos <- datos %>%
    mutate(
      y_base = mu + efecto_riego[Riego] + efecto_nitro[Nitro],
      # Interacción biológica: R_Alto potencia N100 y N150
      y_int  = ifelse(Riego == "R_Alto" & Nitro %in% c("N100", "N150"), 400, 0),
      error_A = error_A_vals[as.character(Parcela_Principal_ID)],
      error_B = rnorm(n(), 0, sd_residual),
      rendimiento = y_base + y_int + error_A + error_B
    )
  
  return(datos)
}

# ----------------------------
# 2.1 Datos balanceados (completos)
# ----------------------------
datos_completos <- generar_split_plot()

# Resumen del BALANCE: 1 obs por celda Bloque×Riego×Nitro
resumen_balanceado <- datos_completos %>%
  count(Bloque, Riego, Nitro, name = "n_obs")

cat("=== Estructura balanceada (datos_completos) ===\n")
print(resumen_balanceado)

# ----------------------------
# 2.2 Datos "de campo": pérdidas y desbalance
# ----------------------------
# Simulamos pérdida del 20% de datos (plantas no cosechadas, etiquetas perdidas, etc.)
set.seed(999) 
datos_sucios <- datos_completos %>% sample_frac(0.80)

resumen_sucio <- datos_sucios %>%
  count(Bloque, Riego, Nitro, name = "n_obs")

cat("\n=== Estructura desbalanceada (datos_sucios) ===\n")
print(resumen_sucio)

# Tabla comparativa: qué celdas se perdieron al pasar de completos a sucios
estructura_completa <- datos_completos %>%
  count(Bloque, Riego, Nitro, name = "n_completo")

estructura_sucia <- datos_sucios %>%
  count(Bloque, Riego, Nitro, name = "n_sucio")

tabla_celdas <- estructura_completa %>%
  left_join(estructura_sucia, by = c("Bloque", "Riego", "Nitro")) %>%
  mutate(
    n_sucio = replace_na(n_sucio, 0L),
    estado  = if_else(n_sucio == 0L,
                      "PERDIDA (dato faltante en campo)",
                      "PRESENTE")
  ) %>%
  arrange(Bloque, Riego, Nitro)

cat("\n=== Comparación de celdas (balanceadas vs. desbalanceadas) ===\n")
print(tabla_celdas)

# ----------------------------
# 2.3 Dataset especial para EDA con outliers
# ----------------------------
# Copiamos datos_completos y forzamos dos errores extremos de digitación
datos_eda <- datos_completos
datos_eda$rendimiento[1] <- 12000 # Valor imposible / error de dedo hacia arriba
datos_eda$rendimiento[5] <- 50    # Valor muy bajo (planta prácticamente muerta)

# Detección numérica de outliers por Riego usando la regla de 1.5×IQR
limites_outlier <- datos_eda %>%
  group_by(Riego) %>%
  summarise(
    Q1  = quantile(rendimiento, 0.25),
    Q3  = quantile(rendimiento, 0.75),
    IQR = IQR(rendimiento),
    limite_inf = Q1 - 1.5 * IQR,
    limite_sup = Q3 + 1.5 * IQR,
    .groups = "drop"
  )

datos_eda_out <- datos_eda %>%
  left_join(limites_outlier, by = "Riego") %>%
  mutate(es_outlier = rendimiento < limite_inf | rendimiento > limite_sup)

glimpse(datos_eda_out)

datos_eda_out %>%
  dplyr::select(Bloque, Riego, Nitro, rendimiento, es_outlier)

# ------------------------------------------------------------------------------
# 3. ANÁLISIS EXPLORATORIO (EDA)
# ------------------------------------------------------------------------------
# Objetivo:
#   - Detectar outliers ANTES de ajustar el modelo.
#   - Ilustrar una variable porcentual para discutir arcoseno(√p).
#   - Visualizar la interacción Riego×Nitro como hipótesis previa.

# 3.1 Visualización de outliers (Boxplot + jitter)
#     - Eje X: Riego (factor A, parcela principal)
#     - Eje Y: rendimiento (kg/ha)
#     - Puntos rojos: candidatos a outlier según la regla de IQR dentro de cada Riego.
ggplot(datos_eda_out, aes(x = Riego, y = rendimiento)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = es_outlier),
              width = 0.2, alpha = 0.6, size = 2) +
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  labs(
    title    = "EDA: Detección de outliers en rendimiento",
    subtitle = "Puntos rojos: valores detectados como outliers por regla IQR dentro de cada Riego",
    x        = "Riego (parcela principal)",
    y        = "Rendimiento (kg/ha)",
    color    = "Outlier"
  ) +
  theme_bw()

# 3.2 Variable porcentual y transformación arcoseno(√p)
#     - Creamos una variable de ejemplo en [0,1] (proporción de daño, % plantas afectadas).
#     - Aplicamos la transformación arcoseno(√p), clásica para estabilizar varianzas,
#       pero hoy discutible frente a GLM/GLMM binomiales modernos.
set.seed(321)
datos_eda <- datos_eda %>%
  mutate(
    porcentaje_daño    = runif(n(), 0, 1),           # proporción en [0,1]
    porcentaje_daño_100 = porcentaje_daño * 100,     # misma info en %
    asin_transform     = asin(sqrt(porcentaje_daño)) # transformación arcoseno(√p)
  )

# Vista rápida de las tres variables
head(
  datos_eda %>%
    dplyr::select(rendimiento, porcentaje_daño, porcentaje_daño_100, asin_transform)
)

# Histograma antes de la transformación
ggplot(datos_eda, aes(x = porcentaje_daño)) +
  geom_histogram(bins = 15, color = "white") +
  labs(
    title = "Distribución de la proporción de daño (p)",
    x     = "Proporción de daño (0–1)",
    y     = "Frecuencia"
  ) +
  theme_bw()

# Histograma después de la transformación arcoseno(√p)
ggplot(datos_eda, aes(x = asin_transform)) +
  geom_histogram(bins = 15, color = "white") +
  labs(
    title    = "Distribución después de arcoseno(√p)",
    subtitle = "Se 'estiran' los extremos 0 y 1; ejemplo clásico de docencia.",
    x        = "arcsin(√p)",
    y        = "Frecuencia"
  ) +
  theme_bw()

# 3.3 Tendencia de interacción (usando datos LIMPIOS y balanceados)
#     - Cada punto es la media por combinación Riego×Nitro.
ggplot(datos_completos,
       aes(x = Nitro, y = rendimiento, group = Riego, color = Riego)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(
    title    = "Tendencia esperada (datos limpios y balanceados)",
    subtitle = "Líneas por nivel de Riego; anticipa la interacción Riego×Nitro",
    y        = "Rendimiento medio (kg/ha)",
    x        = "Dosis de Nitrógeno"
  ) +
  theme_bw()

# ------------------------------------------------------------------------------
# 4. EL MODELO "NAIVE" (INCORRECTO) vs. MODELO LMM (CORRECTO)
# ------------------------------------------------------------------------------
# Idea central:
#   - MODELO A (naive): lm/ANOVA que ignora la estructura split-plot y usa un solo error.
#   - MODELO B (LMM): lmer con efectos aleatorios de Bloque y Bloque:Riego (error A/B),
#                     ajustado por REML, robusto al desbalance.

cat("\n=== COMPARACIÓN DE MODELOS CON DATOS DESBALANCEADOS (N =",
    nrow(datos_sucios), ") ===\n")

# 4.1 Inspección del desbalance por Bloque×Riego×Nitro
cat("\n--- Conteo por celda en datos_sucios ---\n")
print(
  datos_sucios %>%
    count(Bloque, Riego, Nitro, name = "n_obs") %>%
    arrange(Bloque, Riego, Nitro)
)

# --- MODELO A: GLM simple (incorrecto) ---
# Usa sumas de cuadrados secuenciales (Tipo I) y un solo error residual.
m_incorrecto <- lm(rendimiento ~ Bloque + Riego * Nitro, data = datos_sucios)

cat("\n--- ANOVA MODELO INCORRECTO (GLM/LM) ---\n")
print(anova(m_incorrecto))
# Comentario:
#   - El mismo residuo se usa para probar Riego, Nitro y Riego×Nitro.
#   - Con desbalance, el F de Riego suele ser demasiado optimista (riesgo de falso positivo).

# --- MODELO B: LMM (correcto para Split-Plot) ---
#    (1 | Bloque)       -> efecto aleatorio de bloque
#    (1 | Bloque:Riego) -> parcelas principales (error A)
#    Residual           -> subparcelas (error B)
m_lmm <- lmer(rendimiento ~ Riego * Nitro + (1 | Bloque) + (1 | Bloque:Riego), 
              data = datos_sucios, REML = TRUE)

cat("\n--- ANOVA MODELO MIXTO (Correcto) ---\n")
print(anova(m_lmm))
# Observa los DenDF (grados de libertad denominador) de Riego:
# son bajos (≈ bloques × niveles de Riego) y el test es más conservador.

# El mismo modelo mixto con datos balanceados
m_lmm_balanceado <- lmer(rendimiento ~ Riego * Nitro + (1 | Bloque) + (1 | Bloque:Riego),
                         data = datos_completos, REML = TRUE)

cat("\n--- ANOVA MODELO MIXTO en datos COMPLETOS (referencia) ---\n")
print(anova(m_lmm_balanceado))

# ------------------------------------------------------------------------------
# 5. DIAGNÓSTICO E INTERPRETACIÓN DE VARIANZA
# ------------------------------------------------------------------------------
# Objetivo:
#   - Descomponer la variación en Bloque, parcela principal (Bloque:Riego) y Residual.
#   - Calcular el ICC de parcela principal.
#   - Tener una tabla % de varianza para discutir en clase.

cat("\n=== Componentes de varianza (modelo desbalanceado) ===\n")
print(VarCorr(m_lmm), comp = c("Variance", "Std.Dev."))

vars_desb <- as.data.frame(VarCorr(m_lmm))
sigma2_Bloque <- vars_desb$vcov[vars_desb$grp == "Bloque"]
sigma2_A      <- vars_desb$vcov[vars_desb$grp == "Bloque:Riego"]
sigma2_B      <- vars_desb$vcov[vars_desb$grp == "Residual"]
var_total     <- sigma2_Bloque + sigma2_A + sigma2_B

tabla_var_desb <- tibble(
  Fuente      = c("Bloque", "Parcela principal (Bloque:Riego)", "Residual"),
  Varianza    = c(sigma2_Bloque, sigma2_A, sigma2_B),
  Porcentaje  = 100 * c(sigma2_Bloque, sigma2_A, sigma2_B) / var_total
)

cat("\n=== Porcentaje de varianza explicada por cada componente ===\n")
print(tabla_var_desb)

# ICC (coeficiente de correlación intraclase) para la parcela principal
ICC <- sigma2_A / (sigma2_A + sigma2_B)
cat(sprintf("\nICC (Parcela Principal, modelo desbalanceado): %.3f\n", ICC))
# Interpretación típica: ICC > 0.1 indica correlación apreciable dentro de la misma
# parcela principal y refuerza el uso de un modelo mixto.

# Gráficos de residuos (validación de supuestos)
par(mfrow = c(1, 2))
plot(fitted(m_lmm), resid(m_lmm),
     main = "Homocedasticidad",
     xlab = "Valores predichos", ylab = "Residuos")
abline(h = 0, col = "red")
qqnorm(resid(m_lmm), main = "Normalidad de residuos")
qqline(resid(m_lmm), col = "red")
par(mfrow = c(1, 1))

# ------------------------------------------------------------------------------
# 6. COMPARACIONES MÚLTIPLES (POST-HOC) - EL "MONEY SHOT"
# ------------------------------------------------------------------------------
# Usamos el modelo mixto (m_lmm) y emmeans para:
#   - Obtener la tabla de medias Riego×Nitro ajustadas por el desbalance.
#   - Hacer comparaciones de Tukey de dosis de N dentro de cada Riego.

# 6.1 Medias marginales para la interacción Riego×Nitro
emms_interaccion <- emmeans(m_lmm, ~ Nitro | Riego)

cat("\n=== Medias estimadas (ajustadas por desbalance) ===\n")
print(emms_interaccion)

# 6.2 Contrastes simples (Tukey) de dosis de N dentro de cada Riego
pares <- pairs(emms_interaccion)
cat("\n=== Resumen de comparaciones Tukey de dosis de N dentro de cada Riego ===\n")
print(pares)

# ------------------------------------------------------------------------------
# 7. GRÁFICOS AVANZADOS DE RESULTADOS
#    (Visualización + significancia explícita)
# ------------------------------------------------------------------------------
# Aquí unificamos:
#   - Gráficos de medias e ICs.
#   - Letras de significancia (CLD).
#   - Mapa de p-valores (PWPP).
#   - Forest plot de diferencias.
# Meta: que el estudiante sepa QUÉ mirar en cada figura.

# 7.1 Gráfico 1: Comparaciones visuales con flechas (plot de emmeans)
#     - Cada línea roja es un IC 95% para una media de Nitro dentro de un Riego.
#     - Si dos flechas no se solapan, la diferencia suele ser significativa.
plot(emms_interaccion, comparisons = TRUE) +
  theme_bw() +
  labs(
    title    = "Comparación de Nitrógeno dentro de cada nivel de Riego",
    subtitle = "Flechas rojas: si no se solapan, p < 0.05 aproximadamente",
    x        = "Rendimiento estimado (kg/ha)",
    y        = "Dosis de Nitrógeno"
  )

# 7.2 Gráfico 2: Interacción con barras de error (formato de publicación)
#     - Eje X: dosis de N.
#     - Eje Y: medias ajustadas de rendimiento.
#     - Color: nivel de Riego.
#     - Barras: IC 95% del modelo mixto.
emms_df <- as.data.frame(emms_interaccion)

ggplot(emms_df, aes(x = Nitro, y = emmean, color = Riego, group = Riego)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, linewidth = 0.8) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(
    title    = "Efecto de Riego y Nitrógeno (medias ajustadas)",
    subtitle = "Barras: IC del 95% del modelo mixto (REML)",
    y        = "Rendimiento predicho (kg/ha)",
    x        = "Dosis de Nitrógeno"
  ) +
  theme_classic() +
  theme(legend.position = "top")

# 7.3 Gráfico 3: CLD (Compact Letter Display) – letras de significancia
#     - Letras distintas = medias significativamente diferentes (p < 0.05).
#     - Formato estándar en tablas de papers agronómicos.
cld_res <- cld(emms_interaccion, alpha = 0.05, Letters = letters, adjust = "tukey")
cld_res$.group <- trimws(cld_res$.group) # Limpiar espacios extra

ggplot(cld_res,
       aes(x = Nitro, y = emmean, color = Riego, group = Riego)) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                width = 0.2, linewidth = 0.8,
                position = position_dodge(0.1)) +
  geom_line(linewidth = 1, position = position_dodge(0.1)) +
  geom_point(size = 3, position = position_dodge(0.1)) +
  # Letras encima de las barras/segmentos
  geom_text(aes(label = .group, y = upper.CL + 200),
            vjust = 0, size = 5, fontface = "bold",
            position = position_dodge(0.1),
            show.legend = FALSE) +
  labs(
    title    = "Comparación de medias con letras (Tukey)",
    subtitle = "Letras distintas indican diferencias significativas (p < 0.05)",
    y        = "Rendimiento (kg/ha)",
    x        = "Dosis de Nitrógeno"
  ) +
  theme_classic() +
  scale_color_brewer(palette = "Dark2")

# 7.4 Gráfico 4: Mapa de p-valores (PWPP) – “quién gana a quién”
#     - Cada panel resume las comparaciones dentro de un Riego.
#     - Más a la derecha = p-valor más pequeño (mayor evidencia de diferencia).
pwpp(emms_interaccion) +
  theme_bw() +
  labs(
    title    = "Mapa de p-valores de todas las comparaciones",
    subtitle = "Escala visual de significancia (derecha = más significativo)",
    x        = "P-valor (escala transformada)",
    y        = "Dosis de Nitrógeno"
  )

# 7.5 Gráfico 5: Forest plot de diferencias (contrastes)
#     - Eje X: diferencia en rendimiento entre pares de dosis de N.
#     - Línea vertical en 0: “no diferencia”.
#     - Si la barra de IC cruza el 0 -> diferencia no significativa.
#     - Color rojo: contraste significativo al 5%.
contrastes <- confint(pairs(emms_interaccion)) %>% as.data.frame()
contrastes$Significativo <- ifelse(
  contrastes$lower.CL > 0 | contrastes$upper.CL < 0,
  "Sí (p<0.05)", "No (ns)"
)

ggplot(contrastes, aes(x = estimate, y = contrast)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower.CL, xmax = upper.CL, color = Significativo),
                 height = 0.2, linewidth = 1) +
  geom_point(size = 3, aes(color = Significativo)) +
  facet_grid(Riego ~ ., scales = "free_y") +
  labs(
    title    = "Diferencias estimadas entre dosis de N",
    subtitle = "Si la barra cruza la línea punteada (0), no hay diferencia significativa",
    x        = "Diferencia en rendimiento (kg/ha)",
    y        = "Comparación"
  ) +
  theme_bw() +
  scale_color_manual(values = c("No (ns)" = "gray", "Sí (p<0.05)" = "red"))
