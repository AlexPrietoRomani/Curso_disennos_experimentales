# Ver la ruta actual del entorno de trabajo
getwd()

# Cambiar la ruta del entorno de trabajo
setwd("C:/Users/ALEX/OneDrive/Asesorias/Escuela CEDEPA/2da capacitación/Curso_disennos_experimentales")

# Vector con todas las librerías requeridas
pkgs <- c(
  "shiny", "tidyverse", "agricolae", "readxl", "janitor",
  "moments", "plotly", "bslib", "ggplot2", "patchwork",
  "grid", "car", "dplyr", "MASS", "effectsize", "broom",
  "emmeans", "DT", "pwr", "dunn.test", "rstatix",
  "ggfortify", "scatterplot3d", "GGally", "effects"
)

# Instalar sólo las que no estén instaladas
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
