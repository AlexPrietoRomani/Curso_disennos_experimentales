# Ver la ruta actual del entorno de trabajo
getwd()

# Cambiar la ruta del entorno de trabajo
setwd("C:/Users/ALEX/OneDrive/Asesorias/Escuela CEDEPA/2da capacitación/Curso_disennos_experimentales")

# Vector con todas las librerías requeridas
pkgs <- c(
  "shiny", "bslib", "shinythemes",
  "tidyverse", "readxl", "janitor", "broom", "moments", "systemfonts",
  "agricolae", "MASS", "emmeans", "effectsize", "pwr", "dunn.test", "rstatix", "lme4", "lmerTest",
  "ggplot2", "plotly", "patchwork", "GGally", "ggfortify", "scatterplot3d", "effects", "RColorBrewer", "grid",
  "DT", "car",
  "FielDHub", "augmentedRCBD", "broom.mixed"
)

# Instalar sólo las que no estén instaladas
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[,"Package"])]
if(length(new_pkgs)) install.packages(new_pkgs, dependencies = TRUE)
