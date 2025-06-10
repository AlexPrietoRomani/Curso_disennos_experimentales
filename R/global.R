# R/global.R
library(shiny)
library(tidyverse)
library(agricolae)
library(readxl)
library(janitor)
library(moments)
library(plotly)
library(bslib)
library(ggplot2)
library(patchwork)
library(grid)
library(car)
library(dplyr)
library(MASS)
library(effectsize)
library(broom)
library(emmeans)
library(DT)

# Cargar módulos
mod_files <- list.files("R/modules", full.names = TRUE, pattern = "\\.R$")
lapply(mod_files, source)

# Mapa de partes a sesiones
sesiones <- list(
  "Parte I (Básica)"      = paste0("Sesión ", 1:4),
  "Parte II (Intermedia)" = paste0("Sesión ", 5:8)
)