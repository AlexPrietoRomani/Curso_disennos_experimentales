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
library(pwr)
library(dunn.test)
library(rstatix)
library(ggfortify)
library(scatterplot3d)
library(GGally)
library(effects)
library(FielDHub)
library(augmentedRCBD)
library(systemfonts)

# Cargar módulos
mod_files <- list.files("R/modules", full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
mod_files <- sort(mod_files)
invisible(lapply(mod_files, source))

# Estructura de cursos, partes y sesiones
estructura_cursos <- list(
  "Diseños estadísticos V2" = list(
    "Parte I (Básica)" = list(
      sesiones = list(
        "Sesión 1" = list(module = "session1", id = "s1"),
        "Sesión 2" = list(module = "session2", id = "s2"),
        "Sesión 3" = list(module = "session3", id = "s3"),
        "Sesión 4" = list(module = "session4", id = "s4")
      )
    ),
    "Parte II (Intermedia)" = list(
      sesiones = list(
        "Sesión 5" = list(module = "session5", id = "s5"),
        "Sesión 6" = list(module = "session6", id = "s6"),
        "Sesión 7" = list(module = "session7", id = "s7"),
        "Sesión 8" = list(module = "session8", id = "s8"),
        "Sesión 9" = list(module = "session9", id = "s9")
      )
    )
  ),
  "Diseños estadísticos V3" = list(
    "Parte I (IA)" = list(
      sesiones = list(
        "Sesión 1" = list(module = "session1_v3", id = "v3_p1_s1")
      )
    ),
    "Parte II (Intermedia)" = list(
      sesiones = list(
        "Sesión 2" = list(module = "session2_v3", id = "v3_p2_s2"),
        "Sesión 3" = list(module = "session3_v3", id = "v3_p2_s3"),
        "Sesión 4" = list(module = "session4_v3", id = "v3_p2_s4")
      )
    ),
    "Parte III (Avanzada)" = list(
      sesiones = list(
        "Sesión 5" = list(module = "session5_v3", id = "v3_p3_s5"),
        "Sesión 6" = list(module = "session6_v3", id = "v3_p3_s6"),
        "Sesión 7" = list(module = "session7_v3", id = "v3_p3_s7"),
        "Sesión 8" = list(module = "session8_v3", id = "v3_p3_s8")
      )
    )
  )
)

# Registro de módulos con implementación disponible
module_registry <- list()
for (curso in names(estructura_cursos)) {
  partes <- estructura_cursos[[curso]]
  for (parte in names(partes)) {
    sesiones <- partes[[parte]]$sesiones
    for (sesion in names(sesiones)) {
      info <- sesiones[[sesion]]
      modulo <- info$module
      if (!is.null(modulo)) {
        if (is.null(module_registry[[modulo]])) {
          module_registry[[modulo]] <- info$id
        } else {
          module_registry[[modulo]] <- c(module_registry[[modulo]], info$id)
        }
      }
    }
  }
}

curso_predeterminado <- names(estructura_cursos)[1]
partes_predeterminadas <- names(estructura_cursos[[curso_predeterminado]])

obtener_info_sesion <- function(curso, parte, sesion) {
  estructura_cursos[[curso]][[parte]]$sesiones[[sesion]]
}
