# R/global.R

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("El paquete '%s' es requerido para esta sesión. Instálalo primero.", pkg), call. = FALSE)
  }
}

safe_library <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Paquete opcional no disponible: %s (la app seguirá sin este módulo).", pkg))
    return(FALSE)
  }
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
  TRUE
}

load_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  invisible(lapply(pkgs, function(pkg) {
    need_pkg(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

# polyfill de callout si tu bslib no lo tiene
bs_callout <- function(..., title = NULL, type = c("info","warning","danger","success")) {
  type <- match.arg(type)
  # Usa callout si existe; si no, degrade a alert
  if (requireNamespace("bslib", quietly = TRUE) && "callout" %in% getNamespaceExports("bslib")) {
    return(bslib::callout(title = title, ...))
  }
  # Bootstrap alert compatible
  div(class = paste0("alert alert-", if (type == "danger") "danger" else type),
      if (!is.null(title)) tags$h5(title),
      ...)
}

package_groups <- list(
  shiny = c("shiny", "bslib", "shinythemes"),
  data = c("tidyverse", "readxl", "janitor", "broom", "moments", "systemfonts"),
  modelling = c("agricolae", "MASS", "emmeans", "effectsize", "pwr", "dunn.test", "rstatix", "lme4", "lmerTest"),
  visualization = c("ggplot2", "plotly", "patchwork", "GGally", "ggfortify", "scatterplot3d", "effects", "RColorBrewer", "grid"),
  interface = c("DT", "car")
)

load_packages(unlist(package_groups))

optional_packages <- c("FielDHub", "augmentedRCBD", "broom.mixed")
invisible(lapply(optional_packages, safe_library))

# Registro de archivos de módulos para carga diferida
mod_files <- list.files("R/modules", full.names = TRUE, pattern = "\\.R$", recursive = TRUE)
mod_files <- sort(mod_files)

module_sources <- new.env(parent = emptyenv())

register_module_source <- function(module_name, file) {
  if (!nzchar(module_name)) {
    return()
  }

  if (!exists(module_name, envir = module_sources, inherits = FALSE)) {
    assign(module_name, file, envir = module_sources)
  }
}

extract_module_names <- function(file) {
  lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
  module_names <- character()

  for (line in lines) {
    match <- regexec("^\\s*([a-zA-Z0-9_]+)\\s*<-\\s*function", line, perl = TRUE)
    capture <- regmatches(line, match)
    if (length(capture) > 0 && length(capture[[1]]) > 1) {
      fname <- capture[[1]][2]
      if (grepl("(UI|Server)$", fname)) {
        module_name <- sub("(UI|Server)$", "", fname)
        module_names <- unique(c(module_names, module_name))
      }
    }
  }

  module_names
}

for (file in mod_files) {
  modules_in_file <- extract_module_names(file)
  for (module_name in modules_in_file) {
    register_module_source(module_name, file)
  }
}

ensure_module_loaded <- function(module_name) {
  if (!nzchar(module_name)) {
    return(FALSE)
  }

  ui_fun_name <- paste0(module_name, "UI")
  server_fun_name <- paste0(module_name, "Server")

  ui_loaded <- exists(ui_fun_name, mode = "function", inherits = TRUE)
  server_loaded <- exists(server_fun_name, mode = "function", inherits = TRUE)

  if (ui_loaded || server_loaded) {
    return(TRUE)
  }

  if (!exists(module_name, envir = module_sources, inherits = FALSE)) {
    warning(sprintf("No se encontró el archivo fuente para el módulo '%s'.", module_name), call. = FALSE)
    return(FALSE)
  }

  source(get(module_name, envir = module_sources), local = FALSE, encoding = "UTF-8")

  exists(ui_fun_name, mode = "function", inherits = TRUE) ||
    exists(server_fun_name, mode = "function", inherits = TRUE)
}

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
