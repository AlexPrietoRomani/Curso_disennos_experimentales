# R/global.R
# -------------------------------------------------------------------------
# Configuración global para la aplicación Shiny:
#  - Gestión de paquetes (instalación automática en shinyapps.io si faltan)
#  - Registro diferido de módulos en R/modules
#  - Estructura de cursos consumida por la UI
# -------------------------------------------------------------------------

# 0) Variables de entorno del proyecto (.Renviron local si existe)
project_renviron <- file.path(getwd(), ".Renviron")
if (file.exists(project_renviron)) {
  readRenviron(project_renviron)
}

# 1) Autenticación
# 1) Autenticación y Helpers
# source("R/authentication.R") # Deprecated
source("R/helpers/auth.R")
source("R/helpers/db.R")
source("R/helpers/email.R")
source("R/helpers/utils.R")

# 1.1) Módulos de autenticación
source("R/modules/auth/register.R")
source("R/modules/auth/password_reset.R")
source("R/modules/auth/login_ui.R")

# 1.2) Módulos de UI (Landing, Cursos, Shared)

source("R/modules/courses/courses_ui.R")
source("R/modules/shared/navbar.R")

# 2) Paquetes: instala automáticamente los críticos si no están
auto_install_pkgs <- c("mongolite", "sodium", "openssl", "emayili", "shinyjs")

ensure_default_repos <- function() {
  repos <- getOption("repos"); if (is.null(repos)) repos <- character()
  repos_names <- names(repos)
  cran_idx <- which(repos_names %in% c("CRAN", "@CRAN@"))
  needs_default <-
    length(repos) == 0L ||
    (length(cran_idx) > 0L && (repos[cran_idx[1]] %in% c("", "@CRAN@"))) ||
    all(repos == "")
  if (needs_default) {
    repos <- c(CRAN = "https://cloud.r-project.org")
    options(repos = repos)
  }
  repos
}

ensure_user_lib <- function() {
  user_lib <- Sys.getenv("R_LIBS_USER")
  if (!nzchar(user_lib)) {
    user_lib <- file.path("~", "R",
      paste0(R.version$platform, "-library"),
      paste0(R.version$major, ".", R.version$minor))
  }
  user_lib <- path.expand(user_lib)
  if (!dir.exists(user_lib)) dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
  if (!(user_lib %in% .libPaths())) .libPaths(c(user_lib, .libPaths()))
  user_lib
}

need_pkg <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    if (pkg %in% auto_install_pkgs) {
      repos <- ensure_default_repos()
      lib_path <- ensure_user_lib()
      message(sprintf("Instalando paquete faltante '%s' desde CRAN...", pkg))
      tryCatch(
        utils::install.packages(pkg, repos = repos, lib = lib_path),
        error = function(e) {
          stop(sprintf("No se pudo instalar automáticamente el paquete '%s': %s",
                       pkg, conditionMessage(e)), call. = FALSE)
        }
      )
    }
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("El paquete '%s' es requerido para esta sesión. Instálalo primero.", pkg), call. = FALSE)
    }
  }
}

safe_library <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Paquete opcional no disponible: %s (la app seguirá sin este módulo).", pkg))
    return(FALSE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  TRUE
}

load_packages <- function(pkgs) {
  pkgs <- unique(pkgs)
  invisible(lapply(pkgs, function(pkg) {
    need_pkg(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }))
}

# Polyfill de callout si tu bslib no lo tiene
bs_callout <- function(..., title = NULL, type = c("info","warning","danger","success")) {
  type <- match.arg(type)
  if (requireNamespace("bslib", quietly = TRUE) && "callout" %in% getNamespaceExports("bslib")) {
    return(bslib::callout(title = title, ...))
  }
  div(class = paste0("alert alert-", if (type == "danger") "danger" else type),
      if (!is.null(title)) tags$h5(title), ...)
}

package_groups <- list(
  shiny = c("shiny", "bslib", "bsicons", "shinythemes"),
  data = c("tidyverse", "readxl", "janitor", "broom", "moments", "systemfonts", "jsonlite"),
  modelling = c("agricolae", "MASS", "emmeans", "effectsize", "pwr", "dunn.test", "rstatix", "lme4", "lmerTest"),
  visualization = c("ggplot2", "plotly", "patchwork", "GGally", "ggfortify", "scatterplot3d", "effects", "RColorBrewer", "grid"),
  interface = c("DT", "car"),
  auth = c("mongolite", "sodium") # sodium cargado para verificación en runtime
)

load_packages(unlist(package_groups))

optional_packages <- c("FielDHub", "augmentedRCBD", "broom.mixed")
invisible(lapply(optional_packages, safe_library))

# Registro diferido de módulos (R/modules)
mod_files <- sort(list.files("R/modules", full.names = TRUE, pattern = "\\.R$", recursive = TRUE))
module_sources <- new.env(parent = emptyenv())

register_module_source <- function(module_name, file) {
  if (!nzchar(module_name)) return()
  if (!exists(module_name, envir = module_sources, inherits = FALSE)) {
    assign(module_name, file, envir = module_sources)
  } else {
    # Si ya existe, agregamos el archivo a la lista si no está
    current <- get(module_name, envir = module_sources)
    if (!file %in% current) {
      assign(module_name, c(current, file), envir = module_sources)
    }
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
        module_names <- unique(c(module_names, sub("(UI|Server)$", "", fname)))
      }
    }
  }
  module_names
}

for (file in mod_files) {
  modules_in_file <- extract_module_names(file)
  for (module_name in modules_in_file) register_module_source(module_name, file)
}

ensure_module_loaded <- function(module_name) {
  if (!nzchar(module_name)) return(FALSE)
  ui_fun_name <- paste0(module_name, "UI")
  server_fun_name <- paste0(module_name, "Server")
  
  ui_loaded <- exists(ui_fun_name, mode = "function", inherits = TRUE)
  server_loaded <- exists(server_fun_name, mode = "function", inherits = TRUE)
  
  # Si ya están ambas cargadas, retornamos TRUE
  if (ui_loaded && server_loaded) return(TRUE)
  
  if (!exists(module_name, envir = module_sources, inherits = FALSE)) {
    warning(sprintf("No se encontró el archivo fuente para el módulo '%s'.", module_name), call. = FALSE)
    return(FALSE)
  }
  
  # Cargar todos los archivos asociados al módulo
  files <- get(module_name, envir = module_sources)
  for (f in files) {
    source(f, local = FALSE, encoding = "UTF-8")
  }
  
  # Verificamos de nuevo
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
    ),
    "Parte IV (Extras)" = list(
      sesiones = list(
        "Sesión 9" = list(module = "session9_v3", id = "v3_p4_s9"),
        "Sesión 10" = list(module = "session10_v3", id = "v3_p4_s10"),
        "Sesión 11" = list(module = "session11_v3", id = "v3_p4_s11")
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
        if (is.null(module_registry[[modulo]])) module_registry[[modulo]] <- info$id
        else module_registry[[modulo]] <- c(module_registry[[modulo]], info$id)
      }
    }
  }
}

curso_predeterminado <- names(estructura_cursos)[1]
partes_predeterminadas <- names(estructura_cursos[[curso_predeterminado]])

obtener_info_sesion <- function(curso, parte, sesion) {
  estructura_cursos[[curso]][[parte]]$sesiones[[sesion]]
}

get_practice_files <- function(course_name, session_name) {
  # Map course name to folder name
  # "Diseños estadísticos V3" -> "Diseños_estadisticos_V3"
  folder_name <- gsub(" ", "_", course_name)
  folder_name <- gsub("estadísticos", "estadisticos", folder_name)
  
  base_path <- file.path("Clases", folder_name)
  
  if (!dir.exists(base_path)) return(character(0))
  
  # Extract session number (e.g. "Sesión 6" -> "6")
  # Use regex to capture the number
  num <- sub("Sesión\\s+(\\d+).*", "\\1", session_name, ignore.case = TRUE)
  
  # Pattern: session6.R or session6-something.R
  # ^session{num}(\\-.+)?\\.R$
  pattern <- sprintf("^session%s(-.+)?\\.R$", num)
  
  files <- list.files(base_path, pattern = pattern, full.names = FALSE)
  
  # Return full paths? Or just names?
  # Return list with name and full path for easy usage
  if (length(files) > 0) {
    return(data.frame(
      name = files,
      path = file.path(base_path, files),
      stringsAsFactors = FALSE
    ))
  } else {
    return(NULL)
  }
}
