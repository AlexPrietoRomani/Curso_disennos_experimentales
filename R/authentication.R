# R/authentication.R
# -------------------------------------------------------------------
# Autenticación MongoDB robusta para Shiny
# - Lectura segura con mongolite::iterate() (evita listas en data.frame)
# - Hash de contraseñas con {sodium} / libsodium (Argon2id)
# - Pepper opcional vía AUTH_PASSWORD_PEPPER (.Renviron)
# - Soporta usuarios con username tipo email (case-insensitive via username_norm en DB)
# Referencias:
#   * sodium::password_store/password_verify: cadena ASCII con sal + parámetros. 
#     https://search.r-project.org/CRAN/refmans/sodium/html/password.html
#   * Libsodium crypto_pwhash_str (Argon2id, retrocompatibilidad): 
#     https://libsodium.gitbook.io/doc/password_hashing
#   * mongolite iterate() (sin simplificación a data.frame):
#     https://jeroen.github.io/mongolite/query-data.html
# -------------------------------------------------------------------

# ==============
# Utilitarios básicos
# ==============

sanitize_credential_input <- function(value) {
  if (is.null(value)) return("")
  value <- trimws(as.character(value))
  Encoding(value) <- "UTF-8"
  value
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ==============
# Validación y normalización de username (admite email)
# ==============

# Email pragmático estilo RFC (longitud total <= 254)
is_email <- function(x) {
  if (is.null(x)) return(FALSE)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(FALSE)
  if (nchar(x, type = "chars", allowNA = FALSE) > 254) return(FALSE)
  grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)
}

# Normaliza username: si es email -> lowercase (para matching con username_norm)
normalize_username <- function(x) {
  x <- sanitize_credential_input(x)
  if (is_email(x)) tolower(x) else x
}

# Acepta email o patrón clásico [A-Za-z0-9._-] (<=64)
is_valid_username <- function(username) {
  if (!nzchar(username)) return(FALSE)
  u <- sanitize_credential_input(username)
  is_email(u) || (
    nchar(u, type = "chars", allowNA = FALSE) <= 64 &&
      grepl("^[A-Za-z0-9._-]+$", u)
  )
}

# Política mínima (coincide con la de tu app; puedes elevarla si quieres)
is_valid_password <- function(password) {
  if (!nzchar(password)) return(FALSE)
  nchar(password, type = "chars", allowNA = FALSE) >= 8
}

# ==============
# Configuración Mongo (desde .Renviron)
# ==============

get_mongo_config <- function() {
  list(
    url        = Sys.getenv("MongoDB_url",        unset = ""),
    database   = Sys.getenv("MongoDB_basedatos",  unset = ""),
    collection = Sys.getenv("MongoDB_tabla",      unset = "")
  )
}

validate_mongo_config <- function(config) {
  all(vapply(config, nzchar, logical(1)))
}

create_mongo_connection <- function() {
  config <- get_mongo_config()
  if (!validate_mongo_config(config)) {
    stop("Variables de entorno de MongoDB incompletas. Verifica el archivo .Renviron.", call. = FALSE)
  }
  mongolite::mongo(
    collection = config$collection,
    db         = config$database,
    url        = config$url
  )
}

# ==============
# Pepper opcional (endurece hash si lo defines en .Renviron)
# ==============

# Si AUTH_PASSWORD_PEPPER está definido, se concatena al password.
# OJO: cambiar el pepper invalida verificaciones de hashes creados con el pepper anterior.
get_password_pepper <- function() {
  p <- Sys.getenv("AUTH_PASSWORD_PEPPER", unset = "")
  if (!is.character(p) || !nzchar(p)) return("")
  p
}

# ==============
# Hash y verificación con {sodium} (libsodium)
# ==============

# Devuelve string ASCII con sal y parámetros embebidos (libsodium crypto_pwhash_str)
hash_password <- function(password) {
  password <- sanitize_credential_input(password)
  if (!is_valid_password(password)) {
    stop("La contraseña debe tener al menos 8 caracteres.", call. = FALSE)
  }
  pepper <- get_password_pepper()
  sodium::password_store(paste0(password, pepper))
}

# Verifica de forma robusta:
# 1) intenta con pepper (si existe), 2) cae a sin pepper (compatibilidad hacia atrás)
verify_password <- function(password, hash) {
  norm1 <- function(x) {
    if (is.null(x)) return("")
    if (is.list(x)) x <- x[[1L]]
    x <- as.character(x)
    if (!length(x)) return("")
    trimws(x[[1L]])
  }
  pw <- norm1(password)
  hs <- norm1(hash)
  if (!nzchar(pw) || !nzchar(hs)) return(FALSE)

  pepper <- get_password_pepper()
  # Orden correcto: password_verify(hash, password)
  ok <- sodium::password_verify(hs, paste0(pw, pepper))
  if (!ok) {
    # Compatibilidad con cuentas anteriores (sin pepper)
    ok <- sodium::password_verify(hs, pw)
  }
  ok
}

# ==============
# Lectura robusta del usuario desde Mongo
# ==============

# Extrae hash como character(1) o devuelve NULL
extract_password_hash <- function(user_doc) {
  for (nm in c("password_hash", "password")) {
    if (nm %in% names(user_doc)) {
      h <- user_doc[[nm]]
      if (is.null(h)) next
      if (is.list(h)) h <- h[[1L]]
      h <- as.character(h)
      if (length(h) >= 1L && !is.na(h[1L]) && nzchar(h[1L])) {
        return(h[1L])
      }
    }
  }
  NULL
}

# Uso de iterate(): evita simplificación a data.frame (que generaba List of 1)
fetch_user_by_username <- function(username) {
  username <- sanitize_credential_input(username)
  if (!is_valid_username(username)) return(NULL)

  collection <- create_mongo_connection()
  on.exit(collection$disconnect(), add = TRUE)

  q <- jsonlite::toJSON(list(
    "$or" = list(
      list(username_norm = normalize_username(username)),
      list(username      = username)
    )
  ), auto_unbox = TRUE)

  fields <- '{"_id":0,"username":1,"username_norm":1,"password_hash":1,"password":1,"roles":1,"status":1}'

  it  <- collection$iterate(query = q, fields = fields)
  doc <- it$one()
  if (is.null(doc)) return(NULL)

  coerce1 <- function(x) {
    if (is.null(x)) return(NULL)
    if (is.list(x)) x <- x[[1L]]
    x <- as.character(x)
    if (length(x)) x[[1L]] else NULL
  }
  doc$username      <- coerce1(doc$username)
  doc$username_norm <- coerce1(doc$username_norm)
  doc$password_hash <- coerce1(doc$password_hash)
  doc$password      <- coerce1(doc$password)
  doc
}

# ==============
# Flujo de autenticación
# ==============

authenticate_user <- function(username, password) {
  username <- sanitize_credential_input(username)
  password <- sanitize_credential_input(password)

  if (!is_valid_username(username)) {
    return(list(success = FALSE, message = "Usuario inválido. Usa solo letras, números o ._-"))
  }
  if (!is_valid_password(password)) {
    return(list(success = FALSE, message = "La contraseña debe tener al menos 8 caracteres."))
  }

  tryCatch({
    user <- fetch_user_by_username(username)
    if (is.null(user)) {
      return(list(success = FALSE, message = "Credenciales incorrectas."))
    }

    password_hash <- extract_password_hash(user)
    if (is.null(password_hash)) {
      return(list(success = FALSE, message = "La cuenta no tiene una contraseña válida configurada."))
    }

    if (!verify_password(password, password_hash)) {
      return(list(success = FALSE, message = "Credenciales incorrectas."))
    }

    # Limpia credenciales del objeto usuario antes de retornarlo
    user$password_hash <- NULL
    user$password      <- NULL
    list(success = TRUE, message = "Autenticación exitosa.", user = user)
  }, error = function(e) {
    list(success = FALSE, message = "No fue posible conectar con el servicio de autenticación. Inténtalo más tarde.")
  })
}

# ==============
# Utilitario soporte (opcional)
# ==============

support_mailto_link <- function(subject = "Soporte cursos", body = NULL) {
  email  <- "alexprieto1997@gmail.com"
  params <- list(subject = subject)
  if (!is.null(body) && nzchar(body)) params$body <- body
  query <- paste(
    vapply(names(params), function(name) {
      paste0(name, "=", utils::URLencode(as.character(params[[name]]), reserved = TRUE))
    }, character(1)),
    collapse = "&"
  )
  sprintf("mailto:%s?%s", email, query)
}
