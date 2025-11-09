# R/utils.R
# Funciones utilitarias para autenticación segura con MongoDB

sanitize_credential_input <- function(value) {
  if (is.null(value)) {
    return("")
  }
  value <- trimws(as.character(value))
  Encoding(value) <- "UTF-8"
  value
}

is_valid_username <- function(username) {
  if (!nzchar(username)) {
    return(FALSE)
  }
  nchar(username, type = "chars", allowNA = FALSE) <= 64 &&
    grepl("^[A-Za-z0-9._-]+$", username)
}

is_valid_password <- function(password) {
  if (!nzchar(password)) {
    return(FALSE)
  }
  nchar(password, type = "chars", allowNA = FALSE) >= 8
}

get_mongo_config <- function() {
  list(
    url = Sys.getenv("MongoDB_url", unset = ""),
    database = Sys.getenv("MongoDB_basedatos", unset = ""),
    collection = Sys.getenv("MongoDB_tabla", unset = "")
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
    db = config$database,
    url = config$url
  )
}

hash_password <- function(password) {
  password <- sanitize_credential_input(password)
  if (!is_valid_password(password)) {
    stop("La contraseña debe tener al menos 8 caracteres.", call. = FALSE)
  }
  sodium::password_store(password)
}

verify_password <- function(password, hash) {
  if (!nzchar(password) || !nzchar(hash)) {
    return(FALSE)
  }
  sodium::password_verify(hash, password)
}

fetch_user_by_username <- function(username) {
  username <- sanitize_credential_input(username)
  if (!is_valid_username(username)) {
    return(NULL)
  }

  collection <- create_mongo_connection()
  on.exit(collection$disconnect(), add = TRUE)

  query <- jsonlite::toJSON(list(username = username), auto_unbox = TRUE)
  result <- collection$find(query = query, limit = 1)

  if (NROW(result) == 0) {
    return(NULL)
  }

  user <- as.list(result[1, , drop = FALSE])
  user
}

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

    password_hash <- user$password_hash %||% user$password
    if (is.null(password_hash) || !is.character(password_hash) || !nzchar(password_hash)) {
      return(list(success = FALSE, message = "La cuenta no tiene una contraseña válida configurada."))
    }

    if (!verify_password(password, password_hash)) {
      return(list(success = FALSE, message = "Credenciales incorrectas."))
    }

    user$password_hash <- NULL
    user$password <- NULL

    list(success = TRUE, message = "Autenticación exitosa.", user = user)
  }, error = function(e) {
    list(success = FALSE, message = "No fue posible conectar con el servicio de autenticación. Inténtalo más tarde.")
  })
}

support_mailto_link <- function(subject = "Soporte cursos", body = NULL) {
  email <- "alexprieto1997@gmail.com"
  params <- list(subject = subject)
  if (!is.null(body) && nzchar(body)) {
    params$body <- body
  }
  query <- paste(
    vapply(names(params), function(name) {
      paste0(name, "=", utils::URLencode(as.character(params[[name]]), reserved = TRUE))
    }, character(1)),
    collapse = "&"
  )
  sprintf("mailto:%s?%s", email, query)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
