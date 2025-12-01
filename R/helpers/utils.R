# R/helpers/utils.R

# Sanitize input
sanitize_credential_input <- function(value) {
  if (is.null(value)) return("")
  if (length(value) == 0L) return("")
  value <- value[[1L]]
  if (is.null(value) || is.na(value)) return("")
  value <- trimws(as.character(value))
  Encoding(value) <- "UTF-8"
  value
}

# Validate email format
is_email <- function(x) {
  if (is.null(x)) return(FALSE)
  x <- trimws(as.character(x))
  if (!nzchar(x)) return(FALSE)
  if (nchar(x) > 254) return(FALSE)
  grepl("^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$", x)
}

# Validate username format
is_valid_username <- function(username) {
  if (!nzchar(username)) return(FALSE)
  u <- sanitize_credential_input(username)
  # Allow email as username OR standard username
  is_email(u) || (
    nchar(u) <= 64 && grepl("^[A-Za-z0-9._-]+$", u)
  )
}

# Generate mailto link for support
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

# Sanitize ID for HTML elements
sanitize_id <- function(text) {
  id <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  if (is.na(id)) {
    id <- text
  }
  id <- tolower(id)
  id <- gsub("[^a-z0-9]+", "_", id)
  id <- gsub("(^_|_$)", "", id)
  id
}
