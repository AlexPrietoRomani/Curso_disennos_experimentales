# R/helpers/auth.R

# --- Password Hashing & Verification (using sodium/openssl) ---

# Validate password complexity
# Must contain: 1 uppercase, 1 lowercase, 1 special char, min 8 chars
validate_password_complexity <- function(password) {
  if (nchar(password) < 8) {
    return(list(valid = FALSE, message = "La contraseña debe tener al menos 8 caracteres."))
  }
  if (!grepl("[A-Z]", password)) {
    return(list(valid = FALSE, message = "La contraseña debe contener al menos una letra mayúscula."))
  }
  if (!grepl("[a-z]", password)) {
    return(list(valid = FALSE, message = "La contraseña debe contener al menos una letra minúscula."))
  }
  if (!grepl("[^A-Za-z0-9]", password)) {
    return(list(valid = FALSE, message = "La contraseña debe contener al menos un carácter especial (ej. !@#$%)."))
  }
  return(list(valid = TRUE, message = ""))
}

# Hash password using sodium (preferred) or pbkdf2 fallback
hash_password <- function(password) {
  # Use a pepper if available in environment
  pepper <- Sys.getenv("AUTH_PASSWORD_PEPPER", unset = "")
  
  if (requireNamespace("sodium", quietly = TRUE)) {
    return(sodium::password_store(paste0(password, pepper)))
  } else {
    stop("Package 'sodium' is required for password hashing.")
  }
}

# Verify password
verify_password <- function(password, hash) {
  if (is.null(password) || is.null(hash) || is.na(password) || is.na(hash)) return(FALSE)
  
  pepper <- Sys.getenv("AUTH_PASSWORD_PEPPER", unset = "")
  
  # Try sodium verify
  if (requireNamespace("sodium", quietly = TRUE)) {
    # Try with pepper
    valid <- tryCatch(sodium::password_verify(hash, paste0(password, pepper)), error = function(e) FALSE)
    if (valid) return(TRUE)
    
    # Try without pepper (backward compatibility)
    valid_no_pepper <- tryCatch(sodium::password_verify(hash, password), error = function(e) FALSE)
    if (valid_no_pepper) return(TRUE)
  }
  
  return(FALSE)
}

# Generate a random token for password reset / email verification
generate_token <- function(n = 32) {
  if (requireNamespace("openssl", quietly = TRUE)) {
    return(openssl::rand_bytes(n) |> openssl::base64_encode())
  } else {
    # Fallback if openssl not present (though it should be for shiny apps usually)
    paste0(sample(c(letters, LETTERS, 0:9), n, replace = TRUE), collapse = "")
  }
}
