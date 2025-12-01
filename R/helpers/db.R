# R/helpers/db.R

# --- MongoDB Connection & User Management ---

get_mongo_connection <- function() {
  url <- Sys.getenv("MongoDB_url")
  db <- Sys.getenv("MongoDB_basedatos")
  collection <- Sys.getenv("MongoDB_tabla")
  
  if (url == "" || db == "" || collection == "") {
    stop("MongoDB configuration missing in .Renviron")
  }
  
  mongolite::mongo(collection = collection, db = db, url = url)
}

# Check if username/email exists
user_exists <- function(username) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  # Case insensitive search for username or email
  count <- con$count(
    query = jsonlite::toJSON(list(
      "$or" = list(
        list(username = username),
        list(email = username) # Assuming username can be email
      )
    ), auto_unbox = TRUE)
  )
  return(count > 0)
}

# Create new user
create_user <- function(username, email, password, first_name, last_name) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  if (user_exists(username) || user_exists(email)) {
    return(list(success = FALSE, message = "El usuario o correo ya existe."))
  }
  
  hashed_pw <- hash_password(password)
  
  user_doc <- data.frame(
    username = username,
    username_norm = tolower(username), # Add normalized username for compatibility
    email = email,
    first_name = first_name,
    last_name = last_name,
    password_hash = hashed_pw,
    status = "pending", # Default status
    created_at = Sys.time(),
    roles = I(list(c("user"))), # Default role
    stringsAsFactors = FALSE
  )
  
  con$insert(user_doc)
  return(list(success = TRUE, message = "Usuario creado exitosamente."))
}

# Get user by username or email
get_user <- function(username) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  user <- con$find(
    query = jsonlite::toJSON(list(
      "$or" = list(
        list(username = username),
        list(username_norm = tolower(username)), # Check normalized username
        list(email = username)
      )
    ), auto_unbox = TRUE),
    limit = 1
  )
  
  if (nrow(user) == 0) return(NULL)
  return(user)
}

# Update user status (for admin approval)
update_user_status <- function(username, status) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  con$update(
    query = jsonlite::toJSON(list(username = username), auto_unbox = TRUE),
    update = jsonlite::toJSON(list("$set" = list(status = status)), auto_unbox = TRUE)
  )
}

# Save password reset token
save_reset_token <- function(email, token) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  # Set token and expiration (e.g., 1 hour)
  expiration <- Sys.time() + 3600 
  
  con$update(
    query = jsonlite::toJSON(list(email = email), auto_unbox = TRUE),
    update = jsonlite::toJSON(list(
      "$set" = list(
        reset_token = token,
        reset_expires = expiration
      )
    ), auto_unbox = TRUE)
  )
}

# Verify reset token
verify_reset_token <- function(email, token) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  user <- con$find(
    query = jsonlite::toJSON(list(
      email = email,
      reset_token = token
    ), auto_unbox = TRUE),
    limit = 1
  )
  
  if (nrow(user) == 0) return(FALSE)
  
  # Check expiration
  # Note: MongoDB returns POSIXct, ensure compatibility
  if (user$reset_expires < Sys.time()) return(FALSE)
  
  return(TRUE)
}

# Update password
update_password <- function(email, new_password) {
  con <- get_mongo_connection()
  on.exit(try(con$disconnect(), silent = TRUE))
  
  hashed_pw <- hash_password(new_password)
  
  con$update(
    query = jsonlite::toJSON(list(email = email), auto_unbox = TRUE),
    update = jsonlite::toJSON(list(
      "$set" = list(
        password_hash = hashed_pw,
        reset_token = NULL, # Clear token
        reset_expires = NULL
      )
    ), auto_unbox = TRUE)
  )
}

# Authenticate user
authenticate_user <- function(username, password) {
  user <- get_user(username)
  
  if (is.null(user)) {
    return(list(success = FALSE, message = "Usuario o contraseña incorrectos."))
  }
  
  # Check password
  # Note: verify_password is in R/helpers/auth.R
  if (!verify_password(password, user$password_hash)) {
    return(list(success = FALSE, message = "Usuario o contraseña incorrectos."))
  }
  
  # Check status
  if (user$status != "active") {
    return(list(success = FALSE, message = "Tu cuenta aún no ha sido aprobada por el administrador."))
  }
  
  return(list(success = TRUE, user = user))
}
