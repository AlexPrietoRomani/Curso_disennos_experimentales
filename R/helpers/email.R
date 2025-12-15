# R/helpers/email.R

# --- Email Sending Helpers (using emayili/blastula) ---

# Helper to get SMTP config
get_smtp_server <- function() {
  # Requires .Renviron variables:
  # SMTP_SERVER, SMTP_PORT, SMTP_USER, SMTP_PASSWORD
  
  if (!requireNamespace("emayili", quietly = TRUE)) {
    stop("Package 'emayili' is required for sending emails.")
  }
  
  emayili::server(
    host = Sys.getenv("SMTP_SERVER"),
    port = as.numeric(Sys.getenv("SMTP_PORT")),
    username = Sys.getenv("SMTP_USER"),
    password = Sys.getenv("SMTP_PASSWORD")
  )
}



# Send welcome email to user (after approval)
send_welcome_email <- function(user_email, username) {
  smtp <- get_smtp_server()
  
  # Ensure inputs are character vectors
  user_email <- as.character(user_email)
  username <- as.character(username)
  
  message(">>> Sending welcome email to: ", user_email)
  
  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(user_email) |>
    emayili::subject("Bienvenido a la plataforma") |>
    emayili::text(paste0(
      "Hola ", username, ",\n\n",
      "¡Bienvenido a la plataforma de Alex Prieto Romani!\n\n",
      "Tu cuenta ha sido activada y ya puedes iniciar sesión.\n\n",
      "Saludos,\nAlex Prieto Romani"
    ))
  
  smtp(email, verbose = TRUE)
}

# Send password reset code (OTP)
send_password_reset_code <- function(user_email, code) {
  smtp <- get_smtp_server()
  
  user_email <- as.character(user_email)
  code <- as.character(code)
  
  message(">>> Sending password reset code to: ", user_email, " Code: ", code)
  
  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(user_email) |>
    emayili::subject("Código de recuperación de contraseña") |>
    emayili::text(paste0(
      "Hola,\n\n",
      "Tu código de verificación para restablecer la contraseña es:\n\n",
      "   ", code, "\n\n",
      "Este código expirará en 15 minutos.\n",
      "Si no solicitaste esto, ignora este correo.\n\n",
      "Saludos,\nAlex Prieto Romani"
    ))
  
  smtp(email, verbose = TRUE)
}
