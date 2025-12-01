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

# Send approval request to admin
send_approval_request_email <- function(new_user_data, approval_link) {
  smtp <- get_smtp_server()
  admin_email <- "alexprieto1997@gmail.com"
  
  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(admin_email) |>
    emayili::subject(paste("Nueva solicitud de acceso:", new_user_data$username)) |>
    emayili::text(paste0(
      "Hola Alex,\n\n",
      "Se ha registrado un nuevo usuario:\n",
      "Nombre: ", new_user_data$first_name, " ", new_user_data$last_name, "\n",
      "Usuario: ", new_user_data$username, "\n",
      "Email: ", new_user_data$email, "\n\n",
      "Para aprobar este usuario, haz clic en el siguiente enlace:\n",
      approval_link, "\n\n",
      "Saludos,\nTu App"
    ))
  
  smtp(email, verbose = TRUE)
}

# Send welcome email to user (after approval)
send_welcome_email <- function(user_email, username) {
  smtp <- get_smtp_server()
  
  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(user_email) |>
    emayili::subject("Bienvenido a la plataforma") |>
    emayili::text(paste0(
      "Hola ", username, ",\n\n",
      "Tu cuenta ha sido aprobada exitosamente. Ya puedes iniciar sesión en la plataforma.\n\n",
      "Saludos,\nEl Equipo"
    ))
  
  smtp(email, verbose = TRUE)
}

# Send password reset email
send_password_reset_email <- function(user_email, reset_link) {
  smtp <- get_smtp_server()
  
  email <- emayili::envelope() |>
    emayili::from(Sys.getenv("SMTP_USER")) |>
    emayili::to(user_email) |>
    emayili::subject("Restablecimiento de contraseña") |>
    emayili::text(paste0(
      "Hola,\n\n",
      "Hemos recibido una solicitud para restablecer tu contraseña.\n",
      "Haz clic en el siguiente enlace para crear una nueva contraseña:\n",
      reset_link, "\n\n",
      "Si no solicitaste esto, ignora este correo.\n\n",
      "Saludos,\nTu App"
    ))
  
  smtp(email, verbose = TRUE)
}
