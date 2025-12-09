# Plataforma Shiny de Diseños Experimentales

Esta aplicación reúne el contenido de los cursos "Diseños estadísticos" en un entorno Shiny restringido para alumnos. Incluye autenticación de usuarios y navegación por cursos/sesiones.

Toda la estructura de carpetas y módulos se documenta en `Estructura.txt` para facilitar el mantenimiento del proyecto.

## ¿Cómo ejecutar la app?

1. **Clona el repositorio**
   ```bash
   git clone https://github.com/AlexPrietoRomani/Curso_disennos_experimentales.git
   cd Curso_disennos_experimentales
   ```
2. **Restaura las dependencias**
   ```r
   install.packages("renv")
   renv::restore()
   ```
3. **Inicia la aplicación**
   ```r
   shiny::runApp()
   # o abre app.R en RStudio y presiona "Run App"
   ```

## Personalización básica

- Actualiza las imágenes de cursos y sesiones colocando archivos `.jpg` en `www/images/courses/` y `www/images/sesiones/`.

## Contacto

- ✉️ [alexprieto1997@gmail.com](mailto:alexprieto1997@gmail.com)
