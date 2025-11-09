# Plataforma Shiny de Diseños Experimentales

Esta aplicación reúne el contenido de los cursos "Diseños estadísticos" en un entorno Shiny que también funciona como carta de presentación profesional. Incluye una landing page, navegación por cursos/sesiones y un portafolio de proyectos para mostrar experiencia aplicada en agricultura de precisión.

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

- Reemplaza el CV de ejemplo en `www/docs/` por la versión oficial manteniendo el mismo nombre de archivo.
- Ajusta la información de proyectos en `R/server.R` para reflejar nuevos trabajos.
- Actualiza las imágenes de cursos y sesiones colocando archivos `.jpg` en `www/images/courses/` y `www/images/sesiones/`.

## Contacto

- ✉️ [alexprieto1997@gmail.com](mailto:alexprieto1997@gmail.com)
- 💼 [linkedin.com/in/alex-prieto-romani](https://www.linkedin.com/in/alex-prieto-romani/)
- 💻 [github.com/AlexPrietoRomani](https://github.com/AlexPrietoRomani)
