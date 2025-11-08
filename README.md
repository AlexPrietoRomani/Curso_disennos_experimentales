# Plataforma Shiny de Diseños Experimentales y Portafolio Profesional

Esta aplicación Shiny combina el temario de los cursos de diseños experimentales con una landing page personal que resume la experiencia de Alex Prieto Romani. Desde un menú fijo se puede navegar por la presentación profesional, acceder al listado de cursos, revisar proyectos destacados y descargar el CV actualizado.

## Características principales

- **Landing page personal:** Hero con CTA hacia LinkedIn y GitHub, resumen bilingüe, focos profesionales y stack tecnológico.
- **Selección interactiva de cursos:** Tarjetas con partes y sesiones; al seleccionar un curso se habilita la navegación por módulos detallados.
- **Portafolio de proyectos:** Cuatro proyectos destacados con enlaces a los repositorios de GitHub y etiquetas de tecnologías clave.
- **Descarga de CV y contacto:** Botón de descarga directa (archivo de ejemplo incluido) y datos de contacto (correo, LinkedIn y GitHub).
- **Arquitectura modular:** Cada sesión del temario vive en su propio módulo Shiny para facilitar el mantenimiento y la extensión futura.

## Estructura del repositorio

```plaintext
Curso_disennos_experimentales/
├── R/
│   ├── global.R                     # Librerías y configuración global
│   ├── ui.R                         # Tema y bootstrap del render dinámico
│   ├── server.R                     # Navegación entre landing, cursos y módulos
│   └── modules/                     # Módulos por curso/parte/sesión
│       ├── Diseños_estadisticos_V2/
│       │   ├── Parte I (Básica)/session1.R ... session4.R
│       │   └── Parte II (Intermedia)/session5.R ... session9.R
│       └── Diseños_estadisticos_V3/
│           ├── Parte I (IA)/session1.R
│           ├── Parte II (Intermedia)/session1.R ... session3.R
│           └── Parte III (Avanzada)/session1.R ... session4.R
├── www/
│   ├── css/custom.css               # Estilos para landing, navegación y cursos
│   ├── js/custom.js                 # Interacciones personalizadas
│   ├── docs/Alex_Prieto_Romani_CV.pdf  # Archivo temporal para la descarga del CV
│   └── images/                      # Portadas de cursos y miniaturas de sesiones
├── data/                            # Conjuntos de datos de apoyo
├── app.R                            # Punto de entrada de la aplicación
├── DESCRIPTION                      # Metadatos del proyecto
├── renv/ y renv.lock                # Gestión de dependencias
└── README.md                        # Este documento
```

## Cómo ejecutar la aplicación

1. **Clonar el repositorio**
   ```bash
   git clone https://github.com/AlexPrietoRomani/Curso_disennos_experimentales.git
   cd Curso_disennos_experimentales
   ```
2. **Restaurar dependencias con `renv`**
   ```r
   install.packages("renv")
   renv::restore()
   ```
3. **Iniciar la app Shiny**
   ```r
   shiny::runApp()
   # o abrir app.R en RStudio y ejecutar "Run App"
   ```

## Personalización rápida

- **Imágenes de cursos y sesiones:** Coloca archivos `.jpg` en `www/images/courses/` y `www/images/sesiones/` utilizando identificadores en minúsculas y sin espacios.
- **Contenido de las sesiones:** Edita cada archivo del directorio `R/modules/...` para incorporar material, gráficos o código reproducible.
- **CV descargable:** Sustituye `www/docs/Alex_Prieto_Romani_CV.pdf` por la versión oficial del currículum manteniendo el mismo nombre de archivo.
- **Portafolio:** Ajusta la lista `projects_info` en `R/server.R` para añadir nuevos proyectos o actualizar descripciones y etiquetas.

## Despliegue en shinyapps.io

1. Configura `rsconnect`:
   ```r
   install.packages("rsconnect")
   rsconnect::setAccountInfo(name = "<CUENTA>", token = "<TOKEN>", secret = "<SECRET>")
   ```
2. Publica la aplicación:
   ```r
   rsconnect::deployApp()
   ```
3. Accede mediante `https://<CUENTA>.shinyapps.io/<NOMBRE_APP>/`.

## Contacto

- ✉️ Correo: [alexprieto1997@gmail.com](mailto:alexprieto1997@gmail.com)
- 💼 LinkedIn: [linkedin.com/in/alex-prieto-romani](https://www.linkedin.com/in/alex-prieto-romani/)
- 💻 GitHub: [github.com/AlexPrietoRomani](https://github.com/AlexPrietoRomani)

---
Desarrollado por **Alex Prieto Romani** · Agriculture Data Science & Precision Agriculture
