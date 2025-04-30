# Shiny App de Estadística Agrícola en R

Este repositorio contiene una aplicación Shiny desarrollada en R para servir como plataforma interactiva de enseñanza del Temario de Estadística Agrícola, dividida en dos partes (básica e intermedia) y 8 sesiones teórico-prácticas.

## Descripción del Proyecto

La aplicación está diseñada para:

- **Presentar** el contenido de cada sesión de forma clara y estructurada.

- **Proporcionar** ejemplos de código reproducibles y plantillas para cada concepto.

- **Permitir** la escalabilidad futura mediante una arquitectura modular basada en módulos de Shiny por sesión.

- **Soportar** la incorporación de recursos multimedia (imágenes, gráficos) en la carpeta www/images/.

## Objetivos

1. Facilitar el aprendizaje de R aplicado a datos agronómicos.

2. Enseñar conceptos estadísticos desde lo descriptivo hasta diseños experimentales avanzados.

3. Ofrecer un entorno interactivo para que los estudiantes practiquen y reproduzcan análisis en R.

## Estructura del Repositorio

```plaintext
my_shiny_app/
├── R/
│   ├── global.R           # Librerías y configuración global
│   ├── ui.R               # Definición de la interfaz de usuario
│   ├── server.R           # Lógica principal del servidor
│   └── modules/           # Módulos por sesión
│       ├── session1.R     # Sesión 1: Importación y Exploración
│       ├── session2.R     # Sesión 2: Estadística Descriptiva Avanzada
│       ├── session3.R     # Sesión 3: Probabilidad y Distribuciones
│       ├── session4.R     # Sesión 4: ANOVA y Diseños Básicos
│       ├── session5.R     # Sesión 5: DCA y RCBD
│       ├── session6.R     # Sesión 6: Diseño Factorial
│       ├── session7.R     # Sesión 7: Split-Plot y Cuadro Latino
│       └── session8.R     # Sesión 8: Potencia y Tamaño de Muestra
├── www/
│   └── images/            # Recursos visuales (imágenes de ejemplo)
├── data/                  # Datos de ejemplo (CSV, Excel)
├── app.R                  # Punto de entrada de la aplicación
├── DESCRIPTION            # Metadatos del paquete (opcional)
├── renv.lock              # Lockfile de dependencias con renv
├── README.md              # Documentación del proyecto
└── .gitignore             # Archivos y carpetas a ignorar en Git
```

## Instalación y Ejecución Local

1. Clonar el repositorio:

```bash
git clone https://github.com/tu_usuario/my_shiny_app.git
cd my_shiny_app
```

2. Instalar R y dependencias:

```bash
install.packages("renv")
renv::restore()  # Instala versiones exactas listadas en renv.lock
```

3. Ejecutar la aplicación:

```bash
library(shiny)
runApp()  # O presionar Run App en RStudio sobre app.R
```

## Despliegue en Shinyapps.io

1. Instalar y configurar rsconnect:

```bash
install.packages("rsconnect")
rsconnect::setAccountInfo(
  name   = "<TU_CUENTA>",
  token  = "<TU_TOKEN>",
  secret = "<TU_SECRET>"
)
```

2. Desplegar:

```bash
rsconnect::deployApp()
```

3. Acceder en: https://<TU_CUENTA>.shinyapps.io/<NOMBRE_APP>/

## Temario de Sesiones

### Parte I: Fundamentos de Estadística Agrícola (4 sesiones, 8 h)

| Sesión   | Título                                           |
|:--------:|:-------------------------------------------------|
| Sesión 1 | Importación de Datos y Exploración Inicial en R  |
| Sesión 2 | Estadística Descriptiva Avanzada                 |
| Sesión 3 | Probabilidad y Distribuciones Esenciales         |
| Sesión 4 | Introducción a Diseños Estadísticos y ANOVA      |

### Parte II: Diseños Experimentales en R (4 sesiones, 8 h)

| Sesión   | Título                                                       |
|:--------:|:-------------------------------------------------------------|
| Sesión 5 | Diseño Completamente al Azar (DCA) y RCBD                    |
| Sesión 6 | Diseño Factorial y Análisis de Interacciones                 |
| Sesión 7 | Split-Plot y Cuadro Latino                                   |
| Sesión 8 | Potencia y Tamaño de Muestra para Diseños Avanzados          |

Cada sesión incluye:

- Contexto agronómico relevante.

- Objetivos de aprendizaje teóricos y prácticos.

- Actividades detalladas con cronograma.

- Ejemplos de código listos para copiar y ejecutar.

- Recursos visuales (imágenes y gráficos).

## Contacto

Para comentarios, sugerencias o reportes de errores, abre un issue o escribe a alexprieto1997@gmail.com.


Desarrollado por Alex Prieto, Magíster en Big Data y Data Science
