#################################################################
# CURSO DISEÑOS EXPERIMENTALES PARA AGRONOMÍA CON R
# ---------------------------------------------------------------
# Script de Práctica - SESIÓN 1:
# Preparando el Terreno: Importación y Exploración de Datos
#################################################################

# BIENVENIDA
# En esta primera sesión, aprenderemos los fundamentos de R y el flujo de trabajo
# esencial para cualquier análisis de datos: importar, limpiar y explorar.
# El objetivo es tomar un archivo "crudo" (como un Excel o CSV) y convertirlo
# en un conjunto de datos ordenado y listo para el análisis.


# -----------------------------------------------------------------
# PASO 0: INSTALACIÓN DE PAQUETES
# -----------------------------------------------------------------
# Esto solo se hace UNA VEZ. Si ya los tienes, puedes saltar este paso.
# Un "paquete" es una colección de funciones, datos y documentación que
# extiende las capacidades de R.

# install.packages("tidyverse") # Colección esencial para la ciencia de datos (incluye dplyr, ggplot2, readr)
# install.packages("readxl")    # Para leer archivos de Excel (.xls, .xlsx)
# install.packages("janitor")   # Para limpiar nombres de columnas y datos


# -----------------------------------------------------------------
# PASO 1: CARGAR LOS PAQUETES (LIBRERÍAS)
# -----------------------------------------------------------------
# Esto se debe hacer al inicio de CADA sesión de R.
# Cargar un paquete es como poner tus herramientas sobre la mesa de trabajo.

library(tidyverse) # Carga ggplot2, dplyr, readr, y más
library(readxl)
library(janitor)


# -----------------------------------------------------------------
# PASO 2: FUNDAMENTOS DE R (SINTAXIS BÁSICA)
# -----------------------------------------------------------------
# R puede usarse como una calculadora.

# --- Operadores Aritméticos ---
5 + 3          # Suma
10 - 4         # Resta
6 * 7          # Multiplicación
100 / 5        # División
2^3            # Exponenciación

# --- Asignación de variables ---
# Usamos `<-` para guardar un valor en un objeto (variable).
rendimiento_promedio <- 6.5 # en t/ha
costo_por_hectarea <- 350   # en USD

ingreso_bruto <- rendimiento_promedio * costo_por_hectarea
print(ingreso_bruto)
ingreso_bruto

# --- Vectores ---
# Un vector es la estructura de datos más básica: una secuencia de valores.
# Usamos `c()` para combinar o concatenar elementos.

# Vector numérico: Rendimiento de 4 parcelas
rendimientos_parcelas = c(5.8, 6.2, 5.5, 6.0)

# Vector de caracteres: Nombres de los tratamientos
tratamientos <- c("Control", "Fertilizante_A", "Fertilizante_B", "Compost")

# R es "vectorizado": las operaciones se aplican a cada elemento.
rendimientos_en_qq_ha <- rendimientos_parcelas * 100 # Convertir t/ha a qq/ha
print(rendimientos_en_qq_ha)


# -----------------------------------------------------------------
# PASO 3: IMPORTACIÓN DE DATOS
# -----------------------------------------------------------------
# El primer paso real de cualquier análisis.


library(readxl)

# --- Opción A: Importar un archivo CSV ---
# Supongamos que tienes un archivo llamado `datos_ensayo.csv` en una carpeta `data`
# dentro de tu proyecto de RStudio.

# Usaremos `read_csv()` del paquete `readr` (parte del tidyverse). Es más rápido
# y consistente que `read.csv()` de base R.
# Nota: Si el archivo no está en una carpeta 'data', ajusta la ruta.
try({
  datos_csv <- read_csv("C:\Users\ALEX\OneDrive\Asesorias\Escuela CEDEPA\1ra capacitación\Cursos_dictados\Curso_Cedepa_experimentales\Datasets\DBCA_datos_no_normal.csv")
})


# --- Opción B: Importar un archivo Excel ---
# Supongamos que tienes un archivo `datos_ensayo.xlsx`.
try({
  datos_excel <- read_excel("data/datos_ensayo.xlsx", sheet = "Hoja1")
})


# --- Creación de Datos de Ejemplo (si no tienes los archivos) ---
# Si las líneas anteriores dieron error, puedes crear un data.frame de ejemplo
# para seguir practicando. Este data.frame simula datos "sucios".
datos_crudos <- data.frame(
  `ID Parcela` = 1:10,
  `Variedad de Cultivo` = rep(c("Variedad_X", "Variedad_Y"), each = 5),
  `Rendimiento (kg/ha)` = c(4500, 4800, 4300, NA, 5200, 5600, 5900, 4800, "s/d", 6100),
  `Humedad %` = c(15.5, 14.8, 16.1, 15.2, 14.9, 15.8, 16.3, 15.1, 14.7, 15.5),
  check.names = FALSE # Esto permite nombres de columna "malos"
)

datos_crudos

# -----------------------------------------------------------------
# PASO 4: LIMPIEZA BÁSICA DE DATOS
# -----------------------------------------------------------------
# Rara vez los datos vienen listos para analizar.

library(janitor)

# --- 4.1. Limpiar Nombres de Columnas ---
# Los nombres con espacios, paréntesis o símbolos son problemáticos.
# `janitor::clean_names()` los estandariza automáticamente.
datos_nombres_limpios <- clean_names(datos_crudos)
names(datos_nombres_limpios) # Ver los nuevos nombres: id_parcela, variedad_de_cultivo, etc.

library(tidyverse)
# --- 4.2. Inspeccionar y Corregir Tipos de Datos ---
# `glimpse()` es la mejor función para una primera inspección.
glimpse(datos_nombres_limpios)

# ¡PROBLEMA! `rendimiento_kg_ha` fue leído como <chr> (caracter) porque
# contenía el texto "s/d". Debemos convertirlo a numérico.
# R forzará los textos que no pueda convertir a NA (¡lo cual es bueno!).
datos_tipos_corregidos <- datos_nombres_limpios %>%
  mutate(
    rendimiento_kg_ha = as.numeric(as.character(rendimiento_kg_ha))
  )

glimpse(datos_tipos_corregidos) # Ahora rendimiento_kg_ha es <dbl> (numérico)


# --- 4.3. Manejo de Valores Faltantes (NA) ---
# Primero, detectamos y contamos los NAs.
colSums(is.na(datos_tipos_corregidos))

# Vemos que ahora hay 2 NAs en `rendimiento_kg_ha`: el que ya estaba
# y el que se generó al convertir "s/d".

# Opción A: Eliminar las filas con cualquier NA (usar con cuidado)
datos_sin_na <- datos_tipos_corregidos %>%
  drop_na() # del paquete tidyr, parte de tidyverse

print(datos_sin_na)

# Opción B: Imputar (reemplazar) NAs con la media (un enfoque simple)
datos_imputados <- datos_tipos_corregidos %>%
  mutate(
    rendimiento_kg_ha = ifelse(is.na(rendimiento_kg_ha),
                               mean(rendimiento_kg_ha, na.rm = TRUE), # Calcula la media ignorando NAs
                               rendimiento_kg_ha)
  )

print(datos_imputados)

# Para el resto del análisis, usaremos el dataset sin NAs.
datos_limpios <- datos_sin_na


# -----------------------------------------------------------------
# PASO 5: EXPLORACIÓN INICIAL DE DATOS
# -----------------------------------------------------------------
# Ahora que los datos están limpios, podemos explorarlos.

# --- 5.1. Resumen Estadístico ---
# `summary()` nos da un resumen de cada columna.
summary(datos_limpios)


# --- 5.2. Exploración Visual ---

# Histograma de una variable numérica
ggplot(data = datos_limpios, aes(x = rendimiento_kg_ha)) +
  geom_histogram(bins = 5, fill = "skyblue", color = "black") +
  labs(
    title = "Distribución del Rendimiento",
    x = "Rendimiento (kg/ha)",
    y = "Frecuencia"
  ) +
  theme_bw()


# Boxplot para comparar una variable numérica entre grupos
ggplot(data = datos_limpios, aes(x = variedad_de_cultivo, y = rendimiento_kg_ha, fill = variedad_de_cultivo)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Comparación de Rendimiento entre Variedades",
    x = "Variedad",
    y = "Rendimiento (kg/ha)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


# Gráfico de dispersión para ver la relación entre dos variables numéricas
ggplot(data = datos_limpios, aes(x = humedad_percent, y = rendimiento_kg_ha)) +
  geom_point(size = 3, color = "darkgreen", alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE, color = "red") + # Añade una línea de tendencia
  labs(
    title = "Relación entre Humedad y Rendimiento",
    x = "Humedad a la Cosecha (%)",
    y = "Rendimiento (kg/ha)"
  ) +
  theme_linedraw()


#################################################################
# FIN DE LA PRÁCTICA DE LA SESIÓN 1
# ¡Felicidades! Has completado el ciclo básico de importación,
# limpieza y exploración. Ya estás listo para el análisis descriptivo
# y estadístico de las próximas sesiones.
#################################################################