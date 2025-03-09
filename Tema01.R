#==============================================================================#
# ESPECIALIZACIÓN AVANZADA ESTADISTICA APLICADA A LA INVESTIGACIÓN CON R STD.  #
# Modulo: Estadística inferencial                                              #
# Tema: Técnicas de muestreo                                                   #
# Sesión: 1                                                                    #
# Fecha: 23/02/2025                                                            #
# Docente: Alexis Adonai Morales Alberto                                       #
# ENFOPE - Escuela Nacional de Formación Pública y Empresarial                 #
# Estudiante: Ricardo Antonio Tapia Frez                                       #
#==============================================================================#

## LIMPIEZA DE ENTORNO ----
# Se eliminan todos los objetos del entorno de trabajo para evitar conflictos con datos previos.
rm(list = ls())
cat("Entorno de trabajo limpiado.\n")

#==============================================================================#

## VERIFICACIÓN E INSTALACIÓN DEL PAQUETE 'pacman' ----------------------------- 
# 'pacman' facilita la carga e instalación de paquetes en R.
# Se comprueba si está instalado y, en caso contrario, se instala automáticamente.

if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman", dependencies = TRUE)
  cat("Paquete 'pacman' instalado correctamente.\n")
} else {
  cat("El paquete 'pacman' ya está instalado.\n")
}

# CARGA DE LIBRERÍAS NECESARIAS USANDO 'pacman' --------------------------------
# Se usa 'p_load' para cargar múltiples paquetes de manera eficiente.
# Si un paquete no está instalado, 'p_load' lo instala automáticamente.

pacman::p_load(
  "tidyverse",  # Colección de paquetes para manipulación y visualización de datos
  "haven",      # Importación de archivos de software estadístico como Stata, SPSS y SAS
  "foreign",    # Importación de datos en formatos como Stata, SPSS y otros
  "readxl",     # Lectura de archivos Excel (.xls y .xlsx)
  "openxlsx"    # Lectura y escritura de archivos Excel sin necesidad de dependencias externas
)
cat("Todos los paquetes han sido cargados correctamente.\n")

#==============================================================================#

## IMPORTACIÓN DE DATOS --------------------------------------------------------
# En esta sección, se cargan los datos necesarios para el análisis.

## DATOS IRIS ------------------------------------------------------------------
# 'iris' es un dataset incorporado en R que contiene información sobre tres especies de flores.
# Se almacena en una variable para su manipulación posterior.

Iris <- iris  # Carga del dataset iris en la variable 'Iris'
cat("El conjunto de datos 'iris' ha sido cargado correctamente.\n")

# Opcional: Mostrar las primeras filas del dataset para verificar su contenido.
head(Iris)

## LECTURA DE DATOS DESDE UN CSV -----------------------------------------------
# En esta sección, se carga un conjunto de datos almacenado en un archivo CSV.

# Se usa la función read.csv() para leer el archivo "Mobiles Dataset (2025).csv" ubicado en la carpeta "Datos".
# Es importante asegurarse de que el archivo existe en la ruta especificada.
# Si hay problemas con la codificación o separadores, se pueden agregar parámetros como sep=";", encoding="UTF-8".

Telefonos <- read.csv("Datos/Mobiles Dataset (2025).csv")
cat("El archivo 'Mobiles Dataset (2025).csv' ha sido cargado correctamente.\n")

# Opcional: Mostrar las primeras filas del dataset para verificar su contenido.
head(Telefonos)

#==============================================================================#

## TIPOS DE MUESTREO -----------------------------------------------------------
# En esta sección, se aplican diferentes métodos de muestreo para extraer subconjuntos de datos.

#==============================================================================#

## MUESTREO ALEATORIO SIMPLE CON REEMPLAZO -------------------------------------
# Este tipo de muestreo selecciona elementos de la población de manera aleatoria y con posibilidad de repetición.

### Fundamento:
# El muestreo aleatorio simple con reemplazo es un método de selección en el que cada elemento de la población
# tiene la misma probabilidad de ser elegido en cada extracción. Dado que se realiza con reemplazo,
# un mismo elemento puede aparecer más de una vez en la muestra.

# Principales características:
# - Cada elemento de la población tiene la misma probabilidad de ser seleccionado.
# - Un elemento puede seleccionarse varias veces, ya que después de cada selección se vuelve a incluir en la población.
# - Es útil cuando se necesita modelar situaciones en las que la repetición es posible (ejemplo: simulaciones, experimentos estadísticos).

# Fortalezas:
# - Fácil de implementar y comprender.
# - Mantiene la independencia entre observaciones, lo que facilita el análisis probabilístico.
# - Adecuado para simulaciones y modelos teóricos.

# Debilidades:
# - Puede generar muestras poco representativas si algunos valores se repiten con demasiada frecuencia.
# - No garantiza una buena cobertura de toda la población, ya que algunos elementos pueden no ser seleccionados.
# - En poblaciones pequeñas, el muestreo sin reemplazo suele ser preferible para asegurar mayor representatividad.

# Se genera una muestra de 15 valores aleatorios entre 1 y 100, con reemplazo.
MAS_CP <- sample(1:100, 15, replace = TRUE)

# Se imprimen los valores generados.
cat("Muestra aleatoria simple con reemplazo generada:\n")
print(MAS_CP)

# Se cuenta la frecuencia de cada número en la muestra.
cat("Frecuencia de valores en la muestra:\n")
print(table(MAS_CP))

### Ejemplo con el dataset IRIS ------------------------------------------------
# Se obtiene el número de fila de cada observación en el dataset Iris.
indices_iris <- as.numeric(row.names(Iris))

# Se extrae una muestra aleatoria de 30 posiciones con reemplazo.
Posiciones <- sample(indices_iris, 30, replace = TRUE)

# Se muestra la frecuencia de selección de cada posición.
cat("Frecuencia de selección de posiciones en IRIS:\n")
print(table(Posiciones))

# Se extraen las filas correspondientes a las posiciones seleccionadas.
Iris_MASCP <- Iris[Posiciones,]

cat("Muestra aleatoria simple con reemplazo creada para IRIS.\n")

### Ejemplo con el dataset "Telefonos" -----------------------------------------
# Se calcula el tamaño de la muestra según la fórmula basada en el estadístico Z.

# VALOR DE CONFIANZA
# 90% = 1.650
# 95% = 1.960
# 99% = 2.580
Z = 1.960  # Valor de Z para un nivel de confianza del 95%
p = 0.5     # Probabilidad de éxito (suposición conservadora)
q = 1 - p   # Probabilidad de fracaso
N = dim(Telefonos)[1]  # Tamaño de la población (número de filas en Telefonos)
e = 0.04    # Margen de error del 4%

# Cálculo del tamaño de muestra usando la fórmula estadística.
n = (Z^2 * N * p * q) / (e^2 * (N - 1) + Z^2 * p * q)
n = round(n)  # Redondeamos al entero más cercano

cat("Tamaño de la muestra calculado:", n, "observaciones.\n")

# Se obtiene el número de fila de cada observación en Telefonos.
indices_telefonos <- as.numeric(row.names(Telefonos))

# Se extrae una muestra aleatoria de tamaño 'n' con reemplazo.
Posiciones <- sample(indices_telefonos, n, replace = TRUE)

# Se extraen las filas correspondientes a las posiciones seleccionadas.
Telefonos_MASCR <- Telefonos[Posiciones,]

cat("Muestra aleatoria simple con reemplazo creada para Telefonos.\n")

#==============================================================================#

## MUESTREO ALEATORIO SIMPLE SIN REEMPLAZO -------------------------------------
# En este tipo de muestreo, cada elemento de la población tiene la misma probabilidad de ser seleccionado,
# pero una vez seleccionado, no puede volver a ser elegido en la misma muestra.

### Fundamento:
# El muestreo aleatorio simple sin reemplazo es un método en el que cada elemento de la población tiene
# la misma probabilidad de ser elegido, pero solo puede aparecer una vez en la muestra.
# Es útil cuando queremos garantizar una mayor diversidad en la selección de elementos.

# Principales características:
# - Cada elemento tiene la misma probabilidad de ser seleccionado en cada extracción.
# - Un mismo elemento NO puede aparecer más de una vez en la muestra.
# - Asegura que la muestra cubra diferentes elementos de la población sin repeticiones.

# Fortalezas:
# - Garantiza mayor representatividad de la población al evitar repeticiones.
# - Es adecuado para estudios en los que cada individuo u objeto debe ser único (por ejemplo, encuestas).
# - Mejora la precisión de las estimaciones estadísticas porque evita sesgos por sobre-representación de ciertos elementos.

# Debilidades:
# - No es adecuado para situaciones donde la repetición de valores es necesaria, como en simulaciones o procesos aleatorios.
# - Puede ser menos eficiente en términos computacionales para poblaciones muy grandes.
# - Si la muestra es pequeña en relación con la población, la aleatoriedad puede verse limitada.

# Se genera una muestra de 15 valores aleatorios entre 1 y 100 sin reemplazo.
MAS_SP <- sample(1:100, 15, replace = FALSE)

# Se imprimen los valores generados.
cat("Muestra aleatoria simple sin reemplazo generada:\n")
print(MAS_SP)

# Se cuenta la frecuencia de cada número en la muestra (debe ser 1 para cada número).
cat("Frecuencia de valores en la muestra:\n")
print(table(MAS_SP))

### Ejemplo con el dataset IRIS ------------------------------------------------
# Se obtiene el número de fila de cada observación en el dataset Iris.
indices_iris <- as.numeric(row.names(Iris))

# Se extrae una muestra aleatoria de 10 posiciones sin reemplazo.
Posiciones <- sample(indices_iris, 10, replace = FALSE)

# Se muestra la frecuencia de selección de cada posición.
cat("Frecuencia de selección de posiciones en IRIS:\n")
print(table(Posiciones))

# Se extraen las filas correspondientes a las posiciones seleccionadas.
Iris_MASSN <- Iris[Posiciones,]

cat("Muestra aleatoria simple sin reemplazo creada para IRIS.\n")

### Ejemplo con el dataset "Telefonos" -----------------------------------------
# Se calcula el tamaño de la muestra según la fórmula basada en el estadístico Z.

Z = 1.960  # Valor de Z para un nivel de confianza del 95%
p = 0.5     # Probabilidad de éxito (suposición conservadora)
q = 1 - p   # Probabilidad de fracaso
N = dim(Telefonos)[1]  # Tamaño de la población (número de filas en Telefonos)
e = 0.04    # Margen de error del 4%

# Cálculo del tamaño de muestra usando la fórmula estadística.
n = (Z^2 * N * p * q) / (e^2 * (N - 1) + Z^2 * p * q)
n = round(n)  # Redondeamos al entero más cercano

cat("Tamaño de la muestra calculado:", n, "observaciones.\n")

# Se obtiene el número de fila de cada observación en Telefonos.
indices_telefonos <- as.numeric(row.names(Telefonos))

# Se extrae una muestra aleatoria de tamaño 'n' sin reemplazo.
Posiciones <- sample(indices_telefonos, n, replace = FALSE)

# Se extraen las filas correspondientes a las posiciones seleccionadas.
Telefonos_MASSR <- Telefonos[Posiciones,]

cat("Muestra aleatoria simple sin reemplazo creada para Telefonos.\n")

#==============================================================================#

## MUESTREO ALEATORIO SISTEMÁTICO ----------------------------------------------
# En esta sección se realizará un muestreo sistemático, donde seleccionamos una muestra 
# siguiendo un patrón regular a partir de una posición inicial aleatoria.

### Ejemplo con la base IRIS ---------------------------------------------------
cat("\nIniciando muestreo sistemático con la base IRIS...\n")

# Se establece una semilla para garantizar reproducibilidad en la selección aleatoria.
set.seed(15)

# Se selecciona una primera posición aleatoria dentro del dataset Iris.
primera_flor <- sample(as.numeric(row.names(Iris)), 1)
cat("Primera posición seleccionada en IRIS:", primera_flor, "\n")

# Se calcula el incremento para seleccionar las siguientes observaciones.
incremento <- length(row.names(Iris)) / 10  # Dividimos el total de filas en 10 grupos.
cat("Incremento calculado para IRIS:", incremento, "\n")

# Se genera la secuencia de posiciones siguiendo el incremento.
muestra <- seq(from = primera_flor, by = incremento, length.out = 10)
cat("Secuencia de posiciones seleccionadas:\n")
print(muestra)

# Ajustamos las posiciones para asegurarnos de que están dentro del rango válido (1 a 150).
muestra <- (muestra - 1) %% 150 + 1  # Garantiza que los valores estén dentro del rango 1-150.
cat("Secuencia ajustada de posiciones:\n")
print(muestra)

# Se extraen las filas correspondientes al muestreo sistemático.
Iris_MASis <- Iris[muestra,]
cat("Muestreo sistemático realizado con éxito para IRIS.\n")

### Ejemplo con la base Telefonos ----------------------------------------------
cat("\nIniciando muestreo sistemático con la base Telefonos...\n")

# Se establece una semilla para garantizar reproducibilidad.
set.seed(12)

# Se selecciona una primera posición aleatoria dentro del dataset Telefonos.
primer_pos <- sample(as.numeric(row.names(Telefonos)), 1)
cat("Primera posición seleccionada en Telefonos:", primer_pos, "\n")

# Se calcula el incremento con base en el tamaño de la muestra previamente calculado.
incremento <- length(row.names(Telefonos)) / n  
incremento <- round(incremento)  # Se redondea al número entero más cercano.
cat("Incremento calculado para Telefonos:", incremento, "\n")

# Se genera la secuencia de posiciones siguiendo el incremento.
muestra <- seq(from = primer_pos, by = incremento, length.out = n)
cat("Secuencia de posiciones seleccionadas:\n")
print(muestra)

# Ajustamos las posiciones para asegurarnos de que están dentro del rango válido.
muestra <- (muestra - 1) %% n + 1  # Garantiza que los valores estén dentro del rango 1-n.
cat("Secuencia ajustada de posiciones:\n")
print(muestra)

# Se extraen las filas correspondientes al muestreo sistemático.
Telefonos_MASis <- Telefonos[muestra,]
cat("Muestreo sistemático realizado con éxito para Telefonos.\n")

#==============================================================================#

## MUESTREO ALEATORIO ESTRATIFICADO --------------------------------------------
# En este muestreo, la población se divide en grupos homogéneos (estratos) y se selecciona 
# una muestra aleatoria dentro de cada uno de ellos. En este caso, se usan las especies de IRIS.

# Se establece una semilla para garantizar la reproducibilidad del muestreo.
set.seed(25)
cat("\nIniciando muestreo aleatorio estratificado con la base IRIS...\n")

# Se identifican los estratos dentro del dataset (las especies de flores).
cat("Especies únicas en el dataset IRIS:\n")
print(unique(Iris$Species))  # Verifica los estratos presentes en la base de datos.

# Se seleccionan aleatoriamente 4 observaciones de cada especie (con reemplazo).
muestra_setosa <- sample(1:50, 4, replace = TRUE)  # Filas 1 a 50 corresponden a "setosa"
muestra_versicolor <- sample(51:100, 4, replace = TRUE)  # Filas 51 a 100 son "versicolor"
muestra_virginica <- sample(101:150, 4, replace = TRUE)  # Filas 101 a 150 son "virginica"

# Mensajes de confirmación de selección de muestras.
cat("\nPosiciones seleccionadas para cada especie:\n")
cat("Setosa:", muestra_setosa, "\n")
cat("Versicolor:", muestra_versicolor, "\n")
cat("Virginica:", muestra_virginica, "\n")

# Se combinan las observaciones seleccionadas en una nueva tabla.
MASST <- Iris[c(muestra_setosa, muestra_versicolor, muestra_virginica),]

cat("Muestreo aleatorio estratificado realizado con éxito.\n")