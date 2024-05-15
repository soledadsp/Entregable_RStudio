
# Cargando la biblioteca readr para leer el archivo CSV
library(readr)

# Importar el archivo auto_mpg.csv
auto_mpg <- read_csv("auto-mpg.csv")

# Visualizar las primeras filas del conjunto de datos
head(auto_mpg)

# Paso 2: Entender la base de datos
# Renombrar las columnas al español
auto_mpg <- auto_mpg %>%
  rename(
    consumo_combustible = mpg,
    cilindros = cylinders,
    desplazamiento = displacement,
    potencia = horsepower,
    peso = weight,
    aceleracion = acceleration,
    ano_modelo = `model year`,
    origen = origin,
    nombre_auto = `car name`
  )

# Muestra la estructura del conjunto de datos
str(auto_mpg)

# Resumen estadístico del conjunto de datos
summary(auto_mpg)

# Gráfico de dispersión: Consumo de combustible vs. Desplazamiento del motor
plot(auto_mpg$desplazamiento, auto_mpg$consumo_combustible, 
     xlab = "Desplazamiento del motor (pulgadas cúbicas)", 
     ylab = "Consumo de combustible (millas por galón)",
     main = "Relación entre el consumo de combustible y el desplazamiento del motor")

# Gráfico de barras: Distribución de cilindros
barplot(table(auto_mpg$cilindros),
        xlab = "Número de cilindros",
        ylab = "Frecuencia",
        main = "Distribución de cilindros")

# Gráfico de cajas: Peso del vehículo por número de cilindros
boxplot(auto_mpg$peso ~ auto_mpg$cilindros,
        xlab = "Número de cilindros",
        ylab = "Peso del vehículo (libras)",
        main = "Peso del vehículo por número de cilindros")

# Gráfico de barras: Distribución de años del modelo
barplot(table(auto_mpg$ano_modelo),
        xlab = "Año del modelo",
        ylab = "Frecuencia",
        main = "Distribución de años del modelo")


# Estos comandos muestran la estructura y el resumen del conjunto de datos "Auto MPG". 
# Son útiles para comprender la naturaleza de los datos antes de realizar el análisis.

library(dplyr)


# Este bloque de código renombra las columnas del conjunto de datos al español para facilitar su comprensión.

# Paso 3: Generar la regresión lineal
regresion <- lm(consumo_combustible ~ cilindros + desplazamiento + potencia + peso + aceleracion + ano_modelo + origen, data = auto_mpg)
summary(regresion)

# Este bloque ajusta un modelo de regresión lineal utilizando las variables seleccionadas y muestra un resumen del modelo.

# Paso 4: Evaluar y ajustar la regresión lineal si es necesario
# Puedes ajustar las variables predictoras según la significancia estadística y el rendimiento del modelo

# En este paso, se evalúa el modelo y se puede ajustar eliminando variables no significativas.

# Paso 5: Construir un modelo de predicción
# Supongamos que queremos predecir el consumo de combustible de un automóvil con las siguientes características:
# Convertir la columna potencia a tipo numérico
auto_mpg$potencia <- as.numeric(auto_mpg$potencia)

# Ajustar nuevamente el modelo de regresión lineal

nuevos_datos <- data.frame(
  cilindros = 8,
  desplazamiento = 350,
  potencia = 165,
  peso = 3693,
  aceleracion = 11.5,
  ano_modelo = 70,
  origen = 1
)
predict(regresion, nuevos_datos)

# En este paso, se utilizan los coeficientes estimados del modelo para hacer una predicción sobre nuevos datos.

# Conclusiones
# Analiza los resultados de la regresión y las predicciones para sacar conclusiones sobre el modelo predictivo.

# Los siguientes bloques de código realizan la limpieza y ajuste de datos necesarios antes de ajustar el modelo.
auto_mpg$potencia <- as.numeric(auto_mpg$potencia)
regresion_ajustada <- lm(consumo_combustible ~ peso + ano_modelo + origen, data = auto_mpg)
summary(regresion_ajustada)

# En este bloque de código, se ajusta el modelo de regresión lineal nuevamente después de realizar la limpieza y ajuste de datos.

# Crear un nuevo conjunto de datos con las características del automóvil para predecir
nuevos_datos <- data.frame(
  peso = 3000,
  ano_modelo = 80,
  origen = 1
)

# Hacer la predicción
prediccion <- predict(regresion_ajustada, nuevos_datos)
prediccion

# Finalmente, se hace una predicción utilizando el modelo ajustado y se muestra el resultado.
