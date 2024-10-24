# Importar librerías
library(readr)      # Para leer archivos CSV
library(modeest)    # Para calcular la moda
library(dplyr)      # Para manipulación de datos
library(ggplot2)    # Para gráficos

# Seleccionar e importar el archivo CSV especificando el separador
Datos_clientes <- read_delim("C:\\Users\\LENOVO I3\\OneDrive\\Escritorio\\datos_clientes_empresa_nuevo.csv", delim = ";")

# Mostrar las primeras filas del dataframe
print(head(Datos_clientes))

# Si quieres ver el dataframe completo en RStudio
View(Datos_clientes)

# Verificar si hay valores nulos en las columnas 'Ingresos' y 'Ubicación'
print(paste("Valores nulos en Ingresos:", sum(is.na(Datos_clientes$Ingresos))))
print(paste("Valores nulos en Ubicación:", sum(is.na(Datos_clientes$Ubicación))))

# Calcular medidas de tendencia central y dispersión por ubicación
resumen_ingresos <- Datos_clientes %>%
  group_by(Ubicación) %>%
  summarise(
    Media = mean(Ingresos, na.rm = TRUE),
    Mediana = median(Ingresos, na.rm = TRUE),
    Moda = mlv(Ingresos, method = "mfv", na.rm = TRUE),
    Desviacion_Estandar = sd(Ingresos, na.rm = TRUE),
    n = n()  # Número de observaciones por ubicación
  )

# Mostrar el resumen de ingresos por ubicación
print(resumen_ingresos)

# Visualizar la distribución de ingresos por ubicación con un histograma
ggplot(Datos_clientes, aes(x = Ingresos, fill = Ubicación)) +
  geom_histogram(binwidth = 500, position = "dodge", alpha = 0.7) +
  labs(title = "Distribución de Ingresos por Ubicación", x = "Ingresos", y = "Frecuencia") +
  theme_minimal()

# Calcular frecuencia y frecuencia acumulada para el diagrama de Pareto
frecuencia_ubicacion <- Datos_clientes %>%
  group_by(Ubicación) %>%
  summarise(Frecuencia = n()) %>%
  arrange(desc(Frecuencia)) %>%
  mutate(Frecuencia_Acumulada = cumsum(Frecuencia),
         Porcentaje = Frecuencia / sum(Frecuencia) * 100,
         Porcentaje_Acumulado = cumsum(Porcentaje))

# Crear el diagrama de Pareto
ggplot(frecuencia_ubicacion, aes(x = reorder(Ubicación, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_line(aes(y = Frecuencia_Acumulada), group = 1, color = "red", size = 1) +
  geom_point(aes(y = Frecuencia_Acumulada), color = "red", size = 2) +
  geom_text(aes(label = round(Porcentaje_Acumulado, 1)), vjust = -0.5) +
  labs(title = "Diagrama de Pareto de Ingresos por Ubicación",
       x = "Ubicación",
       y = "Frecuencia") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Frecuencia Acumulada")) +
  theme_minimal()
