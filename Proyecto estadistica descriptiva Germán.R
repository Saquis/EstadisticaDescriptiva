install.packages("readr")
install.packages("readxl")
install.packages("modeest")
install.packages("dplyr")
install.packages("stats") #Ya no instalar, si es que es necesario ya que R lo ejecuta primero. 

#MOdelado de datos

install.packages("plotrix")


#Carga de paquetes necesarios.

library(readr)
library(dplyr)
library(ggplot2)
library(modeest)
library(stats)
library(ggplot2)
library(plotrix)
library(readxl)



#Carga de archivo 
datos <- read_delim("C:/Users/RYZEN 5 GAMER/Desktop/ITQ/ESTADISTICA D/PROYECTO/datos_clientes_empresa_nuevo.csv", delim = ";")


head(datos)


#Calcular medidas de tendencia central y dispersion por Ubicacion

medidas_estadisticas <- datos %>%
  group_by(Ubicación) %>%
  summarise(
    media_ingresos = mean(Ingresos, na.rm = TRUE),
    mediana_ingresos = median(Ingresos, na.rm = TRUE),
    moda_ingresos = mfv(Ingresos)[1],  # Tomar solo una moda si hay múltiples
    varianza_ingresos = var(Ingresos, na.rm = TRUE),
    desviacion_std_ingresos = sd(Ingresos, na.rm = TRUE),
    rango_ingresos = max(Ingresos, na.rm = TRUE) - min(Ingresos, na.rm = TRUE)
  )
print(medidas_estadisticas)

# Calcular la moda para cada ubicación
moda_por_ubicacion <- datos %>%
  group_by(Ubicación) %>%
  summarise(moda_ingresos = mfv(Ingresos)[1])

# Crear boxplot de ingresos por ubicación y agregar puntos de media
ggplot(datos, aes(x = Ubicación, y = Ingresos, fill = Ubicación)) +
  geom_boxplot(alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", shape = 4, size = 3, color = "red") + # Agregar media como un punto rojo
  geom_point(data = moda_por_ubicacion, aes(x = Ubicación, y = moda_ingresos), 
             shape = 17, color = "blue", size = 3) + # Agregar moda como un triángulo azul
  labs(title = "Boxplot de Ingresos por Ubicación con Media y Moda Indicadas", 
       x = "Ubicación", 
       y = "Ingresos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Histograma de Ubicación
library(ggplot2)

# Crear histograma de ingresos por ubicación
ggplot(datos, aes(x = Ingresos, fill = Ubicación)) +
  geom_histogram(binwidth = 5000, alpha = 0.7, position = "dodge") +
  labs(title = "Distribución de Ingresos por Ubicación", x = "Ingresos", y = "Frecuencia") +
  theme_minimal()

# Crear boxplot de ingresos por ubicación
ggplot(datos, aes(x = Ubicación, y = Ingresos, fill = Ubicación)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Boxplot de Ingresos por Ubicación", x = "Ubicación", y = "Ingresos") +
  theme_minimal()

#DIAGRAMA DE PARETO
# Crear tabla de frecuencias por ubicación
datos_pareto <- datos %>%
  group_by(Ubicación) %>% 
  summarise(frecuencia = n()) %>%
  arrange(desc(frecuencia)) %>%
  mutate(frecuencia_acumulada = cumsum(frecuencia),
         porcentaje_acumulado = cumsum(frecuencia) / sum(frecuencia) * 100)

# Graficar el diagrama de Pareto
ggplot(datos_pareto, aes(x = reorder(Ubicación, -frecuencia), y = frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_line(aes(y = frecuencia_acumulada, group = 1, color = "Frecuencia acumulada")) +
  scale_y_continuous(sec.axis = sec_axis(~ . / sum(datos_pareto$frecuencia) * 100, name = "Porcentaje Acumulado")) +
  labs(x = "Ubicación", y = "Frecuencia", title = "Diagrama de Pareto por Ubicación") +
  theme_minimal()

## Añadir Gráfico Circular 
frecuencia_ubicacion <- datos %>%
  group_by(Ubicación) %>%
  summarise(Frecuencia = n())

piepercent <- round(100 * frecuencia_ubicacion$Frecuencia / sum(frecuencia_ubicacion$Frecuencia), 1)

pie(frecuencia_ubicacion$Frecuencia, labels = piepercent,
    main = "Distribución de Clientes por Ubicación",
    col = rainbow(length(frecuencia_ubicacion$Frecuencia)))

legend("topright", legend = frecuencia_ubicacion$Ubicación, fill = rainbow(length(frecuencia_ubicacion$Frecuencia)))



















