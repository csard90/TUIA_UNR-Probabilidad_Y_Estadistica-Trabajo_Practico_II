################################# Actividad 1 ###################################################
#################################################################################################
# Copia del portapapeles los datos de la base de datos.
 datos_corte <- read.delim("clipboard", col.names = "profunidad_corte")

 # Reemplaza las "," por "." pero queda como vector de carácter
 datos_corte <- gsub(",", ".", datos_corte$profunidad_corte)
# Transforma a tipo de dato numeric
 datos_corte <- as.numeric(datos_corte)

 # Transforma a tipo de dato dataframe
 datos_corte <- as.data.frame(datos_corte)

 # Cambia el nombre a la columna
 colnames(datos_corte) <- c("profundidad_corte")

 # Limite inferior del primer intervalo y limite superior del ultimo
 lim_inf <- 4.99 # Min = 5.14
 lim_sup <- 8.24 # Max = 8.04

 # Cantidad de intervalos: 13 clases (raiz cuadrada de la cantidad de datos)
 intervalos <- 13

 # Genera una secuencia de valores desde min hasta max con la misma amplitud
 cortes <- seq(lim_inf, lim_sup, length.out=intervalos+1)

 # Divide los datos de las profundidades de corte en clases
 division <- cut(datos_corte$profundidad_corte, cortes)

 # Tabla de frecuencia absoluta
 frec_absolutas <- table(division)

 # Tabla de frecuencia relativa
 frec_relativas <- round((frec_absolutas/sum(frec_absolutas)),2)

 # Tabla de frecuencia absoluta acumulada
 abs_acumuladas <- cumsum(frec_absolutas)

 # Tabla de frecuencia relativa acumulada
 rel_acumuladas <- cumsum(frec_relativas)

 # Une todas las tablas de frecuencias calculadas en una tabla
 tabla_frecuencias <- cbind(frec_absolutas, frec_relativas, abs_acumuladas, rel_acumuladas)

 # Tranforma a data frame la tabla de frecuencias
 tabla_frecuencias <- as.data.frame(tabla_frecuencias)
# Medidas resumen:
 # Minimo
 min = min(datos_corte) # 4.99

 # Maximo
 max = max(datos_corte) # 8.24

 mean = mean(datos_corte$profundidad_corte) # 6.57
 rango = max - min # 3.25
 desvio_estandar = sd(datos_corte$profundidad_corte) # 0.54
 proporcion = 117/150 # 0.78


 # Graficos:

 # Boxplot de los datos
 boxplot(datos_corte, horizontal = TRUE)

 # Histograma de las distintas clases
 library(ggplot2)

 ggplot(datos_corte, aes(profundidad_corte)) +
  #Se solicita realizar un histograma y se especifican los colores
  #y los límites de los intervalos con la opción (breaks=cortes)
  geom_histogram(aes(y=..count..),
             	color="black", fill="#f9b28c", breaks=cortes) +
  scale_x_continuous(breaks=cortes)+
  #Configuraciones generales
  #Nombres de los ejes
  labs(x = "Profundidad del corte (cm)", y = "Cantidad de placas") +
#Configuraciones de formato
#Estilo
theme_classic()+
  scale_y_continuous(expand=c(0,0), breaks=seq(0, max(tabla_frecuencias$frec_absolutas), 2))+
  #Fuente para los ejes
  theme(axis.title.x = element_text(face="bold", colour="black", size = 12),
    	axis.title.y = element_text(face="bold", colour="black", size = 12))
 
 
# Calculo del intervalo de confianza con una confianza del 95% de la proporcion de piezas
 # de plastico que tienen una profundidad de corte inferior a 7cm con metodo exacto.
 library(DescTools)

 # Cantidad placas que tuvieron un corte inferior a 7cm
 exitos <- 117

 # Total de la muestra
 total <- 150

 # Calculo de los intervalos de confianza
 BinomCI(exitos, total, conf.level = 0.95, method = "clopper-pearson")




################################# Actividad 2 ###################################################
#################################################################################################

## Carga de datos del segundo sheet
library(readxl)
library(DescTools)
library(stats)
library(nortest)

install.packages("readxl")
install.packages("DescTools")

library(readxl)
library(DescTools)

#Ruta donde tenemos el archivo
datos = read_excel("C:/Users/augus/Downloads/Trabajo-práctico-datos.xlsx", sheet = 2)

df_datos <- as.data.frame(datos)

##############ESPESOR##################################
# Definir los límites de las clases

# Cantidad de intervalos: 10 clases (raiz cuadrada de la cantidad de datos)
intervalos <- 10


clase_esp <- seq(min(df_datos$Espesor), max(df_datos$Espesor), length.out=intervalos+1)

# Agrupar los datos en clases
espesor <- cut(df_datos$Espesor, breaks = clase_esp)

# Tabla de frecuencia absoluta
frec_espesor_abs <- table(espesor)
# Tabla de frecuencia relativa
frec_espesor_rel <- round((frec_espesor_abs/sum(frec_espesor_abs)),2)

# Tabla de frecuencia absoluta acumulada
abs_acumuladas_esp <- cumsum(frec_espesor_abs)

# Tabla de frecuencia relativa acumulada
rel_acumuladas_esp <- cumsum(frec_espesor_rel)

# Une todas las tablas de frecuencias calculadas en una tabla	 
tabla_frecuencias_esp <- cbind(frec_espesor_abs, frec_espesor_rel, abs_acumuladas_esp, rel_acumuladas_esp)
# Transforma a data frame la tabla de frecuencias
tabla_frecuencias_esp <- as.data.frame(tabla_frecuencias_esp)

# Medidas resumen:
# Minimo
min = min(df_datos$Espesor) #13.26

# Maximo
max = max(df_datos$Espesor) #35.28

#Promedio
mean = mean(df_datos$Espesor) #22.41
#Rango
rango = max - min #22.02
#Desvio
desvio_estandar = sd(df_datos$Espesor) #4.56

tabla_frecuencias_esp

#Histograma y boxplot espesor
windows(width = 10, height = 6)
hist(df_datos$Espesor, breaks = 15, xlab = "Espesor", ylab = "Frecuencia")

windows(width = 10, height = 6)
boxplot(df_datos$Espesor, horizontal=TRUE,ylab = "Espesor")
####################################################################

###################### RESISTENCIA ####################################

# Definir los límites de las clases
# Cantidad de intervalos: 10 clases (raiz cuadrada de la cantidad de datos)
intervalos <- 10
    
clase_resist <- seq(min(df_datos$Resistencia), max(df_datos$Resistencia), length.out=intervalos+1)

# Agrupar los datos en clases
resistencia <- cut(df_datos$Resistencia, breaks = clase_resist)

# Tabla de frecuencia absoluta
frec_resist_abs <- table(resistencia)

# Tabla de frecuencia relativa
frec_resist_rel <- round((frec_resist_abs/sum(frec_resist_abs)),2)

# Tabla de frecuencia absoluta acumulada
abs_acumuladas_res <- cumsum(frec_resist_abs)

# Tabla de frecuencia relativa acumulada
rel_acumuladas_res <- cumsum(frec_resist_rel)

# Une todas las tablas de frecuencias calculadas en una tabla	 
tabla_frecuencias_res <- cbind(frec_resist_abs, frec_resist_rel, abs_acumuladas_res, rel_acumuladas_res)

# Transforma a data frame la tabla de frecuencias
tabla_frecuencias_res <- as.data.frame(tabla_frecuencias_res)

# Medidas resumen:

# Minimo
min = min(df_datos$Resistencia) #28.34

# Maximo
max = max(df_datos$Resistencia) #33.04

#Promedio
mean = mean(df_datos$Resistencia) #30.96

#Rango
rango = max - min #4.69

#Desvio
desvio_estandar = sd(df_datos$Resistencia) #1.02

tabla_frecuencias_res


#Histograma y boxplot resistencia
windows(width = 10, height = 6)
hist(df_datos$Resistencia, breaks = 15, xlab = "Resistencia", ylab = "Frecuencia")

windows(width = 10, height = 6)
boxplot(df_datos$Resistencia, horizontal = TRUE,ylab = "Resistencia")

#Diagrama tallo y hoja resistencia
datos_ordenados <- sort(df_datos$Resistencia)
stem(datos_ordenados)
#######################################################################



## a) Analizamos si los MPa estén entre los valores que quisieramos
### Cálculo de variables básicas
n = length(datos$Resistencia)
menores_a_30 = sum(datos$Resistencia < 30)
### Cálculo del intervalo de confianza
BinomCI(menores_a_30, n, conf.level = 0.99, method="clopper-pearson")

## b) Analizamos entre qué valores debe estar el promedio
### Analizamos si se asemeja a una distribución normal
plot.new()
qqnorm(datos$Espesor, main ="Q-Q Plot de espesores con la Normal", xlab = "Cuartiles de la Normal", ylab = "Cuartiles de los espesores")
qqline(datos$Espesor)
ad.test(datos$Espesor)
### Cálculo de IC del promedio
MeanCI(datos$Espesor, conf.level = 0.95)

## c) Analizamos varianzas
### Resistencia
#### Analizamos si es parecida a la distribución normal para usar "Classic" como método
plot.new()
qqnorm(datos$Resistencia, main ="Q-Q Plot de resistencia con la Normal", xlab = "Cuartiles de la Normal", ylab = "Cuartiles de la resistencia")
qqline(datos$Resistencia)
ad.test(datos$Resistencia)
#### IC
VarCI(datos$Resistencia, method="classic", conf.level = 0.95)
### Espesor
#### Ya previamente observamos que la distribución era parecida a la normal podemos usar "classic"
#### IC
VarCI(datos$Espesor, method="classic", conf.level = 0.95)
#### Sacamos de las varianzas las desviaciones estándar
sqrt(0.81)
sqrt(1.41)
sqrt(16.05)
sqrt(28.09)

