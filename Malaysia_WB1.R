library(deSolve)
library(ggplot2)
state1<-c(y=51.835) # Estado inicial#
times1<-seq(0,5,1/10000) #6 dias con 10000 intervalos por día#
parameters1<-c(Y1=189.45,Y2=85.456) #vector con los valores de los parametros#
#Papalexandratou et al., 2013 Malasia 1 <- Malasia - CM1#
#doi:10.1016/j.fm.2013.02.015#
#Malasia - CM1#

# (i) DATOS PARA ALIMENTAR EL MODELO # FRUCTOSA

#nu1#
U<- 0.4839
f<- 51.835
K<-0.6650
L<-0.0029113
U1 <- (U*f/f+K*L)*L
U1
#nu2#
U2<- 0.4593
f2<- 51.835
K2<-0.6096
BAL2<-0.0006775

U2 <- (U2*f2/f2+K2*BAL2)*BAL2
U2

#Ecuación diferencial#
ODE1<-function(times,state,parameters){ 
  y<-state[1] #estado inicial
  Y1<-parameters[1] #párametro 1
  Y2<-parameters[2] #párametro 2
  dy<--(Y1*U1/0.2)*y-(Y2*U2/0.2)*y #ecuación 
  res<-dy
  list(res)
}


#solución númerica#
solution1<-lsoda(state1, times1, ODE1, parameters1)

# (ii) DATOS EXPERIMENTALES DEL ARTICULO#

ts = c(0,6/24,12/24,24/24,30/24,36/24,48/24,54/24,60/24,72/24,84/24,96/24,120/24)
fru<-c(51.835,45.98,38.734,18.983,4.591,3.002,1.017,1.911,1.216,1.414,0.72,2.407,2.903)

#Grafico#

df <- data.frame(time = solution1[,1], fructose = solution1[,2])
d<-ggplot() +
  geom_line(data = df, aes(x = time, y = fructose), color = "red") +
  geom_point(data = data.frame(time = ts, fructose = fru), aes(x = time, y = fructose), color = "blue") +
  xlab("Tiempo (dias)") + ylab("Fructosa (mg/g(pulpa))") +
  ggtitle("Malasia - CM1")
plot(d)


#Validación estadistica#

#Para validar estadísticamente el modelo matemático se pueden utilizar diversas herramientas de análisis,
#pero una de las más comunes es comparar los resultados del modelo con datos experimentales y evaluar la calidad del ajuste.
#En este caso, podemos utilizar el coeficiente de determinación (R²) y el error cuadrático medio (MSE) como medidas de ajuste#

#Para calcular el R² y el MSE, podemos comparar los valores predichos por el modelo con los datos experimentales.
#Primero, podemos extraer los valores predichos por el modelo en los mismos puntos en los que se tomaron las mediciones experimentales,
#utilizando la función approx() de R:

pred <- approx(solution1[,1], solution1[,2], ts)$y

#Luego, podemos calcular el R² y el MSE utilizando las siguientes fórmulas:
SSE <- sum((fru - pred)^2)
SST <- sum((fru - mean(fru))^2)
R2 <- 1 - SSE/SST
MSE <- SSE/length(fru)
RECM <- sqrt(MSE)

#Donde SSE es la suma de los errores cuadráticos,
#SST es la suma total de cuadrados, 
#R² es el coeficiente de determinación y MSE es el error cuadrático medio#

cat("R² = ", round(R2, 4), "\n")
cat("MSE = ", round(MSE, 4), "\n")
cat("RECM = ", round(RECM, 4), "\n")

#El R² mide la proporción de la variación total en los datos experimentales
#que es explicada por el modelo. Un valor de R² cercano a 1 indica un buen ajuste del modelo a los datos experimentales.

#El MSE mide el promedio de los errores al cuadrado entre los valores experimentales
#y los valores predichos por el modelo. Un valor bajo de MSE indica una buena capacidad predictiva del modelo.

d <- ggplot() +
  geom_line(data = df, aes(x = time, y = fructose), color = "red") +
  geom_point(data = data.frame(time = ts, fructose = fru), aes(x = time, y = fructose), color = "blue") +
  xlab("Tiempo (dias)") + ylab("Fructosa (mg/g(pulpa))") +
  ggtitle("Malasia - CM1") +
  annotate("text", x = 4, y = 75, label = paste0("R² = ", round(R2, 4), ", MSE = ", round(MSE, 4)))

plot(d)

#Prueba de hipotesis con un alfa de 0.05#
#para determinar si hay una diferencia significativa entre los valores predichos por el modelo y los datos experimentales#

# Prueba de hipótesis
alpha <- 0.05

# Calcula los residuos
residuals <- fru - pred

# Prueba t para comparar los valores predichos con los datos experimentales
t_stat <- t.test(residuals)$statistic
p_value <- t.test(residuals)$p.value

#En este código, se calculan los residuos restando
#los valores predichos por el modelo de los datos experimentales. 
#Luego, se realiza una prueba t para comparar los residuos con una 
#media de cero (hipótesis nula: no hay diferencia significativa).
#El resultado de la prueba t incluye el estadístico de prueba (t_stat) y el valor p (p_value).
# Compara el p-valor con el nivel de significancia
if (p_value < alpha) {
  cat("La diferencia entre los valores predichos y los datos experimentales es significativa.\n")
} else {
  cat("No se encontró una diferencia significativa entre los valores predichos y los datos experimentales.\n")
}

cat("Prueba t estadística:", round(t_stat, 4), "\n")
cat("Valor p:", round(p_value, 4), "\n")

#Final plot#

# Primero, definimos los colores que queremos utilizar para cada elemento
colores <- c(Modelo = "red", Artículo = "blue")

# Luego, modificamos el script para asignar un nombre a cada geom y establecer la escala de colores manualmente
d <- ggplot() +
  geom_line(data = df, aes(x = time, y = fructose, color = "Modelo"), show.legend = TRUE) +
  geom_point(data = data.frame(time = ts, fructose = fru), aes(x = time, y = fructose, color = "Artículo"), show.legend = TRUE) +
  xlab("Tiempo (días)") + ylab("Fructosa (mg/g(pulpa))") +
  ggtitle("Malasia - CM1") +
  annotate("text", x = 3, y = 52, label = paste0("R² = ", round(R2, 4), ", RECM = ", round(RECM, 4), ", p-valor =", round(p_value, 4))) +
  labs(color = "Datos") +  # Cambiamos el título de la leyenda
  scale_color_manual(values = colores) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 1)) +  # Límites del eje x y los intervalos entre marcas
  scale_y_continuous(limits = c(0, 53), breaks = seq(0, 53, 10))   # Límites del eje y y los intervalos entre marcas


# Gráfica#
print(d)
