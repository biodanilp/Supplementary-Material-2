#Libraries#
install.packages("deSolve")
install.packages("ggplot2")
install.packages("ggtrendline")
##Packages##
library(deSolve)
library(ggplot2)
library(ggtrendline)

#Study data#

Datos <- list(U_gluc_Y <- 0.0963,
              K_gluc_Y <- 0.6397,
              G_gluc_Y <- 55.482,
              L_Y <- 0.1754249,
              U_gluc_LAB <- 0.2004,
              K_gluc_LAB <- 0.6367,
              L_LAB <- 0.0030472,
              Y_Glc_Y <- 225.32,
              Y_Glc_LAB <-127.98, 
              estudio <- "Brazil - WB1",
              compuesto <- "Glucose",
              Tiempos <- c(0,6/24,12/24,24/24,30/24,36/24,48/24,54/24,60/24,72/24,84/24,96/24,120/24,144/24),
              Variacion_glc <- c(55.482,26.802,16.311,13.706,13.248,7.54,1.278,2.694,3.08,0.757,2.465,2.673,3.183,7.631))

#Initial concentration of glucose#  

state1 <-c(y = Datos[[3]]) # Initial status

# Time to model: 10000 data per day 

times1 <-seq(0,6,1/100) 

## Values of the parameters 

parameters1 <-c(Y1=Datos[[8]], Y2=Datos[[9]])

#Differential equation

#Glucose_Y
U_Yeast_Glc <- (Datos[[1]]*Datos[[3]]/Datos[[3]]+Datos[[2]]*Datos[[4]])*Datos[[4]]

#Glucose_LAB

U_LAB_Glc <- (Datos[[5]]*Datos[[3]]/Datos[[3]]+Datos[[6]]*Datos[[7]])*Datos[[7]]

#constant

constate <- c(4.5)


#ODE#
for (i in constate) {
  
  ODE1<-function(times,state,parameters){ 
    y<-state[1] 
    Y1<-parameters[1] 
    Y2<-parameters[2] 
    dy<--Y1*U_Yeast_Glc/i*y-Y2*U_LAB_Glc/i*y 
    res<-dy
    list(res)
  }
  
  #NUMERICAL SOLUTION OF THE EQUATION#
  
  solution1 <-lsoda(state1, times1, ODE1, parameters1)
  solution1 
  
  # (ii) Experimental article data#
  
  ## Graph
  
  Solucion <- as.data.frame(solution1)
  Datos_exp <- data.frame(Tiempos = Datos[[12]], Glucosa =Datos[[13]])
  
  ## x and y axis
  
  eje_titulo <- paste("Experimental Data", Datos[[10]])
  eje_y <- paste(Datos[[11]],"Glucose(mg/g)")
  eje_x <- "Time(days)"
  
  ## Plot 
  
  grafo <- ggplot(data = Solucion, aes(x = time, y = y))+
    geom_line(colour = "red", size =0.9)+
    geom_point(data = Datos_exp, aes(x = Tiempos, y = Glucosa), colour  = "blue")+
    labs(y = eje_y, x = eje_x, title = eje_titulo)
  
  print(grafo)
  
}  

### adjustment ###

Tiempo <- Solucion$time; Tiempo <- Tiempo[-1]
Consumo_glucosa <- Solucion$y; Consumo_glucosa <- Consumo_glucosa[-1]
Solucion <- data.frame(Tiempo, Consumo_glucosa)

modelos <- c("line2P", "line3P", "log2P", "exp2P", "power2P", "power3P")



for (i in modelos) {
  
  p <- ggtrendline(Solucion$Tiempo, Solucion$Consumo_glucosa, model = i, CI.level = 0.95, CI.alpha = 0.05)+
    labs(x = "Time(days)", y = "Glucose(mg/g)")
  print("######################################################")
  print(p)
  print("######################################################")

}



