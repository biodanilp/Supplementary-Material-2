#Packages#
install.packages("deSolve")
install.packages("ggplot2")
#Libraries#
library(deSolve)
library(ggplot2)

state1<-c(y=49.669) #Initial state #
times1<-seq(0,6,1/10000) #6 days with 10000 intervals per day#
parameters1<-c(Y1=223.11,Y2=126.73) #Parameter values#
#Papalexandratou et al., 2011 Brazil <- Brazil - WB1#
#doi:10.1016/j.fm.2011.06.003#
#Brazil - WB1#

#(i). DATA TO FEED THE FRUCTOSE MODEL#

#nu1#
U<- 0.0963
f<- 49.669
K<-0.6103
L<-0.1754249
U1 <- (U*f/f+K*L)*L
U1
#nu2#
U2<- 0.2004 
f2<- 49.669
K2<-0.6273
BAL2<-0.0030472

U2 <- (U2*f2/f2+K2*BAL2)*BAL2
U2

#Differential equation#
ODE1<-function(times,state,parameters){ 
    y<-state[1] #inital state
   Y1<-parameters[1] #parameter 1
   Y2<-parameters[2] #parameter 2
   dy<--(Y1*U1/5)*y-(Y2*U2/5)*y #equation 
   res<-dy
   list(res)
}


#number solution#
solution1<-lsoda(state1, times1, ODE1, parameters1)

#(ii). EXPERIMENTAL DATA OF ARTICLE#

ts = c(0,6/24,12/24,24/24,30/24,36/24,48/24,54/24,60/24,72/24,84/24,96/24,120/24,144/24)
fru<-c(49.669,31.959,18.937,19.238,15.217,10.351,0.809,1.007,1.205,0.757,1.996,2.673,3.183,2.287)

#Graph#

df <- data.frame(time = solution1[,1], frucose = solution1[,2])
d<-ggplot() +
   geom_line(data = df, aes(x = time, y = frucose), color = "red") +
   geom_point(data = data.frame(time = ts, frucose = fru), aes(x = time, y = frucose), color = "blue") +
   xlab("Time (days)") + ylab("Fructose (mg/g)") +
   ggtitle("Brazil - WB1")
plot(d)


#Statistical validation#

#To statistically validate the mathematical model, various analysis tools can be used,
#but one of the most common is to compare the model results with experimental data and evaluate the quality of the fit.
#In this case, one can use the coefficient of determination (R²), the mean squared error (MSE), and the root mean square error (RMSE) as fitting measures#

#To calculate R², MSE, and RMSE, one can compare the values predicted by the model with
#the experimental data. First, extract the values predicted by the model at the same points 
#where the experimental measurements were taken, 
#using the approx() function in R:


pred <- approx(solution1[,1], solution1[,2], ts)$y

#Then, R², MSE, and RMSE can be calculated using the following formulas:
SSE <- sum((fru - pred)^2)
SST <- sum((fru - mean(fru))^2)
R2 <- 1 - SSE/SST
MSE <- SSE/length(fru)
RMSE <- sqrt(MSE)

#Where SSE is the sum of squared errors,
#SST is the total sum of squares,
#R² is the coefficient of determination, MSE is the mean squared error and RMSE is the root mean square error.

cat("R² = ", round(R2, 4), "\n")
cat("MSE = ", round(MSE, 4), "\n")
cat("RMSE = ", round(RMSE, 4), "\n")

#R² measures the proportion of total variation in the experimental data
#that is explained by the model. A value of R² close to 1 indicates a good fit of the model to the experimental data

#MSE measures the average of squared errors between experimental values
#and the values predicted by the model. A low MSE value indicates good predictive capability of the model.

#The root mean square error (RMSE) was determined, which quantifies the magnitude of deviation of simulated values from observed values.

d <- ggplot() +
  geom_line(data = df, aes(x = time, y = frucose), color = "red") +
  geom_point(data = data.frame(time = ts, frucose = fru), aes(x = time, y = frucose), color = "blue") +
  xlab("Time (days)") + ylab("Fructose (mg/g)") +
  ggtitle("Brazil - WB1") +
  annotate("text", x = 2, y = 50, label = paste0("R² = ", round(R2, 4), ", MSE = ", round(MSE, 4)))

plot(d)

#Two-sample Kolmogorov–Smirnov test - KS#
#Ho: The simulation results follow the same distribution of experimental data#
#Ha: The simulation results do not follow the same distribution of experimental data#
#Hypothesis test with an alpha of 0.05#
#to determine if there is a significant difference between the data and the exponential distribution#
#Hypothesis test
alpha <- 0.05

# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(fru, pred)
ks_test

# Results Kolmogorov-Smirnov test
cat("Kolmogorov-Smirnov test:", "\n")
cat("Test statistic:", round(ks_test$statistic, 4), "\n")
cat("p-value:", round(ks_test$p.value, 4), "\n")

if (ks_test$p.value < alpha) {
  cat("The simulation results do not follow the same distribution of experimental data.\n")
} else {
  cat("The simulation results follow the same distribution of experimental data.\n")
}


#Final plot#
p_value <- round(ks_test$p.value, 4)
D <- round(ks_test$statistic, 4)

# First, define the colors we want to use for each element:
colors <- c(Model = "red", Article = "blue")

# Then the script is modified to assign a name to each geom and set the color scale manually
d <- ggplot() +
  geom_line(data = df, aes(x = time, y = frucose, color = "Model"), show.legend = TRUE) +
  geom_line(data = data.frame(time = ts, frucose = fru), aes(x = time, y = frucose, color = "Article"), show.legend = TRUE) +
  xlab("Time (days)") + ylab("Fructose (mg/g)") +
  ggtitle("Brazil - WB1") +
  annotate("text", x = 4, y = 52, label = paste0("R² = ", round(R2, 4), ", RMSE = ", round(RMSE, 4), ", p-value =", round(p_value, 4), ", D =", round(D, 4))) +
  labs(color = "Data") +  # Change the title of the legend
  scale_color_manual(values = colors) +
  scale_x_continuous(limits = c(0, 6), breaks = seq(0, 6, 1)) +  # x-axis boundaries and intervals between marks
  scale_y_continuous(limits = c(0, 53), breaks = seq(0, 53, 10))   # y-axis boundaries and intervals between marks


#PLOT#
print(d)


