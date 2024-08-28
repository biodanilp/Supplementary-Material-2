# Install the 'ggplot2' package if you haven't installed it yet
# install.packages("ggplot2")

# Load the necessary library
library(ggplot2)

# Define parameters
time <- seq(0, 24, by = 1)  # Hours
lag_phase <- 4  # Duration of lag phase (in hours)
exponential_phase <- 10  # Duration of exponential phase (in hours)
stationary_phase <- 6  # Duration of stationary phase (in hours)
death_phase <- 4  # Duration of death phase (in hours)

# Function to simulate microbial growth
simulate_growth <- function(t) {
  if (t < lag_phase) {
    return(0)
  } else if (t < lag_phase + exponential_phase) {
    return(2^(t - lag_phase))
  } else if (t < lag_phase + exponential_phase + stationary_phase) {
    return(2^exponential_phase)
  } else if (t < lag_phase + exponential_phase + stationary_phase + death_phase) {
    return(2^exponential_phase * exp(-(t - lag_phase - exponential_phase - stationary_phase)))
  } else {
    return(0)
  }
}

# Simulation of microbial growth
growth <- sapply(time, simulate_growth)

# Create a data frame with the results
data <- data.frame(Time = time, Growth = growth)

# Plot the microbial growth curve
ggplot(data, aes(x = Time, y = Growth)) +
  geom_line() +
  labs(x = "Time (hours)", y = "Microbial Growth (UFC)") +
  ggtitle("Microbial Growth Curve") +
  theme_minimal()
