library(ggplot2)
library(gridExtra)

# Define main variables and coefficients
n <- 30
beta1 <- 0.5
beta2 <- 0.5
errorX <- 0.25
errorY <- 0.25

# Confounder
Con <- rnorm(n)

# Variables influenced by the confounder and noise
X_with_Con <- beta1 * Con + errorX * rnorm(n)
Y_with_Con <- beta2 * Con + errorY * rnorm(n)

# Variables without the influence of the confounder (only noise)
X_without_Con <- errorX * rnorm(n)
Y_without_Con <- errorY * rnorm(n)

# Plot Y against X without the effect of confounder
p1 <- ggplot(data.frame(X = X_without_Con, Y = Y_without_Con), aes(x = X, y = Y)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Y against X without the effect of Confounder", x = "X", y = "Y")

# Print the plot for scenario without confounder
print(p1)

# Plot Y against X with the effect of confounder
p2 <- ggplot(data.frame(X = X_with_Con, Y = Y_with_Con), aes(x = X, y = Y)) +
  geom_point(color = "lightblue") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Y against X with the effect of Confounder", x = "X", y = "Y")

# Print the plot for scenario with confounder
print(p2)

# Linear regression analyses
model1 <- lm(Y_without_Con ~ X_without_Con)
model2 <- lm(Y_with_Con ~ X_with_Con + Con)

# Output model summaries
summary1 <- summary(model1)
summary2 <- summary(model2)

# Display regression summaries
print(summary1)
print(summary2)

# Data frame for plotting
data_Y <- data.frame(Con = Con, Y_without_Con = Y_without_Con, Y_with_Con = Y_with_Con)

# Plot Y with the effect of confounder
g1 <- ggplot(data_Y, aes(x = Con, y = Y_with_Con)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Outcome Y with the effect of Confounder Con", x = "Confounder (Con)", y = "Outcome (Y)")

print(g1)

# Plot Y without the effect of confounder
g2 <- ggplot(data_Y, aes(x = Con, y = Y_without_Con)) +
  geom_point(color = "orange") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Outcome Y without the effect of Confounder Con", x = "Confounder (Con)", y = "Outcome (Y)")

print(g2)


# Data frame for plotting
data_X <- data.frame(Con = Con, X_without_Con = X_without_Con, X_with_Con = X_with_Con)

# Plot X with the effect of confounder
d1 <- ggplot(data_X, aes(x = Con, y = X_with_Con)) +
  geom_point(color = "pink") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Exposure X with the effect of Confounder Con", x = "Confounder (Con)", y = "Exposure (X)")

print(d1)

# Plot X without the effect of confounder
d2 <- ggplot(data_X, aes(x = Con, y = X_without_Con)) +
  geom_point(color = "pink") +
  geom_smooth(method = "lm", color = "darkblue", se = FALSE) +
  labs(title = "Exposure X without the effect of Confounder Con", x = "Confounder (Con)", y = "Exposure (X)")

print(d2)

# Arrange the plots side by side
grid.arrange(d1, d2, g1, g2, p2, p1, ncol = 2, nrow = 3)


# Linear model analyses
# As these are essentially random noise, modeling them against Con is not meaningful.
# We create a sequence for plotting or theoretical modeling purposes.
seq_index <- 1:n

model_y_without <- lm(Y_without_Con ~ seq_index)  # Not meaningful, but included for structure
model_y_with <- lm(Y_with_Con ~ Con)

model_x_without <- lm(X_without_Con ~ seq_index)  # Not meaningful, but included for structure
model_x_with <- lm(X_with_Con ~ Con)

# Output model summaries
summary_y_without <- summary(model_y_without)
summary_y_with <- summary(model_y_with)
summary_x_without <- summary(model_x_without)
summary_x_with <- summary(model_x_with)

# Print the summaries
print(summary_y_without)
print(summary_y_with)
print(summary_x_without)
print(summary_x_with)


# ADDITIONAL ANALYSIS WITH SIM_CON FUNCTION
sim_Con <- function(betaConX) {
  n <- 5000
  betaConY <- 0.5
  betaXY <- 0.0
  errorCon <- 1 
  errorX <- 1
  errorY <- 1
  Con_sim <- errorCon * rnorm(n)
  X_sim <- betaConX * Con_sim + errorX * rnorm(n)
  Y_sim <- betaConY * Con_sim + betaXY * X_sim + errorY * rnorm(n)
  lm1 <- summary(lm(Y_sim ~ X_sim))
  lm2 <- summary(lm(Y_sim ~ X_sim + Con_sim))
  coefXY <- lm1$coefficients[2, 1]
  coefXYCon <- lm2$coefficients[2, 1]
  return(data.frame(coefXY, coefXYCon))
}

betaConX_seq <- seq(-1.0, 1.0, by = 0.1)
results <- lapply(betaConX_seq, function(x) cbind(betaConX = x, sim_Con(x)))
combined_df <- do.call(rbind, results)

# Plotting the additional analysis results
ggplot(combined_df, aes(x = betaConX)) +
  geom_point(aes(y = coefXY), color = "red") +
  geom_point(aes(y = coefXYCon), color = "blue") +
  geom_hline(yintercept = 0.0, linetype = 'dashed', color = 'black') +
  labs(x = "Beta Con X", y = "Coefficient Estimate") +
  theme_minimal()


