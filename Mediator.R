# Load the necessary libraries
library(ggplot2)
library(gridExtra)

# Set the parameters
n <- 30       # 30, 100, 5000     # The number of observations in the study
beta1 <- 0.5        # The coefficient for the relationship between X and M
beta2 <- 0.5        # The coefficient for the relationship between M and Y
errorX <- 0.25      # The random noise for X    
errorY <- 0.25      # The random noise for Y
errorM <- 0.25      # The random noise for M

# Generate the exposure variable (X)
X <- rnorm(n)

# Simulate the relationship between X and the mediator (M)
M <- beta1 * X + errorM * rnorm(n)

# Simulate the relationship between the mediator (M) and the outcome (Y)
Y <- beta2 * M + errorY * rnorm(n)

# Fit the regression model for the mediator M on exposure X
model_M_on_X <- lm(M ~ X)

# Fit the regression model for the outcome Y on mediator M
model_Y_on_M <- lm(Y ~ M)

# Create the plots
# Scatter plot of the mediator (M) against the exposure (X)
p1 <- ggplot(data.frame(X, M), aes(X, M)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Effect of Exposure (X) on Mediator (M)",
       x = "Exposure (X)", y = "Mediator (M)")

# Scatter plot of the outcome (Y) against the mediator (M)
p2 <- ggplot(data.frame(M, Y), aes(M, Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Effect of Mediator (M) on Outcome (Y)",
       x = "Mediator (M)", y = "Outcome (Y)")

# Arrange the plots side by side
grid.arrange(p1, p2, ncol = 2)

# Print the summaries of the regression models
cat("Summary of the regression model for M on X:\n")
print(summary(model_M_on_X))
cat("\n")
cat("Summary of the regression model for Y on M:\n")
print(summary(model_Y_on_M))


# Calculate the indirect effect
indirect_effect <- coef(model_M_on_X)[2] * coef(model_Y_on_M)[2]

# Bootstrap the indirect effect
set.seed(123)  # for reproducibility
n_bootstraps <- 1000
bootstrapped_indirect_effects <- numeric(n_bootstraps)

for (i in 1:n_bootstraps) {
  sample_indices <- sample(1:n, size = n, replace = TRUE)
  boot_X <- X[sample_indices]
  boot_M <- M[sample_indices]
  boot_Y <- Y[sample_indices]
  
  boot_model_M_on_X <- lm(boot_M ~ boot_X)
  boot_model_Y_on_M <- lm(boot_Y ~ boot_M)
  
  bootstrapped_indirect_effects[i] <- coef(boot_model_M_on_X)[2] * coef(boot_model_Y_on_M)[2]
}

# Calculate the confidence interval for the indirect effect
conf_interval <- quantile(bootstrapped_indirect_effects, c(0.025, 0.975))

# Print the results
cat("Indirect effect:", indirect_effect, "\n")
cat("95% Confidence Interval for the indirect effect:", conf_interval, "\n")


# Plot the histogram of the bootstrapped indirect effects
hist(bootstrapped_indirect_effects, breaks = 30, main = "Distribution of Bootstrapped Indirect Effects", xlab = "Indirect Effect", col = "lightblue", border = "darkblue")

# Add a vertical line for the original indirect effect
abline(v = indirect_effect, col = "red", lwd = 2)

# Add vertical lines for the 95% confidence interval
abline(v = conf_interval[1], col = "darkgreen", lty = 2, lwd = 2)
abline(v = conf_interval[2], col = "darkgreen", lty = 2, lwd = 2)

# Add a legend
legend("topright", legend = c("Observed Indirect Effect", "95% Confidence Interval"), col = c("red", "darkgreen"), lty = c(1, 2), lwd = 2)



