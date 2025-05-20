# Load necessary library
library(ggplot2)
library(gridExtra)

# Parameters for the simulation
n <- 5000       # Number of observations
beta1 <- 0.5  # Coefficient for the relationship between X and the collider
beta2 <- 0.5  # Coefficient for the relationship between Y and the collider
error_x <- 0.25
error_y <- 0.25
error_col <- 0.25

# Generate data
X <- rnorm(n)
Y <- rnorm(n)
Col <- beta1 * X + beta2 * Y + error_col * rnorm(n)

# Model without the collider
model_without_col <- lm(Y ~ X)
data_without_col <- data.frame(X = X, Y = Y, Y_pred = predict(model_without_col))

# Model with the collider
model_with_col <- lm(Y ~ X + Col)
data_with_col <- data.frame(X = X, Y = Y, Y_pred = predict(model_with_col))

# Print model summaries for analysis
cat("Summary of the model without the collider:\n")
print(summary(model_without_col))
cat("\nSummary of the model with the collider:\n")
print(summary(model_with_col))

# Plotting
g1 <- ggplot(data_without_col, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Y against X without adjusting for Collider", x = "X", y = "Y")

print(g1)

g2 <- ggplot(data_with_col, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(title = "Y against X with adjusting for Collider", x = "X", y = "Y")

print(g2)

# Additional simulation function for collider effect
sim_Col <- function(betaColX, betaColY) {
  n <- 10000
  betaXYCol <- 0.0  # The true effect of X on Y, set as zero
  errorCol <- 1 
  errorX <- 1
  errorY <- 1
  Col <- errorCol * rnorm(n)  # Generate Collider
  X <- betaColX * Col + errorX * rnorm(n)  # Generate X influenced by Collider
  Y <- betaColY * Col + betaXYCol * X + errorY * rnorm(n)  # Generate Y influenced by Collider and X
  
  lm1_Col <- summary(lm(Y ~ X))  # Simple regression
  lm2_Col <- summary(lm(Y ~ X + Col))  # Multiple regression including the collider
  
  coefXYCol <- lm1_Col$coefficients[2, 1]  # Coefficient from simple regression
  coefXYColCon <- lm2_Col$coefficients[2, 1]  # Coefficient from multiple regression
  
  return(data.frame(coefXYCol, coefXYColCon))  # Return the coefficients
}

# Applying the simulation function over a range of betaColX values
betaColX <- seq(-1.0, 1.0, by = 0.1)
betaColY <- 0.5  # Fixed value for betaColY

results_Col <- lapply(betaColX, function(x) cbind(betaColX=x, sim_Col(x, betaColY)))
combined_df_Col <- do.call(rbind, results_Col)

# Plotting the results for the collider scenario
ggplot(combined_df_Col, aes(x = betaColX)) +
  geom_point(aes(y = coefXYCol), color = "darkblue") +
  geom_point(aes(y = coefXYColCon), color = "orange") +
  geom_hline(yintercept = 0.0, linetype = 'dashed', color = 'black') +
  labs(x = "Beta Col X", y = "Coefficient Estimate") +
  theme_minimal()




