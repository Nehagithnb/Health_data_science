# To Start with we are just going to show how artificial data is created to represent scenarios of confounding, colliding and mediating

# SCENARIO 1: CONFOUNDING WITH LINEAR REGRESSION (CONTINUOUS VARIABLES)
# Simulate the situation of a traditional confounder
# We are investigating the relationship between an exposure (risk factor) X and and outcome Y
# In reality, they are NOT associated. However there is a strong confounding variable: Con
# So that Con causes X and also Con causes Y (but X does not cause Y)
# You could draw this scenario as a DAG


# X -- Exposure
# Y -- Outcome

# A single linear regression simulation of this could be coded by:
n <- 100            # the number of observations in the study
beta1 <- 0.5        # the coefficient in the true relation ship between Con and X
beta2 <- 0.5        # the coefficient in the true relation ship between Con and Y
errorX <- 0.25      # The random noise for X    
errorY <- 0.25      # The random noise for Y

Con <- rnorm(n)                  # Start by generating the n observed values of Con from a standard normal distribution   
X <- beta1*Con + errorX*rnorm(n) # X values are linearly caused by the confounder, plus some noise
Y <- beta2*Con + errorY*rnorm(n) # Y values are linearly caused by the confounder, plus some noise

par(mar = c(4,4,2,2))
plot(Y ~ X)                # A quick plot will usually show a strong linear association between Y and X (because of the confounder)
summary(lm(Y ~ X))         # A linear model will usually show that the effect of X on Y is highly statistically significant
summary(lm(Y ~ X + Con))   # The addition of the confounder to the model will usually show that the effect of X 'disappears' and Con is revealed as a confounder

# The above code has random noise, so you will get a different answer each time you run it. If the noise level is high then true relationships 
# may also be obscured. Run the simulations several times changing the parameter values to get a feel for the outcomes

# When you understand the above code, use a similar approach to try and generate examples for colliders and mediators




# SCENARIO 2: COLLIDER WITH LINEAR REGRESSION (CONTINUOUS VARIABLES)
# Set the parameters
n <- 100            # the number of observations in the study
beta1 <- 0.5        # the coefficient in the true relationship between X and Y
beta2 <- 0.1
errorX <- 0.25      # The random noise for X    
errorY <- 0.25      # The random noise for Y
errorCol <- 0.25

# Generate a common cause (Collider) variable Col
X <- rnorm(n)
Y <- rnorm(n)

Col <- beta1*X + beta2*Y + errorCol*rnorm(n)

# Simulate the relationship between X and Col

# Create a scatter plot of Y against X
par(mar = c(4,4,2,2))
plot(Y ~ X)

# Fit a linear regression model with just X as the predictor
summary(lm(Y ~ X))
#this shows effect of X on Y which is statistically significant
#because it causes direct relationship between them

# Fit a linear regression model with X and Col as predictors
summary(lm(Y ~ X + Col))
#Here, the collider is included, effect of X on Y, 
#which is statistically non-significant
#That is the Collider Effect, that is including a collider variable creates 
#spurious association between X and Y, even though they are not directly related.
#Col variable creates artificial relationship between X and Y.


# SCENARIO 3: MEDIATOR WITH LINEAR REGRESSION (CONTINUOUS VARIABLES)
# Set the parameters
n <- 100            # the number of observations in the study
beta1 <- 0.5        # the coefficient in the true relationship between X and M
beta2 <- 0.5        # the coefficient in the true relationship between M and Y
errorX <- 0.25      # The random noise for X    
errorY <- 0.25      # The random noise for Y
errorM <- 0.25      # The random noise for M

# Generate the exposure variable (X)
X <- rnorm(n)

# Simulate the relationship between X and the mediator (M)
Med <- beta1 * X + errorM * rnorm(n)

# Simulate the relationship between the mediator (M) and the outcome (Y)
Y <- beta2 * Med + errorY * rnorm(n)

# Create a scatter plot of Y against X
par(mar = c(4,4,2,2))
plot(Y ~ X)

# Fit a linear regression model with just X as the predictor
summary(lm(Y ~ X))
#effect of X on Y appears statistically significant 
#bcz it captures direct relationship btwn them.

# Fit a linear regression model with X and M as predictors
summary(lm(Y ~ X + Med))
#here the Mediator/Intermediate Variable (Med) is included,
#the effect of X on Y might be partially mediated through Med
#and the significance relationship might change.
#This is MEdiation effect, where Med explains or mediates 
#part of the relationship between X and Y




################################################################



# SCENARIO 4: CONFOUNDING WITH LOGISTIC REGRESSION (CATEGORICAL VARIABLES / FACTORS)

# You may want to consider logistic regression, which is commonly used in epidemiology
# The simulation of the data is a little more complicated

# Remember that a logistic regression is a model for the probability of an outcome, 
# It is like a linear regression on the log(odds) scale, where
#  p = 1 / (1 + exp-(b0 + b1x1...))

n <- 100      # the number of observations in the study
intX <- 0     # the intercept for the logistic regressions for the effect of the confounder on X
intY <- 0     # the intercept for the logistic regressions for the effect of the confounder on Y
bX <- 2       # the coefficient for the effect of the confounder on X (the log of the odds ratio)
bY <- 2       # the coefficient for the effect of the confounder on Y (the log of the odds ratio)

Con <-rbinom(n, 1, 0.5)           # Con is now a categorical factor, with two levels, so simulate n values of Con from the binomial distribution
Con <- rnorm(n, 0, 1)

pX <- 1 / (1 + exp(-intX - bX * Con))   # Use the logistic model to generate the EXPECTED probabilities of X given the value of the confounder
pY <- 1 / (1 + exp(-intY - bY * Con))   # Use the logistic model to generate the EXPECTED probabilities of Y given the value of the confounder
X <- rbinom(n,1, pX)              # Use the Expected probabilities for X to generate a value either 1 or 0 for the OBSERVED value of X
Y <- rbinom(n,1, pY)              # Use the Expected probabilities for Y to generate a value either 1 or 0 for the OBSERVED value of Y

# Now run the logistic regressions with the 'observed' values of X, Y and Con
summary(glm(Y ~ X, family = binomial))       
# Will usually show that the effect of X on Y is highly statistically significant
summary(glm(Y ~ X + Con, family = binomial)) 
# The addition of the confounder to the model will usually show that 
# the effect of X 'disappears' and Con is revealed as a confounder

# When you understand the above code, use a similar approach to try and generate examples for colliders and mediators


# SCENARIO 5: COLLIDER WITH LOGISTIC REGRESSION (CATEGORICAL VARIABLES / FACTORS)
# Set the parameters
n <- 10            # the number of observations in the study
bX <- 2             # the coefficient for the effect of X
bY <- 2             # the coefficient for the effect of Y
b0 <- 0

# Generate observed values for X and Y based on the expected probabilities
X <- rbinom(n, 1, 0.5)
Y <- rbinom(n, 1, 0.5)

pCol <- 1 / (1 + exp(-b0 - bX * X - bY * Y))

Col <- rbinom(n, 1, pCol)

# Run logistic regressions with the observed values of X, Y, and Col
summary(glm(Y ~ X, family = binomial))
# This logistic regression model examines the relationship between X and Y
#without considering the collider variable. 
#The 'family = binomial' argument specifies that it's a logistic regression. 
#It will usually show a statistically significant effect of X on Y, 
#due to the collider effect.

summary(glm(Y ~ X + Col, family = binomial))
# This logistic regression model includes the collider variable (Col) as a covariate.
#It examines the effect of X on Y while controlling for the collider. 
#The 'family = binomial' argument specifies logistic regression. 
#The addition of the collider will usually show that the effect of X 'disappears,'
#and the collider variable (Col) is revealed as a confounder. 
#This is a common manifestation of the collider bias.
#This is known as collider bias, where controlling for a variable that lies on the 
#causal pathway between the exposure and outcome can introduce spurious associations.


# SCENARIO 6: MEDIATOR WITH LOGISTIC REGRESSION (CATEGORICAL VARIABLES / FACTORS)
# Set the parameters
n <- 100            # the number of observations in the study
intX <- 0           # the intercept for the logistic regression model for the effect of X
intY <- 0           # the intercept for the logistic regression model for the effect of Y
intM <- 0           # the intercept for the logistic regression model for the effect of M
bX <- 2             # the coefficient for the effect of X
bY <- 2             # the coefficient for the effect of Y
bM <- 2             # the coefficient for the effect of M

# Generate the exposure variable (X)
X <- rbinom(n, 1, 0.5)
# X is a binary categorical variable, simulating n values from a binomial distribution with a 0.5 probability of success.

# Use logistic regression models to generate expected probabilities for M based on X
pM <- 1 / (1 + exp(-intM - bM * X))
# This logistic regression model calculates the expected probabilities of M given the value of X. The logistic function maps values to probabilities.

# Generate observed values for M based on the expected probabilities
Med <- rbinom(n, 1, pM)
# This line uses the expected probabilities to generate observed values for M. The rbinom function generates binary outcomes (0 or 1) based on the expected probabilities.

# Use logistic regression models to generate expected probabilities for Y based on M
pY <- 1 / (1 + exp(-intY - bY * Med))
# This logistic regression model calculates the expected probabilities of Y given the value of M. The logistic function maps values to probabilities.

# Generate observed values for Y based on the expected probabilities
Y <- rbinom(n, 1, pY)
# This line uses the expected probabilities to generate observed values for Y. The rbinom function generates binary outcomes (0 or 1) based on the expected probabilities.

# Run logistic regressions with the observed values of X, Y, and M
summary(glm(Y ~ X, family = binomial))
# This logistic regression model examines the relationship between X and Y without considering the mediator variable (M). The 'family = binomial' argument specifies that it's a logistic regression. It will usually show a statistically significant effect of X on Y, as the mediator is not part of the model.

summary(glm(Y ~ X + Med, family = binomial))
# This logistic regression model includes the mediator variable (M) as a covariate.
# It examines the effect of X on Y while controlling for the mediator. 
# The 'family = binomial' argument specifies logistic regression. 
# The addition of the mediator will usually show that the effect of X on Y may be 
# partially mediated through M, and the significance of the relationship may change. 
# This demonstrates a mediation effect, where the mediator (M) explains or mediates 
# part of the relationship between the exposure (X) and the outcome (Y).


# one model with only the exposure (X) as a predictor 
# and another model with both the exposure (X) and the mediator (M) as predictors. 
# The summary function provides the results of these models. 
# The addition of the mediator in the second model typically demonstrates how the 
# effect of the exposure (X) on the outcome (Y) may be partially mediated through 
# the mediator (M), and the significance of the relationship may change. 
# This demonstrates a mediation effect, where the mediator (M) explains or mediates 
# part of the relationship between the exposure (X) and the outcome (Y).




##########################################################################

#Scenario 7: Confounding with Linear Regression (Continuous var)

n <- 100
Con <- rnorm(n)
X <- beta1 * Con + errorX * rnorm(n)
Y <- beta2 * Con + errorY * X + betaCon * rnorm(n)
summary(lm(Y ~ X))
summary(lm(Y ~ X + Con))

#OR

n <- 100
Con <- rnorm(n)
X <- beta1 * Con + errorX * rnorm(n)
Y <- beta2 * Con + errorY * X + betaC * rnorm(n)
Z <- beta3 * X + betaC * Y + errorX * Con + rnorm(n)
summary(lm(Y ~ X))
summary(lm(Y ~ X + Con))

#Scenario 8: Collider with Linear Regression (Continuous var)

n <- 100
X <- rnorm(n)
Y <- rnorm(n)
Col <- beta1 * X + beta2 * Y + errorCol * rnorm(n)
summary(lm(Y ~ X))
summary(lm(Y ~ X + Col))

#Scenario 9: Mediator with Linear Regression (Continuous var)

n <- 100
X <- rnorm(n)
Med <- beta1 * X + errorM * rnorm(n)
Y <- beta2 * Med + errorY * rnorm(n)
summary(lm(Y ~ X))
sumamry(lm(Y ~ X + Med))

#Scenario 10: Confounding with Logistic Regression (Categorical var)

n <- 100
Con <- rnorm(n)
pX <- 1 / (1 + exp(-intX - bX * Con))
pY <- 1 / (1 + exp(-intY - bY * Con))
X <- rbinom(n, 1, pX)
Y <- rbinom(n, 1, pY)
summary(glm(Y ~ X, family = binomial))
summary(glm(Y ~ X + Con, family = binomial))

#Scenario 11: Collider with Logistic Regression (Categorical var)

n <- 100
X <- rbinom(n, 1, 0.4)
Y <- rbinom(n, 1, 0.6)
pCol <- 1 / (1 + exp(-intX * X -intY * Y))
Col <- rbinom(n, 1, pCol)
summary(glm(Y ~ X, family = binomial))
summary(glm(Y ~ X + Col, family = binomial))

#Scenario 12: Mediator with Logistic Regression (Categorical var)

n <- 100
X <- rbinom(n, 1, 0.6)
pM <- 1 / (1 + exp(-intM - bM * X))
Med <- rbinom(n, 1, pM)
pY <- 1 / (1 + exp(-intY - bY * Med))
Y <- rbinom(n, 1, pY)
summary(glm(Y ~ X, family = binomial))
summary(glm(Y ~ X + Med, family = binomial))

