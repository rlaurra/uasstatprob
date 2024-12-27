# Load necessary libraries
library(ggplot2)

# Step 1: Generate Simulated Data
set.seed(123) # For reproducibility
n <- 100 # Number of observations
x <- rnorm(n, mean = 50, sd = 10) # Independent variable
y <- 2 + 0.5 * x + rnorm(n, mean = 0, sd = 5) # Dependent variable

# Create a data frame
data <- data.frame(x = x, y = y)

# Step 2: Check Assumptions
# 2.1. Linearity Assumption
plot(data$x, data$y, main = "Scatterplot: Linearity Check", xlab = "X", ylab = "Y", pch = 19)
abline(lm(y ~ x, data = data), col = "blue", lwd = 2)

# 2.2. Normality of Residuals
model <- lm(y ~ x, data = data)
residuals <- model$residuals
hist(residuals, main = "Histogram: Residuals Normality Check", xlab = "Residuals", breaks = 10, col = "lightblue")

# 2.3. Homoscedasticity (Constant Variance)
plot(model$fitted.values, residuals, main = "Residuals vs Fitted: Homoscedasticity Check", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 19)
abline(h = 0, col = "red", lwd = 2)

# Step 3: Analysis
summary(model)

# Step 4: Visualization
# Scatterplot with regression line
scatter_plot <- ggplot(data, aes(x = x, y = y)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  labs(title = "Simple Linear Regression", x = "Independent Variable (X)", y = "Dependent Variable (Y)") +
  theme_minimal()

print(scatter_plot)

# Step 5: Interpretation
# Interpreting the regression summary
cat("Interpretation:\n")
cat("1. The coefficient for the independent variable (x) represents the expected change in the dependent variable (y) for a one-unit change in x.\n")
cat("2. The p-value for the x coefficient tests the null hypothesis that the coefficient is equal to 0. A small p-value (typically < 0.05) indicates a significant relationship.\n")
cat("3. The R-squared value indicates the proportion of variance in y explained by x.\n")
