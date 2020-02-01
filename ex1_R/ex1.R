library(ggplot2)

rm(list = ls())

# Part 1: Data Preparation ####
df <- data.frame(read.table("ex1data1.txt", sep = ","))
colnames(df) <- c("X", "y")
m <- nrow(df)
X <- cbind(rep(1, m), df[, 1]) # Add a column of ones to x
y <- df$y
# Part 2: Plotting ####
p <- ggplot(df, aes(X, y)) + geom_point()
plot(p)
# Some gradient descent settings
iterations = 1500
alpha = 0.01

# Part 3: Cost and Gradient descent ####
# Cost Function
computeCost <- function(X, y, theta) {
  
  hypo <- X %*% theta                        # hypothesis
  prediction <- hypo - y                    # Prediction
  J <- 1 / (2 * m) * sum (prediction ^ 2)  # Cost Function
  return(J)
  
}

# compute and display initial cost (for test)
theta <- matrix(rep(0, 2), nrow = 2, ncol = 1) # initialize fitting parameters

cat("With theta = c(-1, 2)\nCost computed = ",
             computeCost(X, y, theta ),". ",
      " \nExpected cost value (approx) 32.07.", sep = "")


# Gradient Descent
gradientDescent <- function(X, y, theta, alpha, iterations) 
{
  J_history <- matrix(rep(0, iterations), ncol = 1)
  
  for (iter in 1:iterations) 
   {
    
    hypo <- X %*% theta                         # hypothesis
    prediction <- hypo - y                    # Prediction
    gradient <- alpha * (1/m) * (t(X) %*% prediction)   # Gradient Descent
    theta = theta - gradient    # Update theta

   }
  
  return(theta)
  
}
theta <- gradientDescent(X, y, theta, alpha, iterations)

cat("\n\nTheta found by gradient descent:\n",
    gradientDescent(X, y, theta, alpha, iterations)[1], ", ",
    gradientDescent(X, y, theta, alpha, iterations)[2],
    " \nExpected theta values (approx):\n-3.6303, 1.1664", sep = "")

# Plot the  linear fit
prediction_line <- data.frame(X = X[, 2], y = X %*% theta)
plot(p  + geom_line(data = prediction_line, aes(X, y)) + theme())
  
# Predict values for population sizes of 35,000 and 70,000
predict1 = c(1, 3.5) %*% theta
cat(sprintf('For population = 35,000, we predict a profit of %f\n',
        predict1*10000))

predict2 = c(1, 7) %*% theta;
cat(sprintf('For population = 70,000, we predict a profit of %f\n',
        predict2*10000))

# Part 4: Visualizing J(theta_0, theta_1) ####
# Grid over which we will calculate J
theta0_vals <- seq(-10, 10, length.out = 100)
theta1_vals <- seq(-1, 4, length.out = 100)

# initialize J_vals to a matrix of 0's
J_vals <- matrix(0, length(theta0_vals), length(theta1_vals))

# Fill out J_vals
for (i in 1:length(theta0_vals)) 
{
  for (j in 1:length(theta1_vals))
  {
  
    t <- rbind(theta0_vals[i], theta1_vals[j])
    J_vals[i, j] <- computeCost(X, y, t)  
  }
}

# Because of the way meshgrids work in the surf command, we need to
# transpose J_vals before calling surf, or else the axes will be flipped
J_vals <- t(J_vals)

library(plotly)
three_d_plot <- plot_ly(showscale = TRUE) %>%
  add_surface(z = ~J_vals)

three_d_plot

# Contour plot
# Plot J_vals as 15 contours spaced logarithmically between 0.01 and 100
# logarithmic contours are denser near the center
logspace <- function(d1, d2, n) 
  {
  return(exp(log(10)*seq(d1, d2, length.out=n)))
}
    
contour(theta0_vals, theta1_vals, J_vals, levels = logspace(-2, 3, 20))

ggplot() + geom_contour(aes(theta0_vals, theta1_vals, z = density))
