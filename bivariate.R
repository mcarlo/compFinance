# Standard deviations and correlation
sig_x = 0.10
sig_y = 0.05
rho_xy = -0.9
# Covariance between X and Y
sig_xy = rho_xy * sig_x * sig_y
# Covariance matrix
Sigma_xy = matrix(c(sig_x^2, sig_xy, sig_xy, sig_y^2), nrow = 2, ncol = 2)

# Load the mvtnorm package
library("mvtnorm");
# The covariance matrix (Sigma_xy) is still in your workspace

# Means
mu_x = 0.05
mu_y = 0.025

# Simulate 100 observations
set.seed(123)  # for reproducibility
xy_vals = rmvnorm(100, mean = c(mu_x, mu_y), sigma = Sigma_xy)
head(xy_vals)

# The simulated values (xy_vals) are still in your workspace

# Create scatterplot
plot(xy_vals[,1], xy_vals[,2], pch = 16, cex = 2, col = "blue", main = "Bivariate normal: rho = 0.9", xlab = "x", ylab = "y")
lines(abline(v = mu_x, h = mu_y))

# Means (mu_x, mu_y) and covariance matrix (Sigma_xy) are still in your workspace

# create scatterplot
plot(xy_vals[, 1], xy_vals[, 2], pch = 16, cex = 2, col = "blue", 
     main = "Bivariate normal: rho=0.9", xlab = "x", ylab = "y")
# Add lines
abline(h = mu_y, v = mu_x)

# Add line segments
segments(x0 = 0, y0 = -1e10, x1 = 0, y1 = 0, col="red")
segments(x0 = -1e10, y0 = 0, x1 = 0, y1 = 0, col="red")

# Compute joint probability
pmvnorm(lower=-Inf, upper=0, mean=c(mu_x, mu_y), sigma = Sigma_xy)[1]