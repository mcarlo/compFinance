# Load relevant packages
library("PerformanceAnalytics")
library("zoo")

# Load the data
rm(returns_df)
load(url("http://s3.amazonaws.com/assets.datacamp.com/course/compfin/lab8.RData"))

# Explore the data set
head(returns_df)
tail(returns_df)

# The returns_df data is preloaded in your workspace

# Estimate the parameters: multivariate
mu_hat_annual = apply(returns_df,2,mean)*12   
sigma2_annual = apply(returns_df,2,var)*12
sigma_annual = sqrt(sigma2_annual)
cov_mat_annual = cov(returns_df)*12 
cov_hat_annual = cov(returns_df)[1,2]*12    
rho_hat_annual = cor(returns_df)[1,2]

# The annual estimates of the CER model parameters for Boeing and Microsoft
mu_boeing = mu_hat_annual["rboeing"]
mu_msft =  mu_hat_annual["rmsft"]
sigma2_boeing = sigma2_annual["rboeing"]
sigma2_msft =  sigma2_annual["rmsft"]
sigma_boeing = sigma_annual["rboeing"]
sigma_msft = sigma_annual["rmsft"]
sigma_boeing_msft = cov_hat_annual
rho_boeing_msft = rho_hat_annual

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# The ratio Boeing stock vs Microsoft stock (adds up to 1)
boeing_weights = seq(from=-1, to=2, by=0.1)
msft_weights = 1 - boeing_weights

# Portfolio parameters
mu_portfolio = (cbind(boeing_weights, msft_weights) %*% c(mu_boeing, mu_msft))[,1]

sigma2_portfolio = diag(cbind(boeing_weights, msft_weights) %*% (cov_mat_annual %*% t(cbind(boeing_weights, msft_weights))))
  

sigma_portfolio = sqrt(sigma2_portfolio)

# Plotting the different portfolios
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="Microsoft", pos=4)

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# Annual risk-free rate of 3% per year for the T-bill
t_bill_rate = .03

# Ratio Boeing stocks
boeing_weights = seq(from=-1, to=2, by=0.1)

# Portfolio parameters
mu_portfolio_boeing_bill = .03*(1 - boeing_weights) + mu_boeing * boeing_weights

sigma_portfolio_boeing_bill = boeing_weights * sigma_boeing

# Plot previous exercise
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="MSFT", pos=4)

# Portfolio Combination Boeing and T-bills
points(sigma_portfolio_boeing_bill, mu_portfolio_boeing_bill, type="b", col="blue")

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# Sharp ratio Boeing
sharp_ratio_boeing = (mu_boeing - .03)/sigma_boeing

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# The global minimum variance portfolio
global_min_var_portfolio = globalMin.portfolio(mu_hat_annual, cov_mat_annual)

global_min_var_portfolio

# Summary of global_min_var_portfolio that takes into account the annual risk-free rate of 3% per year
summary.portfolio(global_min_var_portfolio, risk.free = .03)
  
# Portfolio weights Boeing and Microsoft
barplot(global_min_var_portfolio$weights)
  
# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="MSFT", pos=4)
  
# Plot the position of the global minimum variance portfolio
text(x=global_min_var_portfolio$sd, y=global_min_var_portfolio$er, labels="Global min", pos=2)

t(global_min_var_portfolio$weights) %*% c(mu_boeing, mu_msft)

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# The tangency portfolio
tangency_portfolio =  tangency.portfolio(mu_hat_annual, cov_mat_annual, risk.free = .03)
  
tangency_portfolio

# Summary of tangency_portfolio with annual risk free rate of 3%
mu_tan <- summary.portfolio(tangency_portfolio, risk.free = .03)$er
sig_tan <- summary.portfolio(tangency_portfolio, risk.free = .03)$sd


# Portfolio weights Boeing and Microsoft
barplot(tangency_portfolio$weights)

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# Annual risk-free rate of 3% per year for the T-bill
t_bill_rate = 0.03

# Set of tangency portfolio weights
tangency_weights = seq(from=0, to=2, by=0.1)

# Portfolio parameters
mu_portfolio_tangency_bill = tangency_weights * tangency_portfolio$er +
  (1 - tangency_weights) * .03
  
sigma_portfolio_tangency_bill = tangency_weights * tangency_portfolio$sd
  
# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="MSFT", pos=4)

# Plot portfolio combinations of tangency portfolio and T-bills
text(x=tangency_portfolio$sd, y=tangency_portfolio$er, labels="Tangency", pos=2)
points(sigma_portfolio_tangency_bill, mu_portfolio_tangency_bill, col = "blue", type = "b", pch = 16)

# All data and CER parameters are preloaded in your workspace.
# Type "ls()" in the console to see them.

# Define the portfolio ratio's
tangency_weight = .3
t_bill_weight = 1 - tangency_weight
  
# Define the portfolio parameters
mu_portfolio_efficient = tangency_weight * tangency_portfolio$er +
  (1 - tangency_weight) * .03
  
sd_portfolio_efficient = tangency_weight * tangency_portfolio$sd
  
# Plot previous exercises
plot(sigma_portfolio, mu_portfolio, type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="MSFT", pos=4)
text(x=tangency_portfolio$sd, y=tangency_portfolio$er, labels="Tangency", pos=2)
points(sigma_portfolio_tangency_bill, mu_portfolio_tangency_bill, type="b", col="blue", pch=16)

# Plot Efficient Portfolio with 30% Tangency
points(sigma_portfolio_efficient, mu_portfolio_efficient, col = "orange", type = "b", pch = 16, cex = 2)

text(x=sd_portfolio_efficient, y=mu_portfolio_efficient, labels="Efficient Portfolio with 30% Tangency", pos=4, cex=0.75)

# All data and CER parameters are preloaded in your workspace. 
# Type "ls()" in the console to see them. 

# Calculate the weight of the tangency portfolio in the portfolio
tangency_weight  = sigma_boeing/tangency_portfolio$sd
  
# Calculate the portfolio parameters
mu_portfolio_efficient = .03 + tangency_weight*(tangency_portfolio$er - .03)
  
sd_portfolio_efficient = tangency_weight*tangency_portfolio$sd
  
# Plot previous exercises
plot(sigma_portfolio, mu_portfolio,bg="NA", type="b", pch=16, ylim=c(0, max(mu_portfolio)), xlim=c(0, max(sigma_portfolio)), xlab=expression(sigma[p]), ylab=expression(mu[p]),col=c(rep("green", 18), rep("red", 13)))
text(x=sigma_boeing, y=mu_boeing, labels="Boeing", pos=4)
text(x=sigma_msft, y=mu_msft, labels="MSFT", pos=4)
text(x=tangency_portfolio$sd, y=tangency_portfolio$er, labels="Tangency", pos=2)
points(sigma_portfolio_tangency_bill, mu_portfolio_tangency_bill, type="b", col="blue", pch=16)

# Plot Efficient Portfolio with the same risk as Boeing
points(sd_portfolio_efficient, mu_portfolio_efficient, type="p", col="orange", pch=16, cex=2)
text(x=sd_portfolio_efficient, y=mu_portfolio_efficient, labels="Efficient Portfolio with same risk as Boeing", pos=2, cex=0.75)
