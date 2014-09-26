options(digits=4, width=70)
library("zoo")
source(file="http://spark-public.s3.amazonaws.com/compfinance/R%20code/portfolio.r")

returns_lab = read.csv("http://spark-public.s3.amazonaws.com/compfinance/R%20code/lab9returns.csv", stringsAsFactors=F)

returns_df <- returns_lab[,-1]
rownames(returns_df) <- returns_lab$Date
head(returns_df)
tail(returns_df)

# Timeplots of stocks on seperate graphs
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}
plot(returns_df, lwd=2, panel=my.panel, col="blue")

# Timeplots of stocks on same graph
#par(mfcol = c(1,1))
plot(returns_df, plot.type = "single", main="Returns", col=1:4, lwd=2)
abline(h=0)
legend(x="bottomleft", legend=colnames(returns_df), col=1:4, lwd=2)

# All data is preloaded in your workspace.  Type 'ls()' in the console to see what has been loaded.

# Parameters CER model
mu_hat_month = apply(returns_df, 2, mean)
mu_hat_month
sigma2_month = apply(returns_df, 2, var)

sigma2_month
sigma_month = apply(returns_df, 2, sd)

sigma_month
cov_mat_month = var(returns_df)
cov_mat_month
cor_mat_month = cor(returns_df)

cor_mat_month

# Pairwise scatterplots
pairs(coredata(returns_df), col = "blue", pch = 16)

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# Calculate the global minimum variance portfolio
global_min_var_portfolio = globalMin.portfolio(mu_hat_month, cov_mat_month)
  
global_min_var_portfolio

# Plot the portfolio weights of our four stocks
barplot(global_min_var_portfolio$weights)

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# set restriction matrices
D_matrix = 2* cov_mat_month
D_matrix
d_vector = rep(0,4)
d_vector
A_matrix = cbind(rep(1,4),diag(4))
A_matrix
b_vector = c(1,rep(0,4))
b_vector

# use solve.QP to minimize portfolio variance
library(quadprog)
quad_prog = solve.QP(D_matrix, d_vector, A_matrix, b_vector, meq = 1)
  
quad_prog

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# The global minimum variance portfolio
global_min_var_portfolio  = globalMin.portfolio(mu_hat_month, cov_mat_month, shorts = F)
  
global_min_var_portfolio 

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# highest average return
mu_target = max(mu_hat_month)
  
# short sales allowed
efficient_porfolio_short = efficient.portfolio(mu_hat_month, cov_mat_month, mu_target)
    
efficient_porfolio_short
barplot(efficient_porfolio_short$weights)
  
# no short sales allowed
efficient_porfolio_no_short = efficient.portfolio(mu_hat_month, cov_mat_month, mu_target, shorts = F)
    
efficient_porfolio_no_short
barplot(efficient_porfolio_no_short$weights)

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# The efficient frontier of risky assets 
efficient_frontier = efficient.frontier(er = mu_hat_month, cov = cov_mat_month, shorts = F, nport=21, alpha.min=-1.0, alpha.max=1.0)
  
summary(efficient_frontier)

# The plot
plot(efficient_frontier, plot.assets=TRUE, col="blue", lwd=2)  

# All data and CER parameters are preloaded in your workspace.  Type 'ls()' in the console to see them.

# risk free rate
t_bill_rate = 0.005

# Tangency portfolio short sales allowed
tangency_portfolio_short = tangency.portfolio(mu_hat_month, cov_mat_month, risk.free = t_bill_rate, shorts = TRUE)
  
summary(tangency_portfolio_short)
#plot
barplot(tangency_portfolio_short$weights)

# Tangency portfolio short sales not allowed
tangency_portfolio_no_short = tangency.portfolio(mu_hat_month, cov_mat_month, risk.free = t_bill_rate, shorts = F)
  
tangency_portfolio_no_short$weights
#plot
barplot(tangency_portfolio_no_short$weights)
