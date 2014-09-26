set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1_sim = arima.sim(model = list(ma=0.5), n = 250, mean = 0, sd = 0.1) + .05

plot(ma1_sim, main = "MA(1) Process: mu=0.05, theta=0.5", type = "l", xlab = 
       "time", ylab = "y(t)")
lines(abline(h = 0))

# Use the ARMAacf function to calculate the theoretical autocorrelations up to lag 10. Assign the result to acf_ma1_model.
set.seed(123);
# Simulate 250 observations from the described MA(1) model
ma1_sim = arima.sim(model=list(ma=0.5), n=250, mean=0, sd=0.1) + 0.05;
# Generate the theoretical ACF with upto lag 10
acf_ma1_model <- ARMAacf(ma = 0.5, lag.max = 10)

# Split plotting window in three rows
par(mfrow=c(3,1))
# First plot: The simulated observations
plot(ma1_sim, type="l",main="MA(1) Process: mu=0.05, theta=0.5",xlab="time",ylab="y(t)")
abline(h=0)
# Second plot: Theoretical ACF
plot(1:10, acf_ma1_model[2:11], type="h", col="blue",  ylab="ACF", main="theoretical ACF")
# Third plot: Sample ACF
tmp = acf(ma1_sim, lag.max = 10)
# Reset graphical window to only one graph
par(mfrow=c(1,1))

rho1 <- function(rho){rho/(1 + rho^2)}
rho1(.9)
# Use the ARMAacf function to calculate the theoretical autocorrelations up to lag 10. Assign the result to acf_ma1_model.
set.seed(123);
rho <- .9
# Simulate 250 observations from the described MA(1) model
ma1_sim = arima.sim(model=list(ma=0.9), n=250, mean=0, sd=0.1) + 0.05;
# Generate the theoretical ACF with upto lag 10
acf_ma1_model <- ARMAacf(ma = 0.9, lag.max = 10)

# Split plotting window in three rows
par(mfrow=c(3,1))
# First plot: The simulated observations
plot(ma1_sim, type="l",main="MA(1) Process: mu=0.05, theta=0.9",xlab="time",ylab="y(t)")
abline(h=0)
# Second plot: Theoretical ACF
plot(1:10, acf_ma1_model[2:11], type="h", col="blue",  ylab="ACF", main="theoretical ACF")
# Third plot: Sample ACF
tmp = acf(ma1_sim, lag.max = 10)
# Reset graphical window to only one graph
par(mfrow=c(1,1))

set.seed(123);
# Simulate 250 observations from the described AR(1) model
ar1_sim = arima.sim(model=list(ar=0.5), n=250, mean=0, sd=0.1) + 0.05;
# Generate the theoretical ACF with ten lags
acf_ar1_model = ARMAacf(ar = 0.5, lag.max = 10)

# Split plotting window in three rows
par(mfrow=c(3,1))
# Generate the same three graphs as in the previous exercise 
# First plot: The simulated observations
plot(ar1_sim, type="l", main="AR(1) Process: mu=0.05, phi=0.5",xlab="time",ylab="y(t)")
abline(h=0)
# Second plot: Theoretical AFC
plot(1:10, acf_ar1_model[2:11], type="h", col="blue", main="theoretical ACF")
# Third plot: Sample AFC
tmp = acf(ar1_sim, lag.max = 10)
# Reset plotting window to default
par(mfrow=c(1,1));
