# Assign the URL to the CSV file
data_url = "http://faculty.washington.edu/ezivot/econ424/sbuxPrices.csv"
# Load the data frame using read.csv
sbux_df = read.csv(file = data_url, stringsAsFactors = F, header = T)
# sbux_df should be a data frame object. Data frames are rectangular data objects that typically contain observations in rows and variables in columns

# The sbux_df data frame is already loaded in your work space
str(sbux_df)
head(sbux_df)
tail(sbux_df)
class(sbux_df$Date)

closing_prices = sbux_df[,"Adj.Close", drop = F]
index_1 <- which(sbux_df$Date == "3/1/1994")
index_2 <- which(sbux_df$Date == "3/1/1995")
some_prices <- sbux_df[index_1:index_2,"Adj.Close"]

# The sbux_df data frame is already loaded in your work space

# Create a new data frame that contains the price data with the dates as the row names
sbux_prices_df = sbux_df[, "Adj.Close", drop=FALSE]
rownames(sbux_prices_df) = sbux_df$Date
head(sbux_prices_df)

# With Dates as rownames, you can subset directly on the dates.
# Find indices associated with the dates 3/1/1994 and 3/1/1995.

price_1 <- sbux_prices_df["3/1/1994", 1]
price_2 <- sbux_prices_df["3/1/1995", 1]

plot(sbux_df$Adj.Close, type = "l", col = "blue", lwd = 2, 
     main = "Monthly closing price of SBUX", ylab = "Adjusted Close")
legend(x='topleft',legend='SBUX', lty=1, lwd=2, col='blue')

# The sbux_df data frame is already loaded in your work space
sbux_prices_df = sbux_df[, "Adj.Close", drop=FALSE]

# Denote n the number of time periods
n = nrow(sbux_prices_df)
sbux_ret = (sbux_prices_df[2:n,1] - sbux_prices_df[1:(n-1),1])/
  sbux_prices_df[1:(n-1),1]
class(sbux_ret)

names(sbux_ret) <- sbux_df[2:(n-1),1]
str(sbux_df)
str(sbux_prices_df)
  # Notice that sbux_ret is not a data frame object
  class(sbux_ret)

sbux_ccret  <- log(1+sbux_ret)

sbux_ccret2 = log(sbux_prices_df[2:n,1]) - log(sbux_prices_df[1:(n-1),1])
names(sbux_ccret2) <- names(sbux_ccret)
sum(abs(sbux_ccret - sbux_ccret2) > 1e-13)
sum(abs(sbux_ccret - sbux_ccret2) <= 1e-5)

head(cbind(sbux_ret, sbux_ccret))

# The simple returns (sbux_ret) and the continuously compounded returns (sbux_ccret) have been preloaded in your workspace

# Plot the returns on the same graph
plot(sbux_ret, type="l", col="blue", lwd=2, ylab="Return",
     main="Monthly Returns on SBUX")

# Add horizontal line at zero
abline(h=0)

# Add a legend
legend(x="bottomright", legend=c("Simple", "CC"), 
       lty=1, lwd=2, col=c("blue","red"))

# Add the continuously compounded returns
lines(sbux_ccret, col  = "red", lwd = 2)

# The simple returns (sbux_ret) and the continuously compounded returns (sbux_ccret) have been preloaded in your workspace

# Compute gross returns
sbux_gret = sbux_ret + 1

# Compute future values
sbux_fv = cumprod(sbux_gret)

# Plot the evolution of the $1 invested in SBUX as a function of time
plot(sbux_fv, type="l", col="blue", lwd=2, ylab="Dollars", 
     main="FV of $1 invested in SBUX")

(27/31.18)^12 - 1
log(27/31.18) 
(30.51/31.18) - 1

mu_X <- .05
sigma_X <- .10
pnorm(-.10, mean = mu_X, sd = sigma_X)
