library(PerformanceAnalytics);library(zoo);library(tseries);
ticker <- "XLU"
start <- "1980-12-12"; end <- Sys.Date();
adjHist <- get.hist.quote(instrument = ticker, start, end, provider = 'yahoo', 
               quote = 'AdjClose', quiet = TRUE, retclass="zoo")
openHist  <- get.hist.quote(instrument = ticker, 
                            start, end, provider = 'yahoo', quote = 'Open',
                            quiet = TRUE, retclass="zoo")
closeHist  <- get.hist.quote(instrument = ticker, 
                            start, end, provider = 'yahoo', quote = 'Close',
                            quiet = TRUE, retclass="zoo")
all_prices <- merge(openHist, closeHist, adjHist)
colnames(all_prices) <- c("Open", "Close", "Adj")
all_returns = diff(log(all_prices))
head(all_returns)
head(all_prices)
all_returns$Intraday <- log(all_prices$Close/all_prices$Open)
all_returns$Intraday[1] <- NA
all_returns$Overnight <- all_returns$Adj - all_returns$Intraday
head(all_returns)
risk_retDF <- cbind(exp(apply(all_returns[-1,], 2, sd) * sqrt(252)) - 1, exp(apply(all_returns[-1,], 2, mean)*252) - 1)
rownames(risk_retDF) <- colnames(all_returns)
colnames(risk_retDF) <- c("Ann. S.D. (%)", "Ann. Return (%)")
rrLabelsX <- seq(0, round(max(risk_retDF[3:5,1]),1)+.10, .1)
rrLabelsY <- seq(round(min(risk_retDF[3:5,2]),1)-.1, round(max(risk_retDF[3:5,2]),1)+.1, .1)

plot(risk_retDF[3:5,], pch = 19, cex = 1.5, col = "red", xlim = c(min(rrLabelsX), max(rrLabelsX)), xlab = "Volatility \n (Annualized standard deviation)", xaxt = "n", ylim = c(min(rrLabelsY), max(rrLabelsY)), ylab = "Annualized Return", yaxt = "n", main = "Return vs. Risk", ps = 12, cex.main = 1.5, mar = c(4,4,0.5,0.5) + .1)
text(risk_retDF[3:5,], labels=rownames(risk_retDF)[3:5], cex= 1., pos=1)
axis(1, at = rrLabelsX, lab=paste0(rrLabelsX * 100 , " %"), las=TRUE)
axis(2, at = rrLabelsY, lab=paste0(rrLabelsY * 100, " %"), las=TRUE)

return_matrix = coredata(all_returns[-1,]);
colnames(return_matrix) <- colnames(all_returns)
boxplot(return_matrix[ ,4],outchar=T, main="Boxplot", col="slateblue1")
chart.Boxplot(all_returns[ , -c(1:2)])
