#setwd("~/Serafin_Documents/Cornell/Spring_2020/Opt_modeling_for_Finance/Project/ORIE5370-Project-master") #Tom S - PLEASE DO NOT DELETE
library(timeSeries)
library(fPortfolio)
library(quantmod)
library(caTools)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
library(corrplot)


# First I am testing on stocks to set up a base framework. 
# Next factors 

# --- Creating Returns from Time Series

# Sample list of tickers 
tickers <- c("AXP", "WFC", "C")

# Calcualting Daily Reutnrs
portfolioPrices <- NULL # creating empty object 
for (Ticker in tickers)
  portfolioPrices <- cbind(portfolioPrices, getSymbols.yahoo(Ticker, from ="2016-01-01", auto.assign=FALSE)[,4])

# Deleting any dates in the time series with no prices
portfolioPrices <- portfolioPrices[apply(portfolioPrices, 1, function(x) all(!is.na(x))),]

# Renaming columns 
colnames(portfolioPrices) <- tickers

# Calculating Returns below: Daily RoC
portfolioReturns <- na.omit(ROC(portfolioPrices, type="discrete"))
portfolioReturns <- as.timeSeries(portfolioReturns)

# Now calculating monthly returns (change it to weekly below in the lapply function with you want weekly or something else)
stock_data <- tickers %>% lapply(function(x) getSymbols.yahoo(x, from="2016-01-01", auto.assign=FALSE)[,4]) %>%
  lapply(function(x) monthlyReturn(x))

portfolioReturns <- do.call(merge, stock_data)

# keeping only the dates that have closing prices for all tickers 
portfolioReturns <- portfolioReturns[apply(portfolioReturns,1,function(x) all(!is.na(x))),]
colnames(portfolioReturns) <- tickers

# ----- Now calculating and plotting the frontier & efficeint portfolios
portfolioReturns <- as.timeSeries(portfolioReturns) # can not be an xts object!! 
effFrontier <- portfolioFrontier(portfolioReturns, constraints = "Longonly")
head(effFrontier)

# Here are the options in the frontier ...
# 1: Plot Efficient froniter
# 2: Plot Minimum Variance Portfolio
# 3: Plot Tangency Portfolio
# 4: Plot Risk Returns of Each Asset
# 5: Plot Equal Weights Portfolio
# 6: Plot Two Asset Frontiers (Long)
# 7: Plot Monte Carlo Portfolios
# 8: Plot Sharpe Ratio 

plot(effFrontier, c(1, 2, 3, 4, 5, 6, 7, 8))

# Plot Froniter weights (can Adjust Number of Points)
# WEIGHTS 
frontierweights <- getWeights(effFrontier) # getting allocations for each instrument for each point on the efficient froniter
head(frontierweights)

colnames(frontierweights) <- tickers
risk_return <- frontierPoints(effFrontier)
head(risk_return)
#write.csv(risk_return, "risk_return.csv")  #testing, note to self - you want to output to a csv 

# ------- Correlation and covariance matrix ! !  ------
cor_matrix <- cor(portfolioReturns) # correaltion matrix
cov_matrix <- cov(portfolioReturns) # covariance matrix 

#plotting! - attach this to report 
corrplot(cor_matrix, type = 'upper', method = 'number', tl.cex = 0.9)

# **** Annualizing my Data ***** 
riskReturnPoints <-frontierPoints(effFrontier) # getting my Risk and return values for the points on the efficient frontier 
# below creating my data frame 
annualizedPoints <- data.frame(targetRisk=riskReturnPoints[, "targetRisk"] * sqrt(252),
                               targetReturn = riskReturnPoints[,"targetReturn"] * 252)

plot(annualizedPoints)

# plotting sharpe ratios for each point on the efficient froniter
riskfreerate <- 0
plot((annualizedPoints[,"targetReturn"]-riskfreerate) / annualizedPoints[,"targetRisk"], xlab="point on efficient frontier", ylab = "Sharpe ratios")

# Plotting Frontier weights (need to tranpose matrix first, doing this right below) Remember that these are not the optimal weights 
barplot(t(frontierweights), main="Frontier weights", col=cm.colors(ncol(frontierweights)+2), legend=colnames(frontierweights))

# Now I am getting the minimum variance portfolio, tangency portfolio, etc. 
mvp <- minvariancePortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
summary(mvp)
mvp

tangencyPort <- tangencyPortfolio(portfolioReturns, spec=portfolioSpec(), constraints="LongOnly")
tangencyPort

mvpweights <- getWeights(mvp) # stupid easy 
mvpweights 
pie(mvpweights)  #potential add this to report 

tangencyweights <- getWeights(tangencyPort)
tangencyweights 
pie(tangencyweights)

# extract value at risk just doing for fun - not required for Project
covRisk(portfolioReturns, mvpweights)
varRisk(portfolioReturns, mvpweights, alpha=0.05)
cvarRisk(portfolioReturns, mvpweights, alpha = 0.05)

 # graphs! this is where the real fun begins
# plot MVP weight
barplot(mvpweights, main="Minimum variance portfolio weights", xlab="Asset", ylab = "Weight in Portfolio (%)",col=cm.colors(ncol(frontierweights)+2), legend=colnames(frontierweights))
pie(mvpweights, col=cm.colors(ncol(frontierweights)+2))

# ggplot! MVP weights, give a second to run
df <- data.frame(mvpweights)
assets <- colnames(frontierweights)
ggplot(data=df, aes(x = assets, y = mvpweights, fill = assets)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  geom_text(aes(label=sprintf("%.02f %%", mvpweights*100)),
            position=position_dodge(width = 0.9), vjust=-.025, check_overlap = TRUE) + 
              ggtitle("Minimum Variance Portfolio Optimal Weights")+ theme(plot.title = element_text(hjust = 0.5)) + 
                labs(x= "Assets", y = "weights (%)")

# Juicy plot! Enjoy dude 
dft <- data.frame(tangencyweights)
assets <- colnames(frontierweights)
ggplot(data=dft, aes(x=assets, y=tangencyweights, fill=assets)) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black") + 
  geom_text(aes(label=sprintf("%.02f %%", tangencyweights*100)),
            position=position_dodge(width = 0.9), vjust=-0.25, check_overlap = TRUE) +
  ggtitle("Tangency Portfolio Weights") +theme(plot.title = element_text(hjust=0.5)) + 
  labs(x="Assets", y = "weights (%")

# Piece chart doing for min variance portfolio Weights 
#beautiful! ggplot Pie chart. Enjoy 
bar <- ggplot(df, aes(x = "", y= mvpweights, fill=assets)) + geom_bar(width = 1, stat = "identity") + ggtitle("Minimum Variance Portfolio Weights")
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette = "Blues") + 
  theme_minimal()


# Piece chart doing for tan variance portfolio Weights 
bar <- ggplot(df, aes(x = "", y= tangencyweights, fill=assets)) + geom_bar(width = 1, stat = "identity") + ggtitle("Tangency Portfolio Weights")
pie <- bar + coord_polar("y", start=0)
pie + scale_fill_brewer(palette = "Blues") + 
  theme_minimal()

# ----------------------------------------------------------------
# Wworking with constraints 
# ----------------------------------------------------------------
# Example constarints
# "minw[asset]=percentage" for box constraints resp
# "maxsumw[assets] = percentage" for sector constraints 
# eqsumconstraints(data, spec=portfoliospec(), constarints="LongONly")

# set specs
Spec <- portfolioSpec()
setSolver(Spec) = "solveRShortExact"
setTargetRisk(Spec) = 0.12
constraints <- c("minw[1:length(tickers)]=-1","maxw[1:length(tickers)]=.60","Short")

effFrontierShort <- portfolioFrontier(portfolioReturns, Spec, constraints = constraints)
weights <- getWeights(effFrontierShort)











































