# Equity Derivatives Project 
# Volatility Forcasting Project 
# 5 initial macroeconomic variables 
# There is a lot in this workbook that is scratch or exploratory code. There are some areas of the code that are finshed and some that were only done for exploration. 
setwd("/Users/thomasserafin/Serafin_Documents/Cornell/Fall 2020/ORIE_5254 - Equity Derv **/Project") 
library(readxl) 
library(dplyr)
library(tidyverse)
library(boot)
library(GGally)
library(leaps)
library(glmnet) # for ridge and lasso
library(splines)
library(gam)
library(ggplot2)
library(ggpubr)
library(factoextra)

#  ---- Notes --- 
# I am doing a 20 year horizon which includes "3 market cycles" 


# -------- loading data ------ 

# Loading VIX vol data 
Vix_Prices<- read.csv("Data/VIX_Data.csv")
Vix_Prices <- Vix_Prices %>% arrange(desc(DATE))
Vix_Prices <- Vix_Prices[1:240,]
colnames(Vix_Prices)[2] <- "Price" # Renaming the column 

# ** Variable 1 - 10yr US rate from Fred
US_Treasury_10yr <- read.csv("Data/Fred_Data_T10Y.csv")
US_Treasury_10yr <- US_Treasury_10yr %>% arrange(desc(DATE))
colnames(US_Treasury_10yr)[2] <- "Rate" # Renaming the column

# ** Variable 2 - 
# Changed from an index to US Unemployment rate gathered from FRED 
unemployment_rate<- read.csv("Data/US_Unemployment_Rate.csv")
unemployment_rate <- unemployment_rate %>% arrange(desc(DATE))

# ** Variable 3 - 
# Changed from an index to Effective Federal Funds Rate 
fed_funds_rate <- read.csv("Data/US_Fed_Funds_Rate.csv")
fed_funds_rate <- fed_funds_rate %>% arrange(desc(DATE))

# ** Variable 4 - Spread b/t 10yr & 2yr US rate from Fred
Spread_10_2yr <- read.csv("Data/Fred_Data_T10Y2Y.csv")
colnames(Spread_10_2yr)[2] <- "Spread" # Renaming the column
Spread_10_2yr$DATE <- rev(Vix_Prices$DATE) # easier way of organzing the date column. Using Vix dates which are the same
Spread_10_2yr <- Spread_10_2yr %>% arrange(desc(DATE))

# ** Variable 5 - Monthly Brent Prices 
# from EIA.Gov
Brent_Prices <- read_excel("Data/Brent_Prices.xls", 2, skip = 2)
Brent_Prices <- Brent_Prices %>% arrange(desc(Date)) # arrrange by decreasing order for Date 
Brent_Prices$Date <- format(as.Date(Brent_Prices$Date, format ="%B %d, %Y")) # Reformat the Date Column 
Brent_Prices$YEAR<-format(as.Date(Brent_Prices$Date, format="%Y"), "%Y")
colnames(Brent_Prices)[2] <- "Price" # Renaming the column 

ggplot(Brent_Prices, aes(x=YEAR, y=Price, group=1)) + stat_summary(fun="mean", geom="line", col="navy") + 
  ggtitle("Average Brent Price Over Years") + ylab("Price") + xlab("Year") +
  theme(axis.text.x = element_text(angle = 40)) 

Brent_Prices <- Brent_Prices[1:240,] # Creating a 20 year horizon for analysis 
# check this later - https://www.kaggle.com/andreykolysh/oil-prices-predict

# -------- Data Analysis ------ 
# adding every variable to the same dataframe 
Vol.df <- cbind.data.frame(Vix_Prices, 
                           Brent_Price = Brent_Prices$Price, 
                           Fed_Funds_Rate = fed_funds_rate$FEDFUNDS, # already in % format 
                           Spread_10_2yr = Spread_10_2yr$Spread, 
                           Unemployment_Rate = unemployment_rate$UNRATE,  # already in % format 
                           US_Treasury_10yr_rate = US_Treasury_10yr$Rate)

colnames(Vol.df)[2] <- "Vix"

Vol.df.2 <- Vol.df
row.names(Vol.df.2) <- Vol.df.2$DATE
Vol.df.2 <- Vol.df.2 %>% select(-DATE)

qqplot(Vol.df$Brent_Price, Vol.df$Vix) # to use for presentation 
qqplot(Vol.df$Spread_10_2yr, Vol.df$Vix) # to use for presentation 
hist(Vol.df$Vix, col="skyblue") # distribution of outcomes. Add this to my presentaion 
hist <- hist(Vol.df$Vix, breaks = 20, col="skyblue", xlab = "Vix", main = "Distribution of Outcomes for Vix") # add this to my presentation 

mean(Vol.df$Vix)
# [1] 19.80839

mode(Vol.df$Vix)

# ** how good is 1 predictor for Vix? ** 
ggplot(Vol.df, aes(x=Brent_Price, y=Vix)) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE) + 
  labs(title = "Vix vs. Brent Price Linear Regression") +
  xlab("Brent Price") +
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90,100,110,120,130,140))

ggplot(Vol.df, aes(x=Spread_10_2yr, y=Vix)) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE) + 
  labs(title = "Vix vs. 10/2yr Spread Linear Regression") + 
  xlab("10/2yr Spread") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(-0.25, 0,0.25,0.5,0.75,1.0, 1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0))

ggplot(Vol.df, aes(x=Unemployment_Rate, y=Vix)) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE)  + 
  labs(title = "Vix vs. Unenployment Linear Regression") + 
  xlab("Unemployment Rate") +
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) + 
  scale_x_continuous(breaks = c(3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0))

ggplot(Vol.df, aes(x=US_Treasury_10yr_rate, y=Vix)) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE)  + 
  labs(title = "Vix vs. 10year Treasury Linear Regression") + 
  xlab("US 10yr Rate") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(0, 1.0, 1.5, 2.0,2.5,3.0,3.5,4.0, 4.5, 5.0, 5.5, 6.0))

ggplot(Vol.df, aes(x=Fed_Funds_Rate, y=Vix)) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE)  + 
  labs(title = "Vix vs. Fed Funds Rate Linear Regression") + 
  xlab("Fed Funds Rate") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0))

# plotting quantiles - idea of how to exploit trading 
# ----------  potential idea for a trading strat below  ----------------- 
ggplot(Vol.df, aes(x=Spread_10_2yr, y=Vix)) + geom_point() + 
  #geom_smooth(method = lm, color = "red", formula = y ~ poly(x, 4), se= FALSE) + 
  geom_quantile(color = "red", quantiles = c(0.75, 0.25), size =2, alpha = 0.6) + # plotting the percentiles 
  geom_quantile(color = "blue", quantiles = c(0.5), size = 2, alpha = 0.6) + 
  labs(title = "Vix vs. 10/2yr Spread Linear Regression") + 
  xlab("10/2yr Spread") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(-0.25, 0,0.25,0.5,0.75,1.0, 1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0))
# -----------------------------------------------------------------------

# linear regressions on a shorter time horizon 

ggplot(Vol.df[1:24,], aes(x=Spread_10_2yr[1:24], y=Vix[1:24])) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE) + 
  labs(title = "Vix vs. 10/2yr Spread Linear Regression - 2 year horizon") + 
  xlab("10/2yr Spread") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(-0.25, 0,0.1,0.2,0.3,0.4,0.5,0.6, 0.7, 0.8,1.0))

ggplot(Vol.df[1:24,], aes(x=US_Treasury_10yr_rate[1:24], y=Vix[1:24])) + geom_point() + 
  stat_smooth(method="lm", color="red", se = FALSE)  + 
  labs(title = "Vix vs. 10year Treasury Linear Regressio - 2 year horizon") + 
  xlab("US 10yr Rate") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(0, 1.0, 1.5, 2.0,2.5,3.0))

ggplot(Vol.df[1:24,], aes(x=US_Treasury_10yr_rate[1:24], y=Vix[1:24])) + geom_point() + 
  geom_smooth(span=1.6, se = FALSE) 

# --- correlations below --- 

cor(Vol.df$Vix, Vol.df$Brent_Price) # correlation over a 20 yr horizon 
cor(Vol.df$Vix, Vol.df$Spread_10_2yr)  # correlation over a 20 yr horizon 
cor(Vol.df$Vix, Vol.df$Unemployment_Rate) # correlation over a 20 yr horizon 
cor(Vol.df$Vix, Vol.df$US_Treasury_10yr_rate)  # correlation over a 20 yr horizon 
cor(Vol.df$Vix, Vol.df$Fed_Funds_Rate)  # correlation over a 20 yr horizon 

cor(Vol.df$Vix[1:120], Vol.df$Brent_Price[1:120]) # correlation over a 10 yr horizon 
cor(Vol.df$Vix[1:120], Vol.df$Spread_10_2yr[1:120])  # correlation over a 10 yr horizon 
cor(Vol.df$Vix[1:120], Vol.df$Unemployment_Rate[1:120]) # correlation over a 10 yr horizon 
cor(Vol.df$Vix[1:120], Vol.df$US_Treasury_10yr_rate[1:120])  # correlation over a 10 yr horizon 
cor(Vol.df$Vix[1:120], Vol.df$Fed_Funds_Rate[1:120])  # correlation over a 10 yr horizon 

cor(Vol.df$Vix[1:60], Vol.df$Brent_Price[1:60]) # correlation over a 5 yr horizon 
cor(Vol.df$Vix[1:60], Vol.df$Spread_10_2yr[1:60])  # correlation over a 5 yr horizon 
cor(Vol.df$Vix[1:60], Vol.df$Unemployment_Rate[1:60]) # correlation over a 5 yr horizon 
cor(Vol.df$Vix[1:60], Vol.df$US_Treasury_10yr_rate[1:60])  # correlation over a 5 yr horizon 
cor(Vol.df$Vix[1:60], Vol.df$Fed_Funds_Rate[1:60])  # correlation over a 5 yr horizon 


cor(Vol.df$Vix[1:24], Vol.df$Brent_Price[1:24]) # correlation over a 2 yr horizon 
cor(Vol.df$Vix[1:24], Vol.df$Spread_10_2yr[1:24])  # correlation over a 2 yr horizon 
cor(Vol.df$Vix[1:24], Vol.df$Unemployment_Rate[1:24]) # correlation over a 2 yr horizon 
cor(Vol.df$Vix[1:24], Vol.df$US_Treasury_10yr_rate[1:24])  # correlation over a 2 yr horizon 
cor(Vol.df$Vix[1:24], Vol.df$Fed_Funds_Rate[1:24])  # correlation over a 2 yr horizon 

cor(Vol.df$Vix[1:12], Vol.df$Brent_Price[1:12]) # correlation over a 1 yr horizon 
cor(Vol.df$Vix[1:12], Vol.df$Spread_10_2yr[1:12])  # correlation over a 1 yr horizon 
cor(Vol.df$Vix[1:12], Vol.df$Unemployment_Rate[1:12]) # correlation over a 1 yr horizon 
cor(Vol.df$Vix[1:12], Vol.df$US_Treasury_10yr_rate[1:12])  # correlation over a 1 yr horizon 
cor(Vol.df$Vix[1:12], Vol.df$Fed_Funds_Rate[1:12])  # correlation over a 1 yr horizon 


# ** Linear regression with one variable ** 
Vol_LM_01 <- lm(formula = Vix~Brent_Price, data =Vol.df)
summary(Vol_LM_01) # Adjusted R-squared:  0.01559 
Vol_LM_02 <- lm(formula = Vix~Fed_Funds_Rate, data =Vol.df)
summary(Vol_LM_02) # Adjusted R-squared:  0.0152 
Vol_LM_03 <- lm(formula = Vix~Spread_10_2yr, data =Vol.df)
summary(Vol_LM_03) # Adjusted R-squared:  0.07046 
Vol_LM_04 <- lm(formula = Vix~Unemployment_Rate, data =Vol.df)
summary(Vol_LM_04) # Adjusted R-squared:  -0.0001169 
Vol_LM_05 <- lm(formula = Vix~US_Treasury_10yr_rate, data =Vol.df)
summary(Vol_LM_05) # Adjusted R-squared:  -0.004191 

# ** Now will transform the ones with highest R-squared 
Vol_LM_01_transformed <- lm(formula = Vix~poly(Brent_Price,3), data =Vol.df)
summary(Vol_LM_01_transformed) # Adjusted R-squared:  0.07

# Testing a few transformations on the 10/2 Spread
# ** Now will transform the ones with highest R-squared 
Vix_Spread_pred_test1 <- lm(Vix~ns(Spread_10_2yr,10), data =Vol.df)
summary(Vix_Spread_pred_test1) # Adjusted R-squared:  0.15

Vix_Spread_pred_test2 <- gam(Vix~s(Spread_10_2yr,3), data =Vol.df)
summary(Vix_Spread_pred_test2) # Adjusted R-squared:  0.15

# now removing outliers for Vol_LM_01_transformed
Vol.df.outliers_removed <- subset(Vol.df, abs(rstudent(Vol_LM_01_transformed)) <= 3.0) # test 

Vol_LM_01_transformed.new <- lm(Vix~poly(Brent_Price,3), data=Vol.df.outliers_removed)
summary(Vol_LM_01_transformed.new)$adj.r.squared # Adjusted R-squared 0.2 # best one 

Vol_LM_02_transformed.new <- lm(Vix~poly(Spread_10_2yr,3), data=Vol.df.outliers_removed)
summary(Vol_LM_02_transformed.new)$adj.r.squared # Adjusted R-squared 0.2 # best one 



# ** multi-linear regression ** 

# Best Subset selection 
regfit.full <- regsubsets(Vix~Brent_Price + Fed_Funds_Rate + Spread_10_2yr + Unemployment_Rate + US_Treasury_10yr_rate, data=Vol.df, nvmax=5, intercept = F)
reg.summary <- summary(regfit.full)
reg.summary # This indicates the best 1 varaible model is Unemployment Rate and best 5 variable model contains all. 
reg.summary$adjr2

max_adjr2 <- which.max(summary(regfit.full)$adjr2)
plot(summary(regfit.full)$adjr2 , xlab="Number of Variables", ylab="Adj R Squared ", type='l')
points(max_adjr2,reg.summary$adjr2[max_adjr2], col="red", cex=2, pch=20) # Plotting the max point  reveals 3 variables is the best choice 


Vol_LM = lm(formula=Vix~Brent_Price + Fed_Funds_Rate + Spread_10_2yr + Unemployment_Rate + US_Treasury_10yr_rate, data=Vol.df)
summary(Vol_LM) # adjusted R-squared of 0.1495

# Now I remove the Unemployment rate b/c p-value is not signficant 
Vol_LM_v2 = lm(formula=Vix~Brent_Price + Fed_Funds_Rate + Spread_10_2yr + US_Treasury_10yr_rate, data=Vol.df)
summary(Vol_LM_v2) # adjusted R-squared of 0.15 

# I transformed the US_Treasury 10 year rate due to its p-value and then I played around with the 10_2yr spread to find the optimal 
Vol_LM_v3 = lm(formula=Vix~ Brent_Price + Fed_Funds_Rate + poly(Spread_10_2yr,5) + poly(US_Treasury_10yr_rate, 5), data=Vol.df)
summary(Vol_LM_v3) # adjusted R-squared of 0.24. This transformation improved the model 


anova(Vol_LM, Vol_LM_v2, Vol_LM_v3)

#  Finding high leverage points 
lev <- hatvalues(Vol_LM)
plot(lev, type="h")
Vol.df$DATE[which.max(lev)]

# To decide whether or not a data point is a candidate outlier point, we can use the studentized residual test
# e_i are the error residuals 

studRes <- rstudent(Vol_LM)
plot(studRes, type="h")
sum(abs(studRes) > 3) # How many outliers, 3 means standard deviations here 


# randomly 2/3 of the original data will be the training data
# rest will be the test data 
#Vol.df_training_data <- sample_frac(Vol.df,2/3) #Another method not used 
#Vol.df_test_data  <- anti_join(Vol.df, Vol.df_training_data) #Another method not used 

set.seed(1)
train_ind <- sample(1:nrow(Vol.df), 2/3*nrow(Vol.df))
Vol.df.train <- Vol.df[train_ind, ] # Our training data 
Vol.dftest <- Vol.df[-train_ind, ] # Our testing data 

# k-fold CV
glm.fit.kfold = glm(Vix~poly(Brent_Price, 5)+poly(Fed_Funds_Rate,5), data = Vol.df)
error_3 = cv.glm(Vol.df, glm.fit.kfold, K = 10)$delta[1]
error_3

set.seed(1)
cv.error_kfold <- matrix(nrow = 5, ncol = 6)

for (i in 1:5){
  glm.fit_kfold = glm(Vix~poly(Brent_Price, i)+poly(Fed_Funds_Rate,i), data = Vol.df)
  for (j in 5:10){
    cv.error_kfold[i,j-4]= cv.glm(Vol.df, glm.fit_kfold, K = j)$delta[1]
  }
}

# Just renamming the matrix for styling purposes 
rownames(cv.error_kfold) <- c("1st degree poly->", "2nd degree poly->", "3rd degree poly->", "4th degree poly->", "5th degree poly->")
colnames(cv.error_kfold) <- c("K=5", "K=6", "K=7", "K=8", "K=9", "K=10")

# Please see results matix below 
cv.error_kfold

# I see evidence that using cubic or higher-order polynomial terms leads to lower test error than simply using a quadratic fit. 

plot(c(1:5), cv.error_kfold[,1], type = "b", col = "red", xlab = "degree", ylab = "cv errors") #k=5
points(c(1:5), cv.error_kfold[,2], type = "b", col="blue", pch="*") #k=6
points(c(1:5), cv.error_kfold[,3], type = "b",col="black", pch="*") #k=7
points(c(1:5), cv.error_kfold[,4], type = "b",col="yellow", pch="*") #k=8
points(c(1:5), cv.error_kfold[,5], type = "b",col="dark red", pch="*") #k=9
points(c(1:5), cv.error_kfold[,6], type = "b",col="green", pch="*") #k=10


ggpairs(data=Vol.df, columns = 2:7)

# ridge gression 
x <- model.matrix(Vix~Brent_Price + Fed_Funds_Rate + Spread_10_2yr + Unemployment_Rate + US_Treasury_10yr_rate, Vol.df)[,-1]
y <- Vol.df$Vix

# We construct 100 equally-spaced penalty values on log-scale and fit a ridge regression with them. 
grid <- 10^seq(10,-2, length=100) # lambda sequence 
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid, intercept=FALSE)


l2_norm <- rep(0,100)
for (i in 1:100){
  l2_norm[i] <-sqrt(sum(ridge.mod$beta[,i]^2))
}

plot(log(grid), l2_norm)

# ** Non-linear modeling further ** 
gam1 <- lm(Vix~ns(Brent_Price,1),data=Vol.df)
gam2 <- gam(Vix~s(Brent_Price,4),data=test)

# Now we see view the fitted curves and each respective cofidence interval 
# GAMs use 3 predictors, so we are going make a plot with 3 frames 
par(mfrow=c(1,3))

# calling the right delegate of the function to get the right behavior
plot.Gam(gam1, se=TRUE, col="blue")

# Since gam2 was greated using the gam library we can a simple plot call
plot(gam2, se=TRUE, col="red")


test_model_v1 <- lm(Vix~poly(Brent_Price,3),data=Vol.df)
test_model_v2 <- lm(Vix~poly(Brent_Price,4),data=Vol.df)
test_model_v3 <- lm(Vix~poly(Brent_Price,5),data=Vol.df)
test_model_v4 <- lm(Vix~poly(Brent_Price,6),data=Vol.df)
summary(test_model_v1)

anova(test_model_v1, test_model_v2, test_model_v3, test_model_v4)

plot(Vol.df$Vix,Vol.df$Brent_Price)

plot(Vol.df$DATE , Vol.df$Vix)

ggplot(data=Vol.df) +
  geom_point(mapping=aes(x=Brent_Price, y=Vix)) +
  geom_smooth(mapping=aes(x=Brent_Price, y=Vix), span=3.0)

ggplot(data=Vol.df) +
  geom_point(mapping=aes(x=Brent_Price, y=Vix)) +
  geom_smooth(mapping=aes(x=Brent_Price, y=Vix), span=.5)

ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(span=.1) 

ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(method = lm) # We see a confidence interval arround this function 

ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(formula = y ~ poly(x, 2)) 

 


ggplot(Vol.df.outliers_removed, aes(x=Brent_Price, y=Vix)) + geom_point() + 
  geom_smooth(method = lm, formula = y ~ poly(x, 5), se= FALSE) + # posting 
  geom_quantile(color = "red", quantiles = c(0.32, 0.60, 0.75), size =2, alpha = 0.6) + 
  labs(title = "Vix vs. Brent Price using 5th order polynomial") +
  xlab("Brent Price") +
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(20,30,40,50,60,70,80,90,100,110,120,130,140))


ggplot(Vol.df, aes(x=Spread_10_2yr, y=Vix)) + geom_point() + 
  #geom_smooth(method = lm, color = "red", formula = y ~ poly(x, 4), se= FALSE) + 
  geom_quantile(color = "red", quantiles = c(0.75, 0.25), size =2, alpha = 0.6) + # plotting the percentiles 
  geom_quantile(color = "blue", quantiles = c(0.5), size = 2, alpha = 0.6) + 
  labs(title = "Vix vs. 10/2yr Spread Linear Regression") + 
  xlab("10/2yr Spread") + 
  scale_y_continuous(breaks = c(0,10,15,20,25,30,35,40,45,50,55,60,65)) +
  scale_x_continuous(breaks = c(-0.25, 0,0.25,0.5,0.75,1.0, 1.25,1.5,1.75,2.0,2.25,2.5,2.75,3.0))


ggplot(Vol.df, aes(x=Spread_10_2yr, y=Vix)) + geom_point() + 
  #geom_smooth(method = lm, formula = y ~ poly(x, 4), se= FALSE) + 
  geom_quantile(quantiles = c(0.75, 0.5, 0.25)) # plotting the quantiles 





ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 15))

ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 150)) # interesting chart 



ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(span=.1) 

ggplot(data=Vol.df, aes(x=Brent_Price, y=Vix)) +
  geom_point() +
  geom_smooth(span=.1, se = FALSE) 




# ---------------------- EDITING THE DATA  -------------------------------- 

# Creating a new dataset where I remove outliers 

hist(Vol.df.outliers_removed$Vix, col="skyblue") # distribution of outcomes. Add this to my presentaion 
hist <- hist(Vol.df.outliers_removed$Vix, breaks = 20, col="skyblue", xlab = "Vix", main = "Distribution of Outcomes for Vix") # add this to my presentation 

xfit<-seq(min(Vol.df.outliers_removed$Vix),max(Vol.df.outliers_removed$Vix),length=140)
yfit<-dnorm(xfit,mean=mean(Vol.df.outliers_removed$Vix),sd=sd(Vol.df.outliers_removed$Vix))
yfit <- yfit*diff(hist$mids[1:2])*length(Vol.df.outliers_removed$Vix)
lines(xfit, yfit, col="blue", lwd=2) 

boxplot(Vol.df$Vix)

# With outliers removed 
ggplot(data=Vol.df.outliers_removed, aes(x=Brent_Price, y=Vix)) +
  ylim(0, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 150)) # interesting chart lots of noice 

ggplot(data=Vol.df.outliers_removed, aes(x=Brent_Price, y=Vix)) +
  ylim(0, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 20)) # interesting chart lots of noice 

ggplot(data=Vol.df.outliers_removed, aes(x=Brent_Price, y=Vix)) +
  ylim(0, 43) +
  geom_point() +
  geom_smooth(span=.1) 

ggplot(data=Vol.df.outliers_removed, aes(x=Brent_Price, y=Vix)) +
  ylim(5, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) 

# look at my correlation with the Spread_10_2yr rate if I removed outleirs!!!! 
ggpairs(data=Vol.df.outliers_removed, columns = c(2,5)) # My correlation increased from 0.273 to 0.304 

Vol_LM_02_transformed <- lm(formula = Vix~Spread_10_2yr, data =Vol.df)
summary(Vol_LM_02_transformed) # Adjusted R-squared:  0.07

# now removing outliers for Vol_LM_01_transformed
Vol.df.outliers_removed_v2 <- subset(Vol.df, abs(rstudent(Vol_LM_02_transformed)) <= 3.0) # test 
Vol_LM_02_transformed.new <- lm(Vix~poly(Brent_Price,3), data=Vol.df.outliers_removed_v2)
summary(Vol_LM_02_transformed.new)$adj.r.squared # Adjusted R-squared [1] 0.1572097

ggplot(data=Vol.df.outliers_removed_v2, aes(x=Spread_10_2yr, y=Vix)) +
  ylim(5, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x) 

ggplot(data=Vol.df.outliers_removed_v2, aes(x=Spread_10_2yr, y=Vix)) +
  ylim(5, 43) +
  geom_point() +
  geom_smooth(span=.2) 



ggplot(data=Vol.df.outliers_removed_v2, aes(x=US_Treasury_10yr_rate, y=Vix)) +
  ylim(5, 43) +
  geom_point() +
  geom_smooth(span=.70) 


ggplot(data=Vol.df.outliers_removed_v2, aes(x=US_Treasury_10yr_rate, y=Vix)) +
  ylim(10, 43) +
  geom_point() +
  geom_smooth(span=.70, se=FALSE) 


ggplot(data=Vol.df.outliers_removed_v2, aes(x=Fed_Funds_Rate, y=Vix)) +
  ylim(10, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se=FALSE)  # showing this in report 

ggplot(data=Vol.df.outliers_removed_v2, aes(x=Unemployment_Rate, y=Vix)) +
  ylim(10, 43) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ x, se=FALSE) 


# -- clustering fun fun  

clusters_try2 <- kmeans(as.matrix(Vol.df.outliers_removed_v2$US_Treasury_10yr_rate), centers=4, nstart=25)
testdata <- cbind.data.frame(Vol.df.outliers_removed_v2$US_Treasury_10yr_rate, Vol.df.outliers_removed_v2$Vix)
clusters_try3 <- kmeans(as.matrix(testdata[,1]), centers=4, nstart=25)

fviz_cluster(clusters_try3, geom = "point", data = testdata) + ggtitle("k = 2")

k11 <- kmeans(Vol.df.2, centers = 11, nstart = 25)
k7 <- kmeans(Vol.df.2, centers = 7, nstart = 25)
k6 <- kmeans(Vol.df.2, centers = 6, nstart = 25)
k5 <- kmeans(Vol.df.2, centers = 5, nstart = 25)
k4 <- kmeans(Vol.df.2, centers = 4, nstart = 25)
k3 <- kmeans(Vol.df.2, centers = 3, nstart = 25)
k2 <- kmeans(Vol.df.2, centers = 2, nstart = 25)

str(k6)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = Vol.df.2) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = Vol.df.2) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = Vol.df.2) + ggtitle("k = 4")
p4 <- fviz_cluster(k4, geom = "point",  data = Vol.df.2) + ggtitle("k = 5")
p5 <- fviz_cluster(k5, geom = "point",  data = Vol.df.2) + ggtitle("k = 6")
p6 <- fviz_cluster(k6, geom = "point",  data = Vol.df.2) + ggtitle("k = 7")
p7 <- fviz_cluster(k7, geom = "point",  data = Vol.df.2) + ggtitle("k = 8")
p11 <- fviz_cluster(k11, geom = "point",  data = Vol.df.2) + ggtitle("k = 5")



library(gridExtra)
grid.arrange(p4, p5, p6, p7, nrow = 2)


Vol.df.2 %>%
  as_tibble() %>%
  mutate(cluster = k11$cluster, Brent_Price2 = row.names(Brent_Price)) %>%
  ggplot(aes(Spread_10_2yr, Vix, color = factor(cluster), label = Brent_Price)) +
  geom_text()

Vol.df.3 <- Vol.df
Vol.df.3 <- Vol.df.3 %>% mutate(Regime = ifelse(Vix >25, "High",ifelse(Vix<13, "Low", "Mid")))
row.names(Vol.df.3) <- Vol.df.3$Regime
Vol.df.3 <- Vol.df.3 %>% select(-Vix, -DATE)
Vol.df.3$Regime <- as.factor(Vol.df.3$Regime)


