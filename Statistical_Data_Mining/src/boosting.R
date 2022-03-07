library(dplyr)
library(glmnet)
library(leaps)
library(caret)
library(gbm)
setwd("Documents/GitHub/ORIE4710_FINAL")
#setwd("~/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master/") #Tom S - PLEASE DO NOT DELETE

table = read.table("Cleaned_Data/daily_counts_all.csv", header=TRUE, sep=",")
table = table[sample(nrow(table)),]
nrow(table)
count_sd = sd(table$count)
table = table[abs(table$count-mean(table$count))/count_sd < 3.0,] # Remove outliers
nrow(table)
table$date = NULL
table$month_num = NULL
table$shootings = NULL

# Cast these variables to factor type
cols <- c("WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT08", "WT09")
table[cols] <- lapply(table[cols], factor)

table$time_delta_sq = table$time_delta^2 # NEEDS JUSTIFICATION




set.seed(921)
train_ratio = 0.75
train_size = floor(nrow(table)*train_ratio)


#table = subset(table, abs(rstudent(lm(formula=count~.,data=table))) <= 3.0)
#table = table[sample(nrow(table)),]
#train_size = floor(nrow(table)*train_ratio)


table$day = NULL
table$time_delta_sq = NULL
table$WT09 = NULL
table$WT06 = NULL
table$WT05 = NULL
table$WT08 = NULL
table$WT04 = NULL
table$WT03 = NULL
table$WT02 = NULL
table$WT01 = NULL
table$TMIX = table$TMAX * table$TMIN

train = table[1:train_size,]
test = table[(train_size+1):nrow(table),]

lm1 = lm(count~.,data=table)
summary(lm1)
table = subset(table, abs(rstudent(lm1)) < 3.0)

# simple model
lm_time = lm(count~time_delta+time_delta_sq,data=table)
summary(lm_time)
x = table$time_delta
y = predict(lm_time, data=table$time_delta)
plot(table$time_delta, table$count,xlab='Time', ylab='Crimes')
points(x,y,col='blue',type='p')

# simple model
lm_temp = lm(count~TMAX,data=table)
summary(lm_temp)
x = table$TMAX
y = predict(lm_temp, data=table$TMAX)
plot(table$TMAX, table$count,xlab='Temperature', ylab='Crimes')
points(x,y,col='blue',type='b')

# simple model
table22 = table[table$AWND < 30,]
lm_wind = lm(count~AWND,data=table22)
summary(lm_wind)
x = table22$AWND
y = predict(lm_wind, data=x)
plot(table22$AWND, table22$count,xlab='Average Wind Speed', ylab='Crimes')
points(x,y,col='blue',type='b')




high_snow = table[abs(table$SNOW - mean(table$SNOW))/sd(table$SNOW) > 0.25,]
snow_indexes = sample(nrow(high_snow))
train_indexes = unique(c(snow_indexes, sample(nrow(table)*train_ratio)))

#train = table[train_indexes,]
#test = table[-train_indexes,]
train = table[1:train_size,]
test = table[(train_size+1):nrow(table),]

B=1000

find_best_lambda <- function(train, test) {

  lambdas=2^seq(-1, -8, by = -0.1)
  test_mse=rep(0,length(lambdas))
  
  for (i in 1:length(lambdas)) {
    boost=gbm(count ~ .,
              data=train,
              n.trees=B,
              distribution="gaussian",
              shrinkage=lambdas[i])
    
    predictions=predict(boost,test,n.trees = B)
    test_mse[i]=mean((predictions-test$count)^2)
  }
  
  min_mse = min(test_mse)
  best_lambda=lambdas[which.min(test_mse)]
  
  return(best_lambda)
}

best_lambda = find_best_lambda(train, test)
print(best_lambda)


model_1 = gbm(count~.,data=train,distribution="gaussian",n.trees=B,shrinkage=best_lambda)
summary(model_1)

test_x = test
test_x$count = NULL
test_y = test$count
pred=predict(model_1,test_x,n.trees=B)

rss <- sum((pred - test_y) ^ 2)
tss <- sum((test_y - mean(test_y)) ^ 2)
rsq <- 1 - rss/tss
print(rsq)

mse = mean((pred - test_y)^2)
mse
mae = mean(abs(pred - test_y))
mae
acc = 1 - mean(abs(pred - test_y)/test_y)
acc

# lambda = 0.0145784
# R2 = 0.452
# MSE = 588.55
# MAE = 18.77944
# ACC

