# counts_reg is the linear regression on ~2000 data points for counts on each day
library(dplyr)
library(glmnet)
library(leaps)
library(caret)
library(gbm)
library(randomForest)
#setwd("Documents/GitHub/ORIE4710_FINAL")
#setwd("~/Serafin_Documents/Cornell/Spring_2020/ORIE_4740_Statistical_Data_Mining/Project/ORIE4710_FINAL-master/") #Tom S - PLEASE DO NOT DELETE

table = read.table("Cleaned_Data/daily_counts_all.csv", header=TRUE, sep=",")
# removing the following columns 
table <- table %>% select(-date, -month_num, -shootings) 

# Saving a list of columns that I will change to factor type 
cols <- c("WT01", "WT02", "WT03", "WT04", "WT05", "WT06", "WT08", "WT09")

# Lapply here takes each column as part of list cols and converts it into a factor 
table[cols] <- lapply(table[cols], factor)

#check data
str(table)

#quick check, 
#as_tibble(table)

table$time_delta_sq = table$time_delta^2
table$day_sq = table$day^2

set.seed(96)
train_ratio = 0.75

rand_table = table[sample(nrow(table)),]
train_size = floor(nrow(table)*train_ratio)
train = rand_table[1:train_size,]
test = rand_table[(train_size+1):nrow(table),]

# ---------------------------- testing here below 
library(factoextra)
fviz_nbclust(train, FUN = hcut, method = "wss")
train <- train %>% na.omit(train) %>% select(-weekday, -month_cat)  
k4 <- kmeans(train, centers = 4, nstart = 25)
plot(k4)

# ---- correlation plots.------------------------------------------ 
# you can bring over the clustering and how it influences the dataset into this. 
library(corrplot)
library(lubridate) #library to adjust weekdays to integers 
table <- table %>% select(-weekday, -month_cat) # deleting these two columns 

table[cols] <- lapply(table[cols], as.numeric)
table$WT04 <- as.numeric(table$WT04)
table$weekday <- as.integer(table$weekday) # testing to change weekday to a numeric value 
temp_data <- match(table$month_cat,month.abb) # testing to change month to a numeric value   
table$numeric_month <- temp_data # adding new numeric weekday into dataset
table <- table %>% select(-month_cat) #deleting old weekday from dataset 
corrplot(cor(table), type = 'upper', method = 'number', tl.cex = 0.9)

graphical_kmeans_6 <- kmeans(as.matrix(table), centers=6, nstart=25)
# adding our kmeans cluster as "cluster" column into our dataset 
table$k_clust <- graphical_kmeans_6$cluster 
# correlation graph with clustering 
corrplot(cor(table), type = 'upper', method = 'number', tl.cex = 0.9)

# ---------------- Fit 2 ---------------------- 
# About fit - In this fit, we fit all predictors on the data set
train.control <- trainControl(method = "cv", number = 10)
fit.1 <- train(count~.,data=train, method = "lm",
               trControl = train.control)
print(fit.1)
summary(fit.1) # adjusted r-squared of 0.3792

predictions = predict(fit.1, test)
mse = mean((predictions - test$count)^2)
mse # 929
mae = mean(abs(predictions - test$count))
mae # 22.04

pvals = summary(fit.1)$coefficients[,4]
low_pvals = pvals[pvals<0.1]
low_pvals_vars = names(pvals)

table = subset(table, abs(rstudent(lm(formula=count~.,data=table))) <= 3.0)
table = table[sample(nrow(table)),]
train_size = floor(nrow(table)*train_ratio)

y = table$count
x = table
x$count = NULL
train_x = x[1:train_size,]
train_y = y[1:train_size]
test_x = x[(train_size+1):nrow(x),]
test_y = y[(train_size+1):nrow(x)]
# ---------------- Fit 2 ---------------------- 
# About fit - In this fit, I remove the variables with high p-values in Fit 1 

fit.2 = train(count~
             weekday +
             time_delta_sq +
             day_sq +
             WT09 +
             WT04 +
             WT01 +
             PRCP +
             TMAX, 
             data=table,
             method="lm",
           trControl=train.control)
å
print(fit.2)
summary(fit.2) # adjusted r-squared 0.4076

predictions = predict(fit.2, test_x)
mse = mean((predictions - test_y)^2)
mse # MSE 688.4768
mae = mean(abs(predictions - test_y))
mae # MAE 20.5 

# ---------------- Fit 2 (B) ---------------------- 
# About fit - Use only the variables I learned from RF are signficant and really add value. Produes some of the bset results here 

fit.3 = train(count~
                weekday +
                time_delta_sq +
                TMAX, 
              data=table,
              method="lm",
              trControl=train.control)

print(fit.3)
summary(fit.3) # Adjusted R-squared 0.3858 

predictions = predict(fit.3, test_x)
mse = mean((predictions - test_y)^2) 
mse #722.8824

mae = mean(abs(predictions - test_y))
mae # 20.88399

# ---------------- Fit 2 (C)---------------------- 
# About fit - Use only the variables I learned from RF are signficant and really add value. Produes some of the bset results here 
# I slightly cherry picked but at the same time these are the most signficant values from a RF that uses all the variables.
# above 2(B) uses a RF that had only the most important sig variables. 2(C) used RF from all variables. 

fit.4 = train(count~
                weekday +
                day_sq +
                TMAX, 
              data=table,
              method="lm",
              trControl=train.control)

print(fit.4)
summary(fit.4) # Adjusted R-squared 0.394 

predictions = predict(fit.4, test_x)
mse = mean((predictions - test_y)^2) 
mse #713.9212

mae = mean(abs(predictions - test_y))
mae # 20.88248

# ---------------- Fit 2 (D)---------------------- 
# About fit - Use only the variables I learned from RF are signficant and really add value. Produes some of the bset results here 
# I slightly cherry picked but at the same time these are the most signficant values from a RF that uses all the variables.
# above 2(B) uses a RF that had only the most important sig variables. 2(C) used RF from all variables
# This time cherry picking fron and used the best 4 varaibles from the rf.boston_best () random forest 

fit.5 = train(count~
                weekday +
                TMAX + 
                day_sq +
                TAVG, 
              data=table,
              method="lm",
              trControl=train.control)

print(fit.5)
summary(fit.5) # Adjusted R-squared 0.386   ~ 0.39

predictions = predict(fit.5, test_x)
mse = mean((predictions - test_y)^2) 
mse #708.47

mae = mean(abs(predictions - test_y))
mae # 20.87

# do cv on all of the models and compare them 
# retain on all of data 

# ---------------- Fit 3 ---------------------- 
# About fit - Use Lasso to remove some variables

lasso_x <- model.matrix(count~., table)[,-1]
lasso_train_x = lasso_x[1:train_size,]
lasso_test_x = lasso_x[(train_size+1):nrow(table),]

range = seq(0, -4, length=100)
lambdas = 10^range # lambda sequence
#fit.lasso <- glmnet(lasso_train_x, train_y, alpha=1, lambda=lambdas)
cv.out <- cv.glmnet(lasso_train_x, 
                    train_y, 
                    alpha=1, 
                    lambda=lambdas)

optimal_mse = min(cv.out$cvm)
optimal_lambda = as.vector(cv.out$lambda)[abs(as.vector(cv.out$cvm) - optimal_mse) < 0.000001]
print(optimal_mse)
print(optimal_lambda)
fit.lasso <- glmnet(lasso_train_x, train_y, alpha=1, lambda=optimal_lambda)#optimal_lambda[1])
pred <- predict(fit.lasso, newx = lasso_test_x)

rss <- sum((pred - test_y) ^ 2)
tss <- sum((test_y - mean(test_y)) ^ 2)
rsq <- 1 - rss/tss
print(rsq)
coef(fit.lasso)
plot(range, cv.out$cvm)


### Best Subset

predict.regsubsets <- function(regfit, newdata, id, ...) {
  mat <- model.matrix(formula(count~.), newdata)
  coefi <- coef(regfit, id=id)
  xvars <- names(coefi)
  as.matrix(mat[, xvars]) %*% coefi
}

k = 5
nv = 20
folds = sample(1:k,nrow(x),replace=TRUE)
cv.errors = matrix(NA,k,nv, dimnames =list(NULL, paste(1:nv) ))

for(j in 1:k) {
  print(j)
  
  best.fit=regsubsets(count~.,data=table[folds!=j,],nvmax=nv)
  for(i in 1:nv) {
    pred = predict.regsubsets(best.fit, table[folds ==j,],id=i)
    cv.errors[j,i]= mean((table$count[folds==j]-pred)^2)
  }
}

coef(best.fit, 16)
mean.cv.errors=apply(cv.errors, 2, mean)
plot(mean.cv.errors ,type='b')

# below you will find out best fitting & most interpretable model. Although boosting creates the lowest MSE it isn't the model readible and interpretable.
# The MSE, MAE, and R-squared doesn't adjust enough for the lack of interpretability so we find that "best.fit_final" fitting model used for this project. 
best.fit_final <- regsubsets(count~.,data=table,nvmax=20)
pred = predict.regsubsets(best.fit_final, table,id=16)
mean((table$count-pred)^2) # 617.1701

mae = mean(abs(pred - table$count))
mae # 19.56917

summary(best.fit_final)$adjr2[16] # 0.adjusted R-squared for our "best" model #

coef <- coef(best.fit_final,16) # 0.428, coefficients of the 16 variable model
coef
mean.cv.errors[16] 

# My min is 16

# -- test tom ---


best.fit_test = regsubsets(count~.,data=train,nvmax=37)
plot(best.fit, scale="adjr2")

fit.6 = train(count~
                PRCP+
                TMAX+
                WT02+
                WT04+ 
                WT09+
                day+
                month_cat,
              data=table,
              method="lm",
              trControl=train.control)

print(fit.6)
summary(fit.6) # Adjusted R-squared 0.386

predictions = predict(fit.6, test_x)
mse = mean((predictions - test_y)^2) 
mse #708.47


mae = mean(abs(predictions - test_y))
mae # 20.87

# -- test tom ---

### Boosting

B=1000
lambdas=2^seq(-1, -10, by = -0.1)
train_mse=rep(0,length(lambdas))
test_mse=rep(0,length(lambdas))

train = table[1:train_size,]
test = table[(train_size+1):nrow(table),]

for (i in 1:length(lambdas)) {
  boost=gbm(count ~ .,data=train,n.trees=B,distribution="gaussian",shrinkage=lambdas[i])
  predictions=predict(boost,train,n.trees=B)
  train_mse[i]=mean((predictions-train$count)^2)
  
  predictions=predict(boost,test,n.trees = B)
  test_mse[i]=mean((predictions-test$count)^2)
}

plot(lambdas,train_mse,xlab="Lambda", ylab="Train MSE")
plot(lambdas,test_mse,xlab="Lambda",ylab="Test MSE")
min_mse = min(test_mse)
best=lambdas[which.min(test_mse)]

print(min_mse)
print(best)

best_model = gbm(count~.,data=train,distribution="gaussian",n.trees=B,shrinkage=best)
summary(best_model)

pred=predict(best_model,test_x,n.trees=B)

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

# Attempt XGBOOST
require(xgboost)
library(xgboost)
train = table[1:train_size,]
trainMatrix <- model.matrix(~.+0, data=train)
storage.mode(trainMatrix) <- 'double'
trainXGB = xgb.DMatrix(data = trainMatrix, label=as.numeric(train$count))


testMatrix <- model.matrix(~.+0, data=table[(train_size+1):nrow(table),])       # convert data frame to matrices
storage.mode(testMatrix) <- 'double'  # convert integer to real
trainYvec <- as.integer(train_y)    # extract response from training set; class label starts from 0
testYvec  <- as.integer(test_y)    # extract response from test set; class label starts from 0

param <- list("objective" = "reg:linear",
              "eval_metric" = "mae",
              "eta" = 0.3)

nround <- 100 # number of rounds/trees

xgbtree <- xgb.train(param = param,
                   data = trainMatrix,
                   nrounds = nround,  # number of trees
                   max.depth = 4)     # tree depth (not the same as interaction.depth in gbm!)

xgbtree.prob <- predict(xgbtree, testXMatrix)    # vector of predicted class labels
print(xgbtree.acc <- mean(xgbtree.prob == testYvec))  # classification accuracy on test set

# ------------- Random Forest --------------- 
#Boston. = randomForest(count~. , data=train, ntree=100, mtry=2, importance=TRUE) # bagging 
# https://www.blopig.com/blog/2017/04/a-very-basic-introduction-to-random-forests-using-r/

set.seed(1)
test_mse <- rep(0, 10)

for (i in c(1:10)){
  rf.Boston <- randomForest(count~., data = train, mtry=i, importance=TRUE) # not sure if this line would be the right way 
  #rf.Boston <- randomForest(count~., table, subset=train, mtry=i, importance=TRUE) # using this line from lab6
  rf.pred <- predict(rf.Boston, newdata = test)
  test_mse[i] <- mean((rf.pred - test$count)^2)
}


plot(test_mse, type="b")
which.min(test_mse)

varImpPlot(rf.Boston, bg = "cyan")

# using my best case  i =4 from the plot 
rf.Boston_best0 <- randomForest(count~., data = train, mtry=4, importance=TRUE) # MSE of 836

importance(rf.Boston_best0)
varImpPlot(rf.Boston_best0, bg = "cyan") 

rf.pred <- predict(rf.Boston_best0, newdata = test)
test_mse <- mean((rf.pred - test$count)^2)
test_mse

mae = mean(abs(rf.pred - test$count))
mae # 20.94

rf.Boston_best <- randomForest(count~weekday +
                                 time_delta_sq +
                                 day_sq +
                                 WT09 +
                                 WT04 +
                                 WT01 +
                                 PRCP +
                                 TMAX, data = train, mtry=4, importance=TRUE)   # test MSE of 810.5357

importance(rf.Boston_best) 
varImpPlot(rf.Boston_best,bg = "cyan") 
rf.pred2 <- predict(rf.Boston_best, newdata = test)

# Validation set assessment #1: looking at the test mse
test_mse <- mean((rf.pred2 - test$count)^2)
test_mse

# Validation set assessment #1b looking at mae
mae = mean(abs(rf.pred2 - test$count))
mae # 20.90

# validation set assessment #2 
rss <- sum((rf.pred2 - test$count) ^ 2)  ## residual sum of squares
tss <- sum((test$count - mean(test$count)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss # R-squared # not recommended to look at this. 
rsq
