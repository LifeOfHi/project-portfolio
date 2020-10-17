library(dplyr)
df <- read.csv("7406_Project_Data_Div_updated.csv", header = TRUE, sep = ",")
# Note: The first column in the csv was unnamed. Take that into account in column indexing.

head(df)
dim(df)
colnames(df)

##################################################################################################################################################

# Normalization of the predictors(UGDS)
data_norm <- df

for (i in 1:nrow(df)) {
  for (j in 3:14) {
    data_norm[i,j] <- df[i,j] / df[i, 15]  
  }
}

# Uncomment to check different log transformations

modeling.data_quant <- data_norm[, 3:20]
head(modeling.data_quant)
colnames(modeling.data_quant)

# Extract Coaches.Compensation (highest vif) & UGDS (normalization)
#modeling.data_quant["TUITIONFEE_IN"] <- modeling.data_quant["TUITIONFEE_IN"] + modeling.data_quant["TUITIONFEE_OUT"]
#names(modeling.data_quant)[names(modeling.data_quant) == "TUITIONFEE_IN"] <- "TUITIONFEE_TOT"
modeling.data_quant <- modeling.data_quant[, -c(7, 13, 17)]

#modeling.data_quant[1:11] <- lapply(modeling.data_quant[1:11], log)
#modeling.data_quant[15] <- lapply(modeling.data_quant[15], log)
#modeling.data_quant[14] <- modeling.data_quant[14]^(1/2)
head(modeling.data_quant)
colnames(modeling.data_quant)

###################################################################################################################################################

#extract quantiles. If you dont't, the fit will be very good, because you will use a second response as a predictor
modeling.data <- modeling.data_quant

set.seed(1021) # for reproduciblity
## 80% of the sample size as training
n = dim(data_norm)[1] ### total number of observations
n1 = round(n/5) ### number of observations randomly selected for testing data - using 20%
flag = sort(sample(1:n, n1))
train = modeling.data[-flag,]
test = modeling.data[flag,]

# LINEAR REGRESSION
lin.rg <- lm(Total.Points ~ ., data = modeling.data)
summary(lin.rg)
# Chech variance inflation factors
library(car)
vif <- vif(lin.rg)
vif[vif > 5] #The normalization has helped a lot in reducing VIFs.

# 10-fold and LOOCV comparison
library(boot)
n <- nrow(modeling.data)
modglm <- glm(Total.Points ~ ., data = modeling.data)
c(cv.glm(modeling.data, modglm, K = 10)$delta[1], cv.glm(modeling.data, modglm, K=n)$delta[1]) #1262.650 & 1247.732


# LOOCV coded without calling a package (it will be used later)
accuracy_vector <- c()
check_row <- 1
while (check_row <= dim(modeling.data)[1]) {
  lin.rg.loocv <- lm(Total.Points ~ ., data = modeling.data[-check_row, ])
  prediction <- predict(lin.rg.loocv, modeling.data[check_row, ])
  sq.er <- (modeling.data$Total.Points[check_row] - prediction)^2
  accuracy_vector <- c(accuracy_vector, sq.er)
  check_row <- check_row + 1
}
mean(accuracy_vector)#1247.732 - verfies that the code is ok.


# FORWARD STEPWISE REGRESSION
zero_model <- lm(Total.Points ~ 1, data = modeling.data)
full_model <- lm(Total.Points ~ ., data = modeling.data)
forw <- step(zero_model, scope = list(lower = zero_model, upper = full_model), direction = "forward")
summary(forw)

#################################################################

# RIDGE REGRESSION
library(MASS)
library(glmnet)
predictors <- model.matrix(Total.Points ~ ., data = modeling.data)
response <- data.matrix(modeling.data[, 13])
lambdas <- seq(0.01, 10, by = 0.01)
ridge.cv <- cv.glmnet(predictors, response, alpha = 0, nfolds = 10, lambda = lambdas)
ridge <- glmnet(predictors, response, alpha = 0, nlambda = 100)
#ridge.cv$lambda.min
coef(ridge, s = ridge.cv$lambda.min)


#################################################################

# LASSO REGRESSION 
lasso.cv <- cv.glmnet(predictors, response, alpha = 1, nfolds = 10, lambda = lambdas)
#lasso.cv$lambda.min
lasso <- glmnet(predictors, response, alpha = 1, nlambda = 100)
plot(mod5, xvar = "lambda", label = TRUE, lwd = 2)
abline(v = log(lasso.cv$lambda.min), col ='black',lty = 2,lwd = 2)
coef(lasso, s = lasso.cv$lambda.min)
# just wanted to check which coefs become zero. Didn't make predictions.
plot(lasso.cv)
#################################################################

# Extract predictors with zero coefficients from LASSO.

#modeling.data <- modeling.data[, -c(2, 10)]
#colnames(modeling.data)


# KNN Regression                              #Note: Categorical variables cannot be used in KNN (at least I don't know how...)
library(FNN)
library(caret)

cv.error_knn <- c()
for (n in seq(3, 29, 1)) {
  
  knn_error <- c()
  check_row <- 1
  #Perform Leave-one-out cross-validation
  while (check_row <= dim(train)[1]) {
    
    trainData <- train[-check_row, ]
    testData <- train[check_row, ]
      
    fit <- knnreg(x = trainData[, 1:13], y = trainData[, 15], k = n)    # 13 and 14 have to change depending on the predictors we use
    prediction <- predict(fit, testData[, 1:13])                       # Same here
    
    
    
    sq.er <- (testData$Total.Points - prediction)^2
      
    knn_error <- c(knn_error, sq.er)
    check_row <- check_row + 1
  }
  cv.error_knn <- c(cv.error_knn, mean(knn_error))
}

cv.error_knn 
min(cv.error_knn) #1266.66

# GAM -  This is the simplest version of GAM
library(gam)
gam.fit <- gam( Total.Points ~ ., data = modeling.data, trace = TRUE)
summary(gam.fit)


accuracy_vector <- c()
check_row <- 1
while (check_row <= dim(modeling.data)[1]) {
  gam.fit <- gam(Total.Points ~ ., data = modeling.data[-check_row, ])
  prediction <- predict(gam.fit, modeling.data[check_row, ])
  sq.er <- (modeling.data$Total.Points[check_row] - prediction)^2
  accuracy_vector <- c(accuracy_vector, sq.er)
  check_row <- check_row + 1
}
mean(accuracy_vector)


# It shows some warnings, but I can't understand what they mean...

##################################################################################################################################################


# RANDOM FOREST
library(randomForest)
library(ranger)

hyper_grid <- expand.grid(
  mtry       = seq(3, 12, by = 1),
  node_size  = seq(3, 9, by = 1),
  #sampe_size = c(.55, .632, .70, .80),
  num_trees = c(100, 200, 500, 1000, 2000, 5000),
  OOB_RMSE   = 0
)
# total number of combinations
nrow(hyper_grid)

# Tune based on OOB error
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Total.Points ~ ., 
    data            = train, 
    num.trees       = hyper_grid$num_trees[i],
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    #sample.fraction = hyper_grid$sampe_size[i],
    seed            = 1021
  )
  
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)

rf.m1 <- ranger(formula = Total.Points ~ ., data = train, num.trees = 1000, mtry = 6, min.node.size = 3)
pred.tun.1 <- predict(rf.m1, data = test)
sqrt(mean((test$Total.Points - pred.tun.1$predictions)^2)) # 30.86447


rf.m2 <- randomForest(formula = Total.Points ~ ., data = train, ntree = 1000, mtry = 6, nodesize = 3, importance = TRUE)
pred.tun.2 <- predict(rf.m2, newdata = test)
sqrt(mean((test$Total.Points - pred.tun.2)^2)) # 31.02824

importances <- as.matrix(rf.m2$importance[, 2])
ordered.imp <- importances[order(-rf.m2$importance[, 2]),]
#ordered.imp


#############################################################################################################################################################

# Tune based on Test Error
hyper_grid2 <- expand.grid(
  mtry       = seq(3, 12, by = 1),
  node_size  = seq(3, 9, by = 1),
  #sampe_size = c(.55, .632, .70, .80),
  num_trees = c(100, 200, 500, 1000, 2000, 5000),
  test_error   = 0
)


for(i in 1:nrow(hyper_grid2)) {
  rf.m1 <- randomForest(formula = Total.Points ~ ., data = train, ntree = hyper_grid2$num_trees[i], mtry = hyper_grid2$mtry[i], nodesize = hyper_grid2$node_size[i])
  pred.tun.2 <- predict(rf.m2, newdata = test)
  pred.error <- sqrt(mean((test$Total.Points - pred.tun.2)^2))
  hyper_grid2$test_error[i] <- pred.error
}

hyper_grid2 %>% 
  dplyr::arrange(test_error) %>%
  head(10)







####################################################################################################################################################


# BOOSTING

library(gbm)
# Tune based on Test Error
hyper_grid3 <- expand.grid(
  shrinkage       = seq(0.01, 0.2, by = 0.01),
  depth           = seq(2, 6, by = 1),
  num_trees = c(100, 200, 500, 1000, 2000, 5000),
  test_error   = 0
)

nrow(hyper_grid3)

for(i in 1:nrow(hyper_grid3)) {
  boost.1 <- gbm(Total.Points ~ . ,data = train, distribution = "gaussian", n.trees = hyper_grid3$num_trees[i], shrinkage = hyper_grid3$shrinkage[i], interaction.depth = hyper_grid3$depth[i])
  yhat.boost <- predict(boost.1 ,newdata = test, n.trees = hyper_grid3$num_trees[i])
  pred.error <- sqrt(mean((yhat.boost - test$Total.Points)^2)) 
  hyper_grid3$test_error[i] <- pred.error
}

hyper_grid3 %>% 
  dplyr::arrange(test_error) %>%
  head(5)

# Fit the boosting tree with the best parameters
boost <- gbm(Total.Points ~ . ,data = train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.12, interaction.depth = 6)
yhat.boost <- predict(boost ,newdata = test, n.trees = 1000)
sqrt(mean((yhat.boost - test$Total.Points)^2)) #29.30981








###############################################################################################################################################

# MONTE CARLO CROSS-VALIDATION
B = 100
TEALL = NULL

for (i in seq(1, B, 1)) {
  
  n = dim(data_norm)[1] ### total number of observations
  n1 = round(n/5) ### number of observations randomly selected for testing data - using 20%
  flag = sort(sample(1:n, n1))
  train = modeling.data[-flag,]
  test = modeling.data[flag,]
  
  #True Y values for Test set
  ytrue <- test$Total.Points

  #Boosting Model
  model1 <- gbm(Total.Points ~ . ,data = train, distribution = "gaussian", n.trees = 1000, shrinkage = 0.12, interaction.depth = 6)
  yhat.boost <- predict(model1 ,newdata = test, n.trees = 1000)
  error_boost <- sqrt(mean((yhat.boost - ytrue)^2))

  #RF Model
  
  rf.m2 <- randomForest(formula = Total.Points ~ ., data = train, ntree = 1000, mtry = 6, nodesize = 3)
  pred.RF <- predict(rf.m2, newdata = test)
  error_rf <- sqrt(mean((ytrue - pred.RF)^2))
  
  fit <- knnreg(x = train[, 1:13], y = train[, 15], k = 3)
  prediction <- predict(fit, test[, 1:13])
  error_knn <- sqrt(mean((ytrue - prediction)^2))
  
  TEALL = rbind(TEALL, cbind(error_boost, error_rf, error_knn))
  
}


dim(TEALL)
apply(TEALL, 2, mean)
round(apply(TEALL, 2, var), 9)

library(ggplot2)
montecarlo <- as.data.frame(TEALL)

ggplot(montecarlo, aes(x = error_boost))+
  geom_histogram(color="darkblue", fill="lightblue", bins = 20) + labs(x = "rmse", title = "Gradient Boosting - RMSE Distribution") +
  theme_grey(base_size = 15) +theme(plot.title = element_text(hjust = 0.5))

ggplot(montecarlo, aes(x = error_rf)) +
  geom_histogram(color="darkblue", fill="lightgreen", bins = 20) + labs(x = "rmse", title = "Random Forest - RMSE Distribution")+
  theme_grey(base_size = 15) +theme(plot.title = element_text(hjust = 0.5)) 

ggplot(montecarlo, aes(x = error_knn)) +
  geom_histogram(color="darkblue", fill="lightgreen", bins = 20) + labs(x = "rmse", title = "KNN - RMSE Distribution")+
  theme_grey(base_size = 15) +theme(plot.title = element_text(hjust = 0.5))


