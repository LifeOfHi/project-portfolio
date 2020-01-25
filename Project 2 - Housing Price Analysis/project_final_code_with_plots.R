# Hari Ilangovan
# Regression Analysis of Kings County Housing Prices
# data: https://www.kaggle.com/harlfoxem/housesalesprediction

# plotting libraries
library(ggplot2)
library(cowplot)
library(reshape2)

# Data splitting
library(caTools)

# linear mixed effects library
library(lme4)

# forward / backward selection
library(CombMSC)
library(glmnet)

## Keep this script and dataset in same directory
## Make sure to set cwd via:
## Session -> Set Working Directory -> To Source File Location

# 1. read in the data
step1_read_data <- function(file_link) {
  data <- read.table(file_link, header=TRUE, sep=",")
  return(data)
}

# 2. Exploratory analysis on factors
step2_factor_analysis <- function(data) {
  large <- data$sqft_basement > 1000
  medium <- data$sqft_basement <= 1000 & data$sqft_basement > 500
  small <- data$sqft_basement <= 500 & data$sqft_basement > 0
  
  data$sqft_basement[data$sqft_basement == 0] <- "aNo Basement"
  data$sqft_basement[small] <- "Small (Less than 500 sqft)"
  data$sqft_basement[medium] <- "Medium (Less than 1000 sqft)"
  data$sqft_basement[large] <- "Large (Greater than 1000 sqft)"
  
  data$yr_renovated[data$yr_renovated != 0] = "Renovated"
  data$yr_renovated[data$yr_renovated == 0] = "aNone"
  
  data$view[data$view == 0] <- "aNone"
  data$view[data$view == 1] <- "Average"
  data$view[data$view == 2] <- "Good"
  data$view[data$view == 3] <- "Great"
  data$view[data$view == 4] <- "Excellent"
  
  data$waterfront[data$waterfront == 0] <- "aNone"
  data$waterfront[data$waterfront == 1] <- "Included"
  
  data$condition[data$condition == 1] = "aPoor"
  data$condition[data$condition == 2] = "Average"
  data$condition[data$condition == 3] = "Good"
  data$condition[data$condition == 4] = "Great"
  data$condition[data$condition == 5] = "Excellent"
  
  # 3. Separate Month
  data$Month[data$Month == 1] <- "Jan"
  data$Month[data$Month == 2] <- "Feb"
  data$Month[data$Month == 3] <- "Mar"
  data$Month[data$Month == 4] <- "Apr"
  data$Month[data$Month == 5] <- "May"
  data$Month[data$Month == 6] <- "Jun"
  data$Month[data$Month == 7] <- "Jul"
  data$Month[data$Month == 8] <- "Aug"
  data$Month[data$Month == 9] <- "Sep"
  data$Month[data$Month == 10] <- "Oct"
  data$Month[data$Month == 11] <- "Nov"
  data$Month[data$Month == 12] <- "Dec"
  
  #convert to factor
  data$Month <- as.factor(data$Month) # factor with 12 levels
  data$view <- as.factor(data$view) # factor with 4 levels
  data$waterfront <- as.factor(data$waterfront) # factor with 2 levels
  data$condition <- as.factor(data$condition) # factor with 5 levels
  data$yr_renovated <- as.factor(data$yr_renovated) # factor with 2 levels
  data$zipcode <- as.factor(data$zipcode) # factor with 70 levels
  data$sqft_basement <- as.factor(data$sqft_basement)
  return(data)
}

# 3. Box plots
step3_boxplots1 <- function(data) {
  # boxplots
  p<-ggplot(data, aes(x=as.factor(Year), y=price, color=Year)) + geom_boxplot(show.legend = FALSE)
  p1 <- p + labs(title="Price by Year", x ="Year", y = "Price")
  p<-ggplot(data, aes(x=as.factor(Month), y=price, color=Month)) + geom_boxplot(show.legend = FALSE)
  p2 <- p + labs(title="Price by Month", x ="Month", y = "Price")
  p<-ggplot(data, aes(x=view, y=price, color=view)) + geom_boxplot(show.legend = FALSE)
  p4 <- p + labs(title="Price by View", x ="View", y = "Price")
  p<-ggplot(data, aes(x=waterfront, y=price, color=waterfront)) + geom_boxplot(show.legend = FALSE)
  p5 <- p + labs(title="Price by Waterfront", x ="Waterfront", y = "Price")
  p<-ggplot(data, aes(x=condition, y=price, color=condition)) + geom_boxplot(show.legend = FALSE)
  p5 <- p + labs(title="Price by Condition", x ="Condition", y = "Price")
  p<-ggplot(data, aes(x=yr_renovated, y=price, color=yr_renovated)) + geom_boxplot(show.legend = FALSE)
  p6 <- p + labs(title="Price by Year Renovated", x ="Year Renovated", y = "Price")
  
  plot_grid(p1, p2, p4, p5, p6, labels = "AUTO")
}

step3_boxplots2 <- function(data) {
  p<-ggplot(data, aes(x=as.factor(zipcode), y=price, color=zipcode)) + geom_boxplot(show.legend = FALSE)
  p3 <- p + labs(title="Price by Zipcode", x ="Zipcode", y = "Price") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  plot_grid(p3)
}

# 4. Correlation plots
step4_correlation_plot <- function(data) {
  # heatmap plot
  Correlationcheck <- data[,c('price','bedrooms','bathrooms','sqft_living','sqft_lot','floors','grade','sqft_above','yr_built','lat','long','sqft_living15','sqft_lot15')]
  cormat <- round(cor(Correlationcheck),2)
  
  melted_cormat <- melt(cormat)
  ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
    geom_tile()
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed()
  # Print the heatmap
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
}

# 5. Zip Code Analysis
step5_zipcode_analysis <- function(data) {
  data$zipcode <- as.numeric(data$zipcode)
  geo_factors <- data[,c('id','zipcode','lat','long')]
  unique_zipcodes <- unique(geo_factors$zipcode)
  # correlation
  unique_lat <- unique(geo_factors$lat)
  unique_long <- unique(geo_factors$long)
  
  print(paste("There are", length(unique_zipcodes), "unique zip codes"))
  print(paste("There are", length(unique_lat), "unique latitude coordinates"))
  print(paste("There are", length(unique_long), "unique longitude coordinates"))
  
  counts <- rep(0, length(unique_zipcodes))
  zipcode_counts <- as.data.frame(cbind(unique_zipcodes, counts))
  
  nrows <- dim(zipcode_counts)[1]
  
  for(i in 1:nrows) {
    index_match <- data$zipcode == zipcode_counts[i,1]
    zipcode_counts[i, 2] = dim(data[index_match,])[1]
  }
  
  # plot of the distribution of the zip code counts
  #hist(zipcode_counts$counts, breaks=70, xlab = 'Counts', ylab = 'Number of Zipcodes',main = 'Distribution of Zipcode counts')
  p<-ggplot(data, aes(x=zipcode)) + 
    geom_histogram(color="black", fill="white", bins = 70) +
    labs(title="Distribution of Zip code counts", x ="Zip code index", y = "Count of Zip code")
  p1 <- p
  p<-ggplot(data, aes(x=as.factor(zipcode), y=price, color=zipcode)) + geom_boxplot(show.legend = FALSE)
  p3 <- p + labs(title="Price by Zipcode", x ="Zipcode", y = "Price") + theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank())
  
  plot_grid(p1, p3)
}

# 6. ANOVA on Years
step6_anova_years <- function(data) {
  # running ANOVA on Years
  # high p-value, Years is not statistically significant
  model = aov(data$price ~ as.factor(data$Year))
  summary(model)
}

# 7. Fitting Model
step7_fitting_model <- function(data) {
  model1 <- lm(price~ 
                 view+waterfront+condition+yr_renovated # qualitative predictors (non-geographic)
               +bedrooms+bathrooms+sqft_living+sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+sqft_lot15 # quantiative predictors (house features)
               +lat+long+zipcode+Month, # geographic predictors 
               data=data)
  
  return(model1)
}

# 8. Goodness of Fit
step8_gof <- function(data, plot_flag = 0) {
  standard_residuals1=rstandard(model1)
  
  if (plot_flag == 1) {
    plot(model1$fitted.values,standard_residuals1) #megaphone tendancy
    abline(0,0)
    hist(standard_residuals1,breaks=200)
    qqnorm(standard_residuals1)#heavy tailed
    qqline(standard_residuals1) #heavy tailed
    
    plot(data$bedrooms,standard_residuals1) #too much variance
    plot(data$bathrooms,standard_residuals1) # looks linear
    plot(data$sqft_living,standard_residuals1) #issue with values with higher sqft
    plot(data$sqft_lot,standard_residuals1) #Inverted megaphone
    plot(data$floors,standard_residuals1)# relatively linear
    plot(data$grade,standard_residuals1) #megaphone tendancy
    plot(data$sqft_above,standard_residuals1)#issue with values with higher sqft
    plot(data$yr_built,standard_residuals1)# looks linear
    plot(data$sqft_living15,standard_residuals1)#issue with values with higher sqft
    plot(data$sqft_lot15,standard_residuals1) #Inverted megaphone
    
    plot(data$lat,standard_residuals1) #looks evenly distributed
    plot(data$long,standard_residuals1) #feels like a random distribution
  }
  
  
  #look for a response transformation
  library(MASS)
  bc=boxcox(model1)
  lambda <- bc$x[which.max(bc$y)]
  print(paste0("The value of lambda from boxcox is ", lambda))
  #Lambda is close to 0 hence we can use the log transformation
  #Trying multiple variations of transformation give us the following predictor transformations
  
  data['logprice']=log(data['price'])
  data['Transformed_sqft_lot']=(data['sqft_lot']^0.3-1)/0.3
  data['Transformed_sqft_lot15']=(data['sqft_lot15']^0.3-1)/0.3
  
  
  model2 <- lm(logprice~ 
                 view+waterfront+condition+yr_renovated # qualitative predictors (non-geographic)
               +bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15 # quantiative predictors (house features)
               +lat+long+zipcode+Month, # geographic predictors 
               data=data)
  standard_residuals2=rstandard(model2)
  
  if (plot_flag == 1) {
    plot(model2$fitted.values,standard_residuals2)
    abline(0,0)
    hist(standard_residuals2,breaks=200)
    qqnorm(standard_residuals2)
    qqline(standard_residuals2)
    
    plot(data$bedrooms,standard_residuals2)
    plot(data$bathrooms,standard_residuals2)
    plot(data$sqft_living,standard_residuals2)
    plot(data$Transformed_sqft_lot,standard_residuals2)
    plot(data$floors,standard_residuals2)
    plot(data$grade,standard_residuals2)
    plot(data$sqft_above,standard_residuals2)
    plot(data$yr_built,standard_residuals2)
    plot(data$sqft_living15,standard_residuals2)
    plot(data$Transformed_sqft_lot15,standard_residuals2)
    
    plot(data$lat,standard_residuals2) #looks evenly distributed
    plot(data$long,standard_residuals2) #feels like a random distribution
  }
  
  return(model2)
}
step8_log_transform <- function(data) {
  data['logprice']=log(data['price'])
  data['Transformed_sqft_lot']=(data['sqft_lot']^0.3-1)/0.3
  data['Transformed_sqft_lot15']=(data['sqft_lot15']^0.3-1)/0.3
  return(data)
}

# 9. Remove outliers
step9_outlier_removal <- function(data) {
  #outlier detection and removal
  cook_dist=cooks.distance(model2)
  barplot(cook_dist)
  
  # justify use 0.001
  cookdist2=cook_dist[cook_dist < 0.005]
  barplot(cookdist2)
  length(cookdist2)
  
  data_removeOutlier=data[cook_dist < 0.005,]
  data=data_removeOutlier[,c('logprice','Month','bedrooms','bathrooms','sqft_living','Transformed_sqft_lot','floors','waterfront','view','condition','grade','sqft_above','sqft_basement','yr_built','yr_renovated','zipcode','lat','long','sqft_living15','Transformed_sqft_lot15')]
  return(data)
}

# 10. Re-fit Model and get GOF
step10_removed_outlier_model <- function(data, plot_flag = 0) {
  model3 <- lm(logprice~ 
                 view+waterfront+condition+yr_renovated # qualitative predictors (non-geographic)
               +bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15 # quantiative predictors (house features)
               +lat+long+zipcode+Month, # geographic predictors 
               data=data)
  
  standard_residuals3=rstandard(model3)
  if (plot_flag == 1) {
    plot(model3$fitted.values,standard_residuals3)
    abline(0,0)
    hist(standard_residuals3,breaks=200)
    qqnorm(standard_residuals3)
    qqline(standard_residuals3)
    
    plot(data$bedrooms,standard_residuals3)
    plot(data$bathrooms,standard_residuals3)
    plot(data$sqft_living,standard_residuals3)
    plot(data$Transformed_sqft_lot,standard_residuals3)
    plot(data$floors,standard_residuals3)
    plot(data$grade,standard_residuals3)
    plot(data$sqft_above,standard_residuals3)
    plot(data$yr_built,standard_residuals3)
    plot(data$sqft_living15,standard_residuals3)
    plot(data$Transformed_sqft_lot15,standard_residuals3)
    
    plot(data$lat,standard_residuals3) #looks evenly distributed
    plot(data$long,standard_residuals3) #feels like a random distribution
  }
  
  
  return(model3)
}

# 11. Split into training / test
step11_train_test_split <- function(data) {
  smp_size <- floor(0.75 * nrow(data))
  
  ## set the seed to make your partition reproducible
  set.seed(2711)
  train_ind <- sample(seq_len(nrow(data_removeOutlier_clean)), size = smp_size)
  
  return(train_ind)
}

# Step 1 - Read in the data
data <- step1_read_data("kc_house_data.csv")

# Step 2 - Apply factor type and data cleansing for qualitative variables
data <- step2_factor_analysis(data)

# Step 3 - Box plots for qualitative variables
step3_boxplots1(data)
step3_boxplots2(data)

# Step 4 - Correlation plot for all predicting variables
step4_correlation_plot(data)

# Step 5 - Zipcode Distribution Analysis
step5_zipcode_analysis(data)

# Step 6 - Year ANOVA analysis - Not significant - Remove Years from model moving forward
step6_anova_years(data)

# Step 7 - Fit the full model before transformations have been applied
model1 <- step7_fitting_model(data)
summary(model1)

# Step 8 - Apply goodness of fit analysis to determine necessary transformations
# To view all plots resulting from GOF analysis, change plot_flag to 1
model2 <- step8_gof(data, plot_flag = 0)
# Add columns to dataset with transformed variables
data <- step8_log_transform(data)

# Step 9 - Identify outliers and remove from dataset
data_removeOutlier_clean <- step9_outlier_removal(data)

# Step 10 - Re-fit model with outliers removed
# To view all plots resulting from GOF analysis, change plot_flag to 1
model3 <- step10_removed_outlier_model(data_removeOutlier_clean, plot_flag = 0)
summary(model3)

# Step 11 - Split data into Train /  Test and get indices
train_ind <- step11_train_test_split(data_removeOutlier_clean)

train <- data_removeOutlier_clean[train_ind, ]
test <- data_removeOutlier_clean[-train_ind, ]

# Step 12 - Forward and Backward Stepwise Regression

## Forward
forward_step <- step(lm(logprice~ zipcode, data=train), scope = list(lower= logprice~ zipcode, data=train, upper= logprice~view+waterfront+condition+yr_renovated+bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15+lat+long+zipcode+Month, data=train), direction = "forward", k=2, trace = FALSE)
AIC_VAL_forward =cbind(forward_step$anova[1],forward_step$anova[6])

## Backward
backward_step <- step(lm(logprice~view+waterfront+condition+yr_renovated+bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15+lat+long+zipcode+Month, data=train), scope = list(lower=logprice~ zipcode, upper = logprice~view+waterfront+condition+yr_renovated+bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15+lat+long+zipcode+Month, data=train), direction = "backward", k=2, trace = FALSE)
AIC_VAL_backward=cbind(backward_step$anova[1],backward_step$anova[6])

# Step 13 - LASSO
## alpha = 1 for lasso
price.scaled = scale(train$logprice)
Xpred= cbind(train$view, train$waterfront, train$condition, train$yr_renovated, train$bedrooms, train$bathrooms, train$sqft_living, train$Transformed_sqft_lot, train$floors, train$grade, train$sqft_above, train$sqft_basement, train$yr_built, train$sqft_living15, train$Transformed_sqft_lot15, train$lat, train$long, train$zipcode, train$Month)
colnames(Xpred) <- c("view", "waterfront", "condition", "yr_renovated", "bedrooms", "bathrooms","sqft_living", "Transformed_sqft_lot", "floors", "grade", "sqft_above", "sqft_basement", "yr_built", "sqft_living15", "Transformed_sqft_lot15", "lat", "long", "zipcode", "Month")
# Find the optimal lambda using 10-fold CV 
help(model.matrix)
test.matrix <- model.matrix(logprice~view+waterfront+condition+yr_renovated+bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15+lat+long+zipcode+Month, data=train)[,-1]
pricemodel.cv=cv.glmnet(test.matrix,train$logprice,alpha=1,nfolds=10)
## Fit lasso model with 100 values for lambda
pricemodel = glmnet(test.matrix, train$logprice, alpha = 1, nlambda = 100)
## Extract coefficients at optimal lambda
coef(pricemodel,s=pricemodel.cv$lambda.min)
plot(pricemodel,xvar="lambda",lwd=2, main = "LASSO")
abline(v=log(pricemodel.cv$lambda.min),col='black',lty = 2,lwd=2)

# Step 14 - Elastic Net
## alpha = 0.5 (or other values different from 0,1) for elastic net
# Find the optimal lambda using 10-fold CV 
pricemodel.cv=cv.glmnet(test.matrix,train$logprice,alpha=0.5,nfolds=10)
## Fit lasso model with 100 values for lambda
pricemodel = glmnet(test.matrix, train$logprice, alpha = 0.5, nlambda = 100)
## Extract coefficients at optimal lambda
coef(pricemodel,s=pricemodel.cv$lambda.min)
## Plot coefficient paths
plot(pricemodel,xvar="lambda",lwd=2, main = "Elastic Net")
abline(v=log(pricemodel.cv$lambda.min),col='black',lty = 2,lwd=2)

# LME Full Model
model4 <- lmer(logprice~ 
                 view+waterfront+condition+yr_renovated # qualitative predictors (non-geographic)
               +bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15 # quantiative predictors (house features)
               +lat+long+Month # geographic predictors 
               +(1|zipcode), # linear mixed effects model with zipcode as the random effect
               data=data)

summary(model4)
## run Lasso on LME

library(glmmLasso)
colnames(data)[c(7,8,9,10,11,15,16,18,21,22,23,24,26,27)]
scale_data_cols <- c(7,8,9,10,11,15,16,18,21,22,23,24,26,27)
data[,scale_data_cols]<-scale(data[,scale_data_cols],center=TRUE,scale=TRUE)
lme.lasso.model <- glmmLasso(fix = logprice~ view+waterfront+condition+yr_renovated+bedrooms+bathrooms+sqft_living+Transformed_sqft_lot+floors+grade+sqft_above+sqft_basement+yr_built+sqft_living15+Transformed_sqft_lot15+lat+long+Year+Month
          ,rnd=list(zipcode=~1)
          ,lambda = 10
          ,data = data)
summary(lme.lasso.model)

formula_base='price~(1|zipcode)'
symbol_sum='+'
columns=c('view','waterfront','condition','yr_renovated','bedrooms','bathrooms','sqft_living','Transformed_sqft_lot','floors','grade','sqft_above','sqft_basement','yr_built','sqft_living15','Transformed_sqft_lot15','lat','long','Month')

i=1
model_mixed=lmer(formula_base,data=data)
basemodelAIC=AIC(logLik(model_mixed))
AIC_add_list=list('base'=basemodelAIC)
while (TRUE){
  model_mixed=lmer(formula_base,data=data)
  basemodelAIC=AIC(logLik(model_mixed))
  AIClist=as.vector(basemodelAIC)
  
  for (val in columns){
    i=i+1
    model_temp=lmer(paste(formula_base,symbol_sum,val),data=data)
    AIClist[i]=AIC(logLik(model_temp))
  }
  
  i=1
  index=which(AIClist == min(AIClist))[[1]]   
  if (index>1){
    
    AIC_add_list[columns[index-1]]=min(AIClist)
    formula_base=paste(formula_base,symbol_sum,columns[index-1])
    print(formula_base)
    print(min(AIClist))
    columns = columns[columns!= columns[index-1]]
    print(columns)
  }
  else{
    if(length(columns)==0){
      break
    }
    else{
      break
    }
    
  }
}

# Step 15 - Prediction
# LM Full
predict.testdata = predict(model3,test,interval=c("prediction"))

price.pred = predict.testdata[,1]
price.lwr = predict.testdata[,2]
price.upr = predict.testdata[,3]

### Mean Squared Prediction Error (MSPE)
mean((price.pred-train$logprice)^2)

### Precision Measure (PM)
sum((price.pred-train$logprice)^2)/sum((train$logprice-mean(train$logprice))^2)

# LME Full
predict.testdata = predict(model4,test)

price.pred = predict.testdata

### Mean Squared Prediction Error (MSPE)
mean((price.pred-train$logprice)^2)

### Precision Measure (PM)
sum((price.pred-train$logprice)^2)/sum((train$logprice-mean(train$logprice))^2)

model5 <- lm(logprice~ 
               view+waterfront+condition # qualitative predictors (non-geographic)
             +bedrooms+sqft_living+Transformed_sqft_lot+grade+sqft_basement+sqft_living15 # quantiative predictors (house features)
             +zipcode+Month, # geographic predictors 
             data=data)

model6 <- lmer(logprice~ 
                 view+waterfront+condition # qualitative predictors (non-geographic)
               +bedrooms+sqft_living+Transformed_sqft_lot+grade+sqft_basement+sqft_living15 # quantiative predictors (house features)
               +Month # geographic predictors 
               +(1|zipcode), # linear mixed effects model with zipcode as the random effect
               data=data)

# LM Reduced
predict.testdata = predict(model5,test,interval=c("prediction"))

price.pred = predict.testdata[,1]
price.lwr = predict.testdata[,2]
price.upr = predict.testdata[,3]

### Mean Squared Prediction Error (MSPE)
mean((price.pred-train$logprice)^2)

### Precision Measure (PM)
sum((price.pred-train$logprice)^2)/sum((train$logprice-mean(train$logprice))^2)

# LME Full
predict.testdata = predict(model6,test)

price.pred = predict.testdata

### Mean Squared Prediction Error (MSPE)
mean((price.pred-train$logprice)^2)

### Precision Measure (PM)
sum((price.pred-train$logprice)^2)/sum((train$logprice-mean(train$logprice))^2)


standard_residuals1=rstandard(model1)
standard_residuals2=rstandard(model2)

p<-ggplot(data, aes(x=sqft_lot, y=standard_residuals1)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p1 <- p + labs(title="Before", x ="Sqft Lot", y = "Standardized Residuals")
p<-ggplot(data, aes(x=Transformed_sqft_lot, y=standard_residuals2)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p2 <- p + labs(title="After", x ="Transformed Sqft Lot", y = "Standardized Residuals")
plot_grid(p1, p2, nrow = 2, ncol = 1, labels = "AUTO")



p<-ggplot(data, aes(x=sqft_lot15, y=standard_residuals1)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p3 <- p + labs(title="Before", x ="Sqft Lot 15", y = "Standardized Residuals")
p<-ggplot(data, aes(x=Transformed_sqft_lot15, y=standard_residuals2)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p4 <- p + labs(title="After", x ="Transformed Sqft Lot 15", y = "Standardized Residuals")
plot_grid(p3, p4, nrow = 2, ncol = 1, labels = "AUTO")

p <- ggplot(data, aes(sample = standard_residuals1))
p5 <- p + stat_qq() + stat_qq_line() + labs(title="Before")
p <- ggplot(data, aes(sample = standard_residuals2))
p6 <- p + stat_qq() + stat_qq_line() + labs(title="After")
plot_grid(p5, p6, nrow = 2, ncol = 1, labels = "AUTO")

p <- ggplot(data, aes(x = model1$fitted.values, y = standard_residuals1)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p7 <- p + labs(title="Before", x ="Fitted Values", y = "Standardized Residuals")
p <- ggplot(data, aes(x = model2$fitted.values, y = standard_residuals2)) + geom_point(show.legend = FALSE) + geom_hline(yintercept=0)
p8 <- p + labs(title="After", x ="Fitted Values", y = "Standardized Residuals")
plot_grid(p7, p8, nrow = 2, ncol = 1, label = "AUTO")

p <- ggplot(data, aes(x = standard_residuals1)) + geom_histogram(bins = 300, show.legend = FALSE)
p9 <- p + labs(title="Before", x = "Standardized Residuals")
p <- ggplot(data, aes(x = standard_residuals2)) + geom_histogram(bins = 300, show.legend = FALSE)
p10 <- p + labs(title="After", x = "Standardized Residuals")
plot_grid(p9, p10, nrow = 2, ncol = 1, label = "AUTO")

AIC_VAL_forward

AIC_VAL_forward$ind <- as.numeric(row.names(AIC_VAL_forward))
p <- ggplot(AIC_VAL_forward, aes(x = ind, y = AIC)) + geom_point(show.legend = FALSE)
p11 <- p + labs(title="Elbow Plot for LM Forward Stepwise Model") + scale_x_discrete(limits=AIC_VAL_forward$Step) + theme(axis.text.x = element_text(angle=90))
p11

names <- colnames(as.data.frame(AIC_add_list))
AIC_add_list_df <- as.data.frame(names, stringsAsFactors = FALSE)
AIC_add_list_df$val <- as.numeric(AIC_add_list)
AIC_add_list_df$ind <- as.numeric(row.names(AIC_add_list_df))
colnames(AIC_add_list_df) <- c("Step", "AIC", "ind")
p <- ggplot(AIC_add_list_df, aes(x = ind, y = AIC)) + geom_point(show.legend = FALSE)
p12 <- p + labs(title="Elbow Plot for LME Forward Stepwise Model") + scale_x_discrete(limits=AIC_add_list_df$Step) + theme(axis.text.x = element_text(angle=90))
p12
# par(mfrow(1, 2))
# cook_dist <- cooks.distance(model2)
# barplot(cook_dist, main = "Cooks Distances before removal")
# # justify use 0.005
# cookdist2=cook_dist[cook_dist < 0.005]
# barplot(cookdist2, main = "Cooks Distances after removal")
# length(cookdist2)

