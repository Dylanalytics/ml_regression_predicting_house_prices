library("tidyverse")
library("sqldf")
library("readxl")
library("dplyr")
library("readxl")
library("janitor")
library("car")
library("caret")
library("mice")
library("visdat")
library(naniar)

#update the wd when running this code from a different computer
setwd("C:\\Users\\dylan\\house_prices_competition")
getwd()

## Obtain Dataset from kaggle competition: https://www.kaggle.com/competitions/home-data-for-ml-course/overview
#import test and train data
train_data <- read.csv("train.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)
test_data <- read.csv("test.csv", header=TRUE, sep=",", stringsAsFactors = TRUE)

# determine number of records in each set and variables
nrow(train_data)
nrow(test_data)

str(train_data)
str(test_data)


# Combine both Train and Test set for data cleaning
test_data$SalePrice <- NA

combined_data.1 <- rbind(train_data, test_data)

str(combined_data.1)


str(train_data)

##################
#data exploration
##################
explore_train_data <- train_data

#look at Sales Price by Year. If we take Toronto over the past few years for instance, year will make a big difference
explore_train_data <- explore_train_data %>%
  select(YrSold, SalePrice) %>%
  group_by(YrSold) %>%
  summarise(avg_sale_price = mean(SalePrice))

ggplot(data = explore_train_data, aes(x = YrSold, y = avg_sale_price, group =1)) + geom_line()



#look at sales price by lot size
par(mfrow=c(1,1))
hist(explore_train_data$avg_sale_price)
#look at sales price by lot size
plot(SalePrice ~ LotArea, data=train_data)

#look at sales price by neighborhood
plot(SalePrice ~ Neighborhood, data=train_data)
hist(as.integer(train_data$YrSold))

##################
#Dataset Cleanup - Missing Data and Type Data
##################


#convert certain variables from numbers to factors for both train and test sets

var_to_convert <- c('MSSubClass','YearBuilt', 'YearRemodAdd', 'GarageYrBlt', 'MoSold', 'YrSold', 'GarageCars')
combined_data.1[,var_to_convert] <- lapply(combined_data.1[,var_to_convert], factor)



#identifying null values
view(miss_var_summary(combined_data.1))
miss_var_summary(combined_data.1)
vis_miss(combined_data.1)
vis_miss(combined_data.1, cluster = TRUE)
gg_miss_case(combined_data.1)



# replacing missing factor variable values with None, as most of these missing values appear to occur when the property doesn't have that feature
miss_var_summary(combined_data.1)

combined_data.1$PoolQC <- factor(ifelse(is.na(combined_data.1$PoolQC), "None", paste(combined_data.1$PoolQC)))
combined_data.1$GarageType <- factor(ifelse(is.na(combined_data.1$GarageType), "None", paste(combined_data.1$GarageType)))
combined_data.1$GarageYrBlt <- factor(ifelse(is.na(combined_data.1$GarageYrBlt), "None", paste(combined_data.1$GarageYrBlt)))
combined_data.1$GarageFinish <- factor(ifelse(is.na(combined_data.1$GarageFinish), "None", paste(combined_data.1$GarageFinish)))
combined_data.1$GarageQual <- factor(ifelse(is.na(combined_data.1$GarageQual), "None", paste(combined_data.1$GarageQual)))
combined_data.1$GarageCond <- factor(ifelse(is.na(combined_data.1$GarageCond), "None", paste(combined_data.1$GarageCond)))
combined_data.1$MiscFeature <- factor(ifelse(is.na(combined_data.1$MiscFeature), "None", paste(combined_data.1$MiscFeature)))
combined_data.1$Alley <- factor(ifelse(is.na(combined_data.1$Alley), "None", paste(combined_data.1$Alley)))
combined_data.1$Fence <- factor(ifelse(is.na(combined_data.1$Fence), "None", paste(combined_data.1$Fence)))
combined_data.1$FireplaceQu <- factor(ifelse(is.na(combined_data.1$FireplaceQu), "None", paste(combined_data.1$FireplaceQu)))
combined_data.1$BsmtCond <- factor(ifelse(is.na(combined_data.1$BsmtCond), "None", paste(combined_data.1$BsmtCond)))
combined_data.1$BsmtQual <- factor(ifelse(is.na(combined_data.1$BsmtQual), "None", paste(combined_data.1$BsmtQual)))
combined_data.1$BsmtExposure <- factor(ifelse(is.na(combined_data.1$BsmtExposure), "None", paste(combined_data.1$BsmtExposure)))
combined_data.1$BsmtFinType1 <- factor(ifelse(is.na(combined_data.1$BsmtFinType1), "None", paste(combined_data.1$BsmtFinType1)))
combined_data.1$BsmtFinType2 <- factor(ifelse(is.na(combined_data.1$BsmtFinType2), "None", paste(combined_data.1$BsmtFinType2)))
combined_data.1$MasVnrType <- factor(ifelse(is.na(combined_data.1$MasVnrType), "None", paste(combined_data.1$MasVnrType)))
combined_data.1$Electrical <- factor(ifelse(is.na(combined_data.1$Electrical), "None", paste(combined_data.1$Electrical)))
combined_data.1$GarageCars <- factor(ifelse(is.na(combined_data.1$GarageCars), "None", paste(combined_data.1$GarageCars)))
combined_data.1$KitchenQual <- factor(ifelse(is.na(combined_data.1$KitchenQual), "None", paste(combined_data.1$KitchenQual)))
combined_data.1$SaleType <- factor(ifelse(is.na(combined_data.1$SaleType), "None", paste(combined_data.1$SaleType)))
combined_data.1$Utilities <- factor(ifelse(is.na(combined_data.1$Utilities), "None", paste(combined_data.1$Utilities)))
combined_data.1$Exterior1st <- factor(ifelse(is.na(combined_data.1$Exterior1st), "None", paste(combined_data.1$Exterior1st)))
combined_data.1$Exterior2nd <- factor(ifelse(is.na(combined_data.1$Exterior2nd), "None", paste(combined_data.1$Exterior2nd)))
combined_data.1$Functional <- factor(ifelse(is.na(combined_data.1$Functional), "None", paste(combined_data.1$Functional)))

summary(combined_data.1$MSZoning) #this variable seems to have predictive power, so being a bit more careful with it by putting it to the mean
combined_data.1$MSZoning <- factor(ifelse(is.na(combined_data.1$MSZoning), "RL", paste(combined_data.1$MSZoning)))

# replace all integer missing values with mean values

combined_data.1$LotFrontage <- ifelse(is.na(combined_data.1$LotFrontage), mean(filter(combined_data.1, !is.na(LotFrontage))$LotFrontage), combined_data.1$LotFrontage)
combined_data.1$BsmtFullBath <- ifelse(is.na(combined_data.1$BsmtFullBath), mean(filter(combined_data.1, !is.na(BsmtFullBath))$BsmtFullBath), combined_data.1$BsmtFullBath)
combined_data.1$BsmtHalfBath <- ifelse(is.na(combined_data.1$BsmtHalfBath), mean(filter(combined_data.1, !is.na(BsmtHalfBath))$BsmtHalfBath), combined_data.1$BsmtHalfBath)
combined_data.1$BsmtFinSF1 <- ifelse(is.na(combined_data.1$BsmtFinSF1), mean(filter(combined_data.1, !is.na(BsmtFinSF1))$BsmtFinSF1), combined_data.1$BsmtFinSF1)
combined_data.1$MasVnrArea <- ifelse(is.na(combined_data.1$MasVnrArea), mean(filter(combined_data.1, !is.na(MasVnrArea))$MasVnrArea), combined_data.1$MasVnrArea)
combined_data.1$GarageArea <- ifelse(is.na(combined_data.1$GarageArea), mean(filter(combined_data.1, !is.na(GarageArea))$GarageArea), combined_data.1$GarageArea)
combined_data.1$BsmtFinSF2 <- ifelse(is.na(combined_data.1$BsmtFinSF2), mean(filter(combined_data.1, !is.na(BsmtFinSF2))$BsmtFinSF2), combined_data.1$BsmtFinSF2)
combined_data.1$BsmtUnfSF <- ifelse(is.na(combined_data.1$BsmtUnfSF), mean(filter(combined_data.1, !is.na(BsmtUnfSF))$BsmtUnfSF), combined_data.1$BsmtUnfSF)
combined_data.1$TotalBsmtSF <- ifelse(is.na(combined_data.1$TotalBsmtSF), mean(filter(combined_data.1, !is.na(TotalBsmtSF))$TotalBsmtSF), combined_data.1$TotalBsmtSF)



#Attempted to Impute Missing Values, didn't yield good results 
# imputed_data <- mice(combined_data.1, m=5, maxit = 30, meth='cart')
# imputed_data


#testing correlation between numerical variables

integer_columns <- names(select_if(combined_data.1, is.numeric))
factor_columns <-names(select_if(combined_data.1, is.factor))
integer_data <- combined_data.1 %>% select(all_of(integer_columns))


cor_data_train <-(round(cor(integer_data[1:1460,]),1))

write.csv(cor_data_train, file = "train data integer correlation v2.csv")
cor_data_train 

vis_cor(integer_data[1:1460,])

#Feature Engineering
combined_data.2 <- combined_data.1

combined_data.2$TotSF <- as.integer(combined_data.2$TotalBsmtSF + combined_data.2$GrLivArea + combined_data.2$X1stFlrSF +combined_data.2$X2ndFlrSF) #since all SF variables have strong positive corr with SalePrice

combined_data.2$TotBath <- as.integer(combined_data.2$FullBath + combined_data.2$HalfBath*0.5) #since number of bathrooms are a key feature in the house 



#testing correlations with new features
integer_columns.2 <- names(select_if(combined_data.2, is.numeric))
factor_columns.2 <-names(select_if(combined_data.2, is.factor))
integer_data.2 <- combined_data.2 %>% select(all_of(integer_columns.2))

str(combined_data.1)

cor_data_train.2 <-(round(cor(integer_data.2[1:1460,]),1))

write.csv(cor_data_train.2, file = "train data.2 integer correlation v3.csv")

#investigating categorical variables that are causing issues likely because the train dataset doesn't have the value found in the test set

# unique(combined_data.2[1:1460,"YearBuilt"])
# unique(combined_data.2[1461:2919,"YearBuilt"])

#dropping variables because they're causing issues and don't have much predictive power
combined_data.3 <- combined_data.2
combined_data.3 <-subset(combined_data.2, select = -c(MSSubClass, Utilities, YearBuilt, Exterior1st, Exterior2nd, KitchenQual, Functional, GarageYrBlt, GarageCars, SaleType))

#combined_data.3$SaleType <- fct_explicit_na(combined_data.3$SalesType, na_level = "None")




# combined_data.3$Exterior1st

#breakout train and test set, and cross-fold datasets for calculating MAPE


train_data.1 <- combined_data.3[1:1460,]
test_data.1 <- combined_data.3[1461:2919,]

cross_fold.train <- train_data.1[1:1000,]
cross_fold.test <- train_data.1[1001:1460,]


########################
#Create some basic linear regressions
########################

###############################
#create first linear regression 
###############################

reg.norm.all.1 <- lm(SalePrice ~., train_data.1)
summary(reg.norm.all.1)

par(mfrow=c(1,4)) 
plot(reg.norm.all.1)




#the q-q plot is showing that our dependent variable may be exponentially distributed, so let's apply a log function to it in an attmept to normalize the distribution.

reg.log.all.1 <- lm(log(SalePrice) ~., train_data.1)
summary(reg.log.all.1)

par(mfrow=c(1,4)) 
plot(reg.log.all.1)


# predict test data using our first "reg.log.all.1" regression to get some early result feedback
prediction_reg.log.all.1 <- exp(predict(reg.log.all.1, test_data.1))
write.csv(prediction_reg.log.all.1, file = "prediction_reg.log.all.1.csv")

#Calculate MAPE for first linear regression by using cross-fold 
reg.log.all.cross.1 <- lm(log(SalePrice) ~., cross_fold.train)
summary(reg.log.all.cross.1)
par(mfrow=c(1,4)) 
plot(reg.log.all.cross.1)

predicted.reg.log.all.cross.1 <- exp(predict(reg.log.all.cross.1, cross_fold.train))
percent.errors_predicted.reg.log.all.cross.1 <- abs((cross_fold.train$SalePrice - predicted.reg.log.all.cross.1)/cross_fold.train$SalePrice)*100
mean(percent.errors_predicted.reg.log.all.cross.1)

###############################
#Attempt Stepwise
###############################

#stepwise both - top 4% submission - this model yielded the best results for me on Kaggle
reg.log.cross.step.1 <- step(lm(log(SalePrice) ~. , cross_fold.train), direction ="both")

summary(reg.log.cross.step.1)
par(mfrow=c(1,4)) 
plot(reg.log.cross.step.1)

predicted.reg.log.cross.step.1 <- exp(predict(reg.log.cross.step.1, cross_fold.train))
percent.errors_predicted.reg.log.cross.step.1 <- abs((cross_fold.train$SalePrice - predicted.reg.log.cross.step.1)/cross_fold.train$SalePrice)*100
mean(percent.errors_predicted.reg.log.cross.step.1)

predict.prices.reg.log.cross.step.1 <- exp(predict(reg.log.cross.step.1, test_data.1))

write.csv(predict.prices.reg.log.cross.step.1, file = "predicted house prices predict.prices.reg.log.cross.step.1.csv")

#stepwise forward
reg.log.cross.step.forward.1 <- step(lm(log(SalePrice) ~. , cross_fold.train), direction ="forward")

summary(reg.log.cross.step.forward.1)
par(mfrow=c(1,4)) 
plot(reg.log.cross.step.forward.1)

predicted.reg.log.cross.step.forward.1 <- exp(predict(reg.log.cross.step.forward.1, cross_fold.train))
percent.errors_predicted.reg.log.cross.step.forward.1 <- abs((cross_fold.train$SalePrice - predicted.reg.log.cross.step.forward.1)/cross_fold.train$SalePrice)*100
mean(percent.errors_predicted.reg.log.cross.step.forward.1)
#data issues, couldn't run prediction without errors.
predict.prices.reg.log.cross.step.forward.1 <- exp(predict(reg.log.cross.step.forward.1, test_data.1))


#stepwise (forward) on full train-set

reg.log.cross.step.full.forward <- step(lm(log(SalePrice) ~. , train_data.1), direction ="forward")

summary(reg.log.cross.step.full.forward)
par(mfrow=c(1,4)) 
plot(reg.log.cross.step.full.forward)

predict.prices.reg.log.cross.step.full.forward <- exp(predict(reg.log.cross.step.full.forward, test_data.1))
#kaggle submission returned poor results
write.csv(predict.prices.reg.log.cross.step.full.forward, file = "predicted house prices predict.prices.reg.log.cross.step.full.forward.csv")


###############################
#Attempt Lasso & Ridge Regression 
#interactions
###############################
library(glmnet)


y <- log(cross_fold.train$SalePrice)
X <- model.matrix(Id~ OverallQual*MSZoning*Neighborhood*SaleCondition*TotSF*GrLivArea*TotBath + LotArea*OverallQual*MSZoning*Neighborhood*SaleCondition*TotSF*GrLivArea*TotBath + TotSF*MSZoning*Neighborhood*SaleCondition  + GrLivArea*MSZoning*Neighborhood*SaleCondition + TotBath*MSZoning*Neighborhood*SaleCondition + LotFrontage + Neighborhood + SaleCondition + MSZoning + LotArea, combined_data.3)[,-1]

X <-cbind(combined_data.3$Id,X)

#split into training, testing, prediction sets
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)


#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
plot(crossval,xlim=c(-8.5,-6),ylim=c(0.006,0.008)) # lets zoom-in
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing- cross_fold.test$SalePrice)/cross_fold.test$SalePrice*100) #calculate and display MAPE


#creating submission 

predict.prices.lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))

write.csv(predict.prices.lasso.testing, file = "predicted house prices predict.prices.lasso.testing.csv")






#ridge (alpha=0)
ridge.fit<-glmnet(x = X.training, y = y, alpha = 0)
plot(ridge.fit, xvar = "lambda")

#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.training, y = y, alpha = 0)
plot(crossval)
penalty.ridge <- crossval$lambda.min 
log(penalty.ridge) 
ridge.opt.fit <-glmnet(x = X.training, y = y, alpha = 0, lambda = penalty.ridge) #estimate the model with that
coef(ridge.opt.fit)

ridge.testing <- exp(predict(ridge.opt.fit, s = penalty.ridge, newx =X.testing))
mean(abs(ridge.testing-cross_fold.test$SalePrice)/cross_fold.test$SalePrice*100) 

#submit lasso score

predict.lasso.testing.log.all.1 <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.prediction))


write.csv(predict.lasso.testing.log.all.1, file = "lasso.testing.all.log.1.csv")




