install.packages("caret")
install.packages("rockchalk")
library(readr)
library(caret)
library(rockchalk)

# First, read the data. Simultaneously select only the explanatory variables we are interested in,
data1 <- subset(read_csv("~/Downloads/train.csv"), select = c(SalePrice, GarageType, GarageYrBlt, YearBuilt, GarageFinish, GarageCars, GarageArea, GarageQual, GarageCond, LotArea, MSZoning, GrLivArea))

# Next, omit all null values in our data.
data <- na.omit(data1)

# Ensure that each categorical variables will be read as a factor.
data$GarageType <-  as.factor(data$GarageType)
data$GarageFinish <-  as.factor(data$GarageFinish)
data$GarageCars <-  as.factor(data$GarageCars)
data$GarageQual <-  as.factor(data$GarageQual)
data$GarageCond <-  as.factor(data$GarageCond)
data$MSZoning <-  as.factor(data$MSZoning)


#Now, summarize the data.
summary(data)


#Make plots that will allow us to observe and hypothesis trends. Put them side-by-side.
par(mfrow = c(3, 4))
boxplot(SalePrice ~ GarageType, data = data, main = "SalePrice by GarageType", xlab = "Garage Type", ylab = "Sale Price (dollars)")
plot(SalePrice ~ GarageYrBlt, data = data, main = "SalePrice by GarageYrBlt", xlab = "Year Built", ylab = "Sale Price (dollars)")
plot(SalePrice ~ YearBuilt, data = data, main = "SalePrice by YearBuilt", xlab = "Year Built", ylab = "Sale Price (dollars)")
boxplot(SalePrice ~ GarageFinish, data = data, main = "SalePrice by GarageFinish", xlab = "Garage Finish", ylab = "Sale Price (dollars)")
boxplot(SalePrice ~ GarageCars, data = data, main = "SalePrice by GarageCars", xlab = "Numbers of Cars in Garage", ylab = "Sale Price (dollars)")
plot(SalePrice ~ GarageArea, data = data, main = "SalePrice by GarageArea", xlab = "Garage Area (sq feet)", ylab = "Sale Price (dollars)")
boxplot(SalePrice ~ GarageQual, data = data, main = "SalePrice by GarageQual", xlab = "Garage Quality", ylab = "Sale Price (dollars)")
boxplot(SalePrice ~ GarageCond, data = data, main = "SalePrice by GarageCond", xlab = "Garage Condition", ylab = "Sale Price (dollars)")
plot(SalePrice ~ LotArea, data = data, main = "SalePrice by LotArea", xlab = "Lot Area (sq feet)", ylab = "Sale Price (dollars)")
boxplot(SalePrice ~ MSZoning, data = data, main = "SalePrice by MSZoning", xlab = "MS Zoning", ylab = "Sale Price (dollars)")
plot(SalePrice ~ GrLivArea, data = data, main = "SalePrice by GrLivArea", xlab = "Above Ground Living Area", ylab = "Sale Price (dollars)")
plot(0,type='n',axes=FALSE,ann=FALSE)



#model selection according to cp
library(leaps)
best = regsubsets(SalePrice~.,data = data)
summary(best)
cp = summary(best)$cp
#indicate a model with 8 variables: GarageTypeAttchd, GarageTypeDetchd, YearBuilt,
#GarageFinishRFn,GarageFinishUnf,GarageCars3,LotArea,GrLivArea

#combining levels for un

#doing k-fold cross validation for getting a test set
set.seed(0106)
train_control <- trainControl(method = "cv", number = 10)

# building the model
model <- train(SalePrice~., data = data,
               trControl = train_control,
               method = "lm")
print(model)
model$finalModel
model$resample





















