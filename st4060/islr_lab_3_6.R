# Clear all variables
rm(list = ls())
# LoadLibraries function
LoadLibraries <- function(){
  library(MASS)
  library(ISLR2)
  print("The libraries have been loaded")
}
LoadLibraries()
head(Boston)
?Boston
names(Boston)
# Attach dataset or specify data in call to lm
lm.fit <- lm(medv ~ lstat, data=Boston)
attach(Boston)
lm.fit <- lm(medv ~ lstat)
summary(lm.fit)
names(lm.fit)
lm.fit$coefficients
# Confidence interval
confint(lm.fit)
# Prediction
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="confidence")
predict(lm.fit, data.frame(lstat=c(5, 10, 15)), interval="prediction")
# Plotting
plot(lstat, medv, col="blue", pch=3)
abline(lm.fit, lwd=3, col="red")
par(mfrow=c(2, 2))
plot(lm.fit)
# Residuals
plot(predict(lm.fit), residuals(lm.fit))
# Studentized residuals
plot(predict(lm.fit), rstudent(lm.fit))
# Leverage statistics
plot(hatvalues(lm.fit))
# Index of maximum value
which.max(hatvalues(lm.fit))

# Multiple Linear Regression
lm.fit <- lm(medv ~ lstat + age, data=Boston)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data=Boston)
summary(lm.fit)
?summary.lm
# Ignore one variable
lm.fit1 <- lm(medv ~ . - age, data=Boston)
lm.fit1 <- update(lm.fit, ~. - age)
summary(lm.fit1)

# Interaction terms
lm.fit <- lm(medv ~ lstat*age, data=Boston)
summary(lm.fit)
# Non-linear transformation of predictors
lm.fit <- lm(medv ~ lstat)
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
summary(lm.fit2)
anova(lm.fit, lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)
# Polynomial fit
lm.fit5 <- lm(medv ~ poly(lstat, 5, raw=FALSE))
summary(lm.fit5)
lm.fit5 <- lm(medv ~ poly(lstat, 5, raw=TRUE))
summary(lm.fit5)
# Qualitative predictors
head(Carseats)
lm.fit <- lm(Sales ~ . + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)