############################## Read a data into R ##############################
senic <- read.csv("C:/Users/91630/Downloads/SENIC.csv", header=TRUE)
head(senic)

# Check for missing values in the entire dataset
missing_values <- sum(is.na(senic))

# Display the number of missing values
cat("Number of missing values in the dataset:", missing_values, "\n")
### No missing values


##### Analysing response variable 
par(mfrow= c(1,1))
hist(senic$Y)
boxplot(senic$Y)


########################### 1. Introduction ###################################
###################### 1.1 Exploratory Data Analysis. ##########################
## 1. Histograms of Y and Xs
library(dplyr)


par(mfrow= c(3,4))
for (col in c(names(senic))){
  senic %>% pull(col) %>% hist(main= col)
}


library(e1071)
skew_summary <- sapply(senic, function(x) skewness(x))
skew_summary


## 2. Boxplots of Y and Xs

par(mfrow= c(3,4))
for (col in c(names(senic))){
  senic %>% pull(col) %>% boxplot(main= col)
}




## 3. Summary Statistics 
summary(senic)



## 4. Scatter Plot Matrix 
pairs(senic, col= "#FF1493E2", main = "Scatter-Plot matrix of SENIC data")

par(mfrow=c(3,4))
plot(Y~X1, senic,col="blue", main="Scatter-Plot between Y and X1")
plot(Y~X2, senic,col="blue", main="Scatter-Plot between Y and X2")
plot(Y~X3, senic,col="blue", main="Scatter-Plot between Y and X3")
plot(Y~X4, senic,col="blue", main="Scatter-Plot between Y and X4")
plot(Y~X5, senic,col="blue", main="Scatter-Plot between Y and X5")
plot(Y~X6, senic,col="blue", main="Scatter-Plot between Y and X6")
plot(Y~X7, senic,col="blue", main="Scatter-Plot between Y and X7")
plot(Y~X8, senic,col="blue", main="Scatter-Plot between Y and X8")
plot(Y~X9, senic,col="blue", main="Scatter-Plot between Y and X9")
plot(Y~X10, senic,col="blue", main="Scatter-Plot between Y and X10")


## 5. Added-Variable Plots
library(car)
dev.off()
senic.lmfit <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = senic)
avPlots(senic.lmfit)


## 6. correlation matrix
library(caret)
library(corrplot)

#dev.new()
correlation_matrix <- cor(senic)
correlation_matrix
# Create a correlation plot
corrplot(correlation_matrix, method = "circle", diag = TRUE, tl.cex = 0.8)




########################### 2. Model/Methods ###################################
## Fit a regression model with all of the predictors
full.lmfit <- lm(Y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10, data = senic)
summary(full.lmfit)

anova(full.lmfit)


## Model Selection (Stepwise Regression)
# Install packages for the model selection 
# install.packages("leaps")
# install.packages("HH")
# install.packages("StepReg")
# Load HH, leaps, and StepReg packages
library(leaps)
library(HH)
library(StepReg)





#### Stepwise Regression
par(mfrow=c(3,2))
library(olsrr)
b<- ols_step_all_possible(full.lmfit )
plot(b)


b.adjr = data.frame(n=b$n,predictors=b$predictors,adjr=b$adjr)
#print(b.adjr)
print(b.adjr[c(638, 848, 968, 1013, 1023),])


b.cp = data.frame(n=b$n,predictors=b$predictors,cp=b$cp)
#print(b.cp)
print(b.cp[c(638, 848, 968, 1013, 1023),])



b.aic = data.frame(n=b$n,predictors=b$predictors,aic=b$aic)
#print(b.aic)
print(b.aic[c(638, 848, 968, 1013, 1023),])


b.press = data.frame(n=b$n,predictors=b$predictors,press=b$msep)
#print(b.press)
print(b.press[c(638, 848, 968, 1013, 1023),])


k <- ols_step_both_p(full.lmfit,pent=0.10,prem=0.1,details=TRUE)
plot(k)







#### Checking for correlated variables
senic_data <- cbind(senic$Y, x1, x2, x4, x5, x7, x8, x9)
senic_data <- as.data.frame(senic_data)

# Create a correlation plot
dev.new()
corrplot(cor_matrix, method = "circle", diag = TRUE, tl.cex = 0.8)

library(dplyr)
Cols<- c("X3", "X6", "X10")
senic<-senic[, -which(names(senic) %in% Cols)]

cor_matrix <- cor(senic)
cor_matrix
corrplot(cor_matrix, method = "circle", diag = TRUE, tl.cex = 0.8)


# As we can see, lots of variables are highly correlated.
#Therefore Standardization needed for our variables.

x1 <- (senic$X1 -mean(senic$X1))/sd(senic$X1)
x2 <- (senic$X2 -mean(senic$X2))/sd(senic$X2)
x3 <- (senic$X3 -mean(senic$X3))/sd(senic$X3)
x4 <- (senic$X4 -mean(senic$X4))/sd(senic$X4)
x5 <- (senic$X5 -mean(senic$X5))/sd(senic$X5)
x6 <- (senic$X6 -mean(senic$X6))/sd(senic$X6)
x7 <- (senic$X7 -mean(senic$X7))/sd(senic$X7)
x8 <- (senic$X8 -mean(senic$X8))/sd(senic$X8)
x9 <- (senic$X9 -mean(senic$X9))/sd(senic$X9)



#Now very few of them are highly correlated to each other compare to without standardization.
senic.itact.Std <- cbind(senic$Y,x1,x2,x4,x5,x7,x8,x9)
senic.itact.Std <- as.data.frame(senic.itact.Std) # Converting to data Frame.
head(senic.itact.Std)
colnames(senic.itact.Std)[1] <- "Y"



################ Fit a reduced regression model 
reduced.lmfit <- lm(Y ~ x1+x2+x4+x5+x7+x8+x9, data=senic.itact.Std)
summary(reduced.lmfit)




########3 Regression Diagnostics
res <- rstudent(reduced.lmfit)
fitted.y <- fitted(reduced.lmfit)



######## Residual Plots ##########
par(mfrow=c(2,4))
plot(res ~ senic$X1, xlab="X1", ylab="Residual", main="Residuals vs. X1")
abline(h=0)
plot(res ~ senic$X2, xlab="X2", ylab="Residual", main="Residuals vs. X2")
abline(h=0)
plot(res ~ senic$X4, xlab="X4", ylab="Residual", main="Residuals vs. X4")
abline(h=0)
plot(res ~ senic$X5, xlab="X5", ylab="Residual", main="Residuals vs. X5")
abline(h=0)
plot(res ~ senic$X7, xlab="X7", ylab="Residual", main="Residuals vs. X7")
abline(h=0)
plot(res ~ senic$X8, xlab="X8", ylab="Residual", main="Residuals vs. X8")
abline(h=0)
plot(res ~ senic$X9, xlab="X9", ylab="Residual", main="Residuals vs. X9")
abline(h=0)
plot(res ~ fitted.y, xlab="Fitted value", ylab="Residual", main="Residuals vs. Fitted Values")
abline(h=0)


######### Constancy of Error Variances #########
library(lmtest)
bptest(reduced.lmfit)


######### Multicollinearity ##########
vif(reduced.lmfit) 



########## performing transformations as we have high multicollinearity
install.packages("EnvStats")
library(EnvStats)

boxcox.summary <- boxcox(reduced.lmfit, optimize=TRUE)
lambda <- boxcox.summary$lambda
lambda

trans.Y <- senic$Y^lambda

senic <- cbind(senic,trans.Y)
senic




######### Re-fitting a model using the transformed response variable. ##########
boxcox.lmfit <- lm(trans.Y ~ X1 + X2 + X4 + X5+ X7+ X8 +X9, data=senic)
summary(boxcox.lmfit)

boxcox.res <- rstudent(boxcox.lmfit)

boxcox.fitted.y <- fitted(boxcox.lmfit)



############ Checking if transformation decreased the multicollinearity problem
library(car)
vif(boxcox.lmfit)



####### Transformation didn't decrease the multi collinearity problem 
## So removing highly correlated variable X5 and fitting the model again 
full.lmfit <- lm(Y ~ X1+X2+X3+X4+X6+X7+X8+X9+X10, data = senic)
summary(full.lmfit)



#### Stepwise Regression
par(mfrow=c(3,3))
library(olsrr)
b<- ols_step_all_possible(full.lmfit )
plot(b)


b.adjr = data.frame(n=b$n,predictors=b$predictors,adjr=b$adjr)
print(b.adjr)
print(b.adjr[c(256, 382, 466, 502, 511),])


b.cp = data.frame(n=b$n,predictors=b$predictors,cp=b$cp)
print(b.cp)
print(b.cp[c(256, 382, 466, 502, 511),])



b.aic = data.frame(n=b$n,predictors=b$predictors,aic=b$aic)
print(b.aic)
print(b.aic[c(256, 382, 466, 502, 511),])


b.press = data.frame(n=b$n,predictors=b$predictors,press=b$msep)
print(b.press)
print(b.press[c(256, 382, 466, 502, 511),])


k <- ols_step_both_p(full.lmfit,pent=0.10,prem=0.1,details=TRUE)
plot(k)


####### Fitting the reduced model
reduced.lmfit <- lm(Y ~ x1+x2+x4+x7+x8+x9, data=senic)
summary(reduced.lmfit)



########## Checking if removing x5 variable removed multi collinearity
vif(reduced.lmfit) 
###### yes it did 








################ Test for significance of the interaction terms
anova(reduced.lmfit, full.lmfit)

## Again Model Selection (Stepwise Regression) after including interaction terms.
#### Stepwise Regression
#### Adjusted R2
stepwise(data=senic.itact.Std,y="Y",select="adjRsq")
#### Cp
stepwise(data=senic.itact.Std,y="Y",select="CP")
#### AIC
stepwise(data=senic.itact.Std,y="Y",select="AIC")
#### BIC
stepwise(data=senic.itact.Std,y="Y",select="BIC")
################ Fit a regression model with interaction terms
full.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4 + x8x5, data = senic.itact.Std)
summary(full.lmfit)
anova(full.lmfit)
#In the summary table we have seen one interaction variable is have higher p-value than 0.1 so we will drop this 
column
full.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4, data = senic.itact.Std)
summary(full.lmfit)
################ Fit a regression model with no interaction terms
reduced.lmfit <- lm(Y ~ x2 + x7 + x8 + x9 + x1 + x4, data=senic.itact.Std)
summary(reduced.lmfit)
################ Test for significance of the interaction terms
anova(reduced.lmfit, full.lmfit)
# Based on the anova test, we will go with the full model as our final model.
#### Final Model
reduced.lmfit <- lm(Y ~ x2 + x7 + x2x8 + x2x5 + x8 + x9 + x1 + x4 + x2x4, data=senic.itact.Std)
summary(reduced.lmfit)









