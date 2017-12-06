rc<-read.csv("多元回归-数据.csv")#read the data, saved as a frame data
rc
x1=rc[,2]
x2=rc[,3]
x3=rc[,4]
x4=rc[,5]
x5=rc[,6]
y=rc[,7]#assign the corresponding values of the rows of the frame data to independent variables
#-------------------------------------------------------------------------------------
#simple linear regression
lm1=lm(y~x1+x2+x3+x4+x5)#simple linear regression
summary(lm1)#show the consequence of simple linear regression

lm1.step=step(lm1)#stepwise regression
summary(lm1.step)#show the consequence of stepwise regression

drop1(lm1.step)#another function for stepwise regression

lm1.opt=lm(y~x1+x4)#simple regression by selected independent variable
summary(lm1.opt)#show the final consequence of the linear regression
#-------------------------------------------------------------------------------------
#ridge regression
#install.packages('MASS')
library(MASS)
#help("lm.ridge")

lm2=lm.ridge(y~x1+x2+x3+x4+x5,lambda = seq(0,300,length = 301),model = TRUE)
#do the ridge regression
names(lm2)#show the names of variables of the consequence of the ridge regression
lm2$lambda[which.min(lm2$GCV)]#find the lambdaGCV when the GCV get to the minimum
lm2$coef[which.min(lm2$GCV)]#find the corresponding coefficient whenthe GCV get to the minimum
#-------------------------------------------------
#plot the image of ridge regression and mark the vertical line where the lambdaGCV get to the minimum
par(mfrow = c(1, 2))
matplot(lm2$lambda, t(lm2$coef), xlab = expression(lamdba), ylab = "Cofficients", 
        type = "l", lty = 1:20)
abline(v = lm2$lambda[which.min(lm2$GCV)])
#-------------------------------------------------
#plot the image of the relationship between lambda and GCV
plot(lm2$lambda, lm2$GCV, type = "l", xlab = expression(lambda), 
     ylab = expression(beta))
abline(v = lm2$lambda[which.min(lm2$GCV)])
par(mfrow = c(1, 1))
#-------------------------------------------------
#select the ridge parameter via linearRidge function
#install.packages('ridge')
#help("linearRidge")
rc2=data.frame(x1,x2,x3,x4,x5)
library(ridge)
mod=linearRidge(y~.,data=rc2,nPCs=2)#the number of principal components is 2
summary(mod)
#-------------------------------------------------------------------------------------
#lasso
#install.packages('lars')
library(lars)
x = as.matrix(rc[, 2:6])
y = as.matrix(rc[, 7])
(laa = lars(x, y, type = "lar"))
plot(laa)#plot the image of lasso
summary(laa)#show the consequence of lasso
