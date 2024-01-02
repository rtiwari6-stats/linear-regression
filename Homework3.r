#Homework 3 STAT 608

#problem1

#part A
#a
library(car)
setwd("~/tamu/MS-STAT-2022/608")
adRevenue = read.csv("AdRevenue.csv")
y = adRevenue$AdRevenue
x = adRevenue$Circulation
plot(x,y, main = "Circulation v/s AdRevenue", xlab = "Circulation (in millions)",
     ylab = "AdRevenue in thousands of dollars")

#Without Transformations
lin_model = lm(y~x)
abline(lin_model)
par(mfrow = c(2,2))
plot(lin_model)
summary(lin_model)

#shows lack of normality
shapiro.test(y)
shapiro.test(x)

#Check if we need to transform X
summary(p1 <- powerTransform(x)) #power is -0.31, we need to transform

#fit model on transformed x
lin_model_bc = lm(y ~ bcPower(x, p1$lambda))
summary(lin_model_bc)

#now transform y to make residuals normal
summary(p2 <- powerTransform(lin_model_bc)) #rounded power is -0.73, we need to transform

#Final model
lin_model_bc = lm(bcPower(y, p2$lambda) ~ bcPower(x, p1$lambda))
summary(lin_model_bc)
par(mfrow = c(1,1))
plot(bcPower(x, p1$lambda),bcPower(y, p2$lambda ), main = "Circulation v/s AdRevenue Box-Cox transformed", 
     xlab = "Circulation (in millions) ^ -0.31",
     ylab = "AdRevenue in thousands of dollars ^ -0.69")
abline(lin_model_bc)
par(mfrow = c(2,2))
plot(lin_model_bc)
par(mfrow = c(1,1))

#b
yt = log(y)
xt = log(x)
lin_model_transformed = lm(yt ~ xt)
sigma = 0.308
newdata = data.frame(xt = c(log(0.5)))
exp(predict(lin_model_transformed, newdata, interval = 'prediction', level=0.95) + (sigma^2/2))

newdata = data.frame(xt = c(log(20)))
exp(predict(lin_model_transformed, newdata, interval = 'prediction', level=0.95)+ (sigma^2/2))

#problem 6
x = c(-5,-3,-1,2,9)
y = c(3,7,8,11,1)
plot(x,y)
mod = lm(y~x)
mod$residuals
hatvalues(mod)

#problem 8
summary(faithful)
head(faithful)

#part a
plot(faithful$waiting, faithful$eruptions, main = "Dataset Faithful: Waiting v/s eruptions",
     xlab = "Waiting", ylab = "Eruptions")
shapiro.test(faithful$eruptions)
shapiro.test(faithful$waiting)
lm_faithful = lm(faithful$eruptions ~ faithful$waiting)
summary(lm_faithful)
abline(lm_faithful)
par(mfrow = c(2,2))
plot(lm_faithful)
par(mfrow = c(1,1))
#Cook's distance
cd<-cooks.distance(lm_faithful)
par(mar=c(5,5,3,3), cex.axis=1.5,cex.lab=1.5,cex.main=3,pch=19, mfrow=c(1,1))
plot(faithful$waiting,cd, ylab="Cook's Distance")
abline(h=(4/(nrow(faithful)-2)), lty=2)


#part b
summary(p1 <- powerTransform(faithful$eruptions))
#summary(p2 <- powerTransform(faithful$waiting))
par(mfrow = c(1,1))
#lin_model_faithful_bc = lm(bcPower(faithful$eruptions, p1$lambda ) ~ bcPower(faithful$waiting, p2$lambda))

lin_model_faithful_bc = lm(bcPower(faithful$eruptions, p1$lambda ) ~ faithful$waiting)
summary(lin_model_faithful_bc)
plot(faithful$waiting, bcPower(faithful$eruptions, p1$lambda ), main = "Dataset Faithful (transformed): Waiting v/s eruptions",
     xlab = "Waiting", ylab = "Eruptions^1.51")
abline(lin_model_faithful_bc)
par(mfrow = c(2,2))
plot(lin_model_faithful_bc)

#part c
eruptions = faithful$eruptions
waiting = faithful$waiting
lm_sqrt = lm(sqrt(eruptions)~waiting)
anova(lm_sqrt)
newdata = data.frame(waiting = c(55))
predict(lm_sqrt, newdata, interval = 'confidence', level=0.95)^2 + 0.0193

#gam 
library(mgcv)
gam = mgcv::gam(faithful$eruptions ~s(faithful$waiting, k=25, bs="cr"))
plot(faithful$waiting, faithful$eruptions, main = "Dataset Faithful: Waiting v/s eruptions",
     xlab = "Waiting", ylab = "Eruptions")
ord <- order(faithful$waiting)
lines(faithful$waiting[ord], fitted(gam)[ord])