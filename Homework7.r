#problem 2
sigma2_v = 0.5
M=1000
n=50
rho=seq(from=0.0, to=0.9,by=0.1)
beta0 = 0.5
beta1 = 1.5
meany = beta0 + beta1*0.5 # mean y

#outer loop on rhos
#inner loop on simulations
# for each row, simulate x, simulate e and generate y
# call lm (y ~ x)
# construct a CI using predict
#check if sample mean response lies in CI
set.seed(43)
counts = rep(0, length(rho))
z=1
for(i in rho){
  count = 0
  for(j in 1:M){
    xvec = runif(n)
    sigma2_e = sigma2_v / (1-i^2)
    evec = rep(0, n)
    evec[1] = rnorm(1,0,sqrt(sigma2_v)) #setup first e
    for(k in 2:n){
      evec[k] = i * evec[k-1] + rnorm(1,0,sqrt(sigma2_e))
    }
    #generate y
    y = beta0 + beta1 * xvec + evec
    lmodel = lm(y~xvec)
    data = data.frame("xvec"=0.5)
    yci = predict(lmodel, newdata = data, interval = "confidence")[2:3]
    count = count + as.numeric(yci[2] >= meany & yci[1] <= meany)
  }
  counts[z] = count
  z=z+1
}
df = data.frame("rho" = rho, "probability" = counts / M)
df

plot(df$rho, df$probability, type = "p", main = "rho versus coverage probability", xlab = "rho",
     ylab="coverage probability")
lines(df$rho, df$probability)

#problem 3 (9.2)
setwd("~/tamu/MS-STAT-2022/608")
bookstore = read.csv("bookstore.txt", sep = '\t')
head(bookstore)

#part a
# Follow the advice of Abraham and Ledolter (2006, pp. 336–337) and first build a model for Sales ignoring the effects due to Advert and Lag1Advert. 
#Ensure that you produce diagnostic plots to justify your choice of model.
#Describe any weaknesses in your model.

lmsales = lm(Sales ~ Time + Month_2 + Month_3 + Month_4 + Month_5 + Month_6 + Month_7 + Month_8 + Month_9 + Month_10 + Month_11 + Month_12, data = bookstore)
summary(lmsales)
par(mfrow = c(2,2))
plot(lmsales)

library(car)
par(mfrow = c(5,5))
mmps(lmsales)
par(mfrow = c(1,1))
acf(lmsales$residuals)

#part b
#Add the effects due to Advert and Lag1Advert to the model you have developed in (a). Last month’s advertising (Lag1Advert) is thought to have
# an impact on the current month’s sales. Obtain a final model for predicting Sales. Ensure that you produce diagnostic plots to justify your choice of
# model. Describe any weaknesses in your model.

lmsalesb = lm(Sales ~ Time + Month_2 + Month_3 + Month_4 + Month_5 + Month_6 + Month_7 + Month_8 + Month_9 + Month_10 + Month_11 + Month_12 + Advert + Lag1Advert, data = bookstore)
summary(lmsalesb)
par(mfrow = c(2,2))
plot(lmsalesb)

anova(lmsales, lmsalesb)

library(car)
par(mfrow = c(6,6))
mmps(lmsalesb)


par(mfrow = c(1,1))
acf(lmsalesb$residuals)

#anova
anova(lmsales, lmsalesb)


