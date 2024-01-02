# problem 1
# y = pricechange, x=loanpaymentsoverdue
setwd("~/tamu/MS-STAT-2022/608/2022")
indicators = read.table("data/Indicators.txt", header = T)
y = indicators$PriceChange
x = indicators$LoanPaymentsOverdue
lin_model = lm(y ~ x)
lin_model_sum = summary(lin_model)
lin_model_sum

#95% confidence interval for the slope
confint(lin_model)[2,]
lin_model_sum$coefficients[2,1] + qt(0.975, nrow(indicators)-2)*lin_model_sum$coefficients[2,2]
lin_model_sum$coefficients[2,1] - qt(0.975, nrow(indicators)-2)*lin_model_sum$coefficients[2,2]

#predict E(Y|X=4) and find a 95% confidence interval for it
newdata = data.frame(x = c(4))
predict(lin_model, newdata, interval = 'confidence', level=0.95)

#problem 7
invoices = read.table("data/invoices.txt", header = T)
y = invoices$Time
x = invoices$Invoices
lin_model = lm(y ~ x)
lin_model_sum = summary(lin_model)
lin_model_sum

#95% confidence interval for the startup time
confint(lin_model)[1,]
lin_model_sum$coefficients[1,1] + qt(0.975, nrow(invoices)-2)*lin_model_sum$coefficients[1,2]
lin_model_sum$coefficients[1,1] - qt(0.975, nrow(invoices)-2)*lin_model_sum$coefficients[1,2]

#test hypothesis for beta1 = 0.01
tstat = (lin_model_sum$coefficients[2,1]-0.01) / lin_model_sum$coefficients[2,2]
pval = 2*(1-pt(abs(tstat), nrow(invoices)-1))
pval < 0.05

#predict time taken to process 130 invoices
newdata = data.frame(x = c(130))
predict(lin_model, newdata, interval = 'prediction', level=0.95)