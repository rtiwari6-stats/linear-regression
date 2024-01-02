#homework 5

setwd("~/tamu/MS-STAT-2022/608")
cars = read.csv("cars04.csv")
attach(cars)

#transform

tSuggestedRetailPrice = log(SuggestedRetailPrice)
tCylinders = log(Cylinders)
tHorsepower = log(Horsepower)
tWheelBase = log(WheelBase)
tEngineSize = EngineSize^0.25
tHighwayMPG = 1/HighwayMPG

modelfull = lm(tSuggestedRetailPrice ~ tEngineSize + tCylinders +
                 tHorsepower + tHighwayMPG + Weight + tWheelBase + Hybrid)
summary(modelfull)

modelreduced = lm(tSuggestedRetailPrice ~ tEngineSize + tCylinders + tHorsepower + Weight + Hybrid)
summary(modelreduced)

RSSfull = deviance(modelfull)
RSSreduced = deviance(modelreduced)

f = ((RSSreduced-RSSfull)/2)/(RSSfull/(234-7-1))

anova(modelreduced, modelfull)