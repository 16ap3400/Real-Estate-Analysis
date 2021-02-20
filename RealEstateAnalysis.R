#Alex Peterson

library(lubridate)
library(dplyr)
library(readr)

data <- read_csv("Statistical Projects/RealEstate Final/housesalesprediction/kc_house_data.csv", col_types = cols(date = col_character()))
data$RelativeLiving = data$sqft_living / data$sqft_living15
data$RelativeLot = data$sqft_lot / data$sqft_lot15
summary(data$price)

cor(data$RelativeLiving, data$RelativeLot)



#Linear Model Analysis
linmod = lm(price ~ RelativeLiving + RelativeLot, data=data)
summary(linmod)
pred = predict(linmod, data, interval = "prediction")

linmodLivingOnly = lm(price ~ RelativeLiving, data=data)
summary(linmodLivingOnly)



#Logistic using price as a boolean
dataLogistic = data
med = median(dataLogistic$price)
for(i in 1:nrow(dataLogistic)){
  if(dataLogistic[i,3] < med){
    dataLogistic[i,3] = 0
  }else{
    dataLogistic[i,3] = 1
  }
}

library(psych)
library(rcompanion)
null = glm(price ~ 1, data=dataLogistic, family = binomial(link = "logit"))
logitMod = glm(price ~ RelativeLiving + RelativeLot, data=dataLogistic, family=binomial(link="logit"))
summary(logitMod)
nagelkerke(logitMod, null=null)

#Principal Compnents to investigate effect of neighbors
pcdata = data[,-c(1,2)]
pca = princomp(pcdata, cor = TRUE)
summary(pca)
biplot(pca)



#Plotting
predLinmod = ggplot(data, aes(x = data$price, y = predict(linmod, data, interval = "prediction")[,1]))+
  geom_point()
predLinmod



plot(data$price, data$sqft_lot15)
plot(data$price, data$sqft_living15)
plot(data$price, data$RelativeLot)

plt = ggplot(data, aes(x=data$price, y=data$RelativeLiving))+
  geom_point()+
  geom_smooth(formula = y ~ x)
Plt



