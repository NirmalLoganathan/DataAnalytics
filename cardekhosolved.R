setwd("D:/Rprogramming")
card = read.csv("Cardekho.csv")
str(card)

#DATA CLEANING 

#remove part of a string 
#for mileage
card$mileage = gsub("[a-zA-Z ]", "", card$mileage)
card$mileage

#for engine 
card$engine = gsub("[a-zA-Z ]", "", card$engine)
card$engine

#for max_power
card$max_power = gsub("[a-zA-Z]","",card$max_power)
card$max_power

#for torque
card$torque = gsub("[a-zA-Z]","",card$torque)
card$torque
card$torque = gsub("@","",card$torque)
card$torque

#converting characters into numeric & factors
str(card)
card$fuel = as.factor(card$fuel)
card$seller_type = as.factor(card$seller_type)
card$transmission = as.factor(card$transmission)
card$owner = as.factor(card$owner)
card$engine = as.numeric(card$engine)
card$max_power= as.numeric(card$max_power)
card$mileage = as.integer(card$mileage)
card$torque = as.integer(card$torque)
card$seats = as.factor(card$seats)
str(card)

#missing values check
#missing values imputation for mileage feature
table(is.na(card$mileage))
med = median(card$mileage,na.rm = T)
card$mileage[is.na(card$mileage)] = med
table(is.na(card$mileage))

#for engine feature
table(is.na(card$engine))
med1 = median(card$engine,na.rm = T)
card$engine[is.na(card$engine)] = med1
table(is.na(card$engine))

#for engine feature
table(is.na(card$max_power))
med2 = median(card$max_power,na.rm = T)
card$max_power[is.na(card$max_power)] = med2
table(is.na(card$max_power))

#for seats feature
table(is.na(card$seats))
getmode=function(v) {
  uniqv=unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode1=getmode(card$seats)
print(mode1)
card$seats[is.na(card$seats)]= mode1

#handling name column
card$name[1]
card$Brand=sapply(strsplit(card$name, " "), `[`, 1)
card$Brand = as.factor(card$Brand)
table(card$Brand)

table(is.na(card$Brand))
str(card)

#seperating by model
card$name[1]
card$Model=sapply(strsplit(card$name, " "), `[`, 2)
card$Model = as.factor(card$Model)
table(card$Model)

#visualization
library(ggplot2)

gg1 = ggplot(card,aes(year,selling_price,col=owner))+
  geom_point(stat="Identity")
gg1

gg2 = ggplot(card,aes(km_driven,selling_price))+
  geom_point(stat = "Identity",position = "Dodge")
gg2

gg3 = ggplot(card,aes(fuel,selling_price,fill=owner))+
  geom_bar(stat = "identity",position = "Dodge")
gg3

gg4 = ggplot(card,aes(fuel,selling_price,fill=seller_type))+
  geom_bar(stat = "identity",position = "Dodge")
gg4

gg5 = ggplot(card,aes(engine,selling_price))+
  geom_point(stat="Identity")
gg5

gg6 = ggplot(card,aes(seats,selling_price))+
  geom_bar(stat = "identity",position = "Dodge")
gg6

gg7 = ggplot(card,aes(fuel,selling_price,fill=seller_type))+
  geom_bar(stat = "identity",position = "Dodge")
gg7

#data partition
library(caret)
sample = createDataPartition(card$selling_price, p = .75, list = FALSE)
train = card[ sample,]
test = card[-sample,]

dim(train)
dim(test)
dim(card)
str(card)

#model

m1=lm(selling_price~km_driven+fuel+seller_type+year,train)
summary(m1)

prediciton = predict(m1,test)
summary(prediciton)

rmse1 = sqrt(mean(card$selling_price - prediction)^2)
rmse1 
