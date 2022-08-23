setwd("C:\Users\ELCOT\Desktop\Datascience")
boston=read.csv("HousingDataBoston.csv")
boston
library(ggplot2)
library(dplyr)
#finding and imputing missing values
table(is.na.data.frame(boston))

#for Crim feature
table(is.na(boston$CRIM))
med=median(boston$CRIM,na.rm=TRUE)
med
boston$CRIM[is.na(boston$CRIM)]= med

#for zn feature
table(is.na(boston$ZN))
med1=median(boston$ZN,na.rm=TRUE)
med1
boston$ZN[is.na(boston$ZN)]= med1
str(boston)

#for indus feature
table(is.na(boston$INDUS))
med2=median(boston$INDUS,na.rm=TRUE)
boston$INDUS[is.na(boston$INDUS)]= med2

#for CHAS feature
table(is.na(boston$CHAS))
getmode=function(v) {
  uniqv=unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
mode1=getmode(boston$CHAS)
print(mode1)
boston$CHAS[is.na(boston$CHAS)]= mode1

#for room feature
table(is.na(boston$AGE))
me3=median(boston$RM,na.rm=TRUE)
boston$RM[is.na(boston$RM)]=me3

#for age feature
table(is.na(boston$AGE))
med4=median(boston$AGE,na.rm=TRUE)
boston$AGE[is.na(boston$AGE)]=med4

#for lstat feature
table(is.na(boston$LSTAT))
str(boston$LSTAT)
med5=median(boston$LSTAT,na.rm=TRUE)
boston$LSTAT[is.na(boston$LSTAT)]=med5
med5

m6=median(boston$DIS)
m6
#checking outliers
x1=boxplot(boston$NOX)
summary(boston$NOX)

#finding and removing outliers
x2=boxplot(boston$RM)
summary(boston$RM)
length(boston$RM)
outl1=boxplot(boston$RM, plot = FALSE)$out
outl1
iqr=1.5*(6.623-5.886)
iqr
data_no_outlier=boston$RM[-which(boston$RM %in% outl1)]
boxplot(data_no_outlier, plot = FALSE)$out
length(data_no_outlier)


#categorizing by nox level
boston$categorized.nox = cut(boston$NOX, breaks = c(0.3, 0.5, 0.7, 0.8, Inf),
                            labels = c("Low", "Medium", "High","Over"))
#categorizing by room
boston$categorized.rm = cut(boston$RM, breaks = c(1 , 3, 6, 8),
                            labels = c("Small","Medium","Large"))

#visualization
gg1=ggplot(boston,aes(categorized.nox,fill=MEDV))+
  geom_bar(position="Dodge")
gg1

gg2=ggplot(boston,aes(RM,fill=MEDV))+
  geom_point(position="Dodge",stat="identity")
gg2

gg3=ggplot(boston,aes(CRIM,MEDV))+
  geom_point(stat="Identity",position="Dodge")
gg3

gg4=ggplot(boston,aes(ZN,MEDV))+
  geom_point(stat="Identity")
gg4

gg5=ggplot(boston,aes(INDUS,MEDV))+
  geom_point(stat="Identity")
gg5

gg6=ggplot(boston,aes(CHAS,fill=MEDV))+
  geom_bar(position="Dodge")
gg6

gg7=ggplot(boston,aes(NOX,MEDV))+
  geom_point(stat="Identity")
gg7

gg8=ggplot(boston,aes(RM,MEDV))+
  geom_point(stat="Identity")
gg8

gg9=ggplot(boston,aes(DIS,MEDV,col=RM))+
  geom_point(stat="identity")
gg9

gg10=ggplot(boston,aes(TAX,MEDV))+
  geom_point(stat="identity")
gg10

gg11=ggplot(boston,aes(B,MEDV))+
  geom_point(stat="identity")
gg11

#dplyr
x1=boston%>%
  select(RM,MEDV)%>%
  filter(between(RM,6,7))%>%
  summarize(y1=mean(MEDV))
x1
x14=boston%>%
  select(AGE,RM,DIS)%>%
  filter(between(RM,7,8),between(DIS,2,5))%>%
  summarize(c1=mean(AGE))
x14
#Price varies by business(1 to 10)
x2=boston%>%
  select(INDUS,MEDV)%>%
  filter(between(INDUS,1,10))%>%
  summarize(y2=mean(MEDV))
x2
#Price varies by business(10 to 20)
x3=boston%>%
  select(INDUS,MEDV)%>%
  filter(between(INDUS,10,20))%>%
  summarize(y3=mean(MEDV))
x3
#Price according to employment centers distance(1 to 5)
x4=boston%>%
  select(DIS,MEDV)%>%
  filter(between(DIS,1,5))%>%
  summarize(y4=mean(MEDV))
x4
#Price according to employment centers distance(5 to 10)
x5=boston%>%
  select(DIS,MEDV)%>%
  filter(between(DIS,5,10))%>%
  summarize(y5=mean(MEDV))
x5
#Price according to tax(200 to 450)
x6=boston%>%
  select(TAX,MEDV)%>%
  filter(between(TAX,200,450))%>%
  summarize(y6=mean(MEDV))
x6 
#Price according to tax(450 to 700)
x7=boston%>%
  select(TAX,MEDV)%>%
  filter(between(TAX,450,700))%>%
  summarize(y7=mean(MEDV))
x7
#Price by Crime Rate(0 to 10)
x8=boston%>%
  select(CRIM,MEDV)%>%
  filter(between(CRIM,0,10))%>%
  summarize(y8=mean(MEDV))
x8
#Price by Crime Rate(10 to 25)
x9=boston%>%
  select(CRIM,MEDV)%>%
  filter(between(CRIM,10,25))%>%
  summarize(y9=mean(MEDV))
x9
#Price by Blacks ( 0 to 200)
x10=boston%>%
  select(B,MEDV)%>%
  filter(between(B,0,200))%>%
  summarize(y10=mean(MEDV))
x10
#Price by Blacks ( 200 to 400)
x11=boston%>%
  select(B,MEDV)%>%
  filter(between(B,200,400))%>%
  summarize(y11=mean(MEDV))
x11
#Summarize
x12=boston%>%
  select(RM,INDUS,DIS,TAX,CRIM,B)%>%
  filter(between(B,200,400),between(CRIM,0,10),between(TAX,200,450),between(DIS,1,5),between(INDUS,1,10),between(RM,5,7))%>%
  summarize(y12=mean(MEDV))
x12
#Splitting train and test
## 75% of the sample size
smp_size <- floor(0.75 * nrow(boston))
## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(boston)), size = smp_size)

train <- boston[train_ind, ]
test <- boston[-train_ind, ]
str(train)
str(test)

#Decision tree without tuning
library(rpart)
library(rpart.plot)
dtb=rpart(MEDV~RM+AGE+CRIM+DIS+TAX+INDUS,boston,method="anova")
rpart.plot(dtb)
prediction=predict(dtb,test)
prediction
rmse2=sqrt(mean(test$MEDV-prediction)^2)
rmse2

#Decision tree with tuning
library(caret)
library(rpart.plot)
dtb=rpart(MEDV~B+RM+CRIM+DIS+TAX+INDUS,boston,method="anova",
          control = rpart.control(minsplit = 10,maxdepth = 10,cp=0))
dtb$cptable
hyper_grid=expand.grid(minsplit=seq(5,20,1),maxdepth=seq(8,15,1))
head(hyper_grid)
tail(hyper_grid)
nrow(hyper_grid)
models <- list()
library(rpart)

for (i in 1:nrow(hyper_grid)) {
  
  # get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # train a model and store in the list
  models[[i]] <- rpart(MEDV~B+RM+CRIM+DIS+TAX+INDUS,
    data    = train,
    method  = "anova",
    control = list(minsplit = minsplit, maxdepth = maxdepth)
  )
}
# function to get optimal cp
get_cp = function(x) {
  min= which.min(x$cptable[, "xerror"])
  cp = x$cptable[min, "CP"] 
}

# function to get minimum error
get_min_error = function(x) {
  min    = which.min(x$cptable[, "xerror"])
  xerror = x$cptable[min, "xerror"] 
}

hyper_grid %>%
  mutate(
    cp    = purrr::map_dbl(models, get_cp),
    error = purrr::map_dbl(models, get_min_error)
  ) %>%
  arrange(error) %>%
  top_n(5, wt = error)

rpart.plot(dtb)
prediction=predict(dtb,test)
prediction
rmse2=sqrt(mean(test$MEDV-prediction)^2)
rmse2
#Decision tree with crt minsplit and maxdepth
library(caret)
library(rpart.plot)
dtb=rpart(MEDV~B+RM+CRIM+DIS+TAX+INDUS,train,method="anova",control = rpart.control(minsplit = 12,maxdepth = 8,cp=0.01))
dtb$cptable
prediction1=predict(dtb,test)
prediction1
rmse3=sqrt(mean(test$MEDV-prediction1)^2)
rmse3
#Random Forest
table(is.na(train$MEDV))
library(randomForest)
set.seed(123)
rfm1=randomForest(MEDV~AGE+RM+CRIM+DIS+TAX+INDUS+LSTAT,train,importance = TRUE, na.action = na.omit,ntree=100)
varImpPlot(rfm1)
prediction4=predict(rfm1,test)
prediction4
rmse4=sqrt(mean(test$MEDV-prediction4)^2)
rmse4
saveRDS(rfm1,"rfm1.rds")
