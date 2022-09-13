setwd("C:/Users/ELCOT/Desktop/Datascience")
Titan=read.csv("Titanictrain.csv")
Titan
str(Titan)
setwd("C:/Users/ELCOT/Desktop/Datascience")
Titantest=read.csv("Titanictest.csv")
Titantest
str(Titantest)
Titan$Pclass=factor(Titan$Pclass)
Titantest$Pclass=factor(Titantest$Pclass,ordered=FALSE)
str(Titan)
#feature engineering
#finding the missing values
is.na.data.frame(Titan)

#sum of missing values
table(is.na(Titan$Embarked))
table(is.na(Titantest$Survived))
#imputation of the missing values for train dataset
med=median(Titan$Age,na.rm=TRUE)
med
Titan$Age[is.na(Titan$Age)]= med
Titan

#impuation of the missing values for test dataset
med1=median(Titantest$Age,na.rm=TRUE)
med1
Titantest$Age[is.na(Titantest$Age)]= med1
Titantest

med2=median(Titantest$Fare,na.rm=TRUE)
med2
Titantest$Fare[is.na(Titantest$Fare)]=med2
Titantest

#splitting by name
Titantest$Survived = 0
str(Titantest)
combine=rbind(Titan,Titantest)
str(combine)
combine$Name=as.character(combine$Name)
combine$Name[1]

#extracting by title
combine$Title= sapply(combine$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][2]})
combine$Title=sub(' ', '', combine$Title)
combine$Title[combine$Title %in% c('Capt', 'Don', 'Major', 'Sir')] = 'Sir'
combine$Title[combine$Title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer')] = 'Lady'
combine$Title=factor(combine$Title)
table(combine$Title)

#extracting by surname
combine$FamilySize=combine$SibSp + combine$Parch + 1
table(combine$FamilySize)
combine$Surname= sapply(combine$Name, FUN=function(x){strsplit(x, split='[,.]')[[1]][1]})
combine$FamilyID=paste(as.character(combine$FamilySize), combine$Surname, sep="")
combine$FamilyID[combine$FamilySize <= 3]='Smallfamily'
table(combine$FamilyID)

#creating data frame
famIDs=data.frame(table(combine$FamilyID))
famIDs=famIDs[famIDs$Freq <= 3,]
famIDs

#appending small family values to the data frame
combine$FamilyID[combine$FamilyID %in% famIDs$Var1]='Smallfamily'
combine$FamilyID <- factor(combine$FamilyID)
table(combine$FamilyID)
str(combine)

#splitting train and test data
train=combine[1:891,]
test=combine[892:1309,]

#categorizing by age
train$Discretized.age = cut(train$Age, breaks = c(0, 14, 24, 64, Inf),
                            labels = c("Children", "Youth", "Adult", "Senior"))

#visualization and EDA
library(dplyr)
library(ggplot2)

#converting survived into factors
Titan$Survived = factor(train$Survived)
str(train)

#visualizing survived members by sex
gg1=ggplot(train,aes(Sex,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg1

#survived person by class
gg2=ggplot(train,aes(Pclass,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg2

#survived persons by embarked
gg3=ggplot(train,aes(Embarked,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg3

#survived persons by age
gg4=ggplot(train,aes(Discretized.age,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg4

#survived persons by sibsp
gg5=ggplot(train,aes(SibSp,fill=factor(Survived)))+
  geom_bar(position = "Dodge")
gg5

#survived persons by Parch
gg4=ggplot(Titan,aes(Parch,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg4

#survived persons by fair
gg5=ggplot(Titan,aes(Fare,Survived))+
  geom_bar(stat="Identity",position="Dodge")
gg5

#survived persons by family
gg6= ggplot(train,aes(FamilySize,fill=factor(Survived)))+
  geom_bar(position="Dodge")
gg6
#total members in the Titanic
x0=Titan%>%
  select(Sex,Pclass,Survived)%>%
  summarize(tp=length(Pclass))
x0
#total  no.of males
x1=Titan%>%
  select(Sex,Survived)%>%
  filter(Sex=="male")%>%
  summarize(mt=length(Sex))
x1

#total no.of females
x2=Titan%>%
  select(Sex,Survived)%>%
  filter(Sex=="female")%>%
  summarize(ft=length(Sex))
x2

#percentage of males survived
x3=Titan%>%
  select(Sex,Survived)%>%
  filter(Sex=="male",Survived==1)%>%
  summarize(ms=length(Sex))%>%
  mutate(msp=(ms/x0)*100)
x3

#percentage of females survived
x4=Titan%>%
  select(Sex,Survived)%>%
  filter(Sex=="female",Survived==1)%>%
  summarize(fs=length(Sex))%>%
  mutate(msp=(fs/x0)*100)
x4

#total members at the age <18
x5=Titan%>%
  select(Age,Survived)%>%
  filter(Age<18)%>%
  summarize(a18=length(Age))
x5

#percentage of survival at the age <18
x6=Titan%>%
  select(Age,Survived)%>%
  filter(Age<18,Survived==1)%>%
  summarize(a18s=length(Age))%>%
  mutate(sp18=(a18s/x0)*100)
x6

#percentage of survival at the age 18-30
x7=Titan%>%
  select(Age,Survived)%>%
  filter(between(Age,18,30))%>%
  summarize(a30=length(Age))%>%
  mutate(a30p=(a30/x0)*100)
x7

#percentage of survival at the age 30-50
x8=Titan%>%
  select(Age,Survived)%>%
  filter(between(Age,30,50))%>%
  summarize(a50=length(Age))%>%
  mutate(a50p=(a50/x0)*100)
x8

#percentage of survival at the age >50
x9=Titan%>%
  select(Age,Survived)%>%
  filter(Age>50)%>%
  summarize(a70=length(Age))%>%
  mutate(a50p=(a70/x0)*100)
x9

#percentage of survival having 0 sibsp
x10=Titan%>%
  select(SibSp,Survived)%>%
  filter(SibSp==0,Survived==1)%>%
  summarize(sib0=length(SibSp))%>%
  mutate(sib0p=(sib0/x0)*100)
x10

#percentage of survival having 1 sibsp
x11=Titan%>%
  select(SibSp,Survived)%>%
  filter(SibSp==1,Survived==1)%>%
  summarize(sib1=length(SibSp))%>%
  mutate(sib0p=(sib1/x0)*100)
x11

#percentage of survival having >1 sibsp
x12=Titan%>%
  select(SibSp,Survived)%>%
  filter(SibSp>1,Survived==1)%>%
  summarize(sib2=length(SibSp))%>%
  mutate(sib0p=(sib2/x0)*100)
x12

#% of survival for 1st class
x13=Titan%>%
  select(Pclass,Survived)%>%
  filter(Pclass==1,Survived==1)%>%
  summarize(pc1=length(Pclass))%>%
  mutate(pc1s=(pc1/x0)*100)
x13
#% of survival for 2nd class
x14=Titan%>%
  select(Pclass,Survived)%>%
  filter(Pclass==2,Survived==1)%>%
  summarize(pc2=length(Pclass))%>%
  mutate(pc2s=(pc2/x0)*100)
x14
#% of survival for 3rd class
x15=Titan%>%
  select(Pclass,Survived)%>%
  filter(Pclass==3,Survived==1)%>%
  summarize(pc3=length(Pclass))%>%
  mutate(pc3s=(pc3/x0)*100)
x15

#% of embarked at "S"
x16=Titan%>%
  select(Embarked,Survived)%>%
  filter(Embarked=="S",Survived==1)%>%
  summarize(ems=length(Embarked))%>%
  mutate(emss=(ems/x0)*100)
x16

#% of embarked at "C"
x17=Titan%>%
  select(Embarked,Survived)%>%
  filter(Embarked=="C",Survived==1)%>%
  summarize(emc=length(Embarked))%>%
  mutate(emcs=(emc/x0)*100)
x17

#% of embarked at "C"
x18=Titan%>%
  select(Embarked,Survived)%>%
  filter(Embarked=="Q",Survived==1)%>%
  summarize(emq=length(Embarked))%>%
  mutate(emqs=(emq/x0)*100)
x18

x19=Titan%>%
  select(Embarked,Age,SibSp,Sex,Parch)%>%
  filter(Embarked=="S",Age>30,SibSp==1,Sex=="female",Parch==0)%>%
  summarize(ex=length(Embarked))%>%
  mutate(exp1=(ex/x0)*100)
x19

#model building
library(rpart)
library(rpart.plot)
dtm=rpart(Survived~Age+Sex+Pclass+SibSp+Embarked+Fare,train,method = "class",
          control=rpart.control(minsplit=10,cp=0))
rpart.plot(dtm,extra=106)
prediction=predict(dtm,test,type="class")
prediction
table=table(test$Survived,prediction)
table
dt_accuracy = sum(diag(table)) / sum(table)
paste("The accuracy is : ", dt_accuracy)

#random forest
library(randomForest)
set.seed(415)
rfm1=randomForest(as.factor(Survived)~Age+Pclass+Fare+Sex+SibSp+Title,train,importance=TRUE,ntree=3000)
varImpPlot(rfm1)
prediction4=predict(rfm1,test,method="class")
prediction4
table2=table(test$Survived,prediction4)
table2
dt_accuracy = sum(diag(table2)) / sum(table2)
paste("The accuracy is : ", dt_accuracy)

