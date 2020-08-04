library(caret)
library(tidyverse)
library(ggplot2)
library(naivebayes)
library(e1071)
library(Boruta)
library(doSNOW)
#load data

train<-read.csv(file = "D:/data set for analysis/train.csv",
                header = TRUE,stringsAsFactors = FALSE)

head(train)

View(train)

test<-read.csv(file = "D:/data set for analysis/test.csv",header = TRUE,
               stringsAsFactors = F)

#add survived variable to the test to allow for combining dataset.

#as in train data set we have one column as survived.

test.survived<-data.frame(survived=rep("none",nrow(test)),test[,])

head(test.survived)

#now we should combined both the data set.

test.survived<-test.survived[,c(2,1,3:12)]

#data.combined<-rbind(train,test.survived)

#note that column names inboth the data set are not same so check first then rename

names(test.survived)<-names(train)

data.combined<-rbind(train,test.survived)

#string is like summary it gives info about coloumn ,its type and factors...

str(data.combined)

#in machine learning and data analysis computer don't want data to be char
#some times we want levels i.e. male or female or class first ,second or third
#these thing can achieve by factor

data.combined$Survived<-factor(data.combined$Survived)

#it gives 0,1,none in level if you see in string

data.combined$Pclass<-factor(data.combined$Pclass)

str(data.combined)

#take a look on gross survival rate

table(data.combined$Survived)

#o/p is 549 dead,342-survive,418 -none i.e don't know

#distribution about the classes

table(data.combined$Pclass)

#lot of people in 3rd class than first class but in first class has more than 2nd class remind


#now we want to check the survival in each class so note first that 
#we have to take train data as train.combines has null entries


ggplot(train,aes(x=Pclass,fill=factor(Survived)))+
  geom_bar(width=0.5)+xlab("passanger class")+ylab("total count")


count<-table(train$Pclass,train$Survived)

barplot(count,xlab="survived",ylab = "count of pclass",
        col=c("red","blue","green"),beside = T,
        legend=(row.names(count)))
#people who are travel in first class have more chance of survival than third class
#2nd class people have almost 50% chance of survival


#is there any duplicate names
#first check any duplicate name in list or not 
?duplicated

duplicate_names<-as.character(data.combined[which(
  duplicated(as.character(data.combined$Name))),
                                            "Name"])

duplicate_names

#verify their details


data.combined[which(data.combined$Name %in% duplicate_names),]

#duplicated names has same name but ticket number are different




misses<-data.combined[which(str_detect(data.combined$Name,'Miss')),]

misses[1:5,]

mrs<-data.combined[which(str_detect(data.combined$Name,'Mrs')),]

mrs[1:5,]

male<-data.combined[which(str_detect(data.combined$Sex,'male')),]

male[1:5,]

#create function to help with title expression mr,miss..


title<-function(Name){
   as.character(Name)  
  if (length(grep('Miss',Name))>0){
    return("Miss")
  }else if(length(grep('Master',Name))>0){
    return('Master')
  }else if (length(grep('Mrs',Name))>0){
    return('Mrs')
  }else if (length(grep('Mr',Name))>0){
    return('Mr')
  }else{
    return('other')
  }
}

titles<-NULL
#title(data.combined$Name)
for (i in 1:nrow(data.combined)) {
  titles<-c(titles,title(data.combined[i,'Name']))
  }

titles

data.combined<-cbind(data.combined,titles)

data.combined[1,]

as.factor(titles)

ggplot(data.combined[1:891,],aes(titles,fill=Survived))+
  geom_bar(width=0.5)+facet_wrap(~Pclass)+ggtitle('Pclass')

qplot(data=data.combined[1:891,],x=titles,y=Age,facets =Pclass~Survived,col=titles)
#for age plot he removed 177 point which may have  missing values

#visualization of 3level relationship with sex ,class and survival
#Master from 1st and 2nd class are all survived.

table(data.combined$Sex)

ggplot(data=data.combined[1:891,],aes(x=Sex,fill=Survived))+
  geom_bar(width=0.5)+facet_wrap(titles~Pclass)

count(data.combined[1:891,]%>%filter(Pclass==1 & Sex=='female'&Survived==0))

#outofff 94 women travel in first class 91 survied

ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 05)+facet_wrap(~Pclass+Sex)

ggplot(data.combined[1:891,],aes(x=Age,fill=Survived))+
  geom_histogram(binwidth = 05)+facet_wrap(~Pclass+titles)

#master usually boys survival analysis

data.combined$titles

boys<-data.combined %>% filter(titles=='Master')

summary(boys$Age)

dim(boys)

#minimum age of boy is 3 month and max is 14.5 years..

#8 na's

misses<-data.combined[which(data.combined$titles=='Miss'),]

summary(misses$Age)

#misses has 0.17 to 63 years range and 50 na's so hard to analysis

misses_alone<-misses[which(misses$SibSp==0 & misses$Parch==0),]

summary(misses_alone$Age)

#misses_alone has 5 to 58 years range and 33 na's so hard to analysis

ma<-misses_alone[which(misses_alone$Survived==0|misses_alone$Survived==1),]

dim(ma)

ggplot(ma,aes(x=Age,fill=ma$Survived))+geom_bar(width=1)+facet_wrap(~Pclass)

#those who are below 10 years misses travel alone can survied

#who are below 45 all survied

unique(data.combined$titles)

mrs<-data.combined[which(data.combined$titles=='Mrs'),]

summary(mrs$Age)

# it has 27 na's

a<-data.combined[which(data.combined$Sex=='male'& data.combined$Age<=14.5),]

dim(a)

#we will work on ticket 

str(data.combined$Ticket)

ticket_first_charecter<-ifelse(data.combined$Ticket=='',
                               ' ',substr(data.combined$Ticket,1,1))
head(ticket_first_charecter)

unique(ticket_first_charecter)

data.combined$ticket_first_char<-as.factor(ticket_first_charecter)

ggplot(data=data.combined[1:891,],aes(x=ticket_first_char,fill=Survived))+
  geom_bar()+ggtitle('survied_by_ticket_first_char')+
  xlab('ticket_first_char')+ylab('survived count')

ggplot(data=data.combined[1:891,],aes(x=ticket_first_char,fill=Survived))+
  geom_bar()+facet_grid(Pclass~Sex)+
  ggtitle('survied_based on ticket_along_Pclass')+
  ylab('Pclass')
#woen in first class having ticket_first rather than P all survived
#women in 2hd class rather than 2 in tichet first letter all survied

#now we will work on the fare of data_set

str(data.combined$Fare)
hist(data.combined$Fare)
summary(data.combined$Fare)

ggplot(data=data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle('ticket fare')

ggplot(data=data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle('ticket fare')+
  facet_wrap(~titles)

ggplot(data=data.combined[1:891,],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+
  ggtitle('ticket fare')+
  facet_wrap(titles~Pclass)

#fare and ticket not play much significant role in model construction

#work on the cabin 

data.combined$Cabin

str(data.combined$Cabin)

data.combined[which(data.combined$Cabin==''),'Cabin'] <- 'U'

data.combined$Cabin[1:100]

cabin_first_letter<-as.factor(str_sub(data.combined$Cabin,1,1))

cabin_first_letter

data.combined$cabin_first_letter<- cabin_first_letter

ggplot(data=data.combined[1:891,],aes(x=cabin_first_letter,fill=Survived))+
  geom_bar()+ggtitle('survival on cabin first letter')
#a,b,c class has people fron first class

ggplot(data=data.combined[1:891,],aes(x=cabin_first_letter,fill=Survived))+
  geom_bar()+facet_grid(~Pclass)+
  ggtitle('survival on cabin first letter')

ggplot(data=data.combined[1:891,],aes(x=cabin_first_letter,fill=Survived))+
  geom_bar()+facet_grid(titles~Pclass)+
  ggtitle('survival on cabin first letter')

#Cabin has no used....


table(data.combined$Embarked)
summary(data.combined$Embarked)

ggplot(data=data.combined[1:891,],aes(x=Embarked,fill=Survived))+
  geom_bar()+ggtitle('surviaval on embarked')

ggplot(data=data.combined[1:891,],aes(x=Embarked,fill=Survived))+
  geom_bar()+ggtitle('surviaval on embarked')+
  facet_grid(Pclass~titles)

#embarked is usedful so defining survivsl or not in 3rd class 

#detecting multiple cabin
data.combined$multiple_cabin<-ifelse(str_detect(data.combined$Cabin,' '),'Y','N')

data.combined$multiple_cabin

ggplot(data=data.combined[1:891,],aes(multiple_cabin,fill=Survived))+
  geom_bar()+
  facet_grid(~Pclass)+
  ggtitle('multiple cabin servived ')

ggplot(data=data.combined[1:891,],aes(multiple_cabin,fill=Survived))+
  geom_bar()+
  facet_grid(titles~Pclass)+
  ggtitle('multiple cabin servived ')
#multiple cabin seems to be useful for survival or not

#family 
data.combined$family_member<-as.factor(data.combined$SibSp+
                                         data.combined$Parch+
                                       1)
data.combined$family_member

ggplot(data.combined[1:891,],aes(family_member,fill=Survived))+
  geom_bar()+
  facet_grid(titles~Pclass)+
  ggtitle('famil_member_survived')

#family is also important.


#Building a model for evaluation

train_nb<-data.combined[1:891,]

View(train)

test_nb<-data.combined[892:nrow(data.combined),]

boruta_output<-Boruta(train$Survived~.,data=na.omit(train[,-c(1,4,6)]))

getSelectedAttributes(boruta_output,withTentative = T)

data.combined1<-data.combined[1:891,-c(1,2,4,6,17)]
data.combined1$Survived=as.factor(train$Survived)
str(data.combined1)
rPartMod<-train(Survived~.,
                data.combined1,
                method='rpart')
rpartImp<-varImp(rPartMod)
print(rpartImp)
plot(rpartImp)

#titles,sex,Pclass,Sbbsp,Cabin,Family atr important variables
#____________________________________________________



nb.train.1<-data.combined[1:891,c('Pclass','titles')]

nb.train.label<-as.factor(train$Survived)

set.seed(111111)

nb.1<-naiveBayes(x=nb.train.1,y=nb.train.label)

summary(nb.1)

nb.1$apriori

nb.1$tables

#predictionn on train data
res1<-predict(nb.1,nb.train.1)
res1
confusionMatrix(nb.train.label,res1)
# Accuracy : 0.7957 

nb.train.2<-data.combined[1:891,c('Pclass','titles',"family_member")]
nb.2<-naiveBayes(x=nb.train.2,y=nb.train.label)
res2<-predict(nb.2,nb.train.2)
confusionMatrix(nb.train.label,res2)
#Accuracy : 0.8047          

set.seed(12111)
nb.train.3<-data.combined[1:891,c("Pclass","titles","family_member","Cabin")]
str(nb.train.3)
nb.train.3$Cabin<-as.factor(nb.train.3$Cabin)
nb.3<-naiveBayes(x=nb.train.3,y=nb.train.label)
res3<-predict(nb.3,nb.train.3)
confusionMatrix(nb.train.label,res3)
#Accuracy : 0.8418

nb.test.3<-data.combined[892:nrow(data.combined),c("Pclass","titles",
                                                   "family_member","Cabin")]
nb.test.3$Cabin<-as.factor(nb.test.3$Cabin)
test.submit<-predict(nb.3,nb.test.3)
str(test.submit)
table(test.submit)
submit.df<-data.frame(PassengerId=rep(892:1309),Survived=test.submit)
submit.df
write.csv(submit.df,file = 'C:/Users/Akshay/Desktop/test_tiatanic.csv')


#__________________________________________________________________
#Cross Validation

cv.10.folds<-createMultiFolds(nb.train.label,k=10,times = 10)

table(nb.train.label)

table(nb.train.label[cv.10.folds[[33]]])

control.l<-trainControl(method = 'repeatedcv',number = 10,
                        repeats = 10,index = cv.10.folds)

cl<-makeCluster(6,type = 'SOCK')

registerDoSNOW(cl)

set.seed(112211)

nb.3.cv<-train(x=nb.train.3,y=nb.train.label,method='nb',
               trCotrol=control.l,tunelength=3)

nb.3.cv

train_predict<-predict(nb.3.cv,nb.train.3)


confusionMatrix(nb.train.label,train_predict)

stopCluster(cl)

test.submit1<-predict(nb.3.cv,nb.test.3)

test.submit1

table(test.submit1)
submit.df<-data.frame(PassagerId=rep(892:1309),Survived=test.submit1)
submit.df
write.csv(submit.df,file = 'C:/Users/Akshay/Desktop/test_tiatanic1.csv')

#model has a accuracy of 84.18%


#further more we can make improvment in the other variable in titles

?str_split

name.split<-str_split(data.combined$Name,',')

name.split

?sapply()

last_names<-sapply(name.split,'[',1)

last_names

data.combined$last_name<-last_names

name.split<-str_split(sapply(name.split,'[',2)," ")

name.split

titles<-sapply(name.split,'[',2)

titles

titles<-as.factor(titles)

table(titles)

#remap titles
data.combined[which(titles=='the'),]
data.combined[which(titles=='Dona.'),]

titles[titles%in%c('Dona.','the')]<-'Lady.'
titles[titles%in%c('Ms.','Mlle.')]<-'Miss.'
titles[titles%in%c('Mme.')]<-'Mrs.'
titles[titles%in%c('Jonkheer.','Don.')]<-'Sir.'
titles[titles%in%c('Col.','Major.','Capt.')] <- 'Mr.'
titles[titles%in%c('Sir.','Dr.','Rev.')]<-'Mr.'
data.combined[which(titles=='Lady.'),]
data.combined$titles<-titles
str(data.combined)
data.combined$titles

#now we made changes in titles 
#cv.10.folds<-createMultiFolds(nb.train.label,k=10,times = 10)

#table(nb.train.label)

#table(nb.train.label[cv.10.folds[[33]]])

#control.l<-trainControl(method = 'repeatedcv',number = 10,
#                        repeats = 10,index = cv.10.folds)

cl<-makeCluster(6,type = 'SOCK')

registerDoSNOW(cl)

set.seed(112211)

nb.3.cv1<-train(x=nb.train.3,y=nb.train.label,method='nb',
               trCotrol=control.l,tunelength=3)

nb.3.cv1

train_predict1<-predict(nb.3.cv1,nb.train.3)


confusionMatrix(nb.train.label,train_predict1)

stopCluster(cl)

test.submit2<-predict(nb.3.cv,nb.test.3)

test.submit2

table(test.submit2)

#__________________________________________________________