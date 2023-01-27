library(mlbench)
library(ggplot2)
library(mlbench)
library(rpart)
library(randomForest)
library(rpart.plot)
library(caret)
library(rattle)
library(pROC)
library(dplyr) 
library(plotly)
data("BostonHousing")
str(BostonHousing)

#view
View(BostonHousing)
#ploting
?BostonHousing
plot(BostonHousing$crim,BostonHousing$age)
plot(BostonHousing$zn,BostonHousing$age)
plot(BostonHousing$indus,BostonHousing$age)
plot(BostonHousing$chas,BostonHousing$age)
plot(BostonHousing$nox,BostonHousing$age)
plot(BostonHousing$rm,BostonHousing$age)
plot(BostonHousing$dis,BostonHousing$age)
plot(BostonHousing$rad,BostonHousing$age)
plot(BostonHousing$tax,BostonHousing$age)
plot(BostonHousing$ptratio,BostonHousing$age)
plot(BostonHousing$b,BostonHousing$age)
plot(BostonHousing$lstat,BostonHousing$age)
plot(BostonHousing$medv,BostonHousing$age)
 
#histogram
hist(BostonHousing$age)

#modelbuilding

#model one
m1=lm(age~crim,BostonHousing)
summary(m1)

View(BostonHousing)

#model two
m2=lm(age~.,BostonHousing)
summary(m2)

pred1=predict(m2,BostonHousing)
head(pred1)

#error prediction using RMSE

myerror=sqrt(mean((BostonHousing$age-pred1)^2))
summary(myerror)

#basic histogram

ggplot(BostonHousing,aes(age))+geom_histogram()

# Customized histogram

ggplot(BostonHousing,aes(age))+geom_histogram(binwidth = 1000)

#basic scatter plot
ggplot(BostonHousing,aes(age,crim))+geom_point()

# 3 variable scatter point
ggplot(BostonHousing,aes(age,crim,col=zn))+geom_point()
ggplot(BostonHousing,aes(age,crim,col=zn))+geom_point(alpha=0.3)

#basic bar plot
ggplot(BostonHousing,aes(age,crim))+geom_bar(stat ="identity")

# 3 variable bar point
ggplot(BostonHousing,aes(age,crim,fill=zn))+geom_point(stat ="identity")

#customized bar plot
ggplot(BostonHousing,aes(age,crim,fill=zn))+geom_bar(stat ="identity",position ="dodge")

#4 variable bar plot
ggplot(BostonHousing,aes(age,crim,fill=zn))+geom_bar(stat ="identity",position ="dodge")+facet_grid(~b)
ggplot(BostonHousing,aes(age,crim,fill=zn))+geom_bar(stat ="identity",position ="dodge")+facet_grid(b~.)

#checking missing values
colSums(is.na(BostonHousing))

#spliting data set into train and test
set.seed(123)
train.idx = createDataPartition(y = BostonHousing$age, p = 0.75, list = FALSE)
test.idx =createDataPartition(y=BostonHousing$age, p=0.25,list=FALSE)
train = BostonHousing[train.idx, ]
test = BostonHousing[test.idx, ]
summary(BostonHousing)

#model building
lm.a1 <- lm(age~.,data=train)
summary(lm.fit1)

lm.a2 <- lm(age~.-medv-indus+I(lstat^2),data=train)
summary(lm.a2)

lm.a3 <- lm(age~ .-indus-medv-b-zn+rm*lstat+rm*rad+lstat*rad,data=train)
summary(lm.a3)

#prediction
predicted.age <- predict(lm.a3,test)
test %>% 
  ggplot(aes(age,predicted.age)) +
  geom_point(alpha=0.5) + 
  stat_smooth(aes(colour='red')) +
  xlab('Actual value of age') +
  ylab('Predicted value of age') +
  theme_bw()

#RMSE
rmse.lm <- RMSE(predicted.age, test$age)
rmse.lm

#roc
curve = roc(test$age, predicted.age, plot = TRUE,  print.auc = TRUE)

#decision tree

tree=rpart(age~. ,data=BostonHousing)
prp(tree)
fancyRpartPlot(tree)

#prediction

testpred <-predict(tree,newdata=BostonHousing)
testpred

#rmse
rmse.dt<-RMSE(BostonHousing$age,testpred)

#roc
curve1 = roc(BostonHousing$age, testpred, plot = TRUE,  print.auc = TRUE)
curve1

#RANDOM FOREST

rf=randomForest(age~ .,ntree=2000,mtry=1,data=BostonHousing)
rf

#prediction

pred=predict(rf, BostonHousing)
pred

#rmse

rmse.rf<-RMSE(BostonHousing$age,pred)
rmse.rf

#roc

curve3 = roc(BostonHousing$age, pred, plot = TRUE,  print.auc = TRUE)
summary(rf)
varImpPlot(rf)


