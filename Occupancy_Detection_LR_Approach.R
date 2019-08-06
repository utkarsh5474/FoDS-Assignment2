#----------------------------------------------
#LOGISTIC REGRESSION APPROACH TO SOLVE THE TIME SERIES PROBLEM
#----------------------------------------------


setwd("C:/Users/ACER/Desktop/Assignment#2")
library(psych)
roomtrain = read.table('datatraining.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest1= read.table('datatest.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest2= read.table('datatest2.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
room = rbind(roomtrain,roomtest1,roomtest2)

#converting occupancy status into a label 
room = room[-1]
a<-room$Occupancy
room$Occupancy = factor(room$Occupancy)

#Feature Scaling
room_n = scale(room[1:5])#excluding the lable column

room_n = cbind(room_n,room[6])

cor(room_n$Light,a)
#Randomply sample the data into two parts with replacement and probability
ind = sample(2,nrow(room_n),replace = TRUE,prob = c(0.8,0.2))
train = room_n[ind==1,]
test = room_n[ind==2,]


#Model using all vars
model =  glm(Occupancy~.,train,family= binomial(link = "logit"))
#


#plotting graph of correlation of room dataset with pairwise
pairs.panels(room)
#Light CO2 and Temperatures have corr coeffts >=.5 so we'll attenmpt to build a model with theses
model2<-glm(Occupancy~Light+Temperature+CO2,train,family= binomial(link = "logit")) 

#Light and Occupancy have highest corrleation. So modelling the problem through only cadence levels will make our smodel simpler
model3<-glm(Occupancy~Light,train,family= binomial(link = "logit"))            


#pred = predict(model,test[,-6])
#pred = predict(model2,test[,-6])
#pred = predict(model3,test[,-6])

#All values with probability >= than 0.5 are considered occupied. Threshold value is arbitrarily picked to be 0.5 
pred[pred>=0.5] = 1
pred[pred<0.5] = 0
pred = factor(pred)

   
accuracy<-sum(pred==test$Occupancy)/length(pred)
accuracy*100
#Output:Apprrox 98.78% accurate--model1
#Output:Apprrox 98.78% accurate--model2
#Output: Apprrox 98.85% accurate--model3

