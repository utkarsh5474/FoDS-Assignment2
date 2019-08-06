
#----------------------------------------------
#NEURAL NETWORK APPROACH TO SOLVE THE TIME SERIES PROBLEM
#----------------------------------------------
setwd("C:/Users/ACER/Desktop/Assignment#2")

roomtrain = read.table('datatraining.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest1= read.table('datatest.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
roomtest2= read.table('datatest2.txt',header = TRUE,sep = ',',stringsAsFactors = FALSE)
room = rbind(roomtrain,roomtest1,roomtest2)

room_n = scale(room[1:5])#excluding the lable column
room_n = cbind(room_n,room[6])

#Randomply sample the data into two parts with replacement and probability
ind = sample(2,nrow(room_n),replace = TRUE,prob = c(0.8,0.2))
train = room_n[ind==1,]
test = room_n[ind==2,]

#converting occupancy status into a label 
room = room[-1]

library(neuralnet)
#building a neural network with one hidden layer of two nodes
NN = neuralnet(Occupancy ~ Temperature + Humidity + Light + CO2 + HumidityRatio,data=train,linear.output = TRUE,hidden = 2)

plot(NN)
predict_testNN = compute(NN, test[,-6])
pred<-predict_testNN$net.result
plot(pred)
#predict_testNN = (predict_testNN$net.result * (max(data$rating) - min(data$rating))) + min(data$rating)

#From the graph, for all predictions greater than 0.5 ???1 else 0
pred[pred>=0.5] = 1
pred[pred<0.5] = 0
pred = factor(pred)
plot(pred)

accuracy<-sum(pred==test$Occupancy)/length(pred)
accuracy*100
