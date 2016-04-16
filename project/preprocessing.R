fromPath <- "./data/dataset.csv"
data.event <- read.csv(fromPath, sep = ",", header = T)
dim(data.event)
data.event[1:10,]
summary(data.event)
monthYear<-data.event[,1]
summary(monthYear)
monthYear
event.new<-data.event[1:(length(data.event$MonthYear)-4),]

responsePath <-"./data/result.csv"
result.data <- read.csv(responsePath, sep = ",", header = T)
dim(data.result)
result.data[1:10,]
result.monthYear<-result.data$YEAR
summary(result.monthYear)
result.data[length(result.data$YEAR),]
result.new<-result.data[(1979-1914+1):length(result.data$YEAR),3:length(result.data)]
result.new[1:10,]
result.col<-data.frame(result=as.matrix(as.vector(t(as.matrix(data.new))),ncol=1))
result.col
mapper<-matrix(c(1,1,2,3,"postive effect","positive effect", "no effect", "negtive effect"), ncol=2,byrow=F)
mapper
library(plyr)
classes<-mapvalues(result.col$result, from=as.vector(mapper[,2]), to=as.vector(mapper[,1]))
classes
dataset<-cbind(event.new,y=classes)
dim(dataset)
dataset[1:5,]
write.table(dataset, file = "./data/foo.csv", sep = ",",row.names=F, col.names = T)
