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

datasetPath <-"./data/dataset.csv"
d<-read.csv(datasetPath, sep=",", header=T, row.names=1)
dim(d)
d[1:10,1:5]

############################################################
#################### i am delimiter. ####################
############################################################
library(MASS) # for the example dataset 
library(plyr) # for recoding data
library(ROCR) # for plotting roc
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost
library(car) # for recode
library(class) # kNN
library(RGtk2)
library(rattle) # fancyRpartPlot
library(rpart.plot)
library(RColorBrewer)
library(ggplot2)

############################################################
######################### main #########################
############################################################
set.seed(1) # set the seed so you can get exactly the same results whenever you run the code
url = "./data/dataset.csv"
dataset <- read.csv(url, header = TRUE, sep = ',', row.names=1)
dataset[1,]
ds<-dataset[,-1]

summary(ds)
dim(ds)

## change color
coln = 1
col = c("red", "yellow", "green", "blue", "cyan", "purple")

name.list = c("dtree", "svm", "ada")
rowname = c("fscore")
data.list = vector()
fscore.matrix = matrix(c(data.list), nrow = 1, ncol = 3, byrow = TRUE, dimnames = list(rowname, name.list))
fscores = data.frame(fscore.matrix)
f.data <- vector()

rowname = c("AUC")
auc.matrix = matrix(c(data.list), nrow = 1, ncol = 3, byrow = TRUE, dimnames = list(rowname, name.list))
aucs = data.frame(auc.matrix)
a.data <- vector()

##### perform classification #####
my.classifier(ds, cl.name='dtree',do.cv=T)
par(new = TRUE)
my.classifier(ds, cl.name='svm',do.cv=F)
par(new = TRUE)
my.classifier(ds, cl.name='ada',do.cv=T)
legend("bottomright", legend=name.list, col=col, lwd=3)
f.data
a.data

############################################################
#################### performance plots ####################
############################################################
fscores = data.frame(x = name.list, y = f.data)
fscores
## fscore bar chart
ggplot(fscores, aes(x, y, fill = y)) + geom_bar(stat = 'identity')
## auc bar chart
aucs = data.frame(x = name.list, y = a.data)
ggplot(aucs, aes(x, y, fill = y)) + geom_bar(stat = 'identity')

############################################################
#################### functions ####################
############################################################
my.classifier <- function(dataset, cl.name, do.cv=F) {
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.cols <- ncol(dataset) # no. of predictors
  cat('my dataset:',
      n.obs,'observations',
      n.cols-1,'predictors','\n')
  # dataset[1,]
  cat('label (y) distribution:')
  table(dataset$result)
  
  pre.test(dataset, cl.name)
  if (do.cv) k.fold.cv(dataset, cl.name)
}

pre.test <- function(dataset, cl.name, r=0.6, prob.cutoff=0.5) {
  ## Let's use 60% random sample as training and remaining as testing
  ## by default use 0.5 as cut-off
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.train = floor(n.obs*r)
  train.idx = sample(1:n.obs,n.train)
  train.idx
  train.set = dataset[train.idx,]
  test.set = dataset[-train.idx,]
  cat('pre-test',cl.name,':',
      '#training:', nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name, F)
  # prob is an array of probabilities for cases being positive
  
  ## get confusion matrix
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$result
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  cat('error rate:',error,'\n')
  # you may compute other measures based on confusion.matrix
  # @see handout03 p.32-36
  
  ## plot ROC
  result = data.frame(prob,actual)
  pred = prediction(result$prob,result$actual)
  perf = performance(pred, "tpr","fpr")
  plot(perf)
  
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name='auc') {
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    #     print(slot(perf, "x.values"))
    #     print(slot(perf, "y.values"))
    m
  }
  err = mean(get.measure(pred, 'err'))
  accuracy = mean(get.measure(pred, 'acc'))
  precision = mean(get.measure(pred, 'prec'),na.rm=T)
  recall = mean(get.measure(pred, 'rec'),na.rm=T)
  fscore = mean(get.measure(pred, 'f'),na.rm=T)
  cat('error=',err, 'accuracy=',accuracy, 'precision=',precision, 'recall=',recall,'f-score',fscore,'\n')
  auc = get.measure(pred, 'auc')
  cat('auc=',auc,'\n')
}

do.classification <- function(train.set, test.set, cl.name, verbose=T) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(cl.name, 
         dtree = {
           model = rpart(result~ X0311AvgSum + X0311AvgSD + X0311Count + X0311SourcesSum + X071AvgSD + X1244ArticlesSum, data=train.set)
           if (verbose) {
             # detailed summary of splits
             summary(model) 
             ## print the cross-validation results
             ## http://stackoverflow.com/questions/29197213/what-is-the-difference-between-rel-error-and-x-error-in-a-rpart-decision-tree
             printcp(model)
             prob = predict(model, newdata=test.set)
             ## plot the tree
             fancyRpartPlot(model, sub="Decision Tree")
           }           
           
           if (0) { 
             # here we use the default tree, you should evaluate different size of tree prune the tree  
             ## 1. minimum xerror
             cut.at <- which.min(model$cptable[,"xerror"])
             ## 2. minimum xstd
             # cut.at <- which.min(model$cptable[,"xstd"])
             ## 3. A rule of thumb is to choose the lowest level where the rel_error + xstd < xerror
             # cut.at <- min(which(model$cptable[,"rel error"] + model$cptable[,"xstd"] < model$cptable[,"xerror"]))
             pfit <- prune(model, cp = model$cptable[cut.at,"CP"])
             cat('*Tree pruned at level:', cut.at, ', CP:', model$cptable[cut.at,"CP"], '\n')
             printcp(pfit)
             print(pfit$variable.importance)
             prob = predict(pfit, newdata=test.set)
             ## plot the pruned tree 
             fancyRpartPlot(pfit, sub="Pruned Decision Tree") 
           }
           # prob <- cbind(prob,as.character(test.set$result))
           # renormalize the prob
           # prob <- prob[,2]/rowSums(prob)
           prob
         },      
         svm = {
           model = svm(result~ X0311AvgSum + X0311AvgSD + X0311Count + X0311SourcesSum + X071AvgSD + X1244ArticlesSum, data=train.set, probability=T)
           if (1) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(result~., data = train.set, 
                               kernel="linear", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(result~., data = train.set, probability=T, 
                         kernel="linear", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           # print(cbind(prob,as.character(test.set$result)))
           # prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         ada = {
           model = ada(result~., data = train.set)
           prob = predict(model, newdata=test.set, type='probs')
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob)
           prob
         }
  ) 
}

k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5) {
  ## default: 10-fold CV, cut-off 0.5 
  n.obs <- nrow(dataset) # no. of observations 
  s = sample(n.obs)
  errors = dim(k.fold)
  probs = NULL
  actuals = NULL
  for (k in 1:k.fold) {
    test.idx = which(s %% k.fold == (k-1) ) # use modular operator
    train.set = dataset[-test.idx,]
    test.set = dataset[test.idx,]
    cat('\n', k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    prob = do.classification(train.set, test.set, cl.name, T)
    
    predicted = as.numeric(prob > prob.cutoff)
    actual = test.set$result
    confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
    confusion.matrix
    error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
    errors[k] = error
    cat('\terror=',error,'\n')
    probs = c(probs,prob)
    actuals = c(actuals,actual)
    ## you may compute other measures and store them in arrays
  }
  avg.error = mean(errors)
  avg.accuracy = 1 - avg.error
  cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
  
  ## plot ROC
  result = data.frame(probs,actuals)
  pred = prediction(result$probs,result$actuals)
  perf = performance(pred, "tpr","fpr")
  
  if (coln %% 6 != 0)
    plot(perf, col = col[coln %% 6])
  else
    plot(perf, col = col[6])
  coln <- coln + 1
  
  ## get other measures by using 'performance'
  get.measure <- function(pred, measure.name='auc') {
    perf = performance(pred,measure.name)
    m <- unlist(slot(perf, "y.values"))
    #     print(slot(perf, "x.values"))
    #     print(slot(perf, "y.values"))
    m
  }
  err = mean(get.measure(pred, 'err'))
  accuracy = mean(get.measure(pred, 'acc'))
  precision = mean(get.measure(pred, 'prec'),na.rm=T)
  recall = mean(get.measure(pred, 'rec'),na.rm=T)
  fscore = mean(get.measure(pred, 'f'),na.rm=T)
  cat('error=',err, 'accuracy=',accuracy, 'precision=',precision, 'recall=',recall,'f-score',fscore,'\n')
  auc = get.measure(pred, 'auc')
  cat('auc=',auc,'\n')
  
  fscores[cl.name] <- fscore
  aucs[cl.name] <- auc
  f.data[cl.name] <- fscore
  a.data[cl.name] <- auc
}

library("ggplot2")
## IR static
dim(ds)
ds[1,]
