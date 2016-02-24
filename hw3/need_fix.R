# hw3sample.R - sample code for hw3
#
# @author: Yu-Ru Lin
# @date: 2015-02-05
# Modified by Xynoci.

library(MASS) # for the example dataset 
library(plyr) # for recoding data
library(ROCR) # for plotting roc
library(e1071) # for NB and SVM
library(rpart) # for decision tree
library(ada) # for adaboost
library(car) # for recode
library(class) # kNN
library(RGtk2)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

do.classification <- function(train.set, test.set, cl.name, verbose=F) {
  ## note: to plot ROC later, we want the raw probabilities,
  ## not binary decisions
  switch(cl.name, 
         knn = { # here we test k=3; you should evaluate different k's
           prob = knn(train.set[,-11], test.set[,-11], cl=train.set[,11], k = 3, prob=T)
           prob = attr(prob,"prob")
           #print(cbind(prob,as.character(test.set$salary)))
           prob
         },
         lr = { # logistic regression
           model = glm(salary~., family=binomial, data=train.set)
           if (verbose) {
             print(summary(model))             
           }
           prob = predict(model, newdata=test.set, type="response") 
           #print(cbind(prob,as.character(test.set$y)))
           prob
         },
         nb = {
           model = naiveBayes(salary~., data=train.set)
           prob = predict(model, newdata=test.set, type="raw") 
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         dtree = {
           model = rpart(salary~., data=train.set)
           if (verbose) {
             print(summary(model)) # detailed summary of splits
             printcp(model) # print the cross-validation results
             plotcp(model) # visualize the cross-validation results
             ## plot the tree
             # plot(model, uniform=TRUE, main="Classification Tree")
             # text(model, use.n=TRUE, all=TRUE, cex=.8)
           }           
           prob = predict(model, newdata=test.set)
           
           #if (0) { # here we use the default tree, 
           ## you should evaluate different size of tree
           ## prune the tree 
           pfit<- prune(model, cp=model$cptable[which.min(model$cptable[,"xerror"]),"CP"])
           prob = predict(pfit, newdata=test.set)
           ## plot the pruned tree 
           plot(pfit, uniform=TRUE,main="Pruned Classification Tree")
           text(pfit, use.n=TRUE, all=TRUE, cex=.8)          
           fancyRpartPlot(pfit)   
           #}
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob) # renormalize the prob.
           prob
         },
         svm = {
           model = svm(salary~., data=train.set, probability=T)
           if (0) { # fine-tune the model with different kernel and parameters
             ## evaluate the range of gamma parameter between 0.000001 and 0.1
             ## and cost parameter from 0.1 until 10
             tuned <- tune.svm(salary~., data = train.set, 
                               kernel="radial", 
                               gamma = 10^(-6:-1), cost = 10^(-1:1))
             #print(summary(tuned))
             gamma = tuned[['best.parameters']]$gamma
             cost = tuned[['best.parameters']]$cost
             model = svm(salary~., data = train.set, probability=T, 
                         kernel="radial", gamma=gamma, cost=cost)                        
           }
           prob = predict(model, newdata=test.set, probability=T)
           prob = attr(prob,"probabilities")
           #print(cbind(prob,as.character(test.set$y)))
           #print(dim(prob))
           prob = prob[,which(colnames(prob)==1)]/rowSums(prob)
           prob
         },
         ada = {
           model = ada(salary~., data = train.set)
           prob = predict(model, newdata=test.set, type='probs')
           #print(cbind(prob,as.character(test.set$y)))
           prob = prob[,2]/rowSums(prob)
           prob
         }
  ) 
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
      '#training:',nrow(train.set),
      '#testing',nrow(test.set),'\n')
  prob = do.classification(train.set, test.set, cl.name)
  # prob is an array of probabilities for cases being positive
  
  ## get confusion matrix
  predicted = as.numeric(prob > prob.cutoff)
  actual = test.set$salary
  confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
  error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
  cat('error rate:',error,'\n')
  # you may compute other measures based on confusion.matrix
  # @see handout03 p.32-36
  
  ## plot ROC
  result = data.frame(prob,actual)
  pred = prediction(result$prob,result$actual)
  perf = performance(pred, "tpr","fpr")
  #plot(perf)    
}

k.fold.cv <- function(dataset, cl.name, k.fold=10, prob.cutoff=0.5, color='red') {
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
    cat(k.fold,'-fold CV run',k,cl.name,':',
        '#training:',nrow(train.set),
        '#testing',nrow(test.set),'\n')
    prob = do.classification(train.set, test.set, cl.name)
    
    predicted = as.numeric(prob > prob.cutoff)
    actual = test.set$salary
    confusion.matrix = table(actual,factor(predicted,levels=c(0,1)))
    confusion.matrix
    error = (confusion.matrix[1,2]+confusion.matrix[2,1]) / nrow(test.set)  
    errors[k] = error
    cat('\t\terror=',error,'\n')
    probs = c(probs,prob)
    actuals = c(actuals,actual)
    ## you may compute other measures and store them in arrays
  }
  avg.error = mean(errors)
  cat(k.fold,'-fold CV results:','avg error=',avg.error,'\n')
  
  ## plot ROC
  result = data.frame(probs,actuals)
  pred = prediction(result$probs,result$actuals)
  perf = performance(pred, "tpr","fpr")
  plot(perf, col=color)  
  
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
  cat('error=',err, 'accuracy=', accuracy, 'precision=',precision,'recall=',recall,'f-score',fscore,'\n')
  auc = get.measure(pred, 'auc')
  cat('auc=',auc,'\n')
  return(c(accuracy, precision, recall, fscore, auc))
}

my.classifier <- function(dataset, cl.name, do.cv=F, color='red') {
  n.obs <- nrow(dataset) # no. of observations in dataset
  n.cols <- ncol(dataset) # no. of predictors
  cat('my dataset:',
      n.obs,'observations',
      n.cols-1,'predictors','\n')
  print(dataset[1:3,])
  cat('label (y) distribution:')
  print(table(dataset$salary))
  
  pre.test(dataset, cl.name)
  if (do.cv) {
    stat <- k.fold.cv(dataset, cl.name, color)
    return(stat)
  }
}

# main
set.seed(1) # set the seed so you can get exactly the same results whenever you run the code

url = "http://www.yurulin.com/class/spring2016_datamining/data/adult.csv"
dataset <- read.csv(url, header = TRUE, sep = ',')
head(dataset)
summary(dataset)

dataset$age = scale(dataset$age, center = T, scale = T)
dataset$fnlwgt = scale(dataset$fnlwgt, center = T, scale = T)
dataset$edu_num = scale(dataset$edu_num, center = T, scale = T)
dataset$cap_gain = scale(dataset$cap_gain, center = T, scale = T)
dataset$cap_loss = scale(dataset$cap_loss, center = T, scale = T)
dataset$hours = scale(dataset$hours, center = T, scale = T)

dataset$work = recode(dataset$work, "' Federal-gov'=1; ' Local-gov'=2; ' Private'=3; ' Self-emp-inc'=4; ' Self-emp-not-inc'=5; ' State-gov'=6; ' ?'=3")
dataset$relation = recode(dataset$relation, "' Husband'=1; ' Not-in-family'=2; ' Other-relative'=3; ' Own-child'=4; ' Unmarried'=5; ' Wife'=6")
dataset$race = recode(dataset$race, "' Amer-Indian-Eskimo'=1; ' Asian-Pac-Islander'=2; ' Black'=3; ' Other'=4; ' White'=5")
dataset$sex = recode(dataset$sex, "' Male'=1; else=0")
dataset$salary = recode(dataset$salary, "' >50K'=1; else=0")
summary(dataset)

row.name <- c("Logistic", "kNN", "NB", "Decision Tree", "SVM", "Ada")
column.name<-c("Accuracy","Precision", "Recall", "F-Score", "AUC")

color.set <- c('red', 'yellow', 'green', 'blue', 'cyan', 'magenta')
cl.name.set <- c('lr','knn','nb','dtree','svm','ada')

stat<-NULL
for(i in c(1:6)){
  stat<-cbind(stat,my.classifier(dataset, cl.name.set[i], do.cv=T, color.set[i]))
  par(new = T)
}
legend("bottomright", legend=cl.name.set, col=color.set, lwd=6)
stat

