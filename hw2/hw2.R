####################################################################################################
# 1. Read the data description. Identify and report response variable and predictors.
australia<-read.csv("http://www.yurulin.com/class/spring2016_datamining/data/australian.csv")
any(is.na(australia))
# [1] FALSE
head(australia)
#   A1    A2     A3 A4 A5 A6    A7 A8 A9 A10 A11 A12 A13  A14 A15
# 1  1 22.08 11.460  2  4  4 1.585  0  0   0   1   2 100 1213   0
# 2  0 22.67  7.000  2  8  4 0.165  0  0   0   0   2 160    1   0
# 3  0 29.58  1.750  1  4  4 1.250  0  0   0   1   2 280    1   0
# 4  0 21.67 11.500  1  5  3 0.000  1  1  11   1   2   0    1   1
# 5  1 20.17  8.170  2  6  4 1.960  1  1  14   0   2  60  159   1
# 6  0 15.83  0.585  2  8  8 1.500  1  1   2   0   2 100    1   1

# According to the description, response variable is A15(credit) and all other 14 variables are predictors.

####################################################################################################
# 2.Explor the dataset, and generate both statistical and graphical summary with respect to the numerical and 
#   categorical vaiables.
####################################################################################################
# a) Generate a summary table for the data. For each numerical variable, list: variable name, mean, median, 1st 
#   quartile, 3rd quartile, and standard deviation.
australia.num <- australia[c(2,3,7,10,13,14)]
australia.cat <- australia[-c(2,3,7,10,13,14)]
rowName.num <- names(australia.num)
rowName.cat <- names(australia.cat)
columnName<-c("Mean","Median", "1stQuartile", "3rdQuartile", "Sd.")

a2<-c(as.vector(summary(australia.num$A2))[c(4,3,2,5)],sd(australia.num$A2))
a3<-c(as.vector(summary(australia.num$A3))[c(4,3,2,5)],sd(australia.num$A3))
a7<-c(as.vector(summary(australia.num$A7))[c(4,3,2,5)],sd(australia.num$A7))
a10<-c(as.vector(summary(australia.num$A10))[c(4,3,2,5)],sd(australia.num$A10))
a13<-c(as.vector(summary(australia.num$A13))[c(4,3,2,5)],sd(australia.num$A13))
a14<-c(as.vector(summary(australia.num$A14))[c(4,3,2,5)],sd(australia.num$A14))

summary.num<-matrix(c(a2,a3,a7,a10,a13,a14), nrow=6, ncol=5, byrow=TRUE, dimnames=list(rowName.num, columnName))
summary.num
#         Mean Median 1stQuartile 3rdQuartile         Sd.
# A2    31.570  28.62      22.670      37.710   11.853273
# A3     4.759   2.75       1.000       7.208    4.978163
# A7     2.223   1.00       0.165       2.625    3.346513
# A10    2.400   0.00       0.000       3.000    4.862940
# A13  184.000 160.00      80.000     272.000  172.159274
# A14 1018.000   6.00       1.000     396.500 5210.102598

####################################################################################################
# b) For numerfic variables, plot the density distribution. Describe whether the variable has a noral distribution
#   or certain type of skew distribution.
library('ggplot2')
ggplot(australia.num, aes(x = A2)) + geom_density()
ggplot(australia.num, aes(x = A3)) + geom_density()
ggplot(australia.num, aes(x = A7)) + geom_density()
ggplot(australia.num, aes(x = A10)) + geom_density()
ggplot(australia.num, aes(x = A13)) + geom_density()
ggplot(australia.num, aes(x = A14)) + geom_density()

####################################################################################################
# c) For each categorical predictor, generate the conditional histogram plot of response variable.
head(australia.cat)
#   A1 A4 A5 A6 A8 A9 A11 A12 A15
# 1  1  2  4  4  0  0   1   2   0
# 2  0  2  8  4  0  0   0   2   0
# 3  0  1  4  4  0  0   1   2   0
# 4  0  1  5  3  1  1   1   2   1
# 5  1  2  6  4  1  1   0   2   1
# 6  0  2  8  8  1  1   0   2   1

ggplot(australia, aes(factor(A1))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A4))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A5), fill=A5)) + geom_histogram(binwidth=1)+ facet_grid(A15~.)
ggplot(australia, aes(factor(A6))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A8))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A9))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A11))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)
ggplot(australia, aes(factor(A12))) + geom_histogram(binwidth=1)+ facet_grid(.~A15)

####################################################################################################
# 3. Apply logistic regression analysis to predict A15. Evaluate the models through cross-validation and on 
#    holdout samples. Interpret the effect of the predictors.
####################################################################################################
# a) Implement a 10-fold cross-validation scheme by splitting the data into training and testing sets. User 
#    the training set to train a logistic regression model to predict the response variable. Examine the 
#    performance of different models by varing the number of predictors. Report the performance of the models 
#    on testing set user proper measures (accuracy, precision, recall, F1) and plots (ROC, lift).
library(cvTools) # https://cran.r-project.org/web/packages/cvTools/index.html
library(ROCR) # https://cran.r-project.org/web/packages/ROCR/index.html
predictors<-model.matrix(A15~., data=australia)[,-1]
train<-NULL
crossValidation<-function(formula, fold=10){
  theSets<-cvFolds(nrow(australia), K=fold)
  errs<-dim(fold)
  precisions<-dim(fold)
  recalls<-dim(fold)
  fscores<-dim(fold)
  probs<-NULL
  actuals<-NULL
  
  for(i in (1:fold)){
    train<-which(theSets$which!=i)
    test<-which(theSets$which==i)
    model<-glm(formula, family=binomial(link="logit"), data=australia, subset=train)
    
    prob<-predict(model, australia[test,], type=c("response"))
    predicted<-floor(prob+0.5)
    actual<-australia[test,]$A15
    conf.matrix<-table(actual, predicted)
    conf.matrix
    err<-(conf.matrix[1,2]+conf.matrix[2,1])/nrow(australia[test,])
    errs[i]<-err
    
    if(conf.matrix[1,2]+conf.matrix[2,2]==0)
      precision<-0
    else
      precision<-conf.matrix[2,2]/(conf.matrix[1,2]+conf.matrix[2,2])
    precisions[i]<-precision
    
    if(conf.matrix[2,1]+conf.matrix[2,2]==0)
      recall<-0
    else
      recall<-conf.matrix[2,2]/(conf.matrix[2,1]+conf.matrix[2,2])
    recalls[i]<-recall
    
    fscore<-(2 * precision * recall)/(precision + recall)
    fscores[i]<-fscore
    
    probs<-c(probs, prob)
    actuals<-c(actuals, actual)
  }
  
  model<-glm(formula, family=binomial(link="logit"), data = australia)
  print(summary(model))
  
  avg_err<-mean(errs)
  avg_precision<-mean(precisions)
  avg_recall<-mean(recalls)
  avg_fscore<-mean(fscores)
  
  rowName<-c("Model")
  columnName<-c("Accuracy", "Precision", "Recall", "F1 Score", "AIC")
  
  stat<-matrix(c(1-avg_err, avg_precision, avg_recall, avg_fscore, summary(model)$aic), nrow=1, ncol=5, byrow=TRUE, dimnames=list(rowName, columnName))
  print(stat)
  
  plotRocAndLift(probs, actuals)
  return(stat)
}

plotRocAndLift<-function(probs, actuals){
  df<-data.frame(probs, actuals)
  pred<-prediction(df$probs, df$actuals)
  perf<-performance(pred, "tpr", "fpr")
  plot(perf, main="ROC")
  
  rank.df<-as.data.frame(df[order(probs, decreasing=TRUE),])
  colnames(rank.df)<-c('predicted', 'actual')
  baseRate<-mean(actuals)
  baseRate
  total<-length(australia$A15)
  ax<-dim(total)
  ay.base<-dim(total)
  ay.pred<-dim(total)
  
  ax[1]<-1
  ay.base[1]<-baseRate
  ay.pred[1]<-rank.df$actual[1]
  for(i in 2:total){
    ax[i]<-i
    ay.base[i]<-baseRate*i
    ay.pred[i]<-ay.pred[i-1]+rank.df$actual[i]
  }
  
  plot(ax, ay.pred, xlab="Number of Cases", ylab='Number of Successes', main="Lift")
  points(ax, ay.base, type="l")
}

par(mfrow=c(1,2))

f1<-A15~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14
m1<-crossValidation(f1)
# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.8392  -0.3869  -0.1734   0.4162   3.2194  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.515e+00  1.148e+00  -5.673  1.4e-08 ***
# A1           5.079e-05  2.759e-01   0.000  0.99985    
# A2           2.899e-04  1.167e-02   0.025  0.98017    
# A3          -3.594e-02  2.667e-02  -1.348  0.17777    
# A4           8.594e-01  3.044e-01   2.823  0.00476 ** 
# A5           1.968e-01  4.100e-02   4.799  1.6e-06 ***
# A6           3.852e-02  7.399e-02   0.521  0.60258    
# A7           7.417e-02  4.868e-02   1.524  0.12761    
# A8           3.363e+00  3.024e-01  11.121  < 2e-16 ***
# A9           3.354e-01  3.486e-01   0.962  0.33597    
# A10          1.298e-01  5.821e-02   2.230  0.02577 *  
# A11         -2.841e-01  2.602e-01  -1.092  0.27494    
# A12          4.873e-01  4.196e-01   1.161  0.24552    
# A13         -1.953e-03  8.463e-04  -2.307  0.02105 *  
# A14          4.838e-04  1.670e-04   2.898  0.00376 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 423.97  on 675  degrees of freedom
# AIC: 453.97
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score     AIC
# Model 0.8753623  0.838652 0.8884984 0.8613773 453.965

# Accroding to the Model 1, drop the insignificant variable A1 (p-value=0.99985)

f2<-A15~A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14
m2<-crossValidation(f2)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.8392  -0.3869  -0.1734   0.4162   3.2194  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.5148645  1.1152651  -5.842 5.17e-09 ***
# A2           0.0002901  0.0116408   0.025  0.98012    
# A3          -0.0359420  0.0266701  -1.348  0.17777    
# A4           0.8593828  0.3016624   2.849  0.00439 ** 
# A5           0.1967543  0.0409923   4.800 1.59e-06 ***
# A6           0.0385243  0.0739525   0.521  0.60241    
# A7           0.0741676  0.0485903   1.526  0.12691    
# A8           3.3630841  0.3024159  11.121  < 2e-16 ***
# A9           0.3353757  0.3484246   0.963  0.33577    
# A10          0.1297973  0.0582000   2.230  0.02573 *  
# A11         -0.2840542  0.2601693  -1.092  0.27492    
# A12          0.4872772  0.4184383   1.165  0.24422    
# A13         -0.0019526  0.0008452  -2.310  0.02088 *  
# A14          0.0004838  0.0001667   2.902  0.00371 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 423.97  on 676  degrees of freedom
# AIC: 451.97
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score     AIC
# Model 0.8753623  0.838652 0.8884984 0.8613773 451.965

# Accroding to the Model 2, drop the insignificant variable A2 (p-value=0.98012)

f3<-A15~A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+A13+A14
m3<-crossValidation(f3)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.8372  -0.3869  -0.1734   0.4164   3.2201  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.5096572  1.0955769  -5.942 2.82e-09 ***
# A3          -0.0358926  0.0265946  -1.350  0.17714    
# A4           0.8602512  0.2996475   2.871  0.00409 ** 
# A5           0.1965842  0.0404196   4.864 1.15e-06 ***
# A6           0.0385083  0.0739528   0.521  0.60257    
# A7           0.0744677  0.0470682   1.582  0.11362    
# A8           3.3639295  0.3005308  11.193  < 2e-16 ***
# A9           0.3344326  0.3463472   0.966  0.33424    
# A10          0.1299064  0.0580269   2.239  0.02517 *  
# A11         -0.2839616  0.2601414  -1.092  0.27502    
# A12          0.4884013  0.4160539   1.174  0.24044    
# A13         -0.0019520  0.0008448  -2.310  0.02086 *  
# A14          0.0004841  0.0001665   2.908  0.00364 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 423.97  on 677  degrees of freedom
# AIC: 449.97
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8753623  0.838652 0.8884984 0.8613773 449.9656# 

# Accroding to the Model 3, drop the insignificant variable A6 (p-value=0.60257)

f4<-A15~A3+A4+A5+A7+A8+A9+A10+A11+A12+A13+A14
m4<-crossValidation(f4)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.7847  -0.3797  -0.1700   0.4171   3.2190  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.4271556  1.0826212  -5.937 2.91e-09 ***
# A3          -0.0359559  0.0264971  -1.357   0.1748    
# A4           0.8539635  0.2990882   2.855   0.0043 ** 
# A5           0.2018757  0.0390506   5.170 2.35e-07 ***
# A7           0.0791108  0.0464044   1.705   0.0882 .  
# A8           3.3811569  0.2993650  11.294  < 2e-16 ***
# A9           0.3226998  0.3449782   0.935   0.3496    
# A10          0.1306875  0.0579398   2.256   0.0241 *  
# A11         -0.2820945  0.2600183  -1.085   0.2780    
# A12          0.5129293  0.4133658   1.241   0.2147    
# A13         -0.0019220  0.0008445  -2.276   0.0228 *  
# A14          0.0004802  0.0001654   2.902   0.0037 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 424.24  on 678  degrees of freedom
# AIC: 448.24
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision   Recall  F1 Score      AIC
# Model 0.8724638 0.8371482 0.882308 0.8577047 448.2368# 

# Accroding to the Model 4, drop the insignificant variable A9 (p-value=0.3496)

f5<-A15~A3+A4+A5+A7+A8+A10+A11+A12+A13+A14
m5<-crossValidation(f5)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.9556  -0.3813  -0.1694   0.4215   3.2132  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.5285915  1.0730736  -6.084 1.17e-09 ***
# A3          -0.0362179  0.0263602  -1.374 0.169454    
# A4           0.8695613  0.2970942   2.927 0.003424 ** 
# A5           0.2064454  0.0387221   5.331 9.74e-08 ***
# A7           0.0776666  0.0468212   1.659 0.097158 .  
# A8           3.4051603  0.2991876  11.381  < 2e-16 ***
# A10          0.1665186  0.0456906   3.644 0.000268 ***
# A11         -0.2958179  0.2594052  -1.140 0.254132    
# A12          0.5657823  0.4064698   1.392 0.163940    
# A13         -0.0019081  0.0008451  -2.258 0.023956 *  
# A14          0.0004905  0.0001646   2.980 0.002882 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 425.10  on 679  degrees of freedom
# AIC: 447.1
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8695652  0.833312 0.8793668 0.8545928 447.1043

# Accroding to the Model 5, drop the insignificant variable A11 (p-value=0.254132)

f6<-A15~A3+A4+A5+A7+A8+A10+A12+A13+A14
m6<-crossValidation(f6)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.8845  -0.3878  -0.1701   0.4301   3.1709  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -6.5913822  1.0728192  -6.144 8.05e-10 ***
# A3          -0.0361924  0.0262967  -1.376 0.168726    
# A4           0.8874259  0.2974274   2.984 0.002848 ** 
# A5           0.2063557  0.0385893   5.347 8.92e-08 ***
# A7           0.0704874  0.0460717   1.530 0.126030    
# A8           3.3731648  0.2958906  11.400  < 2e-16 ***
# A10          0.1624181  0.0452499   3.589 0.000331 ***
# A12          0.5403451  0.4057435   1.332 0.182945    
# A13         -0.0020319  0.0008368  -2.428 0.015173 *  
# A14          0.0004970  0.0001640   3.030 0.002445 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 426.42  on 680  degrees of freedom
# AIC: 446.42
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8768116 0.8410048 0.8892494 0.8631643 446.4181# 

# Accroding to the Model 6, drop the insignificant variable A12 (p-value=0.182945)

f7<-A15~A3+A4+A5+A7+A8+A10+A13+A14
m7<-crossValidation(f7)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -2.9135  -0.3852  -0.1787   0.4353   3.1867  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.5033213  0.6691167  -8.225  < 2e-16 ***
# A3          -0.0345068  0.0261834  -1.318 0.187541    
# A4           0.8534181  0.2942773   2.900 0.003731 ** 
# A5           0.2091438  0.0382866   5.463 4.69e-08 ***
# A7           0.0649744  0.0456409   1.424 0.154562    
# A8           3.3512529  0.2940501  11.397  < 2e-16 ***
# A10          0.1683910  0.0448146   3.758 0.000172 ***
# A13         -0.0020851  0.0008237  -2.531 0.011365 *  
# A14          0.0005146  0.0001643   3.132 0.001734 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 428.20  on 681  degrees of freedom
# AIC: 446.2
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8753623 0.8344244 0.8959527 0.8628379 446.1996

# Accroding to the Model 7, drop the insignificant variable A3 (p-value=0.187541)

f8<-A15~A4+A5+A7+A8+A10+A13+A14
m8<-crossValidation(f8)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.0086  -0.3883  -0.1894   0.4430   3.1799  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.5718215  0.6634251  -8.399  < 2e-16 ***
# A4           0.8287806  0.2914187   2.844 0.004456 ** 
# A5           0.2059830  0.0380135   5.419    6e-08 ***
# A7           0.0583152  0.0455015   1.282 0.199980    
# A8           3.2910701  0.2889575  11.389  < 2e-16 ***
# A10          0.1649087  0.0449244   3.671 0.000242 ***
# A13         -0.0018708  0.0008047  -2.325 0.020076 *  
# A14          0.0004925  0.0001594   3.089 0.002008 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 429.95  on 682  degrees of freedom
# AIC: 445.95
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8695652 0.8300914 0.8861067 0.8556656 445.9469

# Accroding to the Model 8, drop the insignificant variable A7 (p-value=0.199980)

f9<-A15~A4+A5+A8+A10+A13+A14
m9<-crossValidation(f9)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.0922  -0.3884  -0.1852   0.4527   3.2002  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.5855447  0.6659856  -8.387  < 2e-16 ***
# A4           0.8487850  0.2923186   2.904 0.003689 ** 
# A5           0.2101790  0.0377860   5.562 2.66e-08 ***
# A8           3.3812958  0.2823285  11.976  < 2e-16 ***
# A10          0.1697808  0.0442963   3.833 0.000127 ***
# A13         -0.0018533  0.0007958  -2.329 0.019865 *  
# A14          0.0004829  0.0001583   3.050 0.002285 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 431.68  on 683  degrees of freedom
# AIC: 445.68
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8753623 0.8344525 0.8955605 0.8622741 445.6832# 

# All coefficients are significant under 0.95. 

f10<-A15~A4+A5+A8+A10+A14
m10<-crossValidation(f10)

# Call:
# glm(formula = formula, family = binomial(link = "logit"), data = australia)
# 
# Deviance Residuals: 
#     Min       1Q   Median       3Q      Max  
# -3.1263  -0.3844  -0.1948   0.4765   3.0237  
# 
# Coefficients:
#               Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -5.7635481  0.6625130  -8.700  < 2e-16 ***
# A4           0.8064830  0.2910215   2.771  0.00558 ** 
# A5           0.1977348  0.0369144   5.357 8.48e-08 ***
# A8           3.3546622  0.2794903  12.003  < 2e-16 ***
# A10          0.1817626  0.0441111   4.121 3.78e-05 ***
# A14          0.0004774  0.0001539   3.102  0.00192 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
#     Null deviance: 948.16  on 689  degrees of freedom
# Residual deviance: 437.26  on 684  degrees of freedom
# AIC: 449.26
# 
# Number of Fisher Scoring iterations: 7
# 
#        Accuracy Precision    Recall  F1 Score      AIC
# Model 0.8681159  0.835202 0.8707504 0.8514419 449.2552

####################################################################################################
# b) For the best model, compute the odds ratio and interpret the effect of each predictor.
measures<-rbind(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10)
rownames(measures)<-c('Model 1','Model 2','Model 3','Model 4','Model 5','Model 6','Model 7','Model 8','Model 9','Model 10')
measures

# According to the results above, the Model 9, which has the smallest AIC and all significant coeficnents under 0.01 level, is the best model.
model9<-glm(f9, family=binomial(link="logit"), data=australia)
e<-exp(model9$coef)
for(i in 2:length(e))
  print(sprintf("%s: %.2f%%",names(e)[i],(abs(e[i]-1))*100))
