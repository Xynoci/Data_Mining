########################################################################################################################
# Data description: [bike_description.txt](http://www.yurulin.com/class/spring2016_datamining/data/bike_description.txt)
#
# Original Data source is from: http://archive.ics.uci.edu/ml/datasets/Bike+Sharing+Dataset
# 
# The bike.csv for hw1 is a little different from the table provided from the original source url.
# Please use the bike.csv we provided and read this description for your homework.
# 
# The objective is to explain bike sharing value cnt in terms of the provided possible influential factors.
# (we do not usually use linear regression for count data, so here the response variable is cnt: the square root of bike sharing)
# 
# Input variables:
# - 1 season : season (1:springer, 2:summer, 3:fall, 4:winter)
# - 2 yr : year (0: 2011, 1:2012)
# - 3 mnth : month ( 1 to 12)
# - 4 holiday : weather day is holiday or not (extracted from [Web Link])
# - 5 weekday : day of the week
# - 6 weathersit : 
#   - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#   - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#   - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#   - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog
# - 7 temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
# - 8 atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
# - 9 hum: Normalized humidity. The values are divided to 100 (max)
# - 10 windspeed: Normalized wind speed. The values are divided to 67 (max)
# - 11 cnt: the square root of total rental bike count (including both casual and registered)
########################################################################################################################

url <- 'http://www.yurulin.com/class/spring2016_datamining/data/bike.csv'
ds.bike <- read.csv(url , header = TRUE , sep = ',')
head(ds.bike)

########################################################################################################################
# 1. Read the data description. Check if there is missing value. Identify and report response variable and predictors (also called 
# explanatory variables or features). Report the numerical variables and categorical variables in the dataset.
########################################################################################################################

########################################################################################################################
# 2. Explore the dataset, and generate both statistical and graphical summary.
########################################################################################################################
# - a) Generate a summary table for the numerical variables. For each one, list: variable name, mean, median, 1st quartile,  
#      3rd quartile, and standard deviation.

ds.numeric<-ds.bike[7:10]
rowName<-names(ds.numeric)
columnName<-c("Mean","Median", "1stQuartile", "3rdQuartile", "Sd.")

temp<-c(as.vector(summary(ds.numeric$temp))[c(4,3,2,5)],sd(ds.numeric$temp))
atemp<-c(as.vector(summary(ds.numeric$atemp))[c(4,3,2,5)],sd(ds.numeric$atemp))
hum<-c(as.vector(summary(ds.numeric$hum))[c(4,3,2,5)],sd(ds.numeric$hum))
windspeed<-c(as.vector(summary(ds.numeric$windspeed))[c(4,3,2,5)],sd(ds.numeric$windspeed))

summaryTable<-matrix(c(temp,atemp,hum,windspeed), nrow=4, ncol=5, byrow=TRUE, dimnames=list(rowName, columnName))
summaryTable
#             Mean Median 1stQuartile 3rdQuartile        Sd.
# temp      0.4954 0.4983      0.3371      0.6554 0.18305100
# atemp     0.4744 0.4867      0.3378      0.6086 0.16296118
# hum       0.6279 0.6267      0.5200      0.7302 0.14242910
# windspeed 0.1905 0.1810      0.1349      0.2332 0.07749787

########################################################################################################################
# - b) For numerical variables, plot the density distribution. Describe whether the variable has a normal distribution or certain type of
#      skew distribution.
library('ggplot2') 

ggplot(ds.bike, aes(x = temp)) + geom_density()
ggplot(ds.bike, aes(x = atemp)) + geom_density()
ggplot(ds.bike, aes(x = temp, fill = as.factor(season))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = atemp, fill = as.factor(season))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = hum)) + geom_density(alpha = 0.5, fill = "red") 
ggplot(ds.bike, aes(x = windspeed)) + geom_density(alpha = 0.5, fill = "red")

qqnorm(ds.bike$temp)
qqline(ds.bike$temp)
qqnorm(ds.bike$atemp)
qqline(ds.bike$atemp)
qqnorm(ds.bike$hum)
qqline(ds.bike$hum)
qqnorm(ds.bike$windspeed)
qqline(ds.bike$windspeed)

ks.test(ds.bike$temp, "pnorm", mean = mean(ds.bike$temp), sd =  sqrt(var(ds.bike$temp)))
ks.test(ds.bike$atemp, "pnorm", mean = mean(ds.bike$atemp), sd =  sqrt(var(ds.bike$atemp)))
ks.test(ds.bike$hum, "pnorm", mean = mean(ds.bike$hum), sd =  sqrt(var(ds.bike$hum)))
ks.test(ds.bike$windspeed, "pnorm", mean = mean(ds.bike$windspeed), sd =  sqrt(var(ds.bike$windspeed)))
shapiro.test(ds.bike$temp)
shapiro.test(ds.bike$atemp)
shapiro.test(ds.bike$hum)
shapiro.test(ds.bike$windspeed)

# One-sample Kolmogorov-Smirnov test

# data:  ds.bike$temp
# D = 0.072672, p-value = 0.0008867
# alternative hypothesis: two-sided

# data:  ds.bike$atemp
# D = 0.067418, p-value = 0.002601
# alternative hypothesis: two-sided

# data:  ds.bike$hum
# D = 0.033861, p-value = 0.3717
# alternative hypothesis: two-sided

# data:  ds.bike$windspeed
# D = 0.0627, p-value = 0.006381
# alternative hypothesis: two-sided

########################################################################################################################
# - c) For each numerical predictor, describe its relationship with the response variable through correlation and scatterplot.

cor(ds.numeric$temp,ds.numeric$cnt)
ggplot(ds.bike, aes(x=ds.bike$temp, y=ds.bike$cnt)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# [1] 0.6393774

cor(ds.numeric$atemp,ds.numeric$cnt)
ggplot(ds.bike, aes(x=ds.bike$atemp, y=ds.bike$cnt)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# [1] 0.6446592

cor(ds.numeric$hum,ds.numeric$cnt)
ggplot(ds.bike, aes(x=ds.bike$hum, y=ds.bike$cnt)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# [1] -0.09851479

cor(ds.numeric$windspeed,ds.numeric$cnt)
ggplot(ds.bike, aes(x=ds.bike$windspeed, y=ds.bike$cnt)) + 
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
# [1] -0.2426165

########################################################################################################################
# - d) For each categorical predictor, generate their conditional density plot of response variable.
#      hint: Plot the density of response variable into multiple distributions seperated by the predictor’s categories, 
#      on the same figure. Use different colors or line shapes to differentiate categories. Make sure to use the proper 
#      variable “type” for the categorical variables before plotting.

ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(season))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(yr))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(mnth))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(holiday))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(weekday))) + geom_density(alpha = 0.5)
ggplot(ds.bike, aes(x = ds.bike$cnt, fill = as.factor(weathersit))) + geom_density(alpha = 0.5)

########################################################################################################################
# - e) (extra points) Compare and describe whether the response variable is significantly different on holidays or non-holidays.

shapiro.test(ds.bike$windspeed[ds.bike$holiday==0])

#   Shapiro-Wilk normality test

# data:  ds.bike$windspeed[ds.bike$holiday == 0]
# W = 0.96916, p-value = 4.447e-11

shapiro.test(ds.bike$windspeed[ds.bike$holiday==1])

# Shapiro-Wilk normality test

# data:  ds.bike$windspeed[ds.bike$holiday == 1]
# W = 0.96873, p-value = 0.7047

t.test(ds.bike$cnt~ds.bike$holiday)
#   Welch Two Sample t-test

# data:  ds.bike$cnt by ds.bike$holiday
# t = 1.7362, df = 20.893, p-value = 0.09725
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -1.374997 15.251567
# sample estimates:
#  mean in group 0 mean in group 1 
# 65.44041        58.50213 

########################################################################################################################
# 3. Apply regression analysis on bike data. Evaluate the model as well as the impact of different predictors.
########################################################################################################################
# - a) Use all predictors in a standard linear regression model to predict the response variable. Report the model 
#      performance using R^2, adjusted R^2 and RMSE. Interpret the regression result.
lm1 = lm(cnt ~ ., data = ds.bike)
summary(lm1)

# Call:
# lm(formula = cnt ~ ., data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -56.816  -3.333   0.906   4.546  20.599 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  41.4719     1.9669  21.084  < 2e-16 ***
# season        4.4050     0.4572   9.636  < 2e-16 ***
# yr           15.4152     0.5442  28.325  < 2e-16 ***
# mnth         -0.3420     0.1426  -2.398 0.016719 *  
# holiday      -5.4185     1.6264  -3.332 0.000908 ***
# weekday       0.4924     0.1361   3.619 0.000317 ***
# weathersit   -5.3309     0.6528  -8.166 1.43e-15 ***
# temp          9.8789    11.7179   0.843 0.399477    
# atemp        37.3619    13.2694   2.816 0.005001 ** 
# hum          -8.2629     2.6204  -3.153 0.001681 ** 
# windspeed   -21.1405     3.8084  -5.551 3.99e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.284 on 720 degrees of freedom
# Multiple R-squared:  0.7893,  Adjusted R-squared:  0.7863 
# F-statistic: 269.6 on 10 and 720 DF,  p-value: < 2.2e-16

rmse = sqrt(mean(residuals(lm1)^2))
rmse
# [1] 7.228763

n = length(ds.bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[-k]
  m2 = lm(cnt ~ ., data = ds.bike[train,])
  pred = predict(m2, newdata = ds.bike[-train,])
  obs = ds.bike$cnt[-train]
  error[k] = obs - pred
}
lm1_me = mean(error)
lm1_rmse = sqrt(mean(error^2))
lm1_me
# [1] 0.01347982
lm1_rmse
# [1] 7.474931


########################################################################################################################
# - b) Use different combination of predictors in standard linear and non-linear regression models to predict the 
#      response variable. (Here we don’t consider interaction terms.) Evaluate which model performs better using 
#      out-of-sample RMSE.
#      hint:: Use poly, locfit or other appropriate packages for non-linear regression models.
#      hint: Implement leave-one-out cross-validation for out-of-sample evaluation.

## backward stepwise selection
library(MASS)
stepAIC(lm1, direction = "backward")

# Start:  AIC=2913.94
# cnt ~ season + yr + mnth + holiday + weekday + weathersit + temp + 
#     atemp + hum + windspeed
# 
#              Df Sum of Sq   RSS    AIC
# - temp        1        38 38236 2912.7
# <none>                    38198 2913.9
# - mnth        1       305 38504 2917.8
# - atemp       1       421 38619 2919.9
# - hum         1       528 38726 2922.0
# - holiday     1       589 38787 2923.1
# - weekday     1       695 38893 2925.1
# - windspeed   1      1635 39833 2942.6
# - weathersit  1      3538 41736 2976.7
# - season      1      4926 43124 3000.6
# - yr          1     42565 80763 3459.3
# 
# Step:  AIC=2912.66
# cnt ~ season + yr + mnth + holiday + weekday + weathersit + atemp + 
#     hum + windspeed
# 
#              Df Sum of Sq   RSS    AIC
# <none>                    38236 2912.7
# - mnth        1       304 38540 2916.5
# - hum         1       546 38782 2921.0
# - holiday     1       579 38815 2921.6
# - weekday     1       714 38950 2924.2
# - windspeed   1      1597 39833 2940.6
# - weathersit  1      3517 41753 2975.0
# - season      1      4920 43156 2999.1
# - atemp       1     36936 75172 3404.8
# - yr          1     42588 80824 3457.8
# 
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     atemp + hum + windspeed, data = ds.bike)
# 
# Coefficients:
# (Intercept)       season           yr         mnth      holiday      weekday   weathersit        atemp          hum    windspeed  
#     41.0455       4.4022      15.4189      -0.3415      -5.3704       0.4985      -5.3117      48.4412      -8.3898     -20.6178  

lm2 = lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + atemp + hum + windspeed, data = ds.bike)
summary(lm2)

# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     atemp + hum + windspeed, data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -57.049  -3.368   0.913   4.501  20.470 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  41.0455     1.9004  21.598  < 2e-16 ***
# season        4.4022     0.4571   9.632  < 2e-16 ***
# yr           15.4189     0.5441  28.338  < 2e-16 ***
# mnth         -0.3415     0.1426  -2.395 0.016860 *  
# holiday      -5.3704     1.6251  -3.305 0.000998 ***
# weekday       0.4985     0.1359   3.670 0.000261 ***
# weathersit   -5.3117     0.6523  -8.143 1.69e-15 ***
# atemp        48.4412     1.8355  26.391  < 2e-16 ***
# hum          -8.3898     2.6155  -3.208 0.001397 ** 
# windspeed   -20.6178     3.7568  -5.488 5.63e-08 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.282 on 721 degrees of freedom
# Multiple R-squared:  0.789,  Adjusted R-squared:  0.7864 
# F-statistic: 299.6 on 9 and 721 DF,  p-value: < 2.2e-16

n = length(ds.bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[-k]
  m2 = lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + atemp + hum + windspeed, data = ds.bike[train,])
  pred = predict(m2, newdata = ds.bike[-train,])
  obs = ds.bike$cnt[-train]
  error[k] = obs - pred
}
lm2_me = mean(error)
lm2_rmse = sqrt(mean(error^2))
lm2_me
# [1] -0.01480142
lm2_rmse
# [1] 7.374793


lm3 = lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + temp + hum + windspeed, data = ds.bike)
summary(lm3)

# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     temp + hum + windspeed, data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -56.074  -3.268   0.888   4.514  21.088 
# 
# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  43.1069     1.8883  22.829  < 2e-16 ***
# season        4.4621     0.4589   9.723  < 2e-16 ***
# yr           15.4171     0.5468  28.193  < 2e-16 ***
# mnth         -0.3506     0.1432  -2.448 0.014619 *  
# holiday      -5.5969     1.6329  -3.427 0.000644 ***
# weekday       0.4731     0.1366   3.464 0.000563 ***
# weathersit   -5.4500     0.6546  -8.326 4.19e-16 ***
# temp         42.5551     1.6290  26.123  < 2e-16 ***
# hum          -7.6474     2.6238  -2.915 0.003671 ** 
# windspeed   -22.9461     3.7720  -6.083 1.91e-09 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.319 on 721 degrees of freedom
# Multiple R-squared:  0.7869,  Adjusted R-squared:  0.7843 
# F-statistic: 295.9 on 9 and 721 DF,  p-value: < 2.2e-16

anova(lm1,lm2)

# Analysis of Variance Table
# 
# Model 1: cnt ~ season + yr + mnth + holiday + weekday + weathersit + temp + 
#     atemp + hum + windspeed
# Model 2: cnt ~ season + yr + mnth + holiday + weekday + weathersit + atemp + 
#     hum + windspeed
#   Res.Df   RSS Df Sum of Sq      F Pr(>F)
# 1    720 38198                           
# 2    721 38236 -1   -37.707 0.7107 0.3995
#
# p-value > 0.05, the null hypothesis H0 is accepted. atemp is not needed.

library(car)
crPlots(lm2)

poly1<-lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=2) + hum + windspeed, data=ds.bike)
summary(poly1)
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     poly(atemp, degree = 2) + hum + windspeed, data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -57.168  -2.957   0.671   3.847  22.356 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               70.3673     1.6736  42.045  < 2e-16 ***
# season                     3.9604     0.4036   9.812  < 2e-16 ***
# yr                        14.7018     0.4817  30.523  < 2e-16 ***
# mnth                      -0.4381     0.1257  -3.485 0.000522 ***
# holiday                   -4.6102     1.4319  -3.220 0.001341 ** 
# weekday                    0.4939     0.1196   4.129 4.07e-05 ***
# weathersit                -5.2968     0.5744  -9.222  < 2e-16 ***
# poly(atemp, degree = 2)1 222.2157     7.1430  31.110  < 2e-16 ***
# poly(atemp, degree = 2)2 -98.2746     6.7833 -14.488  < 2e-16 ***
# hum                      -14.1433     2.3371  -6.052 2.30e-09 ***
# windspeed                -24.1234     3.3169  -7.273 9.20e-13 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.412 on 720 degrees of freedom
# Multiple R-squared:  0.8367,  Adjusted R-squared:  0.8344 
# F-statistic: 368.8 on 10 and 720 DF,  p-value: < 2.2e-16

poly2<-lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + atemp + poly(hum, degree=2) + windspeed, data=ds.bike)
summary(poly2)
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     atemp + poly(hum, degree = 2) + windspeed, data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -56.263  -3.239   0.852   4.342  20.237 
# 
# Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             34.4166     1.6571  20.769  < 2e-16 ***
# season                   4.5057     0.4517   9.976  < 2e-16 ***
# yr                      15.2107     0.5390  28.222  < 2e-16 ***
# mnth                    -0.3927     0.1411  -2.782 0.005537 ** 
# holiday                 -5.6565     1.6051  -3.524 0.000452 ***
# weekday                  0.4824     0.1341   3.596 0.000345 ***
# weathersit              -4.2588     0.6850  -6.218 8.55e-10 ***
# atemp                   48.0593     1.8135  26.501  < 2e-16 ***
# poly(hum, degree = 2)1 -40.3186    10.0926  -3.995 7.14e-05 ***
# poly(hum, degree = 2)2 -35.6302     7.9184  -4.500 7.93e-06 ***
# windspeed              -18.9910     3.7252  -5.098 4.39e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.187 on 720 degrees of freedom
# Multiple R-squared:  0.7948,	Adjusted R-squared:  0.792 
# F-statistic: 278.9 on 10 and 720 DF,  p-value: < 2.2e-16

poly3<-lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=2) + poly(hum, degree=2) + windspeed, data=ds.bike)
summary(poly3)
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     poly(atemp, degree = 2) + poly(hum, degree = 2) + windspeed, 
#     data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -56.066  -2.684   0.581   3.822  21.880 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                59.4879     1.2258  48.531  < 2e-16 ***
# season                      4.0774     0.3900  10.456  < 2e-16 ***
# yr                         14.3598     0.4673  30.728  < 2e-16 ***
# mnth                       -0.5170     0.1218  -4.244 2.49e-05 ***
# holiday                    -4.9641     1.3832  -3.589 0.000355 ***
# weekday                     0.4708     0.1155   4.075 5.12e-05 ***
# weathersit                 -3.8072     0.5907  -6.446 2.11e-10 ***
# poly(atemp, degree = 2)1  220.4322     6.9000  31.947  < 2e-16 ***
# poly(atemp, degree = 2)2 -104.8095     6.6091 -15.858  < 2e-16 ***
# poly(hum, degree = 2)1    -67.2565     8.8575  -7.593 9.72e-14 ***
# poly(hum, degree = 2)2    -50.3781     6.8835  -7.319 6.72e-13 ***
# windspeed                 -22.0563     3.2145  -6.862 1.47e-11 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.19 on 719 degrees of freedom
# Multiple R-squared:  0.848,	Adjusted R-squared:  0.8457 
# F-statistic: 364.6 on 11 and 719 DF,  p-value: < 2.2e-16

poly4<-lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=1) + windspeed, data=ds.bike)
summary(poly4)
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     poly(atemp, degree = 3) + poly(hum, degree = 1) + windspeed, 
#     data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -56.515  -2.445   0.524   3.410  22.585 
# 
# Coefficients:
#                          Estimate Std. Error t value Pr(>|t|)    
# (Intercept)               60.6567     1.1700  51.843  < 2e-16 ***
# season                     3.8970     0.3805  10.241  < 2e-16 ***
# yr                        14.7832     0.4541  32.555  < 2e-16 ***
# mnth                      -0.3173     0.1192  -2.663  0.00793 ** 
# holiday                   -4.3820     1.3500  -3.246  0.00122 ** 
# weekday                    0.5496     0.1129   4.867 1.39e-06 ***
# weathersit                -5.1280     0.5417  -9.466  < 2e-16 ***
# poly(atemp, degree = 3)1 220.9311     6.7345  32.806  < 2e-16 ***
# poly(atemp, degree = 3)2 -97.1400     6.3953 -15.189  < 2e-16 ***
# poly(atemp, degree = 3)3 -58.7650     6.1498  -9.556  < 2e-16 ***
# poly(hum, degree = 1)    -60.2832     8.4997  -7.092 3.16e-12 ***
# windspeed                -25.4303     3.1296  -8.126 1.94e-15 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.045 on 719 degrees of freedom
# Multiple R-squared:  0.8551,  Adjusted R-squared:  0.8528 
# F-statistic: 385.6 on 11 and 719 DF,  p-value: < 2.2e-16

poly5<-lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=2) + windspeed, data=ds.bike)
summary(poly5)
# Call:
# lm(formula = cnt ~ season + yr + mnth + holiday + weekday + weathersit + 
#     poly(atemp, degree = 3) + poly(hum, degree = 2) + windspeed, 
#     data = ds.bike)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -55.356  -2.621   0.468   3.333  22.096 
# 
# Coefficients:
#                           Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                58.5631     1.1498  50.931  < 2e-16 ***
# season                      4.0169     0.3647  11.013  < 2e-16 ***
# yr                         14.4303     0.4371  33.016  < 2e-16 ***
# mnth                       -0.3963     0.1145  -3.460 0.000572 ***
# holiday                    -4.7439     1.2936  -3.667 0.000263 ***
# weekday                     0.5269     0.1082   4.871 1.37e-06 ***
# weathersit                 -3.5783     0.5528  -6.473 1.78e-10 ***
# poly(atemp, degree = 3)1  219.0502     6.4539  33.941  < 2e-16 ***
# poly(atemp, degree = 3)2 -103.8941     6.1811 -16.808  < 2e-16 ***
# poly(atemp, degree = 3)3  -60.1440     5.8922 -10.207  < 2e-16 ***
# poly(hum, degree = 2)1    -73.7332     8.3073  -8.876  < 2e-16 ***
# poly(hum, degree = 2)2    -52.2734     6.4398  -8.117 2.07e-15 ***
# windspeed                 -23.3161     3.0085  -7.750 3.15e-14 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 5.789 on 718 degrees of freedom
# Multiple R-squared:  0.8672,  Adjusted R-squared:  0.865 
# F-statistic: 390.9 on 12 and 718 DF,  p-value: < 2.2e-16

n = length(ds.bike$cnt)
error = dim(n)
for (k in 1:n) {
  train1 = c(1:n)
  train = train1[-k]
  m = lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=1) + windspeed, data = ds.bike[train,])
  pred = predict(m2, newdata = ds.bike[-train,])
  obs = ds.bike$cnt[-train]
  error[k] = obs - pred
}
lm_me = mean(error)
lm_rmse = sqrt(mean(error^2))
lm_me
lm_rmse


# poly5: cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=2) + windspeed
# Adjusted R-squared:  0.865 
# me: [1] -0.00681766
# rsme: [1] 5.883244

# poly4: cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=1) + windspeed
# Adjusted R-squared:  0.8528 
# me: [1] 0.006874024
# rsme: [1] 5.737376

# poly3: cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=2) + poly(hum, degree=2) + windspeed
# Adjusted R-squared:  0.8457 
# me: [1] 0.006874024
# rsme: [1] 5.737376

########################################################################################################################
# - c) From the best model, identify the most important predictor in the model, and explain how you determine the 
#      importance of the predictors.

fit1 <- lm(cnt ~ season + yr + mnth + holiday + weekday + weathersit + poly(atemp, degree=3) + poly(hum, degree=2) + windspeed, data=ds.bike)
fit2 <- lm(cnt ~ 1, data=ds.bike)
stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2))

# Start:  AIC=4032.17
# cnt ~ 1
# 
#                           Df Sum of Sq    RSS    AIC
# + poly(atemp, degree = 3)  3     87862  93389 3553.4
# + yr                       1     50913 130337 3793.1
# + season                   1     32513 148737 3889.6
# + weathersit               1     17489 163762 3960.0
# + mnth                     1     15455 165796 3969.0
# + poly(hum, degree = 2)    2     15803 165447 3969.5
# + windspeed                1     10669 170582 3989.8
# + holiday                  1       982 180269 4030.2
# + weekday                  1       608 180643 4031.7
# <none>                                 181251 4032.2
# 
# Step:  AIC=3553.43
# cnt ~ poly(atemp, degree = 3)
# 
#                         Df Sum of Sq   RSS    AIC
# + yr                     1     43090 50298 3103.1
# + poly(hum, degree = 2)  2     20740 72649 3373.9
# + weathersit             1     13696 79693 3439.5
# + season                 1      5635 87753 3509.9
# + windspeed              1      2919 90470 3532.2
# + mnth                   1      2270 91119 3537.4
# + weekday                1       865 92523 3548.6
# + holiday                1       330 93059 3552.8
# <none>                               93389 3553.4
# 
# Step:  AIC=3103.09
# cnt ~ poly(atemp, degree = 3) + yr
# 
#                         Df Sum of Sq   RSS    AIC
# + poly(hum, degree = 2)  2   12744.3 37554 2893.5
# + weathersit             1   11225.6 39073 2920.5
# + season                 1    6898.2 43400 2997.3
# + mnth                   1    2970.9 47327 3060.6
# + windspeed              1    2894.8 47404 3061.8
# + weekday                1     944.5 49354 3091.2
# + holiday                1     421.4 49877 3098.9
# <none>                               50298 3103.1
# 
# Step:  AIC=2893.5
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2)
# 
#              Df Sum of Sq   RSS    AIC
# + season      1    7840.3 29714 2724.3
# + windspeed   1    4050.1 33504 2812.1
# + mnth        1    3572.7 33981 2822.4
# + weathersit  1    2022.8 35531 2855.0
# + weekday     1     726.9 36827 2881.2
# + holiday     1     593.2 36961 2883.9
# <none>                    37554 2893.5
# 
# Step:  AIC=2724.32
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season
# 
#              Df Sum of Sq   RSS    AIC
# + windspeed   1   2658.60 27055 2657.8
# + weathersit  1   1800.79 27913 2680.6
# + weekday     1    725.45 28988 2708.2
# + holiday     1    608.44 29105 2711.2
# + mnth        1    392.03 29322 2716.6
# <none>                    29714 2724.3
# 
# Step:  AIC=2657.8
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed
# 
#              Df Sum of Sq   RSS    AIC
# + weathersit  1   1194.48 25861 2626.8
# + weekday     1    734.01 26321 2639.7
# + holiday     1    586.04 26469 2643.8
# + mnth        1    409.04 26646 2648.7
# <none>                    27055 2657.8
# 
# Step:  AIC=2626.79
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed + weathersit
# 
#           Df Sum of Sq   RSS    AIC
# + weekday  1    905.40 24955 2602.7
# + holiday  1    632.21 25228 2610.7
# + mnth     1    415.74 25445 2616.9
# <none>                 25861 2626.8
# 
# Step:  AIC=2602.74
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed + weathersit + weekday
# 
#           Df Sum of Sq   RSS    AIC
# + holiday  1    492.60 24463 2590.2
# + mnth     1    443.21 24512 2591.6
# <none>                 24955 2602.7
# 
# Step:  AIC=2590.17
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed + weathersit + weekday + holiday
# 
#        Df Sum of Sq   RSS    AIC
# + mnth  1    401.25 24061 2580.1
# <none>              24463 2590.2
# 
# Step:  AIC=2580.08
# cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed + weathersit + weekday + holiday + mnth
# 
# 
# Call:
# lm(formula = cnt ~ poly(atemp, degree = 3) + yr + poly(hum, degree = 2) + 
#     season + windspeed + weathersit + weekday + holiday + mnth, 
#     data = ds.bike)
# 
# Coefficients:
#              (Intercept)  poly(atemp, degree = 3)1  poly(atemp, degree = 3)2  poly(atemp, degree = 3)3                        yr  
#                  58.5631                  219.0502                 -103.8941                  -60.1440                   14.4303  
#   poly(hum, degree = 2)1    poly(hum, degree = 2)2                    season                 windspeed                weathersit  
#                 -73.7332                  -52.2734                    4.0169                  -23.3161                   -3.5783  
#                  weekday                   holiday                      mnth  
#                   0.5269                   -4.7439                   -0.3963  
# 
