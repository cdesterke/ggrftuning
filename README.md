# ggrftuning
Tuning rf with ggplot2 outputs and XAI diagnostic


## load library and perform data preprocess 
```r
#library(randomForest)
data(iris)
## no missing data
iris<-iris[complete.cases(iris),]
## last column of the data need to be a factor
str(iris)
```


## Step 01: Tune the mtry Parameter of a Random Forest Model

This function performs a simple grid search over all possible \code{mtry}
values for a Random Forest classifier, using out-of-bag (OOB) error as the
evaluation metric. It accepts predictors {X} and response {Y}
as separate inputs, fits models for each {mtry}, and returns both
the tuning results and a ggplot2 visualization with ggrepel labels.

parameter: X A data.frame or matrix of predictor variables.

parameter: Y A vector or factor containing the response variable. Must have
 the same number of rows as {X}.

parameter: ntree Integer. Number of trees to grow. Default is 500.

parameter: base_size Numeric. Base font size for the ggplot theme. Default is 16.

return a list with two elements:
 
 item{results} A data.frame containing {mtry} and corresponding OOB error.
 
 item{plot} A ggplot2 object visualizing OOB error vs. {mtry}.
 
```r
## load script 01
source("01_tune_mtry_rf.R")
## split X & Y as data and outcome
X<- iris[, 1:4]
Y <- iris$Species
## run
out <- tune_mtry_rf(X, Y,ntree=500)
## output
out$results
out$plot
## save
write.csv(out$results,file="01_mtry.csv",row.names=T)
```
![res](https://github.com/cdesterke/ggrftuning/blob/main/01_mtry.png)


## Step 02: Tune the mtree Parameter of a Random Forest Model with a selected mtry

This function performs a grid search over a user-defined set of {ntree}
values for a Random Forest classifier, using out-of-bag (OOB) error as the
evaluation metric. It accepts predictors {X} and response \code{Y}
as separate inputs, fits models for each {ntree}, and returns both
the tuning results and a ggplot2 visualization with ggrepel labels.

parameter: X A data.frame or matrix of predictor variables.

parameter: Y A vector or factor containing the response variable. Must have
the same number of rows as {X}.

parameter: mtry Integer. The number of variables randomly sampled at each split. Must be between 1 and {ncol(X)}.

parameter: ntree_grid A numeric vector of ntree values to evaluate. Default is {seq(100, 1000, by = 50)}.

parameter base_size Numeric. Base font size for the ggplot theme. Default is 16.

return A list with two elements:

item {results} A data.frame containing {ntree} and corresponding OOB error.

item {plot} A ggplot2 object visualizing OOB error vs. {ntree}.
 
```r
## load script 02
source("02_tune_ntree_rf.R")
## 
out <- tune_ntree_rf(X, Y, mtry = 2)
out$results
out$plot
## save
write.csv(out$results,file="02_ntree.csv",row.names=T)
```
![res](https://github.com/cdesterke/ggrftuning/blob/main/02_ntree.png)


