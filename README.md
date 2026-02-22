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
## run
out <- tune_ntree_rf(X, Y, mtry = 2)
## output
out$results
out$plot
## save
write.csv(out$results,file="02_ntree.csv",row.names=T)
```
![res](https://github.com/cdesterke/ggrftuning/blob/main/02_ntree.png)



## Step 03: Tune best partition train/test

This function evaluates the effect of different train/test split proportions
on the performance of a Random Forest classifier using the {caret}
interface to the \code{ranger} engine. The user provides predictors {X},
response {Y}, and fixed values for {mtry} and {ntree}.
The function returns accuracy for each split proportion and a ggplot2
visualization with ggrepel labels.

parameter: X A data.frame or matrix of predictor variables.

parameter: Y A factor containing the response variable. Must have the same
number of rows as {X}.

parameter: mtry Integer. Number of variables randomly sampled at each split.

parameter: ntree Integer. Number of trees to grow.

parameter: split_seq Numeric vector of train proportions to evaluate.
Default is \code{c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80)}.

parameter: base_size Numeric. Base font size for the ggplot theme. Default is 16.

return A list with:

item {results} A data.frame with split proportion and accuracy.

item {plot} A ggplot2 object visualizing accuracy vs. split proportion.

```r
source("03_tune_split_ratio_caret.R")
## run
out <- tune_split_ratio_caret(X, Y, mtry = 2, ntree = 500)
## output
out$results
out$plot
## save
write.csv(out$results,file="03_bestsplit.csv",row.names=T)
```
![res](https://github.com/cdesterke/ggrftuning/blob/main/03_bestsplit.png)

## Step 04: run rf model with selected parameters

This function trains a Random Forest classifier using a user-defined
train/test split,{mtry}, and {ntree}. It returns a summary of
model performance, variable importances, and a ggplot2 visualization showing
the evolution of out-of-bag (OOB) error across trees, both globally and for
each class. The global OOB curve is displayed in black dashed style, while
class-specific OOB curves use the standard ggplot2 color palette.

parameter: X A data.frame or matrix of predictor variables.

parameter: Y A factor containing the response variable. Must have the same
number of rows as {X}.

parameter: mtry Integer. Number of variables randomly sampled at each split.

parameter: ntree Integer. Number of trees to grow.

parameter: split Numeric. Proportion of data used for training (between 0 and 1).

parameter: base_size Numeric. Base font size for the ggplot theme. Default is 16.

return A list with:

item {results} A list containing OOB error, test accuracy, and confusion matrix.

item {plot} A ggplot2 object showing OOB error vs. number of trees for each class.

item {importance_df} A data.frame of variable importances.

item {model_rf} The fitted randomForest model.

item{trainX} Training predictors.

item: {trainY} Training response.

item: {testX}Test predictors.

item: {testY}Test response.

```r
source("04_rf_model_with_oob.R")
##run
out <- rf_model_with_oob(X = X, Y = Y, mtry = 2, ntree = 150, split = 0.65)
##
out$results
out$plot
## model access
out$model_rf
## split access
out$tainX
out$trainY
out$testX
out$testY
##save model
save(out,file="model_rf.rda")

```
![res](https://github.com/cdesterke/ggrftuning/blob/main/04_model.png)



