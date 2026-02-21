library(randomForest)
iris%>%filter(Species!="setosa")->iris
iris$Species<-as.character(iris$Species)
 iris$Species<-as.factor(iris$Species)
data(iris)

X<- iris[, 1:4]
  Y <- iris$Species

out <- rf_model_with_oob(X = X,Y=Y,mtry=2,ntree=500,split=0.5)
out$plot


out_imp <- rf_importance_plots(
  model_rf = out$model_rf,
  palette = "mako"
)


roc_plot <- rf_plot_roc(
   model_rf = out$model_rf,
   testX = out$testX,
   testY = out$testY,
    curve_color = "orchid"
   )

 roc_plot


  expl <- rf_explain_trees(
    model_rf = out$model_rf,
     X_train = out$trainX,
     Y_train = out$trainY
   )
 expl$plot

shap <- rf_shap_summary(
    model_rf = out$model_rf,
     X_train = out$trainX,
    palette = "magma"
   )
  shap$plots[[2]]





 pr_plot <- rf_plot_pr(
   model_rf = out$model_rf,
   testX = out$testX,
    testY = out$testY,
     curve_color = "darkorange"
   )

  pr_plot



plot_ice_rf(out$model_rf, out$testX, "Petal.Width")


plot_interaction_heatmap_rf(out$model_rf, out$testX,palette="magma")


pfi <- rf_pfi(
	model_rf = out$model_rf,
  X_test = out$testX,
  y_test = out$testY,
  palette = "inferno"
 )
pfi$plot



 tree <- rf_surrogate_tree(
   model_rf = out$model_rf,
  X_train = out$trainX,
   maxdepth = 3
 )

 rpart.plot::rpart.plot(tree)


 rf_ale(
  model_rf = out$model_rf,
   X_train = out$trainX,
  feature = "Sepal.Width",
 palette = "viridis"
 )


rf_ale2d(
 model_rf = out$model_rf,
  X_train = out$trainX,
  features = c("Sepal.Width", "Petal.Length"),
  palette = "inferno"
)

