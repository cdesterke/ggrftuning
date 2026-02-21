#' Surrogate Decision Tree for Random Forest
#'
#' Fits a shallow decision tree to mimic the Random Forest predictions.
#' Useful for global interpretability.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors.
#' @param maxdepth Maximum depth of the surrogate tree. Default = 3.
#'
#' @return A fitted rpart surrogate tree.
#'
#' @examples
#' \dontrun{
#' tree <- rf_surrogate_tree(
#'   model_rf = out$model_rf,
#'   X_train = out$trainX,
#'   maxdepth = 3
#' )
#'
#' rpart.plot::rpart.plot(tree)
#' }
#'
#' @export
rf_surrogate_tree <- function(model_rf, X_train, maxdepth = 3) {

  if (!requireNamespace("rpart", quietly = TRUE))
    stop("Package 'rpart' is required.")

  X_train <- as.data.frame(X_train)
  y_pred <- predict(model_rf, X_train, type="prob")[,2]

  df <- cbind(X_train, y_pred = y_pred)

  rpart::rpart(
    formula = y_pred ~ .,
    data = df,
    method = "anova",
    control = rpart::rpart.control(maxdepth = maxdepth)
  )
}
