#' Surrogate Decision Tree for Random Forest Predictions
#'
#' Builds a surrogate CART tree that mimics the predictions of a fitted
#' Random Forest model. Useful for interpretability: the surrogate tree
#' approximates the RF decision boundaries in a human-readable form.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors used to fit the model.
#' @param Y_train Training response used to fit the model.
#' @param maxdepth Maximum depth of the surrogate tree. Default = 4.
#'
#' @return A list with:
#' \describe{
#'   \item{tree}{The fitted rpart surrogate tree.}
#'   \item{plot}{A rpart.plot visualization of the surrogate tree.}
#' }
#'
#' @examples
#' \dontrun{
#'   out <- rf_model_with_oob(
#'     X = iris[,1:4],
#'     Y = iris$Species,
#'     mtry = 2,
#'     ntree = 150,
#'     split = 0.65
#'   )
#'
#'   surrogate <- rf_surrogate_tree(
#'     model_rf = out$model_rf,
#'     X_train = out$trainX,
#'     Y_train = out$trainY
#'   )
#'
#'   surrogate$plot
#' }
#'
#' @export
rf_surrogate_tree <- function(model_rf,
                              X_train,
                              Y_train,
                              maxdepth = 4) {

  if (!requireNamespace("rpart", quietly = TRUE)) stop("rpart required")
  if (!requireNamespace("rpart.plot", quietly = TRUE)) stop("rpart.plot required")

  # --- Build surrogate dataset ------------------------------------------------
  df <- data.frame(X_train)
  df$RF_pred <- predict(model_rf, X_train, type = "class")

  # --- Fit surrogate CART tree (model=TRUE avoids rpart.plot warnings) -------
  tree <- rpart::rpart(
    formula = RF_pred ~ .,
    data = df,
    method = "class",
    control = rpart::rpart.control(maxdepth = maxdepth),
    model = TRUE   # <-- CRITICAL: keeps model.frame for rpart.plot
  )

  # --- Plot surrogate tree ----------------------------------------------------
  p <- rpart.plot::rpart.plot(
    tree,
    roundint = FALSE,   # <-- avoids warnings if integers are coerced
    extra = 104,
    box.palette = "RdYlGn",
    shadow.col = "gray",
    nn = TRUE
  )

  list(
    tree = tree,
    plot = p
  )
}
