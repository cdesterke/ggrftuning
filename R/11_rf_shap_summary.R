#' SHAP Summary Plot for Random Forest (Binary & Multiclass)
#'
#' Computes SHAP values using fastshap for a fitted randomForest model
#' and produces a single global SHAP summary plot. For multiclass models,
#' SHAP is computed for the predicted class of each sample (row-wise).
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors used to fit the model.
#' @param target_class Optional. Class for which SHAP values are computed.
#'        If NULL (default), uses the predicted class (multiclass) or
#'        the positive class (binary).
#' @param palette Viridis palette name. Default = "viridis".
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A list with:
#'   \item{shap_values}{Data frame of SHAP values.}
#'   \item{plot}{A ggplot2 SHAP summary plot.}
#'
#' @examples
#' \dontrun{
#'   library(randomForest)
#'   library(fastshap)
#'
#'   # Binary example
#'   data(iris)
#'   iris_bin <- subset(iris, Species != "setosa")
#'   iris_bin$Species <- factor(iris_bin$Species)
#'   rf_bin <- randomForest(Species ~ ., data = iris_bin)
#'   res_bin <- rf_shap_summary(rf_bin, iris_bin[, -5])
#'   res_bin$plot
#'
#'   # Multiclass example
#'   rf_multi <- randomForest(Species ~ ., data = iris)
#'   res_multi <- rf_shap_summary(rf_multi, iris[, -5])
#'   res_multi$plot
#' }
#'
rf_shap_summary <- function(model_rf,
                            X_train,
                            target_class = NULL,
                            palette = "viridis",
                            base_size = 16) {

  classes <- model_rf$classes
  multiclass <- length(classes) > 2

  # Determine target class
  if (is.null(target_class)) {
    if (multiclass) {
      target_class <- predict(model_rf, X_train, type = "response")
    } else {
      target_class <- classes[2]  # positive class
    }
  }

  # Prediction wrapper for fastshap
  f_pred <- function(object, newdata) {
    prob <- predict(object, newdata, type = "prob")

    if (multiclass) {
      if (length(target_class) == nrow(newdata)) {
        return(prob[cbind(seq_len(nrow(prob)),
                          match(target_class, classes))])
      } else {
        return(prob[, target_class])
      }
    } else {
      return(prob[, target_class])
    }
  }

  # Compute SHAP values
  shap_values <- fastshap::explain(
    object = model_rf,
    X = X_train,
    pred_wrapper = f_pred,
    nsim = 100
  )

  # 🔥 Remove ALL classes (explain/matrix/array)
  shap_values <- as.data.frame(unclass(shap_values))

  # Long format for plotting
  shap_long <- shap_values |>
    tibble::as_tibble() |>
    dplyr::mutate(sample = dplyr::row_number()) |>
    tidyr::pivot_longer(-sample, names_to = "feature", values_to = "shap")

  # SHAP summary plot (alpha = 1)
  p <- ggplot2::ggplot(shap_long,
                       ggplot2::aes(x = shap, y = feature, color = shap)) +
    ggplot2::geom_point(alpha = 1) +
    ggplot2::scale_color_viridis_c(option = palette) +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::labs(
      title = "SHAP Summary Plot",
      x = "SHAP value",
      y = "Feature"
    )

  return(list(
    shap_values = shap_values,
    plot = p
  ))
}

