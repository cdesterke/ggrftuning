#' Predict on Test Data and Plot Confusion Matrix Heatmap
#'
#' This function takes a fitted Random Forest model along with test predictors
#' and test response, and returns predictions, accuracy, a confusion matrix,
#' and a ggplot2 heatmap of the confusion matrix with accuracy in the subtitle.
#'
#' @param model_rf A fitted randomForest model.
#' @param testX A data.frame or matrix of predictors for testing.
#' @param testY A factor containing the true test labels.
#' @param base_size Numeric. Base font size for the ggplot theme. Default = 16.
#'
#' @return A list with:
#' \describe{
#'   \item{predictions}{Predicted class labels.}
#'   \item{accuracy}{Classification accuracy on the test set.}
#'   \item{confusion_matrix}{A confusion matrix comparing predictions vs truth.}
#'   \item{plot}{A ggplot2 heatmap of the confusion matrix.}
#' }
#'
#' @examples
#' \dontrun{
#'   out <- rf_model_with_oob(iris[,1:4], iris$Species, mtry = 2, ntree = 50)
#'   pred_out <- rf_predict_test_heatmap(out$model_rf, out$testX, out$testY)
#'   pred_out$plot
#' }
#'
#' @export
rf_predict_test_heatmap <- function(model_rf, testX, testY, base_size = 16) {

  # Checks
  if (missing(model_rf)) stop("model_rf is required.")
  if (missing(testX)) stop("testX is required.")
  if (missing(testY)) stop("testY is required.")
  if (!is.factor(testY)) stop("testY must be a factor.")
  if (!is.data.frame(testX)) testX <- as.data.frame(testX)
  if (nrow(testX) != length(testY)) stop("testX and testY must have the same number of observations.")

  # Predictions
  preds <- predict(model_rf, testX)

  # Accuracy
  acc <- mean(preds == testY)

  # Confusion matrix
  cm <- table(Predicted = preds, True = testY)

  # Convert to tidy format for ggplot
  cm_df <- as.data.frame(cm)
  colnames(cm_df) <- c("Predicted", "True", "Freq")

  # Heatmap plot
  p <- ggplot2::ggplot(cm_df, ggplot2::aes(x = True, y = Predicted, fill = Freq)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = Freq), size = 5) +
    ggplot2::scale_fill_gradient(low = "#e0ecf4", high = "#8856a7") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "Confusion Matrix (Heatmap)",
      subtitle = paste0("Accuracy = ", round(acc, 4)),
      x = "True Class",
      y = "Predicted Class",
      fill = "Count"
    )

  list(
    predictions = preds,
    accuracy = acc,
    confusion_matrix = cm,
    plot = p
  )
}
