#' Predict on Test Data with Full Metrics, OOB Metrics, and Faceted Cleveland Plot
#'
#' This function computes predictions, confusion matrix, accuracy, and
#' class-specific metrics (sensitivity, specificity, PPV, NPV, F1 score,
#' balanced accuracy, and Cohen's Kappa) on the test set. It also extracts
#' OOB metrics from the randomForest model and generates a faceted Cleveland
#' plot (dot + segment) comparing test metrics for each class, with values
#' displayed at the end of each bar.
#'
#' @param model_rf A fitted randomForest model.
#' @param testX A data.frame or matrix of predictors for testing.
#' @param testY A factor containing the true test labels.
#' @param base_size Numeric. Base font size for ggplot themes. Default = 16.
#'
#' @return A list with:
#' \describe{
#'   \item{predictions}{Predicted class labels on the test set.}
#'   \item{confusion_matrix_test}{Confusion matrix on the test set.}
#'   \item{metrics_test}{Data.frame of test metrics per class.}
#'   \item{confusion_matrix_oob}{OOB confusion matrix from the model.}
#'   \item{metrics_oob}{Data.frame of OOB metrics per class.}
#'   \item{kappa_test}{Cohen's Kappa on the test set.}
#'   \item{kappa_oob}{Cohen's Kappa based on the OOB confusion matrix.}
#'   \item{plot_metrics}{Faceted Cleveland plot of test metrics.}
#' }
#'
#' @examples
#' \dontrun{
#'   out <- rf_model_with_oob(iris[,1:4], iris$Species, mtry = 2, ntree = 150)
#'   pred <- rf_predict_metrics(out$model_rf, out$testX, out$testY)
#'   pred$plot_metrics
#' }
#'
#' @export
rf_predict_metrics <- function(model_rf, testX, testY, base_size = 16) {

  if (!requireNamespace("caret", quietly = TRUE)) stop("Package 'caret' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")

  if (!is.data.frame(testX)) testX <- as.data.frame(testX)
  if (!is.factor(testY)) stop("testY must be a factor.")
  if (nrow(testX) != length(testY)) stop("testX and testY must have the same number of observations.")

  #-------------------------
  # TEST PREDICTIONS & CM
  #-------------------------
  preds <- predict(model_rf, testX)
  cm_test <- table(True = testY, Predicted = preds)

  # Cohen's Kappa (test)
  kappa_test <- caret::confusionMatrix(cm_test)$overall["Kappa"]

  classes <- levels(testY)

  # Helper to compute metrics from a confusion matrix (cm) for one class
  compute_metrics_for_class <- function(cm, cl) {
    TP <- cm[cl, cl]
    FN <- sum(cm[cl, ]) - TP
    FP <- sum(cm[, cl]) - TP
    TN <- sum(cm) - TP - FN - FP

    sensitivity <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
    specificity <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
    ppv <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
    npv <- if ((TN + FN) > 0) TN / (TN + FN) else NA_real_
    f1 <- if ((2 * TP + FP + FN) > 0) 2 * TP / (2 * TP + FP + FN) else NA_real_
    bal_acc <- mean(c(sensitivity, specificity), na.rm = TRUE)

    data.frame(
      Class = cl,
      Sensitivity = sensitivity,
      Specificity = specificity,
      PPV = ppv,
      NPV = npv,
      F1 = f1,
      BalancedAccuracy = bal_acc
    )
  }

  #-------------------------
  # TEST METRICS
  #-------------------------
  metrics_test <- do.call(
    rbind,
    lapply(classes, function(cl) compute_metrics_for_class(cm_test, cl))
  )

  #-------------------------
  # OOB CONFUSION & METRICS
  #-------------------------
  cm_oob_full <- model_rf$confusion
  cm_oob <- cm_oob_full[, -ncol(cm_oob_full), drop = FALSE]

  kappa_oob <- caret::confusionMatrix(cm_oob)$overall["Kappa"]

  metrics_oob <- do.call(
    rbind,
    lapply(classes, function(cl) compute_metrics_for_class(cm_oob, cl))
  )

  #-------------------------
  # FACETED CLEVELAND PLOT (TEST METRICS)
  #-------------------------
  metrics_long <- tidyr::pivot_longer(
    metrics_test,
    cols = -Class,
    names_to = "Metric",
    values_to = "Value"
  )

  plot_metrics <- ggplot2::ggplot(metrics_long,
                                  ggplot2::aes(x = Metric, y = Value, color = Metric)) +
    ggplot2::geom_segment(
      ggplot2::aes(x = Metric, xend = Metric, y = 0, yend = Value, color = Metric),
      linewidth = 1
    ) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(Value, 3), y = Value + 0.1),
      size = 4.5,
      show.legend = FALSE
    ) +
    ggplot2::coord_flip() +
    ggplot2::facet_wrap(~ Class, ncol = 1, scales = "free_y") +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "Class-Specific Performance Metrics (Test Set)",
      subtitle = paste0("Cohen's Kappa (test) = ", round(kappa_test, 4)),
      x = "",
      y = "Value",
      color = "Metric"
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1.2)) +
    ggplot2::scale_color_brewer(palette = "Dark2") 
  #-------------------------
  # OUTPUT
  #-------------------------
  list(
    predictions = preds,
    confusion_matrix_test = cm_test,
    metrics_test = metrics_test,
    confusion_matrix_oob = cm_oob,
    metrics_oob = metrics_oob,
    kappa_test = kappa_test,
    kappa_oob = kappa_oob,
    plot_metrics = plot_metrics
  )
}
