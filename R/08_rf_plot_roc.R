#' ROC Curve for Random Forest Model with AUC per Class
#'
#' This function computes and plots the ROC curve for a fitted randomForest
#' model using test data (testX, testY). It supports binary and multiclass
#' classification and displays accuracy on the test set. For multiclass
#' models, one-vs-rest ROC curves are computed and AUC values are shown
#' directly in the legend.
#'
#' @param model_rf A fitted randomForest model.
#' @param testX Test predictors (data.frame or matrix).
#' @param testY True test labels (factor).
#' @param curve_color Color for binary ROC curve. Ignored for multiclass.
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A ggplot ROC curve object.
#'
#' @examples
#' \dontrun{
#'   # Example using iris dataset
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'
#'   # Train a RF model with importance enabled
#'   out <- rf_model_with_oob(
#'     X = X,
#'     Y = Y,
#'     mtry = 2,
#'     ntree = 150,
#'     split = 0.65
#'   )
#'
#'   # Generate ROC curve on test data
#'   roc_plot <- rf_plot_roc(
#'     model_rf = out$model_rf,
#'     testX = out$testX,
#'     testY = out$testY,
#'     curve_color = "darkred"
#'   )
#'
#'   # Display ROC curve
#'   roc_plot
#'
#'   # If multiclass, AUC values appear in the legend:
#'   # e.g., "setosa (AUC = 0.98)"
#' }
#'
#' @export
rf_plot_roc <- function(model_rf, testX, testY,
                        curve_color = "blue",
                        base_size = 16) {

  if (!requireNamespace("pROC", quietly = TRUE))
    stop("Package 'pROC' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")

  testY <- as.factor(testY)

  pred_prob <- predict(model_rf, testX, type = "prob")
  pred_class <- predict(model_rf, testX, type = "class")

  accuracy <- mean(pred_class == testY)

  # -------------------------
  # BINARY CLASSIFICATION
  # -------------------------
  if (nlevels(testY) == 2) {

    positive_class <- levels(testY)[2]

    roc_obj <- pROC::roc(
      response = testY,
      predictor = pred_prob[, positive_class],
      levels = levels(testY),
      direction = "<"
    )

    auc_value <- pROC::auc(roc_obj)

    df_roc <- data.frame(
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities
    )

    p <- ggplot2::ggplot(df_roc, ggplot2::aes(x = fpr, y = tpr)) +
      ggplot2::geom_line(color = curve_color, size = 1.2) +
      ggplot2::geom_abline(linetype = "dashed", color = "gray50") +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::labs(
        title = "ROC Curve (Binary Classification)",
        subtitle = paste0(
          "AUC = ", round(auc_value, 3),
          " | Accuracy = ", round(accuracy, 3)
        ),
        x = "False Positive Rate",
        y = "True Positive Rate"
      )

    return(p)
  }

  # -------------------------
  # MULTICLASS CLASSIFICATION
  # -------------------------

  roc_list <- list()
  auc_list <- c()

  for (cls in levels(testY)) {

    # Convert to binary one-vs-rest
    binary_response <- factor(
      ifelse(testY == cls, cls, paste0("not_", cls)),
      levels = c(paste0("not_", cls), cls)
    )

    roc_obj <- pROC::roc(
      response = binary_response,
      predictor = pred_prob[, cls],
      direction = "<"
    )

    roc_list[[cls]] <- roc_obj
    auc_list[cls] <- pROC::auc(roc_obj)
  }

  # Build combined ROC dataframe
  df_multi <- do.call(rbind, lapply(names(roc_list), function(cls) {
    roc_obj <- roc_list[[cls]]
    data.frame(
      class = cls,
      fpr = 1 - roc_obj$specificities,
      tpr = roc_obj$sensitivities
    )
  }))

  # Legend labels with AUC
  legend_labels <- paste0(names(auc_list), " (AUC = ", round(auc_list, 3), ")")

  p <- ggplot2::ggplot(df_multi, ggplot2::aes(
    x = fpr, y = tpr, color = class
  )) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::geom_abline(linetype = "dashed", color = "gray50") +
    ggplot2::scale_color_discrete(labels = legend_labels) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "ROC Curve (Multiclass One-vs-Rest)",
      subtitle = paste0("Accuracy = ", round(accuracy, 3)),
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Class"
    )

  return(p)
}
