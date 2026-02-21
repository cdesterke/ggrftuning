#' Precision-Recall Curve for Random Forest Model with AUPRC per Class
#'
#' This function computes and plots the Precision-Recall (PR) curve for a
#' fitted randomForest model using test data (testX, testY). It supports
#' binary and multiclass classification. For multiclass models, one-vs-rest
#' PR curves are computed and AUPRC values are shown directly in the legend.
#'
#' @param model_rf A fitted randomForest model.
#' @param testX Test predictors (data.frame or matrix).
#' @param testY True test labels (factor).
#' @param curve_color Color for binary PR curve. Ignored for multiclass.
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A ggplot PR curve object.
#'
#' @examples
#' \dontrun{
#'   # Example using iris dataset
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'
#'   out <- rf_model_with_oob(
#'     X = X,
#'     Y = Y,
#'     mtry = 2,
#'     ntree = 150,
#'     split = 0.65
#'   )
#'
#'   pr_plot <- rf_plot_pr(
#'     model_rf = out$model_rf,
#'     testX = out$testX,
#'     testY = out$testY,
#'     curve_color = "darkorange"
#'   )
#'
#'   pr_plot
#' }
#'
#' @export
rf_plot_pr <- function(model_rf, testX, testY,
                       curve_color = "blue",
                       base_size = 16) {

  if (!requireNamespace("PRROC", quietly = TRUE))
    stop("Package 'PRROC' is required.")
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

    scores_pos <- pred_prob[testY == positive_class, positive_class]
    scores_neg <- pred_prob[testY != positive_class, positive_class]

    pr_obj <- PRROC::pr.curve(
      scores.class0 = scores_pos,
      scores.class1 = scores_neg,
      curve = TRUE
    )

    df_pr <- data.frame(
      recall = pr_obj$curve[, 1],
      precision = pr_obj$curve[, 2]
    )

    p <- ggplot2::ggplot(df_pr, ggplot2::aes(x = recall, y = precision)) +
      ggplot2::geom_line(color = curve_color, size = 1.2) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::labs(
        title = "Precision-Recall Curve (Binary Classification)",
        subtitle = paste0(
          "AUPRC = ", round(pr_obj$auc.integral, 3),
          " | Accuracy = ", round(accuracy, 3)
        ),
        x = "Recall",
        y = "Precision"
      )

    return(p)
  }

  # -------------------------
  # MULTICLASS CLASSIFICATION
  # -------------------------

  pr_list <- list()
  auc_list <- c()

  for (cls in levels(testY)) {

    scores_pos <- pred_prob[testY == cls, cls]
    scores_neg <- pred_prob[testY != cls, cls]

    pr_obj <- PRROC::pr.curve(
      scores.class0 = scores_pos,
      scores.class1 = scores_neg,
      curve = TRUE
    )

    pr_list[[cls]] <- pr_obj
    auc_list[cls] <- pr_obj$auc.integral
  }

  # Build combined PR dataframe
  df_multi <- do.call(rbind, lapply(names(pr_list), function(cls) {
    pr_obj <- pr_list[[cls]]
    data.frame(
      class = cls,
      recall = pr_obj$curve[, 1],
      precision = pr_obj$curve[, 2]
    )
  }))

  # Legend labels with AUPRC
  legend_labels <- paste0(names(auc_list), " (AUPRC = ", round(auc_list, 3), ")")

  p <- ggplot2::ggplot(df_multi, ggplot2::aes(
    x = recall, y = precision, color = class
  )) +
    ggplot2::geom_line(size = 1.2) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::scale_color_discrete(labels = legend_labels) +
    ggplot2::labs(
      title = "Precision-Recall Curve (Multiclass One-vs-Rest)",
      subtitle = paste0("Accuracy = ", round(accuracy, 3)),
      x = "Recall",
      y = "Precision",
      color = "Class"
    )

  return(p)
}
