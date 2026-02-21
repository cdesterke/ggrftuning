#' Train a Random Forest Model and Plot Class-Specific OOB Error
#'
#' This function trains a Random Forest classifier using a user-defined
#' train/test split, \code{mtry}, and \code{ntree}. It returns a summary of
#' model performance, variable importances, and a ggplot2 visualization showing
#' the evolution of out-of-bag (OOB) error across trees, both globally and for
#' each class. The global OOB curve is displayed in black dashed style, while
#' class-specific OOB curves use the standard ggplot2 color palette.
#'
#' @param X A data.frame or matrix of predictor variables.
#' @param Y A factor containing the response variable. Must have the same
#'   number of rows as \code{X}.
#' @param mtry Integer. Number of variables randomly sampled at each split.
#' @param ntree Integer. Number of trees to grow.
#' @param split Numeric. Proportion of data used for training (between 0 and 1).
#' @param base_size Numeric. Base font size for the ggplot theme. Default is 16.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{A list containing OOB error, test accuracy, and confusion matrix.}
#'   \item{plot}{A ggplot2 object showing OOB error vs. number of trees for each class.}
#'   \item{importance_df}{A data.frame of variable importances.}
#'   \item{model_rf}{The fitted randomForest model.}
#'   \item{trainX}{Training predictors.}
#'   \item{trainY}{Training response.}
#'   \item{testX}{Test predictors.}
#'   \item{testY}{Test response.}
#' }
#'
#' @examples
#' \dontrun{
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'
#'   out <- rf_model_with_oob(X = X, Y = Y, mtry = 2, ntree = 150, split = 0.65)
#'
#'   out$results
#'   print(out$plot)
#'   head(out$importance_df)
#' }
#'
#' @export
rf_model_with_oob <- function(X, Y, mtry, ntree, split = 0.7, base_size = 16) {

  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Package 'randomForest' is required.")
  if (!requireNamespace("caret", quietly = TRUE)) stop("Package 'caret' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("scales", quietly = TRUE)) stop("Package 'scales' is required.")

  if (!is.data.frame(X)) X <- as.data.frame(X)
  if (nrow(X) != length(Y)) stop("X and Y must have the same number of observations.")
  if (!is.factor(Y)) stop("Y must be a factor.")
  if (mtry < 1 || mtry > ncol(X)) stop("Invalid mtry.")
  if (split <= 0 || split >= 1) stop("split must be between 0 and 1.")

  set.seed(123)
  idx <- caret::createDataPartition(Y, p = split, list = FALSE)

  X_train <- X[idx, ]
  Y_train <- Y[idx]
  X_test  <- X[-idx, ]
  Y_test  <- Y[-idx]

  model_rf <- randomForest::randomForest(
    Y ~ .,
    data = data.frame(X_train, Y = Y_train),
    mtry = mtry,
    ntree = ntree,
    importance = TRUE
  )

  # OOB error (global + per class)
  oob_df <- data.frame(Trees = seq_len(ntree), model_rf$err.rate)

  oob_long <- tidyr::pivot_longer(
    oob_df,
    cols = -Trees,
    names_to = "Class",
    values_to = "OOB"
  )

  # Variables pour mapping propre
  oob_long <- dplyr::mutate(
    oob_long,
    Group = dplyr::if_else(Class == "OOB", "Global OOB", Class),
    Type  = dplyr::if_else(Class == "OOB", "Global", "Class")
  )

  class_levels <- unique(oob_long$Group[oob_long$Group != "Global OOB"])
  class_colors <- scales::hue_pal()(length(class_levels))
  names(class_colors) <- class_levels

  color_values <- c("Global OOB" = "darkgrey", class_colors)

  p <- ggplot2::ggplot(oob_long, ggplot2::aes(x = Trees, y = OOB)) +
    ggplot2::geom_line(
      ggplot2::aes(color = Group, linetype = Type),
      linewidth = 1
    ) +
    ggplot2::scale_color_manual(values = color_values) +
    ggplot2::scale_linetype_manual(values = c("Global" = "dashed", "Class" = "solid")) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "OOB Error Evolution Across Trees",
      subtitle = paste0("mtry = ", mtry, "   |   ntree = ", ntree,
                        "   |   train proportion = ", split),
      x = "Number of Trees",
      y = "OOB Error",
      color = "Group",
      linetype = "Type"
    )

  preds <- predict(model_rf, X_test)
  acc <- mean(preds == Y_test)
  cm <- table(Predicted = preds, True = Y_test)

  imp <- randomForest::importance(model_rf)
  imp_df <- as.data.frame(imp)
  imp_df$Variable <- rownames(imp_df)

  results <- list(
    final_oob_global = model_rf$err.rate[ntree, "OOB"],
    final_oob_by_class = model_rf$err.rate[ntree, colnames(model_rf$err.rate) != "OOB"],
    test_accuracy = acc,
    confusion_matrix = cm
  )

  list(
    results = results,
    plot = p,
    importance_df = imp_df,
    model_rf = model_rf,
    trainX = X_train,
    trainY = Y_train,
    testX = X_test,
    testY = Y_test
  )
}

