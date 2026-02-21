#' Evaluate Random Forest Performance Across Different Train/Test Splits
#'
#' This function evaluates the effect of different train/test split proportions
#' on the performance of a Random Forest classifier using the \code{caret}
#' interface to the \code{ranger} engine. The user provides predictors \code{X},
#' response \code{Y}, and fixed values for \code{mtry} and \code{ntree}.
#' The function returns accuracy for each split proportion and a ggplot2
#' visualization with ggrepel labels.
#'
#' @param X A data.frame or matrix of predictor variables.
#' @param Y A factor containing the response variable. Must have the same
#'   number of rows as \code{X}.
#' @param mtry Integer. Number of variables randomly sampled at each split.
#' @param ntree Integer. Number of trees to grow.
#' @param split_seq Numeric vector of train proportions to evaluate.
#'   Default is \code{c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80)}.
#' @param base_size Numeric. Base font size for the ggplot theme. Default is 16.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{A data.frame with split proportion and accuracy.}
#'   \item{plot}{A ggplot2 object visualizing accuracy vs. split proportion.}
#' }
#'
#' @examples
#' \dontrun{
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'   out <- tune_split_ratio_caret(X, Y, mtry = 2, ntree = 500)
#'   out$results
#'   out$plot
#' }
#'
#' @export
tune_split_ratio_caret <- function(X, Y,
                                   mtry,
                                   ntree,
                                   split_seq = c(0.55, 0.60, 0.65, 0.70, 0.75, 0.80),
                                   base_size = 16) {

  # Required packages
  if (!requireNamespace("caret", quietly = TRUE)) stop("Package 'caret' is required.")
  if (!requireNamespace("ranger", quietly = TRUE)) stop("Package 'ranger' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

  # Convert X to data.frame if needed
  if (!is.data.frame(X)) X <- as.data.frame(X)

  # Basic checks
  if (nrow(X) != length(Y)) stop("X and Y must have the same number of observations.")
  if (!is.factor(Y)) stop("Y must be a factor for classification.")
  if (mtry < 1 || mtry > ncol(X)) stop("mtry must be between 1 and ncol(X).")

  results <- data.frame()

  # Loop over split proportions
  for (p in split_seq) {

    set.seed(123)
    train_index <- caret::createDataPartition(Y, p = p, list = FALSE)

    X_train <- X[train_index, ]
    Y_train <- Y[train_index]

    X_test  <- X[-train_index, ]
    Y_test  <- Y[-train_index]

    model <- ranger::ranger(
      dependent.variable.name = "Y",
      data = data.frame(X_train, Y = Y_train),
      mtry = mtry,
      num.trees = ntree,
      probability = FALSE,
      classification = TRUE
    )

    preds <- predict(model, data.frame(X_test))$predictions
    acc <- mean(preds == Y_test)

    results <- rbind(results, data.frame(split = p, Accuracy = acc))
  }

  # Plot with subtitle showing mtry and ntree
  p <- ggplot2::ggplot(results, ggplot2::aes(x = split, y = Accuracy)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(size = 3, color = "darkred") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = round(Accuracy, 3)),
      size = 5,
      nudge_y = 0.002,
      color = "black",
      max.overlaps = Inf
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "Train/Test Split vs Random Forest Accuracy",
      subtitle = paste0("mtry = ", mtry, "   |   ntree = ", ntree),
      x = "Train Proportion",
      y = "Accuracy"
    )

  list(
    results = results,
    plot = p
  )
}
