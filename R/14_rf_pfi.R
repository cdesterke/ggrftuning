#' Permutation Feature Importance (PFI) for Random Forest
#'
#' Computes permutation-based feature importance by measuring the decrease in
#' predictive performance when permuting each feature. Supports binary and
#' multiclass classification. Produces a ranked barplot with viridis palettes.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_test Test predictors.
#' @param y_test True labels for the test set.
#' @param metric Performance metric: "accuracy" or "logloss".
#' @param palette Viridis palette: "viridis", "magma", "inferno",
#'   "plasma", "cividis", "turbo".
#' @param base_size Base font size for ggplot2 theme.
#'
#' @return A list with:
#' \describe{
#'   \item{importance}{A data.frame with permutation importances.}
#'   \item{plot}{A ggplot2 barplot.}
#' }
#'
#' @examples
#' \dontrun{
#' pfi <- rf_pfi(
#'   model_rf = out$model_rf,
#'   X_test = out$testX,
#'   y_test = out$testY,
#'   palette = "inferno"
#' )
#' pfi$plot
#' }
#'
#' @export
rf_pfi <- function(model_rf, X_test, y_test,
                   metric = "accuracy",
                   palette = "viridis",
                   base_size = 16) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("viridis", quietly = TRUE)) stop("viridis required")

  valid_palettes <- c("viridis","magma","inferno","plasma","cividis","turbo")
  if (!palette %in% valid_palettes)
    stop("palette must be one of: ", paste(valid_palettes, collapse=", "))

  X_test <- as.data.frame(X_test)
  baseline <- predict(model_rf, X_test, type="prob")

  if (metric == "accuracy") {
    base_perf <- mean(colnames(baseline)[max.col(baseline)] == y_test)
  } else {
    base_perf <- -mean(log(baseline[cbind(seq_len(nrow(baseline)), match(y_test, colnames(baseline)))]))
  }

  imp <- sapply(colnames(X_test), function(v) {
    X_perm <- X_test
    X_perm[[v]] <- sample(X_perm[[v]])

    pred <- predict(model_rf, X_perm, type="prob")

    if (metric == "accuracy") {
      perf <- mean(colnames(pred)[max.col(pred)] == y_test)
    } else {
      perf <- -mean(log(pred[cbind(seq_len(nrow(pred)), match(y_test, colnames(pred)))]))
    }

    base_perf - perf
  })

  df <- data.frame(Feature = names(imp), Importance = imp)
  df <- df[order(df$Importance, decreasing = TRUE), ]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(Feature, Importance),
                                        y = Importance,
                                        fill = Importance)) +
    ggplot2::geom_col() +
    viridis::scale_fill_viridis(option = palette) +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(title = "Permutation Feature Importance",
                  x = "Feature", y = "Importance")

  list(importance = df, plot = p)
}
