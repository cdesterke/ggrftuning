#' Tune the mtry Parameter of a Random Forest Model
#'
#' This function performs a simple grid search over all possible \code{mtry}
#' values for a Random Forest classifier, using out-of-bag (OOB) error as the
#' evaluation metric. It accepts predictors \code{X} and response \code{Y}
#' as separate inputs, fits models for each \code{mtry}, and returns both
#' the tuning results and a ggplot2 visualization with ggrepel labels.
#'
#' @param X A data.frame or matrix of predictor variables.
#' @param Y A vector or factor containing the response variable. Must have
#'   the same number of rows as \code{X}.
#' @param ntree Integer. Number of trees to grow. Default is 500.
#' @param base_size Numeric. Base font size for the ggplot theme. Default is 16.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{results}{A data.frame containing \code{mtry} and corresponding OOB error.}
#'   \item{plot}{A ggplot2 object visualizing OOB error vs. \code{mtry}.}
#' }
#'
#' @examples
#' \dontrun{
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'   out <- tune_mtry_rf(X, Y,ntree=500)
#'   out$results
#'   out$plot
#' }
#'
#' @export
tune_mtry_rf <- function(X, Y, ntree = 500, base_size = 16) {

  # Required packages
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Package 'randomForest' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

  # Convert X to data.frame if needed
  if (!is.data.frame(X)) X <- as.data.frame(X)

  # Basic checks
  if (nrow(X) != length(Y)) stop("X and Y must have the same number of observations.")

  # Build modeling data.frame
  df <- data.frame(X, Y = Y)

  # Grid of mtry values
  mtry_grid <- seq_len(ncol(X))
  results <- data.frame()

  # Loop over mtry values
  for (m in mtry_grid) {
    rf_model <- randomForest::randomForest(
      Y ~ .,
      data = df,
      mtry = m,
      ntree = ntree
    )

    oob <- rf_model$err.rate[ntree, "OOB"]

    results <- rbind(results, data.frame(mtry = m, OOB_Error = oob))
  }

  # Plot with subtitle showing ntree
  p <- ggplot2::ggplot(results, ggplot2::aes(x = mtry, y = OOB_Error)) +
    ggplot2::geom_line(color = "steelblue", linewidth = 1) +
    ggplot2::geom_point(size = 3, color = "darkred") +
    ggrepel::geom_text_repel(
      ggplot2::aes(label = round(OOB_Error, 3)),
      size = 5,
      nudge_y = 0.002,
      color = "black",
      max.overlaps = Inf
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "Tuning of mtry for Random Forest",
      subtitle = paste0("ntree = ", ntree),
      x = "mtry",
      y = "OOB error"
    )

  list(
    results = results,
    plot = p
  )
}
