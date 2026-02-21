#' Tune the ntree Parameter of a Random Forest Model
#'
#' This function performs a grid search over a user-defined set of \code{ntree}
#' values for a Random Forest classifier, using out-of-bag (OOB) error as the
#' evaluation metric. It accepts predictors \code{X} and response \code{Y}
#' as separate inputs, fits models for each \code{ntree}, and returns both
#' the tuning results and a ggplot2 visualization with ggrepel labels.
#'
#' @param X A data.frame or matrix of predictor variables.
#' @param Y A vector or factor containing the response variable. Must have
#'   the same number of rows as \code{X}.
#' @param mtry Integer. The number of variables randomly sampled at each split.
#'   Must be between 1 and \code{ncol(X)}.
#' @param ntree_grid A numeric vector of ntree values to evaluate.
#'   Default is \code{seq(100, 1000, by = 50)}.
#' @param base_size Numeric. Base font size for the ggplot theme. Default is 16.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{results}{A data.frame containing \code{ntree} and corresponding OOB error.}
#'   \item{plot}{A ggplot2 object visualizing OOB error vs. \code{ntree}.}
#' }
#'
#' @examples
#' \dontrun{
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'   out <- tune_ntree_rf(X, Y, mtry = 2)
#'   out$results
#'   out$plot
#' }
#'
#' @export
tune_ntree_rf <- function(X, Y,
                          mtry,
                          ntree_grid = seq(100, 1000, by = 50),
                          base_size = 16) {

  # Required packages
  if (!requireNamespace("randomForest", quietly = TRUE)) stop("Package 'randomForest' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("ggrepel", quietly = TRUE)) stop("Package 'ggrepel' is required.")

  # Convert X to data.frame if needed
  if (!is.data.frame(X)) X <- as.data.frame(X)

  # Basic checks
  if (nrow(X) != length(Y)) stop("X and Y must have the same number of observations.")
  if (mtry < 1 || mtry > ncol(X)) stop("mtry must be between 1 and ncol(X).")

  # Build modeling data.frame
  df <- data.frame(X, Y = Y)

  # Storage
  results <- data.frame()

  # Loop over ntree values
  for (nt in ntree_grid) {
    rf_model <- randomForest::randomForest(
      Y ~ .,
      data = df,
      mtry = mtry,
      ntree = nt
    )

    oob <- rf_model$err.rate[nt, "OOB"]

    results <- rbind(results, data.frame(ntree = nt, OOB_Error = oob))
  }

  # Plot with subtitle showing mtry
  p <- ggplot2::ggplot(results, ggplot2::aes(x = ntree, y = OOB_Error)) +
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
      title = "Tuning of ntree for Random Forest",
      subtitle = paste0("mtry = ", mtry),
      x = "ntree",
      y = "OOB error"
    )

  list(
    results = results,
    plot = p
  )
}
