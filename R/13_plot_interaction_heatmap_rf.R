#' Interaction heatmap (Friedman H-statistic) with palette selection
#'
#' Computes pairwise interaction strengths between features of a Random Forest
#' model using the Friedman H-statistic. The heatmap can be displayed with
#' different color palettes from the `viridis` family.
#'
#' Available palettes:
#' - "viridis"
#' - "magma"
#' - "inferno"
#' - "plasma"
#' - "cividis"
#' - "turbo"
#'
#' @param model A fitted Random Forest model
#' @param data A data.frame of predictors
#' @param palette Character string specifying the color palette
#'        (default: "viridis")
#'
#' @return A ggplot2 heatmap of pairwise H-statistics
#'
#' @examples
#' \dontrun{
#' # Default palette (viridis)
#' plot_interaction_heatmap_rf(out$model_rf, out$testX)
#'
#' # Magma palette
#' plot_interaction_heatmap_rf(out$model_rf, out$testX, palette = "magma")
#'
#' # Inferno palette
#' plot_interaction_heatmap_rf(out$model_rf, out$testX, palette = "inferno")
#' }
#'
#' @export
plot_interaction_heatmap_rf <- function(model, data, palette = "viridis") {

  if (!requireNamespace("pdp", quietly = TRUE)) stop("pdp required")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")
  if (!requireNamespace("reshape2", quietly = TRUE)) stop("reshape2 required")
  if (!requireNamespace("viridis", quietly = TRUE)) stop("viridis required")

  # Vérification palette
  allowed <- c("viridis", "magma", "inferno", "plasma", "cividis", "turbo")
  if (!palette %in% allowed) {
    stop(paste0("Palette must be one of: ", paste(allowed, collapse = ", ")))
  }

  features <- colnames(data)
  n <- length(features)

  Hmat <- matrix(0, n, n, dimnames = list(features, features))

  # Helper: prediction wrapper
  pred_fun <- function(newdata) {
    predict(model, newdata = newdata, type = "prob")[, 2]
  }

  for (i in seq_len(n)) {
    for (j in seq_len(n)) {

      if (i == j) next

      f1 <- features[i]
      f2 <- features[j]

      # PDP univariés
      pd1 <- pdp::partial(model, pred.var = f1, train = data,
                          type = "classification", prob = TRUE)
      pd2 <- pdp::partial(model, pred.var = f2, train = data,
                          type = "classification", prob = TRUE)

      # PDP bivarié
      pd12 <- pdp::partial(model, pred.var = c(f1, f2), train = data,
                           type = "classification", prob = TRUE)

      # Variances nécessaires
      var_f12 <- stats::var(pd12$yhat)
      var_f1  <- stats::var(pd1$yhat)
      var_f2  <- stats::var(pd2$yhat)

      # H-statistic
      H2 <- max(0, (var_f12 - var_f1 - var_f2) / var_f12)
      Hmat[i, j] <- sqrt(H2)
    }
  }

  # Heatmap ggplot
  df <- reshape2::melt(Hmat)
  colnames(df) <- c("Feature1", "Feature2", "H")

  ggplot2::ggplot(df, ggplot2::aes(Feature1, Feature2, fill = H)) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(option = palette) +
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 90, hjust = 1, vjust = 0.5, size = 14
      ),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 16),
      axis.title.y = ggplot2::element_text(size = 16)
    ) +
    ggplot2::labs(
      title = "Interaction Heatmap (Friedman H-statistic)",
      x = "",
      y = "",
      fill = "H"
    )
}
