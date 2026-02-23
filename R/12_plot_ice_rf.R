#' ICE curves for a feature
#'
#' Generates Individual Conditional Expectation (ICE) curves for a given feature,
#' showing how predictions vary for each observation.
#'
#' @param model A fitted Random Forest model
#' @param data Dataset used for ICE computation
#' @param feature Character string: feature to analyze
#'
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' plot_ice_rf(out$model_rf, out$testX, "Petal.Width")
#' }
#'
#' @export
plot_ice_rf <- function(model, data, feature) {
  if (!requireNamespace("pdp", quietly = TRUE)) stop("pdp required")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 required")

  ice <- pdp::partial(
    object = model,
    pred.var = feature,
    train = data,
    type = "classification",
    prob = TRUE,
    ice = TRUE
  )

  df <- as.data.frame(ice)

  ggplot2::ggplot(df, ggplot2::aes_string(x = feature, y = "yhat", group = "yhat.id")) +
    ggplot2::geom_line(alpha = 1, color = "#1f77b4") +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = paste("ICE curves -", feature),
      x = feature,
      y = "Predicted probability"
    )
}

