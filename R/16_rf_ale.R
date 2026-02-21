#' ALE Plot (Univariate) using iml
#'
#' Computes a univariate Accumulated Local Effects (ALE) plot for a given
#' feature using the iml package. ALE plots are robust to correlated predictors
#' and provide a reliable alternative to PDP.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors.
#' @param feature Feature name to analyze.
#' @param palette Viridis palette: "viridis", "magma", "inferno",
#'   "plasma", "cividis", "turbo". Default = "viridis".
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A ggplot2 ALE plot.
#'
#' @examples
#' \dontrun{
#' rf_ale(
#'   model_rf = out$model_rf,
#'   X_train = out$trainX,
#'   feature = "Sepal.Width",
#'   palette = "magma"
#' )
#' }
#'
#' @export
rf_ale <- function(model_rf, X_train, feature,
                   palette = "viridis",
                   base_size = 16) {

  if (!requireNamespace("iml", quietly = TRUE))
    stop("Package 'iml' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!requireNamespace("viridis", quietly = TRUE))
    stop("Package 'viridis' is required.")

  valid_palettes <- c("viridis","magma","inferno","plasma","cividis","turbo")
  if (!palette %in% valid_palettes)
    stop("palette must be one of: ", paste(valid_palettes, collapse=", "))

  X_train <- as.data.frame(X_train)

  # Predictor object for iml
  predictor <- iml::Predictor$new(
    model = model_rf,
    data = X_train,
    y = NULL,
    predict.function = function(model, newdata)
      predict(model, newdata, type = "prob")[,2]
  )

  # ALE object
  ale_obj <- iml::FeatureEffect$new(
    predictor,
    feature = feature,
    method = "ale"
  )

  df <- ale_obj$results

  # iml renvoie .value, pas .ale
  ggplot2::ggplot(df, ggplot2::aes(
    x = .data[[feature]],
    y = .value,
    color = .value
  )) +
    ggplot2::geom_line(linewidth = 1.2) +
    viridis::scale_color_viridis(option = palette) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = paste("ALE Plot —", feature),
      x = feature,
      y = "ALE"
    )
}
