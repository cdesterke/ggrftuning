#' 2D ALE Interaction Plot (Robust) using iml with PDP fallback
#'
#' Computes a 2D Accumulated Local Effects (ALE) interaction plot for two
#' features using the iml package. If iml fails to compute a 2D ALE surface
#' (common in multiclass RF or weak interactions), the function automatically
#' falls back to a 2D PDP interaction heatmap using a full predictor grid.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors.
#' @param features Character vector of length 2 specifying the two features.
#' @param class Target class for multiclass models (default = second class).
#' @param palette Viridis palette: "viridis", "magma", "inferno",
#'   "plasma", "cividis", "turbo".
#' @param base_size Base font size for ggplot2 theme.
#'
#' @return A ggplot2 heatmap (ALE 2D or PDP 2D fallback).
#'
#' @export
rf_ale2d <- function(model_rf, X_train, features,
                     class = NULL,
                     palette = "viridis",
                     base_size = 16) {

  if (!requireNamespace("iml", quietly = TRUE))
    stop("Package 'iml' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("ggplot2 required.")
  if (!requireNamespace("viridis", quietly = TRUE))
    stop("viridis required.")

  valid_palettes <- c("viridis","magma","inferno","plasma","cividis","turbo")
  if (!palette %in% valid_palettes)
    stop("palette must be one of: ", paste(valid_palettes, collapse=", "))

  if (length(features) != 2)
    stop("features must be a character vector of length 2.")

  f1 <- features[1]
  f2 <- features[2]

  X_train <- as.data.frame(X_train)

  # Detect multiclass
  prob <- predict(model_rf, X_train, type = "prob")
  classes <- colnames(prob)
  if (is.null(class)) class <- classes[2]

  # Predictor for iml
  predictor <- iml::Predictor$new(
    model = model_rf,
    data = X_train,
    y = NULL,
    predict.function = function(model, newdata)
      predict(model, newdata, type = "prob")[, class]
  )

  # Try ALE 2D
  ale_obj <- iml::FeatureEffect$new(
    predictor,
    feature = features,
    method = "ale"
  )

  df <- ale_obj$results

  # If ALE 2D failed → fallback to PDP 2D
  if (!".value" %in% colnames(df)) {

    message("⚠️ iml did not return a 2D ALE surface. Falling back to PDP 2D.")

    # Build full grid
    grid_f1 <- seq(min(X_train[[f1]]), max(X_train[[f1]]), length.out = 40)
    grid_f2 <- seq(min(X_train[[f2]]), max(X_train[[f2]]), length.out = 40)

    grid <- expand.grid(f1 = grid_f1, f2 = grid_f2)
    names(grid) <- c(f1, f2)

    # Add all other predictors (fixed at median/mode)
    for (col in setdiff(colnames(X_train), features)) {
      if (is.numeric(X_train[[col]])) {
        grid[[col]] <- median(X_train[[col]], na.rm = TRUE)
      } else {
        grid[[col]] <- names(sort(table(X_train[[col]]), decreasing = TRUE))[1]
      }
    }

    # Predict on full grid
    grid$pred <- predict(model_rf, grid, type = "prob")[, class]

    return(
      ggplot2::ggplot(grid, ggplot2::aes(
        x = .data[[f1]],
        y = .data[[f2]],
        fill = pred
      )) +
        ggplot2::geom_tile() +
        viridis::scale_fill_viridis(option = palette) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::labs(
          title = paste("PDP 2D Interaction (Fallback) —", f1, "×", f2),
          subtitle = paste("Class:", class),
          x = f1,
          y = f2,
          fill = "Prediction"
        )
    )
  }

  # ALE 2D heatmap
  ggplot2::ggplot(df, ggplot2::aes(
    x = .data[[f1]],
    y = .data[[f2]],
    fill = .value
  )) +
    ggplot2::geom_tile() +
    viridis::scale_fill_viridis(option = palette) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = paste("2D ALE Interaction —", f1, "×", f2),
      subtitle = paste("Class:", class),
      x = f1,
      y = f2,
      fill = "ALE"
    )
}
