#' SHAP Summary Plot for Random Forest Models (Binary & Multiclass)
#'
#' Computes SHAP values using fastshap for a fitted Random Forest model and
#' produces a global SHAP summary plot (beeswarm style). Supports both binary
#' and multiclass classification. Allows choosing a viridis palette.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors used to fit the model.
#' @param palette Character. Viridis palette: "viridis", "magma", "inferno",
#'   "plasma", "cividis", or "turbo". Default = "viridis".
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A list with:
#' \describe{
#'   \item{shap_values}{A list of SHAP matrices (one per class).}
#'   \item{plots}{A list of ggplot2 SHAP summary plots.}
#' }
#'
#' @examples
#' \dontrun{
#'   out <- rf_model_with_oob(
#'     X = iris[,1:4],
#'     Y = iris$Species,
#'     mtry = 2,
#'     ntree = 150,
#'     split = 0.65
#'   )
#'
#'   shap <- rf_shap_summary(
#'     model_rf = out$model_rf,
#'     X_train = out$trainX,
#'     palette = "magma"
#'   )
#'
#'   shap$plots[[1]]
#' }
#'
#' @export
rf_shap_summary <- function(model_rf, X_train,
                            palette = "viridis",
                            base_size = 16) {

  # Required packages
  if (!requireNamespace("fastshap", quietly = TRUE))
    stop("Package 'fastshap' is required.")
  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Package 'ggplot2' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Package 'dplyr' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE))
    stop("Package 'tidyr' is required.")
  if (!requireNamespace("viridis", quietly = TRUE))
    stop("Package 'viridis' is required.")

  # Validate palette
  valid_palettes <- c("viridis", "magma", "inferno", "plasma", "cividis", "turbo")
  if (!palette %in% valid_palettes)
    stop("palette must be one of: ", paste(valid_palettes, collapse = ", "))

  X_train <- as.data.frame(X_train)

  # Detect multiclass
  prob_mat <- predict(model_rf, X_train, type = "prob")
  is_multiclass <- is.matrix(prob_mat)

  # Wrapper compatible with fastshap
  pred_fun <- function(object, newdata, class_index, ...) {
    predict(object, newdata, type = "prob")[, class_index]
  }

  shap_list <- list()
  plot_list <- list()

  # -------------------------
  # MULTICLASS CASE
  # -------------------------
  if (is_multiclass) {

    classes <- colnames(prob_mat)

    for (cls in classes) {

      shap_mat <- fastshap::explain(
        object = model_rf,
        X = X_train,
        pred_wrapper = function(object, newdata)
          pred_fun(object, newdata, cls),
        nsim = 50
      )

      shap_df <- cbind(X_train, shap_mat)

      shap_long <- tidyr::pivot_longer(
        shap_df,
        cols = colnames(shap_mat),
        names_to = "Variable",
        values_to = "SHAP"
      )

      p <- ggplot2::ggplot(
        shap_long,
        ggplot2::aes(
          x = SHAP,
          y = Variable,
          color = SHAP
        )
      ) +
        ggplot2::geom_point(alpha = 0.6) +
        viridis::scale_color_viridis(option = palette) +
        ggplot2::theme_minimal(base_size = base_size) +
        ggplot2::labs(
          title = paste("SHAP Summary Plot — Class:", cls),
          x = "SHAP value",
          y = "Variable"
        )

      shap_list[[cls]] <- shap_mat
      plot_list[[cls]] <- p
    }

  } else {

    # -------------------------
    # BINARY CASE
    # -------------------------
    positive_class <- colnames(prob_mat)[2]

    shap_mat <- fastshap::explain(
      object = model_rf,
      X = X_train,
      pred_wrapper = function(object, newdata)
        pred_fun(object, newdata, positive_class),
      nsim = 50
    )

    shap_df <- cbind(X_train, shap_mat)

    shap_long <- tidyr::pivot_longer(
      shap_df,
      cols = colnames(shap_mat),
      names_to = "Variable",
      values_to = "SHAP"

    )

    p <- ggplot2::ggplot(
      shap_long,
      ggplot2::aes(
        x = SHAP,
        y = Variable,
        color = SHAP
      )
    ) +
      ggplot2::geom_point(alpha = 0.6) +
      viridis::scale_color_viridis(option = palette) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::labs(
        title = "SHAP Summary Plot (Binary Classification)",
        x = "SHAP value",
        y = "Variable"
      )

    shap_list[["binary"]] <- shap_mat
    plot_list[["binary"]] <- p
  }

  list(
    shap_values = shap_list,
    plots = plot_list
  )
}

