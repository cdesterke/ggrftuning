#' Plot Variable Importance with Magma Gradient (Robust Version)
#'
#' This function extracts variable importance measures from a fitted
#' randomForest model and generates ggplot2 barplots using a Magma
#' color gradient for all available importance metrics. It automatically
#' adapts to classification or regression models and never fails if some
#' importance metrics are missing.
#'
#' @param model_rf A fitted randomForest model.
#' @param base_size Numeric. Base font size for ggplot themes. Default = 16.
#'
#' @return A list with:
#' \describe{
#'   \item{importance_df}{Data.frame of variable importances.}
#'   \item{plots}{A named list of ggplot2 barplots (one per importance metric).}
#' }
#'
#' @examples
#' \dontrun{
#'   # Example using iris dataset
#'   X <- iris[, 1:4]
#'   Y <- iris$Species
#'
#'   # Train a RF model with importance enabled
#'   model <- randomForest::randomForest(
#'     x = X,
#'     y = Y,
#'     ntree = 100,
#'     mtry = 2,
#'     importance = TRUE
#'   )
#'
#'   # Generate Magma barplots for all available importance metrics
#'   out_imp <- rf_importance_plots(model)
#'
#'   # View importance table
#'   head(out_imp$importance_df)
#'
#'   # Display one of the plots (e.g., MeanDecreaseGini)
#'   out_imp$plots$MeanDecreaseGini
#'   out_imp$plots$MeanDecreaseAccuracy
#' }
#'
#' @export
rf_importance_plots <- function(model_rf, base_size = 16) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("viridis", quietly = TRUE)) stop("Package 'viridis' is required.")

  # Extract importance
  imp <- randomForest::importance(model_rf)
  imp_df <- as.data.frame(imp)
  imp_df$Variable <- rownames(imp_df)

  # Identify available importance metrics
  available_metrics <- setdiff(colnames(imp_df), "Variable")

  if (length(available_metrics) == 0) {
    stop("No importance metrics available in model_rf. Did you train with importance=TRUE ?")
  }

  # Function to generate a magma barplot for one metric
  make_plot <- function(df, metric) {

    df <- df[order(df[[metric]], decreasing = TRUE), ]

    ggplot2::ggplot(
      df,
      ggplot2::aes(x = reorder(Variable, df[[metric]]),
                   y = .data[[metric]],
                   fill = .data[[metric]])
    ) +
      ggplot2::geom_col() +
      ggplot2::geom_text(
        ggplot2::aes(label = round(.data[[metric]], 3),
                     y = .data[[metric]] + max(.data[[metric]]) * 0.05),
        size = 4
      ) +
      viridis::scale_fill_viridis(option = "magma", direction = 1) +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::labs(
        title = paste("Variable Importance -", metric),
        x = "",
        y = "Importance",
        fill = "Value"
      )
  }

  # Generate plots for all available metrics
  plot_list <- lapply(available_metrics, function(m) make_plot(imp_df, m))
  names(plot_list) <- available_metrics

  list(
    importance_df = imp_df,
    plots = plot_list
  )
}
