#' Explainable AI: Tree-wise Variable Importance for Random Forest
#'
#' This function computes a tree-wise permutation importance for each variable
#' in a fitted Random Forest model. For each tree, each variable is permuted
#' and the increase in prediction error is measured. This produces a detailed
#' Explainable AI visualization showing which trees ("tentatives") explain
#' which variables best.
#'
#' @param model_rf A fitted randomForest model.
#' @param X_train Training predictors used to fit the model.
#' @param Y_train Training response used to fit the model.
#' @param base_size Base font size for ggplot2 theme. Default = 16.
#'
#' @return A list with:
#' \describe{
#'   \item{tree_importance}{A long-format data.frame of tree-wise importances.}
#'   \item{plot}{A ggplot2 Explainable AI plot.}
#' }
#'
#' @examples
#' \dontrun{
#'   out <- rf_model_with_oob(X = iris[,1:4], Y = iris$Species,
#'                            mtry = 2, ntree = 150, split = 0.65)
#'
#'   expl <- rf_explain_trees(
#'     model_rf = out$model_rf,
#'     X_train = out$trainX,
#'     Y_train = out$trainY
#'   )
#'
#'   expl$plot
#' }
#'
#' @export
rf_explain_trees <- function(model_rf,
                             X_train,
                             Y_train,
                             base_size = 16) {

  # --- Compute tree-wise permutation importance ------------------------------
  n_trees <- model_rf$ntree
  vars <- colnames(X_train)

  tree_importance <- lapply(seq_len(n_trees), function(t) {
    pred_base <- predict(model_rf, X_train, predict.all = TRUE)$individual[, t]
    err_base <- mean(pred_base != Y_train)

    imp <- sapply(vars, function(v) {
      X_perm <- X_train
      X_perm[[v]] <- sample(X_perm[[v]])
      pred_perm <- predict(model_rf, X_perm, predict.all = TRUE)$individual[, t]
      mean(pred_perm != Y_train) - err_base
    })

    data.frame(
      tree = t,
      variable = vars,
      importance = imp,
      stringsAsFactors = FALSE
    )
  })
  tree_importance <- do.call(rbind, tree_importance)

  # --- Custom 26-color qualitative palette -----------------------------------
  palette26 <- c(
    "#332288", "#117733", "#44AA99", "#88CCEE",
    "#DDCC77", "#CC6677", "#AA4499", "#882255",
    "#661100", "#6699CC", "#AA4466", "#4477AA",
    "#228833", "#66CCEE", "#EE6677", "#CCBB44",
    "#994455", "#EECC66", "#77AADD", "#99DDFF",
    "#44BB99", "#BB5566", "#DDDDDD", "#555555",
    "#000000", "#BBBBBB"
  )

  # --- Plot -------------------------------------------------------------------
  p <- ggplot2::ggplot(
    tree_importance,
    ggplot2::aes(x = tree, y = importance, color = variable)
  ) +
    ggplot2::geom_point(
      size = 2,
      alpha = 1,
      shape = 16
    ) +
    ggplot2::geom_smooth(
      method = "loess",
      se = FALSE,
      linewidth = 1,
      alpha = 1
    ) +
    ggplot2::scale_color_manual(
      values = palette26[seq_along(unique(tree_importance$variable))]
    ) +
    ggplot2::labs(
      title = "Explainable AI: Tree-wise Variable Importance",
      subtitle = "Permutation importance per tree with LOESS smoothing per feature",
      x = "Tree index",
      y = "Permutation importance (Δ error)",
      color = "Variable"
    ) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "right",
      panel.grid.minor = ggplot2::element_blank()
    )

  list(
    tree_importance = tree_importance,
    plot = p
  )
}
