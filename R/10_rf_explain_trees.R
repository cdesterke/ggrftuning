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
rf_explain_trees <- function(model_rf, X_train, Y_train, base_size = 16) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' is required.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' is required.")
  if (!requireNamespace("tidyr", quietly = TRUE)) stop("Package 'tidyr' is required.")

  ntree <- model_rf$ntree
  vars <- colnames(X_train)

  # Storage
  imp_list <- list()

  # Compute permutation importance per tree
  for (t in seq_len(ntree)) {

    # Predictions from tree t only
    pred_t <- predict(model_rf, X_train, predict.all = TRUE)$individual[, t]

    base_err <- mean(pred_t != Y_train)

    for (v in vars) {

      X_perm <- X_train
      X_perm[[v]] <- sample(X_perm[[v]])

      pred_perm <- predict(model_rf, X_perm, predict.all = TRUE)$individual[, t]
      perm_err <- mean(pred_perm != Y_train)

      imp_list[[length(imp_list) + 1]] <- data.frame(
        Tree = t,
        Variable = v,
        Importance = perm_err - base_err
      )
    }
  }

  imp_df <- dplyr::bind_rows(imp_list)

  # Plot
  p <- ggplot2::ggplot(
    imp_df,
    ggplot2::aes(
      x = Tree,
      y = Importance,
      color = Variable,
      shape = Variable
    )
  ) +
    ggplot2::geom_point(size = 2.8, alpha = 0.8) +
    ggplot2::geom_smooth(se = FALSE, linewidth = 1) +
    ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::labs(
      title = "Explainable AI: Tree-wise Variable Importance",
      subtitle = "Permutation importance per tree",
      x = "Tree index",
      y = "Permutation importance"
    )

  list(
    tree_importance = imp_df,
    plot = p
  )
}
