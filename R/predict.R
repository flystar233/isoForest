#' @title Predict anomaly scores using an Isolation Forest model
#' @description
#' Predict anomaly scores using an Isolation Forest model
#' @param object an Isolation Forest model
#' @param newdata a data frame containing the new observations to be predicted
#' @param sample_size the sample size to be used for the anomaly score calculation
#' (default is the minimum of the number of observations in `newdata` and 256)
#' @param num.threads the number of threads to be used for the prediction (default is NULL, which means all available threads)
#' @param type the type of prediction to be made (default is "terminalNodes")
#' @param seed the seed to be used for the random number generator (default is NULL)
#' @param ... additional arguments to be passed to the `predict` function
#' @return a data frame containing the anomaly scores for the new observations
#' @examples
#' # Load the example data
#' data(iris)
#' # Train an Isolation Forest model
#' model <- isoForest(iris[1:2])
#' # Predict anomaly scores for new observations
#' predictions <- predict(model, iris[1:2])
#' @export
predict.isoForest <- function(object,
                              newdata,
                              num.threads = NULL,
                              type = "terminalNodes",
                              seed = NULL,
                              ...) {
  if (is.null(seed)) {
    set.seed(as.numeric(Sys.time()))
  } else {
    set.seed(seed)
  }
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  terminal_nodes_depth <- calculate_leaf_to_root_depth(object$model)
  tnm <- stats::predict(object$model,
                        newdata,
                        type = type,
                        num.threads = num.threads,
                        ...)[["predictions"]]
  tnm <- as.data.frame(tnm)
  colnames(tnm) <- as.character(1:ncol(tnm))
  tnm$id <- 1:nrow(tnm)
  tnm <- tidyr::pivot_longer(tnm, cols = -id, names_to = "treeID", values_to = "nodeID")
  tnm$treeID <- as.integer(tnm$treeID)
  tnm$nodeID <- as.integer(tnm$nodeID)
  obs_depth <- dplyr::inner_join(terminal_nodes_depth, tnm, by = c("treeID", "nodeID"))
  scores <- obs_depth |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      average_depth = mean(depth),
      anomaly_score = computeAnomaly(average_depth, object$sample_size)
    )

  return(scores)
}
