#' @title Predict anomaly scores using an Isolation Forest model
#' @description
#' Predict anomaly scores using an Isolation Forest model
#' @param object an Isolation Forest model
#' @param newdata a data frame containing the new observations to be predicted
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
  # Enhanced parameter validation
  # Check object validity
  if (is.null(object) || !inherits(object, "isoForest")) {
    stop("object must be an isoForest model")
  }
  if (is.null(object$model)) {
    stop("isoForest object is missing the model component")
  }
  
  # Check newdata validity
  if (is.null(newdata) || nrow(newdata) == 0) {
    stop("newdata cannot be NULL or empty")
  }
  if (!any(sapply(newdata, is.numeric))) {
    stop("newdata must contain at least one numeric column")
  }
  if (!is.data.frame(newdata)) {
    newdata <- as.data.frame(newdata)
  }
  
  # Check type parameter
  if (!type %in% c("terminalNodes", "response")) {
    stop("type must be 'terminalNodes' or 'response'")
  }
  
  if (is.null(seed)) {
    set.seed(as.numeric(Sys.time()))
  } else {
    set.seed(seed)
  }
  terminal_nodes_depth <- calculate_leaf_to_root_depth(object$model)
  terminal_node_matrix <- stats::predict(object$model,
    newdata,
    type = type,
    num.threads = num.threads,
    ...
  )[["predictions"]]
  n_obs <- nrow(terminal_node_matrix)
  n_trees <- ncol(terminal_node_matrix)
  
  obs_ids <- rep(seq_len(n_obs), n_trees)
  tree_ids <- rep(seq_len(n_trees), each = n_obs)
  node_ids <- as.vector(terminal_node_matrix)
  
  terminal_nodes_long <- data.frame(
    id = obs_ids,
    treeID = tree_ids,
    nodeID = node_ids
  )
  
  obs_depth <- dplyr::inner_join(terminal_nodes_depth, terminal_nodes_long, by = c("treeID", "nodeID"))
  scores <- obs_depth |>
    dplyr::group_by(id) |>
    dplyr::summarise(
      average_depth = mean(depth),
      anomaly_score = computeAnomaly(average_depth, object$sample_size)
    )

  return(scores)
}
