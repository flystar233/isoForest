#' @title Isolation Forest
#' @description
#' This function implements the Isolation Forest algorithm for anomaly detection by 'ranger'.
#' @param data A data frame or matrix containing the data to be analyzed.
#' @param num_trees The number of trees to be grown in the forest. Default is 500.
#' @param sample_size The size of the sample to be used for each tree. Default is the minimum of the number of rows in the data and 256.
#' @param max_depth The maximum depth of each tree. Default is the ceiling of the logarithm base 2 of the sample size.
#' @param mtry The number of variables to consider when splitting each node. Default is NULL, which means that the number of variables is set to the square root of the number of variables in the data.
#' @param num.threads The number of threads to use for parallel processing. Default is NULL, which means that all available threads are used.
#' @param seed The seed for random number generation. Default is NULL, which means that the current time is used as the seed.
#' @param ... Additional arguments to be passed to the ranger function.
#' @return A list containing the anomaly scores for each data point. The anomaly scores are calculated as the average path length from the data point to the root of the tree.
#' @examples
#' # Load the required libraries
#' library(ranger)
#' library(isoForest)
#' # Load the data
#' data <- iris
#' # Train and predict the Isolation Forest model by 'ranger'.
#' result <- isoForest(data)
#' @export
isoForest <- function(data,
                      num_trees = 500,
                      sample_size = min(nrow(data), 256L),
                      max_depth = ceiling(log2(sample_size)),
                      mtry = NULL,
                      num.threads = NULL,
                      seed = NULL,
                      ...) {
  # Check data validity
  if (is.null(data) || nrow(data) == 0) {
    stop("Data cannot be NULL or empty")
  }
  if (!any(sapply(data, is.numeric))) {
    stop("Data must contain at least one numeric column")
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  
  # Check key parameters
  if (!is.numeric(num_trees) || num_trees <= 0 || num_trees != as.integer(num_trees)) {
    stop("num_trees must be a positive integer")
  }
  if (!is.numeric(sample_size) || sample_size <= 0) {
    stop("sample_size must be a positive number")
  }
  if (sample_size > nrow(data)) {
    warning("sample_size is larger than data size, using full data size")
    sample_size <- nrow(data)
  }
  if (!is.numeric(max_depth) || max_depth <= 0 || max_depth != as.integer(max_depth)) {
    stop("max_depth must be a positive integer")
  }
  if (!is.null(mtry) && (mtry < 1 || mtry > ncol(data))) {
    stop("mtry must be between 1 and number of columns (", ncol(data), ")")
  }
  if (is.null(seed)) {
    set.seed(as.numeric(Sys.time()))
  } else {
    set.seed(seed)
  }
  nr <- nrow(data)
  sample_fraction <- sample_size / nr
  fake_feature <- sample.int(nrow(data))
  model <- ranger::ranger(
    x = data,
    y = fake_feature,
    num.trees = num_trees,
    sample.fraction = sample_fraction,
    max.depth = max_depth,
    mtry = mtry,
    num.threads = num.threads,
    seed = seed,
    min.node.size = 1L,
    num.random.splits = 1L,
    splitrule = "extratrees",
    replace = FALSE,
    ...
  )
  terminal_nodes_depth <- calculate_leaf_to_root_depth(model)
  terminal_node_matrix <- stats::predict(model,
    data,
    type = "terminalNodes",
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
      anomaly_score = computeAnomaly(average_depth, sample_size)
    )

  result <- list(
    model = model,
    scores = scores,
    sample_size = sample_size,
    max_depth = max_depth,
    seed = seed
  )
  class(result) <- c("isoForest")
  return(result)
}
