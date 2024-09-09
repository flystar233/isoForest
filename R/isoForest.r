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
#' @param is_feature_contribution A logical value indicating whether to calculate feature contributions. Default is FALSE.
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
                      is_feature_contribution = FALSE,
                      ...) {
  # Initial check
  if (num_trees <= 0) {
    stop("The number of trees is at least 1")
  }
  if (sample_size <= 0) {
    stop("The sample size is at least 1")
  }
  if (max_depth <= 0) {
    stop("The max depth is at least 1")
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  if (is.null(seed)) {
    set.seed(as.numeric(Sys.time()))
  } else {
    set.seed(seed)
  }
  nr <- nrow(data)
  sample_fraction <- sample_size / nr
  fake_feature <- sample.int(nrow(data))
  feature_contributions = NULL

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
  tnm <- stats::predict(model,
    data,
    type = "terminalNodes",
    num.threads = num.threads,
    ...
  )[["predictions"]]
  tnm <- as.data.frame(tnm)
  colnames(tnm) <- as.character(seq_len(ncol(tnm)))
  tnm$id <- seq_len(nrow(tnm))
  tnm <- tidyr::pivot_longer(tnm, cols = -id, names_to = "treeID", values_to = "nodeID")
  tnm$treeID <- as.integer(tnm$treeID)
  tnm$nodeID <- as.integer(tnm$nodeID)
  obs_depth <- dplyr::inner_join(terminal_nodes_depth, tnm, by = c("treeID", "nodeID"))
  if(is_feature_contribution){
    split_data <- split(obs_depth, obs_depth$id)
    feature_contributions_sum <- lapply(split_data, function(data) calculate_feature_counts(model, data))
    feature_contributions_sum <- do.call(rbind, feature_contributions_sum) |> as.data.frame()
    row_sums <- rowSums(feature_contributions_sum)
    feature_contributions_percent <- as.data.frame(sapply(feature_contributions_sum, function(x) x / row_sums))
  }
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
    seed = seed,
    feature_contributions_percent = feature_contributions_percent
  )
  class(result) <- c("isoForest")
  return(result)
}
