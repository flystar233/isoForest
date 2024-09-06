#' @title Calculate the depth from leaf to root per tree.
#' @description Depth of each terminal node of a single tree in a ranger model.
#' @param df the tree info of ranger return by function 'treeInfo'.
#' @return A dataframe with two columns: 'nodeID', 'depth'.
#' @examples
#' rf <- ranger::ranger(Species ~ ., data = iris)
#' calculate_depth_per_tree(ranger::treeInfo(rf, 1))
#' @export
calculate_depth_per_tree <- function(df) {
  # Store the nodeID of each leaf node and its distance to the root node
  leaf_depth <- data.frame(nodeID = integer(), depth = integer())

  # Iterate over each row of the data frame
  for (i in 1:nrow(df)) {
    # If it's a leaf node (terminal == TRUE)
    if (df$terminal[i]) {
      depth <- 0
      current_node <- i

      # Traverse upwards until the root node (nodeID == 0)
      while (df$nodeID[current_node] != 0) {
        # Find the parent node of the current node
        parent_node <- which(df$leftChild == df$nodeID[current_node] | df$rightChild == df$nodeID[current_node])

        # If no parent node is found, the data might be incorrect
        if (length(parent_node) == 0) {
          stop("Error: No parent node found for nodeID ", df$nodeID[current_node])
        }

        # Update the current node to the parent node and increase the distance
        current_node <- parent_node
        depth <- depth + 1
      }

      # Store the calculated distance and nodeID in the data frame
      leaf_depth <- rbind(leaf_depth, data.frame(nodeID = df$nodeID[i], depth = depth))
    }
  }
  return(leaf_depth)
}
#' @title Calculate the depth from leaf to root.
#' @description Depth of each terminal node of all trees in a ranger model is
#'   returned as a three column dataframe with column names: 'nodeID',
#'   'depth', 'treeID'. Note that root node has the node_id = 0.
#' @param model A ranger model
#' @return A dataframe with three columns: 'nodeID', 'depth', 'treeID'.
#' @examples
#' rf <- ranger::ranger(Species ~ ., data = iris, num.trees = 100)
#' result <- calculate_leaf_to_root_depth(rf)
#' head(result)
#' @export
calculate_leaf_to_root_depth <- function(model) {
  leaf_root_depth <- lapply(1:model$num.trees, function(x) {
    calculate_depth_per_tree(ranger::treeInfo(model, x))
  })
  leaf_root_depth_with_id <- lapply(1:length(leaf_root_depth), function(i) {
    dplyr::mutate(leaf_root_depth[[i]], treeID = i)
  })
  leaf_root_depth_result <- dplyr::bind_rows(leaf_root_depth_with_id)
  return(leaf_root_depth_result)
}
