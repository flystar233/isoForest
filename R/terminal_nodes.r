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
  leaf_nodes <- which(df$terminal)
  leaf_depth <- matrix(NA, nrow = length(leaf_nodes), ncol = 2)
  colnames(leaf_depth) <- c("nodeID", "depth")

  # Iterate over each row of the data frame
  for (i in seq_along(leaf_nodes)) {
    # If it's a leaf node (terminal == TRUE)
    if (df$terminal[leaf_nodes[i]]) {
      depth <- 0
      current_node <- leaf_nodes[i]

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

      # Store the calculated distance and nodeID in the matrix
      leaf_depth[i, "nodeID"] <- df$nodeID[leaf_nodes[i]]
      leaf_depth[i, "depth"] <- depth
    }
  }
  leaf_depth <- as.data.frame(leaf_depth)
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
  leaf_root_depth <- lapply(seq_len(model$num.trees), function(x) {
    calculate_depth_per_tree(ranger::treeInfo(model, x))
  })
  leaf_root_depth_with_id <- lapply(seq_along(leaf_root_depth), function(i) {
    dplyr::mutate(leaf_root_depth[[i]], treeID = i)
  })
  leaf_root_depth_result <- dplyr::bind_rows(leaf_root_depth_with_id)
  return(leaf_root_depth_result)
}

#' @title Calculate the number of times a feature is used in terminal nodes.
#' @description The number of times a feature is used in terminal nodes of all
#'   trees in a ranger model is returned as a dataframe with column names:
#'   'feature', 'count'.
#' @param model A ranger model
#' @param obs_depth A dataframe with columns: 'nodeID', 'treeID'.
#' @return A dataframe with two columns: 'feature', 'count'.
#' @examples
#' 
#' result <- isoForest(iris,feature_contribution = TRUE)
#' head(result$feature_contributions)
calculate_feature_counts <- function(model, obs_depth) {
    
    # Get all feature names
    feature_names <- model$forest$independent.variable.names
    
    # Initialize feature counts
    total_feature_counts <- setNames(rep(0, length(feature_names)), feature_names)
    
    # Process each row in the result dataframe
    for (i in 1:nrow(obs_depth)) {
        nodeID <- obs_depth$nodeID[i]
        treeID <- obs_depth$treeID[i]
        
        # Get information for the current tree
        current_tree <- ranger::treeInfo(model, treeID)
        
        # Traverse from leaf node to root node (including root)
        current_node <- nodeID
        while (TRUE) {
            # Get information for the current node
            node_info <- current_tree[current_tree$nodeID == current_node, ]
            
            # Count the feature used (including root node)
            if (!is.na(node_info$splitvarName) && node_info$splitvarName != "") {
                total_feature_counts[node_info$splitvarName] <- total_feature_counts[node_info$splitvarName] + 1
            }
            
            # Exit loop if root node is reached
            if (current_node == 0) {
                break
            }
            
            # Find the parent node
            parent_node <- current_tree$nodeID[current_tree$leftChild == current_node | current_tree$rightChild == current_node]
            
            # Move to the parent node
            current_node <- parent_node[!is.na(parent_node)]
        }
    }
    
    # Return the feature count results
    return(total_feature_counts)
}
