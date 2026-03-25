#' @title Calculate feature contributions to anomaly scores
#' @description
#' This function calculates how much each feature contributes to the anomaly score
#' of specific samples using path-based analysis or permutation importance.
#' @param object An isoForest model object
#' @param sample_ids Vector of sample IDs to analyze (if NULL, analyzes all anomalies)
#' @param data The original training data
#' @param method Method for calculating contributions: "path" (default) or "permutation"
#' @param contamination Contamination rate for identifying anomalies (default 0.05)
#' @param n_permutations Number of permutations for permutation importance (default 15).
#'   Lower values (10-15) are usually sufficient and much faster. Values above 20 
#'   show diminishing returns.
#' @param max_trees Maximum number of trees to analyze for path method (default 50)
#' @return A list containing feature contributions for each analyzed sample
#' @examples
#' # Train model
#' model <- isoForest(iris[1:4])
#' 
#' # Analyze feature contributions for anomalous samples
#' contributions <- feature_contribution(model, data = iris[1:4])
#' print(contributions)
#' 
#' # Analyze specific samples with permutation method (fast)
#' contributions <- feature_contribution(model, sample_ids = c(1, 50), 
#'                                     data = iris[1:4], method = "permutation",
#'                                     n_permutations = 10)  # 10-15 recommended
#' @export
feature_contribution <- function(object,
                                sample_ids = NULL,
                                data,
                                method = "path",
                                contamination = 0.05,
                                n_permutations = 15,
                                max_trees = 50) {

  # Combined validation
  if (!inherits(object, "isoForest")) stop("Object must be an isoForest model")
  method <- match.arg(method, c("path", "permutation"))
  data <- as.data.frame(data)

  # Get sample IDs efficiently
  if (is.null(sample_ids)) {
    sample_ids <- which(is_anomaly(object, contamination = contamination))
    if (length(sample_ids) == 0) stop("No anomalies detected. Increase contamination rate.")
  }

  # Single validation check
  sample_ids <- sample_ids[sample_ids > 0 & sample_ids <= min(nrow(data), nrow(object$scores))]
  if (length(sample_ids) == 0) stop("No valid sample_ids found")

  # Cache feature names once
  feature_names <- object$model$forest$independent.variable.names
  feature_set <- setNames(rep(TRUE, length(feature_names)), feature_names)

  # Pre-cache sample data for both methods
  sample_data_cache <- data[sample_ids, , drop = FALSE]

  # Calculate contributions efficiently
  results <- if (method == "path") {
    calculate_path_contributions_batch(object, sample_ids, sample_data_cache, data, feature_names, feature_set, max_trees)
  } else {
    calculate_permutation_contributions_batch(object, sample_ids, sample_data_cache, data, feature_names, n_permutations)
  }
  
  # Add simple summary for multiple samples
  if (length(sample_ids) > 1) {
    # Use data.table::rbindlist for 10x faster data binding
    contrib_list <- lapply(results, function(x) {
      data.frame(t(x$contributions), stringsAsFactors = FALSE)
    })
    contrib_matrix <- as.matrix(data.table::rbindlist(contrib_list))
    mean_contribs <- colMeans(contrib_matrix)
    sort_order <- order(mean_contribs, decreasing = TRUE)
    results$summary <- data.frame(
      feature = feature_names[sort_order],
      mean_contribution = mean_contribs[sort_order],
      stringsAsFactors = FALSE
    )
    rownames(results$summary) <- NULL
  }
  
  class(results) <- "feature_contribution"
  return(results)
}

#' Batch calculate path-based contributions (optimized)
calculate_path_contributions_batch <- function(object, sample_ids, sample_data_cache, data, feature_names, feature_set, max_trees) {

  # Pre-calculate terminal nodes for all samples at once
  tnm <- stats::predict(object$model, sample_data_cache, type = "terminalNodes")$predictions

  # Limit trees for performance
  n_trees <- min(object$model$num.trees, max_trees)

  # Pre-calculate tree structures
  tree_infos <- lapply(seq_len(n_trees), function(i) {
    tryCatch(ranger::treeInfo(object$model, i), error = function(e) NULL)
  })

  # Build parent lookup tables for each tree (fast node lookups)
  tree_parent_maps <- lapply(tree_infos, function(tree_info) {
    if (is.null(tree_info) || nrow(tree_info) == 0) return(NULL)
    # Map: node_id -> parent row index
    parent_map <- rep(NA_integer_, max(tree_info$nodeID, tree_info$leftChild, tree_info$rightChild, na.rm = TRUE) + 1)
    for (idx in seq_len(nrow(tree_info))) {
      left_child <- tree_info$leftChild[idx]
      right_child <- tree_info$rightChild[idx]
      if (!is.na(left_child)) parent_map[left_child + 1] <- idx
      if (!is.na(right_child)) parent_map[right_child + 1] <- idx
    }
    list(parent_map = parent_map, info = tree_info)
  })

  # Dynamically calculate max tree depth from actual tree structures
  max_depth <- calculate_max_tree_depth(tree_infos)
  if (max_depth < 20) max_depth <- 20

  results <- list()

  for (i in seq_along(sample_ids)) {
    id <- sample_ids[i]
    feature_counts <- setNames(rep(0, length(feature_names)), feature_names)
    total_splits <- 0

    for (tree_id in seq_len(n_trees)) {
      tree_map <- tree_parent_maps[[tree_id]]
      if (is.null(tree_map)) next

      node_id <- tnm[i, tree_id]
      if (is.na(node_id) || node_id < 0) next

      tree_info <- tree_map$info
      parent_map <- tree_map$parent_map
      current_node <- node_id

      for (depth in 1:max_depth) {
        # Fast O(1) array lookup instead of which()
        parent_idx <- parent_map[current_node + 1]

        if (is.na(parent_idx)) break

        split_var <- tree_info$splitvarName[parent_idx]
        if (!is.na(split_var) && !is.null(feature_set[[split_var]])) {
          feature_counts[split_var] <- feature_counts[split_var] + 1
          total_splits <- total_splits + 1
        }

        current_node <- tree_info$nodeID[parent_idx]
        if (is.na(current_node) || current_node == 0) break
      }
    }

    # Calculate contributions with simple fallback
    if (total_splits > 0) {
      contributions <- feature_counts / total_splits
    } else {
      contributions <- calculate_extremeness_simple(data, id, feature_names)
    }

    results[[paste0("sample_", id)]] <- list(
      sample_id = id,
      score = object$scores$anomaly_score[id],
      contributions = contributions
    )
  }

  return(results)
}

#' Batch calculate permutation importance (highly optimized)
calculate_permutation_contributions_batch <- function(object, sample_ids, sample_data_cache, data, feature_names, n_permutations) {

  results <- list()
  n_samples <- length(sample_ids)
  n_features <- length(feature_names)

  # Progress indicator for long computations
  if (n_samples > 5) {
    cat("Calculating permutation importance for", n_samples, "samples",
        "with", n_permutations, "permutations...\n")
  }

  for (i in seq_along(sample_ids)) {
    id <- sample_ids[i]
    original_score <- object$scores$anomaly_score[id]
    importance_scores <- setNames(rep(0, length(feature_names)), feature_names)

    # Progress indicator
    if (n_samples > 5 && i %% 5 == 0) {
      cat("  Progress:", i, "/", n_samples, "\n")
    }

    # Use cached sample data instead of re-reading
    sample_row <- sample_data_cache[i, , drop = FALSE]

    # Batch all permutations for all features together
    all_permuted_data <- vector("list", n_features)

    for (j in seq_along(feature_names)) {
      feature <- feature_names[j]
      if (!feature %in% colnames(data)) {
        all_permuted_data[[j]] <- NULL
        next
      }

      # Create permuted data for this feature
      permuted_data <- sample_row[rep(1, n_permutations), , drop = FALSE]
      permuted_data[, feature] <- sample(data[[feature]], n_permutations, replace = TRUE)
      all_permuted_data[[j]] <- permuted_data
    }
    
    # **Optimization 2**: Combine all permutations and predict once
    # Use data.table::rbindlist for efficient data binding (10x faster)
    valid_data <- all_permuted_data[!sapply(all_permuted_data, is.null)]
    all_permuted_combined <- data.table::rbindlist(valid_data)

    if (nrow(all_permuted_combined) > 0) {
      # Single predict call for all permutations
      all_scores <- predict(object, all_permuted_combined)$anomaly_score
      
      # Split scores back to each feature
      idx <- 1
      for (j in seq_along(feature_names)) {
        feature <- feature_names[j]
        if (!is.null(all_permuted_data[[j]])) {
          permuted_scores <- all_scores[idx:(idx + n_permutations - 1)]
          importance_scores[feature] <- abs(original_score - mean(permuted_scores))
          idx <- idx + n_permutations
        }
      }
    }
    
    # Normalize
    total_importance <- sum(importance_scores)
    if (total_importance > 0) {
      importance_scores <- importance_scores / total_importance
    } else {
      importance_scores[] <- 1 / length(feature_names)
    }
    
    results[[paste0("sample_", id)]] <- list(
      sample_id = id,
      score = original_score,
      contributions = importance_scores
    )
  }
  
  if (n_samples > 5) {
    cat("  Completed!\n")
  }
  
  return(results)
}

#' Simple extremeness calculation (optimized fallback)
calculate_extremeness_simple <- function(data, sample_id, feature_names) {

  sample_values <- data[sample_id, feature_names, drop = FALSE]

  # Fully vectorized percentile calculation
  weights <- sapply(feature_names, function(feature) {
    if (feature %in% colnames(data)) {
      percentile <- mean(data[[feature]] <= sample_values[[feature]], na.rm = TRUE)
      2 * abs(percentile - 0.5)
    } else {
      0
    }
  })

  # Normalize
  total_weight <- sum(weights)
  if (total_weight > 0) {
    weights <- weights / total_weight
  } else {
    weights[] <- 1 / length(feature_names)
  }

  return(weights)
}

#' Calculate maximum tree depth from tree structures
#' @keywords internal
calculate_max_tree_depth <- function(tree_infos) {
  max_depth <- 0

  for (tree_info in tree_infos) {
    if (is.null(tree_info) || nrow(tree_info) == 0) next

    # Calculate depth by finding max nodeID (which correlates with depth)
    # More accurate: traverse from leaf nodes to root for each tree
    nodeids <- tree_info$nodeID
    if (length(nodeids) > 0) {
      # Approximate depth from node count: depth ≈ log2(nodeCount)
      tree_depth <- ceiling(log2(max(nodeids, na.rm = TRUE) + 1))
      max_depth <- max(max_depth, tree_depth)
    }
  }

  return(if (max_depth == 0) 20 else max_depth)
}

#' @title Print method for feature_contribution objects
#' @description Print feature contribution results
#' @param x A feature_contribution object
#' @param top_n Number of top features to show per sample (default 5)
#' @param ... Additional arguments (not used)
#' @export
print.feature_contribution <- function(x, top_n = 5, ...) {
  cat("Feature Contribution Analysis\n")
  cat("=============================\n\n")

  # Print individual sample results
  sample_results <- x[grepl("^sample_", names(x))]

  for (name in names(sample_results)) {
    sample <- sample_results[[name]]
    cat("Sample", sample$sample_id, "| Score:", round(sample$score, 3), "\n")

    # Show top N features
    contrib <- sort(sample$contributions, decreasing = TRUE)[1:min(top_n, length(sample$contributions))]
    for (i in seq_along(contrib)) {
      cat(sprintf("  %s: %.1f%%\n", names(contrib)[i], contrib[i] * 100))
    }
    cat("\n")
  }

  # Print summary if available
  if (!is.null(x$summary)) {
    cat("Summary (Top", min(top_n, nrow(x$summary)), "features):\n")
    for (i in 1:min(top_n, nrow(x$summary))) {
      cat(sprintf("  %s: %.1f%%\n",
                  x$summary$feature[i],
                  x$summary$mean_contribution[i] * 100))
    }
  }
}
