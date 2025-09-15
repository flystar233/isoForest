#' @title Calculate feature contributions to anomaly scores
#' @description
#' This function calculates how much each feature contributes to the anomaly score
#' of specific samples using path-based analysis or permutation importance.
#' @param object An isoForest model object
#' @param sample_ids Vector of sample IDs to analyze (if NULL, analyzes all anomalies)
#' @param data The original training data
#' @param method Method for calculating contributions: "path" (default) or "permutation"
#' @param contamination Contamination rate for identifying anomalies (default 0.05)
#' @param n_permutations Number of permutations for permutation importance (default 30)
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
#' # Analyze specific samples with permutation method
#' contributions <- feature_contribution(model, sample_ids = c(1, 50), 
#'                                     data = iris[1:4], method = "permutation")
#' @export
feature_contribution <- function(object, 
                                sample_ids = NULL,
                                data,
                                method = "path",
                                contamination = 0.05,
                                n_permutations = 30,
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
  
  # Calculate contributions efficiently
  results <- if (method == "path") {
    calculate_path_contributions_batch(object, sample_ids, data, feature_names, max_trees)
  } else {
    calculate_permutation_contributions_batch(object, sample_ids, data, feature_names, n_permutations)
  }
  
  # Add simple summary for multiple samples
  if (length(sample_ids) > 1) {
    contrib_matrix <- do.call(rbind, lapply(results, function(x) x$contributions))
    results$summary <- data.frame(
      feature = feature_names,
      mean_contribution = colMeans(contrib_matrix),
      stringsAsFactors = FALSE
    )[order(colMeans(contrib_matrix), decreasing = TRUE), ]
    rownames(results$summary) <- NULL
  }
  
  class(results) <- "feature_contribution"
  return(results)
}

#' Batch calculate path-based contributions (optimized)
calculate_path_contributions_batch <- function(object, sample_ids, data, feature_names, max_trees) {
  
  # Pre-calculate terminal nodes for all samples at once
  sample_data <- data[sample_ids, , drop = FALSE]
  tnm <- stats::predict(object$model, sample_data, type = "terminalNodes")$predictions
  
  # Limit trees for performance
  n_trees <- min(object$model$num.trees, max_trees)
  
  # Pre-calculate tree structures
  tree_infos <- lapply(seq_len(n_trees), function(i) {
    tryCatch(ranger::treeInfo(object$model, i), error = function(e) NULL)
  })
  
  results <- list()
  
  for (i in seq_along(sample_ids)) {
    id <- sample_ids[i]
    feature_counts <- setNames(rep(0, length(feature_names)), feature_names)
    total_splits <- 0
    
    for (tree_id in seq_len(n_trees)) {
      tree_info <- tree_infos[[tree_id]]
      if (is.null(tree_info) || nrow(tree_info) == 0) next
      
      node_id <- tnm[i, tree_id]
      if (is.na(node_id) || node_id < 0) next
      
      # Efficient path traversal using vectorized operations
      current_node <- node_id
      for (depth in 1:20) {  # Reasonable depth limit
        parent_idx <- which(tree_info$leftChild == current_node | tree_info$rightChild == current_node)
        if (length(parent_idx) == 0) break
        
        split_var <- tree_info$splitvarName[parent_idx[1]]
        if (!is.na(split_var) && split_var %in% feature_names) {
          feature_counts[split_var] <- feature_counts[split_var] + 1
          total_splits <- total_splits + 1
        }
        
        current_node <- tree_info$nodeID[parent_idx[1]]
        if (is.na(current_node) || current_node == 0) break
      }
    }
    
    # Calculate contributions with simple fallback
    if (total_splits > 0) {
      contributions <- feature_counts / total_splits
    } else {
      # Simple extremeness-based fallback
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

#' Batch calculate permutation importance (optimized)
calculate_permutation_contributions_batch <- function(object, sample_ids, data, feature_names, n_permutations) {
  
  results <- list()
  
  for (i in seq_along(sample_ids)) {
    id <- sample_ids[i]
    original_score <- object$scores$anomaly_score[id]
    importance_scores <- setNames(rep(0, length(feature_names)), feature_names)
    
    # Create base permuted data once
    sample_row <- data[id, , drop = FALSE]
    
    for (feature in feature_names) {
      if (!feature %in% colnames(data)) next
      
      # Efficient permutation without full data copying
      permuted_data <- sample_row[rep(1, n_permutations), , drop = FALSE]
      permuted_data[, feature] <- sample(data[[feature]], n_permutations, replace = TRUE)
      
      # Calculate importance
      permuted_scores <- predict(object, permuted_data)$anomaly_score
      importance_scores[feature] <- abs(original_score - mean(permuted_scores))
    }
    
    # Normalize
    total_importance <- sum(importance_scores)
    if (total_importance > 0) {
      importance_scores <- importance_scores / total_importance
    } else {
      importance_scores[] <- 1 / length(feature_names)  # Equal weights
    }
    
    results[[paste0("sample_", id)]] <- list(
      sample_id = id,
      score = original_score,
      contributions = importance_scores
    )
  }
  
  return(results)
}

#' Simple extremeness calculation (optimized fallback)
calculate_extremeness_simple <- function(data, sample_id, feature_names) {
  
  weights <- setNames(rep(0, length(feature_names)), feature_names)
  sample_values <- data[sample_id, feature_names, drop = FALSE]
  
  # Vectorized percentile calculation
  for (feature in feature_names) {
    if (feature %in% colnames(data)) {
      percentile <- mean(data[[feature]] <= sample_values[[feature]], na.rm = TRUE)
      weights[feature] <- 2 * abs(percentile - 0.5)
    }
  }
  
  # Normalize
  total_weight <- sum(weights)
  if (total_weight > 0) {
    weights <- weights / total_weight
  } else {
    weights[] <- 1 / length(feature_names)
  }
  
  return(weights)
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
