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

#' Batch calculate permutation importance (highly optimized)
calculate_permutation_contributions_batch <- function(object, sample_ids, data, feature_names, n_permutations) {
  
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
    
    # Create base sample once
    sample_row <- data[id, , drop = FALSE]
    
    # **Optimization 1**: Batch all permutations for all features together
    # Instead of calling predict n_features * n_permutations times,
    # we create one large matrix and call predict once
    
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
    all_permuted_combined <- do.call(rbind, all_permuted_data[!sapply(all_permuted_data, is.null)])
    
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

#' @title Plot feature boxplot with all data points and anomaly highlighted
#' @description
#' Create boxplots for features with their contributions to anomaly scores.
#' All data points are shown as small black dots, with the anomalous point
#' highlighted in red (or custom color) for easy identification.
#' @param contribution_obj A feature_contribution object from feature_contribution()
#' @param data The original training data
#' @param sample_id The sample ID to visualize (if NULL, uses the first sample in contribution_obj)
#' @param top_n Number of top contributing features to display (default 5, NULL for all)
#' @param highlight_color Color for the anomaly point (default "red")
#' @param highlight_size Size of the anomaly point (default 3)
#' @param highlight_shape Shape of the anomaly point (default 16 = filled circle, not used currently)
#' @param show_contribution Whether to show contribution percentages in title (default TRUE)
#' @return A ggplot object
#' @examples
#' # Train model and calculate contributions
#' model <- isoForest(iris[1:4])
#' contributions <- feature_contribution(model, sample_ids = 42, data = iris[1:4])
#' 
#' # Plot boxplots with all data points and anomaly highlighted
#' plot_feature_boxplot(contributions, iris[1:4], sample_id = 42)
#' 
#' # Show all features
#' plot_feature_boxplot(contributions, iris[1:4], sample_id = 42, top_n = NULL)
#' @export
plot_feature_boxplot <- function(contribution_obj,
                                 data,
                                 sample_id = NULL,
                                 top_n = 5,
                                 highlight_color = "red",
                                 highlight_size = 3,
                                 highlight_shape = 17,
                                 show_contribution = TRUE) {
  
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function. Please install it with: install.packages('ggplot2')")
  }
  
  # Validate inputs
  if (!inherits(contribution_obj, "feature_contribution")) {
    stop("contribution_obj must be a feature_contribution object")
  }
  
  data <- as.data.frame(data)
  
  # Get sample results
  sample_results <- contribution_obj[grepl("^sample_", names(contribution_obj))]
  if (length(sample_results) == 0) {
    stop("No sample results found in contribution object")
  }
  
  # Determine which sample to plot
  if (is.null(sample_id)) {
    sample_result <- sample_results[[1]]
    sample_id <- sample_result$sample_id
    message("Using sample_id: ", sample_id)
  } else {
    sample_key <- paste0("sample_", sample_id)
    if (!sample_key %in% names(sample_results)) {
      stop("Sample ID ", sample_id, " not found in contribution object")
    }
    sample_result <- sample_results[[sample_key]]
  }
  
  # Validate sample_id
  if (sample_id < 1 || sample_id > nrow(data)) {
    stop("sample_id out of range")
  }
  
  # Get contributions and sort
  contributions <- sample_result$contributions
  contributions_sorted <- sort(contributions, decreasing = TRUE)
  
  # Select features to display
  if (!is.null(top_n)) {
    top_n <- min(top_n, length(contributions_sorted))
    features_to_plot <- names(contributions_sorted)[1:top_n]
  } else {
    features_to_plot <- names(contributions_sorted)
  }
  
  # Check if features exist in data
  features_to_plot <- features_to_plot[features_to_plot %in% colnames(data)]
  if (length(features_to_plot) == 0) {
    stop("No valid features found in data")
  }
  
  # Prepare data for plotting
  plot_data_list <- lapply(features_to_plot, function(feat) {
    values <- unname(data[[feat]])  # Remove row names to avoid warning
    anomaly_value <- data[sample_id, feat]
    contribution_pct <- contributions[feat] * 100
    
    # Mark which points are anomalies
    is_anomaly_point <- seq_along(values) == sample_id
    
    data.frame(
      feature = feat,
      value = values,
      contribution = contribution_pct,
      is_anomaly = is_anomaly_point,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  
  plot_data <- do.call(rbind, plot_data_list)
  rownames(plot_data) <- NULL  # Remove row names from combined data
  
  # Create feature labels with contributions if requested
  if (show_contribution) {
    feature_labels <- sapply(features_to_plot, function(f) {
      sprintf("%s\n(%.1f%%)", f, contributions[f] * 100)
    })
    plot_data$feature <- factor(plot_data$feature, 
                                levels = features_to_plot,
                                labels = feature_labels)
  } else {
    plot_data$feature <- factor(plot_data$feature, levels = features_to_plot)
  }
  
  # Separate data for normal and anomaly points
  normal_points <- plot_data[!plot_data$is_anomaly, ]
  anomaly_points <- plot_data[plot_data$is_anomaly, ]
  
  # Create plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = feature, y = value)) +
    ggplot2::geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_point(data = normal_points,
                       ggplot2::aes(x = feature, y = value),
                       color = "black",
                       size = 1,
                       alpha = 0.4,
                       position = ggplot2::position_jitter(width = 0.2, height = 0)) +
    ggplot2::geom_point(data = anomaly_points, 
                       ggplot2::aes(x = feature, y = value),
                       color = highlight_color,
                       size = highlight_size,
                       shape = 16) +
    ggplot2::labs(
      title = sprintf("Feature Distribution with Anomaly Point (Sample #%d)", sample_id),
      subtitle = sprintf("Anomaly Score: %.3f | Red dot indicates anomalous value", 
                        sample_result$score),
      x = if(show_contribution) "Feature (Contribution %)" else "Feature",
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  return(p)
}

#' @title Plot multiple feature boxplots (faceted) with all data points
#' @description
#' Create faceted boxplots for better comparison when many features are involved.
#' All data points are shown as small black dots, with the anomalous point
#' highlighted in red for easy identification. This is useful when top_n is 
#' large or you want to see all features.
#' @param contribution_obj A feature_contribution object from feature_contribution()
#' @param data The original training data
#' @param sample_id The sample ID to visualize (if NULL, uses the first sample)
#' @param top_n Number of top contributing features to display (default 8, NULL for all)
#' @param ncol Number of columns in facet grid (default 2)
#' @param highlight_color Color for the anomaly point (default "red")
#' @param scales Should scales be fixed ("fixed") or free ("free", "free_y")? Default "free_y"
#' @return A ggplot object
#' @examples
#' model <- isoForest(iris[1:4])
#' contributions <- feature_contribution(model, sample_ids = 42, data = iris[1:4])
#' 
#' # Faceted view with all data points
#' plot_feature_boxplot_faceted(contributions, iris[1:4], sample_id = 42)
#' @export
plot_feature_boxplot_faceted <- function(contribution_obj,
                                        data,
                                        sample_id = NULL,
                                        top_n = 8,
                                        ncol = 2,
                                        highlight_color = "red",
                                        scales = "free_y") {
  
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it with: install.packages('ggplot2')")
  }
  
  # Validate inputs
  if (!inherits(contribution_obj, "feature_contribution")) {
    stop("contribution_obj must be a feature_contribution object")
  }
  
  data <- as.data.frame(data)
  
  # Get sample results
  sample_results <- contribution_obj[grepl("^sample_", names(contribution_obj))]
  if (length(sample_results) == 0) {
    stop("No sample results found in contribution object")
  }
  
  # Determine which sample to plot
  if (is.null(sample_id)) {
    sample_result <- sample_results[[1]]
    sample_id <- sample_result$sample_id
    message("Using sample_id: ", sample_id)
  } else {
    sample_key <- paste0("sample_", sample_id)
    if (!sample_key %in% names(sample_results)) {
      stop("Sample ID ", sample_id, " not found in contribution object")
    }
    sample_result <- sample_results[[sample_key]]
  }
  
  # Validate sample_id
  if (sample_id < 1 || sample_id > nrow(data)) {
    stop("sample_id out of range")
  }
  
  # Get contributions and sort
  contributions <- sample_result$contributions
  contributions_sorted <- sort(contributions, decreasing = TRUE)
  
  # Select features to display
  if (!is.null(top_n)) {
    top_n <- min(top_n, length(contributions_sorted))
    features_to_plot <- names(contributions_sorted)[1:top_n]
  } else {
    features_to_plot <- names(contributions_sorted)
  }
  
  # Check if features exist in data
  features_to_plot <- features_to_plot[features_to_plot %in% colnames(data)]
  if (length(features_to_plot) == 0) {
    stop("No valid features found in data")
  }
  
  # Prepare data for plotting - include all data points with anomaly marking
  plot_data_list <- lapply(features_to_plot, function(feat) {
    values <- unname(data[[feat]])
    # Mark which points are anomalies
    is_anomaly_point <- seq_along(values) == sample_id
    
    data.frame(
      feature = sprintf("%s (%.1f%%)", feat, contributions[feat] * 100),
      value = values,
      is_anomaly = is_anomaly_point,
      stringsAsFactors = FALSE,
      row.names = NULL
    )
  })
  
  plot_data <- do.call(rbind, plot_data_list)
  rownames(plot_data) <- NULL  # Remove row names from combined data
  
  # Order features by contribution
  feature_order <- sprintf("%s (%.1f%%)", features_to_plot, 
                          sapply(features_to_plot, function(f) contributions[f] * 100))
  feature_order <- feature_order[order(contributions[features_to_plot], decreasing = TRUE)]
  plot_data$feature <- factor(plot_data$feature, levels = feature_order)
  
  # Separate normal and anomaly points
  normal_points <- plot_data[!plot_data$is_anomaly, ]
  anomaly_points <- plot_data[plot_data$is_anomaly, ]
  
  # Create faceted plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = "", y = value)) +
    ggplot2::geom_boxplot(fill = "lightblue", alpha = 0.7, outlier.shape = NA) +
    ggplot2::geom_point(data = normal_points,
                       color = "black", 
                       size = 1, 
                       alpha = 0.4,
                       position = ggplot2::position_jitter(width = 0.2, height = 0)) +
    ggplot2::geom_point(data = anomaly_points,
                       color = highlight_color,
                       size = 3,
                       shape = 16) +
    ggplot2::facet_wrap(~ feature, scales = scales, ncol = ncol) +
    ggplot2::labs(
      title = sprintf("Feature Distributions with Anomaly Point (Sample #%d)", sample_id),
      subtitle = sprintf("Anomaly Score: %.3f | Red dot indicates anomalous value", 
                        sample_result$score),
      x = NULL,
      y = "Value"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      axis.text.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      panel.grid.major.x = ggplot2::element_blank()
    )
  
  return(p)
}