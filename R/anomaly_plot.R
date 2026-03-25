#' @title Plot basic anomaly visualization for 2D data
#' @description Plots the anomaly score heatmap or scatter plot for 2D data.
#' This function is specifically designed for 2-dimensional data visualization.
#' @param object The fitted isoForest model object
#' @param data The data to be plotted (must have at least 2 columns).
#' @param plot_type The type of plot to be generated. Either "heatmap" or "scatter".
#' @param contamination The proportion of outliers in the data. Default is 0.05. This parameter is used to determine the threshold for anomaly scores.
#' @return A ggplot object.
#' @examples
#' # Load the data
#' data("iris")
#' # Fit the anomaly detection model
#' model <- isoForest(iris[1:2])
#' # Plot the anomaly scores
#' plot_anomaly_basic(model, iris[1:2], plot_type = "scatter")
#' plot_anomaly_basic(model, iris[1:2], plot_type = "heatmap")
#' @export
plot_anomaly_basic <- function(object,
                         data = NULL,
                         plot_type = c("heatmap", "scatter"),
                         contamination = 0.05) {
  # Enhanced parameter validation
  if (is.null(object) || !inherits(object, "isoForest")) {
    stop("object must be an isoForest model")
  }
  if (is.null(data) || nrow(data) == 0) {
    stop("data cannot be NULL or empty")
  }
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  if (ncol(data) < 2) {
    stop("data must have at least 2 columns for plotting")
  }
  if (!is.numeric(contamination) || contamination <= 0 || contamination >= 1) {
    stop("contamination must be a number between 0 and 1")
  }
  
  plot_type <- match.arg(plot_type)
  column_names <- colnames(data)
  if (plot_type == "scatter") {
    # Define plot constants
    normal_color <- "#1661ab"
    anomaly_color <- "#ef475d"
    normal_size <- 3
    anomaly_size <- 4.5
    normal_shape <- 19
    anomaly_shape <- 17
    
    # Calculate anomaly threshold
    scores <- object$scores$anomaly_score
    anomaly_threshold <- quantile(scores, (1 - contamination), na.rm = TRUE)
    anomaly_labels <- as.factor(scores > anomaly_threshold)
    levels(anomaly_labels) <- c("Normal", "Anomaly")
    
    # Prepare plot data
    plot_data <- data.frame(
      x = data[[1]],
      y = data[[2]], 
      type = anomaly_labels
    )
    
    ggplot2::ggplot(plot_data, ggplot2::aes(x = x, y = y, color = type, shape = type)) +
      ggplot2::geom_point(size = ifelse(plot_data$type == "Normal", normal_size, anomaly_size)) +
      ggplot2::scale_colour_manual(name = "Type", values = c(normal_color, anomaly_color)) +
      ggplot2::scale_shape_manual(name = "Type", values = c(normal_shape, anomaly_shape)) +
      ggplot2::xlab(column_names[1]) +
      ggplot2::ylab(column_names[2]) +
      ggplot2::theme_bw()
  } else if (plot_type == "heatmap") {
    # Define heatmap constants
    grid_resolution <- 0.1
    padding <- 1
    n_colors <- 50
    point_color <- "#0000801A"
    
    # Calculate data boundaries
    data_ranges <- apply(data[1:2], 2, range, na.rm = TRUE)
    x_range <- c(data_ranges[1, 1] - padding, data_ranges[2, 1] + padding)
    y_range <- c(data_ranges[1, 2] - padding, data_ranges[2, 2] + padding)
    
    # Create grid for heatmap (optimized)
    x_seq <- seq(x_range[1], x_range[2], by = grid_resolution)
    y_seq <- seq(y_range[1], y_range[2], by = grid_resolution)
    
    # Limit grid size for performance
    max_grid_size <- 10000  # Maximum number of grid points
    if (length(x_seq) * length(y_seq) > max_grid_size) {
      warning("Grid too large for heatmap, reducing resolution")
      n_points <- ceiling(sqrt(max_grid_size))
      x_seq <- seq(x_range[1], x_range[2], length.out = n_points)
      y_seq <- seq(y_range[1], y_range[2], length.out = n_points)
    }
    
    # Generate prediction grid
    prediction_grid <- expand.grid(x = x_seq, y = y_seq)
    colnames(prediction_grid) <- column_names[1:2]
    
    # Calculate anomaly scores for grid
    grid_scores <- stats::predict(object, prediction_grid)
    heatmap_data <- data.frame(
      x = prediction_grid[[1]],
      y = prediction_grid[[2]],
      score = grid_scores$anomaly_score
    )
    
    # Create plot data for original points
    point_data <- data.frame(
      x = data[[1]],
      y = data[[2]]
    )
    
    # Create heatmap plot
    ggplot2::ggplot() +
      ggplot2::geom_tile(data = heatmap_data, ggplot2::aes(x = x, y = y, fill = score)) +
      ggplot2::scale_fill_gradientn(colors = rev(grDevices::heat.colors(n_colors))) +
      ggplot2::geom_point(data = point_data, ggplot2::aes(x = x, y = y), color = point_color) +
      ggplot2::xlab(column_names[1]) +
      ggplot2::ylab(column_names[2]) +
      ggplot2::labs(fill = "Anomaly Score") +
      ggplot2::theme_bw()
  } else {
    stop("plot_type must be either 'scatter' or 'heatmap'")
  }
}

#' @title Plot anomaly boxplot with feature distributions
#' @description
#' Create boxplots showing how anomalous points compare to the overall feature distributions.
#' All data points are shown as small black dots, with the anomalous point(s)
#' highlighted in red (or custom color) for easy identification.
#' Can work with or without a contribution object.
#' @param contribution_obj A feature_contribution object from feature_contribution(), or NULL.
#'   If NULL, you can specify multiple sample_ids to highlight multiple anomalies.
#' @param data The original training data
#' @param sample_id The sample ID(s) to visualize. Can be a single value or a vector.
#'   - If contribution_obj is provided: only single value is used
#'   - If contribution_obj is NULL: can be a vector to mark multiple anomalies
#'   - If NULL and contribution_obj provided: uses first sample from contribution_obj
#' @param top_n Number of top contributing features to display (default 5, NULL for all).
#'   Only used when contribution_obj is provided.
#' @param highlight_color Color for the anomaly point(s) (default "red")
#' @param highlight_size Size of the anomaly point(s) (default 3).
#'   Automatically adjusted to 1 when >5 anomalies are highlighted
#' @param highlight_shape Shape of the anomaly point(s) (default 17, not used currently)
#' @param show_contribution Whether to show contribution percentages in labels (default TRUE).
#'   Only applies when contribution_obj is provided.
#' @return A ggplot object
#' @examples
#' # With contribution object
#' model <- isoForest(iris[1:4])
#' contributions <- feature_contribution(model, sample_ids = 42, data = iris[1:4])
#' plot_anomaly_boxplot(contributions, iris[1:4], sample_id = 42)
#' 
#' # Without contribution object - mark multiple anomalies
#' anomaly_ids <- c(42, 107, 119)
#' plot_anomaly_boxplot(contribution_obj = NULL, data = iris[1:4], sample_id = anomaly_ids)
#' 
#' # Many anomalies (auto-adjust size)
#' many_ids <- c(42, 61, 99, 107, 119, 132, 135)
#' plot_anomaly_boxplot(NULL, iris[1:4], many_ids)
#' @export
plot_anomaly_boxplot <- function(contribution_obj = NULL,
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
  if (!is.null(contribution_obj) && !inherits(contribution_obj, "feature_contribution")) {
    stop("contribution_obj must be a feature_contribution object or NULL")
  }
  
  data <- as.data.frame(data)
  
  # Branch: With or without contribution_obj
  if (!is.null(contribution_obj)) {
    # ==== MODE 1: With contribution object ====
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
      if (length(sample_id) > 1) {
        warning("Multiple sample_ids provided, but only the first one will be used when contribution_obj is provided")
        sample_id <- sample_id[1]
      }
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
      values <- unname(data[[feat]])
      is_anomaly_point <- seq_along(values) == sample_id
      
      data.frame(
        feature = feat,
        value = values,
        contribution = contributions[feat] * 100,
        is_anomaly = is_anomaly_point,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
    
    # Use data.table::rbindlist for 10x faster data binding
    plot_data <- data.table::rbindlist(plot_data_list)
    rownames(plot_data) <- NULL
    
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
    
    # Title and subtitle
    plot_title <- sprintf("Feature Distribution with Anomaly Point (Sample #%d)", sample_id)
    plot_subtitle <- sprintf("Anomaly Score: %.3f | Red dot indicates anomalous value", 
                            sample_result$score)
    
  } else {
    # ==== MODE 2: Without contribution object ====
    if (is.null(sample_id)) {
      stop("sample_id must be provided when contribution_obj is NULL")
    }
    
    # Validate sample_id
    if (any(sample_id < 1) || any(sample_id > nrow(data))) {
      stop("sample_id out of range")
    }
    
    # Use all features
    features_to_plot <- colnames(data)
    
    # Auto-adjust highlight size if too many anomalies
    n_anomalies <- length(sample_id)
    if (n_anomalies > 5) {
      highlight_size <- 1  # Same as normal points
      message(sprintf("Detected %d anomalies (>5), adjusting point size to 1", n_anomalies))
    }
    
    # Prepare data for plotting
    plot_data_list <- lapply(features_to_plot, function(feat) {
      values <- unname(data[[feat]])
      is_anomaly_point <- seq_along(values) %in% sample_id
      
      data.frame(
        feature = feat,
        value = values,
        is_anomaly = is_anomaly_point,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
    
    # Use data.table::rbindlist for 10x faster data binding
    plot_data <- data.table::rbindlist(plot_data_list)
    rownames(plot_data) <- NULL
    plot_data$feature <- factor(plot_data$feature, levels = features_to_plot)
    
    # Title and subtitle
    if (n_anomalies == 1) {
      plot_title <- sprintf("Feature Distribution with Anomaly Point (Sample #%d)", sample_id)
      plot_subtitle <- sprintf("Red dot indicates anomalous value")
    } else if (n_anomalies <= 5) {
      plot_title <- sprintf("Feature Distribution with %d Anomaly Points", n_anomalies)
      plot_subtitle <- sprintf("Samples: %s | Red dots indicate anomalous values", 
                              paste(sample_id, collapse = ", "))
    } else {
      plot_title <- sprintf("Feature Distribution with %d Anomaly Points", n_anomalies)
      plot_subtitle <- sprintf("Red dots indicate anomalous values")
    }
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
                       shape = 16,
                       position = ggplot2::position_jitter(width = 0.2, height = 0)) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = if(!is.null(contribution_obj) && show_contribution) "Feature (Contribution %)" else "Feature",
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

#' @title Plot anomaly boxplot with faceted feature distributions
#' @description
#' Create faceted boxplots for better comparison when many features are involved.
#' All data points are shown as small black dots, with the anomalous point(s)
#' highlighted in red for easy identification. This is useful when top_n is 
#' large or you want to see all features. Can work with or without a contribution object.
#' @param contribution_obj A feature_contribution object from feature_contribution(), or NULL.
#'   If NULL, you can specify multiple sample_ids to highlight multiple anomalies.
#' @param data The original training data
#' @param sample_id The sample ID(s) to visualize. Can be a single value or a vector.
#'   - If contribution_obj is provided: only single value is used
#'   - If contribution_obj is NULL: can be a vector to mark multiple anomalies
#'   - If NULL and contribution_obj provided: uses first sample from contribution_obj
#' @param top_n Number of top contributing features to display (default 8, NULL for all)
#' @param ncol Number of columns in facet grid (default 2)
#' @param highlight_color Color for the anomaly point(s) (default "red")
#' @param highlight_size Size of the anomaly point(s) (default 3).
#'   Automatically adjusted to 1 when >5 anomalies are highlighted
#' @param scales Should scales be fixed ("fixed") or free ("free", "free_y")? Default "free_y"
#' @return A ggplot object
#' @examples
#' # With contribution object
#' model <- isoForest(iris[1:4])
#' contributions <- feature_contribution(model, sample_ids = 42, data = iris[1:4])
#' plot_anomaly_boxplot_faceted(contributions, iris[1:4], sample_id = 42)
#' 
#' # Without contribution object - mark multiple anomalies
#' anomaly_ids <- c(42, 107, 119, 132, 135)
#' plot_anomaly_boxplot_faceted(NULL, iris[1:4], sample_id = anomaly_ids)
#' @export
plot_anomaly_boxplot_faceted <- function(contribution_obj = NULL,
                                        data,
                                        sample_id = NULL,
                                        top_n = 8,
                                        ncol = 2,
                                        highlight_color = "red",
                                        highlight_size = 3,
                                        scales = "free_y") {
  
  # Check for ggplot2
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required. Please install it with: install.packages('ggplot2')")
  }
  
  # Validate inputs
  if (!is.null(contribution_obj) && !inherits(contribution_obj, "feature_contribution")) {
    stop("contribution_obj must be a feature_contribution object or NULL")
  }
  
  data <- as.data.frame(data)
  
  # Branch: With or without contribution_obj
  if (!is.null(contribution_obj)) {
    # ==== MODE 1: With contribution object ====
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
      if (length(sample_id) > 1) {
        warning("Multiple sample_ids provided, but only the first one will be used when contribution_obj is provided")
        sample_id <- sample_id[1]
      }
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
      values <- unname(data[[feat]])
      is_anomaly_point <- seq_along(values) == sample_id
      
      data.frame(
        feature = sprintf("%s (%.1f%%)", feat, contributions[feat] * 100),
        value = values,
        is_anomaly = is_anomaly_point,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
    
    # Use data.table::rbindlist for 10x faster data binding
    plot_data <- data.table::rbindlist(plot_data_list)
    rownames(plot_data) <- NULL
    
    # Order features by contribution
    feature_order <- sprintf("%s (%.1f%%)", features_to_plot, 
                            sapply(features_to_plot, function(f) contributions[f] * 100))
    feature_order <- feature_order[order(contributions[features_to_plot], decreasing = TRUE)]
    plot_data$feature <- factor(plot_data$feature, levels = feature_order)
    
    # Title and subtitle
    plot_title <- sprintf("Feature Distributions with Anomaly Point (Sample #%d)", sample_id)
    plot_subtitle <- sprintf("Anomaly Score: %.3f | Red dot indicates anomalous value", 
                            sample_result$score)
    
  } else {
    # ==== MODE 2: Without contribution object ====
    if (is.null(sample_id)) {
      stop("sample_id must be provided when contribution_obj is NULL")
    }
    
    # Validate sample_id
    if (any(sample_id < 1) || any(sample_id > nrow(data))) {
      stop("sample_id out of range")
    }
    
    # Use all features or top_n
    all_features <- colnames(data)
    if (!is.null(top_n)) {
      top_n <- min(top_n, length(all_features))
      features_to_plot <- all_features[1:top_n]
    } else {
      features_to_plot <- all_features
    }
    
    # Auto-adjust highlight size if too many anomalies
    n_anomalies <- length(sample_id)
    if (n_anomalies > 5) {
      highlight_size <- 1  # Same as normal points
      message(sprintf("Detected %d anomalies (>5), adjusting point size to 1", n_anomalies))
    }
    
    # Prepare data for plotting
    plot_data_list <- lapply(features_to_plot, function(feat) {
      values <- unname(data[[feat]])
      is_anomaly_point <- seq_along(values) %in% sample_id
      
      data.frame(
        feature = feat,
        value = values,
        is_anomaly = is_anomaly_point,
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    })
    
    # Use data.table::rbindlist for 10x faster data binding
    plot_data <- data.table::rbindlist(plot_data_list)
    rownames(plot_data) <- NULL
    plot_data$feature <- factor(plot_data$feature, levels = features_to_plot)
    
    # Title and subtitle
    if (n_anomalies == 1) {
      plot_title <- sprintf("Feature Distributions with Anomaly Point (Sample #%d)", sample_id)
      plot_subtitle <- sprintf("Red dot indicates anomalous value")
    } else if (n_anomalies <= 5) {
      plot_title <- sprintf("Feature Distributions with %d Anomaly Points", n_anomalies)
      plot_subtitle <- sprintf("Samples: %s | Red dots indicate anomalous values", 
                              paste(sample_id, collapse = ", "))
    } else {
      plot_title <- sprintf("Feature Distributions with %d Anomaly Points", n_anomalies)
      plot_subtitle <- sprintf("Red dots indicate anomalous values")
    }
  }
  
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
                       size = highlight_size,
                       shape = 16,
                       position = ggplot2::position_jitter(width = 0.2, height = 0)) +
    ggplot2::facet_wrap(~ feature, scales = scales, ncol = ncol) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
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

#' @title Plot anomaly projection using dimensionality reduction
#' @description
#' Create 2D visualization of anomalies using PCA or UMAP dimensionality reduction.
#' Anomalies are highlighted in red, normal points in blue.
#' For large datasets (>3000 points), automatic sampling is applied to improve speed.
#' 
#' @param model An isoForest model object
#' @param data The original data (must be numeric)
#' @param method Threshold method for anomaly detection (default: "mad")
#' @param dim_reduction Dimensionality reduction method: "pca" or "umap" (default: "pca")
#' @param contamination Contamination rate (only used if method = "contamination")
#' @param point_size Size of points in the plot (default: 2)
#' @param point_alpha Transparency of points (default: 0.6)
#' @param umap_n_neighbors Number of neighbors for UMAP (default: 15)
#' @param umap_min_dist Minimum distance for UMAP (default: 0.1)
#' @param sample_rate Target anomaly rate in the displayed data (default: 0.05). 
#'   The function will sample normal points so that anomalies represent this 
#'   proportion of total displayed points. Set to NULL to disable sampling.
#'   For example, if sample_rate = 0.05 and there are 100 anomalies, 
#'   the total displayed points will be approximately 2000 (100/0.05).
#' 
#' @return A ggplot2 object showing the 2D projection with anomalies in red
#' 
#' @examples
#' \dontrun{
#' # Using PCA for dimensionality reduction
#' model <- isoForest(iris[1:4])
#' plot_pca <- plot_anomaly_projection(model, iris[1:4], dim_reduction = "pca")
#' print(plot_pca)
#' 
#' # Using UMAP for dimensionality reduction
#' plot_umap <- plot_anomaly_projection(model, iris[1:4], dim_reduction = "umap")
#' print(plot_umap)
#' 
#' # For large datasets, automatic sampling is applied
#' # large_data <- matrix(rnorm(5000 * 5), ncol = 5)
#' # model <- isoForest(large_data)
#' # plot_anomaly_projection(model, large_data)  # Samples so anomalies = 5% of display
#' 
#' # Custom sample rate: show fewer points (anomalies = 10% of display)
#' # plot_anomaly_projection(model, large_data, sample_rate = 0.10)
#' }
#' 
#' @export
plot_anomaly_projection <- function(model, data, 
                              method = "mad",
                              dim_reduction = c("pca", "umap"),
                              contamination = 0.05,
                              point_size = 2,
                              point_alpha = 0.6,
                              umap_n_neighbors = 15,
                              umap_min_dist = 0.1,
                              sample_rate = 0.05) {
  
  # Validate parameters
  dim_reduction <- match.arg(dim_reduction)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for this function")
  }
  
  # Detect anomalies
  result <- set_anomaly_threshold(model, method = method, contamination = contamination)
  is_anomaly <- result$predictions$is_anomaly
  anomaly_scores <- result$predictions$anomaly_score
  
  n_total <- nrow(data)
  n_anomalies <- sum(is_anomaly)
  
  # Smart sampling based on target anomaly rate
  sampled <- FALSE
  sample_indices <- NULL
  
  if (!is.null(sample_rate) && sample_rate > 0 && sample_rate < 1) {
    # Keep all anomalies
    anomaly_indices <- which(is_anomaly)
    normal_indices <- which(!is_anomaly)
    
    # Calculate target total points: n_anomalies / sample_rate
    # For example: if 100 anomalies and sample_rate = 0.05, target = 100/0.05 = 2000 points
    target_total <- ceiling(n_anomalies / sample_rate)
    
    # Calculate how many normal points we need
    n_normal_sample <- target_total - n_anomalies
    
    # Only sample if we have more normal points than needed
    if (n_normal_sample > 0 && 
        length(normal_indices) > n_normal_sample && 
        target_total < n_total) {
      # Randomly sample normal points
      set.seed(42)  # For reproducibility
      normal_sample <- sample(normal_indices, n_normal_sample)
      sample_indices <- c(anomaly_indices, normal_sample)
      sample_indices <- sort(sample_indices)
      
      # Update data and flags
      data <- data[sample_indices, , drop = FALSE]
      is_anomaly <- is_anomaly[sample_indices]
      anomaly_scores <- anomaly_scores[sample_indices]
      sampled <- TRUE
    }
  }
  
  # Standardize data
  data_scaled <- scale(data)
  
  # Perform dimensionality reduction
  if (dim_reduction == "pca") {
    # PCA dimensionality reduction
    pca_result <- stats::prcomp(data_scaled)
    coords <- as.data.frame(pca_result$x[, 1:2])
    colnames(coords) <- c("Dim1", "Dim2")
    
    # Calculate variance explained
    var_exp <- summary(pca_result)$importance[2, 1:2] * 100
    x_label <- sprintf("PC1 (%.1f%%)", var_exp[1])
    y_label <- sprintf("PC2 (%.1f%%)", var_exp[2])
    method_name <- "PCA"
    
  } else if (dim_reduction == "umap") {
    # UMAP dimensionality reduction
    if (!requireNamespace("umap", quietly = TRUE)) {
      stop("Package 'umap' is required. Please install it: install.packages('umap')")
    }
    
    umap_config <- umap::umap.defaults
    umap_config$n_neighbors <- umap_n_neighbors
    umap_config$min_dist <- umap_min_dist
    
    umap_result <- umap::umap(data_scaled, config = umap_config)
    coords <- as.data.frame(umap_result$layout)
    colnames(coords) <- c("Dim1", "Dim2")
    
    x_label <- "UMAP1"
    y_label <- "UMAP2"
    method_name <- "UMAP"
  }
  
  # Prepare plotting data
  plot_data <- coords
  plot_data$type <- ifelse(is_anomaly, "Anomaly", "Normal")
  plot_data$anomaly_score <- anomaly_scores
  
  # Calculate statistics
  n_anomalies_shown <- sum(is_anomaly)
  n_shown <- nrow(data)
  anomaly_rate <- n_anomalies / n_total * 100
  
  # Create title and subtitle
  plot_title <- sprintf("%s Projection: Anomaly Detection Visualization", method_name)
  
  if (sampled) {
    plot_subtitle <- sprintf("Method: %s | Anomalies: %d/%d (%.1f%%) | Threshold: %.3f | Showing: %d/%d points",
                            method, n_anomalies, n_total, anomaly_rate, result$threshold, 
                            n_shown, n_total)
  } else {
    plot_subtitle <- sprintf("Method: %s | Anomalies: %d/%d (%.1f%%) | Threshold: %.3f",
                            method, n_anomalies, n_total, anomaly_rate, result$threshold)
  }
  
  # Create plot - anomalies in red, normal in blue
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = Dim1, y = Dim2, color = type)) +
    ggplot2::geom_point(size = point_size, alpha = point_alpha) +
    ggplot2::scale_color_manual(
      values = c("Normal" = "steelblue", "Anomaly" = "red"),
      name = "Type"
    ) +
    ggplot2::labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_label,
      y = y_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "gray40"),
      legend.position = "bottom"
    )
  
  return(p)
}

#' @title Plot all anomaly projections (PCA and UMAP comparison)
#' @description
#' Create a comparison plot showing both PCA and UMAP projections of the same data,
#' making it easy to see how different dimensionality reduction methods reveal anomalies.
#' For large datasets, automatic sampling is applied to improve speed.
#' 
#' @param model An isoForest model object
#' @param data The original data (must be numeric)
#' @param method Threshold method for anomaly detection (default: "mtt")
#' @param contamination Contamination rate (only used if method = "contamination")
#' @param point_size Size of points in the plot (default: 2)
#' @param point_alpha Transparency of points (default: 0.6)
#' @param umap_n_neighbors Number of neighbors for UMAP (default: 15)
#' @param umap_min_dist Minimum distance for UMAP (default: 0.1)
#' @param sample_rate Target anomaly rate in the displayed data (default: 0.05). 
#'   Set to NULL to disable sampling.
#' 
#' @return A combined ggplot2 object showing both PCA and UMAP projections
#' 
#' @examples
#' \dontrun{
#' model <- isoForest(iris[1:4])
#' comparison_plot <- plot_anomaly_projection_all(model, iris[1:4])
#' print(comparison_plot)
#' }
#' 
#' @export
plot_anomaly_projection_all <- function(model, data,
                                 method = "mtt",
                                 contamination = 0.05,
                                 point_size = 2,
                                 point_alpha = 0.6,
                                 umap_n_neighbors = 15,
                                 umap_min_dist = 0.1,
                                 sample_rate = 0.05) {
  
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required. Please install it: install.packages('gridExtra')")
  }
  
  # Detect anomalies first to get statistics for shared title
  result <- set_anomaly_threshold(model, method = method, contamination = contamination)
  n_anomalies <- sum(result$predictions$is_anomaly)
  n_total <- nrow(data)
  anomaly_rate <- n_anomalies / n_total * 100
  
  # Generate PCA plot without title
  p_pca <- plot_anomaly_projection(model, data, 
                            method = method,
                            dim_reduction = "pca",
                            contamination = contamination,
                            point_size = point_size,
                            point_alpha = point_alpha,
                            sample_rate = sample_rate)
  
  # Generate UMAP plot without title
  p_umap <- plot_anomaly_projection(model, data,
                             method = method,
                             dim_reduction = "umap",
                             contamination = contamination,
                             point_size = point_size,
                             point_alpha = point_alpha,
                             umap_n_neighbors = umap_n_neighbors,
                             umap_min_dist = umap_min_dist,
                             sample_rate = sample_rate)
  
  # Remove individual titles and subtitles
  p_pca <- p_pca + ggplot2::labs(title = NULL, subtitle = NULL)
  p_umap <- p_umap + ggplot2::labs(title = NULL, subtitle = NULL)
  
  # Create shared title and subtitle
  shared_title <- "PCA vs UMAP: Anomaly Detection Comparison"
  
  # Check if data was sampled
  if (!is.null(sample_rate) && sample_rate > 0 && sample_rate < 1) {
    target_total <- ceiling(n_anomalies / sample_rate)
    if (target_total < n_total) {
      shared_subtitle <- sprintf("Method: %s | Anomalies: %d/%d (%.1f%%) | Threshold: %.3f | Showing: ~%d/%d points",
                                method, n_anomalies, n_total, anomaly_rate, result$threshold,
                                target_total, n_total)
    } else {
      shared_subtitle <- sprintf("Method: %s | Anomalies: %d/%d (%.1f%%) | Threshold: %.3f",
                                method, n_anomalies, n_total, anomaly_rate, result$threshold)
    }
  } else {
    shared_subtitle <- sprintf("Method: %s | Anomalies: %d/%d (%.1f%%) | Threshold: %.3f",
                              method, n_anomalies, n_total, anomaly_rate, result$threshold)
  }
  
  # Create title grobs
  title_grob <- grid::textGrob(
    shared_title,
    gp = grid::gpar(fontface = "bold", fontsize = 14)
  )
  
  subtitle_grob <- grid::textGrob(
    shared_subtitle,
    gp = grid::gpar(fontsize = 10, col = "gray40")
  )
  
  # Combine title, subtitle, and plots
  combined_plot <- gridExtra::grid.arrange(
    title_grob,
    subtitle_grob,
    gridExtra::arrangeGrob(p_pca, p_umap, ncol = 2),
    ncol = 1,
    heights = c(0.8, 0.6, 10)
  )
  
  return(combined_plot)
}
