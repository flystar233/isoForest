#' @title Anomaly plot
#' @description Plots the anomaly score heatmap for each point in the space.
#' @param object The fitted isoForest model object
#' @param data The data to be plotted.
#' @param plot_type The type of plot to be generated. Either "heatmap" or "scatter".
#' @param contamination The proportion of outliers in the data. Default is 0.05. This parameter is used to determine the threshold for anomaly scores.
#' @return A ggplot object.
#' @examples
#' # Load the data
#' data("iris")
#' # Fit the anomaly detection model
#' model <- isoForest(iris[1:2])
#' # Plot the anomaly scores
#' anomaly_plot(model, iris[1:2], plot_type = "scatter")
#' anomaly_plot(model, iris[1:2], plot_type = "heatmap")
#' @export
anomaly_plot <- function(object,
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
