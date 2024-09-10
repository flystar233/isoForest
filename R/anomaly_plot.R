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
  plot_type <- match.arg(plot_type)
  column_names <- colnames(data)
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  if (plot_type == "scatter") {
    scores <- object$scores$anomaly_score
    Anomalies <- as.factor(scores > quantile(scores, (1 - contamination), na.rm = TRUE))
    levels(Anomalies) <- c("Normal", "Anomaly")
    data <- data.frame(cbind(data, Anomalies))
    ggplot2::ggplot(data, ggplot2::aes(x = data[[1]], y = data[[2]], color = Anomalies, shape = Anomalies)) +
      ggplot2::geom_point(size = ifelse(Anomalies == "Normal", 3, 4.5)) +
      ggplot2::scale_colour_manual(name = "Type", values = c("#1661ab", "#ef475d")) +
      ggplot2::scale_shape_manual(name = "Type", values = c(19, 17)) +
      ggplot2::xlab(column_names[1]) +
      ggplot2::ylab(column_names[2]) +
      ggplot2::theme_bw()
  } else if (plot_type == "heatmap") {
    # preprocess data
    max_values <- apply(data, 2, max)
    min_values <- apply(data, 2, min)
    x_max <- max_values[1] + 1
    y_max <- max_values[2] + 1
    x_min <- min_values[1] - 1
    y_min <- min_values[2] - 1
    pts <- seq(min(x_min, y_min), max(x_max, y_max), .1)
    space_d <- expand.grid(x = pts, y = pts)
    colnames(space_d) <- column_names
    scores <- stats::predict(object, space_d)
    space_df <- data.frame(space_d, scores$anomaly_score)
    # make plot
    ggplot2::ggplot() +
      ggplot2::geom_tile(data = space_df, ggplot2::aes(x = space_df[[1]], y = space_df[[2]], fill = space_df[[3]])) +
      ggplot2::scale_fill_gradientn(colors = rev(heat.colors(50))) +
      ggplot2::geom_point(data = data, ggplot2::aes(x = data[[1]], y = data[[2]]), color = "#0000801A") +
      ggplot2::xlab(column_names[1]) +
      ggplot2::ylab(column_names[2]) +
      ggplot2::labs(fill = "score") +
      ggplot2::theme_bw()
  } else {
    stop("plot_type must be either 'scatter' or 'heatmap'")
  }
}
