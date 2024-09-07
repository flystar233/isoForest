#' @title Anomaly plot
#' @description Plots the anomaly score heatmap for each point in the space.
#' @param object The fitted isoForest model object
#' @param data The data to be plotted.
#' @return A ggplot object.
#' @examples
#' # Load the data
#' data("iris")
#' # Fit the anomaly detection model
#' model <- isoForest(iris[1:2])
#' # Plot the anomaly scores
#' library(ggplot2)
#' anomaly_plot(model, iris[1:2])
#' @export
anomaly_plot <- function(object, data = NULL) {
  if (!is.data.frame(data)) {
    data <- as.data.frame(data)
  }
  max_values <- apply(data, 2, max)
  min_values <- apply(data, 2, min)
  x_max <- max_values[1] + 1
  y_max <- max_values[2] + 1
  x_min <- min_values[1] - 1
  y_min <- min_values[2] - 1
  pts <- seq(min(x_min, y_min), max(x_max, y_max), .1)
  space_d <- expand.grid(x = pts, y = pts)
  column_names <- colnames(data)
  colnames(space_d) <- column_names
  scores <- stats::predict(object, space_d)
  space_df <- data.frame(space_d, scores$anomaly_score)
  ggplot2::ggplot() +
    ggplot2::geom_tile(data = space_df, ggplot2::aes(x = space_df[[1]], y = space_df[[2]], fill = space_df[[3]])) +
    ggplot2::scale_fill_gradientn(colors = rev(heat.colors(50))) +
    ggplot2::geom_point(data = data, aes(x = data[[1]], y = data[[2]]), color = "#0000801A") +
    ggplot2::xlab(column_names[1]) +
    ggplot2::ylab(column_names[2]) +
    ggplot2::labs(fill = "score")
  #+ xlim(x_min, x_max) + ylim(y_min, y_max)
}
