#' @title Set anomaly detection threshold
#' @description
#' This function provides various methods to set thresholds for anomaly detection
#' based on anomaly scores from isoForest.
#' @param object An isoForest model object
#' @param method The method to use for threshold setting. Options include:
#'   "contamination" (default), "quantile", "iqr", "zscore", "mad", "manual"
#' @param contamination The expected proportion of outliers (for contamination method). Default is 0.05
#' @param quantile_threshold The quantile threshold (for quantile method). Default is 0.95
#' @param iqr_multiplier The IQR multiplier (for iqr method). Default is 1.5
#' @param zscore_threshold The z-score threshold (for zscore method). Default is 2
#' @param mad_multiplier The MAD multiplier (for mad method). Default is 3
#' @param manual_threshold The manual threshold value (for manual method)
#' @return A list containing:
#'   - threshold: The calculated threshold value
#'   - method: The method used
#'   - predictions: A data frame with id, anomaly_score, and is_anomaly columns
#'   - summary: A summary of the results
#' @examples
#' # Load data and train model
#' data(iris)
#' model <- isoForest(iris[1:4])
#' 
#' # Method 1: Contamination-based (most common)
#' result1 <- set_anomaly_threshold(model, method = "contamination", contamination = 0.05)
#' 
#' # Method 2: Quantile-based
#' result2 <- set_anomaly_threshold(model, method = "quantile", quantile_threshold = 0.95)
#' 
#' # Method 3: IQR-based
#' result3 <- set_anomaly_threshold(model, method = "iqr", iqr_multiplier = 1.5)
#' 
#' # Method 4: Z-score based
#' result4 <- set_anomaly_threshold(model, method = "zscore", zscore_threshold = 2)
#' 
#' # Method 5: MAD-based (robust)
#' result5 <- set_anomaly_threshold(model, method = "mad", mad_multiplier = 3)
#' 
#' # Method 6: Manual threshold
#' result6 <- set_anomaly_threshold(model, method = "manual", manual_threshold = 0.6)
#' 
#' # View results
#' print(result1$summary)
#' head(result1$predictions)
#' @export
set_anomaly_threshold <- function(object, 
                                  method = "contamination",
                                  contamination = 0.05,
                                  quantile_threshold = 0.95,
                                  iqr_multiplier = 1.5,
                                  zscore_threshold = 2,
                                  mad_multiplier = 3,
                                  manual_threshold = NULL) {
  
  # Input validation
  if (!inherits(object, "isoForest")) {
    stop("Object must be an isoForest model")
  }
  
  method <- match.arg(method, c("contamination", "quantile", "iqr", "zscore", "mad", "manual"))
  
  scores <- object$scores$anomaly_score
  n_samples <- length(scores)
  
  # Calculate threshold based on method
  threshold <- switch(method,
    "contamination" = {
      if (contamination <= 0 || contamination >= 1) {
        stop("Contamination must be between 0 and 1")
      }
      stats::quantile(scores, 1 - contamination, na.rm = TRUE)
    },
    
    "quantile" = {
      if (quantile_threshold <= 0 || quantile_threshold >= 1) {
        stop("Quantile threshold must be between 0 and 1")
      }
      stats::quantile(scores, quantile_threshold, na.rm = TRUE)
    },
    
    "iqr" = {
      Q1 <- stats::quantile(scores, 0.25, na.rm = TRUE)
      Q3 <- stats::quantile(scores, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      Q3 + iqr_multiplier * IQR
    },
    
    "zscore" = {
      mean_score <- mean(scores, na.rm = TRUE)
      sd_score <- stats::sd(scores, na.rm = TRUE)
      mean_score + zscore_threshold * sd_score
    },
    
    "mad" = {
      median_score <- stats::median(scores, na.rm = TRUE)
      mad_score <- stats::mad(scores, na.rm = TRUE)
      median_score + mad_multiplier * mad_score
    },
    
    "manual" = {
      if (is.null(manual_threshold)) {
        stop("Manual threshold value must be provided")
      }
      manual_threshold
    }
  )
  
  # Make predictions
  is_anomaly <- scores > threshold
  
  # Create predictions data frame
  predictions <- data.frame(
    id = object$scores$id,
    average_depth = object$scores$average_depth,
    anomaly_score = scores,
    is_anomaly = is_anomaly,
    stringsAsFactors = FALSE
  )
  
  # Calculate summary statistics
  n_anomalies <- sum(is_anomaly)
  actual_contamination <- n_anomalies / n_samples
  
  summary_stats <- list(
    method = method,
    threshold = threshold,
    total_samples = n_samples,
    detected_anomalies = n_anomalies,
    actual_contamination_rate = actual_contamination,
    score_range = range(scores, na.rm = TRUE),
    score_mean = mean(scores, na.rm = TRUE),
    score_median = stats::median(scores, na.rm = TRUE),
    score_sd = stats::sd(scores, na.rm = TRUE)
  )
  
  # Return results
  result <- list(
    threshold = threshold,
    method = method,
    predictions = predictions,
    summary = summary_stats
  )
  
  class(result) <- c("anomaly_threshold")
  return(result)
}

#' @title Print method for anomaly_threshold objects
#' @description Print summary of anomaly threshold results
#' @param x An anomaly_threshold object
#' @param ... Additional arguments (not used)
#' @export
print.anomaly_threshold <- function(x, ...) {
  cat("Anomaly Detection Threshold Results\n")
  cat("===================================\n")
  cat("Method:", x$summary$method, "\n")
  cat("Threshold:", round(x$threshold, 4), "\n")
  cat("Total samples:", x$summary$total_samples, "\n")
  cat("Detected anomalies:", x$summary$detected_anomalies, "\n")
  cat("Contamination rate:", round(x$summary$actual_contamination_rate * 100, 2), "%\n")
  cat("Score range: [", round(x$summary$score_range[1], 4), ", ", 
      round(x$summary$score_range[2], 4), "]\n", sep = "")
  cat("Score statistics:\n")
  cat("  Mean:", round(x$summary$score_mean, 4), "\n")
  cat("  Median:", round(x$summary$score_median, 4), "\n")
  cat("  SD:", round(x$summary$score_sd, 4), "\n")
}

#' @title Quick anomaly detection
#' @description
#' A simple wrapper function to quickly identify anomalies using contamination rate
#' @param object An isoForest model object
#' @param contamination The expected proportion of outliers. Default is 0.05 (5%)
#' @return A logical vector indicating which samples are anomalies
#' @examples
#' # Train model and detect anomalies
#' model <- isoForest(iris[1:4])
#' anomalies <- is_anomaly(model, contamination = 0.05)
#' 
#' # Show anomalous samples
#' iris[anomalies, ]
#' @export
is_anomaly <- function(object, contamination = 0.05) {
  if (!inherits(object, "isoForest")) {
    stop("Object must be an isoForest model")
  }
  
  if (contamination <= 0 || contamination >= 1) {
    stop("Contamination must be between 0 and 1")
  }
  
  scores <- object$scores$anomaly_score
  threshold <- stats::quantile(scores, 1 - contamination, na.rm = TRUE)
  return(scores > threshold)
}
