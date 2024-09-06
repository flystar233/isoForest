#' @title Calculate the harmonic approximation
#' @param n number of sample size
#' @return harmonic approximation
#' @export
#' @examples harmonic_approx(100)
harmonic_approx <- function(n) {
  gamma <- 0.5772156649
  harmonic <- log(n) + gamma
  return(harmonic)
}

#' @title Calculate the normalization factor
#' @param sample_size number of sample size
#' @return normalization factor
#' @examples
#' \dontrun{
#' pathLengthNormalizer(100)
#' }
pathLengthNormalizer <- function(sample_size) {
  res <- 0
  if (sample_size == 2) {
    res <- 1
  } else if (sample_size < 2) {
    res <- 0
  } else {
    res <- (2 * harmonic_approx(sample_size - 1)) - (2 * (sample_size - 1) / sample_size)
  }
  return(res)
}

#' @title Calculate the anomaly score
#' @param average_depth average depth of the terminal nodes
#' @param sample_size number of sample size
#' @return anomaly score
#' @examples
#' \dontrun{
#' computeAnomaly(10, 256)
#' }
computeAnomaly <- function(average_depth, sample_size) {
  normalization_factor <- pathLengthNormalizer(sample_size)

  return(2^(-(average_depth / normalization_factor)))
}
