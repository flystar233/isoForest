\name{karcher_threshold}
\alias{karcher_threshold}
\alias{compute_karcher_mean}
\title{Karcher Mean Thresholding for Anomaly Detection}
\description{
The Karcher mean (also known as Fréchet mean or Riemannian center of mass) is a 
robust geometric method for setting thresholds in anomaly detection. It minimizes 
the sum of squared geodesic distances, making it particularly effective for data 
with non-Euclidean structure, heavy-tailed distributions, and extreme outliers.
}

\section{Theory}{
The Karcher mean \eqn{\mu_K} is defined as the point that minimizes the sum of 
squared distances:

\deqn{\mu_K = \arg\min_{\mu} \sum_{i=1}^{n} d^2(x_i, \mu)}

where \eqn{d(x_i, \mu)} is the distance between observation \eqn{x_i} and the 
candidate mean \eqn{\mu}.

For threshold determination, the method:
\enumerate{
  \item Computes the Karcher mean of anomaly scores iteratively
  \item Calculates robust scale using MAD (Median Absolute Deviation) of distances
  \item Sets threshold as: \eqn{threshold = \mu_K + m \times scale}
}

where \eqn{m} is the multiplier parameter (default: 2.5).
}

\section{Advantages}{
\itemize{
  \item \strong{Robust to extreme outliers}: Unlike mean-based methods, the Karcher 
        mean is less influenced by extreme values
  \item \strong{Geometric optimality}: Provides the optimal center in a Riemannian 
        geometric sense
  \item \strong{Heavy-tailed distributions}: Works well when data has long tails
  \item \strong{Adaptive}: The iterative weighting scheme adapts to data structure
  \item \strong{Theoretical foundation}: Backed by solid mathematical theory from 
        Riemannian geometry
}
}

\section{Comparison with Other Methods}{
\tabular{lll}{
  \strong{Method} \tab \strong{Robustness} \tab \strong{Best For} \cr
  Karcher Mean \tab Very High \tab Heavy tails, extreme outliers \cr
  MAD \tab High \tab Symmetric distributions \cr
  IQR \tab Medium-High \tab Box-plot style analysis \cr
  Z-Score \tab Low \tab Normal distributions \cr
  Contamination \tab N/A \tab Known anomaly proportion \cr
}
}

\section{Parameters}{
\describe{
  \item{karcher_multiplier}{
    Controls the threshold strictness. Default: 2.5
    \itemize{
      \item Lower (1.5-2.0): More sensitive, detects more anomalies
      \item Higher (3.0-4.0): More conservative, fewer false positives
    }
  }
  \item{karcher_max_iter}{
    Maximum iterations for convergence. Default: 100
    \itemize{
      \item Increase for very complex distributions
      \item Decrease for faster computation on simple data
    }
  }
  \item{karcher_tol}{
    Convergence tolerance. Default: 1e-6
    \itemize{
      \item Smaller values: More precise but slower
      \item Larger values: Faster but less precise
    }
  }
}
}

\section{Algorithm Details}{
The implementation uses an iterative weighted averaging approach:
\enumerate{
  \item Initialize with the median (robust starting point)
  \item Compute distances from current estimate to all points
  \item Calculate weights inversely proportional to distances
  \item Update estimate using weighted average
  \item Apply adaptive step size for stable convergence
  \item Repeat until convergence or max iterations
}

The adaptive step size \eqn{s_t = 1/(1 + t/10)} ensures convergence even for 
difficult distributions.
}

\section{Computational Complexity}{
\itemize{
  \item Time: \eqn{O(n \times k)} where \eqn{n} is sample size and \eqn{k} is 
        iterations (typically \eqn{k < 50})
  \item Space: \eqn{O(n)} for storing scores and weights
  \item Generally comparable to MAD in practice
}
}

\usage{
set_anomaly_threshold(
  object, 
  method = "karcher",
  karcher_multiplier = 2.5,
  karcher_max_iter = 100,
  karcher_tol = 1e-6
)
}

\arguments{
  \item{object}{An isoForest model object}
  \item{method}{Set to "karcher" for this method}
  \item{karcher_multiplier}{Multiplier for threshold calculation (default: 2.5)}
  \item{karcher_max_iter}{Maximum iterations (default: 100)}
  \item{karcher_tol}{Convergence tolerance (default: 1e-6)}
}

\value{
A list containing:
\item{threshold}{The calculated threshold value}
\item{method}{The method used ("karcher")}
\item{predictions}{Data frame with anomaly predictions}
\item{summary}{Summary statistics including Karcher mean convergence info}
}

\examples{
# Basic usage
library(isoForest)
data(iris)
model <- isoForest(iris[, 1:4])
result <- set_anomaly_threshold(model, method = "karcher")
print(result)

# Tune multiplier for different sensitivity
result_sensitive <- set_anomaly_threshold(
  model, 
  method = "karcher",
  karcher_multiplier = 2.0  # More sensitive
)

result_conservative <- set_anomaly_threshold(
  model, 
  method = "karcher",
  karcher_multiplier = 3.0  # More conservative
)

# Compare anomaly counts
cat("Sensitive:", sum(result_sensitive$predictions$is_anomaly), "anomalies\n")
cat("Default:", sum(result$predictions$is_anomaly), "anomalies\n")
cat("Conservative:", sum(result_conservative$predictions$is_anomaly), "anomalies\n")

# Compare with other robust methods
result_mad <- set_anomaly_threshold(model, method = "mad")
result_iqr <- set_anomaly_threshold(model, method = "iqr")

comparison <- data.frame(
  Method = c("Karcher", "MAD", "IQR"),
  Threshold = c(result$threshold, result_mad$threshold, result_iqr$threshold),
  Anomalies = c(
    sum(result$predictions$is_anomaly),
    sum(result_mad$predictions$is_anomaly),
    sum(result_iqr$predictions$is_anomaly)
  )
)
print(comparison)
}

\references{
Pennec, X. (2006). Intrinsic statistics on Riemannian manifolds: Basic tools for 
geometric measurements. \emph{Journal of Mathematical Imaging and Vision}, 
\strong{25}(1), 127-154.

Afsari, B. (2011). Riemannian Lp center of mass: existence, uniqueness, and 
convexity. \emph{Proceedings of the American Mathematical Society}, 
\strong{139}(2), 655-673.

Fletcher, P. T., Venkatasubramanian, S., & Joshi, S. (2009). The geometric median 
on Riemannian manifolds with application to robust atlas estimation. 
\emph{NeuroImage}, \strong{45}(1), S143-S152.
}

\seealso{
\code{\link{set_anomaly_threshold}}, 
\code{\link{is_anomaly}}
}

\author{
isoForest Development Team
}

\keyword{robust}
\keyword{anomaly-detection}
\keyword{threshold}
\keyword{riemannian-geometry}

