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
The Karcher mean (Frechet mean) \eqn{\mu_K} is defined as the point that minimizes 
the sum of squared distances:

\deqn{\mu_K = \arg\min_{\mu} \sum_{i=1}^{n} d^2(x_i, \mu)}

where \eqn{d(x_i, \mu)} is the distance between observation \eqn{x_i} and the 
candidate mean \eqn{\mu}.

For threshold determination, the method offers two modes:
\enumerate{
  \item \strong{Complex mode (default)}: Uses KDE (kernel density estimation) 
        weighting to account for data distribution
  \item \strong{Simple mode}: Direct computation of weighted arithmetic mean
  \item Calculates scale using standard deviation (SD) of anomaly scores
  \item Sets threshold as: \eqn{threshold = \mu_K + m \times SD}
}

where \eqn{m} is the multiplier parameter (default: 3).
}

\section{Advantages}{
\itemize{
  \item \strong{Distribution awareness}: Complex mode uses KDE to weight scores by 
        their density, providing better handling of non-uniform distributions
  \item \strong{Geometric optimality}: Provides the optimal center (Frechet mean) in 
        Euclidean space
  \item \strong{Flexible computation}: Supports both simple (fast) and complex 
        (distribution-aware) modes
  \item \strong{Proven algorithm}: Based on PyOD KARCH implementation with solid 
        empirical results
  \item \strong{Theoretical foundation}: Grounded in Riemannian geometry and 
        Frechet mean theory
}
}

\section{Comparison with Other Methods}{
\tabular{lll}{
  \strong{Method} \tab \strong{Robustness} \tab \strong{Best For} \cr
  Karcher (complex) \tab High \tab Non-uniform distributions, density-aware \cr
  Karcher (simple) \tab Medium \tab Simple weighted mean approach \cr
  MAD \tab Very High \tab Robust, symmetric distributions \cr
  IQR \tab Medium-High \tab Box-plot style analysis \cr
  Z-Score \tab Low \tab Normal distributions \cr
  Contamination \tab N/A \tab Known anomaly proportion \cr
}
}

\section{Parameters}{
\describe{
  \item{karcher_multiplier}{
    Controls the threshold strictness. Default: 3
    \itemize{
      \item Lower (1-2): More sensitive, detects more anomalies
      \item Higher (3-4): Balanced, standard approach
      \item Even higher (>4): More conservative, fewer false positives
    }
  }
  \item{karcher_method}{
    Computation method. Default: "complex"
    \itemize{
      \item "complex": Uses KDE weighting for better distribution fitting
      \item "simple": Direct weighted mean computation (faster)
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
The implementation follows the PyOD KARCH approach:

\strong{Complex Mode (Default):}
\enumerate{
  \item Normalize anomaly scores to [0, 1]
  \item Generate kernel density estimation (KDE) using Gaussian kernel
  \item Perform element-wise multiplication: KDE values × sorted normalized scores
  \item Compute weighted arithmetic mean (Frechet mean in Euclidean space)
  \item Denormalize back to original scale
  \item Calculate threshold: Frechet mean + multiplier × SD
}

\strong{Simple Mode:}
\enumerate{
  \item Compute weighted arithmetic mean directly on scores
  \item Calculate threshold: mean + multiplier × SD
}

For 1D Euclidean space, the Frechet mean is simply the weighted arithmetic mean.
}

\section{Computational Complexity}{
\itemize{
  \item \strong{Complex mode}: \eqn{O(n \log n)} for KDE computation
  \item \strong{Simple mode}: \eqn{O(n)} for direct mean computation
  \item Space: \eqn{O(n)} for storing scores and KDE values
  \item Complex mode is slightly slower than MAD, but provides better distribution fitting
}
}

\usage{
set_anomaly_threshold(
  object, 
  method = "karcher",
  karcher_multiplier = 3,
  karcher_method = "complex",
  karcher_max_iter = 100,
  karcher_tol = 1e-6
)
}

\arguments{
  \item{object}{An isoForest model object}
  \item{method}{Set to "karcher" for this method}
  \item{karcher_multiplier}{Multiplier for threshold calculation (default: 3)}
  \item{karcher_method}{Computation method: "simple" or "complex" (default: "complex")}
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
  karcher_multiplier = 4.0  # More conservative
)

# Compare simple vs complex method
result_simple <- set_anomaly_threshold(
  model,
  method = "karcher",
  karcher_method = "simple"
)

result_complex <- set_anomaly_threshold(
  model,
  method = "karcher",
  karcher_method = "complex"
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
Zhao, Y., Nasrullah, Z., & Li, Z. (2019). PyOD: A Python Toolbox for Scalable 
Outlier Detection. \emph{Journal of Machine Learning Research}, \strong{20}(96), 1-7.

Pennec, X. (2006). Intrinsic statistics on Riemannian manifolds: Basic tools for 
geometric measurements. \emph{Journal of Mathematical Imaging and Vision}, 
\strong{25}(1), 127-154.

Afsari, B. (2011). Riemannian Lp center of mass: existence, uniqueness, and 
convexity. \emph{Proceedings of the American Mathematical Society}, 
\strong{139}(2), 655-673.

Miolane, N., et al. (2020). Geomstats: A Python Package for Riemannian Geometry 
in Machine Learning. \emph{Journal of Machine Learning Research}, \strong{21}(223), 1-9.
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

