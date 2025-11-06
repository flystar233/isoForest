# isoForest 1.1.0

## New Features

* Added **Karcher Mean (Riemannian Center of Mass)** threshold method to `set_anomaly_threshold()`
  - Highly robust geometric method for anomaly detection thresholding
  - Based on Riemannian geometry principles
  - Particularly effective for heavy-tailed distributions and extreme outliers
  - Added parameters: `karcher_multiplier`, `karcher_max_iter`, `karcher_tol`
  - New internal function: `compute_karcher_mean()` for iterative optimization

* Added **MTT (Modified Thompson Tau Test)** threshold method to `set_anomaly_threshold()`
  - Statistical test based on t-distribution for outlier detection
  - Particularly suitable for small to medium sample sizes (n < 1000)
  - Iterative outlier removal with Bonferroni correction
  - Added parameters: `mtt_alpha`, `mtt_max_iter`
  - New internal functions: `compute_mtt_threshold()`, `thompson_tau_critical()`

## Improvements

* Updated `mtt_max_iter` default value from 10 to 30 for better detection of multiple outliers
* Enhanced documentation with comprehensive method comparison and selection guide
* Added detailed threshold setting section to README.md

# isoForest 1.0.0

* Initial CRAN submission.
