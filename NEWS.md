# isoForest 1.1.0

## New Features

* Added **KDE-Weighted Mean** threshold method to `set_anomaly_threshold()`
  - Density-weighted robust mean using kernel density estimation
  - Automatically weights points by local density for better outlier resistance
  - Particularly effective for heavy-tailed distributions and extreme outliers
  - Added parameter: `kde_multiplier`
  - Uses KDE to compute density-weighted center of anomaly score distribution

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
