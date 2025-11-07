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

* Enhanced **`plot_feature_boxplot()`** and **`plot_feature_boxplot_faceted()`** visualization functions
  - `contribution_obj` parameter is now optional (defaults to NULL)
  - `sample_id` can now be a vector to highlight multiple anomalies at once
  - Auto-optimization: point size adjusts to 1 when >5 anomalies are marked
  - Smart subtitle: shows sample IDs for ≤5 anomalies, simplified text for >5
  - Added `highlight_size` parameter to `plot_feature_boxplot_faceted()`
  - All points (including anomalies) now use jitter effect to avoid overlap
  - Better support for visualizing threshold detection results

## Improvements

* Updated `mtt_max_iter` default value from 10 to 30 for better detection of multiple outliers
* Enhanced documentation with comprehensive method comparison and selection guide
* Added detailed threshold setting section to README.md
* Added "Feature Distribution Visualization" section to README.md with usage examples
* Improved plot clarity when displaying many anomaly points simultaneously
* Visualization functions now more flexible for exploring multiple anomalies

# isoForest 1.0.0

* Initial CRAN submission.
