# isoForest
The isoForest package is a simple replication of the Isolation Forests algorithm for outlier detection, and the [ranger](https://github.com/imbs-hl/ranger) package is used to truly construct the forests.
In addition, the visualization of outliers is also implemented to help better observe the prediction results.

## Installation
```r
# Development version
devtools::install_github("flystar233/isoForest")
```
## Usage
```r
library(isoForest)
result <- isoForest(iris)
head(result$scores)
#     id average_depth anomaly_score
#   <int>         <dbl>         <dbl>
#1     1          7.81         0.554
#2     2          7.82         0.554
#3     3          7.71         0.559
#4     4          7.69         0.559
#5     5          7.78         0.556
#6     6          7.39         0.572
```

## Feature Contribution Analysis

The `feature_contribution()` function helps you understand which features contribute most to a sample's anomaly score. This is crucial for interpreting anomaly detection results and understanding why certain samples are flagged as outliers.

### Methods Available

- **Path-based analysis** (default): Analyzes decision paths in isolation trees to determine feature importance
- **Permutation importance**: Measures how much each feature affects the anomaly score when its values are randomly permuted

### Basic Usage

```r
# Train isolation forest
model <- isoForest(iris[1:4])

# Analyze feature contributions for anomalous samples
contributions <- feature_contribution(model, data = iris[1:4])
print(contributions)

# Analyze specific samples
contributions <- feature_contribution(model, 
                                    sample_ids = c(42, 107, 119), 
                                    data = iris[1:4])
print(contributions)
```

### Using Different Methods

```r
# Path-based analysis (default, faster)
path_contributions <- feature_contribution(model, 
                                         sample_ids = c(1, 50),
                                         data = iris[1:4], 
                                         method = "path")

# Permutation importance (more accurate but slower)
perm_contributions <- feature_contribution(model, 
                                         sample_ids = c(1, 50),
                                         data = iris[1:4], 
                                         method = "permutation",
                                         n_permutations = 50)
```

### Interpreting Results

The function returns contribution percentages showing how much each feature contributes to the anomaly score:

```r
# Example output:
# Sample 42 | Score: 0.723
#   Petal.Length: 45.2%
#   Petal.Width: 32.1% 
#   Sepal.Length: 15.4%
#   Sepal.Width: 7.3%
```

## Anomaly Threshold Setting

The package provides multiple methods for setting anomaly detection thresholds. Instead of manually choosing a threshold, you can use statistical and geometric methods to automatically determine the optimal threshold.

### Available Methods

| Method | Description | Best For |
|--------|-------------|----------|
| **contamination** | Set threshold based on expected outlier proportion | Known anomaly rate |
| **quantile** | Use a specific quantile as threshold | Percentile-based detection |
| **iqr** | Interquartile range (Q3 + 1.5×IQR) | Box-plot style analysis |
| **zscore** | Z-score based (mean + 2×sd) | Normal distributions |
| **mad** | Median Absolute Deviation | Robust, symmetric distributions |
| **kde_weighted** | KDE-weighted mean (density-weighted robust mean) | Heavy tails, extreme outliers |
| **mtt** | Modified Thompson Tau test | Small to medium samples |
| **manual** | User-specified threshold | Custom requirements |

### Quick Start

```r
# Train model
library(isoForest)
model <- isoForest(iris[1:4])

# Method 1: Contamination-based (most common)
result <- set_anomaly_threshold(model, method = "contamination", contamination = 0.05)
print(result)

# Get anomalous samples
anomalies <- iris[result$predictions$is_anomaly, ]
head(anomalies)
```

### Robust Methods

For data with extreme outliers or heavy-tailed distributions:

```r
# KDE-weighted method (density-aware, highly robust)
result_kde <- set_anomaly_threshold(model, method = "kde_weighted", kde_multiplier = 3)

# MAD method (robust and fast)
result_mad <- set_anomaly_threshold(model, method = "mad", mad_multiplier = 3)

# Compare results
cat("KDE-weighted detected:", sum(result_kde$predictions$is_anomaly), "anomalies\n")
cat("MAD detected:", sum(result_mad$predictions$is_anomaly), "anomalies\n")
```

### Statistical Testing

For small to medium sample sizes with statistical guarantees:

```r
# Modified Thompson Tau test
result_mtt <- set_anomaly_threshold(
  model, 
  method = "mtt",
  mtt_alpha = 0.05,      # Significance level
  mtt_max_iter = 30      # Maximum iterations
)

# Adjust sensitivity
result_strict <- set_anomaly_threshold(model, method = "mtt", mtt_alpha = 0.01)  # More conservative
result_loose <- set_anomaly_threshold(model, method = "mtt", mtt_alpha = 0.10)   # More sensitive
```

## Feature Distribution Visualization

The package provides flexible visualization tools to understand how anomalies differ from normal data across features.

### Visualizing Single Anomaly with Contribution Analysis

When you want to understand why a specific sample is anomalous:

```r
# Calculate feature contributions
model <- isoForest(iris[1:4])
contributions <- feature_contribution(model, sample_ids = 42, data = iris[1:4])

# Single boxplot view (shows top contributing features)
plot_anomaly_boxplot(contributions, iris[1:4], sample_id = 42, top_n = 5)

# Faceted view (better for many features)
plot_anomaly_boxplot_faceted(contributions, iris[1:4], sample_id = 42, top_n = 8)
```

### Visualizing Multiple Anomalies (Without Contribution Analysis)

When you want to see where all detected anomalies fall in the feature distributions:

```r
# Detect anomalies using threshold
data <- read.csv('test.csv')
model2 <- isoForest(data)
result <- set_anomaly_threshold(model2, method = "mtt", mtt_alpha = 0.05)
anomaly_ids <- which(result$predictions$is_anomaly)

# Visualize all anomalies at once
plot_anomaly_boxplot(
  contribution_obj = NULL,  # No contribution object needed
  data = data,
  sample_id = anomaly_ids   # Can be a vector of IDs
)

# Faceted view (recommended for multiple features)
plot_anomaly_boxplot_faceted(
  contribution_obj = NULL,
  data = data,
  sample_id = anomaly_ids,
  top_n = NULL  # Show all features
)
```
<img width="568" height="375" alt="2025-11-07_11-16" src="https://github.com/user-attachments/assets/90881255-36ef-4e62-9f89-578b8b911af2" />


## High-Dimensional Data Visualization

For high-dimensional data (>4 features), visualize anomalies in 2D using dimensionality reduction:

```r
# PCA projection (fast, interpretable)
plot_anomaly_projection(model, data, dim_reduction = "pca")

# UMAP projection (better for non-linear patterns, requires 'umap' package)
plot_anomaly_projection(model, data, dim_reduction = "umap")

# Compare both methods side-by-side (requires 'umap' and 'gridExtra' packages)
plot_anomaly_projection_all(model, data)
```
<img width="634" height="317" alt="Rplot" src="https://github.com/user-attachments/assets/df81d45d-5bac-4cdf-8d6e-3413de2d80e2" />


**Features:**
- Anomalies highlighted in red, normal points in blue
- Smart sampling for large datasets (preserves all anomalies)
- Adjust sampling: `sample_rate = 0.05` (default, anomalies = 5% of display)

See `?plot_anomaly_projection` for more details.

## Basic Visualization

```r
result <- isoForest(iris[1:2])
plot_anomaly_basic(result, iris[1:2], plot_type="heatmap")
```
![](https://github.com/user-attachments/assets/8518b445-1631-4e7b-be30-ddcc3dac10ac)
