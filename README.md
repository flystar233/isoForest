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

## Visualization
```r
result <- isoForest(iris[1:2])
anomaly_plot(result,iris[1:2],plot_type="heatmap")
```
![](https://github.com/user-attachments/assets/8518b445-1631-4e7b-be30-ddcc3dac10ac)
