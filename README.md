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
## visualization
```r
result <- isoForest(iris[1:2])
anomaly_plot(result,iris[1:2])
```
![](https://github.com/user-attachments/assets/8518b445-1631-4e7b-be30-ddcc3dac10ac)
