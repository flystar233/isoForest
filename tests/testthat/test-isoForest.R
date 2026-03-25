X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
X_NA <- X
X_NA[2L, "a"] <- NA

# ===== Core isoForest Tests =====
test_that("isoForest(iris) Generate results as expected", {
  result <- isoForest(iris)
  expect_true(nrow(result$scores) == nrow(iris))
  expect_true(inherits(result, "isoForest"))
  expect_true("anomaly_score" %in% colnames(result$scores))
})

test_that("isoForest on simple data, Generate results as expected", {
  result <- isoForest(X)
  expect_true(nrow(result$scores) == nrow(X))
  expect_true(all(is.numeric(result$scores$anomaly_score)))
})

test_that("isoForest on data with NA, can't work normally", {
  expect_error(isoForest(X_NA))
})

test_that("isoForest edge case: single row fails gracefully", {
  # Single row doesn't make sense for anomaly detection
  # isoForest requires at least 2 rows to train a tree
  expect_error(isoForest(iris[1, , drop = FALSE]))
})

test_that("isoForest edge case: single column", {
  result <- isoForest(iris[, 1, drop = FALSE])
  expect_true(nrow(result$scores) == nrow(iris))
})

test_that("isoForest edge case: duplicate values", {
  dup_data <- data.frame(x = rep(1, 50), y = rep(2, 50))
  result <- isoForest(dup_data)
  expect_true(nrow(result$scores) == 50)
})

# ===== Threshold Detection Tests =====
test_that("set_anomaly_threshold with contamination method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "contamination", contamination = 0.05)
  expect_true(inherits(result, "anomaly_threshold"))
  expect_true(sum(result$predictions$is_anomaly) <= nrow(iris) * 0.1)  # Allow some tolerance
})

test_that("set_anomaly_threshold with quantile method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "quantile", quantile_threshold = 0.95)
  expect_true(!is.na(result$threshold))
})

test_that("set_anomaly_threshold with IQR method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "iqr", iqr_multiplier = 1.5)
  expect_true(!is.na(result$threshold))
})

test_that("set_anomaly_threshold with z-score method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "zscore", zscore_threshold = 2)
  expect_true(!is.na(result$threshold))
})

test_that("set_anomaly_threshold with MAD method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "mad", mad_multiplier = 3)
  expect_true(!is.na(result$threshold))
})

test_that("set_anomaly_threshold with MTT method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "mtt", mtt_alpha = 0.05)
  expect_true(!is.na(result$threshold))
  expect_true(nrow(result$predictions) == nrow(iris))
})

test_that("set_anomaly_threshold with manual method", {
  model <- isoForest(iris)
  result <- set_anomaly_threshold(model, method = "manual", manual_threshold = 0.5)
  expect_true(result$threshold == 0.5)
})

test_that("is_anomaly function", {
  model <- isoForest(iris)
  anomalies <- is_anomaly(model, contamination = 0.05)
  expect_true(is.logical(anomalies))
  expect_true(length(anomalies) == nrow(iris))
})

# ===== Feature Contribution Tests =====
test_that("feature_contribution with path method", {
  model <- isoForest(iris)
  contrib <- feature_contribution(model, data = iris, method = "path", contamination = 0.1)
  expect_true(inherits(contrib, "feature_contribution"))
  expect_true(length(contrib) > 0)
})

test_that("feature_contribution with permutation method", {
  model <- isoForest(iris[, 1:4])
  contrib <- feature_contribution(model, data = iris[, 1:4], method = "permutation",
                                 contamination = 0.1, n_permutations = 10)
  expect_true(inherits(contrib, "feature_contribution"))
  expect_true(length(contrib) > 0)
})

test_that("feature_contribution with specific sample_ids", {
  model <- isoForest(iris)
  contrib <- feature_contribution(model, sample_ids = c(1, 2, 3), data = iris)
  expect_true(inherits(contrib, "feature_contribution"))
  # Returns 3 samples + summary = 4 list elements
  expect_true(length(contrib) >= 3)
  # Check that named elements exist for each sample
  expect_true("sample_1" %in% names(contrib))
  expect_true("sample_2" %in% names(contrib))
  expect_true("sample_3" %in% names(contrib))
})

test_that("feature_contribution summary for multiple samples", {
  model <- isoForest(iris)
  contrib <- feature_contribution(model, sample_ids = c(1, 2, 3, 4, 5), data = iris)
  expect_true(!is.null(contrib$summary))
  expect_true("feature" %in% colnames(contrib$summary))
})

# ===== Plotting Tests =====
test_that("plot_anomaly_basic with scatter plot", {
  model <- isoForest(iris[, 1:2])
  plot_obj <- plot_anomaly_basic(model, iris[, 1:2], plot_type = "scatter")
  expect_true(inherits(plot_obj, "ggplot"))
})

test_that("plot_anomaly_basic with heatmap", {
  model <- isoForest(iris[, 1:2])
  plot_obj <- plot_anomaly_basic(model, iris[, 1:2], plot_type = "heatmap", contamination = 0.1)
  expect_true(inherits(plot_obj, "ggplot"))
})

# ===== Edge Cases and Error Handling =====
test_that("invalid contamination value raises error", {
  model <- isoForest(iris)
  expect_error(is_anomaly(model, contamination = 0))
  expect_error(is_anomaly(model, contamination = 1.5))
})

test_that("calculate_depth_per_tree(treeInfo), Generate results as expected", {
  rf <- ranger::ranger(Species ~ ., data = iris)
  depth <- calculate_depth_per_tree(ranger::treeInfo(rf, 1))
  expect_true(nrow(depth) > 0)
})

test_that("calculate_leaf_to_root_depth(model), Generate results as expected", {
  rf <- ranger::ranger(Species ~ ., data = iris)
  all_depth <- calculate_leaf_to_root_depth(rf)
  expect_true(nrow(all_depth) > 0)
})
