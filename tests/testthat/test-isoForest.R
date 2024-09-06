X <- data.frame(a = 1:100, b = 1:100)
X[1L, "a"] <- 1000
X_NA <- X
X_NA[2L, "a"] <- NA

test_that("isoForest(iris) Generate results as expected", {
  result <- isoForest(iris)
  expect_true(nrow(result$scores) == nrow(iris))
})

test_that("isoForest on simple data, Generate results as expected", {
  result <- isoForest(X)
  expect_true(nrow(result$scores) == nrow(X))
})

test_that("isoForest on data with NA, can't work noramlly", {
  expect_error(isoForest(X_NA))
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
