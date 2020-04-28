context('clustering')
library(miscr)

set.seed(63738)
test_matrix <-
  matrix(
    replicate(100, c(sample(1:20, 3), sample(40:60, 3), sample(80:120, 3))),
    nrow = 100, byrow = TRUE)
dimnames(test_matrix) <- list("genes" = paste0('gene_', 1:100),
                              "samples" = paste('sample_', 1:9))
pearson_clustered_columns <-
  c("sample_ 4", "sample_ 7", "sample_ 3", "sample_ 1", "sample_ 9",
    "sample_ 6", "sample_ 8", "sample_ 2", "sample_ 5")
pearson_clustered_columns_ward_d2 <-
  c("sample_ 4", "sample_ 7", "sample_ 2", "sample_ 5", "sample_ 3",
    "sample_ 1", "sample_ 9", "sample_ 6", "sample_ 8")
euclidean_clustered_columns <-
  c("sample_ 9", "sample_ 7", "sample_ 8", "sample_ 2", "sample_ 1",
    "sample_ 3", "sample_ 6", "sample_ 4", "sample_ 5")

test_that("cluster", {
  expect_equal(colnames(cluster(test_matrix)), pearson_clustered_columns)
  expect_equal(colnames(cluster(test_matrix, scale=FALSE, dist_method = "euclidean")),
               euclidean_clustered_columns)
  expect_equal(colnames(cluster(test_matrix, method = "ward.D2")),
               pearson_clustered_columns_ward_d2)
  expect_type(cluster(test_matrix), "integer")
  expect_type(cluster(test_matrix, clustering = TRUE), "list")
  expect_equal(names(cluster(test_matrix, clustering = TRUE)), c("matrix", "clustering") )
})

test_that("cluster_matrix", {
  expect_equal(colnames(cluster_matrix(test_matrix, by_col = TRUE)), pearson_clustered_columns)
  expect_equal(colnames(cluster_matrix(test_matrix, by_col = TRUE, scale=FALSE, dist_method = "euclidean")),
               euclidean_clustered_columns)
  expect_equal(colnames(cluster_matrix(test_matrix, by_col = TRUE, method = "ward.D2")),
               pearson_clustered_columns_ward_d2)
})
