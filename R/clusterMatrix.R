#' hierarchical clustering
#'
#' \code{cluster} hierarchically clusters the supplied matrix by column and
#' returns the reordered matrix and optionally the clustering object
#'
#' @param mat          Matrix of counts to cluster
#' @param scale        logical indicating whether to center and scale the data
#' @param dist_method  character method for ceating the distance matrix
#' @param clustering    logical indicating whether to return the hclust
#' @param ...          Other arguments passed to hclust
#'
#' @return If clustering=FALSE, \code{cluster} returns the matrix
#' with columns reordered based on clustering. If clustering=TRUE,
#' then a list is returned with names matrix and clustering.
#'
#' @examples
#' set.seed(1452)
#' cluster( matrix(rnorm(100), ncol = 5) )
#'
#' @export
#'
cluster <- function(mat, scale = TRUE, dist_method = "pearson",
                    clustering = FALSE, ...){
  # centre and scale numbers
  if (scale) {
    scaled_matrix <- scale(mat)
  } else {
    scaled_matrix <- mat
  }

  # create distance matrix
  if (dist_method == "pearson") {
    distance_matrix <- stats::as.dist( (1 - stats::cor(scaled_matrix))/2 )
  } else if (dist_method == "spearman") {
    distance_matrix <- stats::as.dist( (1 - stats::cor(scaled_matrix, method = "spearman"))/2 )
  } else { # assume euclidian distance
    distance_matrix <- dist(t(scaled_matrix))
  }
  # cluster and reorder correlation matrix
  hclust_obj <- stats::hclust(distance_matrix, ...)
  reordered_matrix <- mat[ , hclust_obj$order]

  if (clustering) {
    return(
      list(
        "matrix" = reordered_matrix,
        "clustering" = hclust_obj
      )
    )
  } else {
    return(reordered_matrix)
  }
}

#' hierarchical cluster matrix by rows or columns (or both)
#'
#' \code{cluster_matrix} hierarchically clusters the supplied matrix by row or column
#' or both
#'
#' @param mat Matrix of counts to cluster
#' @param by_row logical - whether to cluster the rows of the matrix
#' @param by_col logical - whether to cluster the columns of the matrix
#' @param scale logical indicating whether to center and scale the data
#' @param dist_method character method for ceating the distance matrix
#' @param ... other arguments passed to hclust
#'
#' @return reordered matrix based on clustering
#'
#' @examples
#' set.seed(1452)
#' cluster_matrix( matrix(rnorm(100), ncol = 5) )
#'
#' cluster_matrix( matrix(rnorm(100), ncol = 5), by_row = FALSE, by_col = TRUE )
#'
#' @export
#'
cluster_matrix <- function(mat, by_row = TRUE, by_col = FALSE,
                           scale = TRUE, dist_method = "pearson", ...){
  # cluster columns
  if( by_col ){
    mat <- cluster(mat, scale = scale, dist_method = dist_method, ...)
  }
  # cluster rows
  if( by_row ){
    mat <- t( cluster( t(mat), scale = scale, dist_method = dist_method, ... ) )
  }
  return( mat )
}
