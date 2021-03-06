% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_matrix.R
\name{cluster}
\alias{cluster}
\title{hierarchical clustering}
\usage{
cluster(mat, scale = TRUE, dist_method = "pearson", clustering = FALSE, ...)
}
\arguments{
\item{mat}{Matrix of counts to cluster}

\item{scale}{logical indicating whether to center and scale the data}

\item{dist_method}{character method for ceating the distance matrix}

\item{clustering}{logical indicating whether to return the hclust}

\item{...}{Other arguments passed to hclust}
}
\value{
If clustering=FALSE, \code{cluster} returns the matrix
with columns reordered based on clustering. If clustering=TRUE,
then a list is returned with names matrix and clustering.
}
\description{
\code{cluster} hierarchically clusters the supplied matrix by column and
returns the reordered matrix and optionally the clustering object
}
\examples{
set.seed(1452)
cluster( matrix(rnorm(100), ncol = 5) )

}
