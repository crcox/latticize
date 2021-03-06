% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_edges.R
\name{sample_edges}
\alias{sample_edges}
\title{Randomly sample edges from a graph}
\usage{
sample_edges(x, size)
}
\arguments{
\item{x}{An adjacency matrix or igraph object.}

\item{size}{The desired sample size.}
}
\value{
A sample of edges. See details.
}
\description{
Randomly sample edges from a graph
}
\details{
If the input is an adjacency matrix, the return value is a 1-D index
  identifying a sample of non-zero values in the matrix. If the input is an
  igraph, then the return value is an \code{igraph.es} object---the output of
  \code{\link[igraph]{E}}.
}
