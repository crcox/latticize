% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_weights.R
\name{get_weights}
\alias{get_weights}
\alias{get_weights.matrix}
\alias{get_weights.igraph}
\title{Get edge weights}
\usage{
get_weights(x, ...)

\method{get_weights}{matrix}(x, edges)

\method{get_weights}{igraph}(g, edges)
}
\arguments{
\item{x}{An adjacency matrix}

\item{edges}{A matrix specifying edges as source->target ordered pairs.}

\item{g}{An igraph object}
}
\value{
Real-valued edge weights.
}
\description{
A common interface
}
\details{
If an edge does not exist, the function will return a zero weight.
  The length of the output will always equal the number of edges provided as
  input.
}
