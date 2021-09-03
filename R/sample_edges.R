#' Randomly sample edges from a graph
#'
#' @param x An adjacency matrix or igraph object.
#' @param size The desired sample size.
#' @return A sample of edges. See details.
#'
#' @details If the input is an adjacency matrix, the return value is a 1-D index
#'   identifying a sample of non-zero values in the matrix. If the input is an
#'   igraph, then the return value is an \code{igraph.es} object---the output of
#'   \code{\link[igraph]{E}}.
#'
#' @export
sample_edges <- function(x, size) {
    ind <- sample(edge_indices(x), size = size, replace = FALSE)
    return(edge2vertices(x, ind))
}

#' Sample edges with unique vertices
#'
#' Draws random samples until condition is met.
#'
#' @inheritParams sample_edges
#' @return A sample of edges. See details.
#'
#' @details If the input is an adjacency matrix, the return value is a 1-D index
#'   identifying a sample of non-zero values in the matrix. If the input is an
#'   igraph, then the return value is an \code{igraph.es} object---the output of
#'   \code{\link[igraph]{E}}.
#'
#' @export
sample_edges_with_unique_vertices <- function(x, size) {
    assertthat::assert_that(n_vertices(x) > 3)
    while (TRUE) {
        edges <- sample_edges(x, size)
        if (all_unique(edges)) {
            return(edges)
        }
    }
}
