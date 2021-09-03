#' Check that edges exist in a graph
#'
#' Edges are specified by directed source->target pairs arranged in a matrix.
#'
#' @param x An adjacency matrix.
#' @param g An igraph object.
#' @param edges A matrix of source->target ordered pairs representing edges.
#' @return logical.
#'
#' @details Even if querying a single edge, the input must be represented as a
#'   matrix. In the case of one edge, the matrix would have two rows and one
#'   column.
#'
#' @export
edge_exists <- function(x, ...) {
    UseMethod("edge_exists", x)
}

#' @export
edge_exists.matrix <- function(x, edges) {
    assertthat::are_equal(nrow(edges), 2)
    ind <- vertices2edge(x, edges)
    return(x[ind] > 0)
}

#' @export
edge_exists.igraph <- function(g, edges) {
    assertthat::are_equal(nrow(edges), 2)
    return(igraph::get.edge.ids(g, c(edges)) > 0)
    # alternative implementation:
    #   return(igraph::vertex_connectivity(g, edges[1], edges[2]) > 0)
}
