#' Count the number of edges in the graph
#'
#' A common interface for matrix and igraph representations of graph structure.
#'
#' @return Number of edges
n_vertices <- function(x) {
    UseMethod("n_vertices", x)
}

#' @rdname n_vertices
#' @param x An adjacency matrix
#' @export
n_vertices.matrix <- function(x) {
    return(nrow(x))
}

#' @rdname n_vertices
#' @param g An igraph object.
#' @export
n_vertices.igraph <- function(g) {
    return(igraph::vcount(g))
}
