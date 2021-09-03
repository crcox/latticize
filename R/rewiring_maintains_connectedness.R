#' Check that swapping targets between two edges maintains conncectedness
#'
#' Connectedness refers to the ability to get between any pair of vertices in
#' the graph.
#'
#' @param x,g An adjacency matrix or igraph object
#' @param edges A matrix where each column is a source->destination ordered pair
#'   representing an edge.
#' @return logical
#'
#' @export
rewiring_maintains_connectedness <- function(x, ...) {
    UseMethod("rewiring_maintains_connectedness", x)
}

#' @rdname rewiring_maintains_connectedness
#' @export
rewiring_maintains_connectedness.igraph <- function(g, edges) {
    return(igraph::vertex_connectivity(rewire_targets(g, edges)) > 0)
}
