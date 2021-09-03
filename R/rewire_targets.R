#' Mutate the graph to swap targets between a pair of edges
#'
#' @param x An adjacency matrix
#' @param g An igraph object
#' @param edges A matrix where each column is a source->destination ordered pair
#'   representing an edge.
#' @return A mutated version of the graph.
#'
#' @export
rewire_targets <- function(x, ...) {
    UseMethod("rewire_targets", x)
}

#' @rdname rewire_targets
#' @export
rewire_targets.matrix <- function(x, edges) {
    e <- vertices2edge(x, edges)
    r <- vertices2edge(x, swap_targets(edges))
    x[r] <- x[e]
    x[e] <- 0
    return(x)
}

#' @rdname rewire_targets
#' @export
rewire_targets.igraph <- function(g, edges) {
    ix <- attr(g, "shuffle_vertices_index")
    g <- igraph::add_edges(g, c(swap_targets(edges)))
    g <- igraph::delete_edges(g, vertices2edge(g, edges))
    attr(g, "shuffle_vertices_index") <- ix
    return(g)
}
