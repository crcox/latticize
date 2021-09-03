#' Check that mutating the graph make it more like a lattice
#'
#' The objective is to reduce the overall cost of edges with respect to
#' \code{d}. If \code{d} is defined so that cost increases with distance from
#' the diagonal, reducing cost will encourage a lattice-like structure.
#'
#' @param x An adjacency matrix or igraph object
#' @param d A matrix specifying the "cost" of each edge
#' @param edges A matrix specifying edges as source->target ordered pairs.
#' @return logical
#'
#' @export
rewiring_makes_more_lattice_like <- function(x, d, edges) {
    d_old <- d[vertices2edge(d, edges)]
    d_new <- d[vertices2edge(d, swap_targets(edges))]
    weights <- get_weights(x, edges)
    return(sum(d_old * weights) > sum(d_new * weights))
}
