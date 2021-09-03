#' Check that swapping targets between two edges will require creating new edges
#'
#' @param x An adjacency matrix or igraph object
#' @param edges A matrix specifying edges as source->destination ordered
#'   pairs.
#' @return logical
#'
#' @details \code{edges} should be oriented such that each column is an
#'   edge, the first row contains the source vertices, and the second row
#'   contains the target vertices.
#'
#' @export
rewiring_creates_new_edges <- function(x, edges) {
    return(!any(edge_exists(x, swap_targets(edges))))
}
