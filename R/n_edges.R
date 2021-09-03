#' Count the number of edges in the graph
#'
#' A common interface for matrix and igraph representations of graph structure.
#'
#' @return Number of edges
#' @export
n_edges <- function(x, ...) {
    UseMethod("n_edges", x)
}

#' @rdname n_edges
#' @param x An adjacency matrix
#' @param count_loops Logical. Should loops (the diagonal) be included in the
#'   count?
#' @export
n_edges.matrix <- function(x, count_loops = TRUE) {
    if (!count_loops) {
      diag(x) <- 0
    }
    return(sum(x != 0))
}

#' @rdname n_edges
#' @param g An igraph object.
#' @export
n_edges.igraph <- function(g, count_loops = TRUE) {
    return(igraph::ecount(igraph::simplify(g, remove.loops = !count_loops)))
}
