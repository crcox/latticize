#' Get edge weights
#'
#' A common interface
#'
#' @param x An adjacency matrix
#' @param g An igraph object
#' @param edges A matrix specifying edges as source->target ordered pairs.
#' @return Real-valued edge weights.
#'
#' @details If an edge does not exist, the function will return a zero weight.
#'   The length of the output will always equal the number of edges provided as
#'   input.
#'
#' @export
get_weights <- function(x, ...) {
  UseMethod("get_weights", x)
}

#' @rdname get_weights
#' @export
get_weights.matrix <- function(x, edges) {
    return(x[vertices2edge(x, edges)])
}

#' @rdname get_weights
#' @export
get_weights.igraph <- function(g, edges) {
    ix <- vertices2edge(g, edges)
    if (is.null(igraph::E(g)$weight))
        return(ifelse(ix > 0, 1, 0))
    else
        w <- numeric(length(ix))
        w[ix > 0] <- igraph::E(g)[ix[ix > 0]]$weight
        return(w)
}
