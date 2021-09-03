#' Return edge indices
#'
#' @param x,g An adjacency matrix or igraph object.
#' @return Indices corresponding to existing edges.
#'
#' @details If the input is an adjacency matrix, the return value is a 1-D index
#'   identifying the non-zero values in the matrix. If the input is an igraph,
#'   then the return value is an \code{igraph.es} object---the output of
#'   \code{\link[igraph]{E}}.
#'
#' @export
edge_indices <- function(x) {
    UseMethod("edge_indices", x)
}

#' @rdname edge_indices
#' @export
edge_indices.matrix <- function(x) {
    return(which(x != 0))
}

#' @rdname edge_indices
#' @export
edge_indices.igraph <- function(g) {
    return(as.numeric(igraph::E(g)))
}
