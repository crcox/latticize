#' Translate between index and vertex edge references
#'
#' Vertices refer to the \code{[source, target]} ordered pair defining a
#' directed edge. Indices refer to a 1-D index into the graph representation.
#' The functions \code{\link{edge2vertices}} and \code{\link{vertices2edge}}
#' translate between these representations.
#'
#' @return A matrix of vertex pairs representing edges. Each column is an edge,
#'   and the first row is the source vertex.
#'
#' @details Adjacency matrices are assumed to be oriented such that the row
#'   indicates the source vertex and the column indicates that target vertex.
#'
#'   The returned matrix of \code{vertices} will be oriented such that each
#'   column is an edge. The first row contains the source vertices, and the
#'   second row contains the target vertices.
#'
#' @rdname edge2vertices
#' @export
edge2vertices <- function(x, ...) {
    UseMethod("edge2vertices", x)
}

#' @rdname edge2vertices
#' @param x An adjacency matrix
#' @param indices A numeric vector; 1-D index referencing edges in the graph.
#' @export
edge2vertices.matrix <- function(x, indices) {
    row_ind <- function(i, nrow) {
        return(((i - 1) %% nrow) + 1)
    }
    col_ind <- function(i, nrow) {
        return(ceiling(i / nrow))
    }
    return(rbind(source = row_ind(indices, nrow(x)),
                 target = col_ind(indices, nrow(x))))
}

#' @rdname edge2vertices
#' @param g An igraph object
#' @param indices A numeric vector; 1-D index referencing edges in the graph.
#' @export
edge2vertices.igraph <- function(g, indices) {
    v <- t(igraph::as_edgelist(g, names = FALSE)[indices, , drop = FALSE])
    rownames(v) <- c("source", "target")
    return(v)
}
