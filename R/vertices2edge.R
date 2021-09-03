#' Translate between index and vertex edge references
#'
#' @rdname edge2vertices
#' @export
vertices2edge <- function(x, ...) {
    UseMethod("vertices2edge", x)
}

#' @rdname edge2vertices
#' @param x An adjacency matrix
#' @param vertices A matrix specifying edges as source->target ordered pairs.
#' @export
vertices2edge.matrix <- function(x, vertices) {
    sub2ind <- function(x, nrow) {
        column_offset <- (x[2, ] - 1) * nrow
        return(x[1, ] + column_offset)
    }
    assertthat::are_equal(nrow(vertices), 2)
    return(sub2ind(vertices, nrow(x)))
}

#' @rdname edge2vertices
#' @param g An igraph object
#' @param vertices A matrix specifying edges as source->target ordered pairs.
#' @export
vertices2edge.igraph <- function(g, vertices) {
    assertthat::are_equal(nrow(vertices), 2)
    return(as.numeric(igraph::get.edge.ids(g, c(vertices))))
}
