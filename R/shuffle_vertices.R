#' Shuffle the vertices of a graph or adjacency matrix
#'
#' Vertices are randomly sampled without replacement to generate a shuffled
#' version of the input.
#'
#' @param x An adjacency \code{matrix}
#' @param g An \code{igraph} object.
#' @return A shuffled version of the input
#'
#' @details The index used to shuffle the vertices is stored as an attribute,
#'   which is referenced by the \code{unshuffle_vertices} function to get back
#'   to the original order.
#'
#' @export
shuffle_vertices <- function(x) {
    UseMethod("shuffle_vertices", x)
}

#' @rdname shuffle_vertices
#' @export
shuffle_vertices.matrix <- function(x) {
    ix <- sample(nrow(x))
    m <- x[ix, ix]
    attr(m, "shuffle_vertices_index") <- ix
    return(m)
}

#' @rdname shuffle_vertices
#' @export
shuffle_vertices.igraph <- function(g) {
    ix <- sample(igraph::vcount(g))
    g <- igraph::permute(g, ix)
    attr(g, "shuffle_vertices_index") <- ix
    return(g)
}

#' @rdname shuffle_vertices
#' @export
unshuffle_vertices <- function(x) {
    UseMethod("unshuffle_vertices", x)
}

#' @rdname shuffle_vertices
#' @export
unshuffle_vertices.matrix <- function(x) {
    xi <- order(attr(x, "shuffle_vertices_index"))
    return(x[xi, xi])
}

#' @rdname shuffle_vertices
#' @export
unshuffle_vertices.igraph <- function(g) {
    xi <- order(attr(g, "shuffle_vertices_index"))
    return(igraph::permute(g, xi))
}
