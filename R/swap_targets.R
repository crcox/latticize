#' Swap targets between a pair of edges
#'
#' Given two directed edges a->b and c->d, return a->d and c->b.
#'
#' @param edges A 2x2 matrix. Each column is an edge defined by a source
#'   (row 1) and target (row 2) vertex.
#' @return A matrix with the same dimensions as \code{edges}, but with
#'
#' @export
swap_targets <- function(edges) {
    assertthat::assert_that(all(dim(edges) == 2))
    return(matrix(edges[c(1, 4, 3, 2)], nrow = 2,
           dimnames = list(c("source", "target"), NULL)))
}
