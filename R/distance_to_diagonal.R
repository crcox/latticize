#' Generate matrix of distances from its own diagonal
#'
#' @param n Number of vertices
#' @return Matrix
#'
#' @details Distances are determined assuming that the list of vertices loops
#'   back to the start. If there are 4 vertices, the output will be:
#'   0 1 2 1
#'   1 0 1 2
#'   2 1 0 1
#'   1 2 1 0
#'   The value at row 1 col 4 is 1 because vertex 4 is considered to be adjacent
#'   to vertex 1.
#'
#' @export
distance_to_diagonal <- function(n) {
    x <- matrix(0:(n - 1), n, n)
    y <- abs(x - t(x))
    return(pmin(y, n - y))
}
