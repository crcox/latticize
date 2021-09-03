#' Lattice with preserved in/out degree distribution
#'
#'   This function "latticizes" a directed network, while preserving the in-
#'   and out-degree distributions. In weighted networks, the function
#'   preserves the out-strength but not the in-strength distributions. The
#'   function can ensure that the randomized network maintains
#'   connectedness, the ability for every node to reach every other node in
#'   the network. If this is desired, the input network for this function must
#'   be connected.
#'
#'   @param x An adjacency matrix or igraph object representing the graph to
#'     mutate. It is assumed to be directed and weighted, but unweighted is
#'     fine.
#'   @param epochs rewiring parameter
#'               (each edge is rewired approximately ITER times)
#'   @param allow_disconnect A logical indicating where a mutation is allowed to
#'     disrupt the connectedness of the network. If \code{FALSE}, then \code{x}
#'     must be connected to begin with and represented as an \code{igraph} object.
#'     The connectedness check is not implemented for plain adjacency matrices.
#'
#'   @return A mutated version of the input network.
#'
#'   @references Maslov and Sneppen (2002) Science 296:910
#'               Sporns and Zwi (2004) Neuroinformatics 2:145
#'
#'   Mika Rubinov, UNSW, 2007-2010
#'   Olaf Sporns, Indiana University, 2012
#'   Chris Cox, Louisiana State University, 2021 (R port)
#'
#' @export
latticize <- function(x, epochs, allow_disconnect = TRUE) {
    x <- shuffle_vertices(x)
    d <- distance_to_diagonal(n_vertices(x))
    n_changes <- 0
    for (iter in 1:ceiling(n_edges(x) * epochs)) {
        out <- attempt_latticization(x, d, allow_disconnect)
        if (out$success)
            n_changes <- n_changes + 1
        cat(paste("iter:", iter, "changes:", n_changes, "attempts:", out$attempts), "\n")
        x <- out$x
    }
    return(unshuffle_vertices(x))
}

#' Attempt to swap the target vertices between a pair of edges
#'
#' This will typically be used in a loop to incrementally mutate a graph to
#' become more lattice-like.
#'
#' @param x An adjacency matrix or igraph object representing the graph to
#'   mutate.
#' @param d A square matrix with as many rows and columns as vertices in
#'   \code{x}, which expresses distance to the diagonal. A cannonical \code{d}
#'   can be generated with the \code{\link{distance_to_diagonal}} function.
#' @param allow_disconnect A logical indicating where a mutation is allowed to
#'   disrupt the connectedness of the network. If \code{FALSE}, then \code{x}
#'   must be connected to begin with and represented as an \code{igraph} object.
#'   The connectedness check is not implemented for plain adjacency matrices.
#' @return A mutated version of \code{x}.
#'
#' @details
#'   If \code{allow_disconnect == TRUE}, the procedure does not require
#'   \code{igraph}. If \code{FALSE}, \code{igraph} is required.
#'
#'   A mutation will occur only under the following conditions:
#'   1. The mutation will create new edges.
#'   2. The mutation will create edges that correspond to smaller values in
#'   \code{d}.
#'   3. If \code{allow_disconnect == FALSE}, the mutation must not disrupt
#'   network connectedness.
#'
#'   The test for connectedness is slow compared to the other tests and the
#'   rewiring operation itself.
#'
#' @export
attempt_latticization <- function(x, d, allow_disconnect) {
    attempt <- 0
    while (attempt < max_attempts(n_vertices(x), n_edges(x))) {
        edges <- sample_edges_with_unique_vertices(x, size = 2)
        if (rewiring_creates_new_edges(x, edges)
            && rewiring_makes_more_lattice_like(x, d, edges)
            && (allow_disconnect || rewiring_maintains_connectedness(x, edges))) {
            return(list(x = rewire_targets(x, edges),
                        attempts = attempt,
                        success = TRUE))
        }
        attempt <- attempt + 1
    }
    cat("Maximum number of attempts reached --- rewiring failed.\n")
    return(list(x = x, attempts = attempt, success = FALSE))
}

#' Equation for setting the maximum number of attempts
#'
#' @param v number of vertices
#' @param e number of edges
#' @return An integer
max_attempts <- function(v, e) {
    return(as.integer(ceiling((v * e) / (v * (v - 1)))))
}
