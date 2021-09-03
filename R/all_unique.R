#' Test that all values are unique in an array or vector
#'
#' @param x A numeric or array object.
#' @return logical
#'
#' @export
all_unique <- function(x) {
    return(!any(duplicated(x, MARGIN = 0)))
}
