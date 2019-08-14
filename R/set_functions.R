###############################################################################

#' Generic function `as_biadjacency` which returns a biadjacency / incidence
#' matrix
#'
#' @param        x             An object to turn into a biadjacency Matrix
#' @param        ...           Further arguments.
#'
#' @export
#'
as_biadjacency <- function(x, ...) {
  UseMethod("as_biadjacency", x)
}

#' Given a list of vectors, this function returns a biadjacency matrix
#' indicating whether a given element (row) is present in a given vector
#' (column)
#'
#' @param        x             A list of vectors; each entry should be a subset
#'   of \code{universe}.
#' @param        universe      A vector. Incidence mappings will only be
#'   returned for entries that are present in this vector. If missing/NULL, the
#'   union of the genesets is taken to be the universe.
#' @param        ...           Further arguments (unused at present).
#'
#' @importFrom   purrr         reduce
#' @importFrom   Matrix        Matrix
#'
#' @export
#'
as_biadjacency.list <- function(x,
                                universe = NULL,
                                ...) {
  if (length(x) == 0) {
    stop("`x` should contain at least one set in `as_biadjacency`")
  }

  if (is.null(universe)) {
    universe <- sort(unique(purrr::reduce(x, c)))
  }

  if (any(duplicated(universe))) {
    stop("`universe` should contain unique values in `as_biadjacency`")
  }

  adj <- Matrix::Matrix(
    0,
    nrow = length(universe), ncol = length(x),
    dimnames = list(universe, names(x)),
    sparse = TRUE
  )

  for (j in seq_along(x)) {
    adj[, j] <- 1 * universe %in% x[[j]]
  }

  adj
}

###############################################################################
