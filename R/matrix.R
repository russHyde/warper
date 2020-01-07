#' Converts a data-frame into a matrix
#'
#' If the user specifies `rowname_col`, this column is extracted from the
#' data-frame and used to define the rownames of the matrix (the original
#' column is removed).
#'
#' @param        x             A data-frame to be converted to a matrix.
#' @param        rowname_col   An (optional) column of the data-frame that is
#'   to be extracted into the rownames of the returned matrix.
#'
#' @export

as_matrix <- function(x, rowname_col = NULL) {
  stopifnot(is.data.frame(x))

  if (is.null(rowname_col)) {
    as.matrix(x)
  } else {
    new_rownames <- x[[rowname_col]]
    required_cols <- setdiff(colnames(x), rowname_col)
    mat <- as.matrix(x[required_cols])
    rownames(mat) <- new_rownames
    mat
  }
}

###############################################################################

#' Generic function `merge` for merging two matrices by row-names
#'
#' A matrix is returned.
#'
#' @param        x,y           Matrices.
#' @param        ...           Arguments for passing through to base::merge
#'   (the data.frame method). Do not specify a `by` argument.
#' @export

merge.matrix <- function(x, y, ...) {
  stopifnot(is.matrix(y))

  as_matrix(
    merge.data.frame(x, y, by = 0, ...),
    rowname_col = "Row.names"
  )
}
