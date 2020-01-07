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
