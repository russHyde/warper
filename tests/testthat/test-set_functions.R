###############################################################################

context("Tests for set functions")

###############################################################################

test_that("make_biadjacency_from_list", {
  expect_error(
    as_biadjacency(),
    info = "The list `x` should be defined in `as_biadjacency`"
  )

  expect_error(
    as_biadjacency(list()),
    info = "The list `x` should contain at least one set in `as_biadjacency`"
  )

  set_list1 <- list(a = letters[1:3])
  matrix1 <- Matrix::Matrix(
    1,
    nrow = 3, ncol = 1, dimnames = list(letters[1:3], "a"), sparse = TRUE
  )
  expect_equal(
    as_biadjacency(set_list1),
    matrix1,
    info = "A single-set set-list"
  )

  set_list2 <- list(a = letters[1:2], b = letters[2:3])
  matrix2 <- Matrix::Matrix(
    c(1, 1, 0, 0, 1, 1),
    nrow = 3, ncol = 2,
    dimnames = list(letters[1:3], letters[1:2]),
    sparse = TRUE
  )
  expect_equal(
    as_biadjacency(set_list2),
    matrix2,
    info = "Two overlapping sets"
  )

  set_list3 <- list(a = letters[1:3])
  universe3 <- letters[1:4]
  matrix3 <- Matrix::Matrix(
    c(1, 1, 1, 0),
    nrow = 4, ncol = 1, dimnames = list(letters[1:4], "a"),
    sparse = TRUE
  )
  expect_equal(
    as_biadjacency(set_list3, universe = universe3),
    matrix3,
    info = "Universe is a superset of the setlist"
  )

  set_list4 <- list(X = "b", Y = "a")
  matrix4 <- Matrix::Matrix(
    c(0, 1, 1, 0),
    nrow = 2, ncol = 2, dimnames = list(letters[1:2], c("X", "Y")),
    sparse = TRUE, doDiag = FALSE
  )
  expect_equal(
    as_biadjacency(set_list4),
    matrix4,
    info = "If no universe, rownames should be sorted in the output"
  )

  expect_error(
    as_biadjacency(list(X = "a"), universe = c("a", "a")),
    info = "Universe should not contain duplicated values"
  )
})

###############################################################################
