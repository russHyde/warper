###############################################################################

context("Tests for matrix creation / manipulation")

###############################################################################

test_that("as_matrix", {
  df1 <- data.frame(a = 1:3)
  m1 <- matrix(1:3, nrow = 3, dimnames = list(NULL, c("a")))
  expect_equal(
    as_matrix(df1),
    m1,
    info = "no row-index in the input data-frame"
  )

  df2 <- data.frame(a = 1:3, b = letters[1:3])
  m2 <- matrix(1:3, nrow = 3, dimnames = list(letters[1:3], "a"))
  expect_equal(
    as_matrix(df2, rowname_col = "b"),
    m2,
    info = "df -> matrix using second column as rownames"
  )

  df3 <- data.frame(a = letters[1:3], b = 1:3)
  m3 <- matrix(1:3, nrow = 3, dimnames = list(letters[1:3], "b"))
  expect_equal(
    as_matrix(df3, rowname_col = "a"),
    m3,
    info = "df -> matrix using first column as rownames"
  )
})

###############################################################################

test_that("merge two matrices by matching rownames", {
  m1 <- matrix(1:3, nrow = 3, dimnames = list(c("x", "y", "z"), c("a")))
  m2 <- matrix(4:9, nrow = 3, dimnames = list(c("x", "y", "z"), c("b", "c")))

  expect_equivalent(
    merge(m1, m2),
    matrix(
      1:9,
      nrow = 3, dimnames = list(letters[24:26], letters[1:3])
    )
  )

  expect_equivalent(
    merge(m1, m2[1:2, ]),
    matrix(
      c(1, 2, 4, 5, 7, 8),
      nrow = 2, dimnames = list(letters[24:25], letters[1:3])
    )
  )

  expect_equivalent(
    merge(m1, m2[1:2, ], all.x = TRUE),
    matrix(
      c(1, 2, 3, 4, 5, NA, 7, 8, NA),
      nrow = 3, dimnames = list(letters[24:26], letters[1:3])
    )
  )
})
