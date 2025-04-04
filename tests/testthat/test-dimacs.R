
test_that("multiplication works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- read_dimacs(testthat::test_path("test.dimacs"))
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    v1 = c(FALSE, FALSE, FALSE, TRUE),
    v2 = c(FALSE, FALSE, TRUE, FALSE),
    v3 = c(FALSE, TRUE, FALSE, FALSE),
    v4 = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  expect_equal(
    sat$literals,
    c(
      -1L, -2L, 0L, -1L, -3L, 0L, -1L, -4L, 0L, -2L, -3L, 0L, -2L, -4L, 0L, -3L,
      -4L, 0L, 1L, 2L, 3L, 4L, 0L
    )
  )
  
  expect_output(
    print(sat$dimacs),
    "p cnf"
  )
  
  expect_output(
    print(sat),
    "<sat>"
  )
  
  expect_identical(
    sat_literals_to_lgl(sat, c(1, 2, -1)),
    c(v1 = TRUE, v2 = TRUE, v1 = FALSE)
  )
})
