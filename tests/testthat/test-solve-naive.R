
test_that("sat_solve_naive() works", {
  
  sat <- sat_new()
  sat_card_atmost_k(sat, letters[1:4], 3)
  sat$exprs
  sat$names
  
  solns <- sat_solve_naive(sat)
  solns  
  
  
  # construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(
      FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
      FALSE, TRUE, FALSE
    ),
    b = c(
      TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE,
      TRUE, FALSE, FALSE
    ),
    c = c(
      TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE
    ),
    d = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    )
  )
  expect_true(all(rowSums(ref) <= 3))
  
  expect_equal(solns, ref)
})



test_that("sat_solve_naive() works", {
  
  sat <- sat_new()
  sat_card_atleast_k(sat, letters[1:4], 3)
  sat$exprs
  sat$names
  
  solns <- sat_solve_naive(sat)
  solns  
  
  
  # construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    b = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    c = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    d = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_true(all(rowSums(ref) >= 3))
  
  expect_equal(solns, ref)
})
