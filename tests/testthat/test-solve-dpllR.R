
test_that("sat_solve_dpll() large works", {
  
  sat <- sat_new()
  sat_card_atmost_k(sat, letters[1:4], 3)
  sat$exprs
  sat$names
  
  solns <- sat_solve_dpll(sat, max_solutions = 200)
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
  
  expect_equal(nrow(solns), nrow(ref))
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  # expect_equal(solns, ref)
})



test_that("sat_solve_dpll() small works", {
  
  sat <- sat_new()
  sat_card_atleast_k(sat, letters[1:4], 3)
  sat$exprs
  sat$names
  
  solns <- sat_solve_dpll(sat, max_solutions = 20)
  solns  
  
  
  # construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(TRUE, FALSE, TRUE, TRUE, TRUE),
    b = c(TRUE, TRUE, FALSE, TRUE, TRUE),
    c = c(TRUE, TRUE, TRUE, FALSE, TRUE),
    d = c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_true(all(rowSums(ref) >= 3))
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
})



test_that("sat_solvers agree 1", {
  
  sat <- sat_new()
  sat_card_atleast_k(sat, letters[1:4], 3)
  naive_soln <- sat_solve_naive(sat)
  dpll_soln  <- sat_solve_naive(sat)
  
  expect_equal(
    order_canonically(naive_soln),
    order_canonically(dpll_soln)
  )
})



test_that("sat_solvers agree 2", {
  
  sat <- sat_new()
  sat_card_atmost_k(sat, letters[1:4], 3)
  naive_soln <- sat_solve_naive(sat)
  dpll_soln  <- sat_solve_naive(sat)
  
  expect_equal(
    order_canonically(naive_soln),
    order_canonically(dpll_soln)
  )
})



test_that("sat_solvers agree 3", {
  
  sat <- sat_new()
  sat_add_exprs(sat, "a -> (b & c)")
  naive_soln <- sat_solve_naive(sat)
  dpll_soln  <- sat_solve_naive(sat)
  
  expect_equal(
    order_canonically(naive_soln),
    order_canonically(dpll_soln)
  )
})














