
test_that("cardinatlity one (pairwise) works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_exactly_one(sat, c('a', 'b', 'c', 'd'))
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(FALSE, FALSE, FALSE, TRUE),
    b = c(FALSE, FALSE, TRUE, FALSE),
    c = c(FALSE, TRUE, FALSE, FALSE),
    d = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_atleast_one(sat, c('a', 'b', 'c', 'd'))
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(
      TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE,
      TRUE, FALSE, TRUE
    ),
    b = c(
      TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE,
      TRUE, TRUE, FALSE
    ),
    c = c(
      TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE,
      FALSE, FALSE, FALSE
    ),
    d = c(
      TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE,
      FALSE, FALSE, FALSE
    )
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_atmost_one(sat, c('a', 'b', 'c', 'd'))
  solns <- sat_solve_naive(sat)
  
  ref <- data.frame(
    a = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    b = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    c = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    d = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
})


test_that("cardinatlity one (commander encoding) works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_exactly_one(sat, c('a', 'b', 'c', 'd'), method = 'commander')
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(FALSE, FALSE, FALSE, TRUE),
    b = c(FALSE, FALSE, TRUE, FALSE),
    c = c(FALSE, TRUE, FALSE, FALSE),
    d = c(TRUE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_atmost_one(sat, c('a', 'b', 'c', 'd'), method = 'commander')
  solns <- sat_solve_naive(sat)
  
  ref <- data.frame(
    a = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    b = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    c = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    d = c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
})



test_that("cardinatlity k=1 works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_exactly_k(sat, c('a', 'b', 'c', 'd'), k = 2)
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(FALSE, FALSE, TRUE, FALSE, TRUE, TRUE),
    b = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
    c = c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE),
    d = c(TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_atleast_k(sat, c('a', 'b', 'c', 'd'), k = 2)
  solns <- sat_solve_naive(sat)
  
  # constructive::construct(solns, template = list(opts_atomic(compress = FALSE)))
  ref <- data.frame(
    a = c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE),
    b = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE),
    c = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE),
    d = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  sat_card_atmost_k(sat, c('a', 'b', 'c', 'd'), k = 2)
  solns <- sat_solve_naive(sat)
  
  ref <- data.frame(
    a = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE),
    b = c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE),
    c = c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE),
    d = c(TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  
  expect_equal(
    order_canonically(solns),
    order_canonically(ref)
  )  
})
