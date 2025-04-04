

test_that("tseytin works", {
  
  # Compound term
  e <- "!(a | b)"
  f <- tseytin_transform(e)
  
  fsat <- sat_new()
  sat_add_exprs(fsat, f)
  fres <- sat_solve_naive(fsat)
  
  
  e2 <- "(!a) & (!b)" # by de morgan
  esat <- sat_new()
  sat_add_exprs(esat, e2)
  esat$exprs
  eres <- sat_solve_naive(esat)
  
  expect_identical(
    order_canonically(fres),
    order_canonically(eres)
  )
  
  
  
  # Compound term
  e <- "a -> (b & c)"
  f <- tseytin_transform(e)
  f

  fsat <- sat_new()
  sat_add_exprs(fsat, f)
  fres <- sat_solve_naive(fsat)
  fres
  
  # a -> (b & c)
  # !a | (b & c)
  # (!a | b) & (!a | c) # Distribution rule
  e2 <- "(!a | b) & (!a | c)" # by de morgan
  esat <- sat_new()
  sat_add_exprs(esat, e2)
  esat$exprs
  eres <- sat_solve_naive(esat)

  expect_identical(
    order_canonically(fres),
    order_canonically(eres)
  )

  
  
  
})
