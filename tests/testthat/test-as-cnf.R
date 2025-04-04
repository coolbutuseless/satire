
test_that("conversion of boolean formula to CNF works", {

  e <- "a"
  expect_true(is_cnf(e))
  f <- as_cnf(e)
  expect_identical(f, e)
  f <- as_cnf(str2lang(e))
  expect_identical(f, e)
  
  e <- "a | b"
  expect_true(is_cnf(e))
  f <- as_cnf(e)
  expect_identical(f, e)
  f <- as_cnf(str2lang(e))
  expect_identical(f, e)
  
  e <- "(a | b) & (c | !d)"
  expect_true(is_cnf(e))
  f <- as_cnf(e)
  expect_identical(f, e)
  f <- as_cnf(str2lang(e))
  expect_identical(f, e)
  
  e <- "a -> b"
  expect_false(is_cnf(e))
  f <- as_cnf(e)
  expect_true(is_cnf(f))
  expect_identical(f, "!a | b")
  f <- as_cnf(str2lang(e))
  expect_identical(f, "!a | b")
  
  e <- "a == b"
  expect_false(is_cnf(e))
  f <- as_cnf(e)
  expect_true(is_cnf(f))
  expect_identical(f, "(!a | b) & (!b | a)")
  f <- as_cnf(str2lang(e))
  expect_identical(f, "(!a | b) & (!b | a)")

    
  e <- "!(a & b)"
  expect_false(is_cnf(e))
  f <- as_cnf(e)
  expect_true(is_cnf(f))
  f <- as_cnf(str2lang(e))
  expect_true(is_cnf(f))
  expect_equal(
    f,
    "dummy_1_001 & (!dummy_1_002 | !dummy_1_001) & (dummy_1_002 | dummy_1_001) & (!dummy_1_002 | a) & (!dummy_1_002 | b) & (!a | !b | dummy_1_002)"
  )
  
  
  
  
  e <- "a + b"
  expect_error(is_cnf(e))
  expect_error(as_cnf(e))
})
