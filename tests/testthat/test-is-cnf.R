

test_that("is_cnf() works", {
  
  cnfs <- c(
    'a',
    '!a',
    'a | b',
    'a | !b',
    "a | b | c",
    
    '(a | b)',
    "(a)",
    "(a | b)",
    "(a | b) & (c)",
    "(a | !b | !c) & (!d | e | f | d | f)"
  )
  
  for (cnf in cnfs) {
    expect_true(is_cnf(cnf))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # These are boolean logic, but not in CNF
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  not_cnfs <- c(
    "!(a & b)",
    "!(a | b)",
    "a & (b | (d & e))"
  )
  
  for (e in not_cnfs) {
    expect_false(is_cnf(e))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # These should all cause errors as they are not boolean logic
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  not_boolean <- c(
    "1 & 1",
    "mean(3)",
    'a && b'
  )
  for (e in not_boolean) {
    expect_error(is_cnf(e))
  }
})


