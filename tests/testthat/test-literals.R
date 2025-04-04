
test_that("literals work", {
  
  s <- sat_new()
  sat_add_literals(s, c(1, 2))
  sat_add_literals(s, c(3, 4, 0))
  sat_add_literals(s, c(4))
  
  expect_equal(
    s$literals,
    c(1, 2, 0, 3, 4, 0, 4, 0)
  )
  
  expect_equal(
    s$unique_literals,
    c(0, 1, 2, 3, 4)
  )
})



test_that("expressions work", {
  
  s <- sat_new()
  sat_add_exprs(s, "a | b")
  sat_add_exprs(s, "c | d")
  sat_add_exprs(s, "d")
  
  expect_equal(
    s$literals,
    c(1, 2, 0, 3, 4, 0, 4, 0)
  )
  
  expect_equal(
    unlist(s$.internal$name_to_int),
    c(a = 1L, b = 2L, c = 3L, d = 4L)
  )
  
  expect_equal(
    s$named_literals,
    c(a = 1L, b = 2L, c = 3L, d = 4L)
  )
  

  expect_equal(
    s$names,
    c('a', 'b', 'c', 'd')
  )
  
  expect_equal(
    s$exprs,
    c('a | b', 'c | d', 'd')
  )
  
})




test_that("conjunctive expressions work", {
  
  
  s <- sat_new()
  sat_add_exprs(s, "(a | b) & (c | d) & d")
  expect_equal(
    s$literals,
    c(1, 2, 0, 3, 4, 0, 4, 0)
  )
  
  
  s <- sat_new()
  expect_error(
    sat_add_exprs(s, "a || b && (c || d) & d"),
    "Boolean formula"
  )

})





test_that("can add integer literals after expressions", {
  
  s <- sat_new()
  sat_add_exprs(s, "(a | b) & (c | d) & d")
  sat_add_literals(s, c(4, 1))
  expect_equal(
    s$literals,
    c(1, 2, 0, 3, 4, 0, 4, 0, 4, 1, 0)
  )
  
})


test_that("cannot add integer literals out-of-range after expressions", {
  
  s <- sat_new()
  sat_add_exprs(s, "(a | b) & (c | d) & d")
  
  expect_error(
    sat_add_literals(s, c(8, 1)),
    "out-of-range"
  )

})


test_that("cannot add expressions after first add is integer literals", {
  
  s <- sat_new()
  sat_add_literals(s, c(2, 1))
  expect_error(
    sat_add_exprs(s, "a | b & c | d & d"),
    "Cannot add"
  )
  
})
