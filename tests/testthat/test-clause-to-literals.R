

test_that("clause-to-literal translation works", {
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test 2
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  literals <- expr_str_to_literals("alpha_01 | !beta || -gamma | delta", sat = sat)
  expect_equal(literals, c(1, -2, -3, 4, 0))
  
  literals <- expr_str_to_literals("alpha_02 | beta", sat = sat)
  expect_equal(literals, c(5, 2, 0))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Test 3
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat <- sat_new()
  literals <- expr_str_to_literals("(alpha_01 | !beta || -gamma | delta) & (alpha_02 | beta)", 
                               sat = sat)
  expect_equal(literals, c(1, -2, -3, 4, 0, 5, 2, 0))
  
})

