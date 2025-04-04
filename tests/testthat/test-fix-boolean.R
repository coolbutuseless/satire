

# Conversion rules
#  ==    f == g
#        (f -> g) & (g -> f)
#
# ->     f -> g
#        !f | g
#
# !      !(f & g)
#        !f | !g
#
# !      !(f | g)
#        !f & !g
#
# !!     !!f
#          f


test_that("fix_boolean() implication works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # top level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("f -> g")
  f <- fix_boolean(e)
  
  expect_equal(
    as.character(as.expression(f)),
    "!f | g"
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # top level complex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("f -> (a & b)")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "!f | (a & b)"
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sub level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("a | (f -> g)")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "a | (!f | g)"
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sub level complex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("a | (f -> (a & b))")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "a | (!f | (a & b))"
  )
  
  
})



test_that("fix_boolean() equivalence works", {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Top level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("f == g")
  f <- fix_boolean(e)
  
  expect_equal(
    as.character(as.expression(f)),
    "(!f | g) & (!g | f)"
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sub level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("a & (f == g)")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "a & ((!f | g) & (!g | f))"
  )
  
})



test_that("fix_boolean() double negation works", {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # top level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("!!g")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "g"
  )
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # top level complex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("!!(a & b)")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "(a & b)"
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sub-level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("f | !!g")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "f | g"
  )
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # sub-level complex
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  e <- str2lang("f | !!(a & b)")
  f <- fix_boolean(e)
  f
  
  expect_equal(
    as.character(as.expression(f)),
    "f | (a & b)"
  )
  
})
