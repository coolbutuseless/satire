


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This is a worked example taken from some web page.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Assign names to all the non-atomic subformulas
# E.g. !(a & (!a | b)) | c
#     x1 == x2 | c
#     x2 == !x3
#     x3 == a & x4
#     x4 == x5 | b
#     x5 == !a


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a boolean formula to CNF using Tseytin's transformation
#' 
#' @param expr expression (as string or language object)
#' @param dummy_idx value to use for numbering the dummy variables.
#' @return character vector of expressions in conjunctive normal form
#' @examples
#' tseytin_transform("a | (b & c)")
#' tseytin_transform("!(a | b)")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
tseytin_transform <- function(expr, dummy_idx = 1L) {

  if (is.character(expr)) {
    expr <- str2lang(expr)
  } else if (is.expression(expr)) {
    expr <- expr[[1]]
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If this expression is already in CNF, then just return it.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_cnf(expr)) {
    return(as_str(expr))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Substitute for 
  #   *  `->` implication
  #   *  `==` equivalance
  #   *  `!!` double negatives
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  expr <- fix_boolean(expr)
  if (is_cnf(expr)) {
    return(as_str(expr))
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Initialise the epxressions workspace
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  exprs <- list()
  exprs[[1L]] <- expr
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Prepare the core name for dummy variables 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dstr <- sprintf("dummy_%i", dummy_idx)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tseytin Step 1: Name all the non-atomic sub-formulas
  # I.e. Walk the AST, and for each expression, if it isn't a simple named
  # variable, then do a substitution so that it is.
  #
  # Keep track of which which index within 'exprs' is the current one
  # Note: this does not necessary increment with every loop.  E.g. when
  # unpacking a "(" call, this does not create a new expression, but replaces
  # the current one.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  idx <- 1L
  
  while(TRUE) {
    e <- exprs[[idx]]
    
    rerun   <- FALSE
    changed <- FALSE
    if (is.call(e)) {
      if (e[[1]] == '|' || e[[1]] == '&') {
        for (i in 2:3) {
          if (!is.name(e[[i]])) {
            dx <- length(exprs) + 1L
            d  <- sprintf("%s_%03i", dstr, dx)
            exprs[[dx]] <- e[[i]]
            e[[i]] <- as.name(d)
            changed <- TRUE
          }
          exprs[[idx]] <- e
        }
      } else if (e[[1]] == '!') {
        if (!is.name(e[[2]])) {
          dx <- length(exprs) + 1L
          d  <- sprintf("%s_%03i", dstr, dx)
          exprs[[dx]] <- e[[2]]
          e[[2]] <- as.name(d)
          exprs[[idx]] <- e
          changed <- TRUE
        }
      } else if (e[[1]] == '(') {
        exprs[[idx]] <- e[[2]]
        rerun <- TRUE
        changed <- TRUE
      } else {
        stop("Call not handled: ", deparse1(expr), "\n  ==> ", deparse1(e))
      }
      
      
      # Unwrapping a "(" call does not advance the index of the current
      # expression being processed.
      if (!rerun) {
        idx <- idx + 1L
      }
    }
    
    exprs
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # If an iteration happens but no changes are made to any expressions, 
    # then we've reached the end of the road for simplification.
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    if (!changed) break
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(all(vapply(exprs, is_cnf, logical(1))))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Each expression in 'exprs' is currently of the form
  #   dummy00? == y & z
  #   dummy00? == y | z
  #   dummy00? == !y
  #
  # Where:
  #   - 'dummy00?' is the index of the expression in the 'exprs' list
  #   - '==' is logical equivalence
  #
  # There are then standard Tseytin re-write rules to turn these equations
  # into clauses
  #
  # In the following loop, each expression is re-written using these rules
  #
  #
  # x == !y
  #   (!y | !x) & (y | x)
  #
  # x == y | z
  #   (y | z | !x) & (!y | x) & (!z | x)
  #
  # x == y & z
  #   (!x | y) & (!x | z) & (!y | !z | x)
  #
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (idx in seq_along(exprs)) {
    e <- exprs[[idx]]
    x <- as.name(sprintf("%s_%03i", dstr, idx))
    
    # unpack brackets
    while (e[[1]] == '(') {
      e <- e[[2]]
    }
    
    if (e[[1]] == '!') {
      y <- e[[2]]
      exprs[[idx]] <- bquote((!.(y) | !.(x)) & (.(y) | .(x)))
    } else if (e[[1]] == '|') {
      y <- e[[2]]
      z <- e[[3]]
      exprs[[idx]] <- bquote((.(y) | .(z) | !.(x)) & (!.(y) | .(x)) & (!.(z) | .(x)))
    } else if (e[[1]] == '&') {
      y <- e[[2]]
      z <- e[[3]]
      exprs[[idx]] <- bquote((!.(x) | .(y)) & (!.(x) | .(z)) & (!.(y) | !.(z) | .(x)))
    } else {
      stop("Unknown Tseytin translation for op: ", e[[1]])
    }
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # In order to complete the set of clauses, the expression which represents
  # the original clause must be true. i.e. 'dummy_001' is a true clause
  # in the boolean formulat
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  exprs <- append(exprs, as.name(paste0(dstr, "_001")), after = 0)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  stopifnot(all(vapply(exprs, is_cnf, logical(1))))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all the strings and return
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  vapply(exprs, \(x) as.character(as.expression(x)), character(1))
}



if (FALSE) {
  e1 <- str2lang("!(a & (!a | b)) | c")
  expr <- parse(text = "!(a | (b & c))")
  
  tseytin_transform(expr)
}






