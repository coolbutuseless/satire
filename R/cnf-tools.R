

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Determine if the expression (in a string) is in conjunctive normal form
#' 
#' @param s single string containing an expression
#' @return Logical value. TRUE if expression is a CNF. Return FALSE if 
#'         expression is a valid boolean formula but is not in CNF.  
#'         Otherwise an error is raised.
#' @examples
#' # Expect TRUE for these CNF expressions
#' is_cnf("a | !b") 
#' is_cnf("(a | !b) & (c | d | e)") 
#' 
#' # Expect FALSE for these expressions
#' is_cnf("!!a")
#' is_cnf("!(a & b)")
#' is_cnf("!(a | b)")
#' is_cnf("a & (b | (d & e))")
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_cnf <- function(s) {
  if (is.character(s)) {
    e <- str2lang(s)
  } else if (is.language(s)) {
    e <- s
  } else {
    stop("Argument unknown: ", deparse1(s))
  }
  
  
  res <- is_cnf_core(e)
  if (is.na(res)) {
    stop(
      "Expression not valid: ", 
      "  Valid Boolean formula must only contain: c('(', ')', '|', '&', '!', '->', '==') and variable names\n", 
      deparse1(s))
  }
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal recursive function for \code{is_cnf()}
#' @param e language object. expression
#' @param within_or is the current expression nested within an "or" statement?
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_cnf_core <- function(e, within_or = FALSE) {
  if (is.name(e)) {
    return(TRUE)
  } else if (is.call(e)) {
    if (e[[1]] == "==" || e[[1]] == "<-") {
      return(FALSE)
    } else if (e[[1]] == "|") {
      # Both sides of "|" must be CNF
      return(is_cnf_core(e[[2]], within_or = TRUE) && 
             is_cnf_core(e[[3]], within_or = TRUE))  
    } else if (e[[1]] == "&") {
      # This must not occur if we're already nested within an OR 
      # Both sides of "&" must be CNF
      return(!within_or && is_cnf_core(e[[2]]) && is_cnf_core(e[[3]]))  
    } else if (e[[1]] == "!") {
      # Only a NAME is allowed after !
      # This means that "!(a)" is not considered valid here.
      return(is.name(e[[2]]))
    } else if (e[[1]] == '(') {
      return(is_cnf_core(e[[2]], within_or = within_or))
    }
  }
  
  NA
}



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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Replace implilcation, equivalence and double-negation within an AST
#' 
#' @param e expression
#' @param depth current depth
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
fix_boolean <- function(e, depth = 1) {
  if (is.name(e)) {
    return(e)
  } else if (is.call(e)) {
    if (e[[1]] == '==') {
      #  ==    f == g
      #        (f -> g) & (g -> f)
      lhs <- e[[2]]
      rhs <- e[[3]]
      e[[1]] <- as.symbol("&")
      e[[2]] <- bquote(.(lhs) -> .(rhs))
      e[[3]] <- bquote(.(rhs) -> .(lhs))
    } else if (e[[1]] == '<-') {
      lhs <- e[[3]]
      rhs <- e[[2]]
      e[[1]] <- as.symbol("|")
      e[[2]] <- bquote(!.(lhs))
      e[[3]] <- rhs
    } else if (e[[1]] == '!') {
      # print(e)
      f <- e[[2]]
      # print(f)
      if (is.call(f) && f[[1]] == '!') {
        # double bang
        # replace "!!x" by just "x"
        if (is.name(f[[2]])) return(f[[2]])
        e <- f[[2]]
      }
    } else if (e[[1]] == '|' || 
               e[[1]] == '&' ||
               e[[1]] == '(' ) {
      # Allowed, but no rewriting necessary
    } else {
      stop(sprintf("[1] fix(%s, depth = %i)", deparse1(e), depth))
    }
  } else {
    stop(sprintf("[1] fix(%s, depth = %i)", deparse1(e), depth))
  }
  
  
  func <- e[[1]]
  args <- e[-1]
  args <- lapply(args, fix_boolean, depth = depth + 1)
  
  as.call(c(func, args))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Convert an expression to a character
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_str <- function(expr) {
  if (is.character(expr)) {
    return(expr)
  } 
  as.character(as.expression(expr))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert an arbitrary Boolean expression to conjunctive normal form (CNF)
#' 
#' Expression may include nesting with brackets, \code{&}, \code{|}, \code{!},
#' \code{->} (implication) and \code{==} (equivalence). All other elements in 
#' the expression must be names. Names must only contain a-z, A-Z, 0-9 and underscore.
#'
#' @param expr Boolean expression
#' @param dummy_idx starting index for dummy numbering.  If the supplied expression
#'        is not in CNF, then Tseytin transform may be applied, and this often
#'        generates extra dummy variables.  In order to avoid variable name
#'        clashes it is necessary for the caller to provide a \code{dummy_idx} (unique
#'        within a given SAT problem) to number these dummy variables.
#'          
#' @return string containing an expression in CNF
#' @examples
#' as_cnf("a | b")     # Already CNF. returns the argument unchanged
#' as_cnf("a -> b")    # Rewrite via substitution
#' as_cnf("!!a & b")   # Rewrite via substitution
#' as_cnf("!(a | b)")  # Uses Tseytin transform
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
as_cnf <- function(expr, dummy_idx = 1L) {
  if (is_cnf(expr)) {
    return(as_str(expr))
  }
  
  if (is.character(expr)) {
    expr <- str2lang(expr)
  } else if (is.language(expr)) {
    expr <- expr
  } else {
    stop("Argument unknown: ", deparse1(expr))
  }
  
  expr <- fix_boolean(expr)
  if (is_cnf(expr)) return(as_str(expr))
  
  tt <- tseytin_transform(expr, dummy_idx = dummy_idx)
  paste(tt, collapse = " & ")
}


