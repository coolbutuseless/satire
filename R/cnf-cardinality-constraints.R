

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint that "exactly one" of the given variables is true
#' 
#' @param nms character vector of variable names
#' @return a character vector of expression strings representing "exactly one"
#'         in conjunctive normal form
#' @examples
#' card_exactly_one(c('a', 'b', 'c', 'd'))
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_exactly_one <- function(nms, method = 'pairwise') {
  stopifnot(is.character(nms))
  
  c(card_atmost_one(nms, method = method), card_atleast_one(nms))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint that "at least one" of the given variables is true
#' 
#' @inheritParams card_exactly_one
#' @return a character vector of expression strings representing "at least one"
#'         in conjunctive normal form
#' @examples
#' card_atleast_one(c('a', 'b', 'c', 'd'))
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atleast_one <- function(nms) {
  stopifnot(is.character(nms))
  
  paste0("(", paste(nms, collapse = " | "), ")")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint for "at most one" of the given variables is true
#' 
#' There are multiple ways of encoding such a constraint, and this package 
#' offers a naive 'pairwise' method, and a method using recursive commander variable 
#" encoding (more efficient for larger problems).
#' 
#' @inheritParams card_exactly_one
#' @param method nethod to use to encode this constraint. Default: 'pairwise'
#' \describe{
#'   \item{\code{pairwise}}{Generates zero new variables, but \code{O(n^2)} new clauses}
#'   \item{\code{commander}}{Divide-and-conquer approach}
#' }
#' @param dummy_idx The value to be used as part of the suffix for the dummy variables
#'        created using the \code{commander} method.
#' @return a character vector of expression strings representing "at most one"
#'         in conjunctive normal form
#' @examples
#' card_atmost_one(c('a', 'b', 'c', 'd'))
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atmost_one <- function(nms, method = 'pairwise', dummy_idx = 1L) {
  stopifnot(is.character(nms))
  
  if (method == 'pairwise') {
    card_atmost_one_pairwise(nms)
  } else if (method == 'commander') {
    card_atmost_one_commander(nms, dummy_idx = dummy_idx)
  } else {
    stop("atmost_one(): Unknown method: ", method)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint for "at most one" of the given variables is true using
#' pairwise variable comparison
#' 
#' @inheritParams card_exactly_one
#' @return a character vector of expression strings representing "at most one"
#'         in conjunctive normal form
#' @examples
#' card_atmost_one_pairwise(c('a', 'b', 'c', 'd'))
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atmost_one_pairwise <- function(nms) {
  stopifnot(is.character(nms))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Pairwise: No two variables are TRUE for all pairs
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  n <- length(nms)
  exprs <- lapply(seq_len(n - 1L), \(i) {
    paste0("(!", nms[i], " | !", nms[(i+1):n], ")")
  })
  exprs <- unlist(exprs, recursive = FALSE, use.names = FALSE)
  exprs <- paste(exprs, collapse = " & ")
  
  exprs
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint that "at most one" of the given variables is true using
#' command variables
#' 
#' @inheritParams card_atmost_one
#' @param prefix default "".  prefix for the dummy variable name.  Used when
#'        operatoing recursively
#' @param recursive should command variable encoding be applied recusrively?
#'        Default: TRUE.  If FALSE, then the command variables are constrained
#'        using \code{card_atmost_one_pairwise}
#' @return a character vector of expression strings representing "at most one"
#'         in conjunctive normal form
#' @examples
#' card_atmost_one_commander(letters[1:8])
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atmost_one_commander <- function(nms, dummy_idx = 1L, prefix = "", 
                                 recursive = TRUE) {
  stopifnot(is.character(nms))
  
  if (length(nms) == 1) {
    return(character(0))
  } else if (length(nms) <= 3) {
    return(card_atmost_one_pairwise(nms))
  }  
  
  chunks <- chunkify(nms, 3)
  prefix <- paste0(prefix, "_", dummy_idx)
  
  # Commander vars
  cvars  <- paste0("dummy", prefix, "_", seq_along(chunks))
  
  atmost_one_within_chunk <- vapply(chunks, card_atmost_one_pairwise, character(1), USE.NAMES = FALSE)
  
  # Commander variable is true implies that one of the variables must be true
  # com => x1 | x2 | x3
  # is equivalent to
  # !com | x1 | x2 | x3
  commander_true_implies <- vapply(seq_along(chunks), function(i) {
    card_atleast_one(c(not(cvars[i]), chunks[[i]]))
  }, character(1), USE.NAMES = FALSE)
  
  # Commander variable is false implies that none of group vars can be true
  # !com => !x1 & !x2 & !x3
  # is equivalent to
  # (c1 | !x1) & (c2 | !x2) & (c3 | !x3)
  commander_false_implies <- vapply(seq_along(chunks), function(i) {
    paste0("(", cvars[i] %or% not(chunks[[i]]), ")", collapse = " & ")  
  }, character(1), USE.NAMES = FALSE)
  
  if (recursive) {
    commander_atmost_one <- card_atmost_one_commander(cvars, 1L, prefix = prefix)
  } else {  
    commander_atmost_one <- card_atmost_one_pairwise(cvars)
  }
  
  c(
    atmost_one_within_chunk,
    commander_true_implies, 
    commander_false_implies,
    commander_atmost_one
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Cardinality constraint for "at most k" or "at least k" of the given variables are true
#' 
#' Uses Sinz's LTseq formulation.  Algorithm translated from 
#' slide 8 of https://www.carstensinz.de/talks/CP-2005-talk.pdf
#' 
#' @inheritParams card_exactly_one
#' @param k k limit
#' @param dummy_idx The value to be used as part of the suffix for the dummy variables
#'        created. Default: 1
#' @param prefix further name part for dummy variables
#' @return a character vector of expression strings in conjunctive normal form
#' @examples
#' card_atmost_k(c('a', 'b', 'c', 'd'), 2)
#' card_atleast_k(c('a', 'b', 'c', 'd'), 2)
#' card_exactly_k(c('a', 'b', 'c', 'd'), 2)
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atmost_k <- function(nms, k, dummy_idx = 1L, prefix = "_") {
  stopifnot(is.character(nms))
  
  x <- nms
  n <- length(nms)
  stopifnot(k < n)
  stopifnot(k > 0)
  
  # Create a matrix of names for the dummy variables
  s <- paste0("dummy", prefix, dummy_idx, "_", seq((n-1) * k))
  s <- matrix(s, nrow = n-1, ncol = k)
  
  pre <- c(
    not(x[1]) %or% s[1,1],
    not(s[1, iseq(2, k)]) 
  )
  
  post <- not(x[n]) %or% not(s[n-1, k])
  
  mid_exprs <- c()
  for (i in iseq(2, n-1)) {
    expr_pre1 <- not(x[i]) %or% s[i, 1]
    expr_pre2 <- not(s[i-1, 1]) %or% s[i, 1]
    
    mid <- c()
    for (j in iseq(2, k)) {
      mid <- c(mid, c(
        not(x[i]) %or% not(s[i-1, j-1]) %or% s[i,j],
        not(s[i-1,j]) %or% s[i,j]
      ))
    }
    
    expr_post <- not(x[i]) %or% not(s[i-1, k])
    
    mid_exprs <- c(mid_exprs, expr_pre1, expr_pre2, mid, expr_post)
  }
  
  exprs <- bracket(c(
    pre,
    mid_exprs,
    post
  ))
  
  exprs
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname card_atmost_k
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_atleast_k <- function(nms, k, dummy_idx = 1L, prefix = "_") {
  stopifnot(is.character(nms))
  
  n <- length(nms)
  stopifnot(k < n)
  
  card_atmost_k(not(nms), n - k, dummy_idx = dummy_idx, prefix = prefix)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname card_atmost_k
#' @noRd
#' @family Core CNF cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
card_exactly_k <- function(nms, k, dummy_idx = 1L) {
  c(
    card_atmost_k (nms, k, dummy_idx = dummy_idx, prefix = '_lo_'),
    card_atleast_k(nms, k, dummy_idx = dummy_idx, prefix = '_hi_')
  )
}


