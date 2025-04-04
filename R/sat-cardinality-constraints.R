

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add to SAT problem the cardinality constraint for "exactly one", "at most one"
#' or "at least one" of the 
#' given variables is true
#' 
#' @inheritParams sat_add_literals
#' @param nms character vector of variable names
#' @return a character vector of expression strings representing the cardinality constraint
#'         in conjunctive normal form
#' @param method nethod to use to encode this constraint. Default: 'pairwise'
#' \describe{
#'   \item{\code{pairwise}}{Generates zero new variables, but \code{O(n^2)} new clauses}
#'   \item{\code{commander}}{Divide-and-conquer approach}
#' }
#' @examples
#' sat <- sat_new()
#'
#' sat_card_exactly_one(sat, c('a', 'b', 'c', 'd'))
#' sat$exprs
#'
#' sat <- sat_new()
#' sat_card_atleast_one(sat, c('a', 'b', 'c', 'd'))
#' sat$exprs
#'
#' sat <- sat_new()
#' sat_card_atmost_one(sat, c('a', 'b', 'c', 'd'))
#' sat$exprs
#' @export
#' @family SAT cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_exactly_one <- function(sat, nms, method = 'pairwise') {
  assert_is_sat_prob(sat)
  
  exprs <- card_exactly_one(nms, method = method)
  sat_add_exprs(sat, exprs)
  
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sat_card_exactly_one
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_atleast_one <- function(sat, nms) {
  assert_is_sat_prob(sat)
  
  exprs <- card_atleast_one(nms)
  sat_add_exprs(sat, exprs)
  
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sat_card_exactly_one
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_atmost_one <- function(sat, nms, method = 'pairwise') {
  assert_is_sat_prob(sat)
  
  exprs <- card_atmost_one(nms, method = method)
  sat_add_exprs(sat, exprs)
  
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add to SAT problem the cardinality constraint that "exactly k", "at most k"
#' or "at least k" of the given variables are true
#' 
#' This constraint is an implementation of Sinz's LTseq formulation.
#' 
#' @inheritParams sat_add_literals
#' @inheritParams sat_card_exactly_one
#' @param k k limit
#' @return a character vector of expression strings representing the cardinality constraint
#'         in conjunctive normal form
#' @examples
#' sat <- sat_new()
#' sat_card_exactly_k(sat, c('a', 'b', 'c', 'd'), k = 2)
#' sat$exprs
#'
#' sat <- sat_new()
#' sat_card_atleast_k(sat, c('a', 'b', 'c', 'd'), k = 2)
#' sat$exprs
#'
#' sat <- sat_new()
#' sat_card_atmost_k(sat, c('a', 'b', 'c', 'd'), k = 2)
#' sat$exprs
#' @export
#' @family SAT cardinality constraints
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_exactly_k <- function(sat, nms, k) {
  assert_is_sat_prob(sat)
  
  dummy_idx <- sat$.internal$dummy_idx
  exprs <- card_exactly_k(nms = nms, k = k, dummy_idx = dummy_idx)
  sat_add_exprs(sat, exprs)
  sat$.internal$dummy_idx <- sat$.internal$dummy_idx + 1L
  
  invisible(sat)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sat_card_exactly_k
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_atleast_k <- function(sat, nms, k) {
  assert_is_sat_prob(sat)
  
  dummy_idx <- sat$.internal$dummy_idx
  exprs <- card_atleast_k(nms = nms, k = k, dummy_idx = dummy_idx)
  sat_add_exprs(sat, exprs)
  sat$.internal$dummy_idx <- sat$.internal$dummy_idx + 1L
  
  invisible(sat)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname sat_card_exactly_k
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_card_atmost_k <- function(sat, nms, k) {
  assert_is_sat_prob(sat)
  
  dummy_idx <- sat$.internal$dummy_idx
  exprs <- card_atmost_k(nms = nms, k = k, dummy_idx = dummy_idx)
  sat_add_exprs(sat, exprs)
  sat$.internal$dummy_idx <- sat$.internal$dummy_idx + 1L
  
  invisible(sat)
}




