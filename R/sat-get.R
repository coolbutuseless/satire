




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get current state as a vector of named expressions
#' 
#' This call is only valid if the SAT problem was created with expressions via
#' \code{\link{sat_add_exprs}()}.
#' 
#' @inheritParams sat_add_literals
#' @return Character vector of named expressions
#' @examples
#' sat <- sat_new()
#' sat_add_exprs(sat, "!a | b")
#' sat_add_exprs(sat, "!b | c")
#' sat_add_exprs(sat, "!c | a")
#' sat_get_exprs(sat)
#' @family export functions
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_get_exprs <- function(sat) {
  assert_is_sat_prob(sat)
  
  literals <- sat$literals
  
  literals <- literals[-length(literals)] # remove trailing zero
  bool <- literals >= 0
  literals <- abs(literals)
  
  vals <- unlist(sat$.internal$name_to_int, recursive = FALSE)
  idx  <- match(abs(literals), vals)
  
  exprs <- names(sat$.internal$name_to_int)[idx]
  exprs <- ifelse(bool, exprs, paste0("!", exprs))
  exprs[is.na(exprs)] <- "&"
  exprs <- paste(exprs, collapse = " | ")
  exprs <- strsplit(exprs, " \\| \\& \\| ")[[1]]
  
  exprs
}


