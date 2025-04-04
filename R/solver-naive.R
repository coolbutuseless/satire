

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Naive, brute-force SAT solver with up-front memory allocation
#' 
#' This solver is only suitable for small problems as memory use is exponential in 
#' the number of variables and all memory is pre-allocated. E.g. a problem with
#' 21 variables will pre-allocate about a gigabyte of memory.  Each extra
#' variable doubles the memory required - e.g. a problem with 32 variables 
#' would require allocation of 520 GB of memory.
#' 
#' @param sat SAT problem as created by \code{\link{sat_new}()}
#' @param remove_dummy Remove dummy variables from result. Default: TRUE. Dummy 
#'        variables are used in many places (e.g. cardinality constraints). These 
#'        variables are not part of the user's problem statement, and most 
#'        of the time should not be present in the solution.  A variable is
#'        considered a dummy variable if it starts with the word "dummy".
#' @param remove_dupes Remove duplicate solutions? Default: TRUE
#' @param mem_limit only run problems if the estimated memory allocation is
#'        less than this number of MB.  Default: 1024.  This is a guard 
#'        to prevent catastrophic consequences of trying to allocate too much
#'        memory if given a large problem.
#' @param verbosity verbosity level.  Default: 0
#' @return data.frame of logical values. Columns correspond to the variable 
#'         names within the problem.  Each row defines a solution. If problem
#'         is unsatisfiable, return NULL.
#' @examples
#' sat <- sat_new()
#' sat_card_atmost_k(sat, letters[1:4], 3)
#' sat$exprs
#' sat$names
#' sat_solve_naive(sat)
#' @export 
#' @family SAT solvers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_solve_naive <- function(sat, remove_dummy = TRUE, remove_dupes = TRUE, mem_limit = 1024, verbosity = 0L) {
  
  assert_is_sat_prob(sat)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check that this SAT problem has variable names
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nvars <- length(sat$names)
  stopifnot(nvars >= 1)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Estimate memory usage. 'nvar' columns with 2^nvars logical values
  # Each logical value is stored in an int (i.e. 4 bytes)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  est_mem_mb <- nvars * (2 ^ nvars) * 4 / 1024 / 1024
  if (verbosity > 0) {
    cat("#vars =", nvars, ".  Estimated memory: ", est_mem_mb, "MB\n", sep = "")
  }
  if (est_mem_mb > mem_limit) {
    stop(sprintf("Estimated required memory > mem_limit: %.0g MB > %.0f MB", est_mem_mb, mem_limit));
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use expand.grid to create a data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  args <- rep(list(c(T, F)), nvars)
  names(args) <- sat$names
  choices <- do.call(expand.grid, args)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a string of the full boolean expression to be evaluated
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  full_expr <- paste("(", sat$exprs, ")", collapse = " & ")
  full_expr <- parse(text = full_expr)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Evaluate the expression in the context of the boolean universe
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lgl <- with(choices, eval(full_expr))
  table(lgl)
  solns <- choices[lgl, , drop = FALSE]
  
  if (nrow(solns) == 0) {
    # Unsatisfiable
    return(NULL)
  }
  
  if (remove_dummy) {
    vars  <- sat$names[!grepl("^dummy", sat$names)]
    solns <- solns[, vars, drop = FALSE]
  }
  
  if (remove_dupes) {
    solns <- solns[!duplicated(solns), , drop = FALSE]
  }
  
  rownames(solns) <- NULL
  solns
}




if (FALSE) {
  library(satire)
  
  sat <- sat_new()
  sat_card_atmost_k(sat, letters[1:6], 3)
  sat$exprs
  sat$names
  
  sat_solve_naive(sat)
}



