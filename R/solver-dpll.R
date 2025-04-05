

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Problem is UNSATISFIABLE if any clause is empty
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_unsat <- function(state) {
  stopifnot(is.environment(state))
  any(apply(state$grid, 1, \(x) all(is.na(x))))
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# No clauses left, so satisfiable and any unassigned variables 
# can take any value
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
is_sat <- function(state) {
  stopifnot(is.environment(state))
  nrow(state$grid) == 0
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Returns the indexes of unit clauses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_unit_clause_idxs <- function(state) {
  clause_cardinality <- apply(state$grid, 1, \(x) sum(!is.na(x)))
  clause_cardinality
  which(clause_cardinality == 1) |> rev()
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Find variable for unit clause
# Returns the var index
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_unit_clause_var_idx <- function(state, clause_idx) {
  var_idx <- which(!is.na(state$grid[clause_idx, ]))
  if (length(var_idx) != 1) {
    stop("find_unit_clause_var_idx(): Clause ", clause_idx, " expected to be a unit clause but found var indexes = ", deparse1(var_idx))
  }
  var_idx
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the given 'var_idx' variable to be the value 'var_value'
# var_value is either -1 or 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_value <- function(state, var_idx, var_value) {
  stopifnot(var_value %in% c(-1, 1))
  
  # Record the value in the master list
  state$vars[var_idx] <- var_value
  
  # Remove every clauses which contains this signed-literal
  vals        <- state$grid[, var_idx, drop = TRUE]
  clause_idxs <- which(!is.na(vals) & vals == var_value)
  # message("Set Value: Removing clauses: ", deparse1(clause_idxs))
  state$grid  <- state$grid[-clause_idxs, , drop = FALSE]
  
  # Remove every occurrence of the complement of this literal from other clauses
  vals        <- state$grid[, var_idx, drop = TRUE]
  clause_idxs <- which(!is.na(vals) & vals == -var_value)
  state$grid[clause_idxs, var_idx] <- NA_integer_
  
  invisible(state)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set the given 'var_idxs' variables to be the values 'var_values'
# var_value is either -1 or 1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
set_values <- function(state, var_idxs, var_values) {
  stopifnot(all(var_values %in% c(-1, 1)))
  stopifnot(length(var_idxs) == length(var_values))
  
  # Record the value in the master list
  state$vars[var_idxs] <- var_values
  
  # Remove every clauses which contains this signed-literal
  clause_idxs <- list()
  for (i in seq_along(var_idxs)) {
    var_idx   <- var_idxs[i]
    var_value <- var_values[i]
    vals        <- state$grid[, var_idx, drop = TRUE]
    clause_idxs[[i]] <- which(!is.na(vals) & vals == var_value)
    # message("Set Value: Removing clauses: ", deparse1(clause_idxs))
  }
  clause_idxs <- unlist(clause_idxs, recursive = FALSE, use.names = FALSE)
  
  state$grid  <- state$grid[-clause_idxs, , drop = FALSE]
  
  # Remove every occurrence of the complement of this literal from other clauses
  vals        <- state$grid[, var_idx, drop = TRUE]
  clause_idxs <- which(!is.na(vals) & vals == -var_value)
  state$grid[clause_idxs, var_idx] <- NA_integer_
  
  invisible(state)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DPLL technique - Unit Propagation
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpll_unit_propagation <- function(state, verbosity = 0L) {
  while(TRUE) {
    unit_clause_idxs <- find_unit_clause_idxs(state)
    if (length(unit_clause_idxs) == 0) break;
    if (verbosity > 0) cat("Unit clauses:", deparse1(unit_clause_idxs), "\n")
    clause_idx   <- unit_clause_idxs[1]
    
    var_idx      <- find_unit_clause_var_idx(state, clause_idx)
    var_value    <- state$grid[clause_idx, var_idx]
    
    set_value(state, var_idx, var_value)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Find pure literals i.e. literals which only have a single polarity 
# across all clauses i.e. only a single, unique, non-NA value in a column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
find_pure_literal_var_idxs <- function(state) {
  polarity <- apply(state$grid, 2, \(x) length(unique(x[!is.na(x)])))
  # occur    <- apply(state$grid, 2, \(x) sum(!is.na(x)))
  var_idxs <- which(polarity == 1) 
  var_idxs
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DPLL Technique - pure literal elmination
#
# - Find literals which only occur with a single polarity
# - Assign the literal to make it always TRUE
# - Remove clauses which contain this variable
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpll_eliminate_pure_literals <- function(state, verbosity = 0L) {
  while (TRUE) {
    var_idxs <- find_pure_literal_var_idxs(state)
    if (length(var_idxs) == 0) break;
    
    # Find the unique value for this variable
    var_values <- lapply(var_idxs, function(var_idx) {
      sort(unique(state$grid[, var_idx]))
    }) |> unlist()
    
    # Set the values to make all clauses which contain it TRUE
    set_values(state, var_idxs, var_values)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CHoose the variable to split on. 
# VSIDS is the common method
# I'm just going to pick the variable which partakes in the highest number
# of clauses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpll_find_split_var_idx <- function(state) {
  nclauses_per_var <- apply(state$grid, 2, function(x) sum(!is.na(x)))
  if (max(nclauses_per_var) == 0) stop("dpll_find_split_var_idx(): max = 0")
  var_idx <- which.max(nclauses_per_var)
  var_idx
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Internal recursive DPLL solver
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dpll_solve <- function(state, depth = 1L, verbosity = 0L) {
  if (verbosity > 0) message("Depth: ", depth)
  dpll_unit_propagation(state, verbosity = verbosity - 1L)
  dpll_eliminate_pure_literals(state, verbosity = verbosity - 1L)
  
  if (is_sat(state) || is_unsat(state)) {
    return()
  }
  
  var_idx <- dpll_find_split_var_idx(state)
  var_idx
  
  cache_grid <- state$grid
  cache_vars <- state$vars
  set_value(state, var_idx,  1L)
  
  if (verbosity > 0) message("Split: ", var_idx, " = TRUE")
  dpll_solve(state, depth = depth + 1L)
  if (is_sat(state)) {
    return()
  }
  
  state$grid <- cache_grid
  state$vars <- cache_vars
  set_value(state, var_idx, -1L)
  
  if (verbosity > 0) message("Split: ", var_idx, " = FALSE")
  dpll_solve(state, depth = depth + 1L)
  if (is_sat(state)) {
    return()
  }
  
  NULL
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Solve a SAT problem using DPLL
#' 
#' This solver only implements:
#'    - unit propagation
#'    - pure-literal elimination
#' Plenty of scope for improvements. E.g.
#'    - VSIDS for selecting split variable
#'    - Clause learning
#'    
#' @inheritParams sat_solve_naive
#' @return data.frame of booleans if satisfiable. Otherwise NULL
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_solve_dpll_single <- function(sat, verbosity = 0L) {
  
  sat$exprs
  lits <- sat$literals
  lits
  
  nlits <- max(abs(lits))
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Split the literals on '0', then remove all the zeros and empty clauses
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  clauses <- unname(split(lits, cumsum(lits == 0))) 
  clauses <- lapply(clauses, function(clause) {
    clause <- clause[clause != 0]
    clause
  })
  clauses <- Filter(\(x) length(x) > 0, clauses)
  clauses
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Info dump
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nclauses <- length(clauses)
  
  vars <- rep(NA_integer_, nlits)
  names(vars) <- sat$names
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create a grid for keeping track of literal/clause correspondence
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid <- matrix(NA_integer_, nrow = nclauses, ncol = nlits)
  for (i in seq_along(clauses)) {
    clause <- clauses[[i]]
    grid[i, abs(clause)] <- ifelse(clause > 0, 1L, -1L)
  }
  # colnames(grid) <- names(sat$named_literals)
  grid
  
  state <- as.environment(
    list(grid = grid, vars = vars)
  )
  rm(grid)
  rm(vars)
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check before we start that the problem is sane
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is_unsat(state)) {
    return(NULL)
  }
  
  dpll_solve(state, verbosity = verbosity - 1L)
  
  if (is_unsat(state)) {
    return(NULL)
  }
  
  # Unpack state into a solution
  state$vars
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' SAT solver using DPLL technique
#' 
#' This pure R recursive solver uses DPLL methods (unit propagation and pure literal
#' elimination).  This solver works recursively and can be used to solve 
#' problems with 10s to 100s of variables.  However, as all the code is
#' in R, this is not as fast as using a compiled solver. 
#' 
#' For larger problems >100 variables, it may better to export the problem
#' as a DIMACS file and solve externally.
#' 
#' @inheritParams sat_solve_naive
#' @param max_solutions maximum number of solutions to return. Default: 1
#' @return data.frame of logical values. Columns correspond to the variable 
#'         names within the problem.  Each row defines a solution.
#' @examples
#' sat <- sat_new()
#' sat_card_atmost_k(sat, letters[1:4], 3)
#' sat$exprs
#' sat$names
#' sat_solve_naive(sat)
#' sat_solve_dpll(sat, max_solutions = 20)
#' @export
#' @family SAT solvers
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_solve_dpll <- function(sat, max_solutions = 1, remove = "^dummy", verbosity = 0L) {

  start <- Sys.time()
  
  assert_is_sat_prob(sat)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Make a copy of the problem that will be updated with blocked solutions
  # in order to find more answers
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  csat <- sat_copy(sat)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Determine the indicies of the variables which are not dummy variables.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (is.null(remove) || nchar(remove) == 0) {
    keep_idxs <- seq_along(sat$names)
  } else {
    keep_idxs <- which(!grepl(remove, sat$names)) 
  }  
  if (length(keep_idxs) == 0) {
    stop("No variable names to return. Empty problem, or 'remove' too aggressive")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep track of solutions (in logical format) and the solution hashes
  # (character string).
  # Hashes are used to only add new, unique solutions to the list
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  soln_hashes <- character(0)
  solns <- list()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Main solution loop
  #   - solve the given SAT problem
  #   - record the solution and its hash
  #   - block the solution from occuring again
  #   - solve again until either
  #      - max solutions reached
  #      - problem is UNSAT
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  i <- 0
  while(i < max_solutions) {
    
    if (verbosity > 0) {
      print(csat)
    }
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # The result is a named vector of -1,1 values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # start <- Sys.time()
    res <- sat_solve_dpll_single(csat, verbosity = verbosity - 1L)
    # cat(i, Sys.time() - start, "\n")
    
    # Unsatisifed?
    if (is.null(res)) {
      if (verbosity > 0) message("Unsatisfiable")
      break;
    } 
    
    if (FALSE) {
      verbosity <- 0
      csat <- sat_new()
      sat_card_atleast_k(csat, letters[1:5], 3)
      sat_solve_naive(csat)
      res <- sat_solve_dpll_single(csat, verbosity = verbosity - 1L)
      res
    }
    
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Satisfiable! and we have a result of 1, -1 for each variable.
    # Variables may also be NA which means they can take any value.
    # Expand any unknown variables to cover all combinations of possible values
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    df <- expand_unknown_booleans(res)
    
    # cat("MULTIBALL: ", nrow(df), "\n")
    
    for (row_idx in seq_len(nrow(df))) {
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Turn the sequence of (-1, 1) values into a vector of signed literals
      # i.e.     c(-1, 1, 1, -1, 1)
      # becomes  c(-1, 2, 3, -4, 5)
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      res <- unlist(df[row_idx,])
      soln_literals <- unname(seq_along(res) * res)
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # In order to provide all solutions, block this solution in the SAT problem
      # so that the next time the solver is run this exact solution cannot 
      # re-occur
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      sat_block_solution(csat, soln_literals[keep_idxs])
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Convert the named -1,1 values to a boolean vector
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      lgl <- res > 0
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Remove dummy variables 
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      lgl <- lgl[keep_idxs]
      res <- res[keep_idxs]
      
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Add this solution only if it hasn't been seen before.
      # The reason this happens is that dummy variables (e.g. from a cardinality
      # constraint) with different values can produce the same answers for the
      # variables of interest. I.e. If dummy variables are kept, then all the solutions
      # are unique, but if we remove the dummy variables, the remainign variables
      # have repeated solutions.
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      this_hash <- hash(res)
      if (!this_hash %in% soln_hashes) {
        soln_hashes <- c(soln_hashes, this_hash)
        i <- i + 1
        solns[[i]] <- lgl
      }
    }
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return NULL to indicate the problem is UNSATISFIABLE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(solns) == 0) {
    return(NULL)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert to a data.frame
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  solns <- do.call(rbind, solns)
  solns <- as.data.frame(solns)
  rownames(solns) <- NULL
  
  if (verbosity > 0) {
    delta <- as.numeric(difftime(Sys.time(), start, units = 'secs'))
    message(round(delta / nrow(solns), 3), "seconds / solution")
  }
  
  
  solns
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Simple hashing function which just concatenates all values
# into a single string
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
hash <- function(x) {
  paste(x, collapse = "")
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Treat the vector as a row of a data.frame.
#' Any entries which are 'NA' are asssumed to take all possible combinations
#' of "-1" and "1"
#'
#' @param vec vector. Usually an integer vector
#' @return data.frame
#' @examples
#' expand_unknown_booleans(c(2, 3, NA, 1, NA))
#' @noRd 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expand_unknown_booleans <- function(vec) {
  
  idxs <- which(is.na(vec))
  n    <- length(idxs)
  df <- as.data.frame(t(vec))
  if (n == 0) return(df)
  
  lgls <- logical_combinations(n = n)
  
  df <- df[rep(1, 2^n), ]
  
  df[, idxs] <- lgls
  
  df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create all logical combinations of (-1, 1) of degree 'n'
#' @param n number of variables
#' @return data.frame of all combinations of (-1, 1) within those variables
#' @examples
#' logical_combinations(3)
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logical_combinations <- function(n) {
  
  if (n > 20) {
    warning("logical_combinations(): Possible memory explosion: n = ", n)
  }
  
  cols <- rep(list(c(-1, 1)), times = n)
  df <- do.call(expand.grid, cols)
  
  df
}






if (FALSE) {
  
  sat <- sat_new()
  sat_card_atleast_k(sat, letters[1:4], 3)
  sat_solve_naive(sat)
  sat_solve_dpll(sat, max_solutions = 20)
  
  sat <- readRDS("working/sudoku4x4.rds")
  sat_solve_dpll(sat, max_solutions = 4, verbosity = 10)
}


