

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Initialise a new SAT problem
#' 
#' @return \code{sat} object
#' @examples
#' sat <- sat_new()
#' sat
#' sat_add_literals(sat, c(1, 2))
#' sat_add_literals(sat, c(2, -3))
#' sat
#' sat$names
#' sat$named_literals
#' sat$exprs
#' sat$dimacs
#' sat_solve_naive(sat)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_new <- function() {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Some state information so we can keep track of names used in CNF clauses
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat                           <- new.env()
  sat$.internal                 <- list()
  sat$.internal$name_to_int     <- list()
  sat$.internal$max_named       <- 0L     # maximum literal seen. for sat_add_expr()
  sat$.internal$unique_literals <- integer(0)
  sat$.internal$dummy_idx        <- 1L
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the first "add" is 'sat_add_literals()' then you can't 
  # use 'sat_add_exprs' afterwards
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat$.internal$integers_only <- NULL
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # a list of all integer literal vectors added 
  # keep this as a list which avoid re-allocation when adding literals.
  # This will be unpacked to a simple vector when required.
  # Could further optimize this with a 'dirty' bit and only regenerate the
  # 'sat$literals' when the internal literals list changes.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat$.internal$literals <- list() 
  
  
  makeActiveBinding(
    sym = "literals",
    fun = function() {
      unlist(sat$.internal$literals, recursive = FALSE, use.names = FALSE)
    },
    env = sat
  )
  
  makeActiveBinding(
    sym = "names",
    fun = function() {
      names(sat$.internal$name_to_int)
    },
    env = sat
  )
  
  makeActiveBinding(
    sym = "unique_literals",
    fun = function() {
      sat$.internal$unique_literals
    },
    env = sat
  )
  
  makeActiveBinding(
    sym = "named_literals",
    fun = function() {
      unlist(sat$.internal$name_to_int, recursive = FALSE, use.names = TRUE)
    },
    env = sat
  )
  
  makeActiveBinding(
    sym = "exprs",
    fun = function() {
      sat_get_exprs(sat)
    },
    env = sat
  )
  
  makeActiveBinding(
    sym = "dimacs",
    fun = function() {
      literals_to_dimacs(sat$literals)
    },
    env = sat
  )
  
  
  class(sat) <- 'sat_prob'
  sat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Copy a SAT problem
#' 
#' @param sat Original SAT problem
#' @return Copy of SAT problem
#' @examples
#' sat <- sat_new()
#' sat_add_literals(sat, c(1, 2))
#' sat2 <- sat_copy(sat)
#' sat_add_literals(sat, c(3, 4))
#' sat
#' sat2
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_copy <- function(sat) {
  assert_is_sat_prob(sat)
  
  sat2 <- sat_new()
  sat2$.internal <- sat$.internal
  
  sat2
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print 'sat' object
#' 
#' @param x 'sat' object created by \code{\link{sat_new}()}
#' @param ... ignored
#' @return Invisible copy of original \code{x}
#' @examples
#' sat <- sat_new()
#' print(sat)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.sat_prob <- function(x, ...) {
  literals     <- x$literals
  
  if (is.null(literals)) {
    nvars <- 0L
    nclauses <- 0L
  } else {
    nvars    <- length(unique(abs(literals))) - 1L
    nclauses <- sum(literals == 0)
    if (nvars < 0) nvars <- 0
  }
  
  cat("<sat> vars =", nvars, ", clauses =", nclauses, "\n")
  invisible(x)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add integer literals to a SAT problem
#'
#' Note: Adding clauses to a SAT problem can only be via adding integer literals
#' (via \code{sat_add_literals()}) or adding named expressions (via 
#' \code{sat_add_exprs()}) .  The two 
#' methods cannot be mixed within a single SAT problem.
#'
#' @param sat \code{sat} object as created by \code{\link{sat_new}()}
#' @param literals integer vector of positive and negative literal values.
#'        Clauses should be separated using zero (0).  If the vector is not
#'        terminated by a zero (0), then a zero will be added.
#' @return None
#' @examples
#' # Define a small SAT problem
#' sat <- sat_new()
#' sat_add_literals(sat, c(-1, 2))
#' sat_add_literals(sat, c(-2, 3))
#' sat_add_literals(sat, c(-3, 1))
#' sat_solve_naive(sat)
#' 
#' 
#' # Equivalent problem
#' sat <- sat_new()
#' sat_add_literals(sat, c(-1, 2, 0, -2, 3, 0, -3, 1, 0))
#' sat_solve_naive(sat)
#' @family ways of adding clauses
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_add_literals <- function(sat, literals) {
  assert_is_sat_prob(sat)
  
  sat$.internal$integers_only <- sat$.internal$integers_only %||% TRUE
  
  if (!sat$.internal$integers_only) {
    # Check that new literals match one of the current named literals
    unique_literals <- sort(unique(abs(literals)))
    if (!all(unique_literals %in% sat$.internal$unique_literals)) {
      stop(
        "User-specified integers out-of-range of current named variables: ", 
        deparse1(
          setdiff(sat$.internal$unique_literals, unique_literals)
        )
      )
    }
  } else {
    # Add names to match the literals
    biggest_literal <- max(abs(literals))
    suppressWarnings({
      if (biggest_literal > max(sat$.internal$unique_literals)) {
        # Add more names
        sat$.internal$name_to_int <- as.list(seq_len(biggest_literal))
        names(sat$.internal$name_to_int) <- paste0("v", seq_len(biggest_literal))
      }
    })
  }
  
  
  sat_add_literals_raw(sat, literals)
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Unexported helper function to add literals to a SAT problem
#' 
#' @inheritParams sat_add_literals
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_add_literals_raw <- function(sat, literals) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanitize the vector of literals. Add '0' at end if needed.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  literals <- as.integer(literals)
  stopifnot(!anyNA(literals))
  if (literals[length(literals)] != 0) {
    literals <- c(literals, 0L)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Keep record of literals being added 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat$.internal$literals[[length(sat$.internal$literals) + 1L]] <- literals
  
  sat$.internal$unique_literals <- sort(union(sat$.internal$unique_literals, abs(literals)))
  
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Add Boolean expressions to a SAT problem
#'
#' Note: Adding clauses to a SAT problem can only be via adding integer literals
#' (via \code{sat_add_literals()}) or adding named expressions (via 
#' \code{sat_add_exprs()}) .  The two 
#' methods cannot be mixed within a single SAT problem.
#'
#' @inheritParams sat_add_literals
#' @param exprs a character vector of Boolean expressions - liberal use brackets
#'        is encouraged to signify intent. If any expression is not 
#'        in CNF, an attempt will be made to convert it to CNF using Tseytin
#'        transformation. 
#'        Allowed syntax:
#'        \describe{
#'          \item{\code{!}}{Negation}
#'          \item{\code{|}}{OR}
#'          \item{\code{&}}{AND}
#'          \item{\code{->}}{Implication. \code{a -> b} will be rewritten as \code{!a | b}}
#'          \item{\code{==}}{Equivalence. \code{a == b} will be rewritten as 
#'                \code{(a -> b) & (b -> a)}}
#'          \item{\code{!!}}{Double negation. \code{!!a} will be simplified to \code{a}}
#'          \item{alpha-numeric names}{names must only contain a-z, A-Z, 0-9 and underscore}
#'        }
#' @param verbosity Verbosity level. Default: 0
#' @return None
#' @examples
#' # Solve a small SAT problem using strings
#' sat <- sat_new()
#' sat_add_exprs(sat, "!a | b")
#' sat_add_exprs(sat, "!b | c")
#' sat_add_exprs(sat, "!c | a")
#' sat_solve_naive(sat)
#' 
#' # Multiple string clauses may be added a single call.
#' sat <- sat_new()
#' sat_add_exprs(sat, c("!a | b", "!b | c", "!c | a"))
#' sat_solve_naive(sat)
#' @family ways of adding clauses
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_add_exprs <- function(sat, exprs, verbosity = 0L) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity Check
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  assert_is_sat_prob(sat)
  stopifnot(is.character(exprs))
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Sanity check the user is not mixing 'literals' and 'clauses'
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sat$.internal$integers_only <- sat$.internal$integers_only %||% FALSE
  if (isTRUE(sat$.internal$integers_only)) {
    stop("Cannot add expressions if integer literals added first")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Convert all to CNF
  # Remember: If a call to "as_cnf()" needs to use a Tseytin Transformation, 
  #           then dummy variables will be created!
  #           Rather than trying to track which expressions do/don't need
  #           a dummy variable, just incrmement the dummy_idx for each and 
  #           every expression.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  exprs <- lapply(exprs, function(x) {
    res <- as_cnf(x, dummy_idx = sat$.internal$dummy_idx)
    sat$.internal$dummy_idx <- sat$.internal$dummy_idx + 1L
    res
  })
  exprs <- unlist(exprs)
  
  # Collapse multiple expressions as single expression
  expr <- paste0("(", exprs, ")", collapse = " & ")
  
  # Convert to integer literals
  # expr_str_to_literals() is always 0-terminated
  literals <- expr_str_to_literals(expr, sat = sat, verbosity = verbosity)
  
  if (verbosity > 0) {
    cat("sat_add_exprs(): Addling ", deparse1(literals), "\n")
  }
  
  sat_add_literals_raw(sat, literals)
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Block a given result - use this for manually finding multiple solutions
#' to a given problem.
#' 
#' @inheritParams sat_add_literals
#' @param literals A solution. Vector of integer literals.
#' @param verbosity verbosity
#' @return None
#' @examples
#' # Setup a problem and solve
#' sat <- sat_new()
#' sat_add_exprs(sat, "!a | b")
#' sat_add_exprs(sat, "!b | c")
#' sat_add_exprs(sat, "!c | a")
#' sat_solve_naive(sat)
#'
#' # Block the solution where all values are TRUE
#' soln1 <- "a & b & c"
#' 
#' # Block this solution from occurring
#' sat_block_solution(sat, soln1)
#' 
#' # Now only a single solution returned
#' sat_solve_naive(sat)
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_block_solution <- function(sat, literals, verbosity = 0L) {
  assert_is_sat_prob(sat)
  
  if (is.character(literals)) {
    literals <- paste0("(", literals, ")", collapse = " & ")
    literals <- expr_str_to_literals(literals, sat = sat, verbosity = verbosity)
  }
  
  stopifnot(is.numeric(literals))
  
  # Add inverse literals as clause
  # i.e. "a & b & b"  ->   !a | !b | !c
  literals <- literals[literals != 0] # remove AND
  literals <- -1L  * literals;    # negate
  sat_add_literals(sat, literals)
  
  invisible(sat)
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a sequence of integer literals to named logical vector
#' 
#' This function is used when a set of literals (Usually corresponding to a solution)
#' needs to be turned into a logical statement with named variables.
#' 
#' @inheritParams sat_add_literals
#' @param literals literals
#' @param remove regular expression for variables to remove when blocking solutions
#'        and assembling values to return. Default: "^dummy" will block all
#'        variables starting with the word "dummy" (as this is how the 'satire' 
#'        package automatically creates dummy variables.)
#'        If NULL no variables will be removed.
#' @return Named logical vector indicating the names and boolean values of the 
#'         given literals
#' @examples
#' sat <- sat_new()
#' sat_add_exprs(sat, c("(a | !b) & (!c | !d)"))
#' sat$names
#' sat$named_literals
#' sat_literals_to_lgl(sat, c(1, 3, -4))
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sat_literals_to_lgl <- function(sat, literals, remove = "^dummy") {
  assert_is_sat_prob(sat)
  
  stopifnot(is.numeric(literals))
  stopifnot(!anyNA(literals))
  stopifnot(!any(literals == 0))
  
  lgl  <- literals > 0
  literals <- abs(literals)
  
  named_literals <- unlist(sat$.internal$name_to_int, recursive = FALSE, use.names = TRUE)
  idx <- match(literals, named_literals)
  
  if (anyNA(idx)) {
    stop("Literals not found in SAT problem: ", deparse1(setdiff(literals, named_literals)))
  }
  
  nms <- names(named_literals)[idx]
  
  names(lgl) <- nms
  
  if (!is.null(remove) && nchar(remove) > 0) {
    keep <- !grepl(remove, names(lgl))
    lgl <- lgl[keep]
  }
  
  
  lgl
}


