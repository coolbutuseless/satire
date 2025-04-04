

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Read a DIMACS file as a \code{sat} object.
#'
#' This function reads SAT data from standard DIMACS files. Any comment lines
#' (lines starting with 'c') are ignored.
#' 
#' @param filename filename with CNF in DIMACS format
#' @return \code{sat} object
#' @examples
#' filename <- system.file("sudoku.cnf", package = 'satire', mustWork = TRUE)
#' sat <- read_dimacs(filename = filename)
#' sat
#' sat$literals[1:100]
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
read_dimacs <- function(filename) {
  
  lines <- readLines(filename, warn = FALSE)
  lines <- trimws(lines)              # Kill leading/trailing whitespace
  lines <- lines[!grepl("^p", lines)] # Ignore header line "p cnf <vars> <clauses>"
  lines <- lines[!grepl("^c", lines)] # Ignore comment lines starting with 'c'
  lines <- lines[nchar(lines) > 0]    # Kill any blank lines
  
  # Paste as a single string of what should now just be integers
  txt <- paste(lines, collapse = " ")
  # Extract the integers from the string.
  literals <- scan(text = lines, what = integer(), quiet = TRUE)
  
  sat <- sat_new()
  sat_add_literals(sat, literals)
  
  sat
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert literals to a DIMACS-formatted string
#' 
#' @inheritParams sat_add_literals
#' @return Character string holding the DIMACS representation
#' @examples
#' sat <- sat_new()
#' sat_add_exprs(sat, "!a | b")
#' sat_add_exprs(sat, "!b | c")
#' sat_add_exprs(sat, "!c | a")
#' literals_to_dimacs(sat$literals)
#' @family export functions
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
literals_to_dimacs <- function(literals) {
  
  nclauses <- sum(literals == 0)
  nvars    <- length(unique(abs(literals))) - 1L
  
  header <- sprintf("p cnf %i %i\n", nvars, nclauses)
  
  literals <- paste(literals, collapse = " ")
  literals <- gsub(" 0 ", " 0\n", literals)
  
  dimacs <- paste0(header, literals)
  
  class(dimacs) <- 'sat_dimacs'
  return(dimacs)

}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Print a DIMACs representation returned from \code{\link{literals_to_dimacs}()} 
#' @param x \code{sat_dimacs} object
#' @param ... ignored
#' @return None
#' @examples
#' literals <- c(1, 2, 3, 0, -2, 4, 0, 5, 6, 7, 0)
#' dimacs <- literals_to_dimacs(literals)
#' dimacs
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print.sat_dimacs <- function(x, ...) {
  cat(x)
}


