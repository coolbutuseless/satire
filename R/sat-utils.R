
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NULL coalesce operator
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a CNF clause to a sequence of literals
#' 
#' This function also keeps track of any new names in the expression, calculates
#' a new literal integer to represent this variable and then adds a record of this
#' variable/integer relationship to the SAT problem.
#' 
#' @param txt a string containing a Boolean expression in CNF.
#' @param sat a SAT problem as created by \code{\link{sat_new}()}
#' @param verbosity verbosity. Default: 0
#' @return always returns an integer vector with '0' as the last element.
#'         Otherwise an error.
#' @examples
#' sat <- sat_new()
#' 
#' expr_str_to_literals(substitute(alpha_01 | !beta || -gamma | delta), sat = sat)
#' expr_str_to_literals(substitute(alpha_02 | beta), sat = sat)
#' 
#' 
#' expr_str_to_literals("alpha_01 | !beta || -gamma | delta", sat = sat)
#' expr_str_to_literals("alpha_02 | beta", sat = sat)
#' 
#' expr_str_to_literals("(alpha_01 | !beta || -gamma | delta) & (alpha_02 | beta)", 
#'                    sat = sat, verbosity = 2)
#' 
#' @importFrom stringr str_extract_all
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
expr_str_to_literals <- function(
    txt,
    sat,
    verbosity = 0L) {
  
  
  stopifnot(is.character(txt))
  txt0 <- txt
  

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Find all names in the expression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  nms <- stringr::str_extract_all(txt, "\\w+")[[1]]
  nms <- unique(nms)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # There must be at least 1 name!
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(nms) == 0) {
    stop("No names found in clause: ", txt)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Which names are currently not in the state?
  # Add them to the state, and assign them a literal.
  # We can then use 'state' as the golden source of name-to-integer mapping
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  new_nms <- setdiff(nms, names(sat$.internal$name_to_int))
  if (verbosity > 0) {
    cat("New names: ", deparse1(new_nms), "\n")
  }
  if (length(new_nms) > 0) {
    for (i in seq_along(new_nms))  {
      sat$.internal$name_to_int[[new_nms[[i]]]] <- sat$.internal$max_named + i
    }
    sat$.internal$max_named <- sat$.internal$max_named + length(new_nms)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Replace each name in the expression with its integer literal
  # using 'gsub()' and a regular expression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for (nm in nms) {
    re  <- paste0("\\b", nm, "\\b")
    val <- sat$.internal$name_to_int[[nm]]
    
    txt <- gsub(re, val, txt)
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Tidy the expression
  #  - Use '-' for negation (instead of '!')
  #  - Logical 'OR' is implied between literals, so remove '|' and '||'
  #  - If there are multiple clauses separated by '&&', '&' or '+', then 
  #    delimit these with the literal interger zero (0)
  #  - brackets are redundant in CNF. Remove them.
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  while (any(grepl("!!", txt))) {
    txt <- gsub("!!", "", txt)
  }
  
  
  txt <- gsub("!", "-", txt) # use "-" for all negation
  txt <- gsub("\\|\\||\\|", " ", txt) # replace | with just a space
  
  # delimit multiple clauses
  txt <- gsub("&&|&|\\+", " 0 ", txt)
  
  # Remove brackets
  txt <- gsub("\\(|\\)", " ", txt)
  
  # replace multiple spaces with single space. Just for neatness :)
  txt <- gsub("\\s+", " ", txt) 
  
  
  if (verbosity > 0) {
    cat("txt: ", deparse1(txt), "\n")
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use 'scan()' to pull all the integers out of the expression
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  res <- tryCatch(
    scan(text = txt, what = integer(), quiet = TRUE),
    error = function(e) {
      stop("Failed to scan literals from: ", 
           dQuote(txt0, q = FALSE), " => ", dQuote(txt, q = FALSE))
    }
  )
  
  
  if (length(res) == 0) {
    stop("No literals found: ", dQuote(txt0, q = FALSE))
  }
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add a trailing zero if not there
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (res[length(res)] != 0) {
    res <- c(res, 0L)
  }
  
  res
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Split a vector into chunks
#' @param v vector
#' @param n maximum length of each chunk
#' @return list of chunked vectors
#' @examples
#' chunk(letters, 5)
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
chunkify <- function(v, n) {
  stopifnot(n > 1)
  
  chunks <- split(v, ceiling(seq_along(v)/n))
  nchunks <- length(chunks)
  
  # don't let a last chunk be a soliton
  if (nchunks > 1 && length(chunks[[nchunks]]) == 1) {
    chunks[[nchunks - 1L]] <- c(chunks[[nchunks - 1L]], chunks[[nchunks]])
    chunks[[nchunks]] <- NULL
  }
  
  chunks
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Helper to negate a string variable
#' 
#' @param nms variable names
#' @return variable names prefixed with "!"
#' @examples
#' not("a")
#' not(c('a', 'b', '!c'))
#' not(c(' a', '! b', '!c'))
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
not <- function(nms) { 
  if (length(nms) == 0) {
    return(character(0))
  }
  
  nms <- trimws(nms)
  ifelse(
    startsWith(nms, "!"), 
    sub("^!\\s*", "", nms),
    paste0("!", nms) 
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Inline operatoe to join strings with "|"
#' @param x,y character strings
#' @return single string
#' @examples
#' "a" %or% "b"
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"%or%" <- function(x, y) {
  paste(x, "|", y) 
}



bracket <- function(x) {
  paste0("(", x, ")")
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Integer sequence that only runs if sequence is ascending
#' @param from,to limits of sequence (inclusive)
#' @return if \code{to >= from} then returns the sequence, otherwise it returns
#'         an empty integer vector
#'         
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
iseq <- function(from, to) {
  if (to < from) {
    integer(0)
  } else {
    seq.int(from, to)
  }
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Apply a canonical ordering to a data.frame
#'
#' Different SAT solving techniques will returns results in a different order.
#' To quickly compare identical solutions, order both data.frames in 
#' the same manner
#'
#' @param df data.frame
#' @return Data.frame rows in canonical ordering
#' @examples
#' order_canonically(mtcars)
#' 
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
order_canonically <- function(df) {
  stopifnot(is.data.frame(df))
  canon_order <- order(apply(df, 1, function(x) {
    paste(x, collapse = "-")
  }))
  df <- df[canon_order,]
  rownames(df) <- NULL
  
  df
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Check that 'sat' argument is a proper SAT object
#' @param sat sat object
#' @return None. Throws error if not a SAT object
#' @noRd
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
assert_is_sat_prob <- function(sat) {
  stopifnot(inherits(sat, "sat_prob"))
}





