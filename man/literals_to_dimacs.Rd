% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimacs.R
\name{literals_to_dimacs}
\alias{literals_to_dimacs}
\title{Convert literals to a DIMACS-formatted string}
\usage{
literals_to_dimacs(literals)
}
\arguments{
\item{literals}{integer vector of positive and negative literal values.
Clauses should be separated using zero (0).  If the vector is not
terminated by a zero (0), then a zero will be added.}
}
\value{
Character string holding the DIMACS representation
}
\description{
Convert literals to a DIMACS-formatted string
}
\examples{
sat <- sat_new()
sat_add_exprs(sat, "!a | b")
sat_add_exprs(sat, "!b | c")
sat_add_exprs(sat, "!c | a")
literals_to_dimacs(sat$literals)
}
\concept{export functions}
