% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dimacs.R
\name{read_dimacs}
\alias{read_dimacs}
\title{Read a DIMACS file as a \code{sat} object.}
\usage{
read_dimacs(filename)
}
\arguments{
\item{filename}{filename with CNF in DIMACS format}
}
\value{
\code{sat} object
}
\description{
This function reads SAT data from standard DIMACS files. Any comment lines
(lines starting with 'c') are ignored.
}
\examples{
filename <- system.file("sudoku.cnf", package = 'satire', mustWork = TRUE)
sat <- read_dimacs(filename = filename)
sat
sat$literals[1:100]
}
