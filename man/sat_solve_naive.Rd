% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solver-naive.R
\name{sat_solve_naive}
\alias{sat_solve_naive}
\title{Naive, brute-force SAT solver with up-front memory allocation}
\usage{
sat_solve_naive(sat, remove = "^dummy", mem_limit = 1024, verbosity = 0L)
}
\arguments{
\item{sat}{SAT problem as created by \code{\link{sat_new}()}}

\item{remove}{regular expression for variables to remove when blocking solutions
and assembling values to return. Default: "^dummy" will block all
variables starting with the word "dummy" (as this is how the 'satire' 
package automatically creates dummy variables.)
If NULL no variables will be removed.}

\item{mem_limit}{only run problems if the estimated memory allocation is
less than this number of MB.  Default: 1024.  This is a guard 
to prevent catastrophic consequences of trying to allocate too much
memory if given a large problem.}

\item{verbosity}{verbosity level.  Default: 0}
}
\value{
data.frame of logical values. Columns correspond to the variable 
        names within the problem.  Each row defines a solution. If problem
        is unsatisfiable, return NULL.
}
\description{
This solver is only suitable for small problems as memory use is exponential in 
the number of variables and all memory is pre-allocated. E.g. a problem with
21 variables will pre-allocate about a gigabyte of memory.  Each extra
variable doubles the memory required - e.g. a problem with 32 variables 
would require allocation of 520 GB of memory.
}
\examples{
sat <- sat_new()
sat_card_atmost_k(sat, letters[1:4], 3)
sat$exprs
sat$names
sat_solve_naive(sat)
}
\seealso{
Other SAT solvers: 
\code{\link{sat_solve_dpll}()}
}
\concept{SAT solvers}
