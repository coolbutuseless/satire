% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sat-core.R
\name{sat_copy}
\alias{sat_copy}
\title{Copy a SAT problem}
\usage{
sat_copy(sat)
}
\arguments{
\item{sat}{Original SAT problem}
}
\value{
Copy of SAT problem
}
\description{
Copy a SAT problem
}
\examples{
sat <- sat_new()
sat_add_literals(sat, c(1, 2))
sat2 <- sat_copy(sat)
sat_add_literals(sat, c(3, 4))
sat
sat2
}
