% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix-verification.R
\name{integrity_report}
\alias{integrity_report}
\title{Produce a report on the integrity of proposed fixes}
\usage{
integrity_report(verified_fixes, file = NULL, include_problem_cases = TRUE)
}
\arguments{
\item{verified_fixes}{Output of \link{verify_fixes}}

\item{file}{If not NULL, a path to where the integrity report should be saved}

\item{include_problem_cases}{If a request has the \code{Problem} field being \code{TRUE},
then the request will be treated as erroneous, even if no diagnostic flags
have been raised for that fix request}
}
\value{
A data.frame with the integrity report
}
\description{
The integrity report provides diagnostic information to fix authors
to resolve any internal data integrity issues (duplicates, referential
integrity, loss of data, etc.)
}
