% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fix-verification.R
\name{verify_fixes}
\alias{verify_fixes}
\title{Verifies the prospective fixes}
\usage{
verify_fixes(
  fixes,
  id_col,
  unique_id_col = "unique_id",
  databases = NULL,
  reference = NULL,
  foreign_keys = NULL,
  fix_history = NULL,
  include_problem_cases = TRUE,
  review_fields = c("problem", "verifier", "note"),
  edit_fields = c(what = "what", change_to = "change_to", change_from = "change_from"),
  verbose = TRUE
)
}
\arguments{
\item{fixes}{A \code{data.frame} in the fix format}

\item{id_col}{The name of the column that contains the primary key}

\item{unique_id_col}{The name of the column that contains the surrogate key}

\item{databases}{A list of \code{data.frames} used to validate the fixes}

\item{reference}{A master \code{data.frame} that contains the "ground truth"
of the information found in the \code{databases}}

\item{foreign_keys}{Not used.}

\item{fix_history}{A previous iteration of fixes when, if provided, will
be used to determine if fixes found in \code{fixes} are amendments of
previous fixes.}

\item{include_problem_cases}{If \code{TRUE}, records marked as a "problem",
which is an internal communication column for further review, will be
included in the fix verification metrics.}

\item{review_fields}{The names of fields to be used for verification}

\item{edit_fields}{The names of the fix columns}

\item{verbose}{Enables logging messages}
}
\value{
A \code{data.frame} of fixes with the "verified_fixes" attribute,
along with the fix verification metrics.
}
\description{
Computes metrics to determine if the requested fixes are valid
and won't cause record-level corruption. These fixes \emph{don't}
perform referential integrity checks. That must be done externally.
}
