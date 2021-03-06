% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/survey_count_tally.R
\name{survey_tally}
\alias{survey_tally}
\alias{survey_count}
\title{Count/tally survey weighted observations by group}
\usage{
survey_tally(
  x,
  wt,
  sort = FALSE,
  name = "n",
  vartype = c("se", "ci", "var", "cv")
)

survey_count(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = "n",
  .drop = dplyr::group_by_drop_default(x),
  vartype = c("se", "ci", "var", "cv")
)
}
\arguments{
\item{x}{A tbl_svy object, as created by \code{as_survey} and related functions.}

\item{wt}{(Optional) A variable to weight on (in addition to the survey weights,
which are always used). If left unspecified, \code{tally()} will use a variable
named "n" if one exists, but \code{count()} will not. Override this behavior by
specifying \code{wt = NULL}.}

\item{sort}{Whether to sort the results (defaults to \code{FALSE})}

\item{name}{Name of count variable created (defaults to n). If the variable already
exists, will add "n" to the end until it does not.}

\item{vartype}{What types variation estimates to calculate, passed to
\code{\link{survey_total}}.}

\item{...}{Variables to group by, passed to \code{group_by()}.}

\item{.drop}{When .drop = TRUE, empty groups are dropped, see \code{\link[dplyr]{group_by}}
documentation for more details.}
}
\description{
Analogous to \code{\link[dplyr]{tally}} and count, calculates the survey weighted
count of observations. \code{survey_tally} will call \code{\link{survey_total}} empty (resulting
in the count of each group) or on \code{wt} if it is specified (resulting in the
survey weighted total of \code{wt}). \code{survey_count} is similar, but calls \code{group_by}
before calculating the count and then returns the data to the original groupings.
}
\details{
If \code{n} already exists, \code{tally} will use it as the weight, but \code{count}
will not.
}
\examples{
library(survey)
data(api)

dstrata <- apistrat \%>\%
  as_survey_design(strata = stype, weights = pw)

dstrata \%>\%
  group_by(awards) \%>\%
  survey_tally()

dstrata \%>\%
  survey_count(awards)

}
