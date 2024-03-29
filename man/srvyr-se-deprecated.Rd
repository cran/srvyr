% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_survey.r, R/as_survey_design.r,
%   R/as_survey_rep.r, R/as_survey_twophase.r, R/cascade.r, R/manip.r,
%   R/summarise.r
\name{srvyr-se-deprecated}
\alias{srvyr-se-deprecated}
\alias{as_survey_}
\alias{as_survey_design_}
\alias{as_survey_rep_}
\alias{as_survey_twophase_}
\alias{cascade_}
\alias{mutate_}
\alias{transmute_}
\alias{select_}
\alias{rename_}
\alias{filter_}
\alias{mutate_each_}
\alias{summarise_each_}
\alias{summarize_each_}
\alias{funs_}
\alias{summarise_}
\alias{summarize_}
\title{Deprecated SE versions of main srvyr verbs}
\usage{
as_survey_(.data, ...)

as_survey_design_(
  .data,
  ids = NULL,
  probs = NULL,
  strata = NULL,
  variables = NULL,
  fpc = NULL,
  nest = FALSE,
  check_strata = !nest,
  weights = NULL,
  pps = FALSE,
  variance = c("HT", "YG")
)

as_survey_rep_(
  .data,
  variables = NULL,
  repweights = NULL,
  weights = NULL,
  type = c("BRR", "Fay", "JK1", "JKn", "bootstrap", "successive-difference", "ACS",
    "other"),
  combined_weights = TRUE,
  rho = NULL,
  bootstrap_average = NULL,
  scale = NULL,
  rscales = NULL,
  fpc = NULL,
  fpctype = c("fraction", "correction"),
  mse = getOption("survey.replicates.mse")
)

as_survey_twophase_(
  .data,
  id,
  strata = NULL,
  probs = NULL,
  weights = NULL,
  fpc = NULL,
  subset,
  method = c("full", "approx", "simple")
)

cascade_(.data, ..., .dots, .fill = NA)
}
\arguments{
\item{.data}{a data.frame or an object from the survey package}

\item{...}{other arguments, see other functions for details}

\item{ids}{Variables specifying cluster ids from largest level to smallest level
(leaving the argument empty, NULL, 1, or 0 indicate no clusters).}

\item{probs}{Variables specifying cluster sampling probabilities.}

\item{strata}{Variables specifying strata.}

\item{variables}{Variables specifying variables to be included in survey.
Defaults to all variables in .data}

\item{fpc}{Variables specifying a finite population correct, see
\code{\link[survey]{svydesign}} for more details.}

\item{nest}{If \code{TRUE}, relabel cluster ids to enforce nesting within strata.}

\item{check_strata}{If \code{TRUE}, check that clusters are nested in strata.}

\item{weights}{Variables specifying weights (inverse of probability).}

\item{pps}{"brewer" to use Brewer's approximation for PPS sampling without replacement.
"overton" to use Overton's approximation. An object of class HR to use the Hartley-Rao
approximation. An object of class ppsmat to use the Horvitz-Thompson estimator.}

\item{variance}{For pps without replacement, use variance="YG" for the Yates-Grundy estimator
instead of the Horvitz-Thompson estimator}

\item{repweights}{Variables specifying the replication weight variables}

\item{type}{Type of replication weights}

\item{combined_weights}{\code{TRUE} if the \code{repweights} already
include the sampling weights. This is usually the case.}

\item{rho}{Shrinkage factor for weights in Fay's method}

\item{bootstrap_average}{For \code{type = "bootstrap"}, if the bootstrap
weights have been averaged, gives the number of iterations averaged over.}

\item{scale, rscales}{Scaling constant for variance, see
\code{\link[survey]{svrepdesign}} for more information.}

\item{fpctype}{Finite population correction information}

\item{mse}{if \code{TRUE}, compute variances based on sum of squares
around the point estimate, rather than the mean of the replicates}

\item{id}{list of two sets of variable names for sampling unit identifiers}

\item{subset}{bare name of a variable which specifies which observations are selected in phase 2}

\item{method}{"full" requires (much) more memory, but gives unbiased variance estimates for
general multistage designs at both phases. "simple" or "approx" use less memory, and is correct for
designs with simple random sampling at phase one and stratified randoms sampling at phase two. See
\code{\link[survey]{twophase}} for more details.}

\item{.dots}{Used to work around non-standard evaluation. See
\code{vignette("nse", package = "dplyr")} for details.}

\item{.fill}{Value to fill in for group summaries}
}
\description{
srvyr has updated it's standard evaluation semantics to match dplyr 0.7, so
these underscore functions are no longer required (but are still supported
for backward compatibility reasons). See \code{\link[dplyr]{se-deprecated}} or the
dplyr vignette on programming (\code{vignette("programming", package =
"dplyr")}) for more details.
}
