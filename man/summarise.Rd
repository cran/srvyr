% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarise.r
\name{summarise}
\alias{summarise}
\alias{summarize}
\title{Summarise multiple values to a single value.}
\arguments{
\item{.data}{tbl A \code{tbl_svy} object}

\item{...}{Name-value pairs of summary functions}

\item{.groups}{Defaults to "drop_last" in srvyr meaning that the last group is peeled
off, but if there are more groups they will be preserved. Other options are "drop", which
drops all groups, "keep" which keeps all of them and "rowwise" which converts the object
to a rowwise object (meaning calculations will be performed on each row).}

\item{.unpack}{Whether to "unpack" named \code{data.frame} columns. \code{srvyr} predates
\code{dplyr}'s support for data.frame columns so it does not treat them the same way by
default.}
}
\description{
Summarise multiple values to a single value.
}
\details{
Summarise for \code{tbl_svy} objects accepts several specialized functions.
Each of the functions a variable (or two, in the case of
\code{survey_ratio}), from the data.frame and default to providing the measure
and its standard error.

The argument \code{vartype} can choose one or more measures of uncertainty,
\code{se} for standard error, \code{ci} for confidence interval, \code{var}
for variance, and \code{cv} for coefficient of variation. \code{level}
specifies the level for the confidence interval.

The other arguments correspond to the analagous function arguments from the
survey package.

The available functions from srvyr are:

\describe{
\item{\code{\link{survey_mean}}}{
   Calculate the survey mean of the entire population or by \code{groups}.
   Based on \code{\link[survey]{svymean}}.}
\item{\code{\link{survey_total}}}{
   Calculate the survey total of the entire population or by \code{groups}.
   Based on \code{\link[survey]{svytotal}}.}
 \item{\code{\link{survey_ratio}}}{
   Calculate the ratio of 2 variables in the entire population or by \code{groups}.
   Based on \code{\link[survey]{svyratio}}.}
\item{\code{\link{survey_quantile}}}{
   Calculate quantiles in the entire population or by \code{groups}. Based on
   \code{\link[survey]{svyquantile}}.}
 \item{\code{\link{survey_median}}}{
   Calculate the median in the entire population or by \code{groups}.
   \code{\link[survey]{svyquantile}}.}
 \item{\code{\link{unweighted}}}{
   Calculate an unweighted estimate as you would on a regular \code{tbl_df}.
   Based on dplyr's \code{\link[dplyr]{summarise}}.}
}
}
\examples{
library(survey)
data(api)

dstrata <- apistrat \%>\%
  as_survey_design(strata = stype, weights = pw)

dstrata \%>\%
  summarise(api99_mn = survey_mean(api99),
            api00_mn = survey_mean(api00),
            api_diff = survey_mean(api00 - api99))

dstrata_grp <- dstrata \%>\%
  group_by(stype)

dstrata_grp \%>\%
  summarise(api99_mn = survey_mean(api99),
            api00_mn = survey_mean(api00),
            api_diff = survey_mean(api00 - api99))

}
