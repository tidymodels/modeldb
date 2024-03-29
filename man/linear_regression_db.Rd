% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lm.R
\name{linear_regression_db}
\alias{linear_regression_db}
\title{Fits a Linear Regression model}
\usage{
linear_regression_db(df, y_var = NULL, sample_size = NULL, auto_count = FALSE)
}
\arguments{
\item{df}{A Local or remote data frame}

\item{y_var}{Dependent variable}

\item{sample_size}{Prevents a table count. It is only used for models
with three or more independent variables}

\item{auto_count}{Serves as a safeguard in case sample_size is not
passed inadvertently.  Defaults to FALSE.  If it is ok for the
function to count how many records are in the sample, then set to
TRUE.  It is only used for models with three or more independent variables}
}
\description{
It uses 'tidyeval' and 'dplyr' to create a linear
regression model.
}
\details{
The linear_regression_db() function only calls one of three unexported functions.
The function used is determined by the number of independent variables.  This is
so any model of one or two variables can use a simpler formula, which in turn
will have less SQL overhead.
}
\examples{
library(dplyr)

mtcars \%>\%
  select(mpg, wt, qsec) \%>\%
  linear_regression_db(mpg)

}
