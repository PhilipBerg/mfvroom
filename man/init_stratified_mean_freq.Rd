% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init_stratified_mean_freq.R
\name{init_stratified_mean_freq}
\alias{init_stratified_mean_freq}
\title{Initialize The Data With Missing Values Stratified On Row Means}
\usage{
init_stratified_mean_freq(data, bins, ...)
}
\arguments{
\item{data}{A data frame or matrix to replace missing values in}

\item{bins}{The number of bins to group the data in}

\item{...}{Currently not used}
}
\value{
The input data with missing values replaced with the mean (\code{numerical})
or the label with the highest occurrence (\code{categorical}).
}
\description{
Replace data matrix with the mean for \code{numerical} data and the most frequent
instance for categorical data (\code{character} or \code{factor}) in stratified bins of
row means.
It will produce \code{bins} number of of groups with equal number of values in
each bin and then fills out the missing values as in \link{init_mean_freq}.
}
\examples{
df <- data.frame(
  a = c(runif(100), rep(NA, 5)),
  b = c(letters[sample.int(26, 100, replace = TRUE)], rep(NA, 5))
)
init_stratified_mean_freq(df, 20)
}
