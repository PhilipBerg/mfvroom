#' Mean or Frequency Imputation
#' @param x The vector to replace missing values in
#'
#' @description
#' Replaces `NA` values with the mean for `numeric` or the most frequent observation for `character`.
#' For `character`s, ties are broken uniformly at random
#'
#' @return
#' A vector with missing values replaced by the mean or most frequent observation
#' @export
#'
#' @examples
#' mean_freq_impute(c(runif(10), NA))
mean_freq_impute <- function(x)
  UseMethod("mean_freq_impute", x)

#' @export
mean_freq_impute.factor <- function(x) {
  mean_freq_impute.character(x)
}

#' @export
mean_freq_impute.numeric <- function(x) {
  m <- mean(x, na.rm = TRUE)
  x[is.na(x)] <- m
  x
}

#' @export
mean_freq_impute.character <- function(x) {
  tab <- table(x, useNA = "no")
  out <- which(tab == max(tab))
  f <- names(out[sample.int(length(out), 1)])
  x[is.na(x)] <- f
  x
}
