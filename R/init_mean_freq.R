#' Initialize The Data With Missing Values Using The Mean Or Frequency Imputation
#'
#' @description
#' Replace data matrix with the mean for `numerical` data and the most frequent
#' instance for categorical data (`character` or `factor`)
#'
#'
#' @param data A data matrix to replace missing values in
#'
#' @return The input data with missing values replaced with the mean (`numerical`)
#' or the label with the highest occurrence (`categorical`).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = c(runif(10), rep(NA, 5)),
#'   b = c(letters[sample.int(26, 10)], rep(NA, 5))
#' )
#' init_mean_freq(df)
init_mean_freq <- function(data) {
  UseMethod("init_mean_freq")
}

#' @export
init_mean_freq.data.frame <- function(data) {
  data %>%
    dplyr::mutate(
      across(everything(), mean_freq_impute)
    )
}

#' @export
init_mean_freq.matrix <- function(data) {
  apply(data, 2, mean_freq_impute)
}
