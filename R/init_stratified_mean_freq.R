#' Initialize The Data With Missing Values Stratified On Row Means
#'
#' @description
#' Replace data matrix with the mean for `numerical` data and the most frequent
#' instance for categorical data (`character` or `factor`) in stratified bins of
#' row means.
#' It will produce `bins` number of of groups with equal number of values in
#' each bin and then fills out the missing values as in [init_mean_freq].
#'
#'
#' @param data A data frame or matrix to replace missing values in
#' @param bins The number of bins to group the data in
#' @param ... Currently not used
#'
#' @return The input data with missing values replaced with the mean (`numerical`)
#' or the label with the highest occurrence (`categorical`).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   a = c(runif(100), rep(NA, 5)),
#'   b = c(letters[sample.int(26, 100, replace = TRUE)], rep(NA, 5))
#' )
#' init_stratified_mean_freq(df, 20)
init_stratified_mean_freq <- function(data, bins, .fun, ...) {
  UseMethod("init_stratified_mean_freq")
}

#' @export
init_stratified_mean_freq.data.frame <- function(data, bins, .fun, ...) {
  stratified_init <- function(data, bins, .fun) {
    if (!"long_name_for_order" %in% colnames(data)) {
      data <- tibble::rownames_to_column(data, "long_name_for_order")
      initial_bins <- bins
    }
    if (anyNA(data)) {
      data <- split(data,
                    ggplot2::cut_number(
                      rowMeans(
                        data[purrr::map_lgl(data, is.numeric)], na.rm = TRUE
                      ), bins
                    )
      ) %>%
        purrr::map(
          .fun
        ) %>%
        dplyr::bind_rows()
      if (bins == initial_bins) {
        stratified_init(data, bins - 1, initial_bins)
      } else {
        rlang::warn(
          paste0(
            "Warning, initial number of bins: ",
            initial_bins,
            " did not fill out all missing values."
          ),
          paste0("Trying a smaller number of bins:", bins - 1)
        )
        stratified_init(data, bins - 1, initial_bins)
      }
    }
    data <- data[order(as.integer(data$long_name_for_order)), -which(colnames(data) == "long_name_for_order")]
    return(data)
  }
}

#' @export
init_stratified_mean_freq.matrix <- function(data, bins, ...) {
  cols <- ncol(data)
  split(data, ggplot2::cut_number(rowMeans(data), bins)) %>%
    purrr::map(matrix, ncol = cols) %>%
    purrr::map(apply, 2, mean_freq_impute) %>%
    purrr::reduce(rbind)
}
