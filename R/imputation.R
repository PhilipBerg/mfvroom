utils::globalVariables(c("across", "everything", "predictions"))
#' Single imputation
#'
#' Performs a single imputation run and returns the data with NA values replaced
#' by imputed values.
#'
#' @param data a `data.frame` or a `matrix` to perform the imputation on,
#' missing values has to be `NA`.
#' @param workers Number of parallel workers to use when building the forest
#' @param init A function for initialization of missing values in the data
#' @param mtry Number of columns to use in each tree
#' @param ... Additional arguments to [ranger::ranger].
#'
#' @return a `data.frame` with `NA` values replaced by imputed values.
#' @export
#' @importFrom ranger ranger
#' @importFrom stats formula
#' @importFrom stats setNames
#'
#' @examples
#' iris_miss <- iris
#' iris_miss[
#'   matrix(
#'     c(
#'         sample.int(nrow(iris_miss), 50),
#'         sample.int(ncol(iris_miss), 50, replace = TRUE)
#'       ), ncol = 2)
#'   ] <- NA
#' imputation(iris_miss)
imputation <- function(data,
                       workers = 1,
                       mtry = floor(ncol(data) / 3),
                       init = init_mean_freq,
                       ...) {

  mis_indx <- apply(data, 2, \(.x) which(is.na(.x)))
  if (length(mis_indx) == 0) {
    rlang::warn(c("Data has no missing values.", "Returning as is."))
    return(data)
  }
  mis_indx <- mis_indx[purrr::map_lgl(mis_indx, ~ length(.x) != 0)]

  imp_order <- purrr::map_dbl(mis_indx, length) %>%
    sort()
  mis_indx <- mis_indx[names(imp_order)]

  dif <- vector()
  n_diff <- Inf
  n_oob <- NA

  imp_mat <- init(data)

  oob <- setNames(
    vector(mode = "numeric", length = length(mis_indx)),
    names(mis_indx)
  )

  rf_pars <- list(mtry = mtry)
  in_pars <- rlang::list2(...)
  rf_pars[names(in_pars)] <- in_pars[names(in_pars)]
  rf <- purrr::partial(ranger::ranger, !!!rf_pars)
  fc <- purrr::partial(formatC, digits = 3, format = "e")

  cat("Estimating Imputation Values\n")
  while (TRUE) {
    tic <- Sys.time()
    for (i in names(mis_indx)) {
      form <- formula(
        paste0(i, "~ .")
      )

      idx <- mis_indx[[i]]

      rf_model <- rf(
        form, dplyr::slice(imp_mat, -idx),
        num.threads = workers
      )

      pred <- rf_model %>%
        stats::predict(dplyr::slice(imp_mat, idx)) %>%
        magrittr::use_series(predictions)

      dif[i] <- evaluate_imputation(imp_mat[idx, i], pred)

      imp_mat[idx, i] <- pred
      oob[i] <- rf_model$prediction.error
    }
    if (n_diff > sum(dif)) {
      s_oob <- sum(oob)
      sn_diff <- sum(dif)
      sig <- ifelse(sum(n_oob) < s_oob, "<", ">")
      x <- matrix(c(dif, oob), nrow = 2, byrow = T,)
      colnames(x) <- names(dif)
      rownames(x) <- c("\U0394:", "OOB error:")
      cat(
        "Previous \U0394   (total):\t", fc(n_diff), "\t>\tCurrent \U0394 (total): ", fc(sn_diff), "\n",
        "Previous OOB (total):\t", fc(sum(n_oob)), "\t", sig, "\tCurrent OOB (total): ", fc(s_oob), "\n",
        sep=''
      )
      print(fc(x), quote = F)
      n_diff <- sn_diff
      n_oob <- s_oob
      imp_out <- imp_mat
      print_it_time(tic)
    } else {
      sig <- ifelse(sum(n_oob) < s_oob, "<", ">")
      cat(
        "Previous \U0394   (total):\t", fc(n_diff), "\t<\tCurrent \U0394 (total): ", fc(sum(dif)), "\n",
        "Previous OOB (total):\t", fc(sum(n_oob)), "\t", sig, "\tCurrent OOB (total): ", fc(s_oob), "\n",
        "\nBreaking \n", sep = ""
      )
      print_it_time(tic)
      break
    }
  }
  for (i in names(data)) {
    idx <- mis_indx[[i]]
    data[idx, i] <- imp_out[idx, i]
  }
  return(data)
}
print_it_time <- function(tic) {
  toc <- Sys.time() - tic
  cat(
    "Iteration time:\n",
    format(toc), "\n\n"
  )
}


evaluate_imputation <- function(old, current) {
  UseMethod("evaluate_imputation")
}

evaluate_imputation.numeric <- function(old, current) {
  sum(
    (old - current)^2
  ) / sum(current^2)
}

evaluate_imputation.character <- function(old, current) {
  mean(old != current)
}

evaluate_imputation.factor <- function(old, current) {
  mean(old != current)
}
