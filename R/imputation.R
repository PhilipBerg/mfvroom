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
#' @param stop_crit To stop the first time the OOB or delta increases
#' @param init_args An optional list of arguments to be passed to the init function
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
                       stop_crit = c("delta", "oob"),
                       sparsity = c("none", "trim", "weight", "sse"),
                       case_weights = NULL,
                       init_args = list(),
                       ...) {

  stop_crit <- match.arg(stop_crit)
  sparsity <- match.arg(sparsity)
  mis_indx <- purrr::map(data, \(.x) which(is.na(.x)))
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
  n_oob <- Inf


  init_call <- rlang::expr(init(data, !!!init_args))
  imp_mat   <- rlang::eval_tidy(init_call)

  oob <- setNames(
    vector(mode = "numeric", length = length(mis_indx)),
    names(mis_indx)
  )

  rf_pars <- list(mtry = mtry, num.threads = workers)
  in_pars <- rlang::list2(...)
  rf_pars[names(in_pars)] <- in_pars[names(in_pars)]
  rf <- purrr::partial(ranger::ranger, !!!rf_pars)
  fc <- purrr::partial(formatC, digits = 3, format = "e")

  if (stop_crit == "delta") {
    stopper <- rlang::quo(n_diff > sum(dif))
  } else {
    stopper <- rlang::quo(n_oob > sum(oob))
  }

  cat("Estimating Imputation Values\n")
  while (TRUE) {
    tic <- Sys.time()
    for (i in names(mis_indx)) {
      form <- formula(
        paste0(i, "~ .")
      )

      idx <- mis_indx[[i]]
      t_data <- dplyr::slice(imp_mat, -idx)
      cw <- case_weights[[i]][-idx]

      if (sparsity == "trim") {
        rf_model <- lasso_trim_rf(form, i, t_data, rf, cw)
      } else if (sparsity == "weight") {
        rw <- ridge_weights(form, i, t_data, rf, cw)
        rf_model <- rw$rf_mod
        w <- rw$w
      } else if (sparsity == "sse") {
        sw <- sse_weights(form, i, t_data, rf, cw)
        rf_model <- sw$rf_mod
        w <- sw$w
      } else {
        rf_model <- rf(
          form, t_data, case.weights = cw
        )
      }

      if (sparsity %in% c("trim", "none")) {
        pred <- rf_model %>%
          stats::predict(dplyr::slice(imp_mat, idx)) %>%
          magrittr::use_series(predictions)
      } else {
        pred <- rf_model %>%
          stats::predict(dplyr::slice(imp_mat, idx), predict.all = T) %>%
          magrittr::use_series(predictions)
        pred <- pred %*% w
      }

      dif[i] <- evaluate_imputation(imp_mat[[i]][idx], pred)

      imp_mat[idx, i] <- pred
      oob[i] <- rf_model$prediction.error
    }
    if (rlang::eval_tidy(stopper)) {
      s_oob <- sum(oob)
      sn_diff <- sum(dif)
      x <- matrix(dif, nrow = 1, byrow = T)
      colnames(x) <- names(dif)
      rownames(x) <- "\U0394:"
      mp <- paste0(
        "Previous \U0394   (total):\t", fc(n_diff),
        "\t>\tCurrent \U0394 (total):\t", fc(sn_diff)
      )
      if (sparsity == "none") {
        xo <- matrix(oob, nrow = 1, byrow = T)
        rownames(xo) <- "OOB error:"
        x <- rbind(x, xo)
        sig <- ifelse(sum(n_oob) < s_oob, "<", ">")
        mp <- paste0(
          mp,
          paste0(
            "\nPrevious OOB (total):\t", fc(sum(n_oob)), "\t", sig,
            "\tCurrent OOB (total):\t", fc(s_oob), "\n"
          ), collapse = "\n"
        )
      }
      cat(
        mp,
        sep = '\n'
      )
      print(fc(x), quote = F)
      n_diff <- sn_diff
      n_oob <- s_oob
      imp_out <- imp_mat
      print_it_time(tic)
    } else {
      # print_it_stats(oob, n_oob, s_oob, dif, n_diff, sn_diff, trim, fc)
      x <- matrix(dif, nrow = 1, byrow = T)
      colnames(x) <- names(dif)
      rownames(x) <- "\U0394:"
      mp <- paste0(
        "Previous \U0394   (total):\t", fc(n_diff),
        "\t\U2264\tCurrent \U0394 (total):\t", fc(sn_diff)
      )
      if (sparsity == "none") {
        xo <- matrix(oob, nrow = 1, byrow = T)
        rownames(xo) <- "OOB error:"
        x <- rbind(x, xo)
        sig <- ifelse(sum(n_oob) < s_oob, "<", ">")
        mp <- paste0(
          mp,
          paste0(
            "\nPrevious OOB (total):\t", fc(sum(n_oob)), "\t", sig,
            "\tCurrent OOB (total):\t",  fc(s_oob), "\n"
          ), collapse = "\n"
        )
      }
      cat(
        mp, "\n",
        sep = '\n'
      )
      print(fc(x), quote = F)
      cat("Breaking\n")
      print_it_time(tic)
      break
    }
  }
  for (i in names(data)) {
    idx <- mis_indx[[i]]
    data[idx, i] <- imp_out[idx, i]
  }
  return(
    list(
      data = data,
      oob = n_oob
    )
  )
}
print_it_time <- function(tic) {
  toc <- Sys.time() - tic
  cat(
    "Iteration time:\n",
    format(toc), "\n\n"
  )
}

#' print_it_stats <- function(oob, n_oob, s_oob, dif, n_diff, sn_diff, trim, fc) {
#' }

#' @export
evaluate_imputation <- function(old, current) {
  UseMethod("evaluate_imputation")
}
#' @export
evaluate_imputation.numeric <- function(old, current) {
  sum(
    (old - current)^2
  ) / sum(current^2)
}
#' @export
evaluate_imputation.character <- function(old, current) {
  mean(old != current)
}
#' @export
evaluate_imputation.factor <- function(old, current) {
  mean(old != current)
}


calc_nrmse <- function(x, y) {
  {x - y} %>%
    magrittr::raise_to_power(2) %>%
    mean() %>%
    magrittr::divide_by(var(y)) %>%
    sqrt()
}
