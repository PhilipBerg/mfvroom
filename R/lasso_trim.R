lasso_trim_rf <- function(form, y, data, rf_wrapper, case_weights) {
  N <- nrow(data)
  idx <- sample.int(N, round(N*.75), prob = case_weights)
  test  <- dplyr::slice(data, -idx)
  train <- dplyr::slice(data,  idx)
  rf_mod <- rf_wrapper(form, train, case.weights = case_weights[idx])
  y <- test %>%
    dplyr::pull(y)
  pred_test <- predict(rf_mod, test, predict.all = T) %>%
    magrittr::use_series(predictions)
  cv_model <- glmnet::cv.glmnet(
    pred_test,
    y,
    alpha = 1,
    intercept = F,
    nfolds = 5,
    standardize = F
  )
  trim_trees <- rownames(coef(cv_model, s = 'lambda.min'))[coef(cv_model, s = 'lambda.min')[,1]!= 0] %>%
    stringr::str_remove("V") %>%
    as.integer() %>%
    {!seq_len(ncol(pred_test)) %in% .} %>%
    which()
  suppressWarnings(rf_trim <- ranger::deforest(rf_mod, trim_trees))
  rf_trim
}


calc_nrmse <- function(x, y) {
  {x - y} %>%
    magrittr::raise_to_power(2) %>%
    mean() %>%
    magrittr::divide_by(var(y)) %>%
    sqrt()
}


ridge_weights <- function(form, y, data, rf_wrapper, case_weights) {
  N <- nrow(data)
  idx <- sample.int(N, round(N*.75), prob = case_weights)
  test  <- dplyr::slice(data, -idx)
  train <- dplyr::slice(data,  idx)
  rf_mod <- rf_wrapper(form, train, case.weights = case_weights[idx])
  y <- test %>%
    dplyr::pull(y)
  pred_test <- predict(rf_mod, test, predict.all = T) %>%
    magrittr::use_series(predictions)
  cv_model <- glmnet::cv.glmnet(
    pred_test,
    y,
    alpha = 0,
    intercept = F,
    nfolds = 5,
    standardize = F
  )
  w <- abs(coef(cv_model, s = 'lambda.min')[-1,])
  w <- w/sum(w)
  return(
    list(
      rf_mod = rf_mod,
      w = w
    )
  )
}

sse_weights <- function(form, y, data, rf_wrapper, case_weights) {
  N <- nrow(data)
  idx <- sample.int(N, round(N*.75), prob = case_weights)
  test  <- dplyr::slice(data, -idx)
  train <- dplyr::slice(data,  idx)
  rf_mod <- rf_wrapper(form, train, case.weights = case_weights[idx])
  y <- test %>%
    dplyr::pull(y)
  w <- predict(rf_mod, test, predict.all = T) %>%
    magrittr::use_series(predictions) %>%
    apply(2, \(.x) calc_nrmse(.x, y)) %>%
    {./sum(.)}
  return(
    list(
      rf_mod = rf_mod,
      w = w
    )
  )
}
