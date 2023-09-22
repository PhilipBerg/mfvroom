lasso_trim_rf <- function(form, y, train_data, valid_data, rf_wrapper) {
  rf_mod <- rf_wrapper(form, train_data)
  y_trn <- train_data %>%
    dplyr::pull(y)
  x_trn <- train_data %>%
    dplyr::select(-y)
  y_vld <- valid_data %>%
    dplyr::pull(y)
  x_vld <- valid_data %>%
    dplyr::select(-y)
  pred_train <- predict(rf_mod, train_data, predict.all = T) %>%
    magrittr::use_series(predictions)
  cv_model <- cv.glmnet(pred_train, y_trn, alpha = 1, intercept = F)
  trim_trees <- rownames(coef(cv_model, s = 'lambda.min'))[coef(cv_model, s = 'lambda.min')[,1]!= 0] %>%
    stringr::str_remove("V") %>%
    as.integer() %>%
    {which(!seq_len(ncol(pred_train)) %in% .)}
  suppressWarnings(rf_trim <- ranger::deforest(rf_mod, trim_trees))
  trim_valid <- predict(rf_trim, valid_data) %>%
    magrittr::use_series(predictions) %>%
      calc_nrmse(y_vld)
  full_valid <- predict(rf_mod, valid_data) %>%
    magrittr::use_series(predictions) %>%
    calc_nrmse(y_vld)
  if (trim_valid < full_valid) {
    rf_trim
  } else {
    rf_mod
  }
}


calc_nrmse <- function(x, y) {
  {x - y} %>%
    magrittr::raise_to_power(2) %>%
    mean() %>%
    magrittr::divide_by(var(y)) %>%
    sqrt()
}
