#----functions----

read_fit_models <- function(model_name, model_dir = 'fits'){
  #model_name <- 'rtage'
  require(brms)
  fnames <- file.path(model_dir, sprintf('%s_c%02d.rds', model_name, 1:4))
  models <- lapply(fnames, readRDS)
  fit <- do.call(combine_models, models)
  return(fit)
}