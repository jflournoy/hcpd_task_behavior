#----functions----

read_fit_models <- function(model_name, model_dir = 'fits'){
  require(brms)
  fnames <- file.path(model_dir, sprintf('%s_c%02d.rds', model_name, 1:4))
  fnames <- fnames[unlist(lapply(fnames, file.exists))]
  models <- lapply(fnames, readRDS)
  fit <- do.call(combine_models, models)
  return(fit)
}

plot_cond_eff <- function(fit, xvar, xname, yname, conditions = NULL, condition_title = NULL, ribbon_color = '#bc5090', line_color = '#003f5c', mean_center_y = FALSE, method = 'posterior_epred', points = FALSE, points_size = .5, points_alpha = .5) {
  require(brms)
  require(ggplot2)
  require(viridisLite)
  require(data.table)
  epred <- conditional_effects(fit, effects = xvar,
                               conditions = conditions,
                               re_formula = NA, 
                               prob = .95, robust = TRUE, 
                               method = method, plot = FALSE)
  epred[[xvar]] <- as.data.table(epred[[xvar]])
  if(points){
    resp_var <- all.vars(fit$formula$formula)[[1]]
    points_geom <- geom_point(data = fit$data, aes_string(y = resp_var),
                              size = points_size, alpha = points_alpha)
  } else {
    points_geom <- NULL
  }
  if(!is.null(conditions)){
    if(mean_center_y){
      cols <- c('estimate__', 'lower__', 'upper__')
      epred[[xvar]][, mean_estimate__ := mean(estimate__), by = xvar]
      epred[[xvar]][, (cols) := lapply(.SD, \(x) x - mean_estimate__), by = xvar, .SDcols = cols]
    }
    condition_name <- names(conditions)[1]
    set(epred[[xvar]], j = condition_name, value = factor(epred[[xvar]][[condition_name]], levels = conditions[,1]))
    p <- ggplot(epred[[xvar]], aes_string(x = xvar, y = 'estimate__', 
                                          group = condition_name,
                                          color = condition_name,
                                          fill = condition_name)) + 
      points_geom +
      geom_ribbon(aes(ymin = lower__, ymax = upper__, color = NULL), alpha = .25) + 
      geom_line() + 
      scale_color_manual(values = viridisLite::magma(length(conditions[,1]), end = .75),
                         aesthetics = c('color', 'fill')) + 
      labs(x = xname, y = yname, color = condition_title, fill = condition_title)
  } else {
    p <- ggplot(epred[[xvar]], aes_string(x = xvar, y = 'estimate__')) + 
      points_geom +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_color, alpha = .5) + 
      geom_line(color = line_color) + 
      labs(x = xname, y = yname)
  }
  p <- p +
    theme_minimal()
  return(p)
}

posterior_effect_size <- function(fit, newdata, es_name = 'Effect size'){
  y_pred <- posterior_epred(fit, newdata = newdata, re_formula = NA)
  ES_post <- data.frame(ES = y_pred[, 2] - y_pred[, 1])
  names(ES_post) <- es_name
  ES_summary <- posterior_summary(ES_post)
  return(list(summary = ES_summary, posterior = ES_post))
}