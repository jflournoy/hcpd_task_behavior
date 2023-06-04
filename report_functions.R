#----functions----

combine_kfold <- function(klist){
  #klist = lapply(models, \(x) x$criteria$kfold)
  lapply(klist, \(x) x$estimate['elpd_kfold', 'Estimate'])
  
  kfold_elpd_composite <- kfold1$estimates["elpd_kfold","Estimate"] + kfold2$estimates["elpd_kfold","Estimate"]
  kfold_elpd_se_composite <- sqrt(kfold1$estimates["elpd_kfold","SE"]^2+kfold2$estimates["elpd_kfold","SE"]^2)
  
}

read_fit_models <- function(model_name, model_dir = 'fits'){
  require(brms)
  fnames <- file.path(model_dir, sprintf('%s_c%02d.rds', model_name, 1:4))
  fnames <- fnames[unlist(lapply(fnames, file.exists))]
  models <- lapply(fnames, readRDS)
  fit <- combine_models(mlist = models)
  models[[1]][['criteria']]$kfold
  
  return(fit)
}

plot_cond_eff <- function(fit, xvar, xname, yname, conditions = NULL, condition_title = NULL, ribbon_color = '#bc5090', line_color = '#003f5c', mean_center_y = FALSE, method = 'posterior_epred', points = FALSE, points_size = .5, points_alpha = .5, hex = FALSE, hex_args = list(alpha = .8, binwidth = c(.5, .025)), hex_log = FALSE) {
  require(brms)
  require(ggplot2)
  require(viridisLite)
  require(data.table)
  require(ggnewscale)
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
  if(hex){
    resp_var <- all.vars(fit$formula$formula)[[1]]
    mapping <- aes_string(y = resp_var)
    if(hex_log){
      hex_fill_scale <- scale_fill_viridis_c(option = 'mako', trans = 'log', begin = .4, end = .95)
    } else {
      hex_fill_scale <- scale_fill_viridis_c(option = 'mako', begin = .4, end = .95)
    }
    hex_geom <- do.call(geom_hex, c(list(data = fit$data, mapping = mapping), hex_args))
  } else {
    hex_geom <- NULL
    hex_fill_scale <- NULL
  }
  if(!is.null(conditions)){
    if(mean_center_y){
      cols <- c('estimate__', 'lower__', 'upper__')
      epred[[xvar]][, mean_estimate__ := mean(estimate__), by = xvar]
      epred[[xvar]][, (cols) := lapply(.SD, \(x) x - mean_estimate__), by = xvar, .SDcols = cols]
    }
    condition_name <- names(conditions)[1]
    set(epred[[xvar]], j = condition_name, value = factor(epred[[xvar]][[condition_name]], levels = conditions[,1]))
    p <- ggplot(epred[[xvar]], aes_string(x = xvar, y = 'estimate__')) + 
      points_geom +
      hex_geom + 
      hex_fill_scale +
      ggnewscale::new_scale_fill() +
      geom_ribbon(aes_string(ymin = 'lower__', ymax = 'upper__', fill = condition_name, group = condition_name), 
                  alpha = .5) + 
      geom_line(aes_string(color = condition_name, fill = NULL, group = condition_name)) + 
      scale_color_viridis_d(option = 'inferno', begin = 0, end = .6) +
      scale_fill_viridis_d(option = 'inferno', begin = .3, end = 1) +
      labs(x = xname, y = yname, color = condition_title, fill = condition_title)
  } else {
    p <- ggplot(epred[[xvar]], aes_string(x = xvar, y = 'estimate__')) + 
      points_geom +
      hex_geom +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = ribbon_color, alpha = .5) + 
      geom_line(color = line_color) + 
      new_scale_fill() +
      hex_fill_scale + 
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

make_custom_prediction <- function(newdata, varnames = NULL){
  custom_prediction <- function(x, ...){
    pred_draws <- brms::posterior_epred(x, newdata, re_formula = NA)
    pred_draws_df <- posterior::as_draws_df(pred_draws)
    iter_per_chain <- brms::ndraws(x) / brms::nchains(x)
    pred_draws_df$.chain <- (pred_draws_df$.draw - 1) %/% iter_per_chain + 1
    pred_draws_df$.iteration <- (pred_draws_df$.draw - 1) %% iter_per_chain + 1
    if(is.null(varnames)){
      varnames <- 1:nrow(newdata)
    }
    names(pred_draws_df)[1:nrow(newdata)] <- varnames
    return(pred_draws_df)
  }
  return(custom_prediction)
}

remove_pss_draws <- function(psseq, keep){
  #psseq <- x_psseq
  #keep = x_prednames
  draws_cols <- c('.chain', '.iteration', '.draw')
  keep_ <- c(keep, draws_cols)
  psseq$base_draws <- psseq$base_draws[, keep_]
  for(i in seq_along(psseq$prior_scaled$draws_sequence)){
    psseq$prior_scaled$draws_sequence[[i]]$draws <- psseq$prior_scaled$draws_sequence[[i]]$draws[, keep_]
  }
  for(i in seq_along(psseq$likelihood_scaled$draws_sequence)){
    psseq$likelihood_scaled$draws_sequence[[i]]$draws <- psseq$likelihood_scaled$draws_sequence[[i]]$draws[, keep_]
  }
  gc()
  return(psseq)
}
powerscale_sequence_pred <- function(x, x_var, res = 20){
  require(data.table)
  #x is a brms fit
  #x <- rtageprepotalt_null_fit
  #x_var = 'age'
  #res = 20
  draws_cols <- c('.chain', '.iteration', '.draw')
  x_var_seq <- do.call(seq, c(as.list(range(x$data[, x_var])), length.out = 20))
  x_var_newdata <- data.frame(x_var = x_var_seq)
  formula_vars <- all.vars(x$formula$formula)
  response_var <- formula_vars[attr(terms(x$formula$formula), "response")]
  covars <- x$data[1, formula_vars[which(!formula_vars %in% c(x_var, response_var))]]
  names(x_var_newdata) <- x_var
  x_var_newdata <- cbind(x_var_newdata, covars)
  x_prednames <- paste('newdata', 1:nrow(x_var_newdata), sep = '_')
  x_psseq <- 
    remove_pss_draws(
      priorsense::powerscale_sequence(
        x, 
        prediction = make_custom_prediction(x_var_newdata, varnames = x_prednames)),
      keep = x_prednames)
  
  prior_lik_scaled <- c(list(list(draws = x_psseq$base_draws)),
                        x_psseq[['prior_scaled']][['draws_sequence']], 
                        x_psseq[['likelihood_scaled']][['draws_sequence']])
  
  powerscale_pred_draws_list <- lapply(prior_lik_scaled, \(y){
    d <- cbind(posterior_summary(as.data.frame(y$draws)[, x_prednames]), x_var_newdata)
    d$component <- y$powerscaling$component
    d$alpha <- y$powerscaling$alpha
    return(d)
  })
  powerscale_pred_draws <- rbindlist(powerscale_pred_draws_list, fill = TRUE)
  powerscale_pred_draws_base <- powerscale_pred_draws[is.na(alpha)]
  powerscale_pred_draws_ps <- powerscale_pred_draws[!is.na(alpha)]
  powerscale_pred_draws_base[, .id := 1:.N]
  powerscale_pred_draws_base <- powerscale_pred_draws_base[rep(.id, 2)]
  powerscale_pred_draws_base[, c('component', 'alpha', '.id') := 
                               list(rep(c('prior', 'likelihood'), each = .N/2),
                                    rep(1, .N),
                                    NULL)]
  powerscale_pred_draws <- rbindlist(list(powerscale_pred_draws_base, powerscale_pred_draws_ps))
  powerscale_pred_draws[, alpha_label := sprintf('%0.2f', alpha)]
  powerscale_pred_draws_plot <- ggplot(powerscale_pred_draws, aes(x = age, y = Estimate)) + 
    geom_ribbon(aes(ymin = Q2.5, ymax = Q97.5), color = 'blue', fill = '#FAFAFA', alpha = .5) + 
    geom_line(alpha = .5, color = 'red') + 
    facet_grid(component ~ alpha_label, labeller = ) + 
    theme_minimal()
  
  pred_psseq <- list(plot = powerscale_pred_draws_plot, plot_data = powerscale_pred_draws, psseq = x_psseq)
  return(pred_psseq)
}
