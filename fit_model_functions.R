get_model_options <- function(model_name, fit_dir = 'fits', names = FALSE){
  
  default_prior <- c(prior('normal(0,1)', class = 'b'), 
                     prior('student_t(3, 0, 1.25)', class = 'sd'),
                     prior('student_t(3, 0, 2)', class = 'sds'),
                     prior('student_t(3, 0, 1.25)', class = 'sigma'))
  default_prior_no_sigma <- c(prior('normal(0,1)', class = 'b'), 
                              prior('student_t(3, 0, 1.25)', class = 'sd'),
                              prior('student_t(3, 0, 2)', class = 'sds'))
  rt_acc_age_prepotofac_prior <- c(prior('normal(0,1)', class = 'b', resp = 'correct'), 
                                   prior('student_t(3, 0, 1.25)', class = 'sd', resp = 'correct'),
                                   prior('student_t(3, 0, 2)', class = 'sds', resp = 'correct'),
                                   prior('normal(0,1)', class = 'b', resp = 'RTshape'), 
                                   prior('student_t(3, 0, 1.25)', class = 'sd', resp = 'RTshape'),
                                   prior('student_t(3, 0, 2)', class = 'sds', resp = 'RTshape'),
                                   prior('student_t(3, 0, 1.25)', class = 'sigma', resp = 'RTshape'))
  default_cor_prior <- c(prior('lkj(2)', class = 'cor'))
  distmodel_prior <- c(prior('normal(0,1)', class = 'b'), 
                       prior('student_t(3, 0, 1.25)', class = 'sd'),
                       prior('student_t(3, 0, 2)', class = 'sds'),
                       prior('student_t(3, 0, 3)', lb = 0, class = 'Intercept', dpar = 'sigma'),
                       prior('normal(0,3)', class = 'b', dpar = 'sigma'))
  
  #This sets up the specification for all models described in the preregistration.
  brm_model_options_list <- list(
    #1. How does reaction time vary across age?
    rt_age = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + s(age, k = 10) + (1 + time | sID_factor/runN_factor)),
                  family = brms::lognormal(), 
                  prior = default_prior,
                  file = NULL),
    #2. How does sensitivity to prepotency vary across age? Distributional model.
    rt_age_prepot = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + t2(age, exact_prepotency, k = c(10,4)) +
                                        (1 + time | sID_factor:runN_factor) + 
                                        (1 + time + exact_prepotency | sID_factor)),
                         family = brms::lognormal(), 
                         prior = default_prior,
                         file = NULL),
    #2. How does sensitivity to prepotency vary across age? Distributional model.
    
    #mgcv docs: If the factor is ordered, then no smooth is produced for its first
    #level: this is useful for setting up models which have a reference level
    #smooth and then difference to reference smooths for each factor level except
    #the first (which is the reference).
    rt_age_prepotofac = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + mo(exact_prepotency_ofactor) + 
                                            t2(age, exact_prepotency_ofactor, bs = c('tp', 're')) +
                                            (1 + time | sID_factor:runN_factor) + 
                                            (1 + time + mo(exact_prepotency_ofactor) | sID_factor)),
                             family = brms::lognormal(), 
                             prior = default_prior,
                             file = NULL),
    rt_age_prepotofac_long = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + exact_prepotency_ofactor + s(age) + 
                                                 s(age, by = exact_prepotency_ofactor) +
                                                 (1 + time | sID_factor:runN_factor) + 
                                                 (1 + time + exact_prepotency_ofactor + age | sID_factor)),
                                  family = brms::lognormal(), 
                                  prior = default_prior,
                                  file = NULL),
    #2. How does sensitivity to prepotency vary across age? Distributional model.
    rt_age_prepotofac_null = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + exact_prepotency_ofactor + s(age) + 
                                                 (1 + time| sID_factor:runN_factor) + 
                                                 (1 + time + exact_prepotency_ofactor | sID_factor)),
                                  family = brms::lognormal(), 
                                  prior = c(default_prior, default_cor_prior),
                                  file = NULL),
    #2. How does sensitivity to prepotency vary across age? Distributional model.
    rt_age_prepotofac_sigma = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + mo(exact_prepotency_ofactor) + 
                                                  t2(age, exact_prepotency_ofactor, bs = c('tp', 're')) +
                                                  (1 + time | sID_factor:runN_factor) + 
                                                  (1 + time + mo(exact_prepotency_ofactor) | sID_factor),
                                                sigma ~ 1 + mo(exact_prepotency_ofactor) + s(age) + (1 | sID_factor)),
                                        prior = distmodel_prior,
                                        family = brms::lognormal(), 
                                        file = NULL),
    #2. How does sensitivity to prepotency vary across age? (null model for LOOIC
    #comparison)
    rt_age_prepot_null = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + s(age, k = 10) + s(exact_prepotency, k = 4) +
                                             (1 + time | sID_factor:runN_factor) + 
                                             (1 + time + exact_prepotency | sID_factor)),
                              family = brms::lognormal(), 
                              prior = default_prior, 
                              file = NULL),
    #3. How does accuracy vary across age? 
    acc_age = list(formula = bf(correct ~ s(age, k = 10) + time + (1 + time | sID_factor/runN_factor),
                                family = brms::bernoulli()), 
                   prior = default_prior_no_sigma, 
                   file = NULL),
    acc_age_prepotofac = list(formula = bf(correct ~ s(age, k = 10) + time + 
                                             exact_prepotency_ofactor + s(age, by = exact_prepotency_ofactor, k = 10) + 
                                             (1 + time | sID_factor:runN_factor) +
                                             (1 + time + exact_prepotency_ofactor | sID_factor),
                                           family = brms::bernoulli()), 
                              prior = default_prior_no_sigma, 
                              file = NULL),
    
    #4. How does change in reaction time across sequential go-trials predict
    #accuracy?
    acc_rt_prepot = list(formula = bf(RT.shape | subset(subsetRT) ~ time + t2(age, exact_prepotency, k = c(10,3)) + 
                                        (1 + time + exact_prepotency |ID1| sID_factor) + 
                                        (1 + time | sID_factor:runN_factor),
                                      family = brms::lognormal()) + 
                           bf(corReject.total | trials(nogo.total) + subset(subsetACC) ~ 1 + s(age_ACC, k = 10) + 
                                (1 |ID1| sID_factor),
                              family = binomial()) + 
                           set_rescor(rescor = FALSE), 
                         prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                         file = NULL),
    acc_rt_prepot_lin = list(formula = bf(RT.shape | subset(subsetRT) ~ time + age*exact_prepotency + 
                                            (1 + time + exact_prepotency |ID1| sID_factor) + 
                                            (1 + time | sID_factor:runN_factor),
                                          family = brms::lognormal()) + 
                               bf(corReject.total | trials(nogo.total) + subset(subsetACC) ~ 1 + s(age_ACC, k = 10) + 
                                    (1 |ID1| sID_factor),
                                  family = binomial()) + 
                               set_rescor(rescor = FALSE), 
                             prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                             file = NULL),
    #5. How does accuracy vary with previous conditioning (rewarded or punished
    #shapes from the Guessing task)?
    acc_prevcond = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                       1 + nogoCondition + s(age_ACC, by = nogoCondition, k = 10) + 
                                       (1 | sID_factor),
                                     family = binomial()),  
                        prior = default_prior_no_sigma,
                        file = NULL),
    
    #5. How does accuracy vary with previous conditioning (rewarded or punished
    #shapes from the Guessing task)? (null model for LOOIC comparison)
    acc_prevcond_null = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                            1 + nogoCondition + s(age_ACC, k = 10) + (1 | sID_factor),
                                          family = binomial()),  
                             prior = default_prior_no_sigma,
                             file = NULL),
    #6. How do the reaction times of responses in the Guessing task vary between
    #high and low magnitude blocks?
    guessing_rt = list(formula = bf(guessResp.firstRt ~ valueCondition + s(age, by = valueCondition, k = 10) + (1 | sID_factor/runN_factor)),
                       family = brms::lognormal(),  
                       prior = default_prior,
                       file = NULL),
    #6.Null for age interaction re How do the reaction times of responses in the
    #Guessing task vary between high and low magnitude blocks?
    guessing_rt_null = list(formula = bf(guessResp.firstRt ~ valueCondition + s(age, k = 10) + (1 | sID_factor/runN_factor)),
                            family = brms::lognormal(),  
                            prior = default_prior,
                            file = NULL),
    #E1-4. How does change in reaction time across sequential go-trials predict
    #accuracy? Include prepotency in both models.
    rt_acc_age_prepotofac = list(formula = bf(RT.shape | trunc(ub = .8) + subset(subsetRT) ~ time + exact_prepotency_ofactor + s(age) + 
                                                # s(age, by = exact_prepotency_ofactor) +
                                                (1 + time | sID_factor:runN_factor) + 
                                                (1 + time + exact_prepotency_ofactor |ID1| sID_factor),
                                              family = brms::lognormal()) + 
                                   bf(correct | subset(subsetACC) ~ s(age, k = 10) + time + 
                                        exact_prepotency_ofactor + 
                                        # s(age, by = exact_prepotency_ofactor, k = 10) + 
                                        (1 + time | sID_factor:runN_factor) +
                                        (1 + time + exact_prepotency_ofactor |ID1| sID_factor),
                                      family = brms::bernoulli()) + 
                                   set_rescor(rescor = FALSE), 
                                 prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                                 file = NULL),
    rt_acc_age_prepotofac_null = list(formula = bf(RT.shape | trunc(ub = .8) + subset(subsetRT) ~ time + exact_prepotency_ofactor + s(age) + 
                                                     # s(age, by = exact_prepotency_ofactor) +
                                                     (1 + time | sID_factor:runN_factor) + 
                                                     (1 + time |ID1| sID_factor) + 
                                                     (0 + exact_prepotency_ofactor | sID_factor),
                                                   family = brms::lognormal()) + 
                                        bf(correct | subset(subsetACC) ~ s(age, k = 10) + time + 
                                             exact_prepotency_ofactor + 
                                             # s(age, by = exact_prepotency_ofactor, k = 10) + 
                                             (1 + time | sID_factor:runN_factor) +
                                             (1 + time |ID1| sID_factor) + 
                                             (0 + exact_prepotency_ofactor | sID_factor),
                                           family = brms::bernoulli()) + 
                                        set_rescor(rescor = FALSE), 
                                      prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                                      file = NULL),
    rt_acc_age_prepotofac_prevcond = list(formula = bf(RT.shape | trunc(ub = .8) + subset(subsetRT) ~ time + 
                                                         mo(exact_prepotency_ofactor) + 
                                                         t2(age, exact_prepotency, bs = c('tp', 're')) +
                                                         (1 + time | sID_factor:runN_factor) + 
                                                         (1 + time + mo(exact_prepotency_ofactor) |ID1| sID_factor),
                                                       family = brms::lognormal()) + 
                                            bf(correct | subset(subsetACC) ~ time + 
                                                 mo(exact_prepotency_ofactor) + 
                                                 nogoCondition +
                                               t2(age, exact_prepotency, nogoCondition, bs = c('tp', 're', 're')) +
                                                 (1 + time | sID_factor:runN_factor) +
                                                 (1 + time + mo(exact_prepotency_ofactor) |ID1| sID_factor),
                                               family = brms::bernoulli()) + 
                                            set_rescor(rescor = FALSE), 
                                          prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                                          file = NULL),
    rt_acc_age_prepotofac_lin = list(formula = bf(RT.shape | trunc(ub = .8) ~ time + exact_prepotency_ofactor*age +
                                                    (1 + time | sID_factor:runN_factor) + 
                                                    (1 + time + exact_prepotency_ofactor |ID1| sID_factor),
                                                  family = brms::lognormal()) + 
                                       bf(correct ~ s(age, k = 10) + time + exact_prepotency_ofactor*age + 
                                            (1 + time | sID_factor:runN_factor) +
                                            (1 + time + exact_prepotency_ofactor | sID_factor),
                                          family = brms::bernoulli()) + 
                                       set_rescor(rescor = FALSE), 
                                     prior = c(rt_acc_age_prepotofac_prior, default_cor_prior),
                                     file = NULL))
  
  if(names){
    return(names(brm_model_options_list))
  } else {
    model_options <- brm_model_options_list[[model_name]]
    model_options$file <- file.path(fit_dir, model_name)
    return(model_options)
  }
}

get_model_data <- function(MODEL, TEST = FALSE, TESTPROP = .0125, LONG = FALSE, ONLYLONG = FALSE){
  if(!file.exists('carit_data.rds')){
    source('process_carit.R')
  } else {
    carit_data <- readRDS('carit_data.rds')
  }
  if(!file.exists('guessing_data.rds')){
    source('process_guessing.R')
  } else {
    guessing_data <- readRDS('guessing_data.rds')
  }
  
  if(!LONG){
    carit_data <- carit_data[wave == 1]
  } else if(ONLYLONG){
    long_pid <- carit_data[, .(nwaves = length(unique(wave))), by = 'sID'][nwaves > 1, 'sID']
    carit_data <- carit_data[long_pid, on = 'sID']
  } 
  
  carit_no_RT_ids <- carit_data[, list(N_RTs = sum(!is.na(RT.shape))), by = 'sID'][N_RTs == 0]
  carit_data <- carit_data[!carit_no_RT_ids, on = 'sID']
  
  carit_data$sID_factor <- as.factor(carit_data$sID)
  carit_data$runN_factor <- as.factor(carit_data$runN)
  carit_data$sID_runN_factor <- as.factor(paste(carit_data$sID, carit_data$runN))
  carit_data$exact_prepotency_factor <- factor(carit_data$exact_prepotency, levels = c(0, 1, 2, 3, 4))
  carit_data$exact_prepotency_ofactor <- factor(carit_data$exact_prepotency, levels = c(0, 1, 2, 3, 4), ordered = TRUE)
  carit_data$age_c <- carit_data$age - 11
  carit_data[, time := (shapeStartTime - mean(shapeStartTime, na.rm = TRUE)) / 100, by = c('sID_factor')]
  
  guessing_data$sID_factor <- as.factor(guessing_data$sID)
  guessing_data$runN_factor <- as.factor(guessing_data$run)
  
  guessing_data <- merge(guessing_data, unique(carit_data[, c('sessionID', 'age')]), all.x = TRUE, all.y = FALSE, by = 'sessionID')
  
  # SETTING UP, testing with 1.25% of the data
  
  if(TEST){
    set.seed(10) 
    traintest.sID <- unique(carit_data[, 'sID'])[, train := rbinom(length(sID), 1, TESTPROP)]
    carit_data <- merge(carit_data, traintest.sID, by = "sID")[train==1]
    traintest.sID.g <- unique(guessing_data[, 'sID'])[, train := rbinom(length(sID), 1, TESTPROP)]
    guessing_data <- merge(guessing_data, traintest.sID.g, by = "sID")[train==1]
  }

  
  #Different models require different ways of formatting the data.
  
  #Just the "hit" trials (go trials with reaction times). We exclude trials that
  #have RTs that are outside the possible response window. This is about 72
  #trials, so a drop in the bucket
  
  carit.hits <- na.omit(carit_data[corrRespTrialType == 'Hit' & RT.shape > 0 & RT.shape <= .8, 
                                   c('RT.shape', 'age', 
                                     'exact_prepotency', 'exact_prepotency_factor', 'exact_prepotency_ofactor',
                                     'sID_factor', 'runN_factor', 'sID_runN_factor',
                                     'time')])
  
  #Number of correct rejections out of all no-go trials for each participant-run.
  carit.acc <- na.omit(carit_data[trialType == 'nogo', 
                                  list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                                       'nogo.total' = .N,
                                       'age_ACC' = unique(age)),
                                  by = c('sID_factor',
                                         'runN_factor')])
  
  #Trial-level no-go accuracy with time and prepotency
  carit.acc.trial <-  na.omit(carit_data[trialType == 'nogo', c('corrRespTrialType', 'age', 'time', 
                                                                'exact_prepotency', 'exact_prepotency_factor', 'exact_prepotency_ofactor',
                                                                'nogoCondition', 'sID_factor', 'runN_factor', 'sID_runN_factor')])
  carit.acc.trial[, correct := as.numeric(corrRespTrialType == 'corReject')]
  
  #Number of correct rejections out of all no-go trials for each participant-run,
  #for each level of the no-go previously-conditioned trial type.
  carit.acc.pc <-  na.omit(carit_data[trialType == 'nogo', 
                                      list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                                           'nogo.total' = .N,
                                           'age_ACC' = unique(age)),
                                      by = c('sID_factor',
                                             'runN_factor',
                                             'nogoCondition')])
  
  #We combine the two data sets for the model correlating prepotency and accuracy.
  carit.RT.acc <- data.table::rbindlist(list(RT = carit.hits, ACC = carit.acc), fill = TRUE, idcol = 'subset')
  carit.RT.acc[, c('subsetRT', 'subsetACC') :=
                 list(subset == 'RT',
                      subset == 'ACC')]
  
  #We combine the two data sets for the model correlating prepotency and accuracy.
  carit.RT.acc.trial <- data.table::rbindlist(list(RT = carit.hits, ACC = carit.acc.trial), fill = TRUE, idcol = 'subset')
  carit.RT.acc.trial[, c('subsetRT', 'subsetACC') :=
                       list(subset == 'RT',
                            subset == 'ACC')]
  
  message(sprintf('Total participants in CARIT Hit data: %d', unique(carit.hits[, c('sID_factor')])[, .N]))
  message(sprintf('Total participants in CARIT Acc data: %d', unique(carit.acc[, c('sID_factor')])[, .N]))
  message(sprintf('Total participants in CARIT Acc PC data: %d', unique(carit.acc.pc[, c('sID_factor')])[, .N]))
  message(sprintf('Age in years: %0.2f - %0.2f', min(carit.hits$age), max(carit.hits$age)))
  
  message(paste(knitr::kable(
    carit.hits[, .N, by = c('sID_factor', 'runN_factor')][,list(max_Go_trials = max(N),
                                                                min_Go_trials = min(N))],
    format = 'simple'), collapse = '\n'))
  message(paste(knitr::kable(carit.acc.pc[, list(max_NoGo_trials = max(nogo.total),
                                                 min_NoGo_trials = min(nogo.total),
                                                 max_NoGo_FA = max(nogo.total - corReject.total),
                                                 min_NoGo_FA = min(nogo.total - corReject.total)),
                                          by = 'nogoCondition'], 
                             format = 'simple'), collapse = '\n'))
  message(paste(knitr::kable(carit.acc[, list(max_NoGo_trials = max(nogo.total),
                                              min_NoGo_trials = min(nogo.total),
                                              max_NoGo_FA = max(nogo.total - corReject.total),
                                              min_NoGo_FA = min(nogo.total - corReject.total))],
                             format = 'simple'), collapse = '\n'))
  
  if (MODEL %in% c('rt_age',
                   'rt_age_prepot',
                   'rt_age_prepotofac',
                   'rt_age_prepotofac_null',
                   'rt_age_prepotofac_null_sigma',
                   'rt_age_prepot_null')) {
    model_data_name <- 'carit.hits'
  } else if (MODEL %in% 'acc_age') {
    model_data_name <- 'carit.acc'
  } else if (MODEL %in% c('acc_rt_prepot', 'acc_rt_prepot_lin')) {
    model_data_name <- 'carit.RT.acc'
  } else if (MODEL %in% c('acc_prevcond', 'acc_prevcond_null')){
    model_data_name <- 'carit.acc.pc'
  } else if (MODEL %in% c('guessing_rt', 'guessing_rt_null')){
    model_data_name <- 'guessing_data'
  } else if (MODEL %in% c('acc_age', 'acc_age_prepotofac')) {
    model_data_name <- 'carit.acc.trial'
  } else if (MODEL %in% c('rt_acc_age_prepotofac',
                          'rt_acc_age_prepotofac_null',
                          'rt_acc_age_prepotofac_prevcond',
                          'rt_acc_age_prepotofac_prevcond_null',
                          'rt_acc_age_prepotofac_lin')){
    model_data_name <- 'carit.RT.acc.trial'
  } else {
    warning('Model name not found, defaulting to carit.hits')
    model_data_name <- 'carit.hits'
  }
  
  model_data <- get(model_data_name)
  
  return(model_data)
}