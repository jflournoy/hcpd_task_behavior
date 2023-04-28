.libPaths(c('/ncf/mclaughlin/users/jflournoy/R/x86_64-pc-linux-gnu-library/verse-4.2.1', .libPaths()))

library(data.table)
library(brms)
library(argparse)

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add a help option 
parser$add_argument(
  "--model", type = "character", default = 'rtage',
  help = "Which model to fit. Options are [rtage|rtageprepot|rtageprepotalt|rtageprepotnull|accage|accprepot|accprevcond|accprevcondnull|rtguessing|rtguessingnull|rtagepropotdist|acctrial|accprepottrial]")
parser$add_argument('--id', type = "integer", default = '1', help = 'Chain ID')
parser$add_argument("--test", action = "store_true", 
                    help = "Run on small subset of data?")
parser$add_argument("--refit", action = "store_true", 
                    help = "Overwrite existing model?")
if(interactive()){
  args <- parser$parse_args(c('--model', 'rtageprepotnull', '--id', '1', '--refit', '--test'))
} else {
  args <- parser$parse_args()
}

MODEL <- args$model
TEST <- args$test
CHAINID <- args$id
REFIT <- args$refit

get_model_data_name <- function(model){
  if (model %in% c('rtage',
                   'rtageprepot',
                   'rtageprepotalt',
                   'rtageprepotdist',
                   'rtageprepotnull')) {
    model_data_name <- 'carit.hits'
  } else if (MODEL %in% 'accage') {
    model_data_name <- 'carit.acc'
  } else if (MODEL %in% c('accprepot', 'accprepotlin')) {
    model_data_name <- 'carit.RT.acc'
  } else if (MODEL %in% c('accprevcond', 'accprevcondnull')){
    model_data_name <- 'carit.acc.pc'
  } else if (MODEL %in% c('rtguessing', 'rtguessingnull')){
    model_data_name <- 'guessing_data'
  } else if (MODEL %in% c('acctrial', 'accprepottrial')) {
    model_data_name <- 'carit.acc.trial'
  } else {
    warning('Model name not found, defaulting to carit.hits')
    model_data_name <- 'carit.hits'
  }
  return(model_data_name)
}

MODEL_DATA_NAME <- get_model_data_name(MODEL)
  
source('process_carit.R')
source('process_guessing.R')

carit <- carit[wave == 1]
carit <- carit[filename != '/ncf/hcp/data/CCF_HCD_STG_PsychoPy_files//HCD0353538/tfMRI_CARIT_PA/CARIT_HCD0353538_V1_A_run1_wide.csv']
carit_no_RT_ids <- carit[, list(N_RTs = sum(!is.na(RT.shape))), by = 'sID'][N_RTs == 0]
carit <- carit[!carit_no_RT_ids, on = 'sID']

carit$sID_factor <- as.factor(carit$sID)
carit$runN_factor <- as.factor(carit$runN)
carit$sID_runN_factor <- as.factor(paste(carit$sID, carit$runN))
carit$exact_prepotency_factor <- factor(carit$exact_prepotency, levels = c(0, 1, 2, 3))
carit$exact_prepotency_ofactor <- factor(carit$exact_prepotency, levels = c(0, 1, 2, 3), ordered = TRUE)

guessing_data$sID_factor <- as.factor(guessing_data$sID)
guessing_data$runN_factor <- as.factor(guessing_data$run)

guessing_data <- merge(guessing_data, unique(carit[, c('sessionID', 'age')]), all.x = TRUE, all.y = FALSE, by = 'sessionID')

# SETTING UP, testing with 1.25% of the data

if(TEST){
  set.seed(10) 
  traintest.sID <- unique(carit[, 'sID'])[, train := rbinom(length(sID), 1, .0125)]
  carit <- merge(carit, traintest.sID, by = "sID")[train==1]
  traintest.sID.g <- unique(guessing_data[, 'sID'])[, train := rbinom(length(sID), 1, .0125)]
  guessing_data <- merge(guessing_data, traintest.sID.g, by = "sID")[train==1]
}

#Different models require different ways of formatting the data.

#Just the "hit" trials (go trials with reaction times). We exclude trials that
#have RTs that are outside the possible response window. This is about 72
#trials, so a drop in the bucket
carit[, time := shapeStartTime - mean(shapeStartTime, na.rm = TRUE), by = c('sID_factor')]
carit.hits <- na.omit(carit[corrRespTrialType == 'Hit' & RT.shape > 0 & RT.shape <= .8, 
                            c('RT.shape', 'age', 
                              'exact_prepotency', 'exact_prepotency_factor', 'exact_prepotency_ofactor',
                              'sID_factor', 'runN_factor', 'sID_runN_factor',
                              'time')])
dim(carit.hits)
unique(carit.hits[, c('sID_factor')])[, .(N = .N)]

#Number of correct rejections out of all no-go trials for each participant-run.
carit.acc <- na.omit(carit[trialType == 'nogo', 
                           list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                                'nogo.total' = .N,
                                'age_ACC' = unique(age)),
                           by = c('sID_factor',
                                  'runN_factor')])

#Trial-level no-go accuracy with time and prepotency
carit.acc.trial <-  na.omit(carit[trialType == 'nogo', c('corrRespTrialType', 'age', 'time', 
                                                         'exact_prepotency', 'exact_prepotency_factor', 'exact_prepotency_ofactor',
                                                         'nogoCondition', 'sID_factor', 'runN_factor', 'sID_runN_factor')])
carit.acc.trial[, correct := as.numeric(corrRespTrialType == 'corReject')]

#Number of correct rejections out of all no-go trials for each participant-run,
#for each level of the no-go previously-conditioned trial type.
carit.acc.pc <-  na.omit(carit[trialType == 'nogo', 
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
  
fit_dir <- 'fits'

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

model_data <- get(MODEL_DATA_NAME)

default_prior <- c(prior('normal(0,1)', class = 'b'), 
                   prior('student_t(3, 0, 1.25)', class = 'sd'),
                   prior('student_t(3, 0, 2)', class = 'sds'),
                   prior('student_t(3, 0, 1.25)', class = 'sigma'))

#This sets up the specification for all models described in the preregistration.
brm_model_options <- list(
  #1. How does reaction time vary across age?
  rtage = list(formula = bf(RT.shape | trunc(ub = .8) ~ s(age, k = 10) + (1 | sID_factor/runN_factor)),
               family = brms::lognormal(), 
               prior = default_prior,
               file = file.path(fit_dir, 'rtage')),
  #2. How does sensitivity to prepotency vary across age? Distributional model.
  rtageprepot = list(formula = bf(RT.shape | trunc(ub = .8) ~ t2(age, exact_prepotency, k = c(10,4)) +
                                    (1 | sID_factor:runN_factor) + 
                                    (1 + exact_prepotency | sID_factor)),
                     family = brms::lognormal(), 
                     prior = default_prior,
                     file = file.path(fit_dir, 'rtageprepot')),
  #2. How does sensitivity to prepotency vary across age? Distributional model.
  rtageprepotalt = list(formula = bf(RT.shape | trunc(ub = .8) ~ exact_prepotency_ofactor + s(age) + s(age, by = exact_prepotency_ofactor) +
                                       (1 | sID_factor:runN_factor) + 
                                       (1 + exact_prepotency_ofactor | sID_factor)),
                        family = brms::lognormal(), 
                        prior = default_prior,
                        file = file.path(fit_dir, 'rtageprepotalt')),
  #2. How does sensitivity to prepotency vary across age? Distributional model.
  rtageprepotdist = list(formula = bf(RT.shape | trunc(ub = .8) ~ t2(age, exact_prepotency, k = c(10,4)) +
                                        (1 | sID_factor:runN_factor) + 
                                        (1 + exact_prepotency | sID_factor),
                                      sigma ~ 1 + s(age) + (1 | sID_factor/runN_factor)),
                         prior = default_prior,
                         family = brms::lognormal(), 
                         file = file.path(fit_dir, 'rtageprepotdist')),
  #2. How does sensitivity to prepotency vary across age? (null model for LOOIC
  #comparison)
  rtageprepotnull = list(formula = bf(RT.shape | trunc(ub = .8) ~ s(age, k = 10) + s(exact_prepotency, k = 4) +
                                           (1 | sID_factor:runN_factor) + 
                                           (1 + exact_prepotency | sID_factor)),
                            family = brms::lognormal(), 
                            prior = default_prior, 
                            file = file.path(fit_dir, 'rtageprepotnull')),
  #3. How does accuracy vary across age? 
  accage = list(formula = bf(corReject.total | trials(nogo.total) ~ 1 + s(age_ACC, k = 10) + (1 | sID_factor)),
                family = binomial(), 
                file = file.path(fit_dir, 'accage')),
  
  #4. How does change in reaction time across sequential go-trials predict
  #accuracy?
  accprepot = list(formula = bf(RT.shape | subset(subsetRT) ~ t2(age, exact_prepotency, k = c(10,3)) + 
                                     (1 + exact_prepotency |ID1| sID_factor) + 
                                     (1 | sID_factor:runN_factor),
                                   family = brms::shifted_lognormal()) + 
                        bf(corReject.total | trials(nogo.total) + subset(subsetACC) ~ 1 + s(age_ACC, k = 10) + (1 |ID1| sID_factor),
                           family = binomial()) + 
                        set_rescor(rescor = FALSE),
                      file = file.path(fit_dir, 'accprepot')),
  accprepotlin = list(formula = bf(RT.shape | subset(subsetRT) ~ age*exact_prepotency + 
                                  (1 + exact_prepotency |ID1| sID_factor) + 
                                  (1 | sID_factor:runN_factor),
                                family = brms::shifted_lognormal()) + 
                     bf(corReject.total | trials(nogo.total) + subset(subsetACC) ~ 1 + s(age_ACC, k = 10) + (1 |ID1| sID_factor),
                        family = binomial()) + 
                     set_rescor(rescor = FALSE),
                   file = file.path(fit_dir, 'accprepotlin')),
  #5. How does accuracy vary with previous conditioning (rewarded or punished
  #shapes from the Guessing task)?
  accprevcond = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                    1 + nogoCondition + s(age_ACC, by = nogoCondition, k = 10) + (1 | sID_factor)),
                     family = binomial(), 
                     file = file.path(fit_dir, 'accprevcond')),
  
  #5. How does accuracy vary with previous conditioning (rewarded or punished
  #shapes from the Guessing task)? (null model for LOOIC comparison)
  accprevcondnull = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                        1 + nogoCondition + s(age_ACC, k = 10) + (1 | sID_factor)),
                         family = binomial(), 
                         file = file.path(fit_dir, 'accprevcondnull')),
  #6. How do the reaction times of responses in the Guessing task vary between
  #high and low magnitude blocks?
  rtguessing = list(formula = bf(guessResp.firstRt ~ valueCondition + s(age, by = valueCondition, k = 10) + (1 | sID_factor/runN_factor)),
                    family = brms::shifted_lognormal(), 
                    file = file.path(fit_dir, 'rtguessing')),
  #6.Null for age interaction re How do the reaction times of responses in the
  #Guessing task vary between high and low magnitude blocks?
  rtguessingnull = list(formula = bf(guessResp.firstRt ~ valueCondition + s(age, k = 10) + (1 | sID_factor/runN_factor)),
                    family = brms::shifted_lognormal(), 
                    file = file.path(fit_dir, 'rtguessingnull')),
  #Exploratory 1. Examine effect of fatigue on accuracy
  acctrial = list(formula = bf(correct ~ t2(age, time, k = 10) + (1 + time | sID_factor/runN_factor)),
                  family = brms::bernoulli(), 
                  file = file.path(fit_dir, 'acctrial')),
  accprepottrial = list(formula = bf(correct ~ t2(age, time, exact_prepotency, k = c(10, 10, 3)) + (1 + time | sID_factor/runN_factor)),
                        family = brms::bernoulli(), 
                        file = file.path(fit_dir, 'accprepottrial')))[[MODEL]]

file_refit <- ifelse(REFIT, 'always', 'on_change')

brm_model_options$file <- sprintf('%s%s_c%02d', brm_model_options$file, ifelse(TEST, '_test', ''), CHAINID)
brm_options <- c(brm_model_options, 
                 list(data = model_data,
                      file_refit = file_refit,
                      backend = 'cmdstanr',
                      iter = 2500, warmup = 1500, 
                      chain_ids = CHAINID, chains = 1, cores = 1, threads = ifelse(TEST, 4, 48),
                      control = list(adapt_delta = .9999999999999, max_treedepth = 20)))
print(brm_options)
print(get_prior(formula = brm_options$formula, data = brm_options$data))
print(brm_options$prior)
fit <- do.call(brm, brm_options)
summary(fit)
sessionInfo()
brms::add_criterion(fit, c('loo', 'bayes_R2'))


##
# plot(conditional_effects(fit), points = FALSE, ask = FALSE)
# variables(fit)
# plot(fit)
# library(priorsense)
# psens <- priorsense::powerscale_sensitivity(fit)
# print(psens$sensitivity, n = 118)
# pss <- priorsense::powerscale_sequence(fit)
# powerscale_plot_ecdf(pss, variables = variables(fit)[c(1:12)])
# fit_post <- as_draws_array(fit)
# np <- nuts_params(fit)
