.libPaths(c('/ncf/mclaughlin/users/jflournoy/R/x86_64-pc-linux-gnu-library/verse-4.2.1', .libPaths()))

library(data.table)
library(brms)
library(argparse)

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("--model", type = "character", default = 'rtage',
                    help="Which model to fit. Options are [rtage|rtagepropot|rtagepropotnull|accage|accprepot|accprevcond|accprevcondnull]")
parser$add_argument('--id', type = "integer", default = '1', help = 'Chain ID')
parser$add_argument("--test", action = "store_true", 
                    help="Run on small subset of data?")

args <- parser$parse_args()
#args <- parser$parse_args(c('--model', 'accprevcond', '--test', '--id', '1'))

MODEL <- args$model
TEST <- args$test
CHAINID <- args$id

source('process_carit.R')

carit$sID_factor <- as.factor(carit$sID)
carit$runN_factor <- as.factor(carit$runN)

# SETTING UP, testing with 1.25% of the data

if(TEST){
  set.seed(10) 
  traintest.sID <- unique(carit[, 'sID'])[, train := rbinom(length(sID), 1, .0125)]
  carit <- merge(carit, traintest.sID, by = "sID")[train==1]
}

#some timing tests
#First model, .0125
# All 4 chains finished successfully.
# Mean chain execution time: 117.2 seconds.
# Total execution time: 138.4 seconds.
#First model, .05
# All 4 chains finished successfully.
# Mean chain execution time: 308.6 seconds.
# Total execution time: 364.9 seconds.


#Different models require different ways of formatting the data.

#Just the "hit" trials (go trials with reaction times). We exclude trials that
#have RTs that are outside the possible response window.
carit.hits <- na.omit(carit[corrRespTrialType == 'Hit' & RT.shape > 0 & RT.shape <= .8, 
                            c('RT.shape', 'age', 'exact_prepotency', 'sID_factor', 'runN_factor')])

#Number of correct rejections out of all no-go trials for each participant-run.
carit.acc <- carit[trialType == 'nogo', 
                   list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                        'nogo.total' = .N,
                        'age_ACC' = unique(age)),
                   by = c('sID_factor',
                          'runN_factor')]

#Number of correct rejections out of all no-go trials for each participant-run,
#for each level of the no-go previously-conditioned trial type.
carit.acc.pc <- carit[trialType == 'nogo', 
                      list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                           'nogo.total' = .N,
                           'age_ACC' = unique(age)),
                      by = c('sID_factor',
                             'runN_factor',
                             'nogoCondition')]

#We combine the two data sets for the model correlating prepotency and accuracy.
carit.RT.acc <- data.table::rbindlist(list(RT = carit.hits, ACC = carit.acc), fill = TRUE, idcol = 'subset')
carit.RT.acc[, c('subsetRT', 'subsetACC') :=
               list(subset == 'RT',
                    subset == 'ACC')]

if (MODEL %in% c('rtage',
                 'rtagepropot',
                 'rtagepropotnull')) {
  model_data <- carit.hits 
} else if (MODEL %in% 'accage') {
  model_data <- carit.acc
} else if (MODEL %in% c('accprepot', 'accprepotlin')) {
  model_data <- carit.RT.acc
} else if (MODEL %in% c('accprevcond', 'accprevcondnull')){
  model_data <- carit.acc.pc
}
  
fit_dir <- 'fits'


#This sets up the specification for all models described in the preregistration.
brm_model_options <- list(
  
  #1. How does reaction time vary across age?
  rtage = list(formula = bf(RT.shape ~ s(age) + (1 | sID_factor/runN_factor)),
               family = brms::shifted_lognormal(), 
               file = file.path(fit_dir, 'rtage')),
  
  #2. How does sensitivity to prepotency vary across age? 
  rtagepropot = list(formula = bf(RT.shape ~ s(age, exact_prepotency) + 
                                    (1 + exact_prepotency | sID_factor/runN_factor)),
                     family = brms::shifted_lognormal(), 
                     file = file.path(fit_dir, 'rtagepropot')),
  
  #2. How does sensitivity to prepotency vary across age? (null model for LOOIC
  #comparison)
  rtagepropotnull = list(formula = bf(RT.shape ~ s(age) + s(exact_prepotency, k = 3) + 
                                        (1 + exact_prepotency | sID_factor/runN_factor)),
                         family = brms::shifted_lognormal(), 
                         file = file.path(fit_dir, 'rtagepropotnull')),
  
  #3. How does accuracy vary across age? 
  accage = list(formula = bf(corReject.total | trials(nogo.total) ~ 1 + s(age_ACC) + (1 | sID_factor)),
                family = binomial(), 
                file = file.path(fit_dir, 'accage')),
  
  #4. How does change in reaction time across sequential go-trials predict
  #accuracy?
  accprepot = list(formula = bf(RT.shape | subset(subsetRT) ~ s(age, exact_prepotency) + 
                                     (1 + exact_prepotency |ID1| sID_factor) + 
                                     (1 + exact_prepotency | sID_factor:runN_factor),
                                   family = brms::shifted_lognormal()) + 
                        bf(corReject.total | trials(nogo.total) + subset(subsetACC) ~ 1 + s(age_ACC) + (1 |ID1| sID_factor),
                           family = binomial()) + 
                        set_rescor(rescor = FALSE),
                      file = file.path(fit_dir, 'accprepotlin')),
  #5. How does accuracy vary with previous conditioning (rewarded or punished
  #shapes from the Guessing task)?
  accprevcond = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                    1 + nogoCondition + s(age_ACC, by = nogoCondition) + (1 | sID_factor)),
                     family = binomial(), 
                     file = file.path(fit_dir, 'accprevcond')),
  
  #5. How does accuracy vary with previous conditioning (rewarded or punished
  #shapes from the Guessing task)? (null model for LOOIC comparison)
  accprevcondnull = list(formula = bf(corReject.total | trials(nogo.total) ~ 
                                        1 + nogoCondition + s(age_ACC) + (1 | sID_factor)),
                         family = binomial(), 
                         file = file.path(fit_dir, 'accprevcondnull')))[[MODEL]]

brm_model_options$file <- sprintf('%s%s_c%02d', brm_model_options$file, ifelse(TEST, '_test', ''), CHAINID)
brm_options <- c(brm_model_options, 
                 list(data = model_data,
                      file_refit = 'on_change',
                      backend = 'cmdstanr',
                      iter = 2500, warmup = 1500, 
                      chain_ids = CHAINID, chains = 1, cores = 1, threads = 48,
                      control = list(adapt_delta = .999999999, max_treedepth = 15)))

fit <- do.call(brm, brm_options)
summary(fit)
sessionInfo()

