.libPaths(c('/ncf/mclaughlin/users/jflournoy/R/x86_64-pc-linux-gnu-library/verse-4.1.1', .libPaths()))

library(tidyverse)
library(data.table)
library(brms)
library(argparse)

# create parser object
parser <- ArgumentParser()

# specify our desired options 
# by default ArgumentParser will add an help option 
parser$add_argument("--model", type = "character", default = 'rtage',
                    help="Which model to fit. Options are [rtage|rtagepropot|rtagepropotnull|accage|accprepot|accprevcond]")
parser$add_argument('--id', type = "integer", default = '1', help = 'Chain ID')
parser$add_argument("--test", action = "store_true", 
                    help="Run on small subset of data?")

args <- parser$parse_args()
#args <- parser$parse_args(c('--model', 'rtage', '--test', '--id', '1'))

MODEL <- args$model
TEST <- args$test
CHAINID <- args$id

source('process_carit.R') #delete this and load your data here

carit$sID_factor <- as.factor(carit$sID)
carit$runN_factor <- as.factor(carit$runN)

# SETTING UP, testing with 1.25% of the data

if(TEST){
  set.seed(10) 
  traintest.sID <- carit %>% distinct(sID) %>% mutate(train = rbinom(length(sID), 1, .0125))  
  carit <- merge(carit, traintest.sID, by = "sID") %>% filter(train==1)
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


# Using just the 'Hit' trials
carit.hits <- na.omit(carit[corrRespTrialType == 'Hit' & RT.shape > 0 & RT.shape <= .8, 
                            c('RT.shape', 'age', 'exact_prepotency', 'sID_factor', 'runN_factor')])
carit.acc <- carit[trialType == 'nogo', 
                   list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                        'nogo.total' = .N,
                        'age_ACC' = unique(age)),
                   by = c('sID_factor',
                          'runN_factor')]
carit.acc.pc <- carit[trialType == 'nogo', 
                      list('corReject.total' = sum(corrRespTrialType == 'corReject'), 
                           'nogo.total' = .N,
                           'age_ACC' = unique(age)),
                      by = c('sID_factor',
                             'runN_factor',
                             'nogoCondition')]
carit.RT.acc <- data.table::rbindlist(list(RT = carit.hits, ACC = carit.acc), fill = TRUE, idcol = 'subset')

if (MODEL %in% c('rtage',
                 'rtagepropot',
                 'rtagepropotnull')) {
  model_data <- carit.hits 
} else if (MODEL %in% 'accage') {
  model_data <- carit.acc
} else if (MODEL %in% 'accprepot') {
  model_data <- carit.RT.acc
} else if (MODEL %in% 'accprevcond'){
  model_data <- carit.acc.pc
}
  
fit_dir <- 'fits'

brm_model_options <- list(rtage = list(formula = bf(RT.shape ~ s(age) + (1 | sID_factor/runN_factor)),
                                 family = brms::shifted_lognormal(), 
                                 file = file.path(fit_dir, 'rtage')),
                    
                    rtagepropot = list(formula = bf(RT.shape ~ s(age, exact_prepotency) + 
                                                     (1 + exact_prepotency | sID_factor/runN_factor)),
                                       family = brms::shifted_lognormal(), 
                                       file = file.path(fit_dir, 'rtagepropot')),
                    
                    rtagepropotnull = list(formula = bf(RT.shape ~ s(age) + s(exact_prepotency, k = 3) + 
                                                         (1 + exact_prepotency | sID_factor/runN_factor)),
                                       family = brms::shifted_lognormal(), 
                                       file = file.path(fit_dir, 'rtagepropotnull')),
                    
                    accage = list(formula = bf(corReject.total | trials(nogo.total) ~ 1 + s(age_ACC) + (1 | sID_factor)),
                                  family = binomial(), 
                                  file = file.path(fit_dir, 'accage')),
                    
                    accprepot = list(formula = bf(RT.shape | subset(subset == 'RT') ~ age*exact_prepotency + 
                                                   (1 + exact_prepotency |ID1| sID_factor) + 
                                                   (1 + exact_prepotency | sID_factor:runN_factor),
                                                 family = brms::shifted_lognormal()) + 
                                       bf(corReject.total | trials(nogo.total) + subset(subset == 'ACC') ~ 1 + age_ACC + (1 |ID1| sID_factor),
                                          family = binomial()) + 
                                       set_rescor(rescor = FALSE),
                                     file = file.path(fit_dir, 'accprepot')),
                    
                    accprevcond = list(formula = bf(corReject.total | trials(nogo.total) ~ 1 + s(age_ACC) + (1 | sID)),
                                       family = binomial(), 
                                       file = file.path(fit_dir, 'accprevcond')))[[MODEL]]

brm_model_options$file <- sprintf('%s_c%02d', brm_model_options$file, CHAINID)
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
