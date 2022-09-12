.libPaths(c('/ncf/mclaughlin/users/jflournoy/R/x86_64-pc-linux-gnu-library/verse-4.2.1', .libPaths()))

library(data.table)
library(brms)
library(stringi)

model_fits <- data.table(file = dir('fits', pattern = '.*_c.*.rds', full.names = TRUE))
model_fits[, c('model', 'chain') := list(stri_match(file, regex = '.*/(.*)_(.*).rds')[,2],
                                                 stri_match(file, regex = '.*/(.*)_(.*).rds')[, 3])]


model_fit_objects <- lapply(split(model_fits, model_fits$model)$rtagepropot$file, readRDS)
fit <- do.call(combine_models, model_fit_objects)
summary(fit)
plot(conditional_smooths(fit))
