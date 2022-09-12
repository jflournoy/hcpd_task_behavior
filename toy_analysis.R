library(mgcv)
library(tidyverse)
library(brms)

source('process_carit.R')

{set.seed(10)
  d_sid <- carit %>% 
    distinct(sID) %>%
    mutate(selected = rbinom(n(), 1, .025)) %>%
    filter(selected == 1)
  d <- left_join(d_sid, carit, by = 'sID') %>%
    filter(corrRespTrialType %in% c('Hit', 'corReject', 'falseAlarm'))}

d[, sID_f := factor(sID)]
d[, runN_f := factor(runN)]
d[, RE_Group := interaction(sID_f, runN_f)]

d <- d[!is.na(exact_prepotency)]
print(setorder(d[, c('exact_prepotency', 'corrRespTrialType')][, list(.N), by = c('exact_prepotency', 'corrRespTrialType')], 
               'corrRespTrialType', 'exact_prepotency'))

m_0 <- gam(RT.shape ~ 
             s(exact_prepotency, k = 4) + 
             s(age, k = 6) + 
             s(RE_Group, bs = 're'), 
           data = d[corrRespTrialType == 'Hit'])
summary(m_0)

par(mfrow = c(1,3))
plot(m_0)

m_1 <- gam(RT.shape ~ 
             s(exact_prepotency, age, k = 15) + 
             s(RE_Group, bs = 're'), 
           data = d[corrRespTrialType == 'Hit'])
summary(m_1)

par(mfrow = c(1,1))
vis.gam(m_1, view = c('exact_prepotency', 'age'), theta=180-35, phi=2, se = 0)
plot(m_1, select = 1, theta=180-35, phi=32, scheme = 1)

#no interaction
rbind(AIC(m_0, m_1), -sapply(AIC(m_0, m_1), diff))

m_0_b <- brm(RT.shape ~ 
               s(exact_prepotency, k = 3) + 
               s(age, k = 5) + 
               (1 | sID_f/runN_f), 
             data = d[corrRespTrialType == 'Hit'],
             chains = 4, cores = 4, iter = 4000, warmup = 1000, threads = 11,
             control = list(adapt_delta = .99999, max_treedepth = 15), 
             backend = 'cmdstanr',
             file = 'm_0_b', file_refit = 'on_change')
summary(m_0_b)

plot(conditional_smooths(m_0_b))

m_0alt_b <- brm(RT.shape ~ 
                  s(exact_prepotency, k = 4) + 
                  s(age, k = 6) +
                  (1 + age | sID_f/runN_f), 
                data = d[corrRespTrialType == 'Hit'],
                chains = 4, cores = 4, iter = 4000, warmup = 1000,
                control = list(adapt_delta = .99999, max_treedepth = 15), 
                backend = 'cmdstanr',
                file = 'm_0alt_b')
summary(m_0alt_b)

m_1_b <- brm(RT.shape ~ 
               s(exact_prepotency, age, k = 15) + 
               s(RE_Group, bs = 're'), 
             data = d[corrRespTrialType == 'Hit'],
             chains = 4, cores = 4, iter = 4000, warmup = 1000,
             control = list(adapt_delta = .99, max_treedepth = 15), 
             backend = 'cmdstanr',
             file = 'm_1_b')
summary(m_1_b)