library(data.table)
d <- fread('HCPD_LONGITUDINAL20200608.csv')

#See Warner, K. (1965). A general model for the study of developmental problems.
#Psychological Bulletin, 64(2), 92–107. https://doi.org/10.1037/h0022371

library(ggdag)
# C1: null to age
# C2: age to age+SR
# C2': age to age+Hormone
# C3: age+SR to age+SR+Hormone
# 
# (where SR is self-report pubertal). I think the causal model is something like 
# 
# Age -> Hormone -> SR -> Brain
# Age -> Hormone -> Brain
# Age -> Brain
# 
# So one single mediation path and one double mediation path, with the possibility of a direct effect from Age->Brain

tidy_ggdag <- dagify(
  y ~ age + hormones,
  hormones ~ age,
  sr ~ hormones,
  exposure = "sr",
  outcome = "y"
) %>% 
  tidy_dagitty()

ggdag_drelationship(tidy_ggdag)
ggdag_adjustment_set(tidy_ggdag)
             