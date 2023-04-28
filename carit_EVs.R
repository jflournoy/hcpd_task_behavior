num_fac <- function(x, levels = NULL){
  if(is.null(levels)){
    as.numeric(factor(x))  
  } else { 
    as.numeric(factor(x, levels = levels))
  }
}
lag1_num_fac_diff <- function(x, levels = NULL){
  y <- abs(num_fac(x, levels = levels) - num_fac(shift(x, fill = x[[1]], type = 'lag'), levels = levels))
  return(y)
}

#read data
initial_options <- commandArgs(trailingOnly = FALSE)
file_arg_name <- "--file="
script_name <- sub(file_arg_name, '', initial_options[grep(file_arg_name, initial_options)])
script_basename <- dirname(script_name)
load_data <- 'process_carit.R'
if(length(script_basename) != 0){
  load_data <- file.path(script_basename, load_data)
}
source(load_data, echo = TRUE)

scanner_ids <- fread('plenzini_9_29_2022_13_46_20.csv',
                     select = c('MR ID', 'Scanner'),
                     col.names = c('sessionID', 'scanner'))
scanner_ids[, SCANNER_JOIN_ID := stringi::stri_replace(sessionID, regex = '_[A-z]{1,2}$', replacement = '')]
scanner_ids[, sessionID := NULL]
carit[, SCANNER_JOIN_ID := stringi::stri_replace(sessionID, regex = '_[A-z]$', replacement = '')]
carit <- merge(carit, scanner_ids, by = 'SCANNER_JOIN_ID', all.x = TRUE)

FD_covar <- fread('FD_covar.csv', select = c('Subject', 'Visit', 'DB_SeriesDesc', 'ABS_MEAN_RMS'))
FD_covar[, SCANNER_JOIN_ID := paste(Subject, Visit, sep = '_')]
carit[, DB_SeriesDesc := stringi::stri_extract_first_regex(filename, 'tfMRI_CARIT_(AP|PA)')]
carit <- merge(carit, FD_covar, by = c('SCANNER_JOIN_ID', 'DB_SeriesDesc'), all.x = TRUE, all.y = FALSE)


#remove longitudinal subjects:
carit <- carit[!grepl('_V[2-9]_', filename),]
duplicate_run_ids <- unique(carit[, .N, by = c('sID', 'runN')][N > 92, sID])
carit <- carit[!sID %in% duplicate_run_ids]
carit_pr_scan <- copy(carit) #[public_release, on = 'sID', nomatch = 0]
carit_pr_scan <- staged_dlmri[carit_pr_scan, on = 'sID'][has_carit == TRUE]
#Account for 10 dropped volumes at TR=0.8
carit_pr_scan[, evtime := round(shapeStartTime - 8, 6)]

setorder(carit_pr_scan, sID, runN, trialNum)
carit_pr_scan[, trial_type_diff := lag1_num_fac_diff(trialType, levels = c('go', 'nogo')), by = c('runN')]
carit_pr_scan[, chunkID := cumsum(trial_type_diff)]
carit_pr_scan[, N_of_trialType := 1:.N, by = c('runN', 'chunkID')]
carit_pr_scan[trialType == 'go', ppgo := paste0(N_of_trialType, 'go')]
carit_pr_scan[, EVpp := fifelse(is.na(prepotency), ppgo, as.character(prepotency))]
carit_pr_scan[, EVtrialType := fifelse(corrRespTrialType == 'Miss', 
                                       as.character(corrRespTrialType), 
                                       paste(corrRespTrialType, EVpp, sep = '_'))]

carit_pr_scan <- carit_pr_scan[! EVtrialType %in% c('_1go', '_2go', '_3go', '_4go')]
length(unique(carit_pr_scan$EVtrialType))
carit_pr_scan[, scanname := gsub('.*(tfMRI_CARIT_[AP][PA]).*', '\\1', filename)]

save_evs <- function(d){
  #this is a terrible progress checker.
  if(as.numeric(Sys.time())%%5 < .1) print(max(d[, N]))
  EV_dir <- unique(d[,EV_dir])
  if(length(EV_dir) > 1) stop('Too many dir names')
  if(!dir.exists(EV_dir)) dir.create(EV_dir, recursive = TRUE)
  evfn <- file.path(unique(d[,EV_dir]), paste0(unique(d[,EVtrialType]), '.txt'))
  d$dur <- NA_real_
  d$amp <- NA_real_
  d[!is.na(evtime), dur := 0.6]
  d[!is.na(evtime), amp := 1]
  if(dim(na.omit(d[, c('evtime', 'dur', 'amp')]))[[1]] == 0){
    if(file.exists(evfn)){
      file.remove(evfn)
    }
    message('Creating empty ', evfn)
    file.create(evfn)
  } else {
    readr::write_delim(d[, c('evtime', 'dur', 'amp')], evfn, delim = '\t', col_names = FALSE, na = '')
  }
  return(NULL)
}

all_sid_trialtype <- data.table(
  expand.grid(sessionID = unique(carit_pr_scan[, sessionID]), 
              EVtrialType = unique(carit_pr_scan[, EVtrialType]),
              scanname = unique(carit_pr_scan[, scanname])))

carit_pr_scan_expanded <- carit_pr_scan[all_sid_trialtype, on = c('sessionID', 'scanname', 'EVtrialType')]
carit_pr_scan_expanded[, EV_dir := file.path('/net/holynfs01/srv/export/ncf_hcp/share_root/data/HCD-tfMRI-MultiRunFix', sessionID, 'MNINonLinear/Results', scanname, 'EVs')]

carit_pr_scan_expanded[sessionID == 'HCD1328948_V1_MR' & scanname == 'tfMRI_CARIT_AP' & EVtrialType == 'falseAlarm_2go']
setorder(carit_pr_scan_expanded, sessionID, scanname, EVtrialType)
carit_pr_scan_expanded[, N := 1:.N]
carit_pr_scan_expanded_split <- split(carit_pr_scan_expanded, by = c('sessionID', 'scanname', 'EVtrialType'))
if(FALSE){
  fin <- lapply(carit_pr_scan_expanded_split, save_evs)
}

n_trialtype <- dcast(carit_pr_scan[, .N, by = c('sID', 'scanname', 'EVtrialType')],
                     ... ~ EVtrialType, value.var = 'N')
n_trialtype_l <- melt(n_trialtype, id.vars = c('sID', 'scanname'), na.rm = FALSE)
n_trialtype_l[, not_missing := !is.na(value)]
n_trialtype_w <- dcast(n_trialtype_l[, .(sID, scanname, variable, not_missing)], ... ~ variable, value.var = 'not_missing')

n_trialtype_sum <- n_trialtype_l[, .(any_missing = any(is.na(value)),
                                     N_missing = sum(is.na(value))), by = c('sID', 'scanname')]
n_trialtype_sum[any_missing == TRUE]

n_trialtype_full_sum <- n_trialtype_sum[, .(sID, scanname, N_missing)][n_trialtype_w, on = c('sID', 'scanname')]
setorder(n_trialtype_full_sum, N_missing)

readr::write_delim(n_trialtype_full_sum, 'run_summary.txt', delim = '\t')

n_trialtype_full_sum[, c('total_N_missing', 'runs') := list(sum(N_missing), .N), by = 'sID']

EVs <- fread('~/code/hcpd_tfMRI_CARIT/TaskAnalysis_input.txt', sep = ' ', col.names = c('HCPID', 'runs', 'task'))[, c('HCPID', 'runs')]
EVs[, sID := gsub('_V1_MR', '', HCPID)]
EVs[, L2 := 'tfMRI_CARIT']

set.seed(1991)
ten_perfect_sub_runs <- unique(n_trialtype_full_sum[total_N_missing == 0 & runs == 2, 'sID'])[sample(.N, min(10, .N))]
ten_imperfect_sub_runs <- unique(n_trialtype_full_sum[N_missing > 0 & Hit_1go, 'sID'])[sample(.N, min(10, .N))]

readr::write_delim(EVs[ten_perfect_sub_runs, on = 'sID', -'sID'],   delim = ' ', file = '~/code/hcpd_tfMRI_CARIT/ten_perfect.txt')
readr::write_delim(EVs[ten_imperfect_sub_runs, on = 'sID', -'sID'], delim = ' ', file = '~/code/hcpd_tfMRI_CARIT/ten_imperfect.txt')
