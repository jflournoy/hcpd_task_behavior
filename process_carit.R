## @knitr process_carit
library(data.table)
library(psycho)
data.table::setDTthreads(1)

modal <- function(x){
  t <- table(unlist(x))
  n <- dimnames(t)[[1]]
  return(n[which(t == max(t))])
}

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

read_carit_dir <- function(data_path, pattern = "*CARIT.*run[12]_wide.csv"){
  #List all of the CARIT task files in the data directory
  hcd_dirs <- dir(data_path, pattern = '^HCD.*', full.names = TRUE)
  carit_dirs <- unlist(lapply(hcd_dirs, dir, pattern = '^tfMRI_CARIT_(AP|PA)$', full.names = TRUE))
  fnames <- unlist(lapply(carit_dirs, 
                          dir,
                          pattern = pattern, 
                          recursive = TRUE, 
                          full.names = TRUE))
  names(fnames) <- 1:length(fnames)
  
  #Set the column names we want from each csv file
  col_select <- c('condFile', 'trialNum', 'stim',
                  'corrAns', 'prepotency', 'ISI',
                  'shapeStartTime', 'shapeEndTime', 
                  'fixStartTime','fixEndTime',
                  'nogoCondition','resp', 
                  'trialResp.firstKey', 'trialResp.firstRt',
                  'corrRespMsg', 'corrRespCode', 
                  'corrRespTrialType','isiPress.keys',
                  'isiPress.rt', 'hitCount', 'missCount',
                  'falseAlarmCount', 'corrRejectCount', 
                  'totalAcc', 'goAcc', 'nogoAcc', 
                  'goFiveBackAvg', 'nogoFiveBackAvg',
                  'totalFiveBackAvg')
  
  #create a temporary data frame of all the data from all the csv files
  adf <- data.table::rbindlist(
    lapply(fnames, function(f){
      #remove the first 4 rows of each data frame.
      data.table::fread(f, select = col_select)[-(1:4),]
    }),
    idcol = TRUE)
  
  #combine information about the files with the temporary data frame.
  d <- data.table(.id = names(fnames),
                  filename = fnames,
                  sessionID = gsub('.*(HCD[A-Za-z0-9]+_V[0-9]_[A|B|X]).*', '\\1', fnames),
                  sID = gsub('.*(HCD[A-Za-z0-9]+)_V[0-9]_[A|B|X].*', '\\1', fnames))[adf, on = '.id']
  return(d)
}

workspace_fname <- 'process_carit.rda'
if(!file.exists(workspace_fname)){
  
  data_path <- '/ncf/hcp/data/intradb_multiprocfix/'
  
  d <- read_carit_dir(data_path)
  #use the condition file name to figure out the run number
  setnames(d, 'condFile', 'runN')
  d[, runN := as.numeric(gsub('.*/scan([12])\\.csv', '\\1', runN))]
  d <- d[ 
    !filename %in% 
      c('/ncf/hcp/data/intradb_multiprocfix//HCD0353538_V1_MR/tfMRI_CARIT_PA/LINKED_DATA/PSYCHOPY/CARIT_HCD0353538_V1_A_run1_wide.csv')
  ]
  d[
    filename == '/ncf/hcp/data/intradb_multiprocfix//HCD2776374_V1_MR/tfMRI_CARIT_AP/LINKED_DATA/PSYCHOPY/CARIT_HCD2776374_V1_A_run1_wide.csv',
    runN := 2
  ]
  readr::write_csv(d, 'CARIT_allRaw.csv')
  demos <- fread('HCD_Inventory_2022-04-12.csv', 
                         select = c('subject', 'site', 'race', 'ethnic_group', 'M/F',
                                    'event_age', 'redcap_event'))[redcap_event == 'V1']
  setnames(demos, 'subject', 'sID')
  setnames(demos, 'event_age', 'age')
  setnames(demos, 'M/F', 'gender')
  
  staged <- data.table::fread('HCP_Staged_20230314.csv', 
                              select = 'Subject')
  
  # long <- data.table::fread('HCPD_LONGITUDINAL20200608.csv',
  #                           select = c('id', 'LONG_AGE'))
  staged_dlmri <- data.table(sessionID = dir('/ncf/hcp/data/intradb_multiprocfix/', pattern = "HCD.*"))
  
  staged_dlmri[, 'has_task_scan'] <-  unlist(lapply(staged_dlmri$sessionID, function(sess){
    length(dir(file.path('/ncf/hcp/data/intradb_multiprocfix/', 
                         sess, 
                         '/MultiRunIcaFix_proc/',
                         sess, 
                         '/MNINonLinear/Results/'), pattern = 'tfMRI.*')) > 0
  }))
  
  staged_dlmri[, 'has_carit'] <-  unlist(lapply(staged_dlmri$sessionID, function(sess){
    length(dir(file.path('/ncf/hcp/data/intradb_multiprocfix/', 
                         sess, 
                         '/MultiRunIcaFix_proc/',
                         sess, 
                         '/MNINonLinear/Results/'), pattern = 'tfMRI_CARIT.*')) > 0
  }))
  
  staged_dlmri[, 'has_guessing'] <-  unlist(lapply(staged_dlmri$sessionID, function(sess){
    length(dir(file.path('/ncf/hcp/data/intradb_multiprocfix/', 
                         sess, 
                         '/MultiRunIcaFix_proc/',
                         sess, 
                         '/MNINonLinear/Results/'), pattern = 'tfMRI_GUESSING.*')) > 0
  }))
  
  staged_dlmri[, c('sID', 'wave') := 
                 list(gsub('.*(HCD[A-Za-z0-9]+)_V[123]_MR.*', '\\1', sessionID),
                      gsub('.*HCD[A-Za-z0-9]+_V([123])_MR.*', '\\1', sessionID))]
  setnames(staged, 'Subject', 'sID')
  #setnames(long, 'id', 'sID')
  
  #Some columns should be factors
  factor_vars <- c('sessionID',
                   'stim',
                   'corrAns',
                   'prepotency',
                   'nogoCondition',
                   'resp',
                   'trialResp.firstKey',
                   'corrRespMsg',
                   'corrRespTrialType')
  d[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
  #d_intake[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
  
  #d_combined <- data.table::rbindlist(list(staged = d, intake = d_intake), idcol = "data_source")
  
  d_combined <- d
  
  #Integrate demographic variables
  carit <- demos[d_combined, on = 'sID']
  
  #rename corrAns to trialType
  setnames(carit, 'corrAns', 'trialType')
  
  #relabel the nogo condtion to be more accurate
  carit[, nogoCondition := factor(nogoCondition,levels=c("prevRewNogo","neutralNogo"),labels=c("prevRewNogo","prevLossNogo")) 
        ]
  #relabel the prepotency factor to be more clear 
  carit[, prepotency := factor(prepotency,levels=c("2","3","4"),labels=c("2go","3go","4go"))]
  #aggTrialN is trial number across both runs
  carit[, aggTrialN := trialNum + 92*(runN - 1)]
  carit[, RT.shape := trialResp.firstRt - shapeStartTime]
  
  carit[
    stringi::stri_cmp_eq(filename, '/ncf/hcp/data/intradb_multiprocfix//HCD0197045_V1_MR/tfMRI_CARIT_AP/LINKED_DATA/PSYCHOPY/CARIT_HCD0197045_V1_A_run1_wide.csv'),
    runN := 2]
  carit[, wave := stringi::stri_match(sessionID, regex = '.*_V([123]{1})_\\w$')[,2]]
  setorder(carit, sID, wave, runN, trialNum)
  carit[, corrRespTrialType_diff := lag1_num_fac_diff(corrRespTrialType, levels = c("", "corReject", "falseAlarm", "Hit", "Miss")), by = c('sID', 'runN')]
  carit[, chunkID := cumsum(corrRespTrialType_diff)]
  carit[, N_of_corrRespTrialType := 1:.N, by = c('sID', 'runN', 'chunkID')]
  carit[, prev_corrRespTrialType := shift(corrRespTrialType, type = 'lag'), by = c('sID', 'runN')]
  carit[corrRespTrialType == 'Hit', exact_prepotency := shift(N_of_corrRespTrialType, type = 'lag', fill = 0), by = c('sID', 'runN', 'chunkID', 'wave')]
  carit[, lag_N_of_corrRespTrialType := shift(N_of_corrRespTrialType, type = 'lag'), by = c('sID', 'runN')]
  carit[trialType == 'nogo' & prev_corrRespTrialType == 'Miss', exact_prepotency := 0]
  carit[trialType == 'nogo' & prev_corrRespTrialType == 'Hit', exact_prepotency := lag_N_of_corrRespTrialType]
  #View(carit[, c('sID', 'runN', 'corrRespTrialType', 'exact_prepotency', 'corrRespTrialType_diff', 'chunkID', 'N_of_corrRespTrialType', 'prev_corrRespTrialType', 'lag_N_of_corrRespTrialType')])
  #View(carit[trialType == 'nogo' & prev_corrRespTrialType == 'Hit', c('sID', 'runN', 'corrRespTrialType', 'corrRespTrialType_diff', 'chunkID', 'N_of_corrRespTrialType', 'prev_corrRespTrialType', 'hit_prepotency', 'lag_N_of_corrRespTrialType', 'prepotency')])
  #unique(carit[, 'exact_prepotency'])
  
  carit_by_run_SDT <- dcast(carit[, .N, by = c('sID', 'runN', 'corrRespTrialType') ], ... ~ corrRespTrialType, value.var = 'N', fill =  0) 
  carit_by_run_SDT[, c('dprime', 'beta', 'aprime', 'bppd', 'c') := psycho::dprime(Hit, falseAlarm, Miss, corReject)]
  
  readr::write_csv(carit,"CARIT_allSubs.csv")
  readr::write_csv(carit_by_run_SDT,"CARIT_allSubs_dprime.csv")
  save.image(file = workspace_fname)
} else {
  load(workspace_fname)
}
saveRDS(carit, 'carit_data.rds')
