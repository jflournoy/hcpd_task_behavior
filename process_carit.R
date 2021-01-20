## @knitr process_carit
library(data.table)
library(psycho)
data.table::setDTthreads(3)

modal <- function(x){
  t <- table(unlist(x))
  n <- dimnames(t)[[1]]
  return(n[which(t == max(t))])
}

read_carit_dir <- function(data_path, pattern = "*CARIT.*run[12]_wide.csv"){
  #List all of the CARIT task files in the data directory
  fnames <- dir(data_path, 
                pattern = pattern, 
                recursive = TRUE, 
                full.names = TRUE)
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
  
  data_path <- '/ncf/hcp/data/CCF_HCD_STG_PsychoPy_files/'
  intake_data_path <- '/ncf/hcp/data/intradb_intake/'
  
  d <- read_carit_dir(data_path)
  d_intake <- read_carit_dir(intake_data_path)
  #remove long
  
  readr::write_csv(d, 'CARIT_allRaw.csv')
  
  demos <- data.table::fread('HCPD_COMBINED20200608.csv',
                             select = c('id', 'age', 'gender', 'site', 'RACE', 'SES_PLVL', 'SES_RLVL', 'income'))
  staged <- data.table::fread('ccf_hcd_stg_2020-06-09.csv', 
                              select = 'Subject')
  public_release <- data.table::fread('HCD_V1_Release_Struct+fMRI_Merged_Adj.txt', header = FALSE, col.names = 'sID')
  
  long <- data.table::fread('HCPD_LONGITUDINAL20200608.csv',
                            select = c('id', 'LONG_AGE'))
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
  
  staged_dlmri[, sID := gsub('.*(HCD[A-Za-z0-9]+)_V1_MR.*', '\\1', sessionID)]
  setnames(demos, 'id', 'sID')
  setnames(staged, 'Subject', 'sID')
  setnames(long, 'id', 'sID')
  
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
  d_intake[, (factor_vars) := lapply(.SD, as.factor), .SDcols = factor_vars]
  
  d_combined <- data.table::rbindlist(list(staged = d, intake = d_intake), idcol = "data_source")
  
  #Integrate demographic variables
  carit <- demos[d_combined, on = 'sID']
  
  #use the condition file name to figure out the run number
  setnames(carit, 'condFile', 'runN')
  carit[, runN := as.numeric(gsub('.*/scan([12])\\.csv', '\\1', runN))]
  
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
  
  carit_by_run_SDT <- dcast(carit[, .N, by = c('sID', 'runN', 'corrRespTrialType') ], ... ~ corrRespTrialType, value.var = 'N', fill =  0) 
  carit_by_run_SDT[, c('dprime', 'beta', 'aprime', 'bppd', 'c') := psycho::dprime(Hit, falseAlarm, Miss, corReject)]
  
  readr::write_csv(carit,"CARIT_allSubs.csv")
  readr::write_csv(carit_by_run_SDT,"CARIT_allSubs_dprime.csv")
  save.image(file = workspace_fname)
} else {
  load(workspace_fname)
}
