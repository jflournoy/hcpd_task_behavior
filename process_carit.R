## @knitr process_carit
library(data.table)
data.table::setDTthreads(4)

modal <- function(x){
  t <- table(unlist(x))
  n <- dimnames(t)[[1]]
  return(n[which(t == max(t))])
}

data_path <- '/ncf/hcp/data/CCF_HCD_STG_PsychoPy_files/'

#List all of the CARIT task files in the data directory
fnames <- dir(data_path, 
              pattern = "*CARIT.*wide.csv", 
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
                sessionID = gsub('.*(HCD[A-Za-z0-9]+_V1_[A|B|X]).*', '\\1', fnames),
                sID = gsub('.*(HCD[A-Za-z0-9]+)_V1_[A|B|X].*', '\\1', fnames))[adf, on = '.id']
readr::write_csv(d, 'CARIT_allRaw.csv')

demos <- data.table::fread('HCPD_COMBINED20200608.csv',
                           select = c('id', 'age', 'gender', 'site'))
staged <- data.table::fread('ccf_hcd_stg_2020-06-09.csv', 
                            select = 'Subject')
long <- data.table::fread('HCPD_LONGITUDINAL20200608.csv',
                          select = c('id', 'LONG_AGE'))
staged_dlmri <- data.table(sessionID = dir('/ncf/hcp/data/intradb_multiprocfix/', pattern = "HCD.*"))

staged_dlmri[, 'has_task_scan'] <-  unlist(lapply(staged_dlmri$sessionID, function(sess){
  file.exists(file.path('/ncf/hcp/data/intradb_multiprocfix/', 
                        sess, 
                        '/MultiRunIcaFix_proc/',
                        sess, 
                        '/MNINonLinear/Results/tfMRI_CARIT_PA/'))
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

#Integrate demographic variables
carit <- demos[d, on = 'sID']

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

readr::write_csv(carit,"CARIT_allSubs.csv")
