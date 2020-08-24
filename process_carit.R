library(data.table)
data.table::setDTthreads(6)
data_path <- '/data/jflournoy/hcpd/CCF_HCD_STG_PsychoPy_files/'

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
setnames(demos, 'id', 'sID')
setnames(staged, 'Subject', 'sID')


modal <- function(x){
  t <- table(unlist(x))
  n <- dimnames(t)[[1]]
  return(n[which(t == max(t))])
}

#List total number of participatns
length(unique(d$sID))
#List participants without the modal number of trials
d[, .N, by = 'sID'][, as.numeric(modal(N))]
d[, .N, by = 'sID'][N != as.numeric(modal(N))]
d[, .N, by = 'sID'][N != as.numeric(modal(N))][, .N]


#List number of participants with trial data but no demo info
d[!demos, on = 'sID'][, .N]
#List number of participants with demo info but no trial data
demos[!d, on = 'sID'][, .N]

#List number of participants with trial data but not staged
d[!staged, on = 'sID'][, .N]

############
# This should not happen, need to check it out.
####
#List number of participants staged but no trial data
staged[!d, on = 'sID'][, .N]
staged[!d, on = 'sID']


carit <- 
