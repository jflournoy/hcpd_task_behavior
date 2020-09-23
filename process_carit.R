#+ setup, include=FALSE
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)
#https://www.instagram.com/p/CCQvy_BAkWv/
apal <- paste0('#', c('005867', 'FFB335', 'F3F1EA', '2E383D', 'F34226'))
library(showtext)
#{.tabset}
font_add_google("Didact Gothic", "Didact Gothic")
showtext_auto()

jftheme <- theme_minimal() +  
  theme(text = element_text(family = 'Didact Gothic', size = 14),
        panel.background = element_rect(fill = apal[[3]], size = 0, color = apal[[2]]),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        strip.background = element_rect(fill = apal[[2]], size = 0),
        strip.text = element_text(color = '#222222'),
        axis.text =  element_text(color = apal[[1]]), axis.title = element_text(color = apal[[1]]))

#+stuff
library(data.table)
data.table::setDTthreads(2)
#data_path <- '/data/jflournoy/hcpd/CCF_HCD_STG_PsychoPy_files/'
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
staged_dlmri[, sID := gsub('.*(HCD[A-Za-z0-9]+)_V1_MR.*', '\\1', sessionID)]
setnames(demos, 'id', 'sID')
setnames(staged, 'Subject', 'sID')
setnames(long, 'id', 'sID')

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
#            sID Checked in scan list:
#  1: HCD0577358 No task fmri
#  2: HCD0696265 Has "guessing" but not "carit"
#  3: HCD1045128 No task data
#  4: HCD1249649
#  5: HCD1336341
#  6: HCD1529756
#  7: HCD1581455
#  8: HCD1638862
#  9: HCD1714852
# 10: HCD1876272
# 11: HCD1985378
# 12: HCD2082341
#

#List number of participants with dl'd MRI but not in staging list
staged_dlmri[!staged, on = 'sID'][, .N]
#List number of participants in staging list without dl'd MRI
staged[!staged_dlmri, on = 'sID'][, .N]
#Are they just the longitudinal folks?
long[staged[!staged_dlmri, on = 'sID'], on = 'sID'][, list(long = !is.na(LONG_AGE)), by = 'sID'][long != TRUE]
#Nope, there is a mix of longitudinal and non-long subs
#But at least some longitudinal folks look like they have multifx data

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

###
# Check design
##

#CARIT has two rounds of 92 trials each, for a total of 184 trials per subject:
#there should be 184 observations per subject. 
carit[, .N, keyby = 'sID'][N < 184][, .N]
carit[, .N, keyby = 'sID'][N < 184]
#Some participants don't have both runs

# Each round of the task has 68 go trials and 24 nogo trials: each subject
# should have 136 go and 48 nogo trials
dcast(carit[, .N, keyby = c('sID', 'trialType')],
      sID ~ trialType)[go != 136 | nogo != 48]
#Some participants don't have both runs

# Nogo trials should be equivalently distributed between prev reward and prev
# loss trials: each subject should have 24 prevRew, 24 prevLoss
dcast(carit[, .N, keyby = c('sID', 'nogoCondition')], 
      sID ~ nogoCondition)[prevRewNogo != 24 | prevLossNogo != 24]
#Some participants don't have both runs

# Go prepotency: 20 2-go, 16 3-go, 12 4-go:
dcast(carit[, .N, keyby = c('sID', 'prepotency')], 
      sID ~ prepotency)[`2go` != 20 | 
                          `3go` != 16 |
                          `4go` != 12]
#Some participants don't have both runs

#Check reaction times
library(ggplot2)
ggplot(carit[RT.shape > 0], aes(x = RT.shape)) + 
  stat_density(aes(group = sID), geom = "line", color = apal[[1]], alpha = 0.1, position = 'identity') + 
  coord_cartesian(xlim = c(0, 1)) + 
  jftheme

ggplot(carit[sID %in% carit[RT.shape < 0, sID]], aes(x = RT.shape)) + 
  stat_bin(aes(group = sID), geom = "line", color = apal[[1]], alpha = 0.5, position = 'dodge') + 
  jftheme

#A few participants have a single negative RT.
carit[RT.shape < 0, list(N = .N, rt = mean(RT.shape)), by = sID]
