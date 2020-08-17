########## HCPD CARIT ANALYSES ##########
## Constanza M. Vidal Bustamante


## Change the paths / working directories to your own. 


# Load R packages

if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}  
if (!require("plyr")) {install.packages("plyr"); require("plyr")}  
if (!require("dplyr")) {install.packages("dplyr"); require("dplyr")}  
if (!require("reshape")) {install.packages("reshape"); require("reshape")} #to melt datasets
if (!require("RColorBrewer")) {install.packages("RColorBrewer"); require("RColorBrewer")} 
if (!require("gridExtra")) {install.packages("gridExtra"); require("gridExtra")} 
if (!require("nlme")) {install.packages("nlme"); require("nlme")} 
if (!require("plotly")) {install.packages("plotly"); require("plotly")} 


# COMPILE DATA ------------------------------------------------------------

#String all subjects' data files together into one 
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/CARIT/CARIT_ALL_BOX_101418/")
# path = "/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/CARIT/CARIT_ALL_BOX_101418/"
path = "/Users/kastman/Dropbox/ANDL/ProjectAnalyses/HCPD/Behavioral"
setwd(path) #set working directory
out.file<-""
file.names <- dir(paste(path, "/data", sep=""), pattern ="CARIT.*wide.csv", full.names = TRUE)
for(i in 1:length(file.names)){
  file.name = file.names[i]
  file <- data.frame(read_csv(file.name))
  file <- file[-c(1:4),c('condFile','trialNum','stim','corrAns','prepotency','ISI',
                         'shapeStartTime','shapeEndTime','fixStartTime','fixEndTime',
                         'nogoCondition','resp','trialResp.firstKey','trialResp.firstRt','corrRespMsg','corrRespCode',
                         'corrRespTrialType','isiPress.keys','isiPress.rt','hitCount','missCount','falseAlarmCount',
                         'corrRejectCount','totalAcc','goAcc','nogoAcc','goFiveBackAvg','nogoFiveBackAvg','totalFiveBackAvg')] 
  m = regexpr('(HCD[A-Za-z0-9]+_V1_[A|B|X])', file.name)
  
  sessionID = regmatches(file.name, m)
  file$filename <- file.names[i]
  file$sessionID = sessionID
  out.file <- rbind(out.file, file)
}
write_csv(out.file[-1,],"CARIT_allRaw.csv") #removing first blank row that was somehow generated




# DATA WRANGLING ----------------------------------------------------------

# Read csv with all participants' raw CARIT data
CARIT <- data.frame(read_csv("CARIT_allRaw.csv"))
head(CARIT)
str(CARIT)

#turn a few variables into factor form
factorVars <- names(select(CARIT,sessionID,condFile,stim:ISI,nogoCondition:trialResp.firstKey,corrRespMsg,corrRespTrialType))

for (i in c(1:ncol(CARIT[factorVars]))) {
  CARIT[factorVars][[i]] <- as.factor(CARIT[factorVars][[i]])
}
str(CARIT)

# #Subj 0353538 repeated CARIT round 2, and RA added "_2" to that session. Will remove so that both runs are considered to be coming from same subject.
# #Subj 1959680 repeated CARIT round 1, both rounds got added a "_2"; will remove those too.
# CARIT$sessionID <- as.character(CARIT$sessionID) #need to change to character format to edit below
# CARIT$sessionID[CARIT$sessionID == "HCD05058945_V1_A"] <- "HCD0508945_V1_A" #UMinn subject
# CARIT$sessionID[CARIT$sessionID == "HCD0353538_V1_A_2"] <- "HCD0353538_V1_A"
# CARIT$sessionID[CARIT$sessionID == "HCD1959680_V1_A_2"] <- "HCD1959680_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "hcd2664161_V1_A"] <- "HCD2664161_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "1181843_V1_A"] <- "HCD1181843_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCA2925264_V1_A"] <- "HCD1454246_V1_A" #UMN subject
# # CARIT$sessionID[CARIT$sessionID == "HCA2925264_V1_A_x"] <- "HCD2925264_V1_A" #UMN subject; psychopy crashed, ran 2 run 2s for CARIT.
# CARIT$sessionID[CARIT$sessionID == "HCD1079044_V1"] <- "HCD1079044_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD031617_V1_A"] <- "HCD0031617_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD103318_V1_A"] <- "HCD2378463_V1_A"  # UMN subject
# 
# CARIT$sessionID[CARIT$sessionID == "HCA7259581_V1_A"] <- "HCD1612844_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HC2594772_V1_A"] <- "HCD2594772_V1_A" 
# # CARIT$sessionID[CARIT$sessionID == "HCD05415034_V1_A"] <- "HCD0541034_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD06433345_V1_A"] <- "HCD0643345_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD1138541_V1_A"] <- "HCD1139541_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD15065_V1_A"] <- "HCD1502635_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD1576060_V1_A"] <- "HCD1756060_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD2074551_V1_A"] <- "HCD2704551_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD2335433_V1_A"] <- "HCD2335344_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD2696780_V1_B"] <- "HCD2241234_V1_A"
# CARIT$sessionID[CARIT$sessionID == "HCD269678_V1_A"] <- "HCD2696780_V1_A" 
# 
# 
# CARIT$sessionID[CARIT$sessionID == "HCD2842965_V1_A"] <- "HCD2841965_V1_A" 
# CARIT$sessionID[CARIT$sessionID == "HCD0932554_V1_B"] <- "HCD0123925_V1_A"
# CARIT$sessionID[CARIT$sessionID == "1HCD2461450_V1_B"] <- "HCD2461450_V1_B"
# CARIT$sessionID[CARIT$sessionID == "HCDP1135_V1"] <- "HCD0813243_V1_A"
# CARIT$sessionID[CARIT$sessionID == "HCD2949682_V1_A"] <- "HCD2236140_V1_A"
# 
# #HCD1703039, UCLA -- FILES on Box but nothing on IntraDB. Was this participant excluded?
# #HCD1954367, UCLA -- FILES on Box but nothing on IntraDB. Was this participant excluded?
# 
# CARIT$sessionID <- factor(CARIT$sessionID) #change back to factor format


#add subj id (i.e. without "V1_A")
CARIT$sID <- substring(CARIT$sessionID,1,10)

#load and add dataset with age and gender:
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/CARIT/")
# ageGender <- data.frame(read_csv("age_gender_CARIT.csv")) # got from IntraDB
ageGender <- data.frame(read_csv("20190412_Demos_HCPD Staging Participants.csv")) # this file was obtained from Cindy Hernke through REDCap (has all subjects registered to date, with age and gender)


# ageGender <- plyr::rename(ageGender, c(gender="sex",site="scanner"))
ageGender <- dplyr::select(ageGender,sID,age,gender,site)
ageGender <- unique(ageGender)

ageGender %>% group_by(sID) %>% summarise(count= n()) %>% nrow(.) # should have data for 827 subjects, but only have 790. 
  # FIND THE MISSING FILES!

missing_files <- anti_join(ageGender,CARIT, by="sID") %>% unique(.) 
  # 19 have permanently missing data (data was not collected or never saved).
  # 2 have not uploaded the files to IntraDB, but the data was collected.

missing_intraDB <- anti_join(CARIT,ageGender, by="sID") %>% group_by(sID) %>% summarise(count=n())
  # 2 UCLA subjects have Psychopy data on Box, but no data on IntraDB

CARIT <- dplyr::left_join(CARIT, ageGender, by = "sID")

#using condFile to determine run number
CARIT <- plyr::rename(CARIT, c(condFile="runN"))
CARIT$runN <- factor(CARIT$runN,levels=c("conditions/scan1.csv","conditions/scan2.csv"),labels=c("1","2")) 

CARIT <- plyr::rename(CARIT, c(corrAns="trialType"))

# # reformat date column
# CARIT$date <- substring(CARIT$date,1,10)
# (CARIT$date <- as.Date(CARIT$date))

# re-labeling nogo conditions -- not neutral, either rewarded or loss
CARIT$nogoCondition <- factor(CARIT$nogoCondition,levels=c("prevRewNogo","neutralNogo"),labels=c("prevRewNogo","prevLossNogo")) 

# re-labeling prepotency
CARIT$prepotency <- factor(CARIT$prepotency,levels=c("2","3","4"),labels=c("2go","3go","4go")) 

## Make duplicate of trial number column, add +92 to all rows that have runN==2 (so we can count up to 184 trials total).
CARIT$aggTrialN <- CARIT$trialNum

CARIT <- within(CARIT,{ 
  aggTrialN <- NA
  aggTrialN[runN==1] <- trialNum[runN==1] 
  aggTrialN[runN==2] <- trialNum[runN==2] + 92 
})


## Compute RT (incorrectly recorded time stamp of response instead of RT)
CARIT$RT.shape <- CARIT$trialResp.firstRt - CARIT$shapeStartTime

# #reorder columns
# CARIT <- CARIT %>%
  # select(sID,sessionID,date,age,gender,nRuns:trialNum,aggTrialN,stim:trialResp.firstRt,RT.shape,everything())
# 
# CARIT <- CARIT %>%
#   select(sID,sessionID,date,nRuns:trialNum,stim:trialResp.firstRt,RT.shape,everything())

# save this clean dataset to new file
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/CARIT/CARIT_ALL_BOX_101418/")
write_csv(CARIT,"CARIT_allSubs.csv")



# LOAD CLEAN DATAFILE -----------------------------------------------------------

#Load clean dataset with all subs CARIT data:
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/CARIT/CARIT_ALL_BOX_101418/")
CARIT <- data.frame(read_csv("CARIT_allSubs.csv"))

attach(CARIT)

###WHITE THEME
white_theme <- function() {
  
  # Begin construction of chart
  theme_bw(base_size=10) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill='white', color='white')) +
    theme(plot.background=element_rect(fill='white', color='white')) +
    theme(panel.border=element_rect(color='white')) +
    #theme(panel.border=element_rect(color='white'), axis.line = element_line()) +
    
    # Format the grid
    theme(panel.grid.major=element_blank()) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill='white')) +
    theme(legend.text = element_text(size=12,color='black', family = 'Helvetica')) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color='black', size=16, vjust=1.25)) +
    theme(axis.text.x=element_text(size=12,color='black',family = 'Helvetica')) +
    theme(axis.text.y=element_text(size=12,color='black',family = 'Helvetica')) +
    theme(axis.title.x=element_text(size=14,color='black', vjust=0,family = 'Helvetica')) +
    theme(axis.title.y=element_text(size=14,color='black', vjust=1.25,family = 'Helvetica')) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

##GGPLOT FTE THEME
fte_theme <- function() {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart
  theme_bw(base_size=10) +

    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +

    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +

    # Format the legend, but hide by default
    theme(legend.position="none") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=10,color=color.axis.title)) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=16, vjust=1.25)) +
    theme(plot.subtitle=element_text(color=color.title, size=12, vjust=1.25)) +
    theme(axis.text.x=element_text(size=12,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=14,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=14,color=color.axis.title, vjust=1.25)) +

    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}



mystats <- function(x, na.omit=FALSE){
  if (na.omit)
    x <- x[!is.na(x)]
  m <- mean(x)
  n <- length(x)
  s <- sd(x)
  Min <- min(x)
  Max <- max(x)
  skew <- sum((x-m)^3/s^3)/n
  kurt <- sum((x-m)^4/s^4)/n - 3
  return(c(n=n, mean=m, min=Min, max=Max, stdev=s, skew=skew, kurtosis=kurt))
}

se <- function(x) sqrt(var(x)/length(x))


#### DESCRIPTIVES / EDA&VIZ -------------------------------------------------------


# Task design sanity checks ------------------------------------------------------

# Check task design characteristics are met for all participants:

# CARIT has two rounds of 92 trials each, for a total of 184 trials per subject:
table(sID) #there should be 184 observations per subject. Subjects HCD0353538 and HCD0989280 have 92 b/c only one valid run.
  #HCD2971776 only one (and incomplete) run of CARIT - pressed squeeze ball to be taken out. More details on IntraDB.

trialsSubj <- CARIT %>%
  dplyr::group_by(sID) %>%
  dplyr::summarise(
    count = n()
  ) %>%
  arrange(count)

# Check missing data
check <- anti_join(ageGender, trialsSubj, by = "sID") #HCD2867478 from UCLA didn't complete CARIT

# Each round of the task has 68 go trials and 24 nogo trials:
table(sID,trialType) # each subject should have 136 go and 48 nogo trials 

# Nogo trials should be equivalently distributed between prev reward and prev loss trials:
table(sID,nogoCondition) # each subject should have 24 prevRew, 24 prevLoss 

# Go prepotency: 20 2-go, 16 3-go, 12 4-go:
table(sID,prepotency)



# Sample details ----------------------------------------------------------

# How many subjects?
subs <- CARIT %>%
  group_by(sID) %>%
  summarise(
    count = n(),
    age = age[1],
    gender = gender[1],
    # date = date[1],  # No date in cleaned files
    site = site[1]
  ) 
nrow(subs) # 9/5/17: 205 CARIT subjects ; 10/14/18: 808 CARIT subjects

# compute summary stats on age distribution of the sample
summary(ageGender$age, na.rm=T)
table(ageGender$gender) # 417 F, 405 M, 5 U

# number of subjects from each participating site
subs %>%
  group_by(site) %>%
  summarise( count = n())

# Plot age + gender distribution of the 807 subjects I have data for as of 10/14/18
  # removes 3 subjects ages 5-7.
CARIT %>% filter(age>7) %>% group_by(sID) %>% summarise(age = age[1], gender = gender[1]) %>%
ggplot(., aes(x = floor(age))) + 
  geom_histogram(binwidth=1,color='white',aes(fill=gender)) +
  labs(title="Age and gender distribution of the sample",x="Age", y="Count") +
  scale_x_continuous(breaks=seq(8,21,1)) +
  scale_y_continuous(breaks=seq(0,120,10)) +
  scale_fill_manual(values=c('tomato3','snow4')) +
  white_theme() +
  # fte_theme() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(), legend.key=element_rect(fill=NA)) 


# Reaction time -----------------------------------------------------------

RTs <- CARIT %>%
  filter(RT.shape>0) 

mystats(RTs$RT.shape)

# Plot distribution of all RTs, across trials, across subjects:
ggplot(CARIT) +
  geom_histogram(aes(RT.shape), fill = 'seagreen3', colour = 'white') +
  scale_x_continuous(limits=c(0.00,0.80)) +
  labs(title='Distribution of reaction times', x='Reaction time (in seconds)', y='Count') +
  white_theme() # Shape stims shown for 600ms, but participants' responses counted up until the 800ms mark.

# Plot distribution of all RTs by age:
ggplot(CARIT) +
  geom_point(aes(age,RT.shape)) +
  fte_theme() #5 RTs under 0.2s

# Compute number RTs under 0.2 by subject
CARIT %>%
  select(sID,RT.shape,trialType) %>%
  filter(RT.shape < 0.2) %>%
  group_by(sID,trialType) %>%
  summarise(
    count = n() #HCD2001517 (12yo) had 6 RTs under 0.2s
  )

# Plot RTs by go/nogo trials:
CARIT %>%
  ggplot(.) +
  geom_histogram(aes(RT.shape, fill = factor(trialType)), colour = 'white') +
  labs(title='Distribution of reaction times by trial type (not averaged by subject)', x='Reaction time (in seconds)', y='Count') +
  scale_x_continuous(limits=c(0.00,0.90)) +
  facet_grid(trialType ~ .) +
  scale_fill_manual(values=c('deepskyblue','tan1')) +
  white_theme() # Shape stims shown for 600ms, but participants' responses counted up until the 800ms mark.
# Go RT ('hit') distribution to the right side of the response time window, not always the immediate response 
  #(probably especially after a couple of previous gos -- expecting a nogo).
# Most false alarms happen early in the response time window, impulsive reflex.

# How many over 600ms responses for nogo trials (ie false alarms that were not immediate):
longIsiRTnogo <- CARIT %>% #inspect this subset of the main dataframe -- any patterns?
  filter(RT.shape > 0.6, trialType=='nogo') #20 total, distributed among many participants (not one participant having this problem systematically)
                                            # most common for prev loss shape and followed by 2-go prepotency.

longIsiRTall <- CARIT %>% #inspect this subset of the main dataframe -- any patterns?
  filter(RT.shape > 0.6)  
# Confused about the isiPress columns -- I thought this would get populated whenever participant pressed after the 600ms mark (ie during ISI cross), 
  # but this is not the case for most rows.


#Check for presses after the response window (i.e. after 800ms):
CARIT$isiPress.rt <- as.character(CARIT$isiPress.rt)
CARIT$isiPress.rt <- substr(CARIT$isiPress.rt,2,nchar(CARIT$isiPress.rt)-1)
CARIT$isiPress.rt <- as.numeric(as.character(CARIT$isiPress.rt))

# How many subjects had at least one late press?
CARIT %>%
  filter(!is.na(isiPress.rt) & corrRespTrialType %in% c('Miss','corReject')) %>% # about 1,000 presses past response window 
  group_by(sID) %>%
  summarise(
    count = n()
  ) #150

# How many late presses total?

#How many late presses per subject?
latePressSubj <- CARIT %>%
  filter(!is.na(isiPress.rt)) %>%
  group_by(sID) %>%
  summarise(
    age = age[1],
    countSubj = n(),
    propSubj = n()/136
  ) 

se <- function(x) sqrt(var(x)/length(x))

latePressSubj %>%
  group_by(floor(age)) %>%
  summarise(
    count = n(),
    prop = mean(propSubj),
    propSE = se(propSubj)
  ) %>%
  arrange(desc(prop)) %>%
  ggplot(.) +
  geom_bar(aes(`floor(age)`,prop), fill = 'seagreen3', stat='identity') + 
  geom_errorbar(aes(`floor(age)`,prop,ymin=prop-propSE, ymax=prop+propSE),width=.2,color='gray40') +
  scale_x_continuous(breaks=seq(8,21,1)) +
  labs(y= "proportion of late 'go' presses") +
  labs(x='age') +
  white_theme()


lateRTsTrialType <- CARIT %>%
  filter(!is.na(isiPress.rt)) %>%
  group_by(corrRespTrialType) %>%
  summarise(
    count = n()
  )

ggplot(lateRTsTrialType) +
  geom_bar(aes(corrRespTrialType, count, fill = corrRespTrialType), stat = 'identity') +
  labs(x='', title = 'Number of late presses by coded trial type') +
  white_theme()

# isiPress.rt is the time they pressed since the start of the fixation cross. However, a response will still be counted towards accuracy 200ms into the ISI.
  #Therefore I will compute a new variable to reflect how late participants pressed by discounting the 200ms from the isiPress.rt.
CARIT$lateTime <- CARIT$isiPress.rt - 0.2
  
lateRTs <- CARIT %>%
  filter(!is.na(isiPress.rt) & corrRespTrialType %in% c('Miss','corReject') ) %>% # about 1,000 presses past response window 
  group_by(sID, corrRespTrialType) %>%
  summarise(
    age = age[1],
    gender = gender[1],
    site = site[1],
    count = n(),
    meanLatePress = mean(lateTime)
  ) %>%
  arrange(age)


#number of late presses by age
ggplot(lateRTs) +
  geom_point(aes(age,count, color=corrRespTrialType), alpha = 0.3) +
  scale_x_continuous(breaks=seq(8,21,1)) +
  facet_grid(. ~ corrRespTrialType) +
  fte_theme()

#average late press time by age
ggplot(lateRTs) +
  geom_point(aes(age,meanLatePress, color=corrRespTrialType), alpha = 0.4) +
  scale_x_continuous(breaks=seq(8,21,1)) +
  facet_grid(. ~ corrRespTrialType) +
  fte_theme()


# 8yos only:

#How many  8yos pressed late?
CARIT %>%
  filter(!is.na(isiPress.rt) & corrRespTrialType %in% c('Miss','corReject') & age < 9) %>% # about 1,000 presses past response window 
  group_by(sID) %>%
  summarise(
    count = n()
  ) #18

latePress8 <- CARIT %>%
  filter(!is.na(isiPress.rt) & corrRespTrialType %in% c('Miss','corReject') & age < 9) %>% # about 1,000 presses past response window 
  group_by(sID, corrRespTrialType) %>%
  summarise(
    age = age[1],
    gender = gender[1],
    site = site[1],
    count = n(),
    meanLatePress = mean(lateTime)
  ) %>%
  arrange(age) # 18

#number of late presses among 8 yos
latePress8 %>%
  filter(corrRespTrialType == 'Miss') %>%
ggplot(.) +
  geom_point(aes(age,count/136), alpha = 0.5) +
  labs(y = "proportion of late presses\n(out of 136 'go' trials)") +
  fte_theme()

#average late press time among 8 yos
latePress8 %>%
  filter(corrRespTrialType == 'Miss') %>%
  ggplot(.) +
  geom_point(aes(age,meanLatePress,size = count), color = 'navyblue',alpha = 0.5) +
  labs(y='RT (s) after response window') +
  fte_theme()


#Average RT by prepotency
CARIT %>% 
  filter(!is.na(RT.shape)) %>%
  group_by(prepotency) %>%
  summarise(
    avgRT = mean(RT.shape)
  )
#very small difference in RT by prepotency -- might be influenced by the fact that I averaged across all subjects and trial types.
CARIT %>%
  filter(!is.na(prepotency),!is.na(RT.shape)) %>%
  ggplot(.) +
  geom_histogram(aes(RT.shape, fill = factor(prepotency)), colour = 'white') +
  labs(title='False alarm RTs by prepotency (all presses, not averaged by subject)', x='Reaction time (in seconds)', y='Count') +
  facet_grid(prepotency ~ .) +
  fte_theme() 
#Most false alarms following 2-go prepotency, but seemingly no difference in the RT distribution by prepotency.


#Plot average RT by subject (across trial types, i.e. hits and false alarms)
subRT <- CARIT %>%
  filter(!is.na(RT.shape)) %>%  #keeping go trials with a response (i.e. 'hits')
  group_by(sID) %>%
  summarise(
    avgRT = mean(RT.shape), #averaging RTs washes out very short/long RTs
    age = age[1],
    site = site[1]
  ) 

mystats(subRT$avgRT)
  
ggplot(subRT) +
  geom_histogram(aes(x=avgRT),binwidth=0.015,fill='tan1',color='white') +
  labs(title = 'Reaction time across trials, averaged by subject', x = "Average RT (in seconds)", y='Count') +
  fte_theme() 


#Compute average RT for go trials (hits) by subject; add to main CARIT df:
goRT <- CARIT %>%
  filter(trialType=='go',!is.na(RT.shape)) %>%  #keeping go trials with a response (i.e. 'hits')
  group_by(sID) %>%
  summarise(
    avgHitRT = mean(RT.shape) #averaging RTs washes out very short/long RTs
  )
summary(goRT)

nogoRT <- CARIT %>%
  filter(trialType=='nogo',!is.na(RT.shape)) %>%  #keeping go trials with a response (i.e. 'hits')
  group_by(sID) %>%
  summarise(
    avgFalseAlarmRT = mean(RT.shape) #averaging RTs washes out very short/long RTs
  )
summary(nogoRT)


CARIT <- CARIT %>%
  left_join(goRT, by = "sID")
CARIT <- CARIT %>%
  left_join(nogoRT, by = "sID")

detach(CARIT)
attach(CARIT)

# Average RT by subject, by trial type, by age:
CARIT %>%
  filter(corrRespTrialType == 'Hit') %>%
  group_by(sID) %>%
  summarise(
    avgHitRT = avgHitRT[1],
    avgFalseAlarmRT = avgFalseAlarmRT[1],
    age = age[1]
  ) %>%
  ggplot(.) +
  geom_point(aes(age,avgHitRT, color='Hit'), size = 1, alpha = 0.7) +
  geom_smooth(method= 'lm',aes(age,avgHitRT, color='Hit'), se=F) +
  geom_point(aes(age,avgFalseAlarmRT, color='False alarm'), size = 1, alpha = 0.7) +
  geom_smooth(method='lm',aes(age,avgFalseAlarmRT, color='False alarm'), se=F) +
  scale_x_continuous(breaks = seq(8,22,2)) +
  coord_cartesian(xlim = c(8,22)) +
  labs(title='', x='Age', y='Average RT (s)') +
  scale_color_manual(name='Trial type', values=c('firebrick','dodgerblue4')) + 
  white_theme() +
  #fte_theme() + # no discernible developmental pattern for RTs
  theme(legend.position = "bottom",legend.key=element_rect(fill=NA))




# Accuracy ----------------------------------------------------------------

CARIT %>% 
  summarise(acc = mean(corrRespCode, na.rm=T))  # average accuracy across sample, across conditions: 85.3%

#Compute overall accuracy by site:
accSite <- CARIT %>%
  group_by(site) %>%
  summarise(
    acc=mean(corrRespCode, na.rm=T)
  ) %>%
  ggplot(.) + 
  geom_bar(aes(reorder(site,acc),acc, fill=site), stat = 'identity') +
  labs(x='',y='Average accuracy') +
  white_theme()


# Plot accuracy by go/nogo:
CARIT %>% 
  group_by(trialType) %>%
  summarise(acc = mean(corrRespCode, na.rm=T)) %>% # Go: 92.8% ; NoGo: 64.3%
  ggplot(aes(trialType,acc)) +
  geom_bar(stat ='identity',aes(fill=factor(trialType))) +
  labs(title='Average accuracy by trial type', x = 'Trial type', y='Average accuracy') +
  #scale_color_manual(name='Trial type', values=c('darkorange','seagreen3')) + 
  scale_fill_manual(values = c('seagreen3', 'tan1')) +
  white_theme()

# t-test comparing accuracy by trial type (go vs nogo)
goNogoTtest <- CARIT %>%
  group_by(sID,trialType) %>%
  summarise(acc = mean(corrRespCode, na.rm=T))

t.test(acc ~ trialType,goNogoTtest,paired=T)

# Compute go/nogo accuracy by subject:
accType <- CARIT %>%  # a lot of subjects were not pressing at all? (perfect nogo, 0 accuracy for go)
  group_by(sID,runN,trialType) %>%
  summarise(
    acc=mean(corrRespCode,na.rm=T),
    age = age[1],
    site = site[1]
  ) %>%
  spread(., key = trialType, value = acc) %>%
  arrange(go)

# nogo accuracy by subject by run by reward history
nogoAcc <- CARIT %>%
  filter(trialType =='nogo') %>%
  group_by(sID,runN,nogoCondition) %>%
  summarise(
    acc=mean(corrRespCode, na.rm=T),
    age = age[1],
    site = site[1]
  ) %>%
  arrange(acc)

countSubj <- nogoAcc %>%
  group_by(sID) %>%
  summarise(
    count = n()
  )
# Write to file:
# setwd("/Users/constanzavidal/Desktop/Projects/HCPD_tfMRI/CARIT/")
write.csv(accType, "CARIT_accBySubj.csv", row.names = F)
write.csv(nogoAcc, "CARIT_nogoAccBySubjRunRew.csv", row.names = F)


# Plot go/nogo accuracy by age
ggplot(accType) +
  geom_point(aes(age,go,color='go'), size = 1, alpha = 0.4) +
  geom_smooth(method='lm',aes(age,go,color='go'),se=F) +
  geom_point(aes(age,nogo,color='nogo'), size = 1, alpha = 0.4) +
  geom_smooth(method='lm',aes(age,nogo,color='nogo'),se=F) +
  scale_x_continuous(breaks = seq(8,22,2)) + 
  labs(title='', x='Age', y='Accuracy') +
  scale_color_manual(name="Trial type", values=c('dodgerblue4','firebrick')) +
  white_theme() +
  theme(legend.position = "bottom",legend.key=element_rect(fill=NA))

write.csv(accType, 'CARIT_accuracy.csv', row.names = F)

# go accuracy histogram by run
ggplot(accType) +
  geom_bar(aes(go), binwidth = 0.05, fill = 'seagreen', color = 'white') +
  labs(title='Go accuracy by run') +
  white_theme()

# nogo accuracy histogram by run
ggplot(accType) +
  geom_bar(aes(nogo), binwidth = 0.05, fill = 'navyblue', color ='white') +
  labs(title='Nogo accuracy by run') +
  white_theme()


# Plot go accuracy across age, and disaggregate by site:
ggplot(accType) +
  geom_point(aes(age,go, color=site)) +
  geom_smooth(aes(age,go)) +
  labs(x='Age', y= 'Go accuracy') +
  facet_grid(. ~ site) +
  white_theme() +
  theme(legend.position = 'bottom')

# Plot no-go accuracy across age, and disaggregate by site:
ggplot(accType) +
  geom_point(aes(age,nogo, color=site)) +
  geom_smooth(aes(age,nogo)) +
  labs(x='Age', y= 'No-go accuracy') +
  facet_grid(. ~ site) +
  white_theme() +
  theme(legend.position = 'bottom')

# add new variables to bigger CARIT dataset
CARIT <- CARIT %>%
  left_join(accType, by = "sID")

# Nogo accuracy: quick look at false alarms:
falseAlarms <- CARIT %>%
  filter(corrRespTrialType=='falseAlarm') 

table(falseAlarms$sID) #distributed among many participants (not just a few participants having this problem systematically)
table(falseAlarms$prepotency) #most common following 2-go prepotency -- consider there are 20 2-go vs 12 4-go total.
table(falseAlarms$nogoCondition) #more common to square stim associated with loss

#count of corrRespTrialType per subject (i.e. # misses, hits, false alarms, correct rejects)
checkPresses<- CARIT %>%
  group_by(sID, corrRespTrialType) %>%
  summarise(
    count = n()
  ) %>%
  spread(.,corrRespTrialType,count)

checkPresses <- replace_na(checkPresses, list(corReject= 0,falseAlarm=0,Hit=0,Miss=0))

FAhistDev <- ggplot(checkPresses) +
  geom_histogram(aes(falseAlarm), binwidth = 1, fill='purple', color= 'white') +
  labs(title='HCPD: Number of FALSE ALARMS per subject', x='# of false alarms (total # nogo trials = 48)') +
  scale_x_continuous(breaks=seq(0,40,5)) +
  white_theme()

MissHistDev <- ggplot(checkPresses) +
  geom_histogram(aes(Miss), binwidth = 1, fill='seagreen3', color= 'white') +
  labs(title='HCPD: Number of MISSES per subject', x='# of misses (total # go trials = 136)') +
  scale_x_continuous(breaks=seq(0,100,5)) +
  white_theme()

grid.arrange(FAhistDev, MissHistDev, ncol=1) 

#Nogo accuracy by prepotency:
CARIT %>% 
  filter(!is.na(prepotency)) %>%
  group_by(prepotency) %>%
  summarise(acc = mean(corrRespCode, na.rm=T)) %>% #Averaging across subjects, nogo accuracy gets increasingly better with higher prepotency.
ggplot(aes(prepotency,acc)) +
  geom_bar(stat ='identity',aes(fill=factor(prepotency))) +
  coord_cartesian(ylim=c(0.4,0.8)) +
  labs(title='Average nogo accuracy by go prepotency', x = 'Prepotency', y='Average accuracy') +
  white_theme()


# Accuracy by no-go reward history
CARIT %>% 
  filter(trialType=='nogo' & !is.na(nogoCondition)) %>%
  group_by(nogoCondition) %>%
  summarise(acc = mean(corrRespCode,na.rm=T)) %>% #Averaging subjects together, much worse accuracy for no-go associated with loss relative to that associated with reward
  ggplot(aes(nogoCondition,acc)) +
  geom_bar(stat ='identity',aes(fill=factor(nogoCondition))) +
  coord_cartesian(ylim=c(0.4,0.8)) +
    labs(title='Average nogo accuracy by reward history', x = '', y='Average accuracy') +
  scale_x_discrete(labels=c('previous LOSS', 'previous REWARD')) +
  white_theme()
#   nogoCondition       acc
# 1 prevLossNogo  0.637
# 2 prevRewNogo   0.650

# NO DIFFERENCE BY REWARD HISTORY!
#t-test comparing accuracy by trial type (PREV REW vs PREV LOSS NOGO)
nogoTtest <- CARIT %>%
  filter(trialType=='nogo') %>%
  group_by(sID,nogoCondition) %>%
  summarise(acc = mean(corrRespCode, NA.RM=T))

t.test(acc ~ nogoCondition,nogoTtest,paired=T)

#nogo accuracy across age, by reward history
nogoAge <- CARIT %>% 
  filter(trialType=='nogo') %>%
  group_by(sID,nogoCondition) %>%
  summarise(acc = mean(corrRespCode)) %>%
  left_join(.,ageGender, by='sID')

ggplot(nogoAge) +
  geom_point(aes(age,acc,color=factor(nogoCondition))) +
  geom_line(aes(age,acc,group = factor(sID))) + 
  geom_smooth(aes(age,acc,color=factor(nogoCondition)), se=F) +
  #coord_cartesian(ylim=c(0.25,1)) +
  scale_x_continuous(breaks = seq(8,22,2), limits = c(8,22)) +
  #scale_y_continuous(breaks = seq(0.25,1,0.25),limits=c(0.25,1)) +
  geom_hline(yintercept = 0.5,colour='black', linetype =2, size = 0.5) +
  labs(title='Average nogo accuracy across age by reward history, by subject', x = 'Age', y='Average accuracy') +
  scale_color_manual(name='Reward history',values=c('red3', 'springgreen4'),
                     labels = c('Prev Loss','Prev Reward')) +
  fte_theme() +
  theme(legend.position = 'bottom',legend.key=element_rect(fill=NA))

  
#Compute reward history bias by subject:
rewBiasdf <- CARIT %>%
  filter(trialType=='nogo') %>%
  group_by(sID,nogoCondition) %>%
  summarise(
    acc = mean(corrRespCode, NA.RM=T)
  ) %>%
  spread(., key = nogoCondition, value = acc) %>%
  mutate(rewBias = prevRewNogo - prevLossNogo) #reward bias computed as the difference score between accuracy to prev reward minus accuracy to prev loss

summary(rewBiasdf$rewBias) #positive scores indicate better performance to prev reward, negative scores better performance to prev loss

ggplot(rewBiasdf) +
  geom_histogram(aes(x=rewBias),binwidth=0.05,fill='tan1',color='white') +
  geom_vline(xintercept = 0, colour='red', size=1) +
  labs(title = 'Reward bias: Accuracy Prev Reward - Accuracy Prev Loss, By Subject', x='Reward bias') +
  scale_x_continuous(limits=c(-0.2,0.6),breaks = c(-0.2,0,0.2,0.4,0.6), labels = c('Worse to REWARD','0','0.2','0.4','Worse to LOSS')) +
  coord_flip() +
  fte_theme()

#Is there significant difference in no go accuracy depending on the trial's reward history?
t.test(rewBiasdf$prevLossNogo,rewBiasdf$prevRewNogo,paired=T) # significant difference in accuracy based on reward history. Subjects show more false alarms to square, 
  #which was previously associated with loss. 

#Given Juliet's findings about this reward bias changing from run to run (by age), will compute rewBias by run as well:
rewBias_r1 <- CARIT %>%
  filter(trialType=='nogo',runN==1) %>%
  group_by(sID,nogoCondition) %>%
  summarise(
    acc = mean(corrRespCode, na.rm=T)
  ) %>%
  spread(., key = nogoCondition, value = acc) %>%
  mutate(rewBias_r1 = prevRewNogo - prevLossNogo) %>%
  select(sID,rewBias_r1) #reward bias computed as the difference score between accuracy to prev reward minus accuracy to prev loss

rewBias_r2 <- CARIT %>%
  filter(trialType=='nogo',runN==2) %>%
  group_by(sID,nogoCondition) %>%
  summarise(
    acc = mean(corrRespCode, na.rm=T)
  ) %>%
  spread(., key = nogoCondition, value = acc) %>%
  mutate(rewBias_r2 = prevRewNogo - prevLossNogo) %>%
  select(sID,rewBias_r2) #reward bias computed as the difference score between accuracy to prev reward minus accuracy to prev loss

#add the new variables computed to the larger CARIT dataset:
CARIT <- CARIT %>%
  left_join(rewBiasdf, by = "sID")
CARIT <- CARIT %>%
  left_join(rewBias_r1, by = "sID")
CARIT <- CARIT %>%
  left_join(rewBias_r2, by = "sID")

detach(CARIT)
attach(CARIT)

#plot reward bias by run by subject:
(rewBias_time <- CARIT %>%
  group_by(sID) %>%
  summarise(
    tot = mean(rewBias),
    r1 = mean(rewBias_r1),
    r2 = mean(rewBias_r2)
  ) %>%
  gather(r1,r2,key = "runN",value="rewBias") %>%
  ggplot(.,aes(runN,rewBias)) +
  geom_line(aes(group = sID, colour = factor(sID))) + 
  geom_point() +
  geom_hline(yintercept = 0,colour='red',size=1) +
  labs(title='Reward bias by run, by subject', x= 'Run number', y = 'Reward bias') +
  scale_y_continuous(limits=c(-0.2,0.6),breaks = c(-0.2,0,0.2,0.4,0.6), labels = c('Worse to REWARD','0','0.2','0.4','Worse to LOSS')) +
  scale_x_discrete(labels = c('run 1','run 2')) +
  scale_color_discrete(name='Subject ID') +
  white_theme()
)

#reward bias by age
CARIT %>%
  group_by(sID) %>%
  summarise(
    age = age.x[1],
    rewBias = rewBias[1]
  ) %>%
  ggplot(.) +
  geom_point(aes(age,rewBias),color='seagreen') +
  geom_smooth(aes(age,rewBias),method='lm',color='seagreen') +
  geom_hline(yintercept = 0, colour='red', size=0.5) +
  scale_y_continuous(limits=c(-0.2,0.6),breaks = seq(-0.2,0.6,0.2), labels = c('Worse to REWARD -0.2','0','0.2','0.4','Worse to LOSS 0.6')) +
  scale_x_continuous(breaks = seq(8,22,2)) +
  labs(title = 'Reward bias by age', x='Age', y='Reward bias') +
  fte_theme()

#rename newly created variables:
CARIT <- plyr::rename(CARIT, c(prevRewNogo="accRew",prevLossNogo="accLoss",go="accGo",nogo="accNogo"))


#Total accuracy by subject:
subAcc <- CARIT %>% 
  group_by(sID) %>%
  summarise(acc = mean(corrRespCode),
            age = age.x[1],
            site = site[1]) %>%
  arrange(desc(acc))

#Subject with best performance:
cols <- c("total"="seagreen","no go"="firebrick","go"="navyblue")
CARIT %>%
  filter(sID == "HCD1500025") %>%
  ggplot(.) +
  geom_line(aes(x = aggTrialN,y = totalAcc, colour='total')) +
  geom_line(aes(aggTrialN, nogoAcc, colour='no go')) +
  geom_line(aes(aggTrialN, goAcc, colour='go')) +
  scale_y_continuous(limits=c(0,1)) +
  scale_colour_manual(name="Trial type", values=cols) +
  labs(x='Aggregate trial number', y='Accuracy', title='HCD1500025 Accuracy Across Trials') +
  fte_theme() +
  theme(legend.position = "right",legend.key=element_rect(fill=NA))

#Subject with worst performance:
cols <- c("total"="seagreen","no go"="firebrick","go"="navyblue")
CARIT %>%
  filter(sID == "HCD0989280") %>%
  ggplot(.) +
  geom_line(aes(aggTrialN,totalAcc, colour='total')) +
  geom_line(aes(aggTrialN,nogoAcc, colour='no go')) +
  geom_line(aes(aggTrialN,goAcc, colour='go')) +
  scale_y_continuous(limits=c(0,1)) +
  scale_colour_manual(name="Accuracy", values=cols) +
  labs(x='Aggregate trial number', y='Accuracy', title='HCD0989280 Accuracy Across Trials') +
  fte_theme() +
  theme(legend.position = "right",legend.key=element_rect(fill=NA))

cols <- c("total"="seagreen","no go"="firebrick","go"="navyblue")
CARIT %>%
  filter(sessionID == "HCDP1135_V1") %>%
  ggplot(.) +
  geom_line(aes(trialNum,totalAcc, colour='total')) +
  geom_line(aes(trialNum,nogoAcc, colour='no go')) +
  geom_line(aes(trialNum,goAcc, colour='go')) +
  scale_y_continuous(limits=c(0,1)) +
  scale_colour_manual(name="Accuracy", values=cols) +
  labs(x='Aggregate trial number', y='Accuracy', title='HCDP1135 Accuracy Across Trials') +
  white_theme() +
  theme(legend.position = "right",legend.key=element_rect(fill=NA))


#HCD0353538: This subject used to be one with worst performance, but through viz/analyses realized that her run 1 file had no recorded responses. See note below.
CARIT %>%
  filter(sessionID == "HCD0353538_V1_A") %>%
  ggplot(.) +
  geom_line(aes(aggTrialN,totalAcc)) +
  scale_y_continuous(limits=c(0.2,1)) + 
  fte_theme()

# NOTE about HCD0353538: This subject was pressing the wrong button during run 1 -- no responses recorded. Noticed at run 2. Stopped, and restarted, but ended up 
  #running round 1 again, and no run 2. Basically, this subject has 2 runs 1, and no run 2. One run 1 has no recorded responses, and the other is normal. 
  #Here using 'normal' run 1 only for analyses.


#noGo accuracy by subject across runs:
nogoAccRuns <- CARIT %>% 
  group_by(sID,runN) %>%
  summarise(
    acc = mean(corrRespCode),
    age = age.x[1],
    gender = gender[1],
    site = site[1]
  ) %>%
    ggplot(.,aes(factor(runN),acc)) +
    geom_line(aes(group = sID, colour = factor(sID))) + 
    geom_point() +
    labs(x='Run #', y='Accuracy') +  
    white_theme()

nogoAccRuns <- nogoAccRuns %>%
  spread(key='runN', value = 'acc')

nogoAccRuns$decAcc <-  nogoAccRuns$`1` - nogoAccRuns$`2`

# Plot difference in accuracy across runs average by site:
nogoAccRuns %>% 
  group_by(site) %>%
  summarise(
    acc = mean(decAcc, na.rm=T)
  ) %>%
  ggplot(.) + 
  geom_bar(aes(reorder(site,acc),acc, fill=site), stat = 'identity') +
  labs(x='',y=expression(paste(Delta,' Accuracy (Run 1 - Run 2)'))) +
  white_theme()

# Plot difference in accuracy across runs by subject across age (by site)
ggplot(nogoAccRuns) +
  geom_hline(yintercept=0, color = 'red', size = 1, linetype =2) +
  geom_point(aes(age,decAcc)) +
  #geom_smooth(aes(age,decAcc)) +
  scale_y_continuous(limits = c(-0.2, 0.6), breaks=seq(-0.2,0.6,0.2), labels = c('WORSE IN RUN 1', '0', '0.2', '0.4', 'WORSE IN RUN 2')) +
  labs(x='Age', y=expression(paste(Delta,' Accuracy (Run 1 - Run 2)'))) +
  white_theme() +
  theme(legend.position = 'bottom')

# Plot difference in accuracy across runs by subject across age, and disaggregate by site:
ggplot(nogoAccRuns) +
  geom_hline(yintercept=0, color = 'black', size = 1, linetype =2) +
  geom_point(aes(age,decAcc, color=site)) +
  geom_smooth(aes(age,decAcc)) +
  scale_y_continuous(limits = c(-0.2, 0.6), breaks=seq(-0.2,0.6,0.2), labels = c('WORSE IN RUN 1', '0', '0.2', '0.4', 'WORSE IN RUN 2')) +
  labs(x='Age', y=expression(paste(Delta,' Accuracy (Run 1 - Run 2)'))) +
  facet_grid(. ~ site) +
  white_theme() +
  theme(legend.position = 'bottom')

# D-Prime -----------------------------------------------------------------

#From Nosek & Banaji, 2001:
# "In signal detection, sensitivity is calculated with the following algorithm: (1) the proportion of hits 
# (correct "go" response for signal items) and false alarms (incorrect "go" response for noise items) are 
# each converted to z-scores; (2) a difference between the z-score values for hits and false alarms is d'."

#More info here: http://www.linguistics.ucla.edu/faciliti/facilities/statistics/dprime.htm

CARIT_summary <- CARIT %>%
  group_by(sID,corrRespTrialType) %>%
  summarise(
    count = n()
    ) %>%
  spread(., key = corrRespTrialType, value = count) #change to wide format

#changing NAs to zeroes so I can sum across columns
CARIT_summary$Hit[is.na(CARIT_summary$Hit)] <- 0
CARIT_summary$Miss[is.na(CARIT_summary$Miss)] <- 0
CARIT_summary$falseAlarm[is.na(CARIT_summary$falseAlarm)] <- 0
CARIT_summary$corReject[is.na(CARIT_summary$corReject)] <- 0

CARIT_summary <- CARIT_summary %>%
  mutate(
    propHit = Hit/(Hit + Miss), #total go trials = 136
    propFalseAlarm = falseAlarm/(falseAlarm + corReject) #total no-go trials = 48
    )

avgHitProp <-mean(CARIT_summary$propHit)
sdHitProp <-sd(CARIT_summary$propHit)
avgFAProp <- mean(CARIT_summary$propFalseAlarm)
sdFAProp <- sd(CARIT_summary$propFalseAlarm)

CARIT_summary <- CARIT_summary %>%
  mutate(
    Z_Hit = (propHit - avgHitProp)/sdHitProp,
    Z_FA = (propFalseAlarm - avgFAProp)/sdFAProp,
    dprime = Z_Hit - Z_FA
    ) %>%
  arrange(dprime)

summary(CARIT_summary$dprime)

#quick view of the distribution of dprime in the sample:
ggplot(CARIT_summary) +
  geom_histogram(aes(x=dprime),binwidth = 0.3, fill= 'tan1', color= 'white')+
  geom_vline(xintercept = 0, colour='red') +
  labs(title='d-prime distribution') +
  fte_theme()

#add the new variables computed to the larger CARIT dataset:
CARIT <- CARIT %>%
  left_join(CARIT_summary, by = "sID")

#dprime by age:
CARIT %>%
  group_by(sID) %>%
  summarise(
    age = age.x[1],
    dprime = dprime[1]
  ) %>%
  ggplot(.) +
  geom_point(aes(age,dprime)) +
  geom_smooth(aes(age,dprime)) +
  scale_x_continuous(breaks=seq(8,22,2)) +
  labs(title="d-prime across age") +
  fte_theme()

