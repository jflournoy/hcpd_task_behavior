########## HCPD GUESSING ANALYSES ##########
## Constanza M. Vidal Bustamante


## Change the paths / working directories to your own. 


# Load R packages

if (!require("tidyverse")) {install.packages("tidyverse"); require("tidyverse")}  
if (!require("reshape")) {install.packages("reshape"); require("reshape")} #to melt datasets
if (!require("RColorBrewer")) {install.packages("RColorBrewer"); require("RColorBrewer")} 
if (!require("gridExtra")) {install.packages("gridExtra"); require("gridExtra")} 
if (!require("nlme")) {install.packages("nlme"); require("nlme")} 
if (!require("plotly")) {install.packages("plotly"); require("plotly")} 


# COMPILE DATA ------------------------------------------------------------

### String all subjects' data files together into one

path = "/Users/kastman/Dropbox/ANDL/ProjectAnalyses/HCPD/Behavioral"
setwd(path) #set working directory
out.file<-""
file.names <- dir(paste(path, "/data", sep=""), pattern ="GUESSING.*wide.csv", full.names = TRUE)
for(i in 1:length(file.names)){
  file.name = file.names[i]
  file <- data.frame(read_csv(file.name))
  # file <- file[-c(1:4), ]
  file <- file[-c(1:4),c('rewShape','nRuns','run','trialNum','ISI1','ISI2',
                        'feedbackName','valueCondition',
                        'guessResp.firstKey','guessResp.firstRt','guessResp.keys',
                        'guessResp.rt','isiPress1.keys','isiPress1.rt',
                        'isiPress2.keys','isiPress2.rt','cumulativeNoResp','cumulativeReward',
                        'rewardAmount')]
  m = regexpr('(HCD[A-Za-z0-9]+_V1_[A|B|X])', file.name)
  
  sessionID = regmatches(file.name, m)
  file$filename <- file.names[i]
  file$sessionID = sessionID
  out.file <- rbind(out.file, file)
}
write_csv(out.file[-1,],"GUESSING_allRaw.csv") #removing first blank row


# DATA WRANGLING ----------------------------------------------------------

#Read csv with all participants' raw GUESSING data
GUESS <- data.frame(read_csv("GUESSING_allRaw.csv"))

head(GUESS) # take a quick look at the first five rows of the data file
str(GUESS) # check the each variable's currently assigned class

# select variables that should be transformed to factor
factorVars <- names(select(GUESS,sessionID, filename,ISI1,ISI2,valueCondition, guessResp.firstKey))

# transform those variables to factor
for (i in c(1:ncol(GUESS[factorVars]))) {
  GUESS[factorVars][[i]] <- as.factor(GUESS[factorVars][[i]])
}
str(GUESS) # check result

#Check for typos on session IDs:
checkID <- GUESS %>%
  group_by(sessionID) %>%
  summarise(n())

# # fix session ID typos:
# GUESS$sessionID <- as.character(GUESS$sessionID) #need to change to character format to edit below
# GUESS$sessionID[GUESS$sessionID == "HCD05058945_V1_A"] <- "HCD0508945_V1_A" #UMinn subject
# GUESS$sessionID[GUESS$sessionID == "HCD2643456_V1_A121"] <- "HCD2643456_V1_A"
# GUESS$sessionID[GUESS$sessionID == "1181843_V1_A"] <- "HCD1181843_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD1646670_V1_A"] <- "HCD1646760_V1_A"
# GUESS$sessionID[GUESS$sessionID == "HCD1277134_V1_A"] <- "HCD1227134_V1_A"
# GUESS$sessionID[GUESS$sessionID == "HCA2925264_V1_A"] <- "HCD2925264_V1_A" #UMN subject; incorrectly entered sessionID as if for Aging.
# GUESS$sessionID[GUESS$sessionID == "HCD1079044_V1"] <- "HCD1079044_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD0840852_V1_A5"] <- "HCD0840852_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD103318_V1_A"] <- "HCD1103318_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD0899280_V1_A"] <- "HCD0989280_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD2302430_V1_At"] <- "HCD2302430_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "HCD16096553_V1_A"] <- "HCD1609653_V1_A" 
# GUESS$sessionID[GUESS$sessionID == "1181843_V1_A"] <- "HCD1181843_V1_A" 

# GUESS$sessionID <- factor(GUESS$sessionID) #change back to factor format

# add subj id column (without V1_A)
GUESS$sID <- substring(GUESS$sessionID,1,10)

# # load and add separate dataset with subjects' age and gender:
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/")
ageGender <- data.frame(read_csv("20190412_Demos_HCPD Staging Participants.csv")) # this file was obtained from Cindy Hernke through REDCap (has all subjects registered to date, with age and gender)

# join the psychopy files with the age and gender file (joining by subject ID)
GUESS <- GUESS %>%
  dplyr::left_join(ageGender, by = "sID")

# # edit format of date column
# GUESS$date <- substring(GUESS$date,1,10)
# (GUESS$date <- as.Date(GUESS$date))


# make duplicate of trial number column, add +24 to all rows that have run==2. (so we can count up to 48 trials)
GUESS$aggTrialN <- GUESS$trialNum

GUESS <- within(GUESS,{ 
  aggTrialN <- NA
  aggTrialN[run==1] <- trialNum[run==1] 
  aggTrialN[run==2] <- trialNum[run==2] + 24 
})

# change blue/yellow presses to 1/2 (so we have uniform responses across participating sites):
GUESS$guessResp.firstKey[GUESS$guessResp.firstKey == "b"] <- "1"
GUESS$guessResp.firstKey[GUESS$guessResp.firstKey == "y"] <- "2"

# reorder columns
GUESS <- GUESS %>%
  select(sID,sessionID,age,gender,nRuns:trialNum,aggTrialN,everything())

# save this edited/clean dataset
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/GUESSING/ALL_rawData/")
write_csv(GUESS,"GUESSING_allSubs.csv")


# LOAD CLEAN DATAFILE -----------------------------------------------------------

#Load clean dataset with all subjects' GUESSING data (generated above):
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/GUESSING/ALL_rawData/")
GUESS <- data.frame(read_csv("GUESSING_allSubs.csv"))

factorVars <- names(select(GUESS,sessionID,ISI1,ISI2,valueCondition,guessResp.firstKey))

for (i in c(1:ncol(GUESS[factorVars]))) {
  GUESS[factorVars][[i]] <- as.factor(GUESS[factorVars][[i]])
}
str(GUESS)

GUESS <- GUESS[-c(1:4),]
attach(GUESS)

### Load "white theme" for ggplot

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

### Load "five thirty eight theme" for ggplot
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
    theme(axis.text.x=element_text(size=12,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=12,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=14,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=14,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# function to quickly compute summary stats
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



#### DESCRIPTIVE STATS / EDA&VIZ -------------------------------------------------------


# Task design sanity checks ------------------------------------------------------
# Check task design characteristics are met for all participants:

# GUESSING has two rounds of 24 trials each, for a total of 48 trials per subject:
table(sID) #there should be 48 observations per subject.
trialsSubj <- GUESS %>%
  group_by(sID) %>%
  summarise(
    count = n()
  ) %>%
  arrange(count)  

# Each stakes/feedback combination should have 12 trials:
table(sID,valueCondition) 

# ISI bins:
table(ISI1)
table(ISI2)

check <- anti_join(ageGender,trialsSubj, by = 'sID')

# Sample details ----------------------------------------------------------

#load and add dataset with age and gender:
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/")
# ageGender <- data.frame(read_csv("ageGender_dev.csv"))

# How many subjects?
subs <- GUESS %>%
  group_by(sID) %>%
  summarise(
    age = age[1],
    gender = gender[1]
  )
nsubs = nrow(subs) #206 subjects with Guessing files, 9/5/17

# Missing files but already added to my ageGender spreadsheet
nrow(ageGender) #206 subjects tested across sites

# number of subjects per site
ageGender %>%
  group_by(site) %>%
  summarise( count = n())

mystats(ageGender$age) # 8-21
table(ageGender$gender) # 95 F, 111 M 

# Plot age + gender distribution
ggplot(ageGender, aes(x = floor(age))) + 
  geom_histogram(binwidth=1,color='white',aes(fill=gender)) +
  labs(title="Age and gender distribution of the sample", subtitle=paste("N=", nsubs),x="Age", y="Count") +
  scale_x_continuous(breaks=seq(8,21,1)) +
  coord_cartesian(xlim = c(8,21)) +
  white_theme() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(), legend.key=element_rect(fill=NA)) 



# Performance -------------------------------------------------------------

# The Guessing task has fixed outcomes: all participants, regardless of their guesses/presses
#  will always get 50% correct feedback and 50% incorrect feedback. 

# Instead of accuracy, check that participants were always pressing a button when they were
# supposed to press, as well as the reaction times associated with those presses. This is important in creating
# the reward association to the shapes presented in the feedback phase (which are then used as nogo stims in CARIT)

# Distribution of guesses (1 vs. 2, or baby vs. adult): is it roughly 50-50?
table(guessResp.firstKey, useNA = 'ifany') # quite a few no-presses

# plot distribution of presses/guesses
GUESS %>%
  group_by(guessResp.firstKey) %>%
  summarise(
    count = n()
  ) %>%
  ggplot(aes(guessResp.firstKey,count)) +
  geom_bar(stat ='identity',aes(fill=factor(guessResp.firstKey))) +
  scale_x_discrete(labels=c('Baby', 'Adult', 'No press')) +
  labs(x='', title='Total count of presses/no presses') +
  white_theme()

# Plot distribution of presses/no presses by subject (somewhat hard to visualize with so many subjects):
GUESS %>%
  group_by(sID,guessResp.firstKey) %>%
  summarise(
    count = n()
  ) %>%
  ggplot(.,aes(guessResp.firstKey,count)) +
  geom_hline(yintercept = 24, linetype = 2, size = 0.5) +
  geom_line(aes(group = sID, colour = factor(sID))) + 
  geom_point() +
  labs(title='Presses by subject', x= 'Guess', y = 'Count') +
  scale_x_discrete(labels = c('Baby','Adult','No press')) +
  scale_color_discrete(name='Subject ID') + 
  fte_theme() 

# table with distribution of presses / no presses
presses <- GUESS %>%
  group_by(sID,run, guessResp.firstKey) %>%
  summarise(
    count = n(),
    age = age[1],
    gender = gender[1],
    site = site[1]
  ) %>%
  spread(.,key=guessResp.firstKey,value=count) %>%
  arrange(desc(`2`)) #HCD2643456 has 34 adult presses (12 baby presses)
  #arrange(desc(`<NA>`)) #HCD2335344 has 15 no responses... rest of subs seem to be fine 

# replace "NAs" with zeros
presses <- replace_na(presses, list(`<NA>`= 0, `1` = 0, `2` = 0))
presses$total <- presses$`1` + presses$`2` + presses$`<NA>`

presses$prop <- (presses$`<NA>`)/(presses$`1` + presses$`2` + presses$`<NA>`) #calculate proportion of no presses out of total 
                                              #possible presses per subject. Doing this instead of just 48 since some subjects did one run only.
presses <- presses %>%
  arrange(desc(prop))

# Write 'presses' to file:
# setwd("/Users/constanzavidal/Desktop/Projects/HCPD_tfMRI/GUESSING/")
# write.csv(presses, "GUESS_noPressesBySubj.csv")


# histogram of % no responses per subject
ggplot(presses) +
  geom_histogram(aes(prop), binwidth = 0.1,color='white', fill='yellowgreen') +
  labs(title='Prop of GUESSING no-responses per run', x='Proportion of no-responses') +
  white_theme()

# histogram of total count of no responses per subject
ggplot(presses) +
  geom_histogram(aes(`<NA>`), binwidth = 1,color='white', fill='yellow3') +
  labs(title='Number of no-responses per subject', x='# no-responses (total trials = 48)') +
  fte_theme()

#closer look at HCD2643456: are his 'adult' guesses evenly distributed b/w the 2 runs?
GUESS %>%
  filter(sID == 'HCD2643456', guessResp.firstKey==2) %>%
  group_by(sID,run) %>%
  summarise(
    count = n() 
  ) 

#closer look at HCD1079044: are her no presses evenly distributed b/w the 2 runs? did he fall asleep?
GUESS %>%
  filter(sID == 'HCD1079044',is.na(guessResp.firstKey)) %>%
  group_by(sID,run) %>%
  summarise(
    count = n()
  ) %>%
  ggplot(aes(factor(run,levels=c(1,2)),count)) +
  geom_bar(stat ='identity',aes(fill=factor(run))) +
  scale_x_discrete(breaks=c(1,2), labels=c('run 1', 'run 2')) +
  scale_fill_manual(breaks=c(1,2),
                    values=c("tan1","mediumseagreen")) +
  labs(x='', title='Distribution of no presses across runs',subtitle='Subject HCD1079044') +
  fte_theme() # Most no presses during second run of the task. In fact, she didn't respond at all during the second run (all 24 trials were no presses).


# proportion of no presses by age:
ggplot(presses) +
  geom_point(aes(age,prop))+
  geom_smooth(aes(age,prop)) +
  white_theme() +
  theme(legend.position = 'bottom') 

# average proportion of no presses by site:
presses %>%
  group_by(site) %>%
  summarise(
    avgPropNoPress = mean(prop)
  ) %>%
ggplot(.) + 
  geom_bar(aes(reorder(site,avgPropNoPress),avgPropNoPress, fill=site), stat = 'identity') +
  labs(x='',y='Proportion of no presses') +
  white_theme()

# proportion of no presses by age by site:
ggplot(presses) +
  geom_point(aes(age,prop, color=factor(site)))+
  geom_smooth(aes(age,prop)) +
  facet_grid(. ~ site) +
  white_theme()

# proportion of no pressess by subject by run:
ggplot(presses) + 
  geom_point(aes(factor(run),prop)) + 
  geom_line(aes(run,prop,group =sID, color=sID)) +
  labs(x = '', y = 'Proportion of no responses by subject') +
  white_theme()
  
presses_spread <- presses %>%
  spread(key='run', value='prop')

pressesSm <- presses_spread[,c(1:4,7,8)] 
pressesSm <- replace_na(pressesSm, replace = list(`1`=0, `2` = 0))
pressesSm$propDiffRuns <- pressesSm$`1` - pressesSm$`2`


ggplot(pressesSm) +
  geom_point(aes(age,propDiffRuns)) +
  geom_hline(yintercept = 0, linetype = 2, color = 'red', size = 0.5) +
  labs(x = 'Age', y = expression(paste(Delta,' Prop of no responses (Run 1 - Run 2)'))) +
  white_theme()

ggplot(pressesSm) +
  geom_point(aes(age,propDiffRuns, color = site)) +
  geom_hline(yintercept = 0, linetype = 2, color = 'red', size = 0.5) +
  labs(x = 'Age', y = expression(paste(Delta,' Prop of no responses (Run 1 - Run 2)'))) +
  facet_grid(. ~ site) + 
  white_theme()


# Reaction times ----------------------------------------------------------

summary(guessResp.firstRt)

# Reaction times total distribution
ggplot(GUESS) +
  geom_histogram(aes(guessResp.firstRt)) +
  white_theme()

# Reaction times by stakes:
GUESS %>%
  filter(!is.na(guessResp.firstKey)) %>%
  ggplot(.) +
  geom_histogram(aes(guessResp.firstRt, fill=guessResp.firstKey),color='white') +
  facet_grid(valueCondition ~ .) +
  labs(title="RT distribution by stakes and guessing choice", x ='Reaction time (s)', y='Count') +
  white_theme() +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name='Guess',labels=c('Baby', 'Adult'))
#No seeming difference in RT between high/low stakes

aggregate(guessResp.firstRt, by=list(valueCondition=valueCondition), summary,na.rm=T)

# boxplots of RTs by value condition
GUESS %>%
  filter(!is.na(guessResp.firstKey)) %>%
  ggplot(.) +
  geom_boxplot(aes(valueCondition, guessResp.firstRt, fill=valueCondition)) +
  labs(title="RT by monetary stakes", x ='', y='Reaction time (s)') +
  scale_fill_manual(values = c('firebrick','dodgerblue3')) +
  white_theme()

# perform t-test: average RT per subject per value condition (paired t test)
pairedTtest <- GUESS %>%
  filter(!is.na(guessResp.firstRt)) %>%
  group_by(sID,valueCondition) %>%
  summarise(
    meanRT = mean(guessResp.firstRt)
  ) #%>%
  #spread(.,key=valueCondition,value=meanRT)

t.test(meanRT ~ valueCondition, pairedTtest,paired=T) 


# Reaction times by guessing choice:
GUESS %>%
  filter(!is.na(guessResp.firstKey)) %>%
  ggplot(.) +
  geom_histogram(aes(guessResp.firstRt, fill=guessResp.firstKey),color='white') +
  facet_grid(guessResp.firstKey ~ .) +
  labs(title="RT distribution by guessing choice", x ='Reaction time', y='Count') +
  fte_theme() 
#No seeming difference in RT between guessing baby/adult


# Calculating average RT to High and Low stakes per participant
RTs <- GUESS %>%
  group_by(sID,valueCondition) %>%
  summarise(
    avgRT = mean(guessResp.firstRt,na.rm=T),
    age = age[1],
    gender = gender[1]
  ) %>%
  spread(.,key=valueCondition,value=avgRT) 

RTs$RTdiff <- RTs$low - RTs$high

#Plot RT difference Low - High stakes by participant across age:
ggplot(RTs) +
  geom_point(aes(age,RTdiff)) +
  geom_smooth(aes(age,RTdiff), method='lm') +
  geom_hline(yintercept = 0, color = 'red', linetype=2, size =0.5) +
  scale_x_continuous(breaks=seq(8,22,2)) +
  labs(x='Age', y = expression(paste(Delta,' RT (Low - High Stakes)'))) +
  white_theme()
  
# Reward conditioning -----------------------------------------------------

# Compute rewarded shape by subject, as well as site and date acquired:
rewBySubj <- GUESS %>%
  group_by(sID) %>%
  summarise (
    rewShape = rewShape[1],
    date = date[1],
    site = site[1]
  )

rewBySubj %>%
  group_by(site, rewShape) %>%
  summarise(
    count = n()
  ) %>%
  ggplot(.) + 
  geom_bar(aes(site,count, fill = rewShape), position='dodge',stat='identity') +
  scale_y_continuous(breaks=seq(0,120,20)) +
  labs(x='', y='') +
  white_theme() +
  theme(legend.position = 'right')


table(rewBySubj$rewShape) # as of 8/19, 125 subjects have associated reward to circle, and 50 to square

# Save this info into new csv file combined with age/gender and site info. Will need it to more easily pair subject-rewHistory for CARIT:
# ageGender$sID <- substring(ageGender$sessionID,1,10)
# 
# ageGender <- ageGender %>%
#   select(sID,sessionID,everything())
# 
# ageGender <- left_join(ageGender,rewBySubj,by=c('sID'))
# 
# setwd("/Users/constanzavidal/Desktop/Projects/HCPD_tfMRI/")
# write.csv(ageGender,'ageGender.csv', row.names = F)  

