########## HCPD EMOTION ANALYSES ##########
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

#String all subjects' data files together into one 
# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/EMOTION/ALL_rawData/")
path = "/Users/kastman/Dropbox/ANDL/ProjectAnalyses/HCPD/Behavioral"
setwd(path) #set working directory
out.file<-""
file.names <- dir(paste(path, "/data", sep=""), pattern ="EMOTION.*wide.csv*", full.names = TRUE)
for(i in 1:length(file.names)){
  file.name = file.names[i]
  file <- data.frame(read_csv(file.names[i]))
  file <- file[-c(1:4),c('counterbalance','trialNum','trialCondition',
                         'stimLeft','stimTop','stimRight','corrAns','resp', 'trialResp.keys',
                         'trialResp.rt', 'corrRespMsg','corrRespCode', 'countConsecutive')]
  
  m = regexpr('(HCD[A-Za-z0-9]+_V1_[A|B|X])', file.name)
  sessionID = regmatches(file.name, m)
  file$filename <- file.name
  file$sessionID = sessionID
  
  out.file <- rbind(out.file, file)
}
write_csv(out.file[-1,],"compiled/EMOTION_allRaw.csv") #removing first blank row that was somehow generated


# DATA WRANGLING ----------------------------------------------------------


#Read csv with all participants' raw EMOTION data
EMOTION <- data.frame(read_csv("EMOTION_allRaw.csv"))
# EMOTION <- EMOTION[-c(1:4),]

head(EMOTION)
str(EMOTION)


#turn a few variables into factor form
factorVars <- names(dplyr::select(EMOTION,sessionID,counterbalance:resp,corrRespMsg:countConsecutive))

for (i in c(1:ncol(EMOTION[factorVars]))) {
  EMOTION[factorVars][[i]] <- as.factor(EMOTION[factorVars][[i]])
}
str(EMOTION)

# check for potential typos in sessionID:
subs <- EMOTION %>%
  group_by(sessionID) %>%
  summarise(count = n()) # e.g. "HCD05058945_V1_A" should be "HCD0508945_V1_A"

# EMOTION$sessionID <- as.character(EMOTION$sessionID) #need to change to character format to edit below
# EMOTION$sessionID[EMOTION$sessionID == "HCD05058945_V1_A"] <- "HCD0508945_V1_A" #UMinn subject
# EMOTION$sessionID[EMOTION$sessionID == "HCD1969058_V1_A"] <- "HCD1959680_V1_A" #no idea what happened here
# EMOTION$sessionID[EMOTION$sessionID == "1181843_V1_A"] <- "HCD1181843_V1_A" 
# EMOTION$sessionID[EMOTION$sessionID == "HCA2925264_V1_A"] <- "HCD2925264_V1_A" #UMN subject; incorrectly entered sessionID as if for Aging.
# EMOTION$sessionID[EMOTION$sessionID == "HCD1079044_V1"] <- "HCD1079044_V1_A" 
# EMOTION$sessionID[EMOTION$sessionID == "HCD103318_V1_A"] <- "HCD1103318_V1_A" 
# EMOTION$sessionID[EMOTION$sessionID == "1181843_V1_A"] <- "HCD1181843_V1_A" 
# 
# EMOTION$sessionID <- factor(EMOTION$sessionID) #change back to factor format

#add subj id column (i.e. session without V1_A)
EMOTION$sID <- substring(EMOTION$sessionID,1,10)

#load and add separate dataset with age and gender:
ageGender <- data.frame(read_csv("20190412_Demos_HCPD Staging Participants.csv")) # this file was obtained from Cindy Hernke through REDCap (has all subjects registered to date, with age and gender)
EMOTION <- EMOTION %>%
  dplyr::left_join(ageGender, by = "sID")

#reformat date
EMOTION$date <- substring(EMOTION$date,1,10)
(EMOTION$date <- as.Date(EMOTION$date))

#remove first and last characters in trialResp.keys 
EMOTION$trialResp.keys <- substr(EMOTION$trialResp.keys, 2, nchar(EMOTION$trialResp.keys)-1)

#some people pressed a button multiple times, and multiple RTs were recorded. Keep only the first one:
EMOTION$trialResp.rt <- substr(EMOTION$trialResp.rt ,2,11)

EMOTION$trialResp.keys <- as.factor(EMOTION$trialResp.keys)
EMOTION$trialResp.rt <- as.numeric(as.character(EMOTION$trialResp.rt))

#reorder columns
EMOTION <- EMOTION %>%
  dplyr::select(sID,sessionID,date,age,gender,everything())

# setwd("/Users/ConstanzaVidal/Desktop/Projects/HCPD_tfMRI/EMOTION/ALL_rawData/")
write_csv(EMOTION,"compiled/EMOTION_allSubs.csv")




# LOAD CLEAN DATAFILE -----------------------------------------------------------

#Load clean dataset with all subs EMOTION data:
EMOTION <- data.frame(read_csv("EMOTION_allSubs.csv"))

attach(EMOTION)

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

###GGPLOT FTE THEME
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
    theme(plot.title=element_text(color=color.title, size=14, vjust=1.25)) +
    theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.25)) +
    
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


#### DESCRIPTIVE STATS / EDA&VIZ -------------------------------------------------------

# Task design sanity checks ------------------------------------------------------

# Check task design characteristics are met for all participants:
# EMOTION has one round of 36 trials:
table(sID) 

# Each subject should have 18 shape and 18 face trials 
table(sID,trialCondition) 

# Each subject should have 18 left and 18 right trials
table(sID,corrAns) # left/right trials not balanced!

#check which counterbalances are asymmetric: 

check1 <- EMOTION %>%
  filter(sID == 'HCD0828660')
  
EMOTION %>%
  group_by(sID,corrAns) %>%
  summarise(
    count = n(),
    counterbalance = counterbalance[1],
    date = date[1]
    ) %>%
  spread(., key = corrAns, value = count) %>%
  filter(left != 18) # older release of Psychopy scripts had error in left/right counterbalancing. This
                      #was addressed in the 2/21 release, but UMinn continued to use older version
                      #until 3/7


# Sample details ----------------------------------------------------------

# How many subjects?
subs <- EMOTION %>%
  group_by(sID) %>%
  summarise(
    count = n(),
    age = age[1],
    gender = gender[1],
    date = date[1],
    site = site[1]
  )
nrow(subs) #203

check <- anti_join(ageGender, subs, by = 'sID')


mystats(ageGender$age) # 8-21
table(ageGender$gender) # 95 F, 111 M

# Plot age + gender distribution
ggplot(ageGender, aes(x = floor(age))) + 
  geom_histogram(binwidth=1,color='white',aes(fill=gender)) +
  labs(title="Age and gender distribution of the sample (N=175)",x="Age", y="Count") +
  scale_x_continuous(breaks=seq(8,21,1)) +
  coord_cartesian(xlim = c(8,21)) +
  white_theme() +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank(), legend.key=element_rect(fill=NA)) 


# Reaction time -----------------------------------------------------------

summary(EMOTION$trialResp.rt, na.rm=T) 

table(resp,useNA = 'ifany')

# Plot distribution of all RTs, across trials, across subjects:
ggplot(EMOTION) +
  geom_histogram(aes(trialResp.rt), binwidth = 0.1, fill = 'seagreen3', colour = 'white') +
  labs(title='Distribution of reaction times', x='Reaction time (in seconds)', y='Count') +
  white_theme() # Stims shown for 2s

# Distribution of all RTs by age:
ggplot(EMOTION) +
  geom_point(aes(age,trialResp.rt)) +
  white_theme() #5 RTs under 0.2s

# RT distribution by left/right condition
EMOTION %>%
  ggplot(.) +
  geom_histogram(aes(trialResp.rt, fill = factor(corrAns)), colour = 'white') +
  labs(title='Distribution of reaction times by matching condition', x='Reaction time (in seconds)', y='Count') +
  facet_grid(corrAns ~ .) +
  scale_fill_manual(values=c('dodgerblue3','gold')) +
  white_theme()

aggregate(trialResp.rt, by=list(corrAns=corrAns), summary,na.rm=T)

# boxplots of RTs by matching side condition
EMOTION %>%
  filter(!is.na(trialResp.rt)) %>%
  ggplot(.) +
  geom_boxplot(aes(corrAns, trialResp.rt, fill=corrAns)) +
  labs(title="", x ='', y='Reaction time (s)') +
  scale_fill_manual(values = c('dodgerblue3','gold')) +
  white_theme()


# perform t-test: average RT per subject per face/shape condition (paired t test)
pairedTtest2 <- EMOTION %>%
  filter(!is.na(trialResp.rt)) %>%
  group_by(sID,corrAns) %>%
  summarise(
    meanRT = mean(trialResp.rt)
  )

t.test(meanRT ~ corrAns, pairedTtest2, paired=T) #no significant difference in means

# RT distribution by face/shape condition
EMOTION %>%
  ggplot(.) +
  geom_histogram(aes(trialResp.rt, fill = factor(trialCondition)), colour = 'white') +
  labs(title='Distribution of reaction times by trial type', subtitle='Not averaged by subject', x='Reaction time (in seconds)', y='Count') +
  facet_grid(trialCondition ~ .) +
  scale_fill_manual(values=c('seagreen3','tan1')) +
  white_theme()

# Average reaction times by face/shape across participants
EMOTION %>%
  filter(!is.na(trialResp.rt)) %>%
  group_by(trialCondition) %>%
  summarise(
    meanRT = mean(trialResp.rt) 
  ) #slightly faster to respond to shape

aggregate(trialResp.rt, by=list(trialCondition=trialCondition), summary,na.rm=T)

# boxplots of RTs by matching condition
EMOTION %>%
  filter(!is.na(trialResp.rt)) %>%
  ggplot(.) +
  geom_boxplot(aes(trialCondition, trialResp.rt, fill=trialCondition)) +
  labs(title="", x ='', y='Reaction time (s)') +
  scale_fill_manual(values = c('seagreen3','darkorange')) +
  white_theme()

#perform t-test: average RT per subject per face/shape condition (paired t test)
pairedTtest <- EMOTION %>%
  filter(!is.na(trialResp.rt)) %>%
  group_by(sID,trialCondition) %>%
  summarise(
    meanRT = mean(trialResp.rt)
  )

t.test(meanRT ~ trialCondition, pairedTtest,paired=T) #no significant difference in means



# Accuracy -------------------------------------------------------------

table(corrRespMsg)
# correct incorrect      miss 
#   5739       426        99 

# distribution of NAs among subjects
EMOTION %>%
  filter(is.na(trialResp.keys)) %>%
  group_by(sID,trialCondition) %>%
  summarise(
    count = n()
    ) # fairly evenly distributed

# double check NAs for keys match NAs for RT:
EMOTION %>%
  filter(is.na(trialResp.rt)) %>%
  select(sID, date, trialCondition, trialResp.keys, trialResp.rt)

# What buttons/how many times were people pressing?
table(trialResp.keys) #some subjects were pressing multiple times per trial. Only first press counts to determine accuracy.

# Keep only the first press in the column:
detach(EMOTION)
EMOTION$trialResp.keys <- substring(EMOTION$trialResp.keys,1,1)
table(EMOTION$trialResp.keys)

# Change  blue/yellow presses to 1/2 (i.e. standardize across participating sites):
EMOTION$trialResp.keys[EMOTION$trialResp.keys == "b"] <- "1"
EMOTION$trialResp.keys[EMOTION$trialResp.keys == "y"] <- "2"
table(EMOTION$trialResp.keys)

attach(EMOTION)

# Who wasn't pressing?
EMOTION %>%
  filter(is.na(trialResp.keys)) %>%
  dplyr::select(sID,age,gender,trialCondition,corrRespMsg) 


# Better accuracy for face > shape
EMOTION %>%
  group_by(trialCondition) %>%
  summarise(
    acc = mean(corrRespCode) #face = 94.0%, shape = 89.3%
  ) %>%
  ggplot(.) +
  geom_bar(aes(trialCondition,acc,fill=trialCondition),stat='identity') +
 # geom_errorbar(aes(ymin=acc-se, ymax=acc+se), colour="black", width=.1) +
  scale_fill_manual(values = c('seagreen3', 'tan1')) +
  #coord_cartesian(ylim=c(0.5,1)) +
  labs(title='', x='', y='Accuracy') +
  white_theme()


pairedTtest_acc <- EMOTION %>%
  group_by(sID,trialCondition) %>%
  summarise(
    acc = mean(corrRespCode)
    )

t.test(acc ~ trialCondition,pairedTtest_acc, paired=T)

# How many incorrect per subject?
EMOTION %>%
  filter(corrRespMsg=='incorrect') %>%
  dplyr::select(sID,age,gender,trialCondition,corrRespMsg) %>% #most incorrect on shapes condition?
  group_by(sID, trialCondition,age) %>%
  summarise(
    count = n()
  ) %>%
  arrange(desc(count))

# Accuracy by face/shape by subject
accSub <- EMOTION %>%
  group_by(sID,trialCondition) %>%
  summarise(
    acc = mean(corrRespCode),
    age = age[1],
    gender = gender[1],
    site = site[1]
  ) %>%
  ggplot(.,aes(trialCondition,acc)) +
  geom_line(aes(group = sID, colour = factor(sID))) + 
  geom_point() +
  scale_color_discrete(name='Subject ID') +
  labs(title='Accuracy by matching condition by subject', x='','accuracy') +
  fte_theme()

#Average accuracy by subject:
accSub <- EMOTION %>%
  group_by(sID) %>%
  summarise(
    acc = mean(corrRespCode),
    age = age[1],
    gender = gender[1],
    site = site[1]
  ) %>%
  arrange(acc)

# Write acccuracy stats to file:
# setwd("/Users/constanzavidal/Desktop/Projects/HCPD_tfMRI/EMOTION/")
write.csv(accSub, "compiled/EMOTION_accBySubj.csv")

# Check individual subjects' accuracy
EMOTION %>%
  filter(sID=='HCD0410322') %>%
  group_by(trialCondition) %>%
  summarise(
    acc = mean(corrRespCode) # faces = 95%, shapes = 67%
  ) %>% 
  ggplot(.) +
  geom_bar(aes(trialCondition,acc,fill=trialCondition), stat='identity') + 
  coord_cartesian(ylim=c(0.5,1)) +
  labs(title='Accuracy, Subject HCD2001517', x='', y='accuracy') +
  fte_theme()

EMOTION %>%
  filter(sID=='HCD0410322') %>%
  group_by(trialCondition,corrRespMsg) %>%
  summarise(
    count = n()
  ) # 2 incorrect, 11 misses total


# How many subjects in each counterbalance condition?
EMOTION %>%
  group_by(counterbalance) %>%
  summarise(
    count = n()/36
  ) # most participants got CB1 so far

# Average accuracy by counterbalance
EMOTION %>%
  group_by(counterbalance,trialCondition) %>%
  summarise(
    acc = mean(corrRespCode)
  ) %>% # CB2 shows opposite accuracy pattern as CB1, ie. better accuracy for shapes > faces. However only 3/23 subjects in CB2 so far.
  ggplot(.) +
  geom_bar(aes(counterbalance,acc,fill=trialCondition),position='dodge',stat='identity') +
  coord_cartesian(ylim=c(0.7,1)) + 
  scale_fill_discrete(name='Matching condition') +
  labs(title ='Average accuracy by counterbalance', x='', y='accuracy') +
  fte_theme() + 
  theme(legend.position = 'bottom',legend.key= element_rect(fill=NA))

# Table with No presses by subject:
noPresses <- EMOTION %>%
  filter(corrRespMsg=='miss') %>%
  group_by(sID) %>%
  summarise(
    count = n(),
    prop = n()/36,
    age = age[1],
    gender = gender[1],
    site = site[1]
  )

# Write to file:
# setwd("/Users/constanzavidal/Desktop/Projects/HCPD_tfMRI/EMOTION/")
# write.csv(noPresses, "compiled/EMOTION_noPressesBySubj.csv")

# No presses by subject across age:
EMOTION %>%
  filter(corrRespMsg=='miss') %>%
  group_by(sID) %>%
  summarise(
    prop = n()/36,
    age = age[1],
    gender = gender[1],
    site = site[1]
  ) %>%
  ggplot(.) + 
  geom_point(aes(age,prop)) +
  geom_smooth(aes(age,prop)) +
  labs(x = 'Age', y = 'Proportion of no responses out of total trials') +
  white_theme() #most no presses among younger participants

# No presses by subject across age, disgregated by site:
countNA <- EMOTION %>%
  group_by(sID,trialResp.keys) %>%
  summarise(
    count = n(),
    age = age[1],
    gender = gender[1],
    site = site[1]
  ) %>%
  spread(., key = 'trialResp.keys', value = count)

countNA <- replace_na(countNA, list(`<NA>`= 0, `1` = 0, `2` = 0))
countNA$propNA <- countNA$`<NA>`/(countNA$`<NA>` + countNA$`1` + countNA$`2`)

ggplot(countNA) + 
  geom_point(aes(age,propNA, color=site)) +
  geom_smooth(aes(age,propNA)) +
  labs(x = 'Age', y = 'Proportion of no responses out of total trials') +
  facet_grid(. ~ site) +
  white_theme()

countNA %>%
  group_by(site) %>%
  summarise(
    avgPropNA = mean(propNA)
  ) %>%
  ggplot(.) + 
  geom_bar(aes(reorder(site,avgPropNA),avgPropNA, fill = site), stat = 'identity') +
  labs(x = '', y='Average proportion of no responses') +
  white_theme()
  

