library(data.table)
data.table::setDTthreads(1)

#simply lifted from the CARIT script
read_carit_dir <- function(data_path, pattern = "*GUESSING.*run[12]_wide.csv"){
  #List all of the CARIT task files in the data directory
  hcd_dirs <- dir(data_path, pattern = '^HCD.*', full.names = TRUE)
  carit_dirs <- unlist(lapply(hcd_dirs, dir, pattern = '^tfMRI_GUESSING_(AP|PA)$', full.names = TRUE))
  fnames <- unlist(lapply(carit_dirs, 
                          dir,
                          pattern = pattern, 
                          recursive = TRUE, 
                          full.names = TRUE))
  names(fnames) <- 1:length(fnames)
  
  #Set the column names we want from each csv file
  col_select <- c('trialNum','countdownStartTime','countdownLabel',
                  'cueStartTime','cueEndTime',
                  'isi1StartTime','isi1EndTime',
                  'ISI1','guessStartTime','guessEndTime',
                  'isi2StartTime','isi2EndTime','ISI2',
                  'feedbackStartTime','feedbackEndTime',
                  'feedbackName','valueCondition',
                  'guessResp.firstKey','guessResp.firstRt',
                  'guessResp.firstGuess','guessResp.keys',
                  'guessResp.rt','isiPress1.keys','isiPress1.rt',
                  'isiPress2.keys','isiPress2.rt','cumulativeNoResp',
                  'cumulativeReward','rewardAmount','rewShape','nRuns','run')
  
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

workspace_fname <- 'process_guessing.rda'
if(!file.exists(workspace_fname)){
  
  data_path <- '/ncf/hcp/data/intradb_multiprocfix/'
  
  guessing_data <- read_carit_dir(data_path)
  
  save.image(file = workspace_fname)
} else {
  load(workspace_fname)
}

saveRDS(guessing_data, '/ncf/hcp/data/analyses/mayalrosen/guessing_data.rds')
saveRDS(guessing_data, '/ncf/hcp/data/analyses/taylor/guessing_data.rds')
saveRDS(guessing_data, 'guessing_data.rds')
