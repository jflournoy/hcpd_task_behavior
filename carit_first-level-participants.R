## @knitr cope1_check

library(data.table)

cope1_check_fn <- 'cope1_exists.rds'
if(!file.exists(cope1_check_fn)){
  data_dir <- '/ncf/hcp/data/HCD-tfMRI-MultiRunFix'
  pid_dirs <- dir(data_dir, pattern = 'HCD.*')
  acquisition_direction <- c('AP', 'PA')
  parcellated_cope1 <- 'MNINonLinear/Results/tfMRI_CARIT_AP/tfMRI_CARIT_AP_hp200_s4_level1_hp0_clean.feat/GrayordinatesStats/cope1.dtseries.nii'
  wholebrain_cope1 <- 'MNINonLinear/Results/tfMRI_CARIT_AP/tfMRI_CARIT_AP_hp200_s4_level1_hp0_clean_ColeAnticevic.feat/ParcellatedStats/cope1.ptseries.nii'
  
  cope1_dt <- data.table(expand.grid(sess_id = pid_dirs, 
                                     acquisition_direction = acquisition_direction,
                                     cope1 = c(parcellated_cope1, 
                                               wholebrain_cope1),
                                     stringsAsFactors = FALSE))
  cope1_dt[, filename := file.path(data_dir, sess_id, cope1)]
  cope1_dt[acquisition_direction == 'PA', filename := stringr::str_replace_all(filename, 'AP', acquisition_direction)]
  
  cope1_dt[, fid := 1:.N]
  cope1_dt[, exists := file.exists(filename), by = fid]
  cope1_dt[, sID := stringr::str_replace(sess_id, '(HCD\\d+)_V.*', '\\1')]
  cope1_dt[, analysis_type := stringr::str_replace(filename, '.*(Parcellated|Grayordinates)Stats.*', '\\1')]
  saveRDS(object = cope1_dt, file = cope1_check_fn)
} else {
  cope1_dt <- readRDS(file = cope1_check_fn)
}

