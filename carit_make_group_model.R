TEST <- FALSE
#TEST <- TRUE
.libPaths(c('/ncf/mclaughlin/users/jflournoy/R/x86_64-pc-linux-gnu-library/verse-4.2.1', .libPaths()))
library(argparse)
create_group_model <- function(x, design, covars, outdir){
  
  x <- x[!grepl('(Miss|falseAlarm)', EVtrialType)]
  x[, condition := factor(EVtrialType, levels = c(paste0('Hit_', 1:4, 'go'), paste0('corReject_', 2:4, 'go')))]
  x[, c('EVtrialType', 'prepotency') := tstrsplit(EVtrialType, '_', fixed = TRUE)]
  # x[, c('EVtrialType', 'prepotency') := list(factor(EVtrialType, 
  #                                                 levels = c('Hit', 'corReject')),
  #                                          factor(prepotency,
  #                                                 levels = c(paste0(1:4, 'go'))))]
  setorder(x, sID, runN, EVtrialType, prepotency)
  if(design == 'maineffects'){
    formula <- '~ 0 + condition'
    contrasts_arg <- list(condition = 'contr.treatment')
    x <- x[grepl('(Hit|corReject)', EVtrialType)]
  } else if(design == 'interaction'){
    formula <- '~ 1 + EVtrialType + prepotency + EVtrialType:prepotency'
    contrasts_arg <- list(EVtrialType = 'contr.sum', prepotency = 'contr.sum')
    x <- x[grepl('(Hit|corReject)_[234]go', condition)]
  }
  
  covar_classes_fac <- NULL
  if(length(covars) > 0){
    formula <- sprintf('%s + %s', 
                       formula,
                       paste(covars, collapse = ' + '))
    covar_classes <- lapply(x[, .SD, .SDcols = covars], class)
    covar_classes_fac <- names(covar_classes[covar_classes %in% c('character', 'factor')])
    if(!is.null(covar_classes_fac)){
      covar_contr <- as.list(rep('contr.sum', length(covar_classes_fac)))
      names(covar_contr) <- covar_classes_fac
      contrasts_arg <- c(contrasts_arg, covar_contr)
    }
  }
  formula <- as.formula(formula)
  mm <- model.matrix(formula, data = x, contrasts.arg = contrasts_arg)
  
  if(length(covars) > 0){
    covar_mm_ix <- which(grepl(sprintf('(%s)', paste(covars, collapse = '|')), dimnames(mm)[[2]]))
    ncol_mm <- dim(mm)[[2]]
    n_covar_mm_ix <- (1:ncol_mm)[!1:ncol_mm %in% covar_mm_ix]
    reordered_mm_ix <- c(n_covar_mm_ix, covar_mm_ix)
    mm <- mm[, reordered_mm_ix]
  } else if(design == 'interaction'){
    #mean center the regressors
    cols_to_center <- grepl(sprintf('(%s)', 
                                    paste(c(covar_classes_fac, 'EVtrialType', 'prepotency', 'EVtrialType:prepotency'), 
                                          collapse = '|')), 
                            dimnames(mm)[[2]])
    mm[, cols_to_center] <- apply(mm[, cols_to_center], 2, scale, center = TRUE, scale = FALSE)
  }
  
  
  if(!dir.exists(file.path(outdir, contrast_name))){
    dir.create(file.path(outdir,contrast_name))
  }
  if(!dir.exists(file.path(outdir, contrast_name, 'logs'))){
    dir.create(file.path(outdir,contrast_name, 'logs'))
  }
  cov_fn <- file.path(outdir, contrast_name, sprintf("%s_cov.txt", contrast_name))
  cov_tracker_fn <- file.path(outdir, contrast_name, sprintf("%s_cov_tracker.csv", contrast_name))
  out_fn_gct <- file.path(outdir, contrast_name, sprintf("SwE_contrast_job_%s.m", contrast_name))
  out_fn_gctr <- file.path(outdir, contrast_name, sprintf("SwE_contrast_job_run_%s.m", contrast_name))
  out_fn_gst <- file.path(outdir, contrast_name, sprintf("SwE_sbatch_%s.bash", contrast_name))
  write.table(mm, file = cov_fn,
              col.names = FALSE, row.names = FALSE)
  write.csv(mm, file = cov_tracker_fn, row.names = FALSE)
  
  scans <- paste(sprintf("'%s,1'", x[, file.path(first_level_dir, copes)]),
                 collapse = '\n')
  subs <- paste(as.numeric(as.factor(x[, sID])), collapse = '\n')
  
  group_contrast_template <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level_vwise/group_contrast_template.m')
  group_contrast_template_run <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level_vwise/group_contrast_template-run.m')
  group_sbatch_template <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level_vwise/group_sbatch_template.bash')
  
  match_list <- list("___MODELDIR___" = contrast_name,
                     "___SCANS___" = scans,
                     "___SUBJECTS___" = subs,
                     "___COVFILE___" = cov_fn,
                     "___JOBFILE___" = out_fn_gct,
                     "___RUNFILE___" = out_fn_gctr
  )
  
  group_contrast_text <- stringi::stri_replace_all(group_contrast_template, 
                                                   regex = names(match_list),
                                                   replacement = match_list,
                                                   vectorize_all = FALSE)
  stringi::stri_write_lines(str = group_contrast_text, con = out_fn_gct)
  
  group_contrast_run_text <- stringi::stri_replace_all(group_contrast_template_run, 
                                                       regex = names(match_list),
                                                       replacement = match_list,
                                                       vectorize_all = FALSE)
  stringi::stri_write_lines(str = group_contrast_run_text, con = out_fn_gctr)
  
  group_sbatch_text <- stringi::stri_replace_all(group_sbatch_template, 
                                                 regex = names(match_list),
                                                 replacement = match_list,
                                                 vectorize_all = FALSE)
  stringi::stri_write_lines(str = group_sbatch_text, con = out_fn_gst)
  return(list(contrast_name = contrast_name, 
              contrast_matrix = mm,
              group_contrast_template = group_contrast_template,
              group_contrast_template_run = group_contrast_template_run,
              group_sbatch_template = group_sbatch_template))
}

prep_data <- function(covars){
  #Read in data that we used to make the EVs
  initial_options <- commandArgs(trailingOnly = FALSE)
  file_arg_name <- "--file="
  script_name <- sub(file_arg_name, '', initial_options[grep(file_arg_name, initial_options)])
  script_basename <- dirname(script_name)
  load_data <- 'carit_EVs.R'
  if(length(script_basename) != 0){
    load_data <- file.path(script_basename, load_data)
    setwd(script_basename)
  } else {
    #manually set the dir
    setwd('~/code/hcpd_task_behavior/')
  }
  source(load_data, echo = TRUE)
  
  data_root <- '/ncf/hcp/data/HCD-tfMRI-MultiRunFix'
  session_label <- 'V1_MR'
  task_name <- 'CARIT'
  cope_pattern <- '^cope\\d{1,2}\\.dtseries\\.nii'
  
  cope_trial_type <- c("Hit_1go", "Hit_2go", "Hit_3go", "Hit_4go", "Miss", "corReject_2go", 
                       "corReject_3go", "corReject_4go", "falseAlarm_2go", "falseAlarm_3go", 
                       "falseAlarm_4go")
  
  possible_scans <- unique(carit[, c('sID', 'filename', 'runN')])
  possible_scans[, direction := stringi::stri_replace_all_regex(filename, '.*_(AP|PA)/.*', '$1')]
  possible_scans[, first_level_dir := file.path(data_root, 
                                                sprintf('%s_%s', sID, session_label),
                                                'MNINonLinear', 'Results',
                                                sprintf('tfMRI_%s_%s', task_name, direction),
                                                sprintf('tfMRI_%s_%s_hp200_s4_level1_hp0_clean.feat', task_name, direction),
                                                'GrayordinatesStats')]
  #Collect the actual copes we have on disk.
  copes_rds <- 'carit_copes.rds'
  if(!file.exists(copes_rds)){
    system.time({copes <- possible_scans[, list(copes = dir(first_level_dir, pattern = cope_pattern)), by = c('sID', 'runN', 'first_level_dir')]})
    saveRDS(copes, copes_rds)
  } else {
    copes <- readRDS(copes_rds)  
  }
  
  N_cope_pxs <- length(unique(copes$sID))
  Avg_run_per_px <- dim(unique(copes[, c('sID', 'first_level_dir')]))[[1]] / N_cope_pxs
  
  message(sprintf('Total participants with copes: %s\nAvg runs per participant: %s',
                  N_cope_pxs, round(Avg_run_per_px, 2)))
  copes_proportion <- copes[, .('Proportion of Max' = round(.N / (N_cope_pxs * 2), 2)), 
        by = 'copes'][, copes := stringi::stri_replace_all(copes, fixed = '.dtseries.nii', '')]
  message('Copes proportion of max possible:')
  nada <- apply(copes_proportion, 1, \(x) message(sprintf('% 6s: %s', x[[1]], x[[2]])))
  
  responses_summary <- carit[, list(goresp = sum(resp %in% 'go'), nogoresp = sum(! resp %in% 'go'), N_trials = .N), by = c('sID', 'runN')]
  responses_summary[, list(min_go = min(goresp), max_go = max(goresp), min_nogo = min(nogoresp), max_nogo = max(nogoresp))]
  response_exclusions <- responses_summary[goresp == N_trials | nogoresp == N_trials]
  response_exclusions[, c('goresp', 'nogoresp', 'N_trials') := NULL]
  
  fd_fn <- 'RelativeRMS_mean.rds'
  if(!file.exists(fd_fn)){
    source('collect_FD.R')
    fd_df <- collect_FD(unique(carit_pr_scan_expanded[, c('sessionID', 'scanname')]),
                        fd_base_dir = '/ncf/hcp/data/intradb_multiprocfix')
    saveRDS(fd_df, fd_fn)
  } else {
    fd_df <- readRDS(fd_fn)
  }
  
  RelativeRMS_mean_median <- median(fd_df[, .(mean = mean(RelativeRMS_mean, na.rm = TRUE)), by = 'sessionID'][, mean])
  fd_df[, RelativeRMS_mean_c := RelativeRMS_mean - RelativeRMS_mean_median]
  
  carit_pr_scan_expanded <- merge(carit_pr_scan_expanded, fd_df, all.x = TRUE, all.y = FALSE, by = c('sessionID', 'scanname'))
  
  copes[, cope_number := as.numeric(stringi::stri_replace_all_regex(copes, 'cope(\\d{1,2})\\.dtseries\\.nii', '$1'))]
  copes[, EVtrialType := cope_trial_type[cope_number]]
  N_type_trials <- carit_pr_scan_expanded[, list(N_trials = .N), by = c('sessionID', 'scanname', 'runN', 'EVtrialType', covars)]
  N_type_trials[, sID := stringi::stri_replace_all_regex(sessionID, '(HCD\\d+)_.*', '$1')]
  N_type_trials[, c('sessionID', 'scanname') := NULL]
  
  N_behav_pxs <- length(unique(N_type_trials$sID))
  Avg_run_per_behave_px <- dim(unique(N_type_trials[!is.na(runN), c('sID', 'runN')]))[[1]] / N_behav_pxs
  
  message(sprintf('Total participants with behavior: %s\nAvg runs per participant: %s',
                  N_behav_pxs, round(Avg_run_per_behave_px, 2)))
  N_type_trials_proportion <- N_type_trials[!is.na(runN), .('Proportion of Max' = round(.N / (N_behav_pxs * 2), 2)), 
                            by = 'EVtrialType']
  message('Trial type proportion of max possible:')
  nada <- apply(N_type_trials_proportion, 1, \(x) message(sprintf('% 14s: %s', x[[1]], x[[2]])))
  
  copes_with_trial_count <- merge(copes, N_type_trials, by = c('sID', 'runN', 'EVtrialType'), all.x = FALSE, all.y = FALSE)
  
  N_copebehav_pxs <- length(unique(copes_with_trial_count$sID))
  N_per_nrun <- unique(copes_with_trial_count[, c('sID', 'runN')])[, .N, by = 'sID'][, .(npx = .N), by = 'N']
  
  Avg_run_per_copebehave_px <- dim(unique(copes_with_trial_count[!is.na(runN), c('sID', 'runN')]))[[1]] / N_behav_pxs
  
  message(sprintf('Total participants with copes and behavior: %s\nAvg runs per participant: %s',
                  N_copebehav_pxs, round(Avg_run_per_copebehave_px, 2)))
  message(sprintf('Number of participants with 2 runs: %s\nNumber of participants with 1 run: %s',
                  N_per_nrun[N == 2, npx], N_per_nrun[N == 1, npx]))
  
  staged_data <- fread('~/code/hcpd_data_status/HCPD_staged_scans.csv')
  missing_copes <- unique(N_type_trials[, c('sID')])[!unique(copes[, c('sID')]), on = c('sID')]
  setnames(missing_copes, 'sID', 'Participant')
  missing_data_report <- staged_data[missing_copes, on = 'Participant'][Visit == 'V1', c('Participant', 'tfMRI', 'Visit')]
  N_missing <- dim(missing_data_report)[[1]]
  message(sprintf('Number of participants missing imaging data: %s', N_missing))
  
  #One subject has missing FD covariate
  
  #check that there are no NAs
  identical(dim(na.omit(copes_with_trial_count)), dim(copes_with_trial_count))
  length(unique(na.omit(copes_with_trial_count)[, sID]))
  length(unique(copes_with_trial_count[, sID]))
  unique(copes_with_trial_count[, c('sID', 'scanner')])[, list(missing_scanner = sum(is.na(scanner)))]
  unique(copes_with_trial_count[, c('sID', 'scanner')])[is.na(scanner)]
  
  #remove rows that match those in response exclusions
  include_cope_list <- na.omit(copes_with_trial_count[!response_exclusions, on = c('sID', 'runN')])
  
  #include_cope_list[, .(.N), by = 'site']
  # list(EVtrialType = c("Hit_1go", "Hit_2go", "Hit_3go", 
  #                      "Hit_4go", "Miss", "corReject_2go", "corReject_3go", "corReject_4go", 
  #                      "falseAlarm_2go", "falseAlarm_3go", "falseAlarm_4go"))
  #Start with CR - Go
  return(include_cope_list)
}

parser <- ArgumentParser(allow_abbrev = FALSE, prefix_chars = '+')
parser$add_argument('++name', type = 'character')
parser$add_argument('++outdir', type = 'character')
parser$add_argument('++design', type = 'character') #main
parser$add_argument('++covariates', type = 'character', nargs = '+')

if(!TEST){
  args <- parser$parse_args()  
} else {
  args <- parser$parse_args(c('++name', 'full_design2',
                              '++outdir', '/ncf/mclaughlin/users/jflournoy/code/hcpd_tfMRI/group_level_vwise',
                              '++design', 'interaction',
                              '++covariates', 'scanner', 'RelativeRMS_mean_c'))
}



print('Arguments are: ')
print(args)

contrast_name <- args$name
outdir <- args$outdir
if(!dir.exists(file.path(outdir, contrast_name))){
  dir.create(file.path(outdir,contrast_name))
}
if(!dir.exists(file.path(outdir, contrast_name, 'logs'))){
  dir.create(file.path(outdir,contrast_name, 'logs'))
}
if(!dir.exists(file.path(outdir, contrast_name))){
  stop('Cannot find or create directory.')
}
if(! length(args$contrast_col_names) == length(args$contrast_col_values)){
  stop('Length of column names and values not equal.')
}
covars <- args$covariates
design <- args$design

message(sprintf(
"
Contrast name:    %s
Design:           %s
Covariates:       %s
Output directory: %s
", contrast_name, 
design,
paste(sprintf("%s", covars), collapse = ' '),
outdir))

#xx <- prep_data()
#x = copy(xx)
CR_m_Go <- create_group_model(x = prep_data(covars), 
                              design = design,
                              covars = covars,
                              outdir = outdir)
