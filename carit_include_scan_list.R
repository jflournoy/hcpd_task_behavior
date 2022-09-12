library(argparse)
create_group_model <- function(x, contrast, contrast_name, outdir, contrast_type = 't'){
  
  # contrast_type = 't'
  # contrast = c("Hit_1go" = -1/4, "Hit_2go" = -1/4, "Hit_3go" = -1/4, "Hit_4go" = -1/4,
  #            "corReject_2go" = 1/3, "corReject_3go" = 1/3, "corReject_4go" = 1/3)
  # contrast_name = "CR_m_Go"
  # outdir = '/ncf/mclaughlin/users/jflournoy/code/hcpd_tfMRI/group_level'
  
  x <- x[EVtrialType %in% names(contrast)]
  x[, condition := factor(EVtrialType, levels = names(contrast))]
  setorder(x, sID, runN, condition)
  mm <- model.matrix(~ 0 + condition + site, data = x)
  
  if(!dir.exists(file.path(outdir, contrast_name))){
    dir.create(file.path(outdir,contrast_name))
  }
  if(!dir.exists(file.path(outdir, contrast_name, 'logs'))){
    dir.create(file.path(outdir,contrast_name, 'logs'))
  }
  cov_fn <- file.path(outdir, contrast_name, sprintf("%s_cov.txt", contrast_name))
  out_fn_gct <- file.path(outdir, contrast_name, sprintf("SwE_contrast_job_%s.m", contrast_name))
  out_fn_gctr <- file.path(outdir, contrast_name, sprintf("SwE_contrast_job_run_%s.m", contrast_name))
  out_fn_gst <- file.path(outdir, contrast_name, sprintf("SwE_sbatch_%s.bash", contrast_name))
  write.table(mm, file = cov_fn,
              col.names = FALSE, row.names = FALSE)
  
  scans <- paste(sprintf("'%s,1'", x[, file.path(first_level_dir, copes)]),
                 collapse = '\n')
  subs <- paste(as.numeric(as.factor(x[, sID])), collapse = '\n')
  
  con <- paste(contrast, collapse = ' ')
  
  group_contrast_template <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level/group_contrast_template.m')
  group_contrast_template_run <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level/group_contrast_template-run.m')
  group_sbatch_template <- stringi::stri_read_lines(con = '~/code/hcpd_tfMRI/group_level/group_sbatch_template.bash')
  
  match_list <- list("___MODELDIR___" = contrast_name,
                     "___SCANS___" = scans,
                     "___SUBJECTS___" = subs,
                     "___COVFILE___" = cov_fn,
                     "___T_CONTRAST___" = con,
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
  return(list(contrast = contrast,
              contrast_name = contrast_name, 
              contrast_matrix = mm,
              group_contrast_template = group_contrast_template,
              group_contrast_template_run = group_contrast_template_run,
              group_sbatch_template = group_sbatch_template))
}

parser <- ArgumentParser(allow_abbrev = FALSE, prefix_chars = '+')
parser$add_argument('++name', type = 'character')
parser$add_argument('++outdir', type = 'character')
parser$add_argument('++contrast_col_names', type = 'character', nargs = '+')
parser$add_argument('++contrast_col_values', type = 'character', nargs = '+')
parser$add_argument('++contrast_type', type = 'character', default = 't')

args <- parser$parse_args()
# args <- parser$parse_args(c('++name', 'CR_m_Go',
#                             '++outdir', '/ncf/mclaughlin/users/jflournoy/code/hcpd_tfMRI/group_level',
#                             '++contrast_col_names', 'Hit_1go', 'Hit_2go', 'Hit_3go', 'Hit_4go', 'corReject_2go', 'corReject_3go', 'corReject_4go',
#                             '++contrast_col_values', '-1/4', '-1/4', '-1/4', '-1/4', '1/3', '1/3', '1/3'))

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
contrast <- lapply(lapply(args$contrast_col_values, parse, file = '', n = NULL), eval)
names(contrast) <- args$contrast_col_names
contrast_type = args$contrast_type

message(sprintf(
"
Contrast name: %s
Contrast columns: %s
Constrast coding: %s
Output directory: %s
", contrast_name, 
paste(sprintf("%s", names(contrast)), collapse = ' '),
paste(sprintf("%0.3f", contrast), collapse = ' '), outdir))

#Read in data that we used to make the EVs
source('carit_EVs.R', echo = TRUE)

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

responses_summary <- carit[, list(goresp = sum(resp %in% 'go'), nogoresp = sum(! resp %in% 'go'), N_trials = .N), by = c('sID', 'runN')]
responses_summary[, list(min_go = min(goresp), max_go = max(goresp), min_nogo = min(nogoresp), max_nogo = max(nogoresp))]
response_exclusions <- responses_summary[goresp == N_trials | nogoresp == N_trials]
response_exclusions[, c('goresp', 'nogoresp', 'N_trials') := NULL]

copes[, cope_number := as.numeric(stringi::stri_replace_all_regex(copes, 'cope(\\d{1,2})\\.dtseries\\.nii', '$1'))]
copes[, EVtrialType := cope_trial_type[cope_number]]
N_type_trials <- carit_pr_scan_expanded[, list(N_trials = .N), by = c('sessionID', 'scanname', 'runN', 'EVtrialType', 'site')]
N_type_trials[, sID := stringi::stri_replace_all_regex(sessionID, '(HCD\\d+)_.*', '$1')]
N_type_trials[, c('sessionID', 'scanname') := NULL]
copes_with_trial_count <- merge(copes, N_type_trials, by = c('sID', 'runN', 'EVtrialType'), all.x = FALSE, all.y = FALSE)

#check that there are no NAs
identical(dim(na.omit(copes_with_trial_count)), dim(copes_with_trial_count))

#remove rows that match those in response exclusions
include_cope_list <- copes_with_trial_count[!response_exclusions, on = c('sID', 'runN')]

#include_cope_list[, .(.N), by = 'site']
# list(EVtrialType = c("Hit_1go", "Hit_2go", "Hit_3go", 
#                      "Hit_4go", "Miss", "corReject_2go", "corReject_3go", "corReject_4go", 
#                      "falseAlarm_2go", "falseAlarm_3go", "falseAlarm_4go"))
#Start with CR - Go

CR_m_Go <- create_group_model(include_cope_list, 
                              contrast = contrast,
                              contrast_name = contrast_name,
                              outdir = outdir, 
                              contrast_type = contrast_type)
