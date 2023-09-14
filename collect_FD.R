collect_FD <- function(pid_df, fd_base_dir = '/ncf/hcp/data/intradb_multiprocfix'){
  fd <- unlist(apply(pid_df, 1, \(x){
    fd_path <- file.path(fd_base_dir, 
                        x[['sessionID']], 
                        sprintf('%s_preproc', x[['scanname']]),
                        x[['sessionID']], x[['scanname']],
                        'Movement_RelativeRMS_mean.txt')
    fd <- ifelse(file.exists(fd_path), 
                 fread(fd_path, header = FALSE, col.names = 'FD', nrows = 1, sep = ","),
                 NA)
    return(fd)
  }))
  pid_df[, RelativeRMS_mean := fd]
  return(copy(pid_df))
}
