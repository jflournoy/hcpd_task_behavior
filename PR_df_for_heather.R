source('process_carit.R')
public_release[, public_release := TRUE]
carit_staged <- unique(carit[data_source == 'staged', c('sID', 'age', 'gender', 'RACE', 'SES_PLVL',
                                                        'SES_RLVL', 'income', 'sessionID')])
carit_with_pr <- public_release[carit_staged, on = 'sID']
carit_with_pr[, public_release := data.table::fifelse(public_release, TRUE, FALSE, na = FALSE)]
carit_with_pr[age >= 18, list(N_PR = sum(public_release),
                              N = .N)]
fwrite(carit_with_pr, 'HCPD_staged_and_pr_w_demogs.csv')
