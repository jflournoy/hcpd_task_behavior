#from brms::.kfold
local_kfold <- function (x, K = 10, Ksub = NULL, folds = 'grouped', group, 
                         save_fits = TRUE, newdata = NULL, resp, future_args = list(), 
                         newdata2 = NULL, cores = 4, chains = 4, threads = NULL, ...) 
{
  # x <- fit
  # group <- 'sID_factor'
  # resp <- fit$formula$resp[[1]]
  suppressWarnings(suppressMessages(attach(getNamespace("brms"))))
  stopifnot(is.brmsfit(x), is.list(future_args))
  if (is.brmsfit_multiple(x)) {
    warn_brmsfit_multiple(x)
    class(x) <- "brmsfit"
  }
  if (is.null(newdata)) {
    newdata <- x$data
  } else {
    newdata <- as.data.frame(newdata)
  }
  if (is.null(newdata2)) {
    newdata2 <- x$data2
  } else {
    bterms <- brmsterms(x$formula)
    newdata2 <- validate_data2(newdata2, bterms)
  }
  N <- nrow(newdata)
  if (!is.null(group)) {
    valid_groups <- brms:::get_cat_vars(x)
    if (length(group) != 1L || !group %in% valid_groups) {
      stop2("Group '", group, "' is not a valid grouping factor. ", 
            "Valid groups are: \n", collapse_comma(valid_groups))
    }
    gvar <- factor(get(group, newdata))
  }
  if (is.null(folds)) {
    if (is.null(group)) {
      fold_type <- "random"
      folds <- loo::kfold_split_random(K, N)
    } else {
      fold_type <- "group"
      folds <- as.numeric(gvar)
      K <- length(levels(gvar))
      message("Setting 'K' to the number of levels of '", 
              group, "' (", K, ")")
    }
  }  else if (is.character(folds) && length(folds) == 1L) {
    opts <- c("loo", "stratified", "grouped")
    fold_type <- match.arg(folds, opts)
    req_group_opts <- c("stratified", "grouped")
    if (fold_type %in% req_group_opts && is.null(group)) {
      stop2("Argument 'group' is required for fold type '", 
            fold_type, "'.")
    }
    if (fold_type == "loo") {
      folds <- seq_len(N)
      K <- N
      message("Setting 'K' to the number of observations (", 
              K, ")")
    } else if (fold_type == "stratified") {
      folds <- loo::kfold_split_stratified(K, gvar)
    }  else if (fold_type == "grouped") {
      folds <- loo::kfold_split_grouped(K, gvar)
    }
  } else {
    fold_type <- "custom"
    folds <- as.numeric(factor(folds))
    if (length(folds) != N) {
      brms:::stop2("If 'folds' is a vector, it must be of length N.")
    }
    K <- max(folds)
    message("Setting 'K' to the number of folds (", K, ")")
  }
  if (is.null(Ksub)) {
    Ksub <- seq_len(K)
  } else {
    is_array_Ksub <- is.array(Ksub)
    Ksub <- as.integer(Ksub)
    if (any(Ksub <= 0 | Ksub > K)) {
      stop2("'Ksub' must contain positive integers not larger than 'K'.")
    }
    if (length(Ksub) == 1L && !is_array_Ksub) {
      Ksub <- sample(seq_len(K), Ksub)
    }
    else {
      Ksub <- unique(Ksub)
    }
    Ksub <- sort(Ksub)
  }
  dots <- list(...)
  # dots <- list()
  ll_arg_names <- brms:::arg_names("log_lik")
  ll_args <- dots[intersect(names(dots), ll_arg_names)]
  ll_args$allow_new_levels <- TRUE
  ll_args$resp <- resp
  ll_args$combine <- TRUE
  up_args <- dots[setdiff(names(dots), ll_arg_names)]
  #up_args <- list(chains = 4, cores = 4, threads = NULL)
  up_args$refresh <- 0
  up_args$cores <- cores
  up_args$chains <- chains
  up_args$threads <- threads
  .kfold_k <- function(k) {
    if (fold_type == "loo" && !is.null(group)) {
      omitted <- which(folds == folds[k])
      predicted <- k
    } else {
      omitted <- predicted <- which(folds == k)
    }
    newdata_omitted <- newdata[-omitted, , drop = FALSE]
    fit <- x
    up_args$object <- fit
    up_args$newdata <- newdata_omitted
    up_args$data2 <- brms:::subset_data2(newdata2, -omitted)
    fit <- SW(do_call(update, up_args))
    ll_args$object <- fit
    ll_args$newdata <- newdata[predicted, , drop = FALSE]
    ll_args$newdata2 <- brms:::subset_data2(newdata2, predicted)
    lppds <- do_call(log_lik, ll_args)
    out <- brms:::nlist(lppds, omitted, predicted)
    if (save_fits) 
      out$fit <- fit
    return(out)
  }
  futures <- vector("list", length(Ksub))
  lppds <- obs_order <- vector("list", length(Ksub))
  if (save_fits) {
    fits <- array(list(), dim = c(length(Ksub), 3))
    dimnames(fits) <- list(NULL, c("fit", "omitted", "predicted"))
  }
  #x <- recompile_model(x)
  future_args$FUN <- .kfold_k
  future_args$seed <- TRUE
  for (k in Ksub) {
    ks <- match(k, Ksub)
    message("Fitting model ", k, " out of ", K)
    future_args$args <- list(k)
    futures[[ks]] <- .kfold_k(k)
  }
  for (k in Ksub) {
    ks <- match(k, Ksub)
    tmp <- futures[[ks]]
    if (save_fits) {
      fits[ks, ] <- tmp[c("fit", "omitted", "predicted")]
    }
    obs_order[[ks]] <- tmp$predicted
    lppds[[ks]] <- tmp$lppds
  }
  lppds <- do_call(cbind, lppds)
  elpds <- apply(lppds, 2, brms:::log_mean_exp)
  obs_order <- unlist(obs_order)
  elpds <- elpds[order(obs_order)]
  ll_args$object <- x
  ll_args$newdata <- newdata
  ll_args$newdata2 <- newdata2
  ll_full <- do_call(log_lik, ll_args)
  lpds <- apply(ll_full, 2, log_mean_exp)
  ps <- lpds - elpds
  pointwise <- cbind(elpd_kfold = elpds, p_kfold = ps, kfoldic = -2 * 
                       elpds)
  est <- colSums(pointwise)
  se_est <- sqrt(nrow(pointwise) * apply(pointwise, 2, var))
  estimates <- cbind(Estimate = est, SE = se_est)
  rownames(estimates) <- colnames(pointwise)
  out <- nlist(estimates, pointwise)
  atts <- nlist(K, Ksub, group, folds, fold_type)
  attributes(out)[names(atts)] <- atts
  if (save_fits) {
    out$fits <- fits
    out$data <- newdata
  }
  suppressWarnings(suppressMessages(detach(getNamespace("brms"))))
  return(structure(out, class = c("kfold", "loo")))
}