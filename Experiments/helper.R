make.filter = function(f.method, task.type = "surv") {
  martingale = substr(f.method, 1, 11) == "martingale."
  importance = substr(f.method, 1, 11) == "importance."
  if (martingale) {
    f.method = substr(f.method, 12, nchar(f.method))
    filter = flt("martingale", filter.method = make.filter(f.method, "regr"))
  } else if (importance) {
    f.method = substr(f.method, 12, nchar(f.method))
    if (f.method == "ranger.permutation") {
      l = lrn(paste0(task.type, ".ranger"), importance = "permutation", num.threads = 1)
    } else if (f.method == "ranger.impurity") {
      l = lrn(paste0(task.type, ".ranger"), importance = "impurity", num.threads = 1)
    } else {
      l = lrn(paste0(task.type, ".xgboost"),
        nrounds = 2000, eta = 0.05, max_depth = 10, subsample = 0.5, colsample_bytree = 0.5)
    }
    filter = flt("importance", learner = l)
  } else if (f.method %in% c("cmim", "disr", "jmi", "jmim", "mim", "mrmr", "njmim")) {
    filter = flt(f.method, threads = 1)
  } else {
    filter = flt(f.method)
  }
    return(filter)
}
