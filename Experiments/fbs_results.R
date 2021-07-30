library(BBmisc)
library(batchtools)
library(data.table)
library(dplyr)
library(stringr)

load("data_info.RData")
loadRegistry(choose.dir())

all = unwrap(getJobTable(findDone()))
ids.ranking = all[algorithm == "ranking", ]
ids.filter = all[algorithm == "filter", ]
ids.filter.boosting = all[algorithm == "filter.boosting", ]
ids.kaplan = all[algorithm == "kaplan", ]

# rename filter methods
filter.rename.fun = function(results) {
  results$filter = str_replace(results$filter, "martingale.", "")
  results$filter = str_replace(results$filter, "importance.", "")
  results$filter = str_replace(results$filter, "ranger.", "")
  results$filter = str_replace(results$filter, "car.surv.scores", "carss")
  results$filter = str_replace(results$filter, "surv.log.rank.test", "cox.score")
  return(results)
}

#################################################################################
# filter
# reduce and aggregate in parts because of memory consumption

fun.filter = function(job, res) {
  data.table(
    job.id = job$job.id,
    dataset = job$prob.name,
    filter = job$algo.pars$f.method,
    outer.iter = job$prob.pars$outer.iter,
    filter.perc = job$algo.pars$filter.perc,
    selected.perc = job$algo.pars$filter.perc,
    graf.train.mean = mean(res$performance.train$surv.graf),
    cindex.train.mean = mean(res$performance.train$surv.uno_c),
    graf.test = res$performance.test["surv.graf"],
    cindex.test = res$performance.test["surv.uno_c"],
    time.train.median = median(res$training.time.train + res$prediction.time.train),
    time.test = res$training.time.test + res$prediction.time.test
  )
}

reduce.and.aggregate.filter = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.filter)
  res = bind_rows(res$result)
  return(res)
}

results.filter1 = reduce.and.aggregate.filter(ids.filter)

fun.filter.boosting = function(job, res) {
  mean.selected = mean(lengths(res$features.train))
  p = data.info[dataset == job$prob.name, ]$p
  selected.perc = mean.selected / p

  data.table(
    job.id = job$job.id,
    dataset = job$prob.name,
    filter = job$algo.pars$f.method,
    outer.iter = job$prob.pars$outer.iter,
    filter.perc = job$algo.pars$filter.perc,
    selected.perc = selected.perc,
    graf.train.mean = mean(res$performance.train$surv.graf),
    cindex.train.mean = mean(res$performance.train$surv.uno_c),
    graf.test = res$performance.test["surv.graf"],
    cindex.test = res$performance.test["surv.uno_c"],
    time.train.median = median(res$training.time.train + res$prediction.time.train),
    time.test = res$training.time.test + res$prediction.time.test
  )
}

reduce.and.aggregate.filter.boosting = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.filter.boosting)
  res = bind_rows(res$result)
  return(res)
}

results.filter2 = reduce.and.aggregate.filter.boosting(ids.filter.boosting)


fun.kaplan = function(job, res) {
  data.table(
    job.id = job$job.id,
    dataset = job$prob.name,
    filter = "Kaplan-Meier",
    outer.iter = job$prob.pars$outer.iter,
    filter.perc = 0,
    selected.perc = 0,
    graf.train.mean = mean(res$performance.train$surv.graf),
    cindex.train.mean = mean(res$performance.train$surv.uno_c),
    graf.test = res$performance.test["surv.graf"],
    cindex.test = res$performance.test["surv.uno_c"],
    time.train.median = median(res$training.time.train + res$prediction.time.train),
    time.test = res$training.time.test + res$prediction.time.test
  )
}

reduce.and.aggregate.kaplan = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.kaplan)
  res = bind_rows(res$result)
  return(res)
}

results.kaplan = reduce.and.aggregate.kaplan(ids.kaplan)

results.filter = rbind(results.filter1, results.filter2, results.kaplan)
results.filter = filter.rename.fun(results.filter)

save(results.filter, file = "fbs_results_filter.RData")

#################################################################################
# ranking

fun.ranking = function(job, res) {
  data.table(
    dataset = job$prob.name,
    filter = job$algo.pars$f.method,
    scores = list(res)
  )
}

reduce.and.aggregate.ranking = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.ranking)
  res = bind_rows(res$result)
  return(res)
}

results.ranking = reduce.and.aggregate.ranking(ids.ranking)
results.ranking = filter.rename.fun(results.ranking)

save(results.ranking, file = "fbs_results_ranking.RData")
