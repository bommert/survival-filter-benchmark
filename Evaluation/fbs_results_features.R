library(data.table)
library(batchtools)
library(dplyr)

loadRegistry(choose.dir())
load("fbs_results_tuning.RData")

fun.features = function(job, res) {
  ft = res$features.train
  if (!is.list(ft)) ft = as.list(as.data.frame(ft))
  data.table(
    job.id = job$job.id,
    dataset = job$prob.name,
    features.train = list(ft),
    features.test = list(res$features.test)
  )
}

reduce.and.aggregate.features = function(ids) {
  ids = findDone(ids)
  res = reduceResultsDataTable(ids, fun = fun.features)
  res = bind_rows(res$result)
  return(res)
}

results.features = reduce.and.aggregate.features(results.tuned$job.id)
save(results.features, file = "fbs_results_features.RData")
