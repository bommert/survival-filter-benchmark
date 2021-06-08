library(data.table)
library(batchtools)
load("fbs_results_features.RData")

stab.fun = function(i) {
  load("fbs_sim_mats.RData")
  load("fbs_results_features.RData")

  dataset = results.features[i, ]$dataset
  sm = sim.mats[[dataset]]
  features = results.features[i, ]$features.train[[1]]

  stab = stabilityIntersectionCount(features = features, sim.mat = sm)
  return(stab)
}

makeRegistry("fbs_stab", packages = "stabm", seed = 1)
batchMap(fun = stab.fun, i = 1:nrow(results.features))

# run when jobs have finished
res = reduceResultsList(fun = function(job, res) data.table(stability = res))
res.stability = dplyr::bind_rows(res)

results.stability = cbind(results.features[, 1:2], res.stability)
save(results.stability, file = "fbs_results_stability.RData")

