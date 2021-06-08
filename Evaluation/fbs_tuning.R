library(dplyr)
library(BBmisc)
library(data.table)

set.seed(2021)
load("fbs_results_filter.RData")

##########################################################################################
# tuning: select best configuration (repl) based on accuracy
# solve ties by selecting the one with fastest runtime

graf.train.best = results.filter[, list(graf.train.mean = min(graf.train.mean)),
  by = c("dataset", "filter", "outer.iter")]

results.tuned = merge(results.filter, graf.train.best,
  by = intersect(colnames(results.filter), colnames(graf.train.best)))

# detect and resolve ties
ties = results.tuned[, .N, by = c("dataset", "filter", "outer.iter")]
ties = ties[N > 1, ]
if (nrow(ties) > 0) {
  remove.rows = numeric(0)
  for (i in 1:nrow(ties)) {
    ids = which(
      results.tuned$filter == ties$filter[i] &
        results.tuned$dataset == ties$dataset[i] &
        results.tuned$outer.iter == ties$outer.iter[i]
    )
    part = results.tuned[ids, ]

    min.time = min(part$time.train.median)
    min.part.id = which(part$time.train.median == min.time)

    if (length(min.part.id) > 1) {
      min.part.id = sample(min.part.id, 1)
    }

    remove.new = ids[-min.part.id]
    remove.rows = c(remove.rows, remove.new)
  }
  results.tuned = results.tuned[-remove.rows, ]
}

# when all filter experiments are done, there should be a 10 in each entry
table(results.tuned[, c("filter", "dataset")], useNA = "ifany")


# aggregate the outer performance measures for each method
results.tuned.aggr = results.tuned[, list(
  graf.test.mean = mean(graf.test),
  time.test.median = median(time.test)),
  by = c("dataset", "filter")]

save(results.tuned, results.tuned.aggr, file = "fbs_results_tuning.RData")
