library(mlr3)
library(data.table)
library(xtable)

load("data_names.RData")
path.part = "Data/"

info = lapply(data.names, function(dn) {
  path = paste0(path.part, dn, ".RData")
  load(path)
  n = task.all$nrow
  p = length(task.all$feature_names)
  n.e = sum(task.all$data()[["status"]])
  r.e = n.e / n
  rm(task.all)
  return(c(n = n, p = p, n.e = n.e, r.e = r.e))
})

info2 = Reduce(rbind, info)

res = cbind(data.frame(dataset = data.names), info2)
res$p = as.integer(res$p)
res$n = as.integer(res$n)
res$n.e = as.integer(res$n.e)

data.info = as.data.table(res)
save(data.info, file = "data_info.RData")

xtab = xtable(data.info)
print(xtab, include.rownames = FALSE)

