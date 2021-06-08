library(mlr3)
library(Matrix)
library(BBmisc)

load("data_names.RData")
path.part = "Data/"

trafoIndex = function(index, nr) {
  i1 = ceiling(index / nr)
  i2 = index - (i1 - 1) * nr
  return(c(i1, i2))
}

sim.mats = lapply(data.names, function(dn, threshold = 0.9) {
  path = paste0(path.part, dn, ".RData")
  load(path)
  data = task.all$data()[, task.all$feature_names, with = FALSE]
  cc = abs(cor(data))

  gt = which(cc >= threshold)
  gt.mat = convertListOfRowsToDataFrame(lapply(gt, trafoIndex, nr = nrow(cc)))
  w = which(gt.mat[, 1] >= gt.mat[, 2])

  nf = nrow(cc)
  sparse.mat = sparseMatrix(gt.mat[w, 1], gt.mat[w, 2], x = cc[gt[w]],
    symmetric = TRUE, dims = c(nf, nf))

  colnames(sparse.mat) = rownames(sparse.mat) = task.all$feature_names

  rm(cc)
  rm(task.all)
  return(sparse.mat)
})

names(sim.mats) = data.names

save(sim.mats, file = "fbs_sim_mats.RData")
