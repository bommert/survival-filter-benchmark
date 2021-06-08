library(mlr3)
library(mlr3proba)
library(OpenML)
library(BBmisc)

set.seed(2020)

if (!dir.exists("Data")) dir.create("Data")
load("datset_ids.RData")

# subset of data sets: at least 50 events
nams = c("BLCA", "BRCA", "HNSC", "KIRC", "LGG", "LUAD", "LUSC", "OV", "PAAD", "SKCM", "STAD")
dataset_ids = datset_ids[sort(nams)]

data.names = names(dataset_ids)
save(data.names, file = "data_names.RData")

# resampling
folds = 10
resampling = rsmp("cv", folds = folds)


for (i in seq_along(dataset_ids)) {
  dat_part = lapply(dataset_ids[[i]], getOMLDataSet)
  dat = Reduce(cbind.data.frame, dat_part)

  # keep targets, mirna and rna data
  keep = c("time", "status", "rna")
  inds = lapply(keep, function(k) grep(colnames(dat), pattern = k))
  inds = unlist(inds)
  dat = dat[, inds]

  task.all = TaskSurv$new(id = names(dataset_ids)[i], backend = dat, time = "time", event = "status", type = "right")
  task.all$col_roles$stratum = "status"
  path.dataset = paste0("Data/", data.names[i],".RData")
  save(task.all, file = path.dataset)

  # resample instances for nested 10 fold CV
  r = resampling$clone()
  r$instantiate(task.all)
  for (j in 1:folds) {
    #task.test = task.all$clone()
    #task.test$filter(r$instance[fold == j, ]$row_id)

    task.train = task.all$clone()
    task.train$filter(r$instance[fold != j, ]$row_id)

    rin.train = resampling$clone()
    rin.train$instantiate(task.train)

    path.tasks = paste0("Data/", data.names[i], "_", j, ".RData")
    save(task.train, rin.train, file = path.tasks)
  }
}

