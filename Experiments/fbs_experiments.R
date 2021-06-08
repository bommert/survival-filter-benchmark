library(batchtools)

load("data_names.RData")

# packages that have to be installed
pcks = c("mlr3", "mlr3learners", "mlr3proba", "mlr3pipelines", "mlr3filters", "R6", "data.table")
other.pcks = c("survival", "glmnet", "carSurv", "care", "praznik", "ranger", "xgboost")

inst.pcks = installed.packages()[, "Package"]
missing.pcks = setdiff(c(pcks, other.pcks), inst.pcks)
if (length(missing.pcks) > 0) {
  stop(paste("Please install the following packages:",
    paste(missing.pcks, collapse = ", ")))
}

reg = makeExperimentRegistry(file.dir = "fbs", packages = pcks, seed = 1)

###################################################################################
#### Definition of problems

for (i in seq_along(data.names)) {
  addProblem(
    name = data.names[i],
    seed = i,
    data = data.names[i],
    fun = function(job, data, outer.iter) {
      path1 = paste0("Data/", data, ".RData")
      load(path1)
      instance = list(task.all = task.all)

      if (outer.iter > 0) {
        path2 = paste0("Data/", data, "_", outer.iter, ".RData")
        load(path2)
        inst = list(task.train = task.train, rin.train = rin.train)
        instance = c(instance, inst)
      }

      return(instance)
    }
  )
}

####################################################################################
### Definition of algorithms


# filter + Cox model
addAlgorithm(
  name = "filter",
  fun = function(job, data, instance, f.method, filter.perc) {
    source("survival_filters.R")
    source("helper.R")

    # L2-regularized Cox model
    learner = ppl("distrcompositor", lrn("surv.cv_glmnet", s = "lambda.min", alpha = 0))

    # performance measures
    measures = list(msr("surv.graf"), msr("surv.cindex", weight_meth = "G2"))
    measures.names = c("surv.graf", "surv.uno_c")

    train.ids = instance$task.train$row_ids
    test.ids = setdiff(instance$task.all$row_ids, train.ids)

    # combine filter method and Cox model
    if (f.method != "none") {
      filter = make.filter(f.method)
      learner_po = po("learner", learner = learner)
      filter_po = po("filter", filter = filter, filter.frac = filter.perc, id = "filter")
      graph = filter_po %>>% learner_po
      glrn = GraphLearner$new(graph)
    } else {
      glrn = GraphLearner$new(learner)
    }

    # resample on training data of outer CV iteration
    rr = resample(instance$task.train, glrn, instance$rin.train, store_models = TRUE)
    performance.train = rr$score(measures)[, measures.names, with = FALSE]
    training.time.train = sapply(rr$learners, function(l) l$timings["train"])
    prediction.time.train = sapply(rr$learners, function(l) l$timings["predict"])

    # evaluate performance on test data of outer CV iteration
    # (may only be looked at, if this configuration is selected based on the results on the training data)
    t1 = Sys.time()
    glrn$train(instance$task.all, row_ids = train.ids)
    t2 = Sys.time()
    training.time.test = as.numeric(difftime(t2, t1), unit = "secs")

    t3 = Sys.time()
    pred = glrn$predict(instance$task.all, row_ids = test.ids)
    t4 = Sys.time()
    prediction.time.test = as.numeric(difftime(t4, t3), unit = "secs")

    performance.test = pred$score(measures, task = instance$task.all, train_set = train.ids)

    # record selected features
    if (f.method != "none") {
      features.train = lapply(rr$learners, function(l) l$model$filter$features)
      features.test = glrn$model$filter$features
    } else {
      features.train = replicate(10, instance$task.all$feature_names)
      features.test = instance$task.all$feature_names
    }

    return(list(
      performance.train = performance.train,
      performance.test = performance.test,
      training.time.train = training.time.train,
      training.time.test = training.time.test,
      prediction.time.train = prediction.time.train,
      prediction.time.test = prediction.time.test,
      features.train = features.train,
      features.test = features.test
    ))
  })


addAlgorithm(
  name = "filter.boosting",
  fun = function(job, data, instance, f.method, filter.perc) {
    source("helper.R")

    # L2-regularized Cox model
    learner = ppl("distrcompositor", lrn("surv.cv_glmnet", s = "lambda.min", alpha = 0))
    glrn = GraphLearner$new(learner)

    # performance measures
    measures = list(msr("surv.graf"), msr("surv.cindex", weight_meth = "G2"))
    measures.names = c("surv.graf", "surv.uno_c")

    train.ids = instance$task.train$row_ids
    test.ids = setdiff(instance$task.all$row_ids, train.ids)

    filter = make.filter(f.method)
    n.filter = round(filter.perc * length(instance$task.train$feature_names))

    n.iters = instance$rin.train$iters
    performance.train = data.frame(numeric(n.iters), numeric(n.iters))
    colnames(performance.train) = measures.names
    training.time.train = numeric(n.iters)
    prediction.time.train = numeric(n.iters)
    features.train = list()

    for (i in 1:n.iters) {
      task.inner = instance$task.train$clone()
      inner.train.ids = instance$rin.train$instance[fold != i, ]$row_id
      inner.test.ids = instance$rin.train$instance[fold == i, ]$row_id
      task.inner$filter(inner.train.ids)

      t1 = Sys.time()
      # filter features, but do not keep more than ranked by boosting
      filter$calculate(task.inner)
      n.not.na = sum(!is.na(filter$scores))
      n.select = max(min(n.not.na, n.filter), 1)
      filtered.feats = names(filter$scores[1:n.select])
      task.inner$select(filtered.feats)

      glrn$train(task.inner)
      t2 = Sys.time()

      training.time.train[i] = as.numeric(difftime(t2, t1), unit = "secs")

      t3 = Sys.time()
      pred = glrn$predict(instance$task.train, row_ids = inner.test.ids)
      t4 = Sys.time()

      prediction.time.train[i] = as.numeric(difftime(t4, t3), unit = "secs")

      perf = pred$score(measures, task = instance$task.train, train_set = inner.train.ids)
      performance.train[i, ] = perf
      features.train[[i]] = filtered.feats
    }

    # evaluate performance on test data of outer CV iteration
    # (may only be looked at, if this configuration is selected based on the results on the training data)
    t1 = Sys.time()
    filter$calculate(instance$task.train)
    n.not.na = sum(!is.na(filter$scores))
    n.select = max(min(n.not.na, n.filter), 1)
    filtered.feats = names(filter$scores[1:n.select])
    instance$task.train$select(filtered.feats)
    glrn$train(instance$task.train)
    t2 = Sys.time()

    training.time.test = as.numeric(difftime(t2, t1), unit = "secs")

    t3 = Sys.time()
    pred = glrn$predict(instance$task.all, row_ids = test.ids)
    t4 = Sys.time()
    prediction.time.test = as.numeric(difftime(t4, t3), unit = "secs")

    performance.test = pred$score(measures, task = instance$task.all, train_set = train.ids)
    features.test = filtered.feats


    return(list(
      performance.train = as.data.table(performance.train),
      performance.test = performance.test,
      training.time.train = training.time.train,
      training.time.test = training.time.test,
      prediction.time.train = prediction.time.train,
      prediction.time.test = prediction.time.test,
      features.train = features.train,
      features.test = features.test
    ))
  })


# filter scores only
addAlgorithm(
  name = "ranking",
  fun = function(job, data, instance, f.method) {
    source("survival_filters.R")
    source("helper.R")

    # calculate scores for all features
    filter = make.filter(f.method)
    filter$calculate(instance$task.all)
    scores = filter$scores

    # order the scores according to the data set
    scores = scores[instance$task.all$feature_names]

    return(scores)
  }
)



#####################################################################################
### Definition of designs

### Design for problems
design.tasks.filter = data.frame(
  outer.iter = 1:10
)

design.tasks.ranking = data.frame(
  outer.iter = 0
)

design.probs.filter = replicate(length(data.names), design.tasks.filter, simplify = FALSE)
names(design.probs.filter) = data.names

design.probs.ranking = replicate(length(data.names), design.tasks.ranking, simplify = FALSE)
names(design.probs.ranking) = data.names


### Design for algorithms


# filters that we want to use
filters.surv = c("surv.log.rank.test", "car.surv.scores", "variance",
  "importance.ranger.impurity", "importance.ranger.permutation")
filters.regr =  c("correlation", "cmim", "disr", "jmi", "jmim", "mim", "mrmr", "njmim")
filters.m = paste("martingale", filters.regr, sep = ".")
filters = sort(c(filters.surv, filters.m))
filters.boost = "importance.boosting"

filter.percs = seq(0.01, 1, length.out = 100)^2

design.algo.ranking = data.table(f.method = c(filters, filters.boost))

design.algo.filter1 = expand.grid(
  f.method = filters,
  filter.perc = filter.percs,
  stringsAsFactors = FALSE
)

design.algo.filter2 = data.table(f.method = "none", filter.perc = 1)

design.algo.filter = rbind(design.algo.filter1, design.algo.filter2)

design.algo.boost = expand.grid(
  f.method = filters.boost,
  filter.perc = filter.percs,
  stringsAsFactors = FALSE
)

#################################################################################
### Add Experiments

addExperiments(
  algo.designs = list(ranking = design.algo.ranking),
  prob.designs = design.probs.ranking
)

addExperiments(
  algo.designs = list(filter = design.algo.filter),
  prob.designs = design.probs.filter
)

addExperiments(
  algo.designs = list(filter.boosting = design.algo.boost),
  prob.designs = design.probs.filter
)


ids.ranking = findExperiments(algo.name = "ranking")
ids.filter = findExperiments(algo.name = "filter")
ids.boost = findExperiments(algo.name = "filter.boosting")
