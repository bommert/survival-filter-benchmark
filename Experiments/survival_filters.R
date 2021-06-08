FilterMartingale = R6Class("FilterMartingale",
  inherit = Filter,
  public = list(
    filter.method = NULL,
    initialize = function(filter.method = mlr3filters::flt("variance")) {
      self$filter.method = filter.method
      super$initialize(
        id = "martingale",
        task_type = "surv",
        feature_types = filter.method$feature_types,
        packages = unique(c(filter.method$packages, "data.table"))
      )
    }
  ),

  private = list(
    .calculate = function(task, nfeat) {
      # convert into regr task with Martingale residuals as target
      time = task$target_names[1L]
      status = task$target_names[2L]

      empty = data.table::data.table(
        time = task$data()[[time]],
        status = task$data()[[status]]
      )

      task.empty = TaskSurv$new(
        id = "empty",
        backend = empty,
        time = "time",
        event = "status",
        type = task$censtype
      )

      learner = lrn("surv.coxph")
      learner$train(task.empty)
      residuals = residuals(learner$model, type = "martingale")

      backend = subset(task$data(), select = task$feature_names)
      backend = cbind(backend, target = residuals)

      id = paste(task$id, "martingale", sep = "_")
      new_task = TaskRegr$new(id = id, backend = backend, target = "target")

      # apply filter.method to regr task
      self$filter.method$calculate(new_task, nfeat = nfeat)
      scores = self$filter.method$scores
      return(scores[!is.na(scores)])
    }
  )
)

mlr_filters$add("martingale", FilterMartingale)




FilterSurvLogRankTest = R6Class("FilterSurvLogRankTest",
  inherit = Filter,

  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.log.rank.test",
        packages = "survival",
        feature_types = c("integer", "numeric"),
        task_type = "surv"
      )
    }
  ),

  private = list(
    .calculate = function(task, nfeat) {
      learner = lrn("surv.coxph")
      scores = sapply(task$feature_names, function(nm) {
        tk = task$clone()
        tk$select(nm)
        learner$train(tk)
        score = abs(summary(learner$model)$sctest[1])
        unname(score)
      })
      return(scores)
    }
  )
)

mlr_filters$add("surv.log.rank.test", FilterSurvLogRankTest)



FilterCarSurvScores = R6Class("FilterCarSurvScores",
  inherit = Filter,

  public = list(
    initialize = function() {
      super$initialize(
        id = "car.surv.scores",
        packages = "carSurv",
        feature_types = c("integer", "numeric"),
        task_type = "surv"
      )
    }
  ),

  private = list(
    .calculate = function(task, nfeat) {
      time = task$target_names[1L]
      status = task$target_names[2L]
      obs.time = task$data()[[time]]
      obs.event = task$data()[[status]]

      X = as.matrix(subset(task$data(), select = task$feature_names))
      scores = carSurv::carSurvScore(obsTime = obs.time, obsEvent = obs.event, X = X)
      scores = abs(scores)
      scores = setNames(scores, task$feature_names)
      return(scores)
    }
  )
)

mlr_filters$add("car.surv.scores", FilterCarSurvScores)
