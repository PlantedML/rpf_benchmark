# Ensure current GitHub dependencies
# run renv::restore() to install dependencies as stored in renv.lock
# renv::update() to update dependencies after e.g. push to randomPlantedForest
# renv::snapshot() to record updated versions in renv.lock.

# Please note GitHub / fork dependencies
# renv::install("mlr-org/mlr3batchmark")
# renv::install("PlantedML/mlr3extralearners@rpf")

library(batchtools)
library(mlr3)
library(mlr3batchmark)
library(mlr3tuning)
library(mlr3learners)
library(mlr3extralearners)
library(mlr3pipelines)
source("get_oml_tasks.R")

# Have renv detect learner dependencies
if (FALSE) {
  library(randomPlantedForest) # renv::install("PlantedML/randomPlantedForest")
  library(ranger)
  library(xgboost)
  library(qs) # for oml task caching
}

# Settings
resample_outer <- rsmp("cv", folds = 10)
resample_inner <- rsmp("cv", folds = 5)
mymsr <- msr("classif.auc")
# mytrm <- trm("evals", n_evals = 50) # Trial mode
mytrm <- trm("evals", n_evals = 200)  # Serious mode
mytnr <- tnr("random_search")

auto_tune <- function(learner, .encode = FALSE, ...) {
  search_space <- ps(...)

  if (.encode) {
    learner_graph <- po("encode", method = "treatment") %>>%
      po("learner", learner)
    learner <- as_learner(learner_graph)
  }
  AutoTuner$new(
    learner = learner,
    resampling = resample_inner,
    measure = mymsr,
    search_space = search_space,
    terminator = mytrm,
    tuner = mytnr
  )
}

# ranger
tuned_ranger <- auto_tune(
  learner = lrn("classif.ranger", predict_type = "prob", num.trees = 50),
  #splitrule = p_fct(c("gini", "hellinger")),
  mtry.ratio = p_dbl(0.1, 1),
  min.node.size = p_int(1, 50),
  replace = p_lgl(),
  sample.fraction = p_dbl(0.1, 1)
)

# xgboost
tuned_xgboost <- auto_tune(
  learner = lrn("classif.xgboost", predict_type = "prob",
                nthread = 1 # Just to be safe
                ),
  .encode = TRUE,
  # Need to prefix params with learner id bc of pipeline
  classif.xgboost.max_depth = p_int(1, 20),
  classif.xgboost.subsample = p_dbl(0.1, 1),
  classif.xgboost.colsample_bytree = p_dbl(0.1, 1),
  classif.xgboost.nrounds = p_int(10, 5000),
  classif.xgboost.eta = p_dbl(0, 1)
)

# xgboost: fixed depth as analogue to rpf_fixmax, suggested by Munir
tuned_xgboost_fixdepth <- auto_tune(
  learner = lrn("classif.xgboost", predict_type = "prob",
                nthread = 1, # Just to be safe
                max_depth = 2, id = "classif.xgboost_fixdepth"),
  .encode = TRUE,
  # Need to prefix params with learner id bc of pipeline
  classif.xgboost_fixdepth.subsample = p_dbl(0.1, 1),
  classif.xgboost_fixdepth.colsample_bytree = p_dbl(0.1, 1),
  classif.xgboost_fixdepth.nrounds = p_int(10, 5000),
  classif.xgboost_fixdepth.eta = p_dbl(0, 1)
)

# rpf
tuned_rpf <- auto_tune(
  learner = lrn("classif.rpf", predict_type = "prob",
                id = "classif.rpf",
                # Fixed to 50 for performance
                ntrees = 50,
                # Ensure upper bound as per Joseph
                max_interaction_limit = 30),
  loss = p_fct(c("L1", "exponential")), # removed logit for now
  splits = p_int(10, 50), # Bumped to lower = 10 as per Munir
  split_try = p_int(1, 20),
  t_try = p_dbl(0.1, 1),
  max_interaction_ratio = p_dbl(0, 1)
)

# Fixed max_interaction as suggested by Munir
# Same params as other rpf otherwise
# Need to set ID for disambiguation with other rpf learner!
tuned_rpf_fixmax <- auto_tune(
  learner = lrn("classif.rpf", predict_type = "prob",
                id = "classif.rpf_fixmax",
                ntrees = 50,
                max_interaction = 2),
  loss = p_fct(c("L1", "exponential")), # removed logit for now
  splits = p_int(10, 50),
  split_try = p_int(1, 20),
  t_try = p_dbl(0.1, 1)
)

# Benchmark design
learners <- list(
  tuned_ranger,
  tuned_xgboost,
  tuned_xgboost_fixdepth,
  tuned_rpf,
  tuned_rpf_fixmax
)

design <- benchmark_grid(
  tasks = tasks, # Loaded in get_oml_tasks.R
  learners = learners,
  resamplings = list(resample_outer)
)


# Registry setup ----------------------------------------------------------

reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
if (!dir.exists("registry")) dir.create("registry")

if (dir.exists(reg_dir)) {
  # Comment this line to prevent stored registry deletion on accident
  unlink(reg_dir, recursive = TRUE)
}

reg <- makeExperimentRegistry(reg_dir, seed = 230749)

# Ensure store_models = TRUE to access to tuning archives
batchmark(design, reg = reg, store_models = TRUE)

# Overview of learner IDs
as.data.frame(table(unwrap(getJobPars())[["learner_id"]]))


# Job subselection --------------------------------------------------------

ids_rpf <- findExperiments(algo.pars = learner_id == "classif.rpf.tuned")
ids_rpf_fixmax <- findExperiments(algo.pars = learner_id == "classif.rpf_fixmax.tuned")
ids_xgb <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost.tuned")
ids_xgb_fixdepth <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost_fixdepth.tuned")
ids_ranger <- findExperiments(algo.pars = learner_id == "classif.ranger.tuned")

task_summary <- readRDS("task_summary.rds")

# Tasks with n*p <= kc2, 7th smallest task
small_tasks <- task_summary |> subset(dim <= 10962)

# should have kept original task names in task_summary...
small_task_ids <- sapply(seq_len(nrow(small_tasks)), function(x) {
  sprintf("Task %d: %s (Supervised Classification)", small_tasks[x, "task_id"], small_tasks[x, "task_name"])
})

# Jobs for small tasks
jobs_small_task_ids <- findExperiments(prob.pars = task_id %in% small_task_ids)

# ijoin with rpf/rpf_fixmax jobs
small_jobs_rpf <- ijoin(jobs_small_task_ids, rbind(ids_rpf, ids_rpf_fixmax))


# Submit ------------------------------------------------------------------

if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  #ids <- findNotStarted()
  ids <- small_jobs_rpf
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 400))
# } else if (grepl("glogin\\d+", system("hostname", intern = TRUE))) {
#   ids <- findErrors()
#   ids[, chunk := chunk(job.id, chunk.size = 10)]
#   submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
#              resources = list(name = reg_name, chunks.as.arrayjobs = FALSE,
#                               partition = "large40", ntasks = 10, # ntaskspernode depends on partition
#                               max.concurrent.jobs = 8000,
#                               #partition = "large40:shared",
#                               # medium40:test - 1h walltime
#                               # medium40 - 48h walltime
#                               ncpus = 1, walltime = 3600*48))
} else {
  submitJobs(findNotSubmitted(ids_rpf))
  submitJobs(findNotSubmitted(ids_rpf_fixmax))
}

# to submit only certain algorithms/tasks:
if (FALSE) {
  # only rpf jobs, or only xgboost jobs
  ids_rpf <- findExperiments(algo.pars = learner_id == "classif.rpf.tuned")
  ids_rpf_fixmax <- findExperiments(algo.pars = learner_id == "classif.rpf_fixmax.tuned")
  ids_xgb <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost.tuned")
  ids_xgb_fixdepth <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost_fixdepth.tuned")
  ids_ranger <- findExperiments(algo.pars = learner_id == "classif.ranger.tuned")

  # only a specific task
  ids_wilt <- findExperiments(prob.pars = task_id == "Task 146820: wilt (Supervised Classification)")
  ids_diabetes <- findExperiments(prob.pars = task_id == "Task 37: diabetes (Supervised Classification)")

  # only rpf on one task
  ids_rpf_wilt <- ijoin(ids_wilt, ids_rpf)
  # ids_rpf_diabetes <- ijoin(ids_diabetes, ids_rpf)
  submitJobs(ids = ids_rpf_wilt)

  # See unwrap(getJobPars()) for all task_ids and learner_ids
}

waitForJobs()
getStatus()
