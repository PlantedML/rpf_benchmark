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
mymsr <- msr("classif.mbrier")
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
  tasks = tasks_multiclass, # Loaded in get_oml_tasks.R
  learners = learners,
  resamplings = list(resample_outer)
)


# Registry setup ----------------------------------------------------------

reg_name <- "rpf_batchmark_multiclass_brier"
reg_dir <- here::here("registry", reg_name)
# Comment this line to prevent stored registry deletion on accident
# unlink(reg_dir, recursive = TRUE)

# "registry" holds the registries, must be ensured to exist
if (!dir.exists("registry")) dir.create("registry")

if (dir.exists(reg_dir)) { # if current registry exists, we continue on
  loadRegistry(reg_dir, writeable = TRUE)
} else { # If registry doesn't exist yet: make registry and batchmark
  reg <- makeExperimentRegistry(reg_dir, seed = 230749)
  # Ensure store_models = TRUE to access to tuning archives
  batchmark(design, reg = reg, store_models = TRUE)
}

# Overview of learner IDs
as.data.frame(table(unwrap(getJobPars())[["learner_id"]]))

# Job subselection --------------------------------------------------------

ids_rpf <- findExperiments(algo.pars = learner_id == "classif.rpf.tuned")
ids_rpf_fixmax <- findExperiments(algo.pars = learner_id == "classif.rpf_fixmax.tuned")
ids_xgb <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost.tuned")
ids_xgb_fixdepth <- findExperiments(algo.pars = learner_id == "encode.classif.xgboost_fixdepth.tuned")
ids_ranger <- findExperiments(algo.pars = learner_id == "classif.ranger.tuned")

# Get full overview of tasks and select multiclass with n*p <= 100.000
task_summary <- readRDS("task_summary.rds")
small_tasks <- subset(task_summary, dim <= 1e5 & !twoclass)

# Job IDs for small tasks
jobs_small_task_ids <- findExperiments(prob.pars = task_id %in% small_tasks$task_name_full)
small_jobs_rpf <- ijoin(jobs_small_task_ids, rbind(ids_rpf, ids_rpf_fixmax))

# Submit ------------------------------------------------------------------

if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  #ids <- findNotStarted()
  ids <- jobs_small_task_ids
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 400))
} else {
  # small tasks rpf first
  submitJobs(findNotSubmitted(small_jobs_rpf))
  # then everything else that's not done already
  submitJobs(findNotDone(jobs_small_task_ids))
}

waitForJobs()
getStatus()
