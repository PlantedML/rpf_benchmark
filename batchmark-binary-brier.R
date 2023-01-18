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
source(here::here("settings.R"))
mymsr <- msr("classif.bbrier")

auto_tune <- function(learner, .encode = FALSE, ...) {
  search_space <- ps(...)

  if (.encode) {
    learner_graph <- po("encode", method = "treatment") %>>%
      po("learner", learner)
    learner <- as_learner(learner_graph)
  }
  AutoTuner$new(
    learner = learner,
    resampling = rsmp("cv", folds = inner_folds),
    measure = mymsr,
    search_space = search_space,
    terminator = trm("evals", n_evals = tuning_budget),
    tuner = tnr("random_search")
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
                ntrees = rpf.ntrees,
                # Ensure upper bound as per Joseph
                max_interaction_limit = rpf.maxintlim),
  loss = rpf.loss,
  splits = rpf.splits,
  split_try = rpf.split_try,
  t_try = rpf.t_try,
  max_interaction_ratio = rpf.maxintratio
)

# Fixed max_interaction as suggested by Munir
# Same params as other rpf otherwise
# Need to set ID for disambiguation with other rpf learner!
tuned_rpf_fixmax <- auto_tune(
  learner = lrn("classif.rpf", predict_type = "prob",
                id = "classif.rpf_fixmax",
                ntrees = rpf.ntrees,
                max_interaction = 2),
  loss = rpf.loss,
  splits = rpf.splits,
  split_try = rpf.split_try,
  t_try = rpf.t_try,
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
  tasks = tasks_binary, # Loaded in get_oml_tasks.R
  learners = learners,
  resamplings = list(rsmp("cv", folds = outer_folds))
)


# Registry setup ----------------------------------------------------------

reg_name <- "rpf_batchmark_binary_brier"
reg_dir <- here::here("registry", reg_name)
# Comment this line to prevent stored registry deletion on accident
# unlink(reg_dir, recursive = TRUE)

if (dir.exists(reg_dir)) { # if current registry exists, we continue on

  loadRegistry(reg_dir, writeable = TRUE)

} else { # If registry doesn't exist yet: make registry and batchmark

  reg <- makeExperimentRegistry(reg_dir, seed = global_seed)

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

# Submit ------------------------------------------------------------------

if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  #ids <- findNotStarted()
  ids <- small_jobs_rpf
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 400))
} else {
  # first rpf, the rest
  submitJobs(ids_rpf)
  submitJobs(ids_rpf_fixmax)
  # then everything else that's not done already
  submitJobs(findNotDone())
}

waitForJobs()
getStatus()
