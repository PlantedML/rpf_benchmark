# Ensure current GitHub dependencies
remotes::install_github("PlantedML/randomPlantedForest")
# mlr3xtralearners fork with branch for rpf learner
remotes::install_github("PlantedML/mlr3extralearners@rpf")
# Not on CRAN
remotes::install_github("mlr-org/mlr3batchmark")

library(batchtools)
library(mlr3)
library(mlr3batchmark)
library(mlr3tuning)
library(mlr3learners)
library(mlr3extralearners)
source("get_oml_tasks.R")

# Settings
resample_outer <- rsmp("cv", folds = 5)
resample_inner <- rsmp("cv", folds = 3)
mymsr <- msr("classif.auc")
mytrm <- trm("evals", n_evals = 50) # 200
mytnr <- tnr("random_search")

auto_tune <- function(learner, ...) {
  AutoTuner$new(
    learner = learner,
    resampling = resample_inner,
    measure = mymsr,
    search_space = ps(...),
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
  learner = lrn("classif.xgboost", predict_type = "prob"),
  max_depth = p_int(1, 20),
  subsample = p_dbl(0.1, 1),
  colsample_bytree = p_dbl(0.1, 1),
  nrounds = p_int(10, 5000),
  eta = p_dbl(0, 1)
)


# rpf
tuned_rpf <- auto_tune(
  learner = lrn("classif.rpf", predict_type = "prob", ntrees = 50),
  loss = p_fct(c("L1", "L2", "logit", "exponential")),
  splits = p_int(1, 50),
  split_try = p_int(1, 20),
  t_try = p_dbl(0.1, 1),
  max_interaction = p_int(1, 30)
)

# Benchmark design
learners <- list(tuned_ranger,
                 tuned_xgboost,
                 tuned_rpf)
design <- benchmark_grid(tasks = tasks,
                         learners = learners,
                         resamplings = list(resample_outer))

# Run with batchtools
reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)

if (dir.exists(reg_dir)) {
  loadRegistry(reg_dir, writeable = TRUE)
  # unlink(reg_dir)
} else {
  dir.create(here::here("registry"))
  reg = makeExperimentRegistry(reg_dir, seed = 230749)
}

batchmark(design, reg = reg)

# Submit
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 200))
} else if (grepl("glogin\\d+", system("hostname", intern = TRUE))) {
  ids <- findErrors()
  ids[, chunk := chunk(job.id, chunk.size = 10)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = FALSE,
                              partition = "large40", ntasks = 10, # ntaskspernode depends on partition
                              max.concurrent.jobs = 8000,
                              #partition = "large40:shared",
                              # medium40:test - 1h walltime
                              # medium40 - 48h walltime
                              ncpus = 1, walltime = 3600*48))
} else {
  submitJobs()
}


# to submit only certain algorithms/tasks:
if (FALSE) {
  # only rpf jobs, or only xgboost jobs
  ids_rpf <- findExperiments(algo.pars = learner_id == "classif.rpf.tuned")
  ids_xgb <- findExperiments(algo.pars = learner_id == "classif.xgboost.tuned")
  # only a specific task
  ids_wilt <- findExperiments(prob.pars = task_id == "Task 146820: wilt (Supervised Classification)")
  ids_diabetes <- findExperiments(prob.pars = task_id == "Task 37: diabetes (Supervised Classification)")
  
  # only rpf on diabetes
  ids_rpf_diabetes <- ijoin(ids_diabetes, ids_rpf)
  submitJobs(ids = ids_diabetes)
  
  # See unwrap(getJobPars()) for all task_ids and learner_ids
}

waitForJobs()
getStatus()
