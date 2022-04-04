
library(batchtools)
library(ggplot2)
library(mlr3)
library(mlr3batchmark)
library(mlr3viz)
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

# ranger
search_space_ranger = ps(
  #splitrule = p_fct(c("gini", "hellinger")),
  mtry.ratio = p_dbl(0.1, 1),
  min.node.size = p_int(1, 50),
  replace = p_lgl(),
  sample.fraction = p_dbl(0.1, 1)
)
tuned_ranger <- AutoTuner$new(
  learner = lrn("classif.ranger", predict_type = "prob", num.trees = 50),
  resampling = resample_inner,
  measure = mymsr,
  search_space = search_space_ranger,
  terminator = mytrm,
  tuner = mytnr
)

# xgboost
search_space_xgboost = ps(
  max_depth = p_int(1, 20),
  subsample = p_dbl(0.1, 1),
  colsample_bytree = p_dbl(0.1, 1),
  nrounds = p_int(10, 5000),
  eta = p_dbl(0, 1)
)
tuned_xgboost <- AutoTuner$new(
  learner = lrn("classif.xgboost", predict_type = "prob"),
  resampling = resample_inner,
  measure = mymsr,
  search_space = search_space_xgboost,
  terminator = mytrm,
  tuner = mytnr
)

# rpf
search_space_rpf = ps(
  splits = p_int(1, 50),
  split_try = p_int(1, 20),
  t_try = p_dbl(0.1, 1),
  max_interaction = p_int(1, 30)
)
tuned_rpf <- AutoTuner$new(
  learner = lrn("classif.rpf", predict_type = "prob", ntrees = 50),
  resampling = resample_inner,
  measure = mymsr,
  search_space = search_space_rpf,
  terminator = mytrm,
  tuner = mytnr
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
unlink("./registry", recursive = TRUE)
makeExperimentRegistry()
batchmark(design)

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


waitForJobs()
getStatus()

bmr <- reduceResultsBatchmark()
saveRDS(bmr, "bmr.Rds")

# Aggregate
aggr <- bmr$aggregate(measures = mymsr)

# Only tasks for which we have results of all three learners
completed_tasks <- aggr[, names(which(table(task_id) == 3))]
aggr <- aggr[task_id %in% completed_tasks, ]

# Plot over individual datasets
bmr$filter(task_ids = completed_tasks)
scores <- bmr$score(measures = mymsr)
scores[, learner_name := gsub("\\.tuned", "", gsub("classif\\.", "", learner_id))]
scores[, task_name := gsub(" \\(Supervised Classification\\)", "", gsub("Task \\d+: ", "", task_id))]
ggplot(scores, aes(x = learner_name, y = classif.auc)) +
  facet_wrap(~ task_name) +
  geom_boxplot() +
  geom_point() +
  theme_bw()
ggsave("bmr.pdf")

autoplot(bmr, measure = mymsr) +
  theme_bw()
ggsave("bmr.pdf")

# Plot over all datasets
aggr[, learner_name := gsub("\\.tuned", "", gsub("classif\\.", "", learner_id))]
ggplot(aggr, aes(x = learner_name, y = classif.auc)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()
ggsave("aggr.pdf")
