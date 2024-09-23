library(mlr3verse)
library(mlr3mbo)
library(mlr3extralearners)
library(batchtools)
source("tasks.R")
source("learners.R")

if (!fs::dir_exists(fs::path_dir(conf$reg_dir))) {
  fs::dir_create(fs::path_dir(conf$reg_dir))
}

if (fs::dir_exists(conf$reg_dir)) {
  if (config::is_active("production")) {
    cli::cli_abort("Refusing to delete existing registry {.file {fs::path_rel(conf$reg_dir)}} in production mode")
  } else {
    cli::cli_alert_warning("Deleting registry at {.file {fs::path_rel(conf$reg_dir)}}")
    fs::dir_delete(conf$reg_dir)
  }
}

reg <- makeExperimentRegistry(
  file.dir = conf$reg_dir,
  work.dir = here::here(),
  seed = conf$seed
)

tuning_measures <- list(
  twoclass = c("classif.bbrier", "classif.auc"),
  multiclass = c("classif.mbrier", "classif.mauc_aunp")
)

designs <- list()

for (this_property in names(tuning_measures)) {
  for (tuning_measure in tuning_measures[[this_property]]) {
    cli::cli_h2("{.val {this_property}} tasks with {.val {tuning_measure}} measure")

    tuning_measure_short <- stringi::stri_replace_all_fixed(tuning_measure, pattern = "classif.", replacement = "")

    current_task_ids <- task_meta[property == this_property, task_id]
    current_tasks <- tasks[names(tasks) %in% current_task_ids]
    current_resamplings <- resamplings[names(tasks) %in% current_task_ids]

    learners <- make_learners(tuning_measure = tuning_measure)

    design = benchmark_grid(
      tasks = current_tasks,
      learners = learners,
      resamplings = current_resamplings,
      paired = TRUE
    )

    ids = mlr3batchmark::batchmark(
      design =  design,
      store_models = TRUE
    )
    # Tagging with the measure is used to disambiguate jobs with identical learner/task but different
    # tuning measure. Not sure if "cleaner" solution available?
    addJobTags(ids, tuning_measure_short)
    addJobTags(ids, this_property)

    # also tag jobs which have been skipped because they are not wrapped
    # into a AutoTuner (and as such don't differ)
    learners_skipped = mlr3misc::ids(learners)[!mlr3misc::map_lgl(learners, inherits, "AutoTuner")]
    ids = findExperiments(algo.pars = learner_id %in% learners_skipped)
    addJobTags(ids, tuning_measure_short)

  }
}

summarizeExperiments(by = c("task_id", "learner_id"))
tab <- ljoin(unwrap(getJobTable()), task_meta, by = "task_id")
data.table::setkey(tab, job.id)

sample_ids = tab[dim_rank <= 10][, .SD[sample(nrow(.SD), 1)],
                 by = c("task_id", "learner_id", "tags")]
sample_ids[, .N, by = .(task_id, learner_id, tags)]

submitJobs(sample_ids)
