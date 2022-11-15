library(OpenML)
library(mlr3)
library(mlr3oml)

# Cache datasets
options("mlr3oml.cache" = TRUE) # requires install.packages('qs')

# Get CC18 tasks
task_ids <- getOMLStudy('OpenML-CC18')$tasks$task.id

# Show all feature types in set: There are only numeric, integer + factor, no need to select, technically
# sapply(task_ids, \(x) unique(tsk("oml", x)$feature_types$type)) |> unlist() |> unique()

# Get task summary for selection and later display
task_info <- do.call(rbind, lapply(task_ids, function(task_id) {
  task <- tsk("oml", task_id)

  # Clean up name
  task_name <- gsub(pattern = "\\s\\(Supervised\\ Classification\\)", "", task$id)
  task_name <- gsub(pattern = "Task \\d+: ", "", task_name)

  task_n <- task$nrow
  task_p <- length(task$feature_names)

  data.frame(
    task_id = task_id,
    task_name = task_name,
    twoclass = ("twoclass" %in% task$properties),
    featuretypes = all(task$feature_types$type %in% c("integer", "numeric", "factor")), # disallow logical
    nomissing = (max(task$missings()) == 0),
    has_factors = "factor" %in% task$feature_types$type,
    n = task_n,
    p = task_p,
    dim = task_n * task_p
  )
}))

# Select for required properties
# Include multiclass, restrict to non-logical features + no missing data
task_info <- subset(task_info, featuretypes & nomissing)
# task_info <- subset(task_info, twoclass & featuretypes & nomissing)

# Rank & sort by dimensionality
task_info$dim_rank <- rank(task_info$dim)
task_info <- task_info[order(task_info$dim_rank), ]

# Rough heuristic: n * p smaller than 7e5, since 723376 for "bank-marketing" is already v slow
task_info_binary <- subset(task_info, dim <= 7e5 & twoclass)
task_ids_selected <- task_info_binary[["task_id"]]

task_info_multiclass <- subset(task_info, dim <= 1e5 & !twoclass)
task_ids_multiclass <- task_info_multiclass[["task_id"]]

# This object is what counts, as it is used in batchmark.R
tasks <- lapply(task_ids_selected, tsk, .key = "oml")
tasks_multiclass <- lapply(task_ids_multiclass, tsk, .key = "oml")

# Save for later reference
saveRDS(task_info, "task_summary.rds")
