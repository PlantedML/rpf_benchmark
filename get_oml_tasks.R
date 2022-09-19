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
    n = task_n,
    p = task_p,
    dim = task_n * task_p
  )
}))

# Select for required properties
task_info <- subset(task_info, twoclass & featuretypes & nomissing)

# Rank & sort by dimensionality
task_info$dim_rank <- rank(task_info$dim)
task_info <- task_info[order(task_info$dim_rank), ]

# Rough heuristic: n * p smaller than 1.000.000, gets 23 tasks in this case.
task_info <- subset(task_info, dim <= 1e6)
task_ids_selected <- task_info[["task_id"]]

# This object is what counts, as it is used in batchmark.R
tasks <- lapply(task_ids_selected, tsk, .key = "oml")

# Save for later reference
saveRDS(task_info, "task_summary.rds")
