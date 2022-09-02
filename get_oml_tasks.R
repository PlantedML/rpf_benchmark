library(OpenML)
library(mlr3)
library(mlr3oml)

# Cache datasets
options("mlr3oml.cache" = TRUE) # requires install.packages('qs')

# Get CC18 tasks
task_ids <- getOMLStudy('OpenML-CC18')$tasks$task.id

# Select only binary classification with numeric input
task_info <- sapply(task_ids, function(task_id) {
  task <- tsk("oml", task_id)
  c(twoclass = ("twoclass" %in% task$properties),
    numerics = all(task$feature_types$type %in% c("integer", "numeric")),
    nomissing = (max(task$missings()) == 0))
})
task_ids_selected <- task_ids[task_info[1, ] & task_info[2, ] & task_info[3, ]]

#dput(task_ids_selected)
# task_ids_selected <- c(37L, 43L, 3902L, 3903L, 3913L, 3917L, 3918L, 9910L, 9946L,
#                        9952L, 9957L, 9976L, 9978L, 10093L, 10101L, 146819L, 146820L,
#                        167120L)

#tasks <- lapply(task_ids_selected[c(1, 3, 5)], tsk, .key = "oml")
tasks <- lapply(task_ids_selected, tsk, .key = "oml")

task_summary <- do.call(rbind, lapply(tasks, function(tsk) {
  data.frame(
    task_name = tsk$id,
    n = tsk$nrow,
    p = length(tsk$feature_names)
  )
}))
