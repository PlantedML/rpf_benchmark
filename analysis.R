library(batchtools)
library(ggplot2)
library(mlr3)
library(mlr3batchmark)
library(mlr3viz)

reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir)

# Get results, cache them
bmr <- reduceResultsBatchmark()
saveRDS(bmr, "bmr.rds")

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

# autoplot(bmr, measure = mymsr) +
#   theme_bw()
# ggsave("bmr.pdf")

# Plot over all datasets
aggr[, learner_name := gsub("\\.tuned", "", gsub("classif\\.", "", learner_id))]
ggplot(aggr, aes(x = learner_name, y = classif.auc)) +
  geom_boxplot() +
  geom_point() +
  theme_bw()
ggsave("aggr.pdf")

