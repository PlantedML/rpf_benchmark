library(batchtools)
library(ggplot2)
library(mlr3)
library(mlr3batchmark)
library(mlr3viz)

mymsr <- msr("classif.auc")

reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir, writeable = TRUE)

# Load task metadata
task_summary <- readRDS("task_summary.rds")

# Get results, cache them
bmr <- reduceResultsBatchmark()
saveRDS(bmr, "bmr.rds")

# Aggregate
aggr <- bmr$aggregate(measures = mymsr)
aggr[, learner_name := gsub("\\.tuned", "", gsub("classif\\.", "", learner_id))]


# Optional: Only tasks for which we have results of all four learners
# completed_tasks <- aggr[, names(which(table(task_id) == 4))]
# aggr <- aggr[task_id %in% completed_tasks, ]
# bmr$filter(task_ids = completed_tasks)

# Plot over individual datasets
scores <- bmr$score(measures = mymsr)
scores[, learner_name := gsub("\\.tuned", "", gsub("classif\\.", "", learner_id))]
scores[, task_name := gsub(" \\(Supervised Classification\\)", "", gsub("Task \\d+: ", "", task_id))]

# Sort task_name levels by n * p from task_summary
scores[, task_name := factor(task_name, levels = task_summary$task_name)]

ggplot(scores, aes(x = learner_name, y = classif.auc, fill = learner_name)) +
  facet_wrap(~ task_name, scales = "free_y") +
  geom_boxplot(alpha = .5) +
  geom_point() +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(
    title = "Scores per task",
    subtitle = "Tasks ordered by n * p, decreasing",
    x = "Learner", y = "AUC"
  ) +
  theme_minimal()
ggsave("bmr.png", width = 12, height = 7, bg = "white")
ggsave("bmr.pdf", width = 12, height = 7)

ggplot(scores, aes(y = learner_name, x = classif.auc, fill = learner_name)) +
  facet_wrap(~ task_name, scales = "free_x") +
  geom_boxplot(alpha = .5) +
  geom_point() +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(
    title = "Scores per task",
    subtitle = "Tasks ordered by n * p",
    x = "AUC", y = "Learner"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(1, "cm")
  )

ggsave("bmr-flip.png", width = 12, height = 7, bg = "white")
ggsave("bmr-flip.pdf", width = 12, height = 7)

# Plot over all datasets
ggplot(aggr, aes(x = learner_name, y = classif.auc, fill = learner_name)) +
  geom_boxplot(alpha = .5) +
  geom_point(position = position_jitterdodge(dodge.width = .25)) +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  labs(
    title = "Aggregated scores over all tasks",
    x = "Learner", y = "AUC"
  ) +
  theme_minimal()

ggsave("aggr.png", width = 12, height = 7, bg = "white")
ggsave("aggr.pdf", width = 12, height = 7)

# Tuning results -------------------------------------------------------------------------------------------------
library(mlr3tuning)

tuning_archive <- extract_inner_tuning_archives(bmr)
tuning_results <- extract_inner_tuning_results(bmr)
