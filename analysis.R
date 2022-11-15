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
if (file.exists("bmr.rds")) {
  bmr <- readRDS("bmr.rds")
} else {
  bmr <- reduceResultsBatchmark()
  saveRDS(bmr, "bmr.rds")
}

# Aggregate
aggr <- bmr$aggregate(measures = mymsr)
aggr[, learner_name := gsub("\\.tuned", "", gsub("(encode\\.)?classif\\.", "", learner_id))]


# Plot over individual datasets
scores <- bmr$score(measures = mymsr)
scores[, learner_name := gsub("\\.tuned", "", gsub("(encode\\.)?classif\\.", "", learner_id))]
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

# tuning archive: all tested hpc's
tuning_archive <- extract_inner_tuning_archives(bmr)

# params_rpf <- c("loss", "splits", "split_try", "t_try", "max_interaction_ratio")
# params_rpf_fixmax <- c("loss", "splits", "split_try", "t_try")

tuning_archive_rpf <- tuning_archive[learner_id == "classif.rpf.tuned", ]
tuning_archive_rpf <- tuning_archive_rpf[, c("loss", "splits", "split_try", "t_try", "max_interaction_ratio", "learner_id", "task_id", "classif.auc", "runtime_learners", "batch_nr", "experiment", "iteration")]

tuning_archive_rpf_fixmax <- tuning_archive[learner_id == "classif.rpf_fixmax.tuned", ]
tuning_archive_rpf_fixmax <- tuning_archive_rpf_fixmax[, c("loss", "splits", "split_try", "t_try", "learner_id", "task_id", "classif.auc", "runtime_learners", "batch_nr", "experiment", "iteration")]

saveRDS(tuning_archive_rpf, "tuning_archive_rpf.rds")
saveRDS(tuning_archive_rpf_fixmax, "tuning_archive_rpf_fixmax.rds")

# tuning results: only best configuration
tuning_results <- extract_inner_tuning_results(bmr)

tuning_results_rpf <- tuning_results[learner_id == "classif.rpf.tuned", ]
tuning_results_rpf <- tuning_results_rpf[, c("loss", "splits", "split_try", "t_try", "max_interaction_ratio", "learner_id", "task_id", "classif.auc", "experiment", "iteration")]

tuning_results_rpf_fixmax <- tuning_results[learner_id == "classif.rpf_fixmax.tuned", ]
tuning_results_rpf_fixmax <- tuning_results_rpf_fixmax[, c("loss", "splits", "split_try", "t_try", "learner_id", "task_id", "classif.auc", "experiment", "iteration")]

saveRDS(tuning_results_rpf, "tuning_results_rpf.rds")
saveRDS(tuning_results_rpf_fixmax, "tuning_results_rpf_fixmax.rds")
