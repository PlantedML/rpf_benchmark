library(batchtools)
library(ggplot2)
library(mlr3)
library(mlr3batchmark)
library(mlr3viz)
library(mlr3tuning)
library(data.table)

if (!dir.exists("data")) dir.create("data")
if (!dir.exists("plots")) dir.create("plots")

# Get results, cache them
if (file.exists("data/bmr-binary.rds")) {

  bmr <- readRDS("data/bmr-binary.rds")

} else {
  reg_name <- "rpf_batchmark"
  reg_dir <- here::here("registry", reg_name)
  loadRegistry(reg_dir, writeable = TRUE)
  tictoc::tic()
  bmr <- reduceResultsBatchmark()
  tictoc::toc()

  # Tuning results -------------------------------------------------------------------------------------------------
  # Extracting tuning results first and storing them so we can remove models from bmr before saving to save time

  # tuning archive: all tested hpc's
  tictoc::tic()
  tuning_archive <- extract_inner_tuning_archives(bmr)
  tictoc::toc()

  # Only rpf/rpf_fixmax tuning results
  tuning_archive_rpf <- tuning_archive[learner_id %in% c("classif.rpf.tuned", "classif.rpf_fixmax.tuned"), ]
  # Remove columns for params of other learners
  tuning_archive_rpf <- tuning_archive_rpf[, which(unlist(lapply(tuning_archive_rpf, function(x) !all(is.na(x))))), with = FALSE]
  # remove redundant x_domain columns since we don't have transformations
  tuning_archive_rpf <- tuning_archive_rpf[, which(!startsWith(names(tuning_archive_rpf), "x_domain")), with = FALSE]
  # remove original resampling results to save space
  tuning_archive_rpf[, resample_result := NULL]

  saveRDS(tuning_archive_rpf, "data/tuning_archive_rpf-binary.rds")

  # tuning results: only best configuration
  tictoc::tic()
  tuning_results <- extract_inner_tuning_results(bmr)
  tictoc::toc()

  # Only rpf/rpf_fixmax tuning results
  tuning_results_rpf <- tuning_results[learner_id %in% c("classif.rpf.tuned", "classif.rpf_fixmax.tuned"), ]
  # Remove columns for params of other learners
  tuning_results_rpf <- tuning_results_rpf[, which(unlist(lapply(tuning_results_rpf, function(x) !all(is.na(x))))), with = FALSE]
  # remove redundant x_domain columns since we don't have transformations
  tuning_results_rpf <- tuning_results_rpf[, which(!startsWith(names(tuning_results_rpf), "x_domain")), with = FALSE]

  saveRDS(tuning_results_rpf, "data/tuning_results_rpf-binary.rds")

  # Now that tuning archives/results are stored, we can remove models from the bmr object
  bmr$discard(models = TRUE)
  saveRDS(bmr, "data/bmr-binary.rds")
}


# Results: Aggregated ---------------------------------------------------------------------------------------------

mymsr <- msrs(c("classif.auc", "classif.bbrier"))
# Define learner colors for somewhat identifiable plots
learner_cols <- c(
  "ranger" = "#2171B5",
  "xgboost" = "#3BA99C",
  "xgboost_fixdepth" = "#256A62",
  "rpf" = "#F73098",
  "rpf_fixmax" = "#CA1694"
)

aggr <- bmr$aggregate(measures = mymsr)
aggr[, learner_name := gsub("\\.tuned", "", gsub("(encode\\.)?classif\\.", "", learner_id))]

aggr_long <- melt(aggr, measure.vars = c("classif.auc", "classif.bbrier"), variable.name = "measure")
aggr_long[, measure := gsub("^classif\\.", "", measure)]

ggplot(aggr_long, aes(x = learner_name, y = value, fill = learner_name)) +
  facet_wrap(vars(measure), ncol = 2, scales = "free_y") +
  geom_boxplot(alpha = .5) +
  geom_point(position = position_jitterdodge(dodge.width = .25)) +
  #scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  coord_cartesian(ylim = c(0, 1)) +
  coord_flip() +
  scale_fill_manual(values = learner_cols, guide = "none") +
  labs(
    title = "Binary Classification: Aggregated scores over all tasks",
    x = "Learner", y = "Score"
  ) +
  theme_minimal(base_size = 14)

ggsave("plots/binary-aggr.png", width = 12, height = 7, bg = "white")
ggsave("plots/binary-aggr.pdf", width = 12, height = 7)


# Results: Per Task ------------------------------------------------------------------------------------------------
# Load task metadata
task_summary <- as.data.table(readRDS("task_summary.rds"))

scores <- bmr$score(measures = msrs(c("classif.auc", "classif.bbrier")))
scores[, learner_name := gsub("\\.tuned", "", gsub("(encode\\.)?classif\\.", "", learner_id))]
scores[, c("task", "learner", "resampling", "prediction", "resampling_id") := NULL]
scores <- scores[task_summary[, -c("task_id")], on = c("task_id" = "task_name_full"), nomatch = 0]

# Sort task_name levels by n * p from task_summary
scores[, task_name := factor(task_name, levels = task_summary$task_name)]
scores_long <- melt(scores, measure.vars = c("classif.auc", "classif.bbrier"), variable.name = "measure")


ggplot(scores, aes(x = learner_name, y = classif.auc, fill = learner_name)) +
  facet_wrap(vars(task_name), ncol = 4) +
  geom_boxplot(alpha = .5) +
  geom_point() +
  scale_fill_manual(values = learner_cols, guide = "none") +
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Scores per task",
    subtitle = "Tasks ordered by n * p, decreasing",
    x = "Learner", y = "AUC (%)"
  ) +
  theme_minimal()

ggsave("plots/binary-pertask-auc.png", width = 12, height = 7, bg = "white")
ggsave("plots/binary-pertask-auc.pdf", width = 12, height = 7)

ggplot(scores, aes(y = learner_name, x = classif.auc, fill = learner_name)) +
  facet_wrap(vars(task_name)) +
  geom_boxplot(alpha = .5) +
  geom_point() +
  scale_fill_manual(values = learner_cols, guide = "none") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Binary classification scores per task",
    subtitle = "Tasks ordered by n * p, decreasing",
    y = "Learner", x = "AUC (%)"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(1, "cm")
  )

ggsave("plots/binary-pertask-auc-flip.png", width = 12, height = 7, bg = "white")
ggsave("plots/binary-pertask-auc-flip.pdf", width = 12, height = 7)

ggplot(scores, aes(y = learner_name, x = classif.bbrier, fill = learner_name)) +
  facet_wrap(vars(task_name)) +
  geom_boxplot(alpha = .5) +
  geom_point() +
  scale_fill_manual(values = learner_cols, guide = "none") +
  scale_x_continuous(labels = scales::percent) +
  labs(
    title = "Binary classification scores per task",
    subtitle = "Tasks ordered by n * p, decreasing",
    y = "Learner", x = "Brier Score (%)"
  ) +
  theme_minimal() +
  theme(
    panel.spacing.x = unit(1, "cm")
  )

ggsave("plots/binary-pertask-brier-flip.png", width = 12, height = 7, bg = "white")
ggsave("plots/binary-pertask-brier-flip.pdf", width = 12, height = 7)

# Attempting to plot both AUC and Brier in one have not been great
# ggplot(scores_long, aes(x = task_name, y = value, fill = learner_name)) +
#   coord_flip() +
#   facet_grid(cols = vars(measure), scales = "free_x") +
#   scale_y_continuous(labels = scales::percent) +
#   geom_boxplot() +
#   theme_minimal() +
#   theme(
#     legend.position = "top",
#     panel.spacing.x = unit(1, "cm")
#   )
