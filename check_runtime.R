# Check runtimes
library(batchtools)
library(ggplot2)

reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir, writeable = FALSE)

# Load task metadata
task_summary <- readRDS("task_summary.rds")

# Get results, cache them

jobs <- unwrap(getJobTable())[, c("job.id", "task_id", "learner_id", "time.running")]
jobs[, time_days := as.numeric(time.running)/60/60/24]
jobs[, time.running := NULL]
jobs[, task_id := gsub(" \\(Supervised Classification\\)", "", gsub("Task \\d+: ", "", task_id))]
jobs[, learner_id := gsub("\\.tuned", "", learner_id)]
jobs[, learner_id := gsub("^(encode\\.)?classif\\.", "", learner_id)]

jobs[, job.id := NULL]
runtimes_avg <- jobs[, .(mean_days = mean(time_days)), keyby = .(learner_id, task_id)]


jobs <- ijoin(jobs, task_summary[, c("task_name", "dim", "n", "p")], by = c("task_id" = "task_name"))

# unique(jobs$learner_id) |> dput()
learner_cols <- c(
  "ranger" = "#2171B5",
  "xgboost" = "#08519C",
  "xgboost_fixdepth" = "#08306B",
  "rpf" = "#F73098",
  "rpf_fixmax" = "#E7298A"
)
# scales::show_col(learner_cols)

jobs[, task_id := sprintf("%s (%d x %d)", task_id, n, p)]

jobs |>
  ggplot(aes(x = reorder(task_id, dim), y = time_days, color = learner_id, fill = learner_id)) +
  geom_boxplot(alpha = .5, position = position_dodge(width = .5), width = 1) +
  coord_flip() +
  scale_y_log10() +
  scale_color_manual(values = learner_cols, aesthetics = c("color", "fill")) +
  labs(
    title = "Job Runtime (on BigBertha)",
    subtitle = "Across all learners and tasks.\nTasks are ordered by n * p, decreasing",
    x = "Task (n x p)", y = "Runtime (days, log10 scale)",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # plot.title.position = "plot",
    legend.position = "bottom"
  )
