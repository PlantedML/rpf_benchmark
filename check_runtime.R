# Check runtimes
library(batchtools)
library(ggplot2)

reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir, writeable = FALSE)

# Load task metadata
task_summary <- readRDS("task_summary.rds")

# Get results
jobs <- unwrap(getJobTable())[, c("job.id", "task_id", "learner_id", "time.running")]

jobs[, time_days := as.numeric(time.running)/60/60/24]
jobs[, time.running := NULL]
jobs[, task_id := gsub(" \\(Supervised Classification\\)", "", gsub("Task \\d+: ", "", task_id))]
jobs[, learner_id := gsub("\\.tuned", "", learner_id)]
jobs[, learner_id := gsub("^(encode\\.)?classif\\.", "", learner_id)]

#jobs[, job.id := NULL]
runtimes_avg <- jobs[, .(mean_days = mean(time_days)), keyby = .(learner_id, task_id)]

jobs <- ijoin(jobs, task_summary[, c("task_name", "dim", "n", "p")], by = c("task_id" = "task_name"))

# Define learner colors for somewhat identifiable plots
learner_cols <- c(
  "ranger" = "#2171B5",
  "xgboost" = "#3BA99C",
  "xgboost_fixdepth" = "#256A62",
  "rpf" = "#F73098",
  "rpf_fixmax" = "#CA1694"
)
# scales::show_col(learner_cols)

jobs[, task_id := sprintf("%s (%d x %d)", task_id, n, p)]

jobs |>
  # subset(dim > 24195) |>
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

jobs |>
  # subset(dim > 24195) |>
  ggplot(aes(x = reorder(task_id, dim), y = time_days, color = learner_id, fill = learner_id)) +
  geom_boxplot(alpha = .5, position = position_dodge(width = .5), width = 1) +
  coord_flip() +
  scale_color_manual(values = learner_cols, aesthetics = c("color", "fill")) +
  labs(
    title = "Job Runtime (on BigBertha)",
    subtitle = "Across all learners and tasks.\nTasks are ordered by n * p, decreasing",
    x = "Task (n x p)", y = "Runtime (days)",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # plot.title.position = "plot",
    legend.position = "bottom"
  )

jobs |>
  ggplot(aes(x = dim, y = time_days, color = learner_id, fill = learner_id)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, span = 1.5) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_manual(values = learner_cols, aesthetics = c("color", "fill")) +
  labs(
    title = "Job Runtime (on BigBertha) and Task Size",
    subtitle = "Across all learners and tasks",
    x = "Task Dimensionanility (n x p, log10 scale)", y = "Runtime (days, log10 scale)",
    color = NULL, fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # plot.title.position = "plot",
    legend.position = "bottom"
  )

# Relative runtimes might be interesting
job_median_wide <- data.table::dcast(jobs, task_id + dim + n + p ~ learner_id, value.var = "time_days", fun.aggregate = median)
data.table::setorder(job_median_wide, -dim)

job_median_wide[, rpf_xgb := rpf/xgboost]
job_median_wide[, rpf_ranger := rpf/ranger]
job_median_wide[, xgb_ranger := xgboost/ranger]

# Predict stuff? ----------------------------------------------------------

jobs <- jobs[learner_id %in% c("rpf", "rpf_fixmax")]
jobs_done <- jobs[!is.na(time_days)]
jobs_todo <- jobs[is.na(time_days)]

mod <- lm(log10(time_days) ~ learner_id + n * p, data = jobs_done)
jobs$predicted_time <- 10^(predict(mod, jobs))

jobs |>
  data.table::melt(id = c("task_id", "learner_id", "dim"), measure = c("time_days", "predicted_time"), variable.name = "type") |>
  ggplot(aes(x = reorder(task_id, dim), y = value, color = type, fill = type, shape = learner_id)) +
  geom_point(size = 3, stroke = .5, alpha = 1/3) +
  coord_flip() +
  scale_y_log10() +
  #scale_color_manual(values = learner_cols, aesthetics = c("color", "fill")) +
  labs(
    title = "Estimated Job Runtime (on BigBertha)",
    subtitle = "Across rpf learners and all tasks.\nTasks are ordered by n * p, decreasing",
    x = "Task (n x p)", y = "Runtime (days, log10 scale)",
    color = NULL, fill = NULL, shape = "Learner"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    # plot.title.position = "plot",
    legend.position = "bottom"
  )
