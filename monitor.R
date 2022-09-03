# monitor current status
library(batchtools)
reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir, writeable = FALSE)


# Running -----------------------------------------------------------------
cli::cli_h1("Running")
getStatus()

cat("\n")

tbl_running <- unwrap(getJobTable(findRunning()))
if (nrow(tbl_running) > 0) {
  tbl_running[, c("job.id", "time.running", "task_id", "learner_id")]
  tbl_running[, .(count = .N), by = learner_id]
}

# Done --------------------------------------------------------------------
cli::cli_h1("Done")
tbl_done <- unwrap(getJobTable(findDone()))
tbl_done <- tbl_done[, c("job.id", "time.running", "task_id", "learner_id")]
tbl_done[, .(count = .N), by = learner_id]

cat("\n")

# Error'd -----------------------------------------------------------------
cli::cli_h1("Errors")
getErrorMessages(findErrors())
