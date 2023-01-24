#! /usr/bin/env Rscript
# monitor current status
#

# infer registry name from cli args
argv <- commandArgs(TRUE)

library(batchtools)

if (length(argv) == 0) {
  # If no argument, list all registries
  reg_name <- list.files(here::here("registry"))
} else {
  reg_name <- argv[[1]]
  checkmate::assert_directory_exists(here::here("registry", reg_name))
}

for (current_reg in reg_name) {

  reg_dir <- here::here("registry", current_reg)
  reg <- loadRegistry(reg_dir, writeable = FALSE, make.default = FALSE)

  cli::cli_h1("Current Status: {current_reg}")
  print(getStatus(reg = reg))
  cat("\n")

  # Running -----------------------------------------------------------------
  cli::cli_h1("Running: {current_reg}")

  tbl_running <- unwrap(getJobTable(findRunning(reg = reg), reg = reg))
  if (nrow(tbl_running) > 0) {
    tbl_running[, c("job.id", "time.running", "task_id", "learner_id")]
    print(tbl_running[, .(count = .N), by = learner_id])
  }

  # Done --------------------------------------------------------------------
  cli::cli_h1("Done: {current_reg}")
  tbl_done <- unwrap(getJobTable(findDone(reg = reg), reg = reg))
  if (nrow(tbl_done) > 0) {
    tbl_done <- tbl_done[, c("job.id", "time.running", "task_id", "learner_id")]
    print(tbl_done[, .(count = .N), by = learner_id])
  }

  cat("\n")

  # Expired -----------
  cli::cli_h1("Expired: {current_reg}")

  tbl_expired <- unwrap(getJobTable(findExpired(reg = reg), reg = reg))
  if (nrow(tbl_expired) > 0) {
    tbl_expired <- tbl_expired[, c("job.id", "time.running", "task_id", "learner_id")]
    print(tbl_expired[, .(count = .N), by = learner_id])
  }

  cat("\n")

  # Error'd -----------------------------------------------------------------
  cli::cli_h1("Errors: {current_reg}")
  print(getErrorMessages(findErrors(reg = reg), reg = reg))

}
