# monitor current status
library(batchtools)
reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)
loadRegistry(reg_dir, writeable = FALSE)


# Running -----------------------------------------------------------------
getStatus()
unwrap(getJobTable(findRunning()))


# Done --------------------------------------------------------------------
tbl_done <- unwrap(getJobTable(findDone()))

tbl_done[, c("job.id", "time.running", "task_id", "learner_id")]

tbl_done[, .(count = .N), by = learner_id]


# Error'd -----------------------------------------------------------------
getErrorMessages(findErrors())
