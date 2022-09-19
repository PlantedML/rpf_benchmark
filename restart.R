#! /usr/bin/env Rscript

library(batchtools)

# Run with batchtools
reg_name <- "rpf_batchmark"
reg_dir <- here::here("registry", reg_name)

if (dir.exists(reg_dir)) {
  # Comment this line to prevent stored registry deletion on accident
  # unlink(reg_dir, recursive = TRUE)
  loadRegistry(reg_dir, writeable = TRUE)
} else {
  stop("No registry to restart from")
}

cli::cli_h2("Jobs not started yet:")
# Overview of learner IDs
table(unwrap(getJobPars(findNotStarted()))[["learner_id"]])

cli::cli_h2("Jobs expired:")
table(unwrap(getJobPars(findExpired()))[["learner_id"]])

cat("\n")

# Submit
if (grepl("node\\d{2}|bipscluster", system("hostname", intern = TRUE))) {
  ids <- findNotStarted()
  ids[, chunk := chunk(job.id, chunk.size = 50)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = TRUE,
                              ncpus = 1, memory = 6000, walltime = 10*24*3600,
                              max.concurrent.jobs = 200))
} else if (grepl("glogin\\d+", system("hostname", intern = TRUE))) {
  ids <- findErrors()
  ids[, chunk := chunk(job.id, chunk.size = 10)]
  submitJobs(ids = ids, # walltime in seconds, 10 days max, memory in MB
             resources = list(name = reg_name, chunks.as.arrayjobs = FALSE,
                              partition = "large40", ntasks = 10, # ntaskspernode depends on partition
                              max.concurrent.jobs = 8000,
                              #partition = "large40:shared",
                              # medium40:test - 1h walltime
                              # medium40 - 48h walltime
                              ncpus = 1, walltime = 3600*48))
} else {
  # ids_rpf <- findExperiments(algo.pars = learner_id == "classif.rpf.tuned")
  # ids_rpf_fixmax <- findExperiments(algo.pars = learner_id == "classif.rpf_fixmax.tuned")
  # ids_rpf_all <- rbind(ids_rpf, ids_rpf_fixmax)
  # ids_rpf_all <- ijoin(findExpired(), ids_rpf_all)
  # ids_rpf_all <- ijoin(findNotStarted(), ids_rpf_all)

  # submitJobs(ids_rpf_all)
  submitJobs(findExpired())
}

waitForJobs()
getStatus()
