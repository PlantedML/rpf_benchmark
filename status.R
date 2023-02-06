#! /usr/bin/env Rscript
# monitor current status
#

library(batchtools)
library(data.table)

reg_names <- list.files(here::here("registry"))

dt <- rbindlist(lapply(reg_names, function(current_reg) {
  reg_dir <- here::here("registry", current_reg)
  reg <- suppressMessages(loadRegistry(reg_dir, writeable = FALSE, make.default = FALSE))
  tbl <- data.table::as.data.table(getStatus(reg = reg))
  tbl <- tbl[, registry := current_reg]
  tbl <- tbl[, progress := paste0(round(100 * done/defined), "%")]

  setcolorder(tbl, neworder = c("registry", "progress"))
  tbl
}))
knitr::kable(dt)
