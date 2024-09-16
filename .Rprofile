source("renv/activate.R")
# Circumvent srcref issue https://github.com/rstudio/renv/issues/1713
options("install.opts" = "--without-keep.source")

config_profile = Sys.getenv('R_CONFIG_ACTIVE', 'default')
cli::cli_alert_info("Loading config {.val {config_profile}}")
conf <- config::get()


Sys.setenv(OMP_NUM_THREADS="1")
Sys.setenv(OPENBLAS_NUM_THREADS="1")
Sys.setenv(MKL_NUM_THREADS="1")

options(
  datatable.print.class = TRUE,
  datatable.print.keys = TRUE,
  batchtools.progress = FALSE # for speedup
)
