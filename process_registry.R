#' Process batchtools registry and store pruned results
#'
#' Preprocessing the batchtools registry to create intermediate result datasets
#' for further analysis and display.
#'
#' - Creates bmr with `reduceResultsBatchmark()` and gets tuning archives
#' with `extract_inner_tuning_archives()` and `_results()`
#' - removes stored models from bmr after extraction
#' - Saves bmr and archive/results as .rds in `data/`
#' - File path includes binary/multiclass and tuning measure as provided to avoid ambiguity
#'
#' @param reg_name Name of batchtools registry to use, e.g. `"rpf_batchmark_binary_auc"`.
#'        Will be read with `writeable = TRUE`!
#' @param target_type One of `c("binary", "multiclass")`.
#' @param tuning_measure One of `c("auc", "brier")`.
#' @param data Path to folder for data saving, `here::here("data")`.
#'        Will be created if it does not exist.
process_registry <- function(
    reg_name,
    target_type,
    tuning_measure,
    data_dir = here::here("data")) {

  checkmate::assert_directory_exists(here::here("registry", reg_name))
  if (!dir.exists(data_dir)) {
    cli::cli_inform("Creating {.path {data_dir}}")
    dir.create(data_dir)
  }

  target_type <- checkmate::assert_choice(target_type, choices = c("binary", "multiclass"))
  tuning_measure <- checkmate::assert_choice(tuning_measure, choices = c("auc", "brier"))

  # Check to see if provided registry path matches target and measure provided
  guessed_registry <- glue::glue("rpf_batchmark_{target_type}_{tuning_measure}")
  checkmate::assert(guessed_registry == reg_name)

  bmr_path <- file.path(data_dir, glue::glue("bmr-{target_type}-{tuning_measure}.rds"))
  tuning_archive_path <- file.path(data_dir, glue::glue("tuning_archive_rpf-{target_type}-{tuning_measure}.rds"))
  tuning_results_path <- file.path(data_dir, glue::glue("tuning_results_rpf-{target_type}-{tuning_measure}.rds"))

  if (file.exists(bmr_path)) {
    cli::cli_alert_warning("{.path {bmr_path}} already exists, not overwriting.")

  } else {
    reg_dir <- here::here("registry", reg_name)
    cli::cli_alert_info("Loading registry at {.path {reg_dir}}")
    suppressMessages(batchtools::loadRegistry(reg_dir, writeable = TRUE))
    cli::cli_alert_info("Running {.fn reduceResultsBatchmark}...")
    t1 <- tictoc::tic()
    bmr <- mlr3batchmark::reduceResultsBatchmark()
    t2 <- tictoc::toc()
    cli::cli_alert_info("{.fn reduceResultsBatchmark}: {t2$callback_msg}")


    # Tuning results -------------------------------------------------------------------------------------------------
    # Extracting tuning results first and storing them so we can remove models from bmr before saving to save time

    # tuning archive: all tested hpc's
    if (file.exists(tuning_archive_path)) {
      cli::cli_alert_warning("{.path {tuning_archive_path}} already exists, not overwriting.")
    } else {
      cli::cli_alert_info("Running {.fn extract_inner_tuning_archives}")
      t1 <- tictoc::tic()
      tuning_archive <- mlr3tuning::extract_inner_tuning_archives(bmr)
      t2 <- tictoc::toc()
      cli::cli_alert_info("{.fn extract_inner_tuning_archives}: {t2$callback_msg}")

      # Only rpf/rpf_fixmax tuning results
      tuning_archive_rpf <- tuning_archive[learner_id %in% c("classif.rpf.tuned", "classif.rpf_fixmax.tuned"), ]
      # Remove columns for params of other learners
      tuning_archive_rpf <- tuning_archive_rpf[, which(unlist(lapply(tuning_archive_rpf, function(x) !all(is.na(x))))), with = FALSE]
      # remove redundant x_domain columns since we don't have transformations
      tuning_archive_rpf <- tuning_archive_rpf[, which(!startsWith(names(tuning_archive_rpf), "x_domain")), with = FALSE]
      # remove original resampling results to save space
      tuning_archive_rpf[, resample_result := NULL]
      cli::cli_alert_info("Saving {.code tuning_archive_rpf} to {.path {tuning_archive_path}}")
      saveRDS(tuning_archive_rpf, tuning_archive_path)
    }

    # tuning results: only best configuration
    if (file.exists(tuning_results_path)) {
      cli::cli_alert_warning("{.path {tuning_results_path}} already exists, not overwriting.")
    } else {
      cli::cli_alert_info("Running {.fn extract_inner_tuning_results}")
      t1 <- tictoc::tic()
      tuning_results <- mlr3tuning::extract_inner_tuning_results(bmr)
      t2 <- tictoc::toc()
      cli::cli_alert_info("{.fn extract_inner_tuning_results}: {t2$callback_msg}")

      # Only rpf/rpf_fixmax tuning results
      tuning_results_rpf <- tuning_results[learner_id %in% c("classif.rpf.tuned", "classif.rpf_fixmax.tuned"), ]
      # Remove columns for params of other learners
      tuning_results_rpf <- tuning_results_rpf[, which(unlist(lapply(tuning_results_rpf, function(x) !all(is.na(x))))), with = FALSE]
      # remove redundant x_domain columns since we don't have transformations
      tuning_results_rpf <- tuning_results_rpf[, which(!startsWith(names(tuning_results_rpf), "x_domain")), with = FALSE]

      cli::cli_alert_info("Saving {.code tuning_results_rpf} to {.path {tuning_results_path}}")
      saveRDS(tuning_results_rpf, tuning_results_path)
    }

    # Now that tuning archives/results are stored, we can remove models from the bmr object
    cli::cli_alert_info("Discarding models from {.code bmr}")
    bmr$discard(models = TRUE)
    cli::cli_alert_info("Saving {.code bmr} to {.path {bmr_path}}")
    saveRDS(bmr, bmr_path)
  }
}

process_registry("rpf_batchmark_binary_auc",       "binary",     "auc",   data_dir = "data")
process_registry("rpf_batchmark_multiclass_auc",   "multiclass", "auc",   data_dir = "data")
process_registry("rpf_batchmark_binary_brier",     "binary",     "brier", data_dir = "data")
process_registry("rpf_batchmark_multiclass_brier", "multiclass", "brier", data_dir = "data")
