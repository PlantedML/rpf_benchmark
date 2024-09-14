if (FALSE) {
  # Packages required but not automatically picked up by renv
  requireNamespace("xgboost")
  requireNamespace("randomPlantedForest")
  requireNamespace("ranger")
  requireNamespace("callr") # For encapsulation
}

checkmate::assert_subset(conf$tuning$tuner, choices = c("random_search", "mbo"))

if (conf$tuning$tuner == "mbo") {
  library(mlr3mbo)

  if (FALSE) {
    requireNamespace("DiceKriging")
    requireNamespace("rgenoud")
  }
}

wrap_autotuner <- function(learner_id, ..., search_space, .encode = FALSE, .tuning_measure) {
  cli::cli_alert_info("Setting up {.val {learner_id}} with {.val {.tuning_measure}}")
  paradox::assert_param_set(search_space)

  base_learner <- lrn(learner_id, predict_type = "prob", ...)

  if (.encode) {
    cli::cli_alert_info("Adding encoding pipeline")
    base_learner <- po("encode", method = "one-hot") %>>%
      po("removeconstants") %>>%
      base_learner |>
      as_learner()
  }

  # Used for XGBoost learners to enable internal tuning / early stopping using test set
  if ("validation" %in% base_learner$properties) {
    cli::cli_alert_info("Setting validation for {.val {learner_id}}")
    set_validate(base_learner, "test")
  }


  if (conf$fallback$inner) {
    # base_learner$encapsulate("callr", lrn("classif.featureless"))
    base_learner$fallback = lrn("classif.featureless", predict_type = "prob")
    base_learner$encapsulate = c(train = "callr", predict = "callr")
  }

  base_learner$timeout = c(train   = conf$timeout$base$train  * 3600,
                           predict = conf$timeout$base$predict * 3600)

  at <- auto_tuner(
    learner = base_learner,
    resampling = switch(
      conf$resampling$inner$strategy,
      "holdout"     = rsmp("holdout"),
      "cv"          = rsmp("cv", folds = conf$resampling$inner$folds),
      "repeated_cv" = rsmp("repeated_cv",
                           folds = conf$resampling$inner$folds,
                           repeats = conf$resampling$inner$repeats),
    ),
    measure = msr(.tuning_measure,
                  id = stringi::stri_replace_all_fixed(.tuning_measure,
                                                       pattern = "classif.", replacement = "")),
    search_space = search_space,
    terminator = trm("combo", list(
      trm("run_time", secs = conf$tuning$runtime),
      trm("evals", n_evals = conf$tuning$evals, k = conf$tuning$multiplier)
    ), any = TRUE),
    tuner = tnr(conf$tuning$tuner),
    store_tuning_instance = TRUE,
    store_benchmark_result = FALSE,
    store_models = FALSE
  )

  if (conf$fallback$outer) {
    # at$encapsulate("callr", lrn("classif.featureless"))
    at$fallback = lrn("classif.featureless", predict_type = "prob")
    at$encapsulate = c(train = "callr", predict = "callr")
  }

  at$timeout = c(train   = conf$timeout$autotuner$train  * 3600,
                 predict = conf$timeout$autotuner$predict * 3600)

  at

}

make_learners <- function(tuning_measure) {
  learners <- list(

    rpf = wrap_autotuner(
      learner_id = "classif.rpf",
      ntrees = 50,
      max_interaction_limit = 20,
      .tuning_measure = tuning_measure,
      search_space = ps(
        max_interaction_ratio = p_dbl(0, 1),
        splits    = p_int(10, 100),
        split_try = p_int(1, 20),
        t_try     = p_dbl(0.1, 1)
      )
    )

    ,

    rpf_fixdepth = wrap_autotuner(
      learner_id = "classif.rpf",
      ntrees = 50,
      max_interaction = 2,
      .tuning_measure = tuning_measure,
      search_space = ps(
        splits    = p_int(10, 100),
        split_try = p_int(1, 20),
        t_try     = p_dbl(0.1, 1)
      )
    )

    ,

    xgb = wrap_autotuner(
      learner_id = "classif.xgboost",
      early_stopping_rounds = 50,
      .encode = TRUE,
      .tuning_measure = tuning_measure,
      search_space = ps(
        classif.xgboost.max_depth        = p_int(1, 20),
        classif.xgboost.subsample        = p_dbl(0.1, 1),
        classif.xgboost.colsample_bytree = p_dbl(0.1, 1),
        classif.xgboost.eta              = p_dbl(1e-4, 1, logscale = TRUE),
        classif.xgboost.nrounds          = p_int(upper = 5000, tags = "internal_tuning",
                                                 aggr = function(x) as.integer(mean(unlist(x))))
      )
    )

    ,

    xgb_fixdepth = wrap_autotuner(
      learner_id = "classif.xgboost",
      max_depth = 2,
      early_stopping_rounds = 50,
      .encode = TRUE,
      .tuning_measure = tuning_measure,
      search_space = ps(
        classif.xgboost.max_depth        = p_int(1, 20),
        classif.xgboost.subsample        = p_dbl(0.1, 1),
        classif.xgboost.colsample_bytree = p_dbl(0.1, 1),
        classif.xgboost.eta              = p_dbl(1e-4, 1, logscale = TRUE),
        classif.xgboost.nrounds          = p_int(upper = 5000, tags = "internal_tuning",
                                                 aggr = function(x) as.integer(mean(unlist(x))))
      )
    )

    ,

    ranger = wrap_autotuner(
      learner_id = "classif.ranger",
      num.trees = 50,
      .tuning_measure = tuning_measure,
      search_space = ps(
        mtry.ratio      = p_dbl(0.1, 1),
        min.node.size   = p_int(1, 50),
        sample.fraction = p_dbl(0.1, 1)
        # replace = p_lgl()
      )
    )

    ,

    featureless = lrn("classif.featureless")
  )

  # Use list names for learner ids for convenience downstream
  mlr3misc::imap(learners, function(l, id) l$id = id)
  learners
}
