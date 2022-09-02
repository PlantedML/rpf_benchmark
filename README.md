# rpf_benchmark

Benchmarking `randomPlantedForest` for classification against `ranger` and `xgboost`.

## Setup

- `batchmark.R` contains learner definitions with parameter spaces and `batchtools` setup via `mlr3batchmark`.
- `analysis.R` is used to investigate results.
- `get_oml_tasks.R` is used to retrieve a benchmarking suite from OpenML.

The project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html) to track dependencies, which is particularly
important for GitHub dependencies (`randomPlantedForest`, `mlr3batchmark`) and our fork of `mlr3extralearners` which
contains the experimental `classif.rpf` learner used here ([see here](https://github.com/PlantedML/mlr3extralearners/tree/rpf)).

OpenML usage benefits from setting an API key, [see here](http://openml.github.io/openml-r/articles/OpenML.html#config).

