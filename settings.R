# Global settings

global_seed <- 230749

inner_folds <- 5
outer_folds <- 10
tuning_budget <- 200 # n_evals, 50 for trial mode

# rpf tuning params
rpf.ntrees <- 50
rpf.maxintlim <- 30
rpf.maxintratio <- p_dbl(0, 1)
rpf.loss <- p_fct(c("L1", "L2", "exponential")) # removed logit due to runtime
rpf.splits <- p_int(10, 100) # Bumped to lower = 10 and upper = 100 as per Munir
rpf.split_try <- p_int(1, 20)
rpf.t_try <- p_dbl(0.1, 1)

# Maximum n * p of selected tasks
task_dim_max <- 1e5


# "registry" holds the registries, must be ensured to exist
if (!dir.exists("registry")) dir.create("registry")

# same idea for "data" and "plots"
if (!dir.exists("data")) dir.create("data")
if (!dir.exists("plots")) dir.create("plots")
