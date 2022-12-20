library(dplyr)
library(kableExtra)

task_info |>
  filter(twoclass, featuretypes, nomissing) |>
  select(-twoclass, -featuretypes, -nomissing, -has_factors) |>
  arrange(dim) |>
  mutate(
    selection = case_when(
      dim <= 1e5 ~ "n*p <= 100.000",
      dim <= 7e5 ~ "n*p <= 700.000",
      TRUE ~ "Zu groß"
    )
  ) |>
  kbl(caption = "Binary Classif") |>
  kable_styling()

task_info |>
  filter(twoclass, featuretypes, nomissing) |>
  select(-twoclass, -featuretypes, -nomissing, -has_factors) |>
  arrange(dim) |>
  mutate(
    selection = case_when(
      dim <= 1e5 ~ "n*p <= 100.000",
      dim <= 7e5 ~ "n*p <= 700.000",
      TRUE ~ "Zu groß"
    )
  ) |>
  count(selection)

task_info |>
  filter(!twoclass, featuretypes, nomissing) |>
  select(-twoclass, -featuretypes, -nomissing, -has_factors) |>
  arrange(dim) |>
  kbl(caption = "Multiclass Classif") |>
  kable_styling()

taskinf <- task_info |>
  filter(featuretypes, nomissing) |>
  mutate(type = ifelse(twoclass, "Binary", "Multiclass")) |>
  mutate(
    selection = case_when(
      dim <= 1e5 ~ "n*p <= 100.000",
      dim <= 7e5 ~ "n*p <= 700.000",
      TRUE ~ "Zu groß"
    )
  ) |>
  select(-twoclass, -featuretypes, -nomissing, -has_factors)

taskinf |>
  count(type, selection) |>
  tidyr::pivot_wider(id_cols = c("type"), names_from = "selection", values_from = "n") |>
  kbl() |>
  kable_styling()

taskinf |>
  write.csv(file = "tmp", row.names = FALSE)
