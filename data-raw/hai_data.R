## Code to prepare `bhai_pps_sample_distribution`, `bhai_strata_summary`, `bhai_pop_est` dataset

library(BHAI)
library(tidyverse)

devtools::load_all()

# ---- Load and prepare German PPS data ----
data(german_pps_2011_repr, package = "BHAI")

# Create German PPS object
german_pps_repr <- PPS(num_hai_patients = num_hai_patients,
                      num_hai_patients_by_stratum = num_hai_patients_by_stratum,
                      num_hai_patients_by_stratum_prior = num_hai_patients_by_stratum_prior,
                      num_survey_patients = num_survey_patients,
                      length_of_stay = length_of_stay,
                      loi_pps = loi_pps,
                      mccabe_scores_distr = mccabe_scores_distr,
                      mccabe_life_exp = mccabe_life_exp,
                      hospital_discharges = hospital_discharges,
                      population = population,
                      country="Germany")

# ---- Run BHAI simulations ----
bhai_german_pps <- bhai(german_pps_repr, nsim = 500)
bhai_eu_pps <- bhai(BHAI::eu_pps, nsim = 500)

# ---- Flatten BHAI results ----
bhai_german_pps_flatten <- flatten_pps(bhai_german_pps)
bhai_eu_pps_flatten <- flatten_pps(bhai_eu_pps)

# ---- Dataset 1: Survey Sample Distribution ----
bhai_pps_sample_distribution <- rbind(
    bhai_eu_pps_flatten,
    bhai_german_pps_flatten
  ) |>
  select(country, infection, num_hai_patients, num_survey_patients)

# ---- Dataset 2: Stratified Summary Results ----

# Extract and process BHAI summary data
bhai_summary_df <- rbind(
    bhai_eu_pps_flatten,
    bhai_german_pps_flatten
  ) |>
  select(country, bhai_summary) |>
  unique() |>
  unnest_longer(bhai_summary) |>
  rename(infection = bhai_summary_id) |>
  unnest_wider(bhai_summary)

# Process stratified results with confidence intervals
bhai_strata_summary <- bhai_summary_df |>
  select(-TOTAL) |>
  unnest_wider(stratum_specific_results) |>
  unnest_longer(c(ncases, ndeath, DALY)) |>
  select(-YLL, -YLD, -ndeath_id, -DALY_id) |>
  rename(sex = ncases_id) |>
  unnest_longer(c(ncases, ndeath, DALY), names_repair = "unique") |>
  mutate(
    # Extract confidence intervals for cases
    ncases_lower_ci = ncases[, 1],
    ncases_estimate = ncases[, 2],
    ncases_upper_ci = ncases[, 3],
    # Extract confidence intervals for deaths
    ndeath_lower_ci = ndeath[, 1],
    ndeath_estimate = ndeath[, 2],
    ndeath_upper_ci = ndeath[, 3],
    # Extract confidence intervals for DALYs
    daly_lower_ci = DALY[, 1],
    daly_estimate = DALY[, 2],
    daly_upper_ci = DALY[, 3]
  ) |>
  rename(age_group = ncases_id) |>
  select(-ncases, -ndeath, -DALY, -ndeath_id, -DALY_id) |>
  mutate(
    # Clean up age group formatting
    age_group = str_replace_all(age_group, "Inf", "120"),
    age_group = str_replace_all(age_group, "\\[\\s*([+-]?\\d+)\\s*;\\s*([+-]?\\d+)\\s*\\]", "\\1-\\2"),
    age_group = factor(age_group, levels = c("0-0","1-4","5-9","10-14","15-19",
                                             "20-24","25-29","30-34","35-39","40-44",
                                             "45-49","50-54","55-59","60-64","65-69",
                                             "70-74","75-79","80-84","85-120"))
  )

# ---- Dataset 3: Total Population Estimates ----

# German population estimates
bhai_german_pop_est <- bhai.prettyTable(bhai_german_pps) |>
  as_tibble(rownames = "infection") |>
  mutate(country = "Germany") |>
  mutate(population = bhai_german_pps@population)

# EU population estimates
bhai_eu_pop_est <- bhai.prettyTable(bhai_eu_pps) |>
  as_tibble(rownames = "infection") |>
  mutate(country = "European Union") |>
  mutate(population = bhai_eu_pps@population)

# Combine and parse confidence intervals
bhai_pop_est <- rbind(bhai_german_pop_est, bhai_eu_pop_est) |>
    # Extract point, lower, and upper values directly from formatted strings
    extract(
      Cases, into = c("cases_point_estimate", "cases_lower_ci", "cases_upper_ci"),
      regex = "([0-9,]+) \\(([0-9,]+) ?[-–] ?([0-9,]+)\\)"
    ) |>
    extract(
      Deaths, into = c("deaths_point_estimate", "deaths_lower_ci", "deaths_upper_ci"),
      regex = "([0-9,]+) \\(([0-9,]+) ?[-–] ?([0-9,]+)\\)"
    ) |>
    extract(
      DALY, into = c("daly_point_estimate", "daly_lower_ci", "daly_upper_ci"),
      regex = "([0-9,]+) \\(([0-9,]+) ?[-–] ?([0-9,]+)\\)"
    ) |>
    extract(
      YLL, into = c("yll_point_estimate", "yll_lower_ci", "yll_upper_ci"),
      regex = "([0-9,]+) \\(([0-9,]+) ?[-–] ?([0-9,]+)\\)"
    ) |>
    extract(
      YLD, into = c("yld_point_estimate", "yld_lower_ci", "yld_upper_ci"),
      regex = "([0-9,]+) \\(([0-9,]+) ?[-–] ?([0-9,]+)\\)"
    ) |>
    # Convert all extracted values to integers (remove commas first)
    mutate(across(matches("estimate|ci$"), ~ as.integer(str_remove_all(.x, ","))))

# ---- Save all datasets ----
usethis::use_data(bhai_pps_sample_distribution, bhai_strata_summary, bhai_pop_est, overwrite = TRUE)
