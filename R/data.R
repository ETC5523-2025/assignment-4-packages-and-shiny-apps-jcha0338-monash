#' BHAI PPS Sample Distribution
#'
#' @description
#' Sample distribution from Point Prevalence Surveys (PPS) showing the number
#' of healthcare-associated infection (HAI) patients and total surveyed patients
#' for Germany and the European Union.
#'
#' @format A data frame with one row per infection type and country:
#' \describe{
#'   \item{country}{Country or region name}
#'   \item{infection}{Type of healthcare-associated infection}
#'   \item{num_hai_patients}{Number of patients with HAI in the survey}
#'   \item{num_survey_patients}{Total number of patients surveyed}
#' }
#'
#' @source Derived from BHAI package using German PPS 2011 and EU PPS data
#' @seealso [bhai_strata_summary], [bhai_pop_est]
#'
#' @examples
#' data(bhai_pps_sample_distribution)
#'
#' # View infection types
#' unique(bhai_pps_sample_distribution$infection)
#'
#' # Calculate prevalence
#' library(dplyr)
#' bhai_pps_sample_distribution |>
#'   mutate(prevalence = num_hai_patients / num_survey_patients)
"bhai_pps_sample_distribution"

#' BHAI Stratified Summary Results
#'
#' @description
#' Detailed stratified results from BHAI (Burden of Healthcare-Associated Infections)
#' analysis, including cases, deaths, and DALYs by country, infection type,
#' sex, and age group, with 95% confidence intervals.
#'
#' @format A data frame with stratified results:
#' \describe{
#'   \item{country}{Country or region name}
#'   \item{infection}{Type of healthcare-associated infection}
#'   \item{sex}{Sex category (male/female)}
#'   \item{age_group}{Age group category (factor)}
#'   \item{ncases_lower_ci}{Lower 95% CI for number of cases}
#'   \item{ncases_estimate}{Median estimate for number of cases}
#'   \item{ncases_upper_ci}{Upper 95% CI for number of cases}
#'   \item{ndeath_lower_ci}{Lower 95% CI for number of deaths}
#'   \item{ndeath_estimate}{Median estimate for number of deaths}
#'   \item{ndeath_upper_ci}{Upper 95% CI for number of deaths}
#'   \item{daly_lower_ci}{Lower 95% CI for DALYs}
#'   \item{daly_estimate}{Median estimate for DALYs}
#'   \item{daly_upper_ci}{Upper 95% CI for DALYs}
#' }
#'
#' @source Derived from BHAI simulations (nsim = 500) on German and EU PPS data
#' @seealso [bhai_pps_sample_distribution], [bhai_pop_est]
#'
#' @examples
#' data(bhai_strata_summary)
#'
#' # View age groups
#' levels(bhai_strata_summary$age_group)
#'
"bhai_strata_summary"

#' BHAI Population Estimates
#'
#' @description
#' Total population-level estimates of healthcare-associated infection burden
#' from BHAI analysis. This dataset contains parsed point estimates and 95%
#' confidence intervals for cases, deaths, DALYs, YLL, and YLD.
#'
#' @format A tibble with population estimates by infection and country:
#' \describe{
#'   \item{infection}{Type of healthcare-associated infection}
#'   \item{country}{Country or region (Germany or European Union)}
#'   \item{cases_point_estimate}{Point estimate for number of cases}
#'   \item{cases_lower_ci}{Lower 95% confidence interval for cases}
#'   \item{cases_upper_ci}{Upper 95% confidence interval for cases}
#'   \item{deaths_point_estimate}{Point estimate for number of deaths}
#'   \item{deaths_lower_ci}{Lower 95% confidence interval for deaths}
#'   \item{deaths_upper_ci}{Upper 95% confidence interval for deaths}
#'   \item{daly_point_estimate}{Point estimate for Disability-Adjusted Life Years}
#'   \item{daly_lower_ci}{Lower 95% confidence interval for DALYs}
#'   \item{daly_upper_ci}{Upper 95% confidence interval for DALYs}
#'   \item{yll_point_estimate}{Point estimate for Years of Life Lost}
#'   \item{yll_lower_ci}{Lower 95% confidence interval for YLL}
#'   \item{yll_upper_ci}{Upper 95% confidence interval for YLL}
#'   \item{yld_point_estimate}{Point estimate for Years Lived with Disability}
#'   \item{yld_lower_ci}{Lower 95% confidence interval for YLD}
#'   \item{yld_upper_ci}{Upper 95% confidence interval for YLD}
#' }
#'
#' @details
#' This dataset is created by:
#' \enumerate{
#'   \item Running BHAI simulations (nsim = 500) on German and EU PPS data
#'   \item Formatting results with \code{bhai.prettyTable()}
#'   \item Parsing formatted strings to extract numeric estimates and confidence intervals
#' }
#'
#' All estimates are population-level annual burden estimates.
#'
#' @source Derived from BHAI simulations on German PPS 2011 and EU PPS data
#' @seealso [bhai_pps_sample_distribution], [bhai_strata_summary]
#'
#' @examples
#' data(bhai_pop_est)
#'
#' # View structure
#' str(bhai_pop_est)
#'
#' # Infection with highest mortality
#' bhai_pop_est |>
#'   filter(country == "Germany") |>
#'   arrange(desc(deaths_point_estimate)) |>
#'   select(infection, deaths_point_estimate, deaths_lower_ci, deaths_upper_ci)
"bhai_pop_est"
