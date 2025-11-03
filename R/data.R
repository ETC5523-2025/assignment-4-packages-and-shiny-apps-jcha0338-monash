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
#' @details
#' The five infection types tracked are:
#' \itemize{
#'   \item \strong{HAP:} Hospital-acquired pneumonia
#'   \item \strong{SSI:} Surgical site infection
#'   \item \strong{BSI:} Bloodstream infection
#'   \item \strong{UTI:} Urinary tract infection
#'   \item \strong{CDI:} \emph{Clostridioides difficile} infection
#' }
#'
#' @source Derived from BHAI package using German PPS 2011 and EU PPS data.
#'   Zacher et al. (2019) \doi{10.2807/1560-7917.ES.2019.24.46.1900135}
#'
#' @references
#' Zacher B, Haller S, Eckmanns T, Noll I, Weiss B, Widders G, Gastmeier P,
#' Monnet DL. Application of a new methodology and R package reveals a high
#' burden of healthcare-associated infections (HAI) in Germany compared to
#' the average in the European Union/European Economic Area, 2011 to 2012.
#' Euro Surveill. 2019;24(46):pii=1900135.
#'
#' @seealso [bhai_strata_summary], [bhai_pop_est], [flatten_pps()]
#'
#' @examples
#' data(bhai_pps_sample_distribution)
#'
#' # View infection types
#' unique(bhai_pps_sample_distribution$infection)
#'
#' # Calculate prevalence
#' \dontrun{
#' bhai_pps_sample_distribution |>
#'   dplyr::mutate(prevalence = num_hai_patients / num_survey_patients)
#' }
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
#'   \item{sex}{Sex category (M/F)}
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
#' @details
#' Stratification enables identification of demographic groups at highest risk.
#' Age groups span from 0-1 years to 85-120 years. Results derived from 500
#' BHAI simulations incorporating uncertainty in prevalence, mortality, and
#' disability weights.
#'
#' @source Derived from BHAI simulations (nsim = 500) on German and EU PPS data.
#'   Zacher et al. (2019) \doi{10.2807/1560-7917.ES.2019.24.46.1900135}
#'
#' @references
#' Zacher B, Haller S, Eckmanns T, Noll I, Weiss B, Widders G, Gastmeier P,
#' Monnet DL. Application of a new methodology and R package reveals a high
#' burden of healthcare-associated infections (HAI) in Germany compared to
#' the average in the European Union/European Economic Area, 2011 to 2012.
#' Euro Surveill. 2019;24(46):pii=1900135.
#'
#' @seealso [bhai_pps_sample_distribution], [bhai_pop_est]
#'
#' @examples
#' data(bhai_strata_summary)
#'
#' # View age groups
#' levels(bhai_strata_summary$age_group)
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
#'   \item{infection}{Type of healthcare-associated infection (or "ALL" for total)}
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
#'   \item{country}{Country or region (Germany or European Union)}
#'   \item{population}{Total population of the country or region}
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
#' All estimates are population-level annual burden estimates. The "ALL" row
#' represents the sum across all five HAI types.
#'
#' @source Derived from BHAI simulations on German PPS 2011 and EU PPS data.
#'   Zacher et al. (2019) \doi{10.2807/1560-7917.ES.2019.24.46.1900135}
#'
#' @references
#' Zacher B, Haller S, Eckmanns T, Noll I, Weiss B, Widders G, Gastmeier P,
#' Monnet DL. Application of a new methodology and R package reveals a high
#' burden of healthcare-associated infections (HAI) in Germany compared to
#' the average in the European Union/European Economic Area, 2011 to 2012.
#' Euro Surveill. 2019;24(46):pii=1900135.
#'
#' @seealso [bhai_pps_sample_distribution], [bhai_strata_summary]
#'
#' @examples
#' data(bhai_pop_est)
#'
#' # View structure
#' str(bhai_pop_est)
#'
#' # Total burden (ALL row)
#' \dontrun{
#' bhai_pop_est |>
#'   dplyr::filter(infection == "ALL", country == "Germany")
#' }
"bhai_pop_est"
