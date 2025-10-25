#' Flatten PPS Data Structure
#'
#' @description
#' Converts nested PPS (Point Prevalence Survey) S4 objects from BHAI
#' into a flat tibble format suitable for analysis and visualization.
#'
#' @param pps_obj An S4 object of class PPS from the BHAI package
#'
#' @return A tibble with one row per infection type, containing:
#' \describe{
#'   \item{infection}{Type of healthcare-associated infection}
#'   \item{num_hai_patients}{Number of patients with HAI}
#'   \item{num_survey_patients}{Total number of surveyed patients}
#'   \item{length_of_stay}{Average length of hospital stay}
#'   \item{hospital_discharges}{Matrix of hospital discharge data (list-column)}
#'   \item{population}{Population size}
#'   \item{country}{Country code or name}
#'   \item{loi_pps}{Length of infection PPS data (list-column)}
#'   \item{num_hai_patients_by_stratum}{HAI patients by stratum (list-column)}
#'   \item{mccabe_scores_distr}{Distribution of McCabe scores (list-column)}
#'   \item{mccabe_life_exp}{Life expectancy by McCabe score (list-column)}
#'   \item{num_hai_patients_by_stratum_prior}{Prior for HAI by stratum (list-column)}
#'   \item{mccabe_by_stratum_prior}{Prior for McCabe by stratum (list-column)}
#'   \item{num_survey_patients_by_stratum}{Survey patients by stratum (list-column)}
#'   \item{bhai_options}{BHAI options used (list-column)}
#'   \item{bhai_summary}{Summary statistics (list-column)}
#' }
#'
#' @export
#' @importFrom methods slot slotNames
#' @importFrom tibble tibble
#'
#' @examples
#' \dontrun{
#'   # Flatten German PPS data
#'   german_df <- flatten_pps(BHAI::german_pps_conv)
#'   head(german_df)
#'
#'   # Flatten EU PPS data
#'   eu_df <- flatten_pps(BHAI::eu_pps)
#'   head(eu_df)
#' }
flatten_pps <- function(pps_obj) {
  # Helper function to safely extract slots
  safe_slot <- function(obj, s, default = NA) {
    if (s %in% methods::slotNames(obj)) {
      val <- methods::slot(obj, s)
      if (length(val) == 0) return(default)
      return(val)
    }
    default
  }

  infections <- safe_slot(pps_obj, "infections")
  n_infections <- length(infections)

  tibble::tibble(
    infection = infections,
    num_hai_patients = safe_slot(pps_obj, "num_hai_patients"),
    num_survey_patients = rep(safe_slot(pps_obj, "num_survey_patients"), n_infections),
    length_of_stay = rep(safe_slot(pps_obj, "length_of_stay"), n_infections),
    # Convert single matrix into list-column
    hospital_discharges = list(safe_slot(pps_obj, "hospital_discharges")),
    population = rep(safe_slot(pps_obj, "population"), n_infections),
    country = rep(safe_slot(pps_obj, "country"), n_infections),
    # List-columns for infection-level nested data
    loi_pps = safe_slot(pps_obj, "loi_pps"),
    num_hai_patients_by_stratum = safe_slot(pps_obj, "num_hai_patients_by_stratum"),
    mccabe_scores_distr = safe_slot(pps_obj, "mccabe_scores_distr"),
    # Shorter than infections; store as metadata list-column
    mccabe_life_exp = list(safe_slot(pps_obj, "mccabe_life_exp")),
    # Empty slots preserved as NULL/NA
    num_hai_patients_by_stratum_prior = list(safe_slot(pps_obj, "num_hai_patients_by_stratum_prior")),
    mccabe_by_stratum_prior = list(safe_slot(pps_obj, "mccabe_by_stratum_prior")),
    num_survey_patients_by_stratum = list(safe_slot(pps_obj, "num_survey_patients_by_stratum")),
    bhai_options = list(safe_slot(pps_obj, "bhai_options")),
    bhai_summary = list(safe_slot(pps_obj, "bhai_summary"))
  )
}
