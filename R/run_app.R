#' Launch the BHAI Interactive Dashboard
#'
#' @description
#' Launches an interactive Shiny dashboard for exploring the Burden of
#' Healthcare-Associated Infections (BHAI) data from the 2011-2012 European
#' Point Prevalence Survey. The dashboard includes three main tabs:
#' \itemize{
#'   \item \strong{Overview:} Survey sample distribution with interactive treemap visualization
#'   \item \strong{Population Estimates:} Annual burden estimates with bubble charts and detailed tables
#'   \item \strong{Stratified Analysis:} Demographic breakdown by age and sex with diverging bar charts
#' }
#'
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}.
#'   Common options include:
#'   \itemize{
#'     \item \code{port}: Port number (e.g., \code{port = 3838})
#'     \item \code{launch.browser}: Whether to open in browser (default: \code{TRUE})
#'     \item \code{host}: Host IP address (default: \code{"127.0.0.1"})
#'   }
#'
#' @return Launches the Shiny application. No return value (called for side effects).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   # Launch the dashboard (opens in default browser)
#'   run_app()
#'
#'   # Launch on specific port
#'   run_app(port = 3838)
#'
#'   # Launch without opening browser automatically
#'   run_app(launch.browser = FALSE)
#' }
#'
#' @seealso
#' \code{\link{bhai_pps_sample_distribution}},
#' \code{\link{bhai_pop_est}},
#' \code{\link{bhai_strata_summary}}
#'
#' @importFrom shiny runApp
run_app <- function(...) {
  # Check for required Shiny app packages
  required_pkgs <- c("shiny", "shinydashboard", "plotly", "DT", "ggplot2", "scales")
  missing_pkgs <- character(0)

  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      missing_pkgs <- c(missing_pkgs, pkg)
    }
  }

  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required to run the app but are not installed:\n  ",
      paste(missing_pkgs, collapse = ", "), "\n\n",
      "Install them with:\n  install.packages(c('",
      paste(missing_pkgs, collapse = "', '"), "'))",
      call. = FALSE
    )
  }

  # Find app directory
  app_dir <- system.file("shiny", package = "healthburdenr")

  if (app_dir == "") {
    stop("Could not find Shiny app directory. Try re-installing `healthburdenr`.", call. = FALSE)
  }

  # Launch app
  shiny::runApp(app_dir, display.mode = "normal", ...)
}

