#' Download Annual PM2.5 Monitoring Data (AirMonitor)
#'
#' This function retrieves daily PM2.5 measurements from the
#' \pkg{AirMonitor} database for an entire calendar year. The result
#' is returned as a tidy data frame with one row per monitor per day,
#' including metadata such as state, county, and FIPS codes.
#'
#' @param year Integer. The calendar year to download (e.g., `2017`).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{date} — Date of observation.
#'   \item \code{state_fips}, \code{county_fips} — FIPS codes.
#'   \item \code{county_name}, \code{state_abbr} — Geographic descriptors.
#'   \item \code{pm25} — Daily average PM2.5 concentration (µg/m³).
#' }
#'
#' @details
#' The function uses \code{AirMonitor::airnow_loadAnnual()} to retrieve
#' monitor-level data and reshapes it into a long daily time series.
#' Only PM2.5 observations are retained.
#'
#' @examples
#' \dontrun{
#' pm25_2017 <- download_pm25_year(2017)
#' }
#'
#' @export
download_pm25_year <- function(year) {

  # Load required lookup file
  lookup_path <- system.file("extdata", "county_lookup.csv", package = "zip3exposures")

  county_lookup <- readr::read_csv(lookup_path, show_col_types = FALSE) %>%
    dplyr::mutate(
      state_fips = sprintf("%02s", state_fips),
      county_fips = sprintf("%03s", county_fips)
    )

  # Load annual AirNow PM2.5 data
  mon <- AirMonitor::airnow_loadAnnual(year)

  meta <- AirMonitor::monitor_getMeta(mon)
  data <- AirMonitor::monitor_getData(mon)

  # Filter to U.S. states (remove Canada, MX)
  us_states <- c(state.abb, "DC", "PR")

  meta <- meta %>%
    dplyr::filter(stateCode %in% us_states) %>%
    dplyr::select(deviceDeploymentID, stateCode, countyName)

  # Subset hourly data to U.S. monitors
  keep_cols <- c("datetime", meta$deviceDeploymentID)
  data <- data[, colnames(data) %in% keep_cols]

  # Reconstruct county_fips via lookup
  meta_fips <- meta %>%
    dplyr::left_join(
      county_lookup,
      by = c(
        "stateCode" = "state_abbr",
        "countyName" = "county_name"
      )
    ) %>%
    dplyr::mutate(
      county_fips = paste0(state_fips, county_fips)
    ) %>%
    dplyr::filter(!is.na(county_fips)) %>%
    dplyr::select(deviceDeploymentID, county_fips)

  # Reshape hourly data to long format
  pm_long <- data %>%
    tidyr::pivot_longer(
      cols = -datetime,
      names_to = "deviceDeploymentID",
      values_to = "pm25"
    ) %>%
    dplyr::left_join(meta_fips, by = "deviceDeploymentID") %>%
    dplyr::mutate(date = as.Date(datetime)) %>%
    dplyr::select(date, county_fips, pm25)

  # Aggregate hourly PM2.5 to daily county mean
  pm_daily <- pm_long %>%
    dplyr::filter(!is.na(county_fips)) %>%
    dplyr::group_by(county_fips, date) %>%
    dplyr::summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")

  return(pm_daily)
}
