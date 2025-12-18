#' Download and clean PM2.5 monitor data for a given year (County-based)
#'
#' This function loads annual PM2.5 from the AirMonitor package,
#' reconstructs valid county FIPS codes using a lookup table,
#' aggregates hourly values into daily county-level means,
#' and returns a tidy dataset ready for ZIP3 weighting.
#'
#' @param year A numeric year (e.g., 2017)
#'
#' @return A tibble with columns:
#'   date, county_fips, pm25
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
