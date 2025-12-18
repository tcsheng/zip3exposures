#' Aggregate County-Level PM2.5 Data to ZIP3 Regions
#'
#' This function converts county-level PM2.5 measurements into ZIP3-level
#' averages using a crosswalk file linking ZIP3 codes to county FIPS codes.
#' Each ZIP3's PM2.5 is computed as the population-weighted average of the
#' counties associated with that ZIP3.
#'
#' @param county_pm25 A tibble containing county-level PM2.5 time series
#'   as returned by \code{download_pm25_year()}.
#' @param crosswalk A tibble mapping ZIP3 codes to counties, containing:
#'   \itemize{
#'     \item \code{zip3} — ZIP3 region.
#'     \item \code{county_fips} — 5-digit FIPS code.
#'     \item \code{weight} — Population-based weight for aggregation.
#'   }
#'
#' @return A tibble with ZIP3-level PM2.5 by date:
#' \itemize{
#'   \item \code{zip3}
#'   \item \code{date}
#'   \item \code{pm25} — Weighted average PM2.5 for that ZIP3 and date.
#' }
#'
#' @details
#' The weights typically come from HUD USPS crosswalk population estimates
#' and ensure ZIP3-level values reflect population distribution across
#' counties.
#'
#' @examples
#' \dontrun{
#' pm25_zip3 <- aggregate_pm25_zip3(pm25_2017, hud_zip3)
#' }
#'
#' @export
aggregate_pm25_zip3 <- function(pm25_county,
                                crosswalk = zip3exposures::hud_zip3) {

  pm25_zip3 <- pm25_county %>%
    dplyr::inner_join(crosswalk, by = "county_fips") %>%  # attach weights
    dplyr::group_by(zip3, date) %>%
    dplyr::summarise(
      pm25 = stats::weighted.mean(pm25, weight, na.rm = TRUE),
      .groups = "drop"
    )

  return(pm25_zip3)
}
