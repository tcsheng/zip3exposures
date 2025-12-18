#' Aggregate county-level PM2.5 to ZIP3 weighted PM2.5
#'
#' @param pm25_county Output from download_pm25_year()
#' @param crosswalk hud_zip3 dataset included in package
#'
#' @return A tibble with ZIP3-level weighted daily PM2.5
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
