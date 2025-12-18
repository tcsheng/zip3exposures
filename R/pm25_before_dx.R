#' Compute ZIP3-Level PM2.5 Exposure Prior to Diagnosis
#'
#' This function assigns PM2.5 exposure to individuals by averaging ZIP3-level
#' PM2.5 measurements over a specified number of days before each individual’s
#' diagnosis date.
#'
#' @param zip3_pm25 A tibble with ZIP3-level PM2.5 time series as produced by
#'   \code{aggregate_pm25_zip3()}.
#' @param individuals A tibble with at least the following columns:
#'   \itemize{
#'     \item \code{id} — Individual identifier.
#'     \item \code{zip3} — ZIP3 code for residence.
#'     \item \code{diagnosis_date} — Date of diagnosis (Date class).
#'   }
#' @param window_days Integer indicating how many days prior to diagnosis
#'   should be averaged (default: 30).
#'
#' @return A tibble identical to \code{individuals} but with additional columns:
#' \itemize{
#'   \item \code{start_date} — First date in the averaging window.
#'   \item \code{pm25_mean} — Mean PM2.5 across the window.
#'   \item \code{n_days} — Number of days with available data.
#' }
#'
#' @details
#' The window is \code{diagnosis_date - window_days + 1} through
#' \code{diagnosis_date}. If fewer than \code{window_days} days are available,
#' the mean is computed using whatever days exist.
#'
#' @examples
#' \dontrun{
#' exposure <- pm25_before_dx(pm25_zip3, individuals, window_days = 30)
#' }
#'
#' @export
pm25_before_dx <- function(zip3_pm25, individuals, window_days = 30) {

  individuals <- individuals %>%
    mutate(start_date = diagnosis_date - window_days)

  out <- individuals %>%
    rowwise() %>%
    mutate(
      pm25_mean = {
        indiv_zip3 <- zip3

        df <- zip3_pm25 %>%
          filter(
            zip3 == indiv_zip3,
            date >= start_date,
            date < diagnosis_date
          )

        if (nrow(df) == 0) NA_real_ else mean(df$pm25, na.rm = TRUE)
      },
      n_days = {
        indiv_zip3 <- zip3

        df <- zip3_pm25 %>%
          filter(
            zip3 == indiv_zip3,
            date >= start_date,
            date < diagnosis_date
          )

        nrow(df)
      }
    ) %>%
    ungroup()

  return(out)
}
