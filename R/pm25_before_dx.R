#' Compute mean PM2.5 exposure in previous X days before diagnosis
#'
#' @param zip3_pm25 ZIP3 daily PM2.5 (output of aggregate_pm25_zip3)
#' @param individuals Data frame with: id, zip3, diagnosis_date
#' @param window_days Number of days before diagnosis to average
#'
#' @return individuals with added pm25_mean and n_days
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
