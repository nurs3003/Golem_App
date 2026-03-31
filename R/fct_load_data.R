#' Load and parse RTL continuous futures data
#'
#' Reads \code{RTL::dflong} (dev version from GitHub) and parses the
#' \code{series} column (e.g. \code{"CL_001"}) into separate \code{market}
#' and \code{contract} columns.
#'
#' @return A tibble: \code{date} (Date), \code{market} (chr),
#'   \code{contract} (int), \code{value} (dbl, price).
#' @export
#' @importFrom dplyr mutate filter rename
fct_load_futures_data <- function() {
  df <- RTL::dflong

  # Normalise column names — dev versions may vary slightly
  names(df) <- tolower(names(df))

  # Parse "TICKERNN" → market / contract  (e.g. "CL01" → "CL", 1)
  df |>
    dplyr::mutate(
      market   = sub("[0-9]+$", "", series),          # strip trailing digits
      contract = as.integer(sub("^[A-Za-z]+", "", series)), # strip leading letters
      date     = as.Date(date)
    ) |>
    dplyr::filter(!is.na(value), !is.na(contract)) |>
    dplyr::select(date, market, contract, value)
}


#' Load FRED Constant Maturity Treasury yield data
#'
#' Fetches daily CMT yields (1 month to 30 years) from FRED via
#' \code{tidyquant::tq_get()}.  No API key is required.
#'
#' Returns \code{NULL} with a warning if the network call fails.
#'
#' @return A tibble: \code{date} (Date), \code{maturity_label} (chr),
#'   \code{maturity_years} (dbl), \code{value} (dbl, yield \%).
#' @export
#' @importFrom dplyr mutate filter bind_rows select rename
#' @importFrom tidyquant tq_get
fct_load_cmt_data <- function() {
  cmt_meta <- data.frame(
    ticker         = c("DGS1MO","DGS3MO","DGS6MO",
                       "DGS1","DGS2","DGS3","DGS5",
                       "DGS7","DGS10","DGS20","DGS30"),
    maturity_years = c(1/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30),
    stringsAsFactors = FALSE
  )

  result <- tryCatch({
    rows <- lapply(seq_len(nrow(cmt_meta)), function(i) {
      raw <- tidyquant::tq_get(cmt_meta$ticker[i], get = "economic.data")
      raw |>
        dplyr::rename(value = price) |>
        dplyr::mutate(
          maturity_label = cmt_meta$ticker[i],
          maturity_years = cmt_meta$maturity_years[i],
          date           = as.Date(date)
        ) |>
        dplyr::select(date, maturity_label, maturity_years, value)
    })
    dplyr::bind_rows(rows) |>
      dplyr::filter(!is.na(value))
  }, error = function(e) {
    warning("Could not fetch CMT data from FRED: ", conditionMessage(e))
    NULL
  })

  result
}
