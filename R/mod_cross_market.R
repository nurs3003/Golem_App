#' cross_market UI Function
#'
#' Morning briefing table: all six markets on one screen.
#' Columns: market, price, 1D/1W/1M/1Y return, 21-day vol,
#' vol regime (vs 1-year median), curve shape, roll yield.
#' Color-coded with DT::formatStyle for instant visual triage.
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom bslib card card_header
#' @importFrom DT DTOutput
mod_cross_market_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    fill  = FALSE,
    bslib::card_header("Morning Briefing — all markets at a glance"),
    shiny::tags$p(
      "Green 1D/1W = positive return. Red 21d Vol = high-vol regime (above 1-year median).
       Backwardation in curve shape = supply tight; Contango = oversupply / storage carry.
       Positive roll yield = earn by rolling; negative = pay to maintain exposure.",
      style = "font-size:0.82rem; color:#666; padding:0.4rem 1rem 0; margin:0;"
    ),
    DT::DTOutput(ns("briefing_table"))
  )
}

#' cross_market Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter arrange mutate select group_by ungroup inner_join bind_rows
#' @importFrom slider slide_dbl
#' @importFrom stats median quantile sd
#' @importFrom utils tail
#' @importFrom DT renderDT datatable formatStyle styleInterval styleEqual
mod_cross_market_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    all_markets <- c("CL", "BRN", "NG", "HO", "RB", "HTT")

    briefing_data <- reactive({
      req(!is.null(r$data))

      lapply(all_markets, function(mkt) {
        mkt_data <- r$data |>
          dplyr::filter(market == mkt, contract == 1L) |>
          dplyr::arrange(date)

        n         <- nrow(mkt_data)
        if (n < 2L) return(NULL)

        latest    <- mkt_data$value[n]
        latest_dt <- mkt_data$date[n]

        # Date-based % changes
        price_at <- function(target_dt) {
          sub <- mkt_data[mkt_data$date <= target_dt, ]
          if (nrow(sub) == 0L) return(NA_real_)
          sub$value[nrow(sub)]
        }

        ret_1d <- (latest / price_at(latest_dt - 1L)  - 1) * 100
        ret_1w <- (latest / price_at(latest_dt - 7L)  - 1) * 100
        ret_1m <- (latest / price_at(latest_dt - 30L) - 1) * 100
        ret_1y <- (latest / price_at(latest_dt - 365L)- 1) * 100

        # 21-day realized vol (most recent window)
        log_rets <- diff(log(mkt_data$value))
        vol_21d  <- if (length(log_rets) >= 21L) {
          stats::sd(utils::tail(log_rets, 21L)) * sqrt(252) * 100
        } else NA_real_

        # Vol regime: current 21d vol vs trailing 1-year (252-day) median of rolling 21d vol
        if (length(log_rets) >= 252L + 21L) {
          all_vols <- slider::slide_dbl(log_rets,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252) * 100,
            .before = 20L, .complete = TRUE)
          med_vol  <- stats::median(utils::tail(all_vols[!is.na(all_vols)], 252L), na.rm = TRUE)
          vol_regime <- if (!is.na(vol_21d) && vol_21d > med_vol) "High" else "Normal"
        } else {
          vol_regime <- NA_character_
        }

        # Curve shape: compare C1 vs C2 (backwardation if C1 > C2)
        c2_last <- r$data |>
          dplyr::filter(market == mkt, contract == 2L, date <= latest_dt) |>
          dplyr::arrange(date)
        c2_val <- if (nrow(c2_last) > 0L) c2_last$value[nrow(c2_last)] else NA_real_
        curve_shape <- if (!is.na(c2_val)) {
          if (latest > c2_val) "Backwardation" else "Contango"
        } else NA_character_

        # Roll yield (annualised)
        roll_yield_pct <- if (!is.na(c2_val) && c2_val > 0) {
          round((latest / c2_val - 1) * 12 * 100, 2)
        } else NA_real_

        # Unit label
        unit <- switch(mkt,
          CL = "$/bbl", BRN = "$/bbl", HTT = "$/bbl",
          HO = "$/gal", RB  = "$/gal", NG  = "$/MMBtu"
        )

        data.frame(
          Market      = paste0(mkt),
          Unit        = unit,
          Price       = round(latest, if (mkt == "NG") 3L else 2L),
          `1D %`      = round(ret_1d, 2),
          `1W %`      = round(ret_1w, 2),
          `1M %`      = round(ret_1m, 2),
          `1Y %`      = round(ret_1y, 2),
          `21d Vol %` = round(vol_21d, 1),
          `Vol Regime`= vol_regime,
          `Curve`     = curve_shape,
          `Roll Yield %/yr` = roll_yield_pct,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }) |> dplyr::bind_rows()
    })

    output$briefing_table <- DT::renderDT({
      df <- briefing_data()
      req(nrow(df) > 0L)

      DT::datatable(
        df,
        rownames  = FALSE,
        options   = list(
          dom        = "t",         # table only, no search/pagination
          pageLength = 10,
          ordering   = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "compact stripe hover"
      ) |>
        # 1D % — green positive, red negative
        DT::formatStyle(
          "1D %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))
        ) |>
        # 1W % — same
        DT::formatStyle(
          "1W %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))
        ) |>
        # 1Y % — green/red
        DT::formatStyle(
          "1Y %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))
        ) |>
        # Vol Regime — red background for High
        DT::formatStyle(
          "Vol Regime",
          backgroundColor = DT::styleEqual(
            c("High", "Normal"),
            c("rgba(192,57,43,0.15)", "rgba(39,174,96,0.12)")
          )
        ) |>
        # Curve shape — blue for backwardation, grey for contango
        DT::formatStyle(
          "Curve",
          color = DT::styleEqual(
            c("Backwardation", "Contango"),
            c("#2980b9", "#7f8c8d")
          ),
          fontWeight = "bold"
        ) |>
        # Roll yield — green positive (earn), red negative (pay)
        DT::formatStyle(
          "Roll Yield %/yr",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))
        )
    })

  })
}
