#' forward_curve UI Function
#'
#' Shows the term structure (forward curve) for commodity futures and the
#' yield curve for US Treasuries.  A date slider lets the user compare
#' the curve shape across three historical snapshots.
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList dateInput checkboxInput numericInput
#' @importFrom bslib layout_columns card card_header
#' @importFrom plotly plotlyOutput
mod_forward_curve_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::dateInput(
        ns("snapshot_date"),
        "Snapshot Date",
        value = Sys.Date() - 30,
        min   = as.Date("2007-01-01"),
        max   = Sys.Date()
      ),
      shiny::checkboxInput(ns("show_history"), "Overlay 2 prior snapshots", TRUE),
      shiny::numericInput(
        ns("history_interval"),
        "Interval between snapshots (days)",
        value = 365, min = 30, max = 1825, step = 30
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header(
        "Forward Curve — hover to inspect, click legend to toggle markets"
      ),
      plotly::plotlyOutput(ns("curve_plot"), height = "100%")
    )
  )
}

#' forward_curve Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer renderText req
#' @importFrom dplyr filter arrange
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_forward_curve_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    output$curve_plot <- plotly::renderPlotly({
      req(r$selected_markets)
      snapshot <- input$snapshot_date
      req(!is.null(snapshot))

      interval    <- input$history_interval %||% 365
      show_hist   <- isTRUE(input$show_history)
      snap_dates  <- if (show_hist) {
        c(snapshot - 2L * interval, snapshot - interval, snapshot)
      } else {
        snapshot
      }
      # Opacity ramp: oldest = faintest
      opacities <- seq(0.3, 1, length.out = length(snap_dates))

      p <- plotly::plot_ly()

      for (mkt in r$selected_markets) {

        if (mkt == "CMT") {
          req(!is.null(r$cmt_data))
          for (i in seq_along(snap_dates)) {
            d    <- snap_dates[i]
            snap <- r$cmt_data |>
              dplyr::filter(date <= d) |>
              dplyr::filter(date == max(date)) |>
              dplyr::arrange(maturity_years)
            if (nrow(snap) == 0L) next
            p <- plotly::add_trace(
              p,
              data   = snap,
              x      = ~maturity_years,
              y      = ~value,
              type   = "scatter",
              mode   = "lines+markers",
              name   = paste0("CMT (", format(d, "%Y-%m-%d"), ")"),
              opacity = opacities[i],
              hovertemplate = paste0(
                "Maturity: %{x:.1f}y<br>Yield: %{y:.2f}%<br>Date: ",
                format(d, "%Y-%m-%d"), "<extra></extra>"
              )
            )
          }

        } else {
          req(!is.null(r$data))
          mkt_data <- dplyr::filter(r$data, market == mkt)
          for (i in seq_along(snap_dates)) {
            d    <- snap_dates[i]
            snap <- mkt_data |>
              dplyr::filter(date <= d) |>
              dplyr::filter(date == max(date)) |>
              dplyr::arrange(contract)
            if (nrow(snap) == 0L) next
            p <- plotly::add_trace(
              p,
              data   = snap,
              x      = ~contract,
              y      = ~value,
              type   = "scatter",
              mode   = "lines+markers",
              name   = paste0(mkt, " (", format(d, "%Y-%m-%d"), ")"),
              opacity = opacities[i],
              hovertemplate = paste0(
                "Contract: %{x}<br>Price: %{y:.2f}<br>Date: ",
                format(d, "%Y-%m-%d"), "<extra></extra>"
              )
            )
          }
        }
      }

      plotly::layout(
        p,
        xaxis      = list(title = "Contract # (futures) / Maturity in years (CMT)"),
        yaxis      = list(title = "Price / Yield (%)"),
        legend     = list(orientation = "h", y = -0.2),
        hovermode  = "x unified",
        margin     = list(b = 80)
      )
    })

  })
}

# Null-coalescing helper (base R equivalent of rlang::`%||%`)
`%||%` <- function(x, y) if (!is.null(x)) x else y
