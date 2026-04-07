#' forward_curve UI Function
#'
#' Two sub-tabs:
#' \enumerate{
#'   \item Commodity Curves — term structure of futures prices.
#'   \item Yield Curve — US Treasury CMT rates.
#' }
#' A date picker and history-overlay toggle are shared across both tabs.
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList dateInput checkboxInput numericInput selectInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
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
        value = {
          d <- Sys.Date() - 30L
          if (weekdays(d) == "Saturday") d <- d - 1L
          if (weekdays(d) == "Sunday")   d <- d - 2L
          d
        },
        min                 = as.Date("2007-01-01"),
        max                 = Sys.Date(),
        daysofweekdisabled  = c(0, 6)
      ),
      shiny::checkboxInput(ns("show_history"), "Overlay 2 prior snapshots", TRUE),
      shiny::numericInput(
        ns("history_interval"),
        "Interval between snapshots (days)",
        value = 365, min = 30, max = 1825, step = 30
      ),
      shiny::selectInput(
        ns("macro_overlay"),
        "Macro overlay (commodity)",
        choices  = c("CL", "BRN", "NG", "HO", "RB", "HTT"),
        selected = "CL"
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header(
        "Forward Curve — hover to inspect, click legend to toggle markets"
      ),
      bslib::navset_tab(
        bslib::nav_panel(
          "Commodity Curves",
          plotly::plotlyOutput(ns("commodity_curve"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Term structure of futures prices by contract month (1 = front month).
             Upward-sloping = contango (storage costs dominate); downward-sloping = backwardation (spot tightness / convenience yield).
             Backwardation in crude often signals supply stress. Faded lines show earlier snapshots for comparison.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Yield Curve",
          plotly::plotlyOutput(ns("yield_curve"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "US Treasury constant-maturity yields (1M to 30Y). Normal = upward-sloping;
             inversion (short > long) historically precedes recessions and signals demand
             destruction risk for energy commodities.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Roll Yield",
          plotly::plotlyOutput(ns("roll_yield"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "When a futures contract approaches expiry, a hedger must sell it and buy the next month forward \u2014 this is called rolling.
             In backwardation (nearby price above deferred), rolling earns a positive return: you sell high and buy low.
             In contango (nearby price below deferred), rolling costs money: you sell low and buy high.
             In steep contango this drag can exceed 2\u20133% per month \u2014 a hedger holding front-month exposure year-round
             may lose more to roll cost than they gain from the hedge itself.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Macro Context",
          plotly::plotlyOutput(ns("macro_context"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "US Treasury 2Y\u201310Y yield spread (left axis) vs the selected commodity front-month price (right axis).
             When the spread goes negative the yield curve is inverted \u2014 historically a leading indicator of recession,
             typically 12\u201318 months ahead. The 2006\u20132007 inversion preceded the 2008\u20132009 crude collapse from $147 to $32;
             the 2019 inversion preceded the 2020 COVID demand shock. Note: the indicator can be overwhelmed by supply shocks \u2014
             the severe 2022\u20132023 inversion did not cause an immediate crude collapse because Russia-Ukraine supply disruption
             and OPEC+ cuts offset the demand signal. Use this alongside the Fundamentals tab for the full picture.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        )
      )
    )
  )
}

#' forward_curve Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer req validate need
#' @importFrom dplyr filter arrange select mutate inner_join
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_forward_curve_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    snap_dates <- shiny::reactive({
      snapshot <- input$snapshot_date
      shiny::req(!is.null(snapshot))
      interval  <- input$history_interval %||% 365
      show_hist <- isTRUE(input$show_history)
      if (show_hist) {
        c(snapshot - 2L * interval, snapshot - interval, snapshot)
      } else {
        snapshot
      }
    })

    # ── Commodity term-structure ────────────────────────────────────────────
    output$commodity_curve <- plotly::renderPlotly({
      shiny::req(r$selected_markets, !is.null(r$data))
      dates     <- snap_dates()
      opacities <- seq(0.3, 1, length.out = length(dates))
      mkts      <- setdiff(r$selected_markets, "CMT")
      shiny::req(length(mkts) > 0L)

      p <- plotly::plot_ly()

      for (mkt in mkts) {
        col      <- market_colors[mkt] %||% "#888888"
        mkt_data <- dplyr::filter(r$data, market == mkt)

        for (i in seq_along(dates)) {
          d    <- dates[i]
          snap <- mkt_data |>
            dplyr::filter(date <= d) |>
            dplyr::filter(date == max(date)) |>
            dplyr::arrange(contract)
          if (nrow(snap) == 0L) next

          # Contango/backwardation label for most-recent snapshot only.
          # Compare C1 vs C2 (the most liquid calendar spread), not C1 vs Cmax.
          curve_label <- ""
          if (i == length(dates) && nrow(snap) >= 2L) {
            c1_val <- snap$value[snap$contract == min(snap$contract)]
            c2_val <- snap$value[snap$contract == sort(snap$contract)[2]]
            if (length(c1_val) > 0 && length(c2_val) > 0) {
              if (c2_val > c1_val) {
                curve_label <- " \u25b2 contango"
              } else if (c2_val < c1_val) {
                curve_label <- " \u25bc backwardation"
              }
            }
          }

          p <- plotly::add_trace(
            p,
            data      = snap,
            x         = ~contract,
            y         = ~value,
            type      = "scatter",
            mode      = "lines+markers",
            name      = paste0(mkt, " (", format(d, "%b %d %Y"), ")", curve_label),
            opacity   = opacities[i],
            line      = list(color = col),
            marker    = list(color = col),
            hovertemplate = paste0(
              mkt, " contract %{x}<br>$%{y:.2f}<br>",
              format(d, "%Y-%m-%d"), "<extra></extra>"
            )
          )
        }
      }

      plotly::layout(
        p,
        xaxis     = list(title = "Contract # (1 = front month)"),
        yaxis     = list(title = "Futures Price (USD)"),
        legend    = list(orientation = "h", y = -0.2),
        hovermode = "x unified"
      )
    })

    # ── US Treasury yield curve ─────────────────────────────────────────────
    output$yield_curve <- plotly::renderPlotly({
      shiny::req(!is.null(r$cmt_data))
      dates     <- snap_dates()
      opacities <- seq(0.3, 1, length.out = length(dates))

      p <- plotly::plot_ly()

      for (i in seq_along(dates)) {
        d    <- dates[i]
        snap <- r$cmt_data |>
          dplyr::filter(date <= d) |>
          dplyr::filter(date == max(date)) |>
          dplyr::arrange(maturity_years)
        if (nrow(snap) == 0L) next

        # Inversion check: 2Y > 10Y
        y2  <- snap$value[which.min(abs(snap$maturity_years - 2))]
        y10 <- snap$value[which.min(abs(snap$maturity_years - 10))]
        inv_label <- if (length(y2) > 0 && length(y10) > 0 && y2 > y10) {
          " \u26a0 inverted"
        } else {
          ""
        }

        p <- plotly::add_trace(
          p,
          data      = snap,
          x         = ~maturity_years,
          y         = ~value,
          type      = "scatter",
          mode      = "lines+markers",
          name      = paste0("CMT (", format(d, "%b %d %Y"), ")", inv_label),
          opacity   = opacities[i],
          line      = list(color = market_colors["CMT"] %||% "#7f8c8d"),
          marker    = list(color = market_colors["CMT"] %||% "#7f8c8d"),
          hovertemplate = paste0(
            "Maturity: %{x:.1f}y<br>Yield: %{y:.2f}%<br>",
            format(d, "%Y-%m-%d"), "<extra></extra>"
          )
        )
      }

      plotly::layout(
        p,
        xaxis     = list(title = "Maturity (years)"),
        yaxis     = list(title = "Yield (%)"),
        legend    = list(orientation = "h", y = -0.2),
        hovermode = "x unified"
      )
    })

    # ── Roll yield time series ──────────────────────────────────────────────
    output$roll_yield <- plotly::renderPlotly({
      shiny::req(!is.null(r$data), r$selected_markets, r$date_range)
      mkts <- setdiff(r$selected_markets, "CMT")
      shiny::req(length(mkts) > 0L)

      p <- plotly::plot_ly()

      for (mkt in mkts) {
        c1 <- r$data |>
          dplyr::filter(market == mkt, contract == 1L,
                        date >= r$date_range[1], date <= r$date_range[2]) |>
          dplyr::select(date, c1 = value)
        c2 <- r$data |>
          dplyr::filter(market == mkt, contract == 2L,
                        date >= r$date_range[1], date <= r$date_range[2]) |>
          dplyr::select(date, c2 = value)

        ry <- dplyr::inner_join(c1, c2, by = "date") |>
          dplyr::mutate(roll_yield = (c1 / c2 - 1) * 12)   # annualised monthly roll

        if (nrow(ry) == 0L) next
        col <- market_colors[mkt] %||% "#888888"

        p <- plotly::add_trace(
          p,
          data = ry, x = ~date, y = ~roll_yield,
          type = "scatter", mode = "lines",
          name = mkt,
          line = list(color = col),
          hovertemplate = paste0(mkt, "<br>%{x|%Y-%m-%d}<br>Roll yield: %{y:.1%}/yr<extra></extra>")
        )
      }

      # Zero reference line
      dr <- r$date_range
      p <- plotly::add_trace(p,
        x = dr, y = c(0, 0),
        type = "scatter", mode = "lines",
        line = list(color = "grey", dash = "dot", width = 1),
        showlegend = FALSE, hoverinfo = "skip")

      plotly::layout(p,
        title     = "Annualised Roll Yield — C1/C2 spread as % of spot (positive = backwardation)",
        xaxis     = list(title = "Date"),
        yaxis     = list(title = "Roll Yield (annualised)", tickformat = ".1%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # ── Macro Context: 2Y-10Y spread vs commodity ───────────────────────────
    output$macro_context <- plotly::renderPlotly({
      shiny::req(!is.null(r$cmt_data), !is.null(r$data), r$date_range)
      mkt <- input$macro_overlay

      # 2Y and 10Y yields from CMT data
      y2 <- r$cmt_data |>
        dplyr::filter(abs(maturity_years - 2)  < 0.1) |>
        dplyr::select(date, y2 = value)
      y10 <- r$cmt_data |>
        dplyr::filter(abs(maturity_years - 10) < 0.1) |>
        dplyr::select(date, y10 = value)

      spread_df <- dplyr::inner_join(y2, y10, by = "date") |>
        dplyr::mutate(spread = y10 - y2) |>
        dplyr::filter(date >= r$date_range[1], date <= r$date_range[2]) |>
        dplyr::arrange(date)

      shiny::req(nrow(spread_df) > 0L)

      # Commodity front-month
      comm_df <- r$data |>
        dplyr::filter(market == mkt, contract == 1L,
                      date >= r$date_range[1], date <= r$date_range[2]) |>
        dplyr::select(date, price = value) |>
        dplyr::arrange(date)

      col <- market_colors[mkt] %||% "#2980b9"

      plotly::plot_ly() |>
        plotly::add_trace(
          data          = spread_df,
          x             = ~date, y = ~spread,
          type          = "scatter", mode = "lines",
          name          = "2Y\u201310Y spread",
          fill          = "tozeroy",
          fillcolor      = "rgba(127,140,141,0.15)",
          line          = list(color = "#7f8c8d", width = 1.5),
          hovertemplate = "%{x|%Y-%m-%d}<br>2Y\u201310Y: %{y:.2f}%<extra></extra>"
        ) |>
        plotly::add_trace(
          data          = comm_df,
          x             = ~date, y = ~price,
          type          = "scatter", mode = "lines",
          name          = paste0(mkt, " front-month"),
          yaxis         = "y2",
          line          = list(color = col, width = 2),
          hovertemplate = paste0(mkt, " %{x|%Y-%m-%d}<br>$%{y:.2f}<extra></extra>")
        ) |>
        plotly::add_trace(
          x = range(spread_df$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = alert_color, dash = "dash", width = 1),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::layout(
          xaxis     = list(title = ""),
          yaxis     = list(title = "2Y\u201310Y Spread (%)"),
          yaxis2    = list(title = paste0(mkt, " Price (USD)"),
                           overlaying = "y", side = "right", showgrid = FALSE),
          hovermode = "x unified",
          legend    = list(orientation = "h", y = -0.2)
        )
    })

  })
}
