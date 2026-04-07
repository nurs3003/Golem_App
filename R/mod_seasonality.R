#' seasonality UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Monthly average log-return bar chart — reveals seasonal biases.
#'   \item Box plots of monthly returns — shows dispersion and outliers.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_seasonality_ui <- function(id) {
  ns <- NS(id)
  bslib::card(
    fill = TRUE,
    bslib::card_header(
      "Seasonal Patterns — monthly log returns, front-month contracts"
    ),
    bslib::navset_tab(
      bslib::nav_panel(
        "Average Monthly Return",
        plotly::plotlyOutput(ns("monthly_avg"), height = "calc(100% - 2.8rem)"),
        shiny::tags$p(
          "Historical average daily log-return by calendar month, computed across all years in the selected date range. Positive bars = months that have been systematically bullish on average.
           Key patterns: CL and BRN tend to be weakest in Q1 (demand lull after winter) and strongest heading into summer driving season.
           HO is typically strongest Oct\u2013Dec as heating demand builds ahead of winter, and weakest in spring.
           NG prices typically bottom in March after the winter draw ends, then rise through late summer as the market prices in the next heating season \u2014 Jan\u2013Feb returns are often negative because winter demand is already priced in well before it arrives.
           RB tends to rally Mar\u2013May as refiners switch to more expensive summer-spec gasoline and driving demand picks up.
           Important: these are averages \u2014 any individual year can deviate significantly due to geopolitical events or supply shocks.",
          style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
        )
      ),
      bslib::nav_panel(
        "Monthly Distribution (box)",
        plotly::plotlyOutput(ns("monthly_box"), height = "calc(100% - 2.8rem)"),
        shiny::tags$p(
          "Full return distribution by calendar month. The box spans the 25th\u201375th percentile (the middle half of all observations); the centre line is the median; whiskers extend to the 10th and 90th percentile; dots beyond are outliers.
           Wide box = high dispersion, the seasonal pattern is unreliable. Narrow box = tight, repeatable pattern.
           Compare the median (centre line) to the bar chart average: if the average bar is positive but the median line sits near zero,
           the positive average is being driven by a few extreme years \u2014 not a pattern you can rely on.
           Long whiskers or outlier dots signal tail risk: individual months can produce very large gains or losses.",
          style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
        )
      ),
      bslib::nav_panel(
        "Year-over-Year",
        plotly::plotlyOutput(ns("yoy"), height = "calc(100% - 2.8rem)"),
        shiny::tags$p(
          "Cumulative log return for the current calendar year (bold line) vs the historical interquartile range (shaded band) built from all prior years.
           The band shows where the middle 50% of historical years sat on each day of the year.
           If the bold line is above the band, this year is outperforming 75% of all historical years at this point in the calendar.
           If it is below the band, performance is in the bottom 25% historically.
           This is a descriptive context tool \u2014 it shows where the current year sits relative to history, not a forecast of where it will go.",
          style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
        )
      )
    )
  )
}

#' seasonality Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req
#' @importFrom dplyr filter arrange mutate group_by ungroup summarise
#' @importFrom lubridate month year yday
#' @importFrom stats quantile
#' @importFrom utils head
#' @importFrom plotly plot_ly layout renderPlotly add_trace
mod_seasonality_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    month_labels <- c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")

    # Reactive: monthly log returns for front-month, selected markets
    monthly_returns <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      r$data |>
        dplyr::filter(
          market   %in% r$selected_markets,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::arrange(market, date) |>
        dplyr::group_by(market) |>
        dplyr::mutate(log_ret = c(NA_real_, diff(log(value)))) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(log_ret)) |>
        dplyr::mutate(month = lubridate::month(date, label = FALSE))
    })

    # Average return by month
    output$monthly_avg <- plotly::renderPlotly({
      df <- monthly_returns()
      req(nrow(df) > 0L)

      avg_df <- df |>
        dplyr::group_by(market, month) |>
        dplyr::summarise(avg_ret = mean(log_ret, na.rm = TRUE), .groups = "drop") |>
        dplyr::mutate(month_lbl = month_labels[month])

      p <- plotly::plot_ly()
      for (mkt in unique(avg_df$market)) {
        sub <- dplyr::filter(avg_df, market == mkt) |>
          dplyr::arrange(month)
        col <- market_colors[mkt] %||% "#888888"
        p <- plotly::add_trace(
          p,
          data          = sub,
          x             = ~month_lbl,
          y             = ~avg_ret,
          type          = "bar",
          name          = mkt,
          marker        = list(color = col),
          hovertemplate = "%{x}: %{y:.2%}<extra></extra>"
        )
      }
      plotly::layout(
        p,
        barmode   = "group",
        xaxis     = list(title = "Month", categoryorder = "array",
                         categoryarray = month_labels),
        yaxis     = list(title = "Avg. Log Return", tickformat = ".1%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # Box plot of returns by month
    output$monthly_box <- plotly::renderPlotly({
      df <- monthly_returns()
      req(nrow(df) > 0L)

      df <- df |>
        dplyr::mutate(month_lbl = month_labels[month])

      p <- plotly::plot_ly()
      for (mkt in unique(df$market)) {
        sub <- dplyr::filter(df, market == mkt)
        col <- market_colors[mkt] %||% "#888888"
        p <- plotly::add_trace(
          p,
          data   = sub,
          x      = ~month_lbl,
          y      = ~log_ret,
          type   = "box",
          name   = mkt,
          marker = list(color = col),
          line   = list(color = col),
          hovertemplate = "%{x}<br>%{y:.2%}<extra></extra>"
        )
      }
      plotly::layout(
        p,
        xaxis  = list(title = "Month", categoryorder = "array",
                      categoryarray = month_labels),
        yaxis  = list(title = "Log Return", tickformat = ".1%"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

    # Year-over-year: current year (bold) vs historical IQR band per market
    output$yoy <- plotly::renderPlotly({
      df_raw <- monthly_returns()
      req(nrow(df_raw) > 0L)

      current_year <- lubridate::year(Sys.Date())

      df_yoy <- df_raw |>
        dplyr::mutate(
          year = lubridate::year(date),
          doy  = lubridate::yday(date)
        ) |>
        dplyr::arrange(market, year, doy) |>
        dplyr::group_by(market, year) |>
        dplyr::mutate(cum_ret = cumsum(log_ret)) |>
        dplyr::ungroup()

      mkts <- unique(df_yoy$market)

      # hex -> rgba helper for IQR band fill
      hex_to_rgba <- function(hex, alpha = 0.18) {
        r <- strtoi(substr(hex, 2, 3), 16L)
        g <- strtoi(substr(hex, 4, 5), 16L)
        b <- strtoi(substr(hex, 6, 7), 16L)
        sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
      }

      p <- plotly::plot_ly()

      for (i in seq_along(mkts)) {
        mkt      <- mkts[i]
        col      <- market_colors[mkt] %||% "#888888"
        fill_col <- hex_to_rgba(col)
        mkt_df   <- dplyr::filter(df_yoy, market == mkt)

        # Historical IQR by day-of-year (all years before current)
        hist_df <- dplyr::filter(mkt_df, year < current_year) |>
          dplyr::group_by(doy) |>
          dplyr::summarise(
            q25 = stats::quantile(cum_ret, 0.25, na.rm = TRUE),
            q75 = stats::quantile(cum_ret, 0.75, na.rm = TRUE),
            .groups = "drop"
          ) |>
          dplyr::arrange(doy)

        # Current-year trace
        curr_df <- dplyr::filter(mkt_df, year == current_year) |>
          dplyr::arrange(doy)

        if (nrow(hist_df) > 0L) {
          # Lower bound (invisible) — must be added first for tonexty fill
          p <- plotly::add_trace(p,
            data = hist_df, x = ~doy, y = ~q25,
            type = "scatter", mode = "lines",
            line = list(color = "transparent"),
            legendgroup = mkt, showlegend = FALSE,
            hoverinfo = "skip", name = paste0(mkt, "_q25")
          )
          # Upper bound with fill back to lower
          p <- plotly::add_trace(p,
            data      = hist_df, x = ~doy, y = ~q75,
            type      = "scatter", mode = "lines",
            fill      = "tonexty",
            fillcolor = fill_col,
            line      = list(color = "transparent"),
            legendgroup = mkt, showlegend = TRUE,
            name      = paste0(mkt, " IQR"),
            hovertemplate = paste0(mkt, " IQR<br>Day %{x}<br>%{y:.1%}<extra></extra>")
          )
        }

        if (nrow(curr_df) > 0L) {
          p <- plotly::add_trace(p,
            data = curr_df, x = ~doy, y = ~cum_ret,
            type = "scatter", mode = "lines",
            name = paste0(mkt, " (", current_year, ")"),
            legendgroup = mkt,
            line = list(color = col, width = 2.5),
            hovertemplate = paste0(mkt, " ", current_year,
                                   "<br>Day %{x}<br>%{y:.2%}<extra></extra>")
          )
        }
      }

      # Month-boundary tick marks so "day of year" is human-readable
      month_starts <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
      month_abbr   <- c("Jan","Feb","Mar","Apr","May","Jun",
                        "Jul","Aug","Sep","Oct","Nov","Dec")

      plotly::layout(p,
        title     = paste0("Year-over-Year \u2014 ", current_year,
                           " cumulative return vs historical IQR"),
        xaxis     = list(
          title       = "",
          tickvals    = month_starts,
          ticktext    = month_abbr,
          tickangle   = 0
        ),
        yaxis     = list(title = "Cumulative Log Return", tickformat = ".1%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

  })
}
