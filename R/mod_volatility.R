#' volatility UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Rolling annualised volatility of the front-month contract over time.
#'   \item Volatility surface heatmap: time on x, contract number on y,
#'         colour = annualised vol.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_volatility_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("window"),
        "Rolling window",
        choices  = c("1 month (21 days)"   = 21,
                     "2 months (42 days)"  = 42,
                     "1 quarter (63 days)" = 63),
        selected = 21
      ),
      shiny::selectInput(
        ns("surface_market"),
        "Vol surface market",
        choices  = c("CL","BRN","NG","HO","RB","HTT"),
        selected = "CL"
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Volatility"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Rolling Vol (front month)",
          plotly::plotlyOutput(ns("vol_ts"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Annualised volatility from daily log returns. The dashed red line marks the 80th-percentile threshold
             across selected markets — sustained readings above it signal a high-vol regime
             (e.g. COVID Mar 2020, Russia-Ukraine Feb 2022).",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Vol Surface (heatmap)",
          plotly::plotlyOutput(ns("vol_heatmap"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Each cell shows annualised vol for a given contract and date, sampled weekly (Mondays) to keep the chart responsive.
             Near-term contracts (C1\u2013C3) are typically more volatile than deferred contracts because nearby prices
             react more sharply to supply/demand shocks \u2014 deferred prices are anchored by longer-term expectations.
             Horizontal red bands spanning all contract months mark market-wide stress events.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Vol Cone",
          plotly::plotlyOutput(ns("vol_cone"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Historical distribution of realized vol at each lookback horizon (10d to 252d).
             The box spans the 25th\u201375th percentile; whiskers show 10th\u201390th.
             The red dot is current realized vol at each horizon.
             Current vol above the 75th percentile = elevated regime; above the 90th = stress.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Value at Risk",
          plotly::plotlyOutput(ns("var_plot"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Rolling 1-day historical VaR at 95% and 99% confidence (252-day lookback window).
             VaR is expressed in USD per contract (1,000 bbl for CL/BRN/HTT, 42,000 gal for HO/RB, 10,000 MMBtu for NG).
             Spikes in VaR coincide with vol regimes: COVID Mar 2020, Russia-Ukraine Feb 2022.
             99% VaR breaches occur on average 2\u20133 times per year under normal conditions.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        )
      )
    )
  )
}

#' volatility Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req observe isolate updateSelectInput
#' @importFrom dplyr filter arrange mutate group_by ungroup select bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom slider slide_dbl
#' @importFrom stats sd quantile
#' @importFrom utils tail
#' @importFrom plotly plot_ly add_trace layout renderPlotly
mod_volatility_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Keep vol surface dropdown in sync with selected markets
    shiny::observe({
      mkts <- setdiff(r$selected_markets, "CMT")
      shiny::req(length(mkts) > 0L)
      cur <- isolate(input$surface_market)
      sel <- if (!is.null(cur) && cur %in% mkts) cur else mkts[1]
      shiny::updateSelectInput(session, "surface_market", choices = mkts, selected = sel)
    })

    # Reactive: log returns for front-month contracts of selected markets
    front_returns <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      window_days <- as.integer(input$window)

      r$data |>
        dplyr::filter(
          market   %in% r$selected_markets,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::arrange(market, date) |>
        dplyr::group_by(market) |>
        dplyr::mutate(
          log_ret  = c(NA_real_, diff(log(value))),
          roll_vol = slider::slide_dbl(
            log_ret,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
            .before   = window_days - 1L,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup()
    })

    # Rolling vol time-series
    output$vol_ts <- plotly::renderPlotly({
      df <- front_returns()
      req(nrow(df) > 0L)

      p <- plotly::plot_ly()
      for (mkt in unique(df$market)) {
        sub <- dplyr::filter(df, market == mkt, !is.na(roll_vol))
        col <- market_colors[mkt] %||% "#888888"
        p <- plotly::add_trace(
          p,
          data = sub, x = ~date, y = ~roll_vol,
          type = "scatter", mode = "lines",
          name = mkt,
          line = list(color = col),
          hovertemplate = "%{x|%Y-%m-%d}<br>Ann. Vol: %{y:.1%}<extra></extra>"
        )
      }

      # 80th-percentile regime threshold across all selected markets
      all_vols <- df$roll_vol[!is.na(df$roll_vol)]
      if (length(all_vols) > 0L) {
        threshold  <- stats::quantile(all_vols, 0.80)
        date_range <- range(df$date[!is.na(df$roll_vol)])
        p <- plotly::add_trace(
          p,
          x    = date_range,
          y    = c(threshold, threshold),
          type = "scatter", mode = "lines",
          name = "80th pctile (regime)",
          line = list(color = alert_color, dash = "dash", width = 1.5),
          hoverinfo = "skip"
        )
      }

      plotly::layout(
        p,
        xaxis     = list(title = "Date"),
        yaxis     = list(title = "Annualised Volatility", tickformat = ".0%"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

    # Vol surface heatmap for selected market
    output$vol_heatmap <- plotly::renderPlotly({
      req(!is.null(r$data), r$date_range)
      mkt         <- input$surface_market
      window_days <- as.integer(input$window)

      surface_df <- r$data |>
        dplyr::filter(
          market == mkt,
          date   >= r$date_range[1],
          date   <= r$date_range[2]
        ) |>
        dplyr::arrange(contract, date) |>
        dplyr::group_by(contract) |>
        dplyr::mutate(
          log_ret  = c(NA_real_, diff(log(value))),
          roll_vol = slider::slide_dbl(
            log_ret,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
            .before   = window_days - 1L,
            .complete = TRUE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::filter(!is.na(roll_vol))

      # Downsample dates for heatmap performance (weekly)
      surface_df <- surface_df |>
        dplyr::filter(lubridate::wday(date) == 2L)  # Mondays only

      wide <- tidyr::pivot_wider(
        surface_df,
        id_cols     = date,
        names_from  = contract,
        values_from = roll_vol
      )

      mat      <- as.matrix(wide[, -1])
      dates    <- wide$date
      contracts <- as.integer(colnames(wide)[-1])

      plotly::plot_ly(
        x         = dates,
        y         = contracts,
        z         = t(mat),
        type      = "heatmap",
        colorscale = "RdYlGn",
        reversescale = TRUE,
        hovertemplate = "Date: %{x|%Y-%m-%d}<br>Contract: %{y}<br>Vol: %{z:.1%}<extra></extra>"
      ) |>
        plotly::layout(
          xaxis = list(title = "Date"),
          yaxis = list(title = "Contract #"),
          title = paste0(mkt, " Volatility Surface")
        )
    })

    # Vol cone: historical vol distribution by lookback horizon
    output$vol_cone <- plotly::renderPlotly({
      req(!is.null(r$data), r$date_range)
      mkt         <- input$surface_market
      horizons    <- c(10L, 21L, 63L, 126L, 252L)
      horizon_lbl <- c("10d", "21d", "63d", "126d", "252d")

      front_data <- r$data |>
        dplyr::filter(market == mkt, contract == 1L,
                      date >= r$date_range[1], date <= r$date_range[2]) |>
        dplyr::arrange(date) |>
        dplyr::mutate(log_ret = c(NA_real_, diff(log(value)))) |>
        dplyr::filter(!is.na(log_ret))

      req(nrow(front_data) > 252L)

      rets <- front_data$log_ret

      # For each horizon compute: full-history distribution + current value
      cone_rows <- lapply(seq_along(horizons), function(i) {
        h    <- horizons[i]
        vols <- slider::slide_dbl(rets, ~ stats::sd(.x, na.rm = TRUE) * sqrt(252),
                  .before = h - 1L, .complete = TRUE)
        vols <- vols[!is.na(vols)]
        cur  <- utils::tail(vols, 1L)
        data.frame(
          horizon = horizon_lbl[i],
          h_days  = h,
          p10     = stats::quantile(vols, 0.10),
          p25     = stats::quantile(vols, 0.25),
          p50     = stats::quantile(vols, 0.50),
          p75     = stats::quantile(vols, 0.75),
          p90     = stats::quantile(vols, 0.90),
          current = cur
        )
      })
      cone_df <- dplyr::bind_rows(cone_rows)

      col <- market_colors[mkt] %||% "#2980b9"

      plotly::plot_ly(data = cone_df, x = ~horizon) |>
        # 10th-90th whisker (invisible lower)
        plotly::add_trace(y = ~p10, type = "scatter", mode = "lines",
          line = list(color = "transparent"), showlegend = FALSE,
          hoverinfo = "skip", name = "_p10") |>
        plotly::add_trace(y = ~p90, type = "scatter", mode = "lines",
          fill = "tonexty", fillcolor = "rgba(150,150,150,0.15)",
          line = list(color = "transparent"), showlegend = FALSE,
          name = "10th\u201390th pctile",
          hovertemplate = "10th\u201390th: %{y:.1%}<extra></extra>") |>
        # 25th-75th box (invisible lower)
        plotly::add_trace(y = ~p25, type = "scatter", mode = "lines",
          line = list(color = "transparent"), showlegend = FALSE,
          hoverinfo = "skip", name = "_p25") |>
        plotly::add_trace(y = ~p75, type = "scatter", mode = "lines",
          fill = "tonexty", fillcolor = "rgba(100,100,200,0.25)",
          line = list(color = "transparent"), showlegend = FALSE,
          name = "25th\u201375th pctile",
          hovertemplate = "25th\u201375th: %{y:.1%}<extra></extra>") |>
        # Median line
        plotly::add_trace(y = ~p50, type = "scatter", mode = "lines+markers",
          line = list(color = "#555"), name = "Median",
          hovertemplate = "Median: %{y:.1%}<extra></extra>") |>
        # Current vol dot
        plotly::add_trace(y = ~current, type = "scatter", mode = "markers",
          marker = list(color = alert_color, size = 10, symbol = "circle"),
          name = "Current",
          hovertemplate = "Current: %{y:.1%}<extra></extra>") |>
        plotly::layout(
          title  = paste0(mkt, " Vol Cone — front-month realized vol by lookback horizon"),
          xaxis  = list(title = "Lookback horizon",
                        categoryorder = "array",
                        categoryarray = horizon_lbl),
          yaxis  = list(title = "Annualised Vol", tickformat = ".0%"),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    # Rolling historical VaR (1-day, 95% & 99%, 252-day lookback)
    output$var_plot <- plotly::renderPlotly({
      shiny::req(!is.null(r$data), r$selected_markets, r$date_range)
      mkts <- setdiff(r$selected_markets, "CMT")
      shiny::req(length(mkts) > 0L)

      # Contract size in natural units (USD per contract = VaR% x price x size)
      contract_size <- c(CL = 1000, BRN = 1000, HTT = 1000,
                         HO = 42000, RB = 42000, NG = 10000)

      var_window <- 252L
      p <- plotly::plot_ly()

      for (mkt in mkts) {
        fd <- r$data |>
          dplyr::filter(market == mkt, contract == 1L,
                        date >= r$date_range[1], date <= r$date_range[2]) |>
          dplyr::arrange(date) |>
          dplyr::mutate(log_ret = c(NA_real_, diff(log(value)))) |>
          dplyr::filter(!is.na(log_ret))

        if (nrow(fd) < var_window + 1L) next

        cs  <- contract_size[mkt] %||% 1000
        col <- market_colors[mkt] %||% "#888888"

        fd <- fd |>
          dplyr::mutate(
            var95 = slider::slide_dbl(
              log_ret,
              ~ abs(stats::quantile(.x, 0.05, na.rm = TRUE)),
              .before = var_window - 1L, .complete = TRUE
            ) * value * cs,
            var99 = slider::slide_dbl(
              log_ret,
              ~ abs(stats::quantile(.x, 0.01, na.rm = TRUE)),
              .before = var_window - 1L, .complete = TRUE
            ) * value * cs
          ) |>
          dplyr::filter(!is.na(var95))

        p <- plotly::add_trace(p,
          data = fd, x = ~date, y = ~var95,
          type = "scatter", mode = "lines",
          name = paste0(mkt, " 95% VaR"),
          line = list(color = col, width = 1.5),
          hovertemplate = paste0(mkt, " 95% VaR<br>%{x|%Y-%m-%d}<br>$%{y:,.0f}/contract<extra></extra>")
        )
        p <- plotly::add_trace(p,
          data = fd, x = ~date, y = ~var99,
          type = "scatter", mode = "lines",
          name = paste0(mkt, " 99% VaR"),
          line = list(color = col, width = 1.5, dash = "dash"),
          hovertemplate = paste0(mkt, " 99% VaR<br>%{x|%Y-%m-%d}<br>$%{y:,.0f}/contract<extra></extra>")
        )
      }

      plotly::layout(p,
        xaxis     = list(title = "Date"),
        yaxis     = list(title = "1-Day VaR (USD per contract)"),
        hovermode = "x unified",
        legend    = list(orientation = "h", y = -0.2)
      )
    })

  })
}
