#' hedge_ratios UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Cross-market hedge ratio: rolling OLS beta of Market A on Market B.
#'   \item Term-structure hedge ratio: for a selected market, rolling OLS beta
#'         of contract 1 on each successive contract (2, 3, ...).
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput numericInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_hedge_ratios_ui <- function(id) {
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
                     "1 quarter (63 days)" = 63,
                     "6 months (126 days)" = 126,
                     "1 year (252 days)"   = 252),
        selected = 63
      ),
      shiny::selectInput(ns("pair_a"), "Cross-market — Hedge Asset (Y)", choices = NULL),
      shiny::selectInput(ns("pair_b"), "Cross-market — Hedging Instrument (X)", choices = NULL),
      shiny::selectInput(
        ns("ts_market"),
        "Term-structure market",
        choices  = c("CL","BRN","NG","HO","RB","HTT"),
        selected = "CL"
      ),
      shiny::numericInput(
        ns("ts_max_contract"),
        "Max contract to compare against C1",
        value = 6, min = 2, max = 24, step = 1
      )
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header(
        "Hedge Ratio Dynamics — rolling OLS beta (log returns, minimum-variance)"
      ),
      bslib::navset_tab(
        bslib::nav_panel(
          "Cross-Market Beta",
          plotly::plotlyOutput(ns("cross_beta"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Beta (left axis): the minimum-variance hedge ratio \u2014 how many contracts of Market B you need to short in order to hedge 1 contract of Market A.
             A beta of 0.95 means: short 0.95 units of B for every 1 unit of A. If beta drifts over time, your hedge ratio needs to be rebalanced.
             The minimum-variance approach finds the beta that minimises the residual (unhedged) risk, estimated from rolling log returns to avoid spurious regression on non-stationary price levels.
             R\u00b2 (right axis, blue dashed, 0\u20131): how much of Market A\u2019s daily variance the hedge actually explains. R\u00b2 of 0.90 means 90% of A\u2019s moves are captured by the hedge; 10% is unhedged basis risk.
             Basis risk % (right axis, green): the unhedgeable component expressed as a fraction of total volatility. Low R\u00b2 + high basis risk = the hedge is unreliable even if beta is stable.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Term-Structure Beta (C1 vs Cn)",
          plotly::plotlyOutput(ns("ts_beta"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "If you produce or buy crude at today\u2019s spot price (C1) but want to hedge using a longer-dated contract (C2, C3...), you need to know how many deferred contracts to use per unit of front-month exposure.
             This chart shows that ratio (the beta of C1 on each Cn) estimated on a rolling window of daily log returns.
             In backwardation, front-month prices move more violently than deferred prices \u2014 so beta > 1, meaning you need more than 1 deferred contract to hedge 1 front-month unit.
             In contango, the curve is flatter and all contracts move more similarly \u2014 betas converge toward 1.
             A beta that drifts over time means your hedge ratio needs to be recalculated regularly, not set once and forgotten.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        )
      )
    )
  )
}

#' hedge_ratios Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive observe req updateSelectInput
#' @importFrom dplyr filter arrange mutate group_by ungroup select inner_join rename
#' @importFrom tidyr pivot_wider
#' @importFrom slider slide2_dbl slide_dbl
#' @importFrom stats lm coef residuals sd complete.cases
#' @importFrom grDevices hcl
#' @importFrom plotly plot_ly layout renderPlotly add_trace
mod_hedge_ratios_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Update pair dropdowns when selected markets change
    observe({
      mkts <- r$selected_markets
      req(length(mkts) >= 1L)
      shiny::updateSelectInput(session, "pair_a", choices = mkts, selected = mkts[1])
      shiny::updateSelectInput(session, "pair_b", choices = mkts,
                               selected = mkts[min(2L, length(mkts))])
    })

    # Helper: rolling OLS beta — slides over y_vec and x_vec in parallel
    rolling_beta <- function(y_vec, x_vec, window) {
      slider::slide2_dbl(
        y_vec, x_vec,
        function(.y, .x) {
          sub <- data.frame(y = .y, x = .x)
          sub <- sub[stats::complete.cases(sub), , drop = FALSE]
          if (nrow(sub) < 10L) return(NA_real_)
          fit <- stats::lm(y ~ x, data = sub)
          stats::coef(fit)[["x"]]
        },
        .before   = window - 1L,
        .complete = TRUE
      )
    }

    # Helper: rolling OLS R² — same window logic
    rolling_r2 <- function(y_vec, x_vec, window) {
      slider::slide2_dbl(
        y_vec, x_vec,
        function(.y, .x) {
          sub <- data.frame(y = .y, x = .x)
          sub <- sub[stats::complete.cases(sub), , drop = FALSE]
          if (nrow(sub) < 10L) return(NA_real_)
          summary(stats::lm(y ~ x, data = sub))$r.squared
        },
        .before   = window - 1L,
        .complete = TRUE
      )
    }

    # Front-month LOG RETURNS in wide format.
    # OLS on price levels is spurious for non-stationary series.
    # Minimum-variance hedge ratio is estimated on returns: β = ρ(σ_y/σ_x).
    front_wide <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      prices <- r$data |>
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
        dplyr::select(date, market, log_ret) |>
        tidyr::pivot_wider(names_from = market, values_from = log_ret) |>
        dplyr::arrange(date)
      prices
    })

    # Cross-market rolling beta
    output$cross_beta <- plotly::renderPlotly({
      wide <- front_wide()
      a    <- input$pair_a
      b    <- input$pair_b
      req(!is.null(a), !is.null(b), a != b,
          a %in% names(wide), b %in% names(wide))
      win  <- as.integer(input$window)

      df_yx <- data.frame(
        date = wide$date,
        y    = wide[[a]],
        x    = wide[[b]]
      ) |>
        dplyr::filter(!is.na(y), !is.na(x)) |>
        dplyr::mutate(
          beta    = rolling_beta(y, x, win),
          r2      = rolling_r2(y, x, win),
          # Basis risk = SD of residual / SD of Y — the unhedgeable fraction
          res_sd  = slider::slide2_dbl(y, x,
            function(.y, .x) {
              sub <- data.frame(y = .y, x = .x)
              sub <- sub[stats::complete.cases(sub), ]
              if (nrow(sub) < 10L) return(NA_real_)
              stats::sd(stats::residuals(stats::lm(y ~ x, data = sub)))
            },
            .before = win - 1L, .complete = TRUE),
          y_sd    = slider::slide_dbl(y, ~ stats::sd(.x, na.rm = TRUE),
                      .before = win - 1L, .complete = TRUE),
          basis_pct = res_sd / y_sd * 100
        )

      plotly::plot_ly(
        data = df_yx, x = ~date, y = ~beta,
        type = "scatter", mode = "lines",
        name = "Beta",
        line = list(color = alert_color),
        hovertemplate = "%{x|%Y-%m-%d}<br>Beta: %{y:.3f}<extra></extra>"
      ) |>
        plotly::add_trace(
          x = range(df_yx$date, na.rm = TRUE), y = c(1, 1),
          type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dot"),
          showlegend = FALSE, hoverinfo = "skip"
        ) |>
        plotly::add_trace(
          data  = df_yx, x = ~date, y = ~r2,
          type  = "scatter", mode = "lines",
          name  = "R\u00b2",
          yaxis = "y2",
          line  = list(color = "#2980b9", dash = "dot"),
          hovertemplate = "%{x|%Y-%m-%d}<br>R\u00b2: %{y:.3f}<extra></extra>"
        ) |>
        plotly::add_trace(
          data  = df_yx, x = ~date, y = ~basis_pct,
          type  = "scatter", mode = "lines",
          name  = "Basis risk %",
          yaxis = "y3",
          line  = list(color = "#27ae60", dash = "dashdot", width = 1.2),
          hovertemplate = "%{x|%Y-%m-%d}<br>Basis risk: %{y:.1f}% of \u03c3<sub>Y</sub><extra></extra>"
        ) |>
        plotly::layout(
          title  = paste0("Hedge ratio (log returns): ", a, " ~ ", b, ", ", win, "-day window"),
          xaxis  = list(title = "Date", domain = c(0, 0.88)),
          yaxis  = list(title = "Beta (min-var hedge ratio)"),
          yaxis2 = list(title = "R\u00b2", overlaying = "y", side = "right",
                        range = c(0, 1), showgrid = FALSE, anchor = "x"),
          yaxis3 = list(title = "Basis risk (%)", overlaying = "y", side = "right",
                        showgrid = FALSE, anchor = "free", position = 0.93),
          legend = list(orientation = "h", y = -0.2)
        )
    })

    # Term-structure rolling beta: contract 1 vs contracts 2..N
    output$ts_beta <- plotly::renderPlotly({
      req(!is.null(r$data), r$date_range)
      mkt      <- input$ts_market
      max_c    <- as.integer(input$ts_max_contract)
      win      <- as.integer(input$window)

      mkt_data <- r$data |>
        dplyr::filter(
          market == mkt,
          date   >= r$date_range[1],
          date   <= r$date_range[2]
        )

      # Use log returns for stationarity; compute for each contract separately.
      log_ret_for <- function(contract_n) {
        mkt_data |>
          dplyr::filter(contract == contract_n) |>
          dplyr::arrange(date) |>
          dplyr::mutate(ret = c(NA_real_, diff(log(value)))) |>
          dplyr::filter(!is.na(ret)) |>
          dplyr::select(date, ret)
      }

      c1 <- log_ret_for(1L) |> dplyr::rename(c1_ret = ret)

      p <- plotly::plot_ly()

      for (cn in seq(2L, max_c)) {
        cn_data <- log_ret_for(cn) |>
          dplyr::rename(cn_ret = ret) |>
          dplyr::inner_join(c1, by = "date") |>
          dplyr::arrange(date) |>
          dplyr::mutate(beta = rolling_beta(c1_ret, cn_ret, win)) |>
          dplyr::filter(!is.na(beta))

        if (nrow(cn_data) == 0L) next

        col <- if (cn <= 6L) {
          grDevices::hcl(h = (cn - 2L) * 40, c = 80, l = 45)
        } else "#888888"
        p <- plotly::add_trace(
          p,
          data = cn_data, x = ~date, y = ~beta,
          type = "scatter", mode = "lines",
          name = paste0("C1 vs C", cn),
          line = list(color = col),
          hovertemplate = paste0("C1 vs C", cn,
                                 "<br>%{x|%Y-%m-%d}<br>Beta: %{y:.3f}<extra></extra>")
        )
      }

      plotly::layout(
        p,
        title  = paste0(mkt, " — term-structure hedge ratios (log returns, C1 as Y)"),
        xaxis  = list(title = "Date"),
        yaxis  = list(title = "Beta"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

  })
}
