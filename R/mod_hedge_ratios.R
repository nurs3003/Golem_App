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
      shiny::numericInput(
        ns("window"),
        "Rolling window (trading days)",
        value = 63, min = 21, max = 252, step = 21
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
        "Hedge Ratio Dynamics — rolling OLS beta (price levels)"
      ),
      bslib::navset_tab(
        bslib::nav_panel(
          "Cross-Market Beta",
          plotly::plotlyOutput(ns("cross_beta"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Beta (left axis): units of Market B needed to hedge 1 unit of Market A.
             R\u00b2 (right axis, 0\u20131): fraction of Market A\u2019s variance explained by the hedge \u2014
             a low R\u00b2 means the hedge is unreliable even if beta looks stable.",
            style = "font-size:0.82rem; color:#666; padding:0.2rem 0.6rem; margin:0;"
          )
        ),
        bslib::nav_panel(
          "Term-Structure Beta (C1 vs Cn)",
          plotly::plotlyOutput(ns("ts_beta"), height = "calc(100% - 2.8rem)"),
          shiny::tags$p(
            "Rolling OLS beta of C1 on deferred contracts. In backwardation, C1 moves more than Cn so beta > 1;
             in contango the curve flattens and betas converge toward 1. Used to size calendar spread hedges.",
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
#' @importFrom slider slide2_dbl
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

    # Front-month prices in wide format
    front_wide <- reactive({
      req(!is.null(r$data), r$selected_markets, r$date_range)
      r$data |>
        dplyr::filter(
          market   %in% r$selected_markets,
          contract == 1L,
          date     >= r$date_range[1],
          date     <= r$date_range[2]
        ) |>
        dplyr::select(date, market, value) |>
        tidyr::pivot_wider(names_from = market, values_from = value) |>
        dplyr::arrange(date)
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
          beta = rolling_beta(y, x, win),
          r2   = rolling_r2(y, x, win)
        )

      plotly::plot_ly(
        data = df_yx, x = ~date, y = ~beta,
        type = "scatter", mode = "lines",
        name = "Beta",
        line = list(color = "#e74c3c"),
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
        plotly::layout(
          title  = paste0("Hedge ratio: ", a, " (Y) ~ ", b, " (X), ", win, "-day window"),
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Beta (hedge ratio)"),
          yaxis2 = list(title = "R\u00b2", overlaying = "y", side = "right",
                        range = c(0, 1), showgrid = FALSE),
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

      c1 <- mkt_data |>
        dplyr::filter(contract == 1L) |>
        dplyr::select(date, c1_price = value)

      p <- plotly::plot_ly()

      for (cn in seq(2L, max_c)) {
        cn_data <- mkt_data |>
          dplyr::filter(contract == cn) |>
          dplyr::select(date, cn_price = value) |>
          dplyr::inner_join(c1, by = "date") |>
          dplyr::arrange(date) |>
          dplyr::mutate(beta = rolling_beta(c1_price, cn_price, win)) |>
          dplyr::filter(!is.na(beta))

        if (nrow(cn_data) == 0L) next

        p <- plotly::add_trace(
          p,
          data = cn_data, x = ~date, y = ~beta,
          type = "scatter", mode = "lines",
          name = paste0("C1 vs C", cn),
          hovertemplate = paste0("C1 vs C", cn,
                                 "<br>%{x|%Y-%m-%d}<br>Beta: %{y:.3f}<extra></extra>")
        )
      }

      plotly::layout(
        p,
        title  = paste0(mkt, " — term-structure hedge ratios (C1 as hedge asset)"),
        xaxis  = list(title = "Date"),
        yaxis  = list(title = "Beta"),
        legend = list(orientation = "h", y = -0.2)
      )
    })

  })
}
