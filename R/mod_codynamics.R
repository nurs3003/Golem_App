#' codynamics UI Function
#'
#' Two views:
#' \enumerate{
#'   \item Correlation matrix heatmap — snapshot at a user-selected date.
#'   \item Rolling pairwise correlation time-series for a chosen market pair.
#' }
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList selectInput dateInput
#' @importFrom bslib layout_columns card card_header navset_tab nav_panel
#' @importFrom plotly plotlyOutput
mod_codynamics_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(3, 9),
    fill       = TRUE,
    bslib::card(
      bslib::card_header("Settings"),
      shiny::selectInput(
        ns("window"),
        "Rolling window (trading days)",
        choices  = c("63 days (~1 qtr)" = 63, "126 days (~6 mo)" = 126, "252 days (~1 yr)" = 252),
        selected = 63
      ),
      shiny::dateInput(
        ns("snapshot_date"),
        "Matrix snapshot date",
        value = Sys.Date() - 30,
        min   = as.Date("2007-01-01"),
        max   = Sys.Date()
      ),
      shiny::selectInput(ns("pair_a"), "Pair — Market A", choices = NULL),
      shiny::selectInput(ns("pair_b"), "Pair — Market B", choices = NULL)
    ),
    bslib::card(
      fill = TRUE,
      bslib::card_header("Cross-Market Co-Dynamics"),
      bslib::navset_tab(
        bslib::nav_panel(
          "Correlation Matrix",
          plotly::plotlyOutput(ns("corr_matrix"), height = "100%")
        ),
        bslib::nav_panel(
          "Rolling Correlation (pair)",
          plotly::plotlyOutput(ns("rolling_corr"), height = "100%")
        )
      )
    )
  )
}

#' codynamics Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive observe req updateSelectInput
#' @importFrom dplyr filter arrange mutate group_by ungroup select
#' @importFrom tidyr pivot_wider
#' @importFrom slider slide_dbl slide2_dbl
#' @importFrom plotly plot_ly layout renderPlotly add_trace
mod_codynamics_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # Update pair dropdowns whenever selected markets change
    observe({
      mkts <- r$selected_markets
      req(length(mkts) >= 1L)
      shiny::updateSelectInput(session, "pair_a", choices = mkts, selected = mkts[1])
      shiny::updateSelectInput(session, "pair_b", choices = mkts, selected = mkts[min(2L, length(mkts))])
    })

    # Reactive: wide returns matrix (one column per market, front-month)
    wide_returns <- reactive({
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
        tidyr::pivot_wider(
          id_cols     = date,
          names_from  = market,
          values_from = log_ret
        ) |>
        dplyr::arrange(date)
    })

    # Correlation matrix at snapshot date
    output$corr_matrix <- plotly::renderPlotly({
      df   <- wide_returns()
      req(nrow(df) > 5L)
      snap <- input$snapshot_date
      win  <- as.integer(input$window)

      # Use the most recent `win` rows up to snap date
      sub <- df[df$date <= snap, , drop = FALSE]
      sub <- tail(sub, win)
      req(nrow(sub) >= 10L)

      mat <- stats::cor(sub[, -1, drop = FALSE], use = "pairwise.complete.obs")
      nms <- colnames(mat)

      plotly::plot_ly(
        x    = nms,
        y    = nms,
        z    = mat,
        type = "heatmap",
        colorscale  = "RdBu",
        reversescale = TRUE,
        zmid = 0,
        zmin = -1, zmax = 1,
        hovertemplate = "%{x} vs %{y}<br>Corr: %{z:.2f}<extra></extra>"
      ) |>
        plotly::layout(
          title  = paste0("Correlation Matrix — ", win, "-day window at ", format(snap)),
          xaxis  = list(title = ""),
          yaxis  = list(title = "", autorange = "reversed")
        )
    })

    # Rolling pairwise correlation for selected pair
    output$rolling_corr <- plotly::renderPlotly({
      df  <- wide_returns()
      req(nrow(df) > 5L)
      a   <- input$pair_a
      b   <- input$pair_b
      req(!is.null(a), !is.null(b), a != b)
      req(a %in% names(df), b %in% names(df))
      win <- as.integer(input$window)

      roll <- df |>
        dplyr::select(date, dplyr::all_of(c(a, b))) |>
        dplyr::filter(!is.na(.data[[a]]), !is.na(.data[[b]])) |>
        dplyr::rename(col_a = dplyr::all_of(a), col_b = dplyr::all_of(b)) |>
        dplyr::mutate(
          roll_corr = slider::slide2_dbl(
            col_a, col_b,
            function(.x, .y) {
              if (length(.x) < 10L) return(NA_real_)
              stats::cor(.x, .y, use = "complete.obs")
            },
            .before   = win - 1L,
            .complete = TRUE
          )
        )

      plotly::plot_ly(
        data = roll, x = ~date, y = ~roll_corr,
        type = "scatter", mode = "lines",
        line = list(color = "#2980b9"),
        hovertemplate = "%{x|%Y-%m-%d}<br>Corr: %{y:.2f}<extra></extra>"
      ) |>
        plotly::add_trace(
          x = range(roll$date), y = c(0, 0),
          type = "scatter", mode = "lines",
          line = list(color = "grey", dash = "dash"),
          showlegend = FALSE
        ) |>
        plotly::layout(
          title  = paste0(a, " vs ", b, " — ", win, "-day rolling correlation"),
          xaxis  = list(title = "Date"),
          yaxis  = list(title = "Correlation", range = c(-1, 1))
        )
    })

  })
}
