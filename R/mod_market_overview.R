#' market_overview UI Function
#'
#' Combined landing page and morning briefing.
#' Row 1 — color-coded DT: latest price, 1D/1W/1M/1Y returns, 21d vol,
#'          vol regime, curve shape, roll yield.
#' Row 2 — rich market profile cards: price drivers, relationships, risk angle.
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList tags uiOutput div
#' @importFrom bslib layout_columns card card_header
#' @importFrom DT DTOutput
mod_market_overview_ui <- function(id) {
  ns <- NS(id)
  shiny::div(
    style = "padding: 1rem; display: flex; flex-direction: column; gap: 1rem;",
    # Row 1 — morning briefing DT
    bslib::card(
      fill  = FALSE,
      bslib::card_header("Morning Briefing \u2014 latest prices, returns & regime signals"),
      shiny::tags$p(
        "Green 1D/1W = positive return. Red Vol Regime = above 1-year median vol.
         Backwardation = supply tight; Contango = oversupply / storage carry.
         Positive roll yield = earn by rolling; negative = pay to maintain exposure.",
        style = "font-size:0.82rem; color:#666; padding:0.4rem 1rem 0; margin:0;"
      ),
      DT::DTOutput(ns("briefing_table"))
    ),
    # Row 2 — market profile cards
    shiny::uiOutput(ns("profile_grid"))
  )
}

#' market_overview Server Function
#'
#' @param id Shiny module id.
#' @param r Shared \code{reactiveValues} environment.
#' @noRd
#' @importFrom shiny moduleServer reactive req renderUI
#' @importFrom dplyr filter arrange mutate select group_by ungroup bind_rows
#' @importFrom slider slide_dbl
#' @importFrom stats median sd quantile
#' @importFrom utils tail
#' @importFrom bslib layout_columns card card_header
#' @importFrom DT renderDT datatable formatStyle styleInterval styleEqual
mod_market_overview_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # ---------------------------------------------------------------------------
    # Market metadata
    # ---------------------------------------------------------------------------
    market_info <- list(

      CL = list(
        name       = "WTI Crude Oil",
        ticker     = "CL",
        unit       = "USD / bbl",
        exchange   = "NYMEX (CME Group)",
        contract   = "1,000 barrels of West Texas Intermediate crude, delivered at Cushing, Oklahoma.",
        drivers    = list(
          "OPEC+ production quotas and compliance \u2014 the single largest supply lever globally.",
          "US weekly inventory report (EIA): Cushing hub stocks move WTI more than any other data release.",
          "Permian Basin production growth \u2014 the US added ~6 mb/d of output from 2010 to 2023.",
          "Global demand signals: Chinese PMI, airline traffic, and freight activity.",
          "USD strength: crude is priced in dollars, so a stronger USD suppresses demand from non-US buyers.",
          "Geopolitical risk premium: Middle East tensions, Russian export flows, Strait of Hormuz."
        ),
        rel        = paste(
          "WTI is the North American benchmark. Its primary spread relationship is with Brent (BRN):",
          "WTI historically traded at a slight premium, but went to a $25 discount in 2011\u20132013 when",
          "Cushing storage filled and US crude could not be exported. After the US export ban was lifted",
          "in December 2015, the spread normalised. WTI is also the input to crack spreads \u2014 the margin",
          "refiners earn by turning crude into HO (distillate) and RB (gasoline)."
        ),
        risk_angle = paste(
          "A risk manager with crude exposure tracks WTI as the hedge vehicle of choice for US barrels.",
          "The term structure (contango vs backwardation) determines the cost of rolling a long position:",
          "in contango you pay to roll; in backwardation you earn. Calendar spread dynamics therefore",
          "directly affect the P&L of any hedged inventory position."
        )
      ),

      BRN = list(
        name       = "Brent Crude Oil",
        ticker     = "BRN",
        unit       = "USD / bbl",
        exchange   = "ICE Futures Europe",
        contract   = "1,000 barrels of Brent blend crude, delivered FOB at Sullom Voe, North Sea.",
        drivers    = list(
          "North Sea field production (Brent, Forties, Oseberg, Ekofisk, Troll \u2014 the BFOET basket).",
          "European and Asian demand: Brent is the benchmark for Atlantic Basin and Asian crude trade.",
          "OPEC+ output cuts affect Brent more directly than WTI because most OPEC crude is priced off Brent.",
          "Tanker freight rates: Brent crude must be shipped; rising freight costs widen the Brent-WTI spread.",
          "Refinery margins in Europe and Asia drive physical demand for Brent-linked grades.",
          "Geopolitical risk: Russian Urals crude is a competing grade; sanctions affect Brent differentials."
        ),
        rel        = paste(
          "Brent is the global benchmark \u2014 roughly 70% of world crude is priced as a differential to Brent.",
          "The WTI-Brent spread is one of the most actively traded inter-market spreads. A narrow or",
          "negative spread (Brent premium) reflects global tightness or US export constraints. A wide",
          "WTI premium (rare) implies strong US domestic demand relative to global supply."
        ),
        risk_angle = paste(
          "International producers, national oil companies, and refiners outside the US price physical",
          "crude against Brent. A trading desk with global exposure typically hedges using both WTI and",
          "Brent, with basis risk between the two. Understanding when and why the WTI-Brent spread",
          "moves is essential for managing that basis."
        )
      ),

      NG = list(
        name       = "Natural Gas",
        ticker     = "NG",
        unit       = "USD / MMBtu",
        exchange   = "NYMEX (CME Group)",
        contract   = "10,000 MMBtu of pipeline-grade natural gas, deliverable at Henry Hub, Louisiana.",
        drivers    = list(
          "Weather \u2014 the dominant driver: HDD (heating degree days, Nov\u2013Mar) and CDD (cooling, Jun\u2013Sep).",
          "Power generation: the largest demand sector at ~40 Bcf/d; gas competes with coal for dispatch.",
          "LNG exports: now the single largest incremental demand driver at ~12 Bcf/d (2024).",
          "Weekly EIA storage report: injections Apr\u2013Oct; withdrawals Nov\u2013Mar. Storage vs 5-yr avg is the key signal.",
          "Pipeline exports to Mexico (~6 Bcf/d) and imports from Canada (~8.8 Bcf/d).",
          "Associated gas from Permian oil drilling adds supply regardless of gas prices."
        ),
        rel        = paste(
          "Natural gas has the weakest correlation to crude oil of any market in this app.",
          "However, LNG exports are increasingly linking Henry Hub to European (TTF) and Asian (JKM) prices.",
          "The calendar spread (C1\u2013C2) is the clearest real-time signal of storage economics vs supply tightness."
        ),
        risk_angle = paste(
          "NG has the highest volatility of any major energy market \u2014 weather events can move prices",
          "20\u201330% in days. Key tail risks: 2005 Katrina, Feb 2021 Texas freeze, 2022 Russia-Ukraine war.",
          "A risk manager must track storage vs 5-year norms, LNG feed-gas demand, and seasonal roll cost."
        )
      ),

      HO = list(
        name       = "Heating Oil (No. 2 Distillate)",
        ticker     = "HO",
        unit       = "USD / gallon",
        exchange   = "NYMEX (CME Group)",
        contract   = "42,000 gallons (1,000 barrels) of No. 2 fuel oil, delivered in New York Harbor.",
        drivers    = list(
          "Distillate demand: heating oil for residential/commercial heating in the US Northeast.",
          "Diesel and jet fuel demand: HO is a global proxy for diesel \u2014 trucking, rail, shipping, aviation.",
          "Refinery yield: distillate yields are higher in winter runs; refiners shift yields between products.",
          "Crude oil cost: HO is a refined product, so crude prices are the dominant input cost.",
          "European diesel demand: ICE gasoil co-moves tightly with HO.",
          "IMO 2020 sulphur regulations increased demand for low-sulphur distillates globally."
        ),
        rel        = paste(
          "The HO crack spread \u2014 (HO price \u00d7 42) minus CL price \u2014 measures the refinery margin on distillate.",
          "HO and RB together form the '3-2-1 crack': 3 barrels of crude yield approximately 2 barrels of",
          "gasoline and 1 barrel of distillate. This is the most common refinery hedge structure."
        ),
        risk_angle = paste(
          "Airlines hedge jet fuel, trucking fleets hedge diesel, utilities hedge heating fuel \u2014 all against HO.",
          "Understanding HO seasonality (autumn build, winter draw) and its relationship to crude",
          "is foundational for pricing and risk-managing these client flows."
        )
      ),

      RB = list(
        name       = "RBOB Gasoline",
        ticker     = "RB",
        unit       = "USD / gallon",
        exchange   = "NYMEX (CME Group)",
        contract   = "42,000 gallons (1,000 barrels) of RBOB gasoline, delivered in New York Harbor.",
        drivers    = list(
          "US driving demand: vehicle miles travelled, consumer confidence, and retail prices.",
          "Refinery maintenance: spring turnarounds reduce supply just as summer driving begins.",
          "Ethanol blending mandates (RFS): RBOB is blended with 10% ethanol at the rack.",
          "Summer/winter grade transitions: more expensive summer-spec gasoline produced Mar\u2013Sep.",
          "Crude oil cost: the dominant input; RB and CL move together over long horizons.",
          "Strategic Petroleum Reserve (SPR) releases affect crude input costs and product prices."
        ),
        rel        = paste(
          "The RB crack spread \u2014 (RB price \u00d7 42) minus CL price \u2014 is the gasoline refinery margin.",
          "It spikes in spring as refiners switch to summer-spec production and driving demand recovers.",
          "RB cracks tend to be more seasonal and more volatile than HO cracks."
        ),
        risk_angle = paste(
          "Gasoline is the largest refined product market by volume in the US. Retail gasoline prices",
          "are politically sensitive. For a risk manager, RB exposure is typically managed alongside",
          "crude and heating oil as part of an integrated refinery or distribution book."
        )
      ),

      HTT = list(
        name       = "WTI Houston (MEH)",
        ticker     = "HTT",
        unit       = "USD / bbl",
        exchange   = "CME Group (Magellan East Houston delivery)",
        contract   = "1,000 barrels of WTI-quality crude, delivered at Magellan East Houston terminal.",
        drivers    = list(
          "Permian Basin pipeline takeaway capacity: bottlenecks widen the CL-HTT discount.",
          "US crude export demand: Houston is the primary export hub; strong demand supports HTT vs CL.",
          "Tanker availability and freight rates at the US Gulf Coast.",
          "Refinery demand along the Gulf Coast \u2014 the most complex refining region in the world.",
          "WTI Cushing dynamics: storage levels at Cushing affect the HTT-CL spread directly.",
          "Pipeline tariffs and capacity expansions (e.g. Permian-to-Gulf Coast lines)."
        ),
        rel        = paste(
          "WTI Houston (MEH) trades at a premium or discount to Cushing WTI (CL) depending on pipeline",
          "logistics and export demand. When Permian production surged in 2018\u20132019 and pipelines were",
          "full, HTT traded at a $10+ discount to CL. After new pipelines opened in 2019\u20132020, the spread",
          "collapsed. The HTT-CL spread is a real-time indicator of US midstream infrastructure stress."
        ),
        risk_angle = paste(
          "For a risk manager overseeing Gulf Coast or export-focused crude operations, HTT is the",
          "relevant benchmark \u2014 not Cushing-delivered WTI. Basis risk between CL and HTT can be",
          "significant during pipeline constraints."
        )
      )
    )

    all_markets <- c("CL", "BRN", "NG", "HO", "RB", "HTT")

    # ---------------------------------------------------------------------------
    # Morning briefing reactive
    # ---------------------------------------------------------------------------
    briefing_data <- shiny::reactive({
      shiny::req(!is.null(r$data))

      lapply(all_markets, function(mkt) {
        mkt_data <- r$data |>
          dplyr::filter(market == mkt, contract == 1L) |>
          dplyr::arrange(date)

        n <- nrow(mkt_data)
        if (n < 2L) return(NULL)

        latest    <- mkt_data$value[n]
        latest_dt <- mkt_data$date[n]

        price_at <- function(target_dt) {
          sub <- mkt_data[mkt_data$date <= target_dt, ]
          if (nrow(sub) == 0L) return(NA_real_)
          sub$value[nrow(sub)]
        }

        ret_1d <- (latest / price_at(latest_dt - 1L)  - 1) * 100
        ret_1w <- (latest / price_at(latest_dt - 7L)  - 1) * 100
        ret_1m <- (latest / price_at(latest_dt - 30L) - 1) * 100
        ret_1y <- (latest / price_at(latest_dt - 365L)- 1) * 100

        log_rets <- diff(log(mkt_data$value))
        vol_21d  <- if (length(log_rets) >= 21L) {
          stats::sd(utils::tail(log_rets, 21L)) * sqrt(252) * 100
        } else NA_real_

        if (length(log_rets) >= 252L + 21L) {
          all_vols   <- slider::slide_dbl(log_rets,
            ~ stats::sd(.x, na.rm = TRUE) * sqrt(252) * 100,
            .before = 20L, .complete = TRUE)
          med_vol    <- stats::median(utils::tail(all_vols[!is.na(all_vols)], 252L), na.rm = TRUE)
          vol_regime <- if (!is.na(vol_21d) && vol_21d > med_vol) "High" else "Normal"
        } else {
          vol_regime <- NA_character_
        }

        c2_last <- r$data |>
          dplyr::filter(market == mkt, contract == 2L, date <= latest_dt) |>
          dplyr::arrange(date)
        c2_val      <- if (nrow(c2_last) > 0L) c2_last$value[nrow(c2_last)] else NA_real_
        curve_shape <- if (!is.na(c2_val)) {
          if (latest > c2_val) "Backwardation" else "Contango"
        } else NA_character_

        roll_yield_pct <- if (!is.na(c2_val) && c2_val > 0) {
          round((latest / c2_val - 1) * 12 * 100, 2)
        } else NA_real_

        unit <- switch(mkt,
          CL = "$/bbl", BRN = "$/bbl", HTT = "$/bbl",
          HO = "$/gal", RB  = "$/gal", NG  = "$/MMBtu"
        )

        data.frame(
          Market           = paste0(
            "<a href='javascript:void(0)' ",
            "onclick='document.getElementById(\"mkt_profile_", mkt, "\").scrollIntoView({behavior:\"smooth\"})' ",
            "style='color:#2980b9;font-weight:600;text-decoration:none;'>", mkt, "</a>"
          ),
          Unit             = unit,
          Price            = round(latest, if (mkt == "NG") 3L else 2L),
          `1D %`           = round(ret_1d, 2),
          `1W %`           = round(ret_1w, 2),
          `1M %`           = round(ret_1m, 2),
          `1Y %`           = round(ret_1y, 2),
          `21d Vol %`      = round(vol_21d, 1),
          `Vol Regime`     = vol_regime,
          `Curve`          = curve_shape,
          `Roll Yield %/yr`= roll_yield_pct,
          check.names      = FALSE,
          stringsAsFactors = FALSE
        )
      }) |> dplyr::bind_rows()
    })

    output$briefing_table <- DT::renderDT({
      df <- briefing_data()
      shiny::req(nrow(df) > 0L)

      DT::datatable(
        df,
        rownames = FALSE,
        escape   = FALSE,
        options  = list(
          dom        = "t",
          pageLength = 10,
          ordering   = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        class = "compact stripe hover"
      ) |>
        DT::formatStyle("1D %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))) |>
        DT::formatStyle("1W %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))) |>
        DT::formatStyle("1Y %",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60"))) |>
        DT::formatStyle("Vol Regime",
          backgroundColor = DT::styleEqual(
            c("High", "Normal"),
            c("rgba(192,57,43,0.15)", "rgba(39,174,96,0.12)"))) |>
        DT::formatStyle("Curve",
          color      = DT::styleEqual(
            c("Backwardation", "Contango"),
            c("#2980b9", "#7f8c8d")),
          fontWeight = "bold") |>
        DT::formatStyle("Roll Yield %/yr",
          color = DT::styleInterval(c(-0.001, 0.001),
            c("#c0392b", "#555555", "#27ae60")))
    })

    # ---------------------------------------------------------------------------
    # Market profile cards — two-column grid, all 6 markets
    # ---------------------------------------------------------------------------
    output$profile_grid <- shiny::renderUI({
      cards <- lapply(all_markets, function(mkt) {
        info <- market_info[[mkt]]
        if (is.null(info)) return(NULL)

        driver_items <- lapply(info$drivers, function(d) {
          shiny::tags$li(d, style = "margin-bottom: 0.25rem; font-size: 0.88rem;")
        })

        shiny::div(
          id = paste0("mkt_profile_", mkt),
          bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$span(
              shiny::tags$code(
                mkt,
                style = "font-size: 0.95rem; background: #eaf2fb; color: #2980b9; padding: 0.1rem 0.4rem; border-radius: 3px; margin-right: 0.5rem;"
              ),
              shiny::tags$strong(info$name),
              shiny::tags$span(
                paste0(" \u2022 ", info$exchange, " \u2022 ", info$unit),
                style = "font-size: 0.8rem; color: #888; font-weight: normal;"
              )
            )
          ),
          shiny::tags$div(
            style = "padding: 0.75rem 1rem; overflow-y: auto;",
            shiny::tags$p(
              shiny::tags$strong("Contract: "), info$contract,
              style = "font-size: 0.88rem; color: #555; margin-bottom: 0.75rem;"
            ),
            shiny::tags$p(shiny::tags$strong("Key Price Drivers"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"),
            shiny::tags$ul(driver_items,
              style = "color: #444; padding-left: 1.1rem; margin-bottom: 0.85rem;"),
            shiny::tags$p(shiny::tags$strong("Market Relationships"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"),
            shiny::tags$p(info$rel,
              style = "font-size: 0.88rem; color: #444; margin-bottom: 0.85rem;"),
            shiny::tags$p(shiny::tags$strong("Risk Management Angle"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"),
            shiny::tags$p(info$risk_angle,
              style = "font-size: 0.88rem; color: #444; margin-bottom: 0;")
          )
        ) # card
        ) # anchor div
      })

      rows <- lapply(seq(1L, length(cards), by = 2L), function(i) {
        pair  <- cards[i:min(i + 1L, length(cards))]
        col_w <- if (length(pair) == 2L) c(6, 6) else c(12)
        do.call(bslib::layout_columns, c(pair, list(col_widths = col_w)))
      })

      shiny::tagList(rows)
    })

  })
}
