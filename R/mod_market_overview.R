#' market_overview UI Function
#'
#' The landing page.  Gives a new user (e.g. a senior risk/trading leader)
#' immediate context: what are these markets, where are prices today, and
#' what does recent price action look like?
#'
#' Layout:
#'   Row 1 — one value box per market (latest price + 1-day change)
#'   Row 2 — 1-year price history (front month, all markets)
#'   Row 3 — rich market descriptions with key drivers, relationships, units
#'
#' @param id Shiny module id.
#' @noRd
#' @importFrom shiny NS tagList tags uiOutput tableOutput div
#' @importFrom bslib layout_columns card card_header
mod_market_overview_ui <- function(id) {
  ns <- NS(id)
  shiny::div(
    style = "padding: 1rem; display: flex; flex-direction: column; gap: 1rem;",
    # Row 1 — price snapshot table
    bslib::card(
      fill  = FALSE,   # do not stretch; take only as much height as needed
      bslib::card_header("Market Snapshot — latest front-month prices"),
      shiny::div(
        style = "overflow-x: auto;",
        shiny::tableOutput(ns("price_table"))
      )
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
#' @importFrom shiny moduleServer reactive req renderUI renderTable
#' @importFrom dplyr filter arrange group_by mutate ungroup slice_tail last first
#' @importFrom bslib layout_columns card card_header
mod_market_overview_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {

    # ---------------------------------------------------------------------------
    # Market metadata — name, unit, and structured narrative for each ticker.
    # Each entry has:
    #   name        display name
    #   unit        price unit shown in value box
    #   exchange    where it trades
    #   contract    what physical delivery looks like
    #   drivers     bullet list of key price drivers
    #   rel         how this market relates to others
    #   risk_angle  why a risk/trading manager should care
    # ---------------------------------------------------------------------------
    market_info <- list(

      CL = list(
        name       = "WTI Crude Oil",
        ticker     = "CL",
        unit       = "USD / bbl",
        exchange   = "NYMEX (CME Group)",
        contract   = "1,000 barrels of West Texas Intermediate crude, delivered at Cushing, Oklahoma.",
        drivers    = list(
          "OPEC+ production quotas and compliance — the single largest supply lever globally.",
          "US weekly inventory report (EIA): Cushing hub stocks move WTI more than any other data release.",
          "Permian Basin production growth — the US added ~6 mb/d of output from 2010 to 2023.",
          "Global demand signals: Chinese PMI, airline traffic, and freight activity.",
          "USD strength: crude is priced in dollars, so a stronger USD suppresses demand from non-US buyers.",
          "Geopolitical risk premium: Middle East tensions, Russian export flows, Strait of Hormuz."
        ),
        rel        = paste(
          "WTI is the North American benchmark. Its primary spread relationship is with Brent (BRN):",
          "WTI historically traded at a slight premium, but went to a $25 discount in 2011–2013 when",
          "Cushing storage filled and US crude could not be exported. After the US export ban was lifted",
          "in December 2015, the spread normalised. WTI is also the input to crack spreads — the margin",
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
          "North Sea field production (Brent, Forties, Oseberg, Ekofisk, Troll — the BFOET basket).",
          "European and Asian demand: Brent is the benchmark for Atlantic Basin and Asian crude trade.",
          "OPEC+ output cuts affect Brent more directly than WTI because most OPEC crude is priced off Brent.",
          "Tanker freight rates: Brent crude must be shipped; rising freight costs widen the Brent-WTI spread.",
          "Refinery margins in Europe and Asia drive physical demand for Brent-linked grades.",
          "Geopolitical risk: Russian Urals crude is a competing grade; sanctions affect Brent differentials."
        ),
        rel        = paste(
          "Brent is the global benchmark — roughly 70% of world crude is priced as a differential to Brent.",
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
        contract   = "10,000 MMBtu of pipeline-grade natural gas, physically deliverable at Henry Hub, Louisiana. Minimum tick: $0.001/MMBtu ($10/contract).",
        drivers    = list(
          "Weather — the dominant driver: HDD (heating degree days, Nov\u2013Mar) for winter heating demand; CDD (cooling degree days, Jun\u2013Sep) for summer power-burn from air conditioning.",
          "Power generation: the largest demand sector at ~40 Bcf/d (36% of US consumption); gas competes with coal for dispatch — low gas prices displace coal and vice versa.",
          "LNG exports: now the single largest incremental demand driver at ~12 Bcf/d (2024); most offtake contracts are indexed to Henry Hub, tying US balances to global gas markets.",
          "Weekly EIA storage report: injections Apr\u2013Oct build inventories; withdrawals Nov\u2013Mar draw them down. Storage vs the 5-year average is the most-watched supply/demand signal.",
          "Pipeline exports to Mexico (~6 Bcf/d) and imports from Canada (~8.8 Bcf/d); infrastructure constraints at key hubs cause regional basis blow-outs.",
          "Associated gas from Permian oil drilling adds supply regardless of gas prices, capping price rallies when oil activity is high."
        ),
        rel        = paste(
          "Natural gas has the weakest correlation to crude oil of any market in this app — its supply/demand",
          "is driven by weather and domestic infrastructure, not OPEC quotas or global refinery margins.",
          "However, LNG exports (12 Bcf/d in 2024) are increasingly linking Henry Hub to European (TTF)",
          "and Asian (JKM) benchmark prices. Historically, US NG prices bottomed in March after the",
          "winter draw and rose through late summer as the market priced in the next heating season,",
          "consistent with the STL decomposition of Henry Hub spot prices (2015\u20132025).",
          "The calendar spread (C1\u2013C2) is the clearest real-time signal of whether the market is",
          "in contango (storage economics dominate) or backwardation (supply is too tight to store)."
        ),
        risk_angle = paste(
          "NG has the highest volatility of any major energy market \u2014 weather events can move prices",
          "20\u201330% in days. Key tail risks: (1) 2005 Hurricane Katrina forced a physical shutdown of",
          "Henry Hub and NYMEX suspended NG futures trading for two weeks; (2) the Feb 2021 Texas",
          "freeze shut down production and caused a price spike to ~$23/MMBtu in spot markets;",
          "(3) the 2022 Russia\u2013Ukraine war combined with the Freeport LNG facility explosion drove",
          "Henry Hub above $9/MMBtu. A risk manager must track storage vs 5-year norms, LNG",
          "feed-gas demand, and the seasonal roll cost across the steep winter forward curve."
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
          "Diesel and jet fuel demand: HO is a global proxy for diesel — trucking, rail, shipping, and aviation.",
          "Refinery yield: distillate yields are higher in winter runs; refiners shift yields between products.",
          "Crude oil cost: HO is a refined product, so crude prices are the dominant input cost.",
          "European diesel demand: ICE gasoil (a European distillate contract) co-moves tightly with HO.",
          "IMO 2020 sulphur regulations increased demand for low-sulphur distillates globally."
        ),
        rel        = paste(
          "The HO crack spread — (HO price × 42) minus CL price — measures the refinery margin on",
          "distillate production. A widening crack spread means refiners earn more per barrel processed,",
          "incentivising higher run rates. HO and RB together form the '3-2-1 crack': 3 barrels of crude",
          "yield approximately 2 barrels of gasoline and 1 barrel of distillate. This is the most common",
          "refinery hedge structure."
        ),
        risk_angle = paste(
          "For a market maker in hedging services, distillate crack spreads are among the most actively",
          "hedged exposures — airlines hedge jet fuel (a close substitute for HO), trucking fleets hedge",
          "diesel, and utilities hedge heating fuel. Understanding HO seasonality (autumn build, winter draw)",
          "and its relationship to crude is foundational for pricing and risk-managing these client flows."
        )
      ),

      RB = list(
        name       = "RBOB Gasoline",
        ticker     = "RB",
        unit       = "USD / gallon",
        exchange   = "NYMEX (CME Group)",
        contract   = "42,000 gallons (1,000 barrels) of RBOB gasoline, delivered in New York Harbor.",
        drivers    = list(
          "US driving demand: vehicle miles travelled, consumer confidence, and gasoline retail prices.",
          "Refinery utilisation and maintenance: spring turnarounds reduce supply just as summer driving begins.",
          "Ethanol blending mandates (RFS): RBOB is blended with 10% ethanol at the rack before retail sale.",
          "Summer/winter grade transitions: more expensive summer-spec gasoline is produced Mar–Sep.",
          "Crude oil cost: the dominant input; RB and CL move together over long horizons.",
          "Strategic Petroleum Reserve (SPR) releases affect crude input costs and product prices."
        ),
        rel        = paste(
          "The RB crack spread — (RB price × 42) minus CL price — is the gasoline refinery margin.",
          "It spikes in spring as refiners switch to summer-spec production (higher cost, lower yield)",
          "and as driving demand recovers. RB cracks tend to be more seasonal and more volatile than",
          "HO cracks. Together they tell the story of overall refinery profitability and product market tightness."
        ),
        risk_angle = paste(
          "Gasoline is the largest refined product market by volume in the US. Retail gasoline prices",
          "are politically sensitive — rising pump prices affect consumer sentiment and are closely",
          "watched by policymakers. For a risk manager, RB exposure is typically managed alongside",
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
          "US crude export demand: Houston is the primary export hub; strong export demand supports HTT vs CL.",
          "Tanker availability and freight rates at the US Gulf Coast.",
          "Refinery demand along the Gulf Coast — the most complex refining region in the world.",
          "WTI Cushing dynamics: storage levels at Cushing affect the HTT-CL spread directly.",
          "Pipeline tariffs and capacity expansions (e.g. Permian-to-Gulf Coast lines)."
        ),
        rel        = paste(
          "WTI Houston (also called MEH — Magellan East Houston) trades at a premium or discount to",
          "Cushing WTI (CL) depending on pipeline logistics and export demand. When Permian production",
          "surged in 2018–2019 and pipelines were full, HTT traded at a $10+ discount to CL.",
          "After new pipelines opened in 2019–2020, the spread collapsed. The HTT-CL spread is",
          "therefore a real-time indicator of US midstream infrastructure stress."
        ),
        risk_angle = paste(
          "For a risk manager overseeing Gulf Coast or export-focused crude operations, HTT is the",
          "relevant benchmark — not Cushing-delivered WTI. Basis risk between CL and HTT can be",
          "significant during pipeline constraints. The HTT-Brent spread captures the economics of",
          "exporting US crude to international buyers and is watched closely by export-oriented traders."
        )
      )

    )

    # All 6 futures markets — always shown, independent of the selector
    all_markets <- c("CL", "BRN", "NG", "HO", "RB", "HTT")

    # ---------------------------------------------------------------------------
    # Price snapshot table: latest price + % changes over 1D, 1W, 1M, 1Y
    # ---------------------------------------------------------------------------
    output$price_table <- shiny::renderTable({
      req(!is.null(r$data))

      lapply(all_markets, function(mkt) {
        info <- market_info[[mkt]]
        mkt_data <- r$data |>
          dplyr::filter(market == mkt, contract == 1L) |>
          dplyr::arrange(date)

        n          <- nrow(mkt_data)
        latest     <- mkt_data$value[n]
        latest_dt  <- mkt_data$date[n]

        # Use calendar-date lookups so data gaps don't corrupt the % change.
        price_at <- function(target_date) {
          sub <- mkt_data[mkt_data$date <= target_date, ]
          if (nrow(sub) == 0L) return(NA_real_)
          sub$value[nrow(sub)]
        }

        data.frame(
          Market    = paste0(info$name, " (", mkt, ")"),
          Unit      = info$unit,
          Price     = round(latest, 2),
          `1D %`    = round((latest / price_at(latest_dt - 1L)  - 1) * 100, 2),
          `1W %`    = round((latest / price_at(latest_dt - 7L)  - 1) * 100, 2),
          `1M %`    = round((latest / price_at(latest_dt - 30L) - 1) * 100, 2),
          `1Y %`    = round((latest / price_at(latest_dt - 365L)- 1) * 100, 2),
          check.names = FALSE
        )
      }) |> dplyr::bind_rows()
    }, striped = TRUE, hover = TRUE, bordered = FALSE, spacing = "s", width = "100%")

    # ---------------------------------------------------------------------------
    # Market profile card grid — always shows all 6 markets.
    # Two-column layout; selector has no effect here.
    # ---------------------------------------------------------------------------
    output$profile_grid <- shiny::renderUI({
      cards <- lapply(all_markets, function(mkt) {
        info <- market_info[[mkt]]
        if (is.null(info)) return(NULL)

        driver_items <- lapply(info$drivers, function(d) {
          shiny::tags$li(d, style = "margin-bottom: 0.25rem; font-size: 0.88rem;")
        })

        bslib::card(
          style = "height: 100%;",
          bslib::card_header(
            shiny::tags$span(
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
              shiny::tags$strong("Contract: "),
              info$contract,
              style = "font-size: 0.88rem; color: #555; margin-bottom: 0.75rem;"
            ),

            shiny::tags$p(
              shiny::tags$strong("Key Price Drivers"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"
            ),
            shiny::tags$ul(
              driver_items,
              style = "color: #444; padding-left: 1.1rem; margin-bottom: 0.85rem;"
            ),

            shiny::tags$p(
              shiny::tags$strong("Market Relationships"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"
            ),
            shiny::tags$p(
              info$rel,
              style = "font-size: 0.88rem; color: #444; margin-bottom: 0.85rem;"
            ),

            shiny::tags$p(
              shiny::tags$strong("Risk Management Angle"),
              style = "margin-bottom: 0.25rem; font-size: 0.9rem;"
            ),
            shiny::tags$p(
              info$risk_angle,
              style = "font-size: 0.88rem; color: #444; margin-bottom: 0;"
            )
          )
        )
      })

      # Two-column grid: pairs of cards side by side
      rows <- lapply(seq(1L, length(cards), by = 2L), function(i) {
        pair <- cards[i:min(i + 1L, length(cards))]
        col_w <- if (length(pair) == 2L) c(6, 6) else c(12)
        do.call(bslib::layout_columns, c(pair, list(col_widths = col_w)))
      })

      shiny::tagList(rows)
    })

  })
}
