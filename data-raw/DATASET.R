# data-raw/DATASET.R
#
# Creates the `eia_storage` package dataset bundled with GolemAppProject.
#
# Run this script once locally whenever you want to refresh the storage data:
#   source("data-raw/DATASET.R")
#
# Requires your EIA API key stored in .Renviron as EIA_KEY:
#   usethis::edit_r_environ()   # add line: EIA_KEY=yourkeyhere
#
# The resulting data/eia_storage.rda is committed to the repo and shipped with
# the package — no API key is required at runtime.
#
# Series:
#   CrudeCushing → CL (WTI Crude), HTT (WTI Houston)  [Thousand barrels]
#   NGLower48    → NG (Natural Gas)                    [Bcf]
#   Distillate   → HO (Heating Oil)  — total distillate, all sulfur grades [Thousand barrels]
#   Gasoline     → RB (RBOB Gasoline) — total gasoline incl. blendstock    [Thousand barrels]
#   BRN (Brent)  → no EIA storage series (North Sea crude, outside EIA scope)

library(dplyr)
library(lubridate)
library(slider)
library(tibble)

EIA_KEY <- Sys.getenv("EIA_KEY")
if (!nzchar(EIA_KEY)) stop("EIA_KEY not set. Add it to .Renviron via usethis::edit_r_environ()")

# ── 1. Fetch from EIA API ─────────────────────────────────────────────────────
message("Fetching EIA storage series via API...")
raw <- RTL::eia2tidy_all(
  tickers = tibble::tribble(
    ~ticker,                         ~name,
    "PET.W_EPC0_SAX_YCUOK_MBBL.W", "CrudeCushing",
    "NG.NW2_EPG0_SWO_R48_BCF.W",   "NGLower48",
    "PET.WDISTUS1.W",               "Distillate",
    "PET.WGTSTUS1.W",               "Gasoline"
  ),
  key  = EIA_KEY,
  long = TRUE
) |>
  dplyr::arrange(series, date)

message("Fetched ", nrow(raw), " rows across ", length(unique(raw$series)), " series.")

# ── 2. Compute 5-year seasonal average (EIA methodology) ─────────────────────
# For each (series, calendar week): average the prior 5 years only (no
# look-ahead), matching the 5-year average published in EIA weekly reports.
eia_storage <- raw |>
  dplyr::mutate(
    week = lubridate::week(date),
    year = lubridate::year(date)
  ) |>
  dplyr::group_by(series, week) |>
  dplyr::mutate(
    five_yr_avg     = slider::slide_index_dbl(
      value, year,
      ~ mean(.x, na.rm = TRUE),
      .before = 5L,
      .after  = -1L
    ),
    surplus_deficit = value - five_yr_avg
  ) |>
  dplyr::ungroup() |>
  dplyr::select(date, series, value, week, year, five_yr_avg, surplus_deficit)

# ── 3. Attach market mapping ──────────────────────────────────────────────────
series_to_markets <- c(
  CrudeCushing = "CL / HTT",
  NGLower48    = "NG",
  Distillate   = "HO",
  Gasoline     = "RB"
)

eia_storage <- eia_storage |>
  dplyr::mutate(market_label = series_to_markets[series])

# ── 4. Save as package dataset ────────────────────────────────────────────────
usethis::use_data(eia_storage, overwrite = TRUE)

message("eia_storage saved: ", nrow(eia_storage), " rows, ",
        length(unique(eia_storage$series)), " series, ",
        format(min(eia_storage$date)), " \u2192 ", format(max(eia_storage$date)))
