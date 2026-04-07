# Market Dynamics Explorer

An interactive R Shiny application for exploring energy futures market dynamics, built as a Golem R package for FIN 451 (Risk and Trading Management).

## What It Does

The app pulls live data from two sources and lets you explore six energy futures markets — WTI Crude (CL), Brent Crude (BRN), Natural Gas (NG), Heating Oil (HO), RBOB Gasoline (RB), and WTI Houston (HTT) — across eight analytical modules.

## Tabs

| Tab | What it shows |
|---|---|
| **Overview** | KPI cards (top gainer/loser, avg return, curve structure, yield curve, most volatile), morning briefing table with returns and regime signals, and detailed market profile cards for each commodity |
| **Fundamentals** | Storage and fundamental data |
| **Curves** | Forward curve term structure, US Treasury yield curve (1M–30Y), roll yield over time, and a macro context overlay comparing the 2Y–10Y spread against commodity prices |
| **Volatility** | Rolling annualised volatility, volatility surface heatmap across contract months, volatility cone (historical percentiles), and 1-day historical VaR at 95% and 99% confidence |
| **Correlations** | Rolling pairwise correlation, correlation matrix heatmap, and PCA factor analysis showing the systemic risk drivers across markets |
| **Seasonality** | Average monthly log returns, monthly return distribution (box plots), and year-over-year cumulative return vs historical IQR band |
| **Spreads** | Calendar spreads and inter-market spread dynamics |
| **Hedging** | Rolling minimum-variance hedge ratios: cross-market beta (Market A vs B) and term-structure beta (C1 vs Cn) |

## Data Sources

- **Futures prices**: `RTL::dflong` (R package, updated daily via GitHub Actions)
- **US Treasury CMT rates**: FRED series DGS1MO through DGS30 via `tidyquant::tq_get()`

Data is cached as Apache Arrow feather files (`inst/extdata/`) and refreshed automatically on weekdays at 18:00 UTC by a GitHub Actions workflow.

## Running the App

### Option 1 — Docker (no R installation required)

**Intel/AMD (linux/amd64):**
```bash
docker pull ghcr.io/nurs3003/golem_app:latest
docker run --rm -p 3838:3838 ghcr.io/nurs3003/golem_app:latest
```

**Apple Silicon (M1/M2/M3 — arm64):**
```bash
docker pull --platform linux/amd64 ghcr.io/nurs3003/golem_app:latest
docker run --rm --platform linux/amd64 -p 3838:3838 ghcr.io/nurs3003/golem_app:latest
```

Open `http://localhost:3838` in your browser.

> The image is `linux/amd64` only. On Apple Silicon it runs via Rosetta — startup is slightly slower but fully functional. `--rm` removes the container when you stop it so no cleanup is needed.

### Option 2 — R (development)

```r
remotes::install_deps(dependencies = TRUE)
remotes::install_github("risktoollib/RTL")

devtools::load_all()
GolemAppProject::run_app()
```

## Architecture

Built with the [Golem](https://github.com/ThinkR-open/golem) framework. Each analytical tab is a self-contained Shiny module. All modules communicate through a shared `reactiveValues` object (`r`) passed by reference from `app_server.R` — modules that own inputs write to `r`, modules that display results only read from it.

```
app_server.R
  r <- reactiveValues(data, cmt_data, selected_markets, date_range)
  ├── mod_market_selector_server   writes r$selected_markets, r$date_range
  ├── mod_market_overview_server   reads r
  ├── mod_forward_curve_server     reads r
  ├── mod_volatility_server        reads r
  ├── mod_codynamics_server        reads r
  ├── mod_seasonality_server       reads r
  ├── mod_spreads_server           reads r
  └── mod_hedge_ratios_server      reads r
```

## CI/CD

Two GitHub Actions workflows:

- **`data-refresh.yml`** — runs weekdays at 18:00 UTC, installs the latest RTL package, regenerates the feather data files, and commits them back to the repo.
- **`docker-publish.yml`** — triggers on every push to `main`, builds a `linux/amd64` Docker image and pushes it to `ghcr.io`.
