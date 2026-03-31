# data-raw/generate_data.R
#
# Run this script manually whenever you want to refresh the bundled data.
# It writes two feather files to inst/extdata/ which the app reads at startup.
#
# Usage (from project root in R):
#   source("data-raw/generate_data.R")
#
# Requirements: arrow, RTL (dev), tidyquant

library(RTL)
library(tidyquant)
library(dplyr)
library(arrow)

extdata_path <- file.path("inst", "extdata")
dir.create(extdata_path, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Futures data from RTL::dflong ----------------------------------------
message("Processing RTL::dflong ...")

futures <- RTL::dflong |>
  mutate(
    market   = sub("[0-9]+$", "", series),
    contract = as.integer(sub("^[A-Za-z]+", "", series)),
    date     = as.Date(date)
  ) |>
  filter(!is.na(value), !is.na(contract)) |>
  select(date, market, contract, value)

arrow::write_feather(futures, file.path(extdata_path, "futures_data.feather"))
message("  Wrote futures_data.feather  (", nrow(futures), " rows)")

# ---- 2. FRED Constant Maturity Treasury data ----------------------------------
message("Fetching CMT data from FRED (11 series) ...")

cmt_meta <- data.frame(
  ticker         = c("DGS1MO","DGS3MO","DGS6MO",
                     "DGS1","DGS2","DGS3","DGS5",
                     "DGS7","DGS10","DGS20","DGS30"),
  maturity_years = c(1/12, 3/12, 6/12, 1, 2, 3, 5, 7, 10, 20, 30),
  stringsAsFactors = FALSE
)

cmt <- lapply(seq_len(nrow(cmt_meta)), function(i) {
  message("  Fetching ", cmt_meta$ticker[i], " ...")
  tq_get(cmt_meta$ticker[i], get = "economic.data") |>
    rename(value = price) |>
    mutate(
      maturity_label = cmt_meta$ticker[i],
      maturity_years = cmt_meta$maturity_years[i],
      date           = as.Date(date)
    ) |>
    select(date, maturity_label, maturity_years, value)
}) |>
  bind_rows() |>
  filter(!is.na(value))

arrow::write_feather(cmt, file.path(extdata_path, "cmt_data.feather"))
message("  Wrote cmt_data.feather  (", nrow(cmt), " rows)")

message("\nDone. Commit inst/extdata/*.feather to the repository.")
