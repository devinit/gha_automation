suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("IHA/fts_curated_flows.R")

#Load in curated FTS (incoming and internal on year boundary, exclude non-destination allocable flows, exclude internal destination flows)
fts_curated <- fts_curated_flows(years = 2016:2022, update_years = NA, dataset_path = "IHA/datasets", base_year = 2020, weo_ver = "Oct2021")

#Aggregate
agg_channels <- fts_curated[, .(total_2020USD = sum(amountUSD_defl, na.rm = T)), by = .(year, destination_orgtype, destination_ngotype, status)][order(year, destination_orgtype, destination_ngotype, status)]

fwrite(agg_channels, "IHA/output/fts_aggregate_channels.csv")
