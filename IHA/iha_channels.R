suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("IHA/fts_curated_flows.R")

#Load in curated FTS (incoming and internal on year boundary, exclude non-destination allocable flows, exclude internal destination flows)
fts_curated <- fts_curated_flows(years = 2016:2022, update_years = NA)

#Aggregate
agg_channels <- fts_curated[, .(total_2019USD = sum(amountUSD_defl, na.rm = T)), by = .(year, org_type, status)]

fwrite(agg_channels, "IHA/output/fts_aggregate_channels.csv")
