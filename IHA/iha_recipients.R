suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("IHA/fts_curated_flows.R")

#Load in curated FTS (incoming and internal on year boundary, exclude non-destination allocable flows, exclude internal destination flows)
fts_curated <- fts_curated_flows(years = 2016:2022, update_years = NA)

#Aggregate
agg_recipients <- fts_curated[, .(total_2019USD = sum(amountUSD_defl, na.rm = T)), by = .(year, recipient, status)]

fwrite(agg_recipients, "IHA/output/fts_aggregate_recipients.csv")
