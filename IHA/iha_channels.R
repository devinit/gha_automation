suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("IHA/fts_curated_flows.R")

#Load in curated FTS (incoming and internal on year boundary, exclude non-destination allocable flows, exclude internal destination flows)
fts_curated <- fts_curated_flows(years = 2016:2022, update_years = NA)

#Set NA channels to 'Uncategorized'
fts_curated[, channel := ifelse(is.na(destinationObjects_Organization.organizationTypes) | destinationObjects_Organization.organizationTypes == "", "Uncategorized", destinationObjects_Organization.organizationTypes)]

#Set subchannels
fts_curated[, subchannel := ifelse(destinationObjects_Organization.organizationSubTypes == "", "Uncategorized", destinationObjects_Organization.organizationSubTypes)]

#Set multiple channels to 'Multi-channel'
fts_curated[grepl(";", channel), `:=` (channel = "Multi-channel", subchannel = "Multi-channel")]

#Tidy up channel names
fts_curated[, subchannel := gsub(" NGO", "", subchannel, ignore.case = T)]

#GHA channels
gha_channels <- setnames(
  data.table(t(data.table(
    c("NGOs and CSOs", "NGO"),
    c("UN Multi", "UN agency"),
    c("UN Multi", "Pooled fund"),
    c("Public Sector", "Government"),
    c("Public Sector", "Inter-governmental"),
    c("RCRC", "Red Cross/Red Crescent"),
    c("Other", "Private organization/foundation"),
    c("Other", "Other"),
    c("Other multi", "Financial institution"),
    c("Uncategorized", "Uncategorized"),
    c("Multi-channel", "Multi-channel")
  )
  )
  ), c("gha_channel", "channel"))
                           

fts_curated <- merge(fts_curated, gha_channels, by = "channel", all.x = T)

#Aggregate
agg_channels <- fts_curated[, .(total_2019USD = sum(amountUSD_defl, na.rm = T)), by = .(year, gha_channel, channel, subchannel, status)]

fwrite(agg_channels, "IHA/output/fts_aggregate_channels.csv")
