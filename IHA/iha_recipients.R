suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source)

years <- 2016:2022
update <- 2021:2022

fts_files <- list.files(pattern = "fts_")
fts_list <- list()
for(i in 1:length(years)){
  run <- T
  if(!(paste0("fts_", years[i], ".csv") %in% fts_files) | years[i] %in% update){
    message(paste0("Downloading ", years[i]))
    while(run){
      tryCatch({
        fts <- fts_get_flows(year = years[i])
        run <- F
        },
        error = function(e) e
      )
      break
    }
    fts[is.null(fts) | fts == "NULL"] <- NA
    fts[, `:=` (reportDetails = NULL, childFlowIds = NULL)]
    fwrite(fts, paste0("fts_", years[i], ".csv"))
  }
  fts_list[[i]] <- fread(paste0("fts_", years[i], ".csv"), encoding = "UTF-8")
}

fts <- rbindlist(fts_list, use.names = T, fill = T)
rm(fts_list)

#Split rows into individual years by destination usage year where multiple are recorded 
fts[, year := destinationObjects_UsageYear.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)

#Set multi-country flows to 'multi-country' in recipient column
setwd("..")
isos <- fread("reference_datasets/isos.csv", encoding = "UTF-8")
fts <- merge(fts, isos[, .(countryname_fts, iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T)
fts[, recipient := destinationObjects_Location.name]
fts[grepl(";", recipient), `:=` (recipient = "Multi-recipient", iso3 = "MULTI")]


#Deflate by source location and destination year
fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
fts_orgs[, `:=` (type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), location = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name)), by = id]

fts_orgs_gov <- fts_orgs[type == "Government", .(sourceObjects_Organization.id = id, donor_country = location)]

#Manual development agency locations
fts_orgs_gov <- rbind(fts_orgs_gov,
                      data.table(sourceObjects_Organization.id = c("9946", "10399", "4058", "2987", "30", "6547"),
                                 donor_country = c("France", "Qatar", "United States", "Germany", "United Arab Emirates", "Taiwan, Province of China"))
)

#Merge orgs types for deflators
fts <- merge(fts, fts_orgs_gov, by = "sourceObjects_Organization.id", all.x = T)
fts[is.na(donor_country), donor_country := "Total DAC"]

deflators <- fread("https://raw.githubusercontent.com/danjwalton/gha_report_2021/main/datasets/Deflators/usd_deflators_2021WEO.csv", header = T)
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]

fts <- merge(fts, deflators, by = c("donor_country", "year"), all.x = T)
fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year", all.x = T)$deflator
fts[, amountUSD_defl := amountUSD/deflator]

fwrite(fts[,-c("versionId", "onBoundary", "parentFlowId", "keywords", "sourceObjects_UsageYear.id", "destinationObjects_UsageYear.id")], "fts_flows_recipients.csv")

agg <- fts[, .(total_2019USD = sum(amountUSD_defl, na.rm = T)), by = .(year, recipient, status)]
agg <- dcast(agg, year + recipient ~ status, value.var = "total_2019USD")

fwrite(agg, "fts_aggregate_recipients.csv")
