suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source)

years <- 2016:2022
update_all <- T

fts_files <- list.files(pattern = "fts_")
fts_list <- list()
for(i in 1:length(years)){
  run <- T
  if(!(paste0("fts_", years[i], ".csv") %in% fts_files) & update_all){
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
  fts_list[[i]] <- fread(paste0("fts_", years[i], ".csv"))
}

fts <- rbindlist(fts_list)
rm(fts_list)

#Split rows into individual years by destination usage year where multiple are recorded 
fts[, year := destinationObjects_UsageYear.name]
fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)

#Set multi-country flows to 'multi-country' in recipient column
isos <- fread("https://raw.githubusercontent.com/danjwalton/gha_report_2021/main/datasets/Countrynames/isos.csv")
fts <- merge(fts, isos[, .(countryname_fts, iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T)
fts[, recipient := destinationObjects_Location.name]
fts[grepl(";", recipient), `:=` (recipient = "Multi-recipient", iso3 = "MULTI")]

#Deflate by source location and destination year
isos <- fread("https://raw.githubusercontent.com/danjwalton/gha_report_2021/main/datasets/Deflators/usd_deflators_2021WEO.csv")
deflators <- melt.data.table(deflators, id.vars = c("name", "ISO3"))
deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]

fts <- merge(fts, deflators, by = c("donor_country", "year"), all.x = T)
fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year")$deflator
fts[, amountUSD_defl := amountUSD/deflator]

