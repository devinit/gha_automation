required.packages <- c("data.table","jsonlite","httr","XML")
lapply(required.packages, require, character.only=T)

setwd(dirname(getActiveDocumentContext()$path))

source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_appeals_data.R")
source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_api_appeals.R")
source("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R")

years <- 2018:2021
update <- ""

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

fts_out <- fts

#Create sector
fts_out[, sector := destinationObjects_GlobalCluster.name]
fts_out[grepl(";", destinationObjects_GlobalCluster.name), sector := "Multiple sectors specified"]

#Create year
fts_out[, year := destinationObjects_UsageYear.name]
fts_out[grepl(";", destinationObjects_UsageYear.name), year := "Multi-year"]

fts_out[, sum(amountUSD), by = .(sector, year)]
