###Function to download and curate FTS flows from the FTS API
#Queries are done by year, meaning the boundary column reflects this. We prioritise flows with incoming boundary classification where a flow is duplicated

##Data which is removed
#Outgoing flows are removed
#Duplicate flows which occur in multiple years are removed - the first occurrence is retained and then split equally between destination usage years
#Pledges are removed
#Flows with no specified destination location are removed (non-country allocable)
#Flows which have the same source and destination location are removed (non-new to country flows)

##Transformations
#Multi-year flows (destination) are split equally between destination years
#Deflation is done according to source organisation and destination year. Non-government source organisations use the OECD DAC deflator.
#Flows with multiple destinations are rendered 'Multi-recipient'
#Organisation types (channels) are as given by FTS's API, except in a few limited cases where government agencies are manually reclassified as such.

fts_curated_flows <- function(years = 2016:2022, update_years = 2022:2022, dataset_path = "IHA/datasets"){
  suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
  
  #Load FTS utility functions
  lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source)
  
  if(!dir.exists(dataset_path)){
    dir.create(dataset_path)
  }
  fts_files <- list.files(path = dataset_path, pattern = "fts_")
  fts_list <- list()
  for(i in 1:length(years)){
    run <- T
    if(!(paste0("fts_", years[i], ".csv") %in% fts_files) | years[i] %in% update_years){
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
      fwrite(fts, paste0(dataset_path, "/fts_", years[i], ".csv"))
    }
    message(paste0("Reading ", years[i]))
    fts_list[[i]] <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"), encoding = "UTF-8")
  }
  
  fts <- rbindlist(fts_list, use.names = T, fill = T)
  rm(fts_list)
  
  #Begin transformation
  message("Curating data...")
  
  #Remove flows which are outgoing on boundary
  fts <- fts[boundary != "outgoing"]
  
  #Remove duplicates which have a shared boundary, and preserve 'incoming' over 'internal' on boundary type
  shared <- rbind(fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id], fts[onBoundary == "shared" & boundary == "internal" & !(id %in% fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id]$id), .SD[1], by = id])
  fts <- rbind(fts[onBoundary != "shared"], shared)
  
  #Split rows into individual years by destination usage year where multiple are recorded 
  fts[, year := destinationObjects_UsageYear.name]
  fts[, multiyear := grepl(";", destinationObjects_UsageYear.name)]
  fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)
  
  #Set multi-country flows to 'multi-country' in recipient column
  isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", showProgress = F)
  fts <- merge(fts, isos[, .(countryname_fts, iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T)
  fts[, recipient := destinationObjects_Location.name]
  fts[grepl(";", recipient), `:=` (recipient = "Multi-recipient", iso3 = "MULTI")]
  
  #Deflate by source location and destination year
  fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
  fts_orgs[, `:=` (source_org_type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), donor_country = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name)), by = id]
  fts_orgs <- fts_orgs[, .(sourceObjects_Organization.id = as.character(id), donor_country, source_org_type)]
  
  #Manual government development agencies
  fts_orgs[sourceObjects_Organization.id %in% c("9946", "10399", "4058", "2987", "30", "6547"), org_type := "Government"]
  
  #Merge orgs types
  fts[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  fts <- merge(fts, fts_orgs, by = "sourceObjects_Organization.id", all.x = T, sort = F)
  fts[org_type != "Government" | is.na(donor_country), donor_country := "Total DAC"]
  
  #Set NA channels to 'Uncategorized'
  fts[, channel := ifelse(is.na(destinationObjects_Organization.organizationTypes) | destinationObjects_Organization.organizationTypes == "", "Uncategorized", destinationObjects_Organization.organizationTypes)]
  
  #Set subchannels
  fts[, subchannel := ifelse(destinationObjects_Organization.organizationSubTypes == "", "Uncategorized", destinationObjects_Organization.organizationSubTypes)]
  
  #Set multiple channels to 'Multi-channel'
  fts[grepl(";", channel), `:=` (channel = "Multi-channel", subchannel = "Multi-channel")]
  
  #Tidy up channel names
  fts[, subchannel := gsub(" NGO", "", subchannel, ignore.case = T)]
  
  #Merge GHA channels
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
  fts <- merge(fts, gha_channels, by = "channel", all.x = T, sort = F)
  
  #Deflate
  deflators <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/usd_deflators_2021WEO.csv", header = T, encoding = "UTF-8", showProgress = F)
  deflators <- suppressWarnings(melt.data.table(deflators, id.vars = c("name", "ISO3")))
  deflators <- deflators[, .(donor_country = name, year = as.character(variable), deflator = value)]
  
  fts <- merge(fts, deflators, by = c("donor_country", "year"), all.x = T)
  fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[donor_country == "Total DAC", -"donor_country"], by = "year", all.x = T)$deflator
  fts[, amountUSD_defl := amountUSD/deflator]
  
  #Remove pledges, remove non-country allocable, remove internal country flows
  fts_out <- fts[year %in% years & status %in% c("paid", "commitment") & (destinationObjects_Location.name != sourceObjects_Location.name | destinationObjects_Location.name == ""), -c("versionId", "onBoundary", "parentFlowId", "keywords", "sourceObjects_UsageYear.id", "destinationObjects_UsageYear.id")]
  
  return(fts_out)
}
