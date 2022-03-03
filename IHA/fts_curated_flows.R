###Function to download and curate FTS flows from the FTS API
#Queries are done by year, meaning the boundary column reflects this. We prioritise flows with incoming boundary classification where a flow is duplicated.

##Data which is removed
#Outgoing flows are removed.
#Duplicate flows which occur in multiple years are removed - the first occurrence is retained and then split equally between destination usage years.
#Pledges are removed.

##Transformations
#Negative dummy flows are added to remove the effect of internal plan flows, ensuring the total inputs and outputs are correct when aggregated.
#Multi-year flows (destination) are split equally between destination years.
#Deflation is done according to source organisation and destination year. Non-government source organisations use the OECD DAC deflator.
#Flows with multiple destinations are rendered 'Multi-recipient'.
#Organisation types (channels) are as given by FTS's API, except in a few limited cases where government agencies are manually reclassified as such.
#European Commission Institutions are coded manually as donor country "European Commission" and use the OECD EU Institutions deflator.

fts_curated_flows <- function(years = 2016:2022, update_years = NA, dataset_path = "IHA/datasets", base_year = 2020, weo_ver = "Oct2021", dummy_intra_flows = F){
  suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))
  
  #Load FTS utility functions and deflators
  lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_get_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/general/deflators.R"), source)
  
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
      fts[, reportDetails := NULL]
      fts[is.null(fts) | fts == "NULL"] <- NA
      fwrite(fts, paste0(dataset_path, "/fts_", years[i], ".csv"))
    }
    message(paste0("Reading ", years[i]))
    fts_list[[i]] <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"), encoding = "UTF-8")
  }
  
  fts <- rbindlist(fts_list, use.names = T, fill = T)
  rm(fts_list)
  
  #Begin transformation
  message("Curating data...")
  
  #Retain column order
  col_order <- names(fts)
  
  #Remove flows which are outgoing on boundary
  fts <- fts[boundary != "outgoing"]
  
  #Remove duplicates which have a shared boundary, and preserve 'incoming' over 'internal' on boundary type
  shared <- rbind(fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id], fts[onBoundary == "shared" & boundary == "internal" & !(id %in% fts[onBoundary == "shared" & boundary == "incoming", .SD[1], by = id]$id), .SD[1], by = id])
  fts <- rbind(fts[onBoundary != "shared"], shared)
  
  #Split rows into individual years by destination usage year where multiple are recorded 
  fts[, year := destinationObjects_UsageYear.name]
  fts[, multiyear := grepl(";", destinationObjects_UsageYear.name)]
  fts <- fts_split_rows(fts, value.cols = "amountUSD", split.col = "year", split.pattern = "; ", remove.unsplit = T)
  
  #Set multi-country flows to 'multi-destination_country' in destination_country column
  isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", showProgress = F)
  fts <- merge(fts, isos[, .(countryname_fts, destination_iso3 = iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T, sort = F)
  fts[, destination_country := destinationObjects_Location.name]
  fts[grepl(";", destination_country), `:=` (destination_country = "Multi-destination_country", destination_iso3 = "MULTI")]
  
  #Add dummy reverse flows to cancel-out intra-plan flows
  fts[, dummy := F]
  if(dummy_intra_flows){
    fts_intraplan <- fts[sourceObjects_Plan.id == destinationObjects_Plan.id]
    source_cols <- grep("sourceObjects", names(fts_intraplan))
    destination_cols <- grep("destinationObjects", names(fts_intraplan))
    names(fts_intraplan)[source_cols] <- gsub("source", "destination", names(fts_intraplan)[source_cols])
    names(fts_intraplan)[destination_cols] <- gsub("destination", "source", names(fts_intraplan)[destination_cols])
    fts_intraplan[, `:=` (amountUSD = -amountUSD, dummy = T)]
    
    fts <- rbind(fts[sourceObjects_Plan.id != destinationObjects_Plan.id | is.na(sourceObjects_Plan.id)], fts_intraplan)
  }
  
  #Deflate by source location and destination year
  fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
  fts_locs <- data.table(fromJSON("https://api.hpc.tools/v1/public/location")$data)
  fts_orgs[, `:=` (source_org_type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), source_country = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name), source_country_id = ifelse(is.null(locations[[1]]$id), NA, locations[[1]]$id)), by = id]
  fts_orgs <- merge(fts_orgs, fts_locs[, .(id, iso3)], by.x = "source_country_id", by.y = "id", all.x = T, sort = F)
  fts_orgs <- fts_orgs[, .(sourceObjects_Organization.id = as.character(id), source_country, source_iso3 = iso3, FTS_source_orgtype = source_org_type)]
  
  #Merge DI coded org types
  source_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/source_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  source_org_dicode <- merge(fts_orgs, source_org_dicode[, .(sourceObjects_Organization.id = as.character(sourceObjects_Organization.id), source_orgtype, source_privatemoney)], by = "sourceObjects_Organization.id", all.x = T)
  
  destination_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/destination_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  
  #Merge source orgs
  fts[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  fts <- merge(fts, source_org_dicode, by = "sourceObjects_Organization.id", all.x = T, sort = F)
  fts[!(FTS_source_orgtype == "Government" | (source_orgtype %in% c("DAC governments", "NDD"))) | is.na(FTS_source_orgtype) | is.na(source_country), `:=` (source_country = "Total DAC", source_iso3 = "DAC")]
  fts[, FTS_source_orgtype := NULL]
  
  #Manual EU institution classifications
  euc_id <- c("8523","2966","8524","6789","2176","8525","8556","8650","8541","8421")
  fts[sourceObjects_Organization.id %in% euc_id, `:=` (source_country = "European Commission", source_iso3 = "EUI")]
  
  #Merge dest orgs
  fts <- merge(fts, destination_org_dicode[, .(destinationObjects_Organization.id = as.character(destinationObjects_Organization.id), destination_orgtype, destination_ngotype, destination_deliverychannel)], by = "destinationObjects_Organization.id", all.x = T, sort = F)
  
  #Fill gaps in DI org coding with FTS
  fts[is.na(source_orgtype) | source_orgtype == "", source_orgtype := gsub("NGO", "NGOs", sourceObjects_Organization.organizationTypes)]
  fts[is.na(source_privatemoney) | source_privatemoney == "", source_privatemoney := ifelse(sourceObjects_Organization.organizationTypes == "Private organization/foundation", "private", "no")]
  fts[is.na(destination_orgtype) | destination_orgtype == "", destination_orgtype := gsub("NGO", "NGOs", destinationObjects_Organization.organizationTypes)]
  
  fts[(is.na(destination_ngotype) | destination_ngotype == "") & destinationObjects_Organization.organizationTypes == "NGO", destination_ngotype := paste0(gsub(" NGO| organization/foundation/individual", "", destinationObjects_Organization.organizationSubTypes), " NGO")]
  fts[, destination_ngotype := gsub("^Affiliated", "Internationally Affiliated", destination_ngotype)]
  fts[is.na(destination_ngotype) & destinationObjects_Organization.organizationTypes == "NGO", destination_ngotype := "Undefined NGO"]
  
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
    ), c("FTS_matched_gha_channel", "destinationObjects_Organization.organizationTypes"))
  fts <- merge(fts, gha_channels, by = "destinationObjects_Organization.organizationTypes", all.x = T, sort = F)
  
  fts[is.na(destination_deliverychannel) | destination_deliverychannel == "", destination_deliverychannel := FTS_matched_gha_channel]
  fts[, FTS_matched_gha_channel := NULL]
  
  #Domestic response
  fts[, domestic_response := F]
  fts[sourceObjects_Organization.organizationTypes == "Government" & source_iso3 == destination_iso3, domestic_response := T]
  
  #New to country
  fts[, new_to_country := T]
  fts[sourceObjects_Location.id == destinationObjects_Location.id & sourceObjects_Location.id != "", new_to_country := F]

  #New to plan
  fts[, new_to_plan := T]
  fts[sourceObjects_Plan.id == destinationObjects_Plan.id & sourceObjects_Plan.id != "", new_to_plan := F]
  
  #New to sector
  fts[, new_to_sector := T]
  fts[sourceObjects_GlobalCluster.id == destinationObjects_GlobalCluster.id & sourceObjects_GlobalCluster.id != "", new_to_plan := F]
  
  #Deflate
  deflators <- get_deflators(base_year = base_year, currency = "USD", weo_ver = weo_ver, approximate_missing = T)
  deflators <- deflators[, .(source_iso3 = ISO, year = as.character(year), deflator = gdp_defl)]
  
  fts <- merge(fts, deflators, by = c("source_iso3", "year"), all.x = T, sort = F)
  fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[source_iso3 == "DAC"], by = "year", all.x = T, sort = F)$deflator
  fts[, `:=` (amountUSD_defl = amountUSD/deflator, amountUSD_defl_millions = (amountUSD/deflator)/1000000)]
  
  #Remove partial multi-year flows outside of requested range and pledges 
  fts <- fts[
    year %in% years 
    & status %in% c("paid", "commitment")
    ]
  
  #Reorder columns nicely
  col_order <- union(col_order, names(fts)[order(names(fts))])
  fts <- fts[, col_order, with = F]

  return(fts)
}
