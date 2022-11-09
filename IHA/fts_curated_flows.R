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

fts_curated_flows <- function(years = 2016:2022, update_years = NA, dataset_path = "IHA/datasets", base_year = 2020, weo_ver = NULL, dummy_intra_country_flows = T){
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
    fts_list[[i]] <- fread(paste0(dataset_path, "/fts_", years[i], ".csv"))
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
  
  #Set multi-country flows to 'multi-destination_org_country' in destination_org_country column
  isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", showProgress = F)
  fts <- merge(fts, isos[, .(countryname_fts, destination_org_iso3 = iso3)], by.x = "destinationObjects_Location.name", by.y = "countryname_fts", all.x = T, sort = F)
  fts[, destination_org_country := destinationObjects_Location.name]
  fts[grepl(";", destination_org_country), `:=` (destination_org_country = "Multi-destination_org_country", destination_org_iso3 = "MULTI")]
  
  #Deflate by source location and destination year
  fts_orgs <- data.table(fromJSON("https://api.hpc.tools/v1/public/organization")$data)
  fts_locs <- data.table(fromJSON("https://api.hpc.tools/v1/public/location")$data)
  fts_orgs[, `:=` (source_org_type = ifelse(is.null(categories[[1]]$name), NA, categories[[1]]$name), source_org_country = ifelse(is.null(locations[[1]]$name), NA, locations[[1]]$name), source_org_country_id = ifelse(is.null(locations[[1]]$id), NA, locations[[1]]$id)), by = id]
  fts_orgs <- merge(fts_orgs, fts_locs[, .(id, iso3)], by.x = "source_org_country_id", by.y = "id", all.x = T, sort = F)
  fts_orgs <- fts_orgs[, .(sourceObjects_Organization.id = as.character(id), source_org_country, source_org_iso3 = iso3, FTS_source_orgtype = source_org_type)]
  
  #Merge DI coded org types
  source_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/source_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  source_org_dicode <- merge(fts_orgs, source_org_dicode[, .(sourceObjects_Organization.id = as.character(sourceObjects_Organization.id), source_orgtype, source_privatemoney)], by = "sourceObjects_Organization.id", all.x = T)
  
  destination_org_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/destination_orgs_DIcode.csv", encoding = "UTF-8", showProgress = F)
  
  #Merge source orgs
  fts[, sourceObjects_Organization.id := as.character(sourceObjects_Organization.id)]
  fts <- merge(fts, source_org_dicode, by = "sourceObjects_Organization.id", all.x = T, sort = F)
  fts[!(FTS_source_orgtype == "Government" | (source_orgtype %in% c("DAC governments", "NDD"))) | is.na(FTS_source_orgtype) | is.na(source_org_country), `:=` (source_org_country = "Total DAC", source_org_iso3 = "DAC")]
  fts[, FTS_source_orgtype := NULL]
  
  #Manual EU institution classifications
  euc_id <- c("8523","2966","8524","6789","2176","8525","8556","8650","8541","8421")
  fts[sourceObjects_Organization.id %in% euc_id, `:=` (source_org_country = "European Commission", source_org_iso3 = "EUI")]
  
  #Merge dest orgs
  fts <- merge(fts, destination_org_dicode[!is.na(destinationObjects_Organization.id), .(destinationObjects_Organization.id = as.character(destinationObjects_Organization.id), destination_orgtype, destination_ngotype, destination_deliverychannel)], by = "destinationObjects_Organization.id", all.x = T, sort = F)
  
  #Fill gaps in DI org coding with FTS
  fts[is.na(source_orgtype) | source_orgtype == "", source_orgtype := gsub("NGO", "NGOs", sourceObjects_Organization.organizationTypes)]
  fts[, source_orgtype := gsub("UN agency", "UN Multi", source_orgtype)]
  
  fts[is.na(source_privatemoney) | source_privatemoney == "", source_privatemoney := ifelse(sourceObjects_Organization.organizationTypes == "Private organization/foundation", "private", "no")]
  fts[is.na(destination_orgtype) | destination_orgtype == "", destination_orgtype := gsub("NGO", "NGOs", destinationObjects_Organization.organizationTypes)]
  fts[, destination_orgtype := gsub("UN agency", "UN Multi", destination_orgtype)]
  
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
  
  #Aggregate multiple matched columns
  fts[grepl(";", source_orgtype), source_orgtype := ifelse(length(unique(strsplit(source_orgtype, "; ")[[1]])) == 1, unique(strsplit(source_orgtype, "; ")[[1]]), "Other")]
  fts[grepl(";", destination_orgtype), destination_orgtype := ifelse(length(unique(strsplit(destination_orgtype, "; ")[[1]])) == 1, unique(strsplit(destination_orgtype, "; ")[[1]]), "Other") ]
  fts[grepl(";", destination_ngotype), destination_ngotype := ifelse(length(unique(strsplit(destination_ngotype, "; ")[[1]])) == 1, unique(strsplit(destination_ngotype, "; ")[[1]]), "Other") ]
  
  #Merge DI coded clusters
  cluster_dicode <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/cluster_mapping_DIcode.csv", showProgress = F)
  
  fts[, upper_cluster := toupper((sourceObjects_Cluster.name))]
  fts <- merge(fts, unique(cluster_dicode[, .(upper_cluster, source_globalcluster = destination_globalcluster)]), by = "upper_cluster", all.x = T, sort = F)
  fts[, upper_cluster := NULL]
  
  fts[, upper_cluster := toupper((destinationObjects_Cluster.name))]
  fts <- merge(fts, unique(cluster_dicode[, .(upper_cluster, destination_globalcluster)]), by = "upper_cluster", all.x = T, sort = F)
  fts[, upper_cluster := NULL]
  
  #Overwrite DI coding with FTS global cluster where exists
  fts[!is.na(destinationObjects_GlobalCluster.name) & destinationObjects_GlobalCluster.name != "" & is.na(destination_globalcluster), destination_globalcluster := destinationObjects_GlobalCluster.name]
  fts[!is.na(sourceObjects_GlobalCluster.name) & sourceObjects_GlobalCluster.name != "" & is.na(source_globalcluster), source_globalcluster := sourceObjects_GlobalCluster.name]
  
  #Identify multi-cluster flows
  fts[grepl(";", destination_globalcluster) | (is.na(destination_globalcluster) & grepl(";", destinationObjects_Cluster.name)), destination_globalcluster := "Multiple clusters specified"]
  fts[grepl(";", source_globalcluster) | (is.na(source_globalcluster) & grepl(";", sourceObjects_Cluster.name)), source_globalcluster := "Multiple clusters specified"]
  
  #Identify unspecified cluster flows
  fts[is.na(destination_globalcluster) | destination_globalcluster == "", destination_globalcluster := "Unspecified"]
  fts[is.na(source_globalcluster) | source_globalcluster == "", source_globalcluster := "Unspecified"]
  
  #Domestic response
  fts[, domestic_response := F]
  fts[sourceObjects_Organization.organizationTypes == "Government" & source_org_iso3 == destination_org_iso3, domestic_response := T]
  
  #New to country
  fts[, new_to_country := T]
  fts[sourceObjects_Location.id == destinationObjects_Location.id | destinationObjects_Location.id == "", new_to_country := F]

  #New to plan
  fts[, new_to_plan := T]
  if("sourceObjects_Plan.id" %in% names(fts)){
    fts[sourceObjects_Plan.id == destinationObjects_Plan.id | destinationObjects_Plan.id == "", new_to_plan := F]
  }
  
  #New to sector
  fts[, new_to_sector := T]
  fts[sourceObjects_GlobalCluster.id == destinationObjects_GlobalCluster.id | destinationObjects_GlobalCluster.id == "", new_to_sector := F]
  
  #COVID
  fts[, COVID := F]
  fts[grepl("COVID", paste0(destinationObjects_Cluster.name, destinationObjects_Cluster.name, destinationObjects_Plan.name, destinationObjects_Emergency.name), ignore.case = T), COVID := T]
  
  #Deflate
  deflators <- get_deflators(base_year = base_year, currency = "USD", weo_ver = weo_ver, approximate_missing = T)
  deflators <- deflators[, .(source_org_iso3 = ISO, year = as.character(year), deflator = gdp_defl)]
  
  fts <- merge(fts, deflators, by = c("source_org_iso3", "year"), all.x = T, sort = F)
  fts[is.na(deflator)]$deflator <- merge(fts[is.na(deflator)][, -"deflator"], deflators[source_org_iso3 == "DAC"], by = "year", all.x = T, sort = F)$deflator
  fts[, `:=` (amountUSD_defl = amountUSD/deflator, amountUSD_defl_millions = (amountUSD/deflator)/1000000)]
  
  #Add dummy reverse flows to cancel-out intra-country flows
  fts[, newMoney_dest := newMoney]
  fts[, dummy := F]
  if(dummy_intra_country_flows){
    fts[sourceObjects_Location.id == destinationObjects_Location.id, newMoney_dest := TRUE]
    fts_intracountry <- fts[sourceObjects_Location.id == destinationObjects_Location.id]
    source_cols <- grep("source", names(fts_intracountry))
    destination_cols <- grep("destination", names(fts_intracountry))
    names(fts_intracountry)[source_cols] <- gsub("source", "destination", names(fts_intracountry)[source_cols])
    names(fts_intracountry)[destination_cols] <- gsub("destination", "source", names(fts_intracountry)[destination_cols])
    fts_intracountry[, `:=` (amountUSD = -amountUSD, amountUSD_defl = -amountUSD_defl, amountUSD_defl_millions = -amountUSD_defl_millions, dummy = T, destination_privatemoney = NULL, source_deliverychannel = NULL, source_ngotype = NULL)]
    
    fts <- rbind(fts, fts_intracountry, fill = T)
  }
  
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
