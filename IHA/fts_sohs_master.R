fts_save_master <- function(years, path = "IHA/datasets/fts_curated_master/"){
  source("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R")
  fts_all <- fts_curated_flows(years = years)
  for(i in 1:length(years)){
    fwrite(fts_all[year == years[[i]]], paste0(path, "fts_curated_", years[[i]], ".csv"))
  }
}

fts_read_master <- function(years = years, extra_columns = NULL){
  main_columns <- c("id", "year", "destinationObjects_Plan.name", "description", "donor_country_iso3", "donor_country", "sourceObjects_Organization.name", "source_org_type", "recipient", "iso3", "destinationObjects_Organization.name", "gha_channel", "channel", "subchannel", "multiyear", "dummy", "status", "amountUSD_defl")
  columns <- c(main_columns, extra_columns)
  fts_curated_all <- list()
  for(i in 1:length(years)){
    year <- years[i]
    gh_url <- paste0("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/datasets/fts_curated_", year, ".csv")
    fts_curated_all[[i]] <- fread(gh_url, showProgress = F, encoding = "UTF-8")
  }
  fts_curated_all <- rbindlist(fts_curated_all)
  fts_curated_all <- fts_curated_all[, ..columns]
  return(fts_curated_all)
}
