suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr", "XML"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/general/deflators.R", "https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R"), source))
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

#Load DAC IHA
dac_iha <- fread("IHA/output/dac_aggregate_donors.csv", encoding = "UTF-8")

#Working years
dac_years <- unique(dac_iha[, min(variable):max(variable)]$variable)
dac_base_year <- max(dac_years) - 1

#Load FTS
fts_read_master <- function(years = years){
  fts_curated_all <- list()
  for(i in 1:length(years)){
    year <- years[i]
    gh_url <- paste0("https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/datasets/fts_curated_master/fts_curated_", year, ".csv")
    fts_curated_all[[i]] <- fread(gh_url, showProgress = F, encoding = "UTF-8")
  }
  fts_curated_all <- rbindlist(fts_curated_all, fill = T, use.names = T)
  #fts_curated_all <- fts_curated_all[, ..columns]
  return(fts_curated_all)
}
#fts <- fts_curated_flows(years = dac_years, update_years = NA, base_year = dac_base_year)
fts <- fts_read_master(years = dac_years)
fts[source_org_country == "Slovakia", source_org_country := "Slovak Republic"]
fts[source_org_country == "Korea, Republic of", source_org_country := "Korea"]
fts[, year := as.character(year)]

#Establish ODA-eligible recipients by year
for(i in 1:(length(dac_years)-1)){
  dac_year <- dac_years[i]
  oda_recipients <- as.data.table(fromJSON(paste0("https://stats.oecd.org/SDMX-JSON/data/TABLE2A/.20005.1.206.D/all?startTime=", dac_year, "&endTime=", dac_year, "&dimensionAtObservation=allDimensions"))$structure$dimensions$observation)[id == "RECIPIENT"]$values[[1]]$name
  oda_recipients <- oda_recipients[!grepl(", Total|, regional", oda_recipients)]
  if(exists("oda_recipients_isos")){
    oda_recipients_isos <- rbind(oda_recipients_isos, cbind(year = dac_year, iso3 = isos[countryname_oecd %in% oda_recipients]$iso3))
  } else {
    oda_recipients_isos <- data.table(year = dac_year, iso3 = isos[countryname_oecd %in% oda_recipients]$iso3)
  }
}
oda_recipients_isos <- rbind(oda_recipients_isos, copy(oda_recipients_isos)[year == dac_base_year][, year := dac_base_year + 1])

#Create FTS ODA-eligibility column
fts[, oda_eligible := F]
fts[paste0(destination_org_iso3, year) %in% oda_recipients_isos[, paste0(iso3, year)], oda_eligible := T]
fts[destination_org_country %in% c("Serbia and Montenegro (until 2006-2009)", "Multi-destination_country", "Global", ""), oda_eligible := T]

#Establish DAC donors
donors <- rbindlist(lapply(xmlToList(htmlParse(GET("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE1")))$body$structure$codelists$codelist, function(x) data.frame(cbind(as.data.table(x)[1, ], as.data.table(x)[2, ]))), fill = T)
dac_donors <- unlist(donors[.attrs.1 == 20001 | .attrs.1 == 20002]$description)

#####
##DAC donors
fts_dac_donors <- fts[(source_orgtype == "DAC governments" | source_org_iso3 == "EUI" ) & domestic_response == F & newMoney == T]
fts_dac_donors[source_org_country == "European Commission", source_org_country := "EU Institutions"]

#DAC donors directly to non-ODA eligible recipients (FTS)
fts_dac_donors_oda <- fts_dac_donors[oda_eligible == T, .(fts_oda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]
fts_dac_donors_nonoda <- fts_dac_donors[oda_eligible == F, .(fts_nonoda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]

#DAC donors imputed EU to non-ODA eligible recipients (FTS)
fts_eu <- fts[source_org_country == "European Commission" & domestic_response == F & newMoney == T & oda_eligible == F]
fts_eu_nonoda <- fts_eu[, .(fts_nonoda_eu_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = year]

dac1_eu <- tabulate_dac_api("TABLE1", list( "", "", 2102, 1140, "D"), dac_years[1], dac_years[length(dac_years)])
dac1_eu <- melt(dac1_eu, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))
dac1_eu[, eu_value_share := value/(as.numeric(value[Donor == "DAC Countries, Total"]) + (value[Donor == "Non-DAC Countries, Total"])), by = variable]
dac1_eu[, variable := as.character(variable)]

dac1_dac_donors_eu <- merge(dac1_eu[Donor %in% dac_donors], fts_eu_nonoda, by.x = "variable", by.y = "year", all.x = T)
fts_dac_donors_nonoda_imeu <- dac1_dac_donors_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(variable, Donor)]

#Total DAC donor HA
total_dac_donor_ha <- merge(dac_iha[Donor %in% dac_donors, .(variable, Donor, total_iha)], fts_dac_donors_nonoda[, .(variable = as.numeric(year), Donor = source_org_country, fts_nonoda_ha)], by = c("variable", "Donor"), all = T)
total_dac_donor_ha[is.na(total_dac_donor_ha)] <- 0
total_dac_donor_ha <- total_dac_donor_ha[, .(total_donor_ha = total_iha + fts_nonoda_ha), by = .(Donor, variable)]

#####
##NDD
fts_ndd <- fts[source_orgtype == "NDD" & domestic_response == F & newMoney == T]

#NDD directly to all recipients (FTS)
fts_ndd <- fts_ndd[, .(fts_ndd_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]

#NDD imputed EU to non-ODA-eligible recipients (FTS)
dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors)], fts_eu_nonoda, by.x = "variable", by.y = "year", all.x = T)
fts_ndd_nonoda_imeu <- dac1_ndd_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(variable, Donor)]

#NDD imputed EU to ODA-eligible recipients (DAC + FTS)
total_eu_ha <- total_dac_donor_ha[Donor == "EU Institutions"]
ndd_eu_share <- dac1_eu[, .(ndd_eu_share = value[Donor == "Non-DAC Countries, Total"]/value[Donor == "DAC Countries, Total"]), by = variable]

ndd_eu_share <- merge(ndd_eu_share[, .(variable = as.numeric(as.character(variable)), ndd_eu_share)], total_eu_ha[, .(variable, total_donor_ha)])
ndd_eu_ha <- ndd_eu_share[, .(ndd_eu_ha = ndd_eu_share*total_donor_ha), by = variable]

dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors), .(Donor, variable = as.numeric(as.character(variable)), eu_value_share)], ndd_eu_ha, by = "variable", all.x = T)
dac1_ndd_imeu <- dac1_ndd_eu[, .(dac1_ndd_imeu_ha = eu_value_share*ndd_eu_ha), by = .(Donor, variable)]

#Total NDD HA
total_ndd_ha <- merge(fts_ndd_nonoda_imeu, fts_ndd, by.x = c("variable", "Donor"), by.y = c("year", "source_org_country"), all = T)
total_ndd_ha <- merge(total_ndd_ha, dac1_ndd_imeu[, .(Donor, variable = as.character(variable), dac1_ndd_imeu_ha)], by = c("variable", "Donor"), all = T)

total_ndd_ha[is.na(total_ndd_ha)] <- 0
total_ndd_ha <- total_ndd_ha[, .(total_donor_ha = fts_nonoda_imeu_ha + fts_ndd_ha + dac1_ndd_imeu_ha), by = .(Donor, variable)]

####
##All donors

#Total donor IHA
total_donor_ha <- rbind(total_dac_donor_ha, total_ndd_ha)[order(variable, Donor)]
total_donor_ha <- total_donor_ha[!grepl(", Total|Total DAC", Donor)]

total_donor_ha <- merge(total_donor_ha, isos[, .(iso3, countryname_fts)], by.x = "Donor", by.y = "countryname_fts", all.x = T)
total_donor_ha[Donor == "EU Institutions", iso3 := "EUI"]
total_donor_ha[Donor == "Korea", iso3 := "KOR"]
total_donor_ha[Donor == "Slovak Republic", iso3 := "SVK"]
total_donor_ha[Donor == "Hong Kong", iso3 := "HKG"]
total_donor_ha[Donor == "Virgin Islands, British", iso3 := "VGB"]
total_donor_ha[Donor == "Serbia and Montenegro (until 2006-2009)", iso3 := "SCG"]

fwrite(total_donor_ha, "IHA/output/total_donor_ha.csv")
