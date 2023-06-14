suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr", "XML"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/general/deflators.R", "https://raw.githubusercontent.com/devinit/gha_automation/main/IHA/fts_curated_flows.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/gha/FTS/fts_split_rows.R"), source))
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8")

#Establish current DAC base year
dac_base_year <- paste0("https://stats.oecd.org/SDMX-JSON/data/TABLE2A/10200.20001.1.216.D/all?startTime=2000&endTime=", year(Sys.Date()))
dac_base_year <- as.numeric(data.table(read_json(dac_base_year, simplifyVector = T)$structure$attributes$series)[name == "Reference period"]$values[[1]]$name)

#Load deflators
defl <- get_deflators(dac_base_year)
defl <- merge(defl, isos[, .(iso3, countryname_oecd)], by.x = "ISO", by.y = "iso3", all.x = T)
defl[, year := as.character(year)]
defl[countryname_oecd == "Russian Federation", countryname_oecd := "Russia"]
defl[countryname_oecd == "Slovakia", countryname_oecd := "Slovak Republic"]

#Load DAC IHA
#dac_iha <- fread("IHA/output/dac_aggregate_donors.csv", encoding = "UTF-8")
dac_iha_bimulti <- fread("IHA/output/dac_aggregate_donors_bimulti.csv", encoding = "UTF-8")[!(grepl(", Total", Donor))]

#Working years
dac_years <- unique(dac_iha_bimulti[, min(variable):max(variable)]$variable)

#Load FTS
fts_read_master <- function(years = 2000:(max(dac_years))){
  fts_curated_all <- list()
  for(i in 1:length(years)){
    year <- years[i]
    gh_url <- paste0("IHA/datasets/fts_curated_master/fts_curated_", year, ".csv")
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

#Set FTS accounting year from donor perspective
fts[status == "paid", year := as.character(year(date))]

fts <- rbind(fts[!grepl(";", year)], fts_split_rows(fts[grepl(";", year)], c("amountUSD_defl"), "year", remove.unsplit = T)[])
fts <- fts[year %in% dac_years]

#Re-deflate FTS based on accounting year
fts <- merge(fts, defl[, .(source_org_iso3 = ISO, year = as.character(year), gdp_defl)], by = c("source_org_iso3", "year"), all.x = T)
fts[dummy == F & !is.na(gdp_defl), `:=` (amountUSD_defl = amountUSD/gdp_defl, amountUSD_defl_millions = (amountUSD/gdp_defl)/1000000)]

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
fts_dac_donors <- fts[(source_orgtype == "DAC governments" | source_org_iso3 == "EUI" ) & domestic_response == F & newMoney == T & dummy == F & destinationObjects_Organization.name != "Central Emergency Response Fund"]
fts_dac_donors[source_org_country == "European Commission", source_org_country := "EU Institutions"]

#DAC donors directly to non-ODA eligible recipients (FTS)
fts_dac_donors_oda <- fts_dac_donors[oda_eligible == T, .(fts_oda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]
fts_dac_donors_nonoda <- fts_dac_donors[oda_eligible == F, .(fts_nonoda_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]

#DAC donors imputed EU to non-ODA eligible recipients (FTS)
fts_eu <- fts[source_org_country == "European Commission" & domestic_response == F & newMoney == T & dummy == F & destinationObjects_Organization.name != "Central Emergency Response Fund"]
fts_eu_oda <- fts_eu[oda_eligible == T, .(fts_oda_eu_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = year]
fts_eu_nonoda <- fts_eu[oda_eligible == F, .(fts_nonoda_eu_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = year]

dac1_eu <- tabulate_dac_api("TABLE1", list( "", "", 2102, 1140, "D"), dac_years[1], dac_years[length(dac_years)])
dac1_eu <- melt(dac1_eu, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))
dac1_eu[, eu_value_share := value/(as.numeric(value[Donor == "DAC Countries, Total"]) + (value[Donor == "Non-DAC Countries, Total"])), by = variable]
dac1_eu[, variable := as.character(variable)]

dac1_dac_donors_oda_eu <- merge(dac1_eu[Donor %in% dac_donors], fts_eu_oda, by.x = "variable", by.y = "year", all.x = T)
fts_dac_donors_oda_imeu <- dac1_dac_donors_oda_eu[, .(fts_oda_imeu_ha = eu_value_share*fts_oda_eu_ha), by = .(variable, Donor)]

dac1_dac_donors_nonoda_eu <- merge(dac1_eu[Donor %in% dac_donors], fts_eu_nonoda, by.x = "variable", by.y = "year", all.x = T)
fts_dac_donors_nonoda_imeu <- dac1_dac_donors_nonoda_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(variable, Donor)]

##For most recent year, decide to use DAC or FTS for ODA-eligible recipients (because preliminary DAC data is... preliminary)
fts_dac_donors_prelim <- merge(fts_dac_donors_oda[year == max(dac_years), .(variable = as.numeric(year), Donor = source_org_country, fts_oda_ha)], fts_dac_donors_oda_imeu[variable == max(dac_years), .(variable = as.numeric(variable), Donor, fts_oda_imeu_ha)], all = T)
dac1_dac_donors_prelim <- dac_iha_bimulti[Donor %in% dac_donors & variable == max(dac_years)]

dac_donors_prelim <- merge(fts_dac_donors_prelim, dac1_dac_donors_prelim, all = T)
dac_donors_prelim[is.na(dac_donors_prelim)] <- 0

#Where FTS bilat is greater than DAC bilat + DAC imputed, use FTS bilat only
dac_donors_prelim[fts_oda_ha > total_bilat + total_imha, `:=` (total_bilat = 0, total_imha = 0)]

#Where FTS bilat is greater than DAC bilat , use FTS bilat and keep DAC imputed 
dac_donors_prelim[fts_oda_ha > total_bilat, `:=` (total_bilat = 0)]

#Otherwise, use DAC bilat and DAC imputed
dac_donors_prelim[fts_oda_ha <= total_bilat, `:=` (fts_oda_ha = 0)]

#Use DAC or FTS for EU imputed for each country based on choice value for EU
dac_eu <- dac_donors_prelim[Donor == "EU Institutions", total_bilat > fts_oda_ha]
dac_donors_prelim[, `:=` (eu_imha = eu_imha*dac_eu, fts_oda_imeu_ha = fts_oda_imeu_ha*(1-dac_eu))]

dac_donors_prelim <- dac_donors_prelim[, .(total_bilat = total_bilat + fts_oda_ha, total_imha, eu_imha = eu_imha + fts_oda_imeu_ha), by = .(Donor, variable)]

##CERF
cerf_list <- list()
for(i in 1:length(max(dac_years))){
  url <- paste0("https://cerf.un.org/contributionsByDonor/",  max(dac_years)[i])
  cerf_list[[i]] <- cbind(read_json(url, simplifyVector = T)$data, variable =  max(dac_years)[i])
}
cerf <- rbindlist(cerf_list)

#Standardise CERF names
{cerf[donor == "Slovakia", donor := "Slovak Republic"]
  cerf[donor == "United States of America", donor := "United States"]
  cerf[donor == "Russian Federation", donor := "Russia"]
  cerf[donor == "Hyogo Prefecture (Japan)", donor := "Japan"]
  cerf[donor == "Belgian Government of Flanders", donor := "Belgium"]
  cerf[donor == "State of South Australia", donor := "Australia"]
  cerf[donor == "Catalan Agency for Development Cooperation", donor := "Spain"]}

missing_cerf <- cerf[, .(Donor = donor, variable = as.character(variable), cerf_ha = paid/1000000)]
missing_cerf <- merge(missing_cerf, defl[, .(countryname_oecd, year, gdp_defl)], by.x = c("Donor", "variable"), by.y = c("countryname_oecd", "year"), all.x = T)

missing_cerf <- missing_cerf[, .(missing_cerf = sum(cerf_ha/gdp_defl, na.rm = T)), by = .(Donor, variable = as.integer(variable))]

dac_donors_prelim <- merge(dac_donors_prelim, missing_cerf, by = c("Donor", "variable"), all.x = T)
dac_donors_prelim[is.na(dac_donors_prelim)] <- 0

dac_donors_prelim <- dac_donors_prelim[, .(total_bilat, total_imha = total_imha + missing_cerf, eu_imha), by = .(Donor, variable)]

dac_iha_bimulti <- rbind(dac_iha_bimulti[variable != max(dac_years)], dac_donors_prelim)

#Total DAC donor HA
total_dac_donor_ha <- merge(dac_iha_bimulti[Donor %in% dac_donors], fts_dac_donors_nonoda[, .(variable = as.numeric(year), Donor = source_org_country, fts_nonoda_ha)], by = c("variable", "Donor"), all = T)
total_dac_donor_ha <- merge(total_dac_donor_ha, fts_dac_donors_nonoda_imeu[, .(variable = as.numeric(variable), Donor, fts_nonoda_imeu_ha)], by = c("variable", "Donor"), all = T)
total_dac_donor_ha[is.na(total_dac_donor_ha)] <- 0

total_dac_donor_ha <- total_dac_donor_ha[, total_donor_ha := total_bilat + total_imha + eu_imha + fts_nonoda_ha + fts_nonoda_imeu_ha, by = .(Donor, variable)]

#####
##NDD
fts_ndd <- fts[source_orgtype == "NDD" & domestic_response == F & newMoney == T & destinationObjects_Organization.name != "Central Emergency Response Fund"]

#NDD directly to all recipients (FTS)
fts_ndd <- fts_ndd[, .(fts_ndd_ha = sum(amountUSD_defl, na.rm = T)/1000000), by = .(source_org_country, year)]

#NDD imputed EU to non-ODA-eligible recipients (FTS)
dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors) & !(grepl(", Total", Donor))], fts_eu_nonoda, by.x = "variable", by.y = "year", all.x = T)
fts_ndd_nonoda_imeu <- dac1_ndd_eu[, .(fts_nonoda_imeu_ha = eu_value_share*fts_nonoda_eu_ha), by = .(variable, Donor)]

#NDD imputed EU to ODA-eligible recipients (DAC + FTS)
total_eu_ha <- total_dac_donor_ha[Donor == "EU Institutions"]
ndd_eu_share <- dac1_eu[, .(ndd_eu_share = value[Donor == "Non-DAC Countries, Total"]/value[Donor == "DAC Countries, Total"]), by = variable]

ndd_eu_share <- merge(ndd_eu_share[, .(variable = as.numeric(as.character(variable)), ndd_eu_share)], total_eu_ha[, .(variable, total_donor_ha)])
ndd_eu_ha <- ndd_eu_share[, .(ndd_eu_ha = ndd_eu_share*total_donor_ha), by = variable]

dac1_ndd_eu <- merge(dac1_eu[!(Donor %in% dac_donors) & !(grepl(", Total", Donor)), .(Donor, variable = as.numeric(as.character(variable)), eu_value_share)], ndd_eu_ha, by = "variable", all.x = T)
dac1_ndd_imeu <- dac1_ndd_eu[, .(dac1_ndd_imeu_ha = eu_value_share*ndd_eu_ha), by = .(Donor, variable)]

#Total NDD HA
total_ndd_ha <- merge(fts_ndd_nonoda_imeu, fts_ndd, by.x = c("variable", "Donor"), by.y = c("year", "source_org_country"), all = T)
total_ndd_ha <- merge(total_ndd_ha, dac1_ndd_imeu[, .(Donor, variable = as.character(variable), dac1_ndd_imeu_ha)], by = c("variable", "Donor"), all = T)

total_ndd_ha[is.na(total_ndd_ha)] <- 0
total_ndd_ha <- total_ndd_ha[, total_donor_ha := fts_nonoda_imeu_ha + fts_ndd_ha + dac1_ndd_imeu_ha, by = .(Donor, variable)]

####
##All donors

#Total donor IHA
total_donor_ha <- rbind(total_dac_donor_ha, total_ndd_ha, fill = T)[order(variable, Donor)]
total_donor_ha <- total_donor_ha[!grepl(", Total|Total DAC", Donor)]

total_donor_ha <- merge(total_donor_ha, isos[, .(iso3, countryname_fts)], by.x = "Donor", by.y = "countryname_fts", all.x = T)
total_donor_ha[Donor == "EU Institutions", iso3 := "EUI"]
total_donor_ha[Donor == "Korea", iso3 := "KOR"]
total_donor_ha[Donor == "Slovak Republic", iso3 := "SVK"]
total_donor_ha[Donor == "Hong Kong", iso3 := "HKG"]
total_donor_ha[Donor == "Virgin Islands, British", iso3 := "VGB"]
total_donor_ha[Donor == "Serbia and Montenegro (until 2006-2009)", iso3 := "SCG"]

fwrite(total_donor_ha, "IHA/output/total_bimulti_donor_ha.csv")
fwrite(total_donor_ha[, .(Donor, variable, total_donor_ha, iso3)], "IHA/output/total_donor_ha.csv")

####
##Annual total

total_public_iha <- total_donor_ha[, .(total_public_iha = sum(total_donor_ha, na.rm = T) - sum(eu_imha, na.rm = T) - sum(fts_nonoda_imeu_ha, na.rm = T) - sum(dac1_ndd_imeu_ha, na.rm = T)), by = (year = variable)][order(year)]
fwrite(total_public_iha, "IHA/output/total_government_iha_by_year.csv")
