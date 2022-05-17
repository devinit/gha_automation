suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/general/deflators.R"), source))
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv")

#Establish current DAC base year
dac_base_year <- "https://stats.oecd.org/SDMX-JSON/data/TABLE2A/10200.20001.1.216.D/all?startTime=2000&endTime=2021"
dac_base_year <- as.numeric(data.table(read_json(dac_base_year, simplifyVector = T)$structure$attributes$series)[name == "Reference period"]$values[[1]]$name)

#Load deflators
defl <- get_deflators(base_year = dac_base_year)
defl <- merge(defl, isos[, .(iso3, countryname_oecd)], by.x = "ISO", by.y = "iso3", all.x = T)
defl[, year := as.character(year)]
defl[countryname_oecd == "Russian Federation", countryname_oecd := "Russia"]
defl[countryname_oecd == "Slovakia", countryname_oecd := "Slovak Republic"]

##DAC2a HA
#All recipients
#Total DAC, Total Multilat, Total Non-DAC, Total Private (20001, 20002, 20006, 21600)
#All parts (Part I only available)
#HA (216)
#Constant prices (D)
#2000-2020
dac2a_all <- tabulate_dac_api("TABLE2A", list("", c(20001, 20002, 20006, 21600), 1, 216, "D"), 2000, dac_base_year + 1)

dac2a_all <- melt(dac2a_all, id.vars = c("Recipient", "Donor", "Part", "Aid type", "Amount type"))

##DAC2a Gross ODA and HA for UNHCR and UNWRA
#All recipients
#UNHCR, UNRWA (967, 964)
#All parts (Part I only available)
#Gross ODA and HA (240, 216)
#Constant prices (D)
#2000-2020
dac2a_un <- tabulate_dac_api("TABLE2A", list("", c(967, 964), 1, c(240, 216), "D"), 2000, dac_base_year + 1)

dac2a_un <- melt(dac2a_un, id.vars = c("Recipient", "Donor", "Part", "Aid type", "Amount type"))
dac2a_un <- dac2a_un[, .(un_value = sum(value[`Aid type` == "Memo: ODA Total, Gross disbursements"] - value[`Aid type` == "Humanitarian Aid"])), by = .(variable, Recipient)]

dac2a_dac_ha <- dac2a_all[Donor == "DAC Countries, Total"]
dac2a_multi_ha <- dac2a_all[Donor == "Multilaterals, Total"]

dac2a_multi_ha <- merge(dac2a_multi_ha, dac2a_un, by = c("variable", "Recipient"), all.x = T)
dac2a_multi_ha[is.na(dac2a_multi_ha)] <- 0
dac2a_multi_ha <- dac2a_multi_ha[, .(multi_value = value + un_value), by = .(variable, Recipient)]

##Total DAC HA
dac2a_agg <- merge(dac2a_dac_ha, dac2a_multi_ha, by = c("variable", "Recipient"), all = T)
dac2a_agg[is.na(dac2a_agg)] <- 0
dac2a_agg <- dac2a_agg[, .(value = value + multi_value), by = .(variable, Recipient)]

##Add CERF data 2010-2017
cerf_years <- 2010:2017
cerf_list <- list()
for(i in 1:length(cerf_years)){
  url <- paste0("https://cerf.un.org/fundingByCountry/", cerf_years[i])
  cerf_list[[i]] <- cbind(year = cerf_years[i], read_json(url, simplifyVector = T))
}
cerf <- rbindlist(cerf_list)

#Standardise CERF names
{cerf[name == "Republic of Congo", name := "Congo"]
  cerf[name == "Republic of the Sudan", name := "Sudan"]
  cerf[name == "China", name := "China (People's Republic of)"]
  cerf[name == "Islamic Republic of Iran", name := "Iran"]
  cerf[name == "occupied Palestinian territory", name := "West Bank and Gaza Strip"]
  cerf[name == "Cote d'Ivoire", name := "Côte d'Ivoire"]
  cerf[name == "United Republic of Tanzania", name := "Tanzania"]
  cerf[name == "Swaziland", name := "Eswatini"]}

cerf_ha <- cerf[, .(variable = as.factor(year), Recipient = name, cerf_value = amount/1000000)]

#Deflate using Total DAC Resource Deflator
dacdefl <- defl[ISO == "DAC"]

cerf_ha <- merge(cerf_ha, dacdefl[, .(deflator = gdp_defl, variable = year)])
cerf_ha <- cerf_ha[, .(cerf_value = cerf_value/deflator), by = .(Recipient, variable)]

###Add OCHA data 2011-onwards (MUMS)

##MUMS 
#Donor: DAC Countries, Total (20001)
#All recipients
#Sector: Humanitarian Aid
#Channel: OCHA (41127)
#Gross disbursements
#Contributions through (20)
#Constant prices (D)
#2011-2020
mums_rec <- tabulate_dac_api("MULTISYSTEM", list(20001, "", 700, 41127, 20, 112, "D"), 2000, dac_base_year + 1)

mums_rec <- melt(mums_rec, id.vars = c("Donor", "Recipient", "Sector", "Channel", "AidToThru", "Flow type", "Amount type"))
mums_rec[, value_share := value/value[Recipient == "Developing Countries, Total"], by = .(variable)]

##MUMS 
#Donor: DAC Countries, Total (20001)
#Developing countries, total (10100)
#Sector: All, Total (1000)
#Channel: OCHA (41127)
#Gross disbursements
#Core contributions to (10)
#Constant prices (D)
#2011-2020
mums_don <- tabulate_dac_api("MULTISYSTEM", list(20001, 10100, 1000, 41127, 10, 112, "D"), 2000, dac_base_year + 1)

mums_don <- melt(mums_don, id.vars = c("Donor", "Recipient", "Sector", "Channel", "AidToThru", "Flow type", "Amount type"))
mums_rec <- merge(mums_rec, mums_don[, .(variable, ocha_total = value)], by = "variable")

mums_ha <- mums_rec[, .(ocha_value = ocha_total*value_share), by = .(variable, Recipient)]

###
##Total IHA
total_iha <- merge(dac2a_agg, cerf_ha, by = c("variable", "Recipient"), all = T)
total_iha <- merge(total_iha, mums_ha, by = c("variable", "Recipient"), all = T)
total_iha[is.na(total_iha)] <- 0
total_iha <- total_iha[, .(total_iha = value + cerf_value + ocha_value), by = .(variable, Recipient)]

fwrite(total_iha, "IHA/output/dac_aggregate_recipient.csv")
