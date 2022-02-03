suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

tabulate_dac_api <- function(api){
  api_out <- read_json(api, simplifyVector = T)
  
  series_names <- api_out$structure$dimensions$series$name
  series_con <- api_out$structure$dimensions$series$values
  series_con <- lapply(series_con, function(x) data.table(cbind(id = as.numeric(rownames(x))-1, name = x$name)))
  
  obs_names <- api_out$structure$dimensions$observation$values[[1]]$name
  
  row_ids <- strsplit(names(api_out$dataSets$series), ":")
  row_names <- rbindlist(lapply(row_ids, function(x) sapply(1:length(x), function(i) series_con[[i]][id == x[[i]]][,2])))
  names(row_names) <- series_names
  
  dac_tab <- rbindlist(lapply(api_out$dataSets$series, function(x) x$observations), fill = T)
  dac_tab[dac_tab == "NULL"] <- 0
  dac_tab <- sapply(dac_tab, function(x) sapply(x, function(y) as.numeric(y[[1]])))
  if(is.null(dim(dac_tab)[1]))
    dac_tab <- t(dac_tab)
  dac_tab <- data.table(dac_tab)
  names(dac_tab) <- obs_names
  
  dac_tab <- cbind(row_names, dac_tab)
  
  return(dac_tab)
}

#Establish current DAC base year
dac_base_year <- "https://stats.oecd.org/SDMX-JSON/data/TABLE2A/10200.20001.1.216.D/all?startTime=2000&endTime=2020"
dac_base_year <- data.table(read_json(dac_base_year, simplifyVector = T)$structure$attributes$series)[name == "Reference period"]$values[[1]]$name

##DAC1 Bilateral HA and EU disb
#All donors
#All parts (Part I only available)
#Disbursements to Humanitarian and EU (70, 2102)
#Net disbursement flows (1140)
#Constant prices (D)
#2000-2020
api_dac1 <- "https://stats.oecd.org/SDMX-JSON/data/TABLE1/..70+2102.1140.D/all?startTime=2000&endTime=2020"
dac1 <- tabulate_dac_api(api_dac1)

dac1 <- melt(dac1, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))

#Bilateral HA
dac1_ha <- dac1[`Aid type` == "hist: humanitarian aid grants"]

##DAC2a Core multilateral ODA
#Disbursements to AfDB, AfDF, AsDBSF, AsDB, IDA, IDB, IDB SOF, UNDP, UNFPA, UNHCR, UNICEF, UNRWA, WFP (913, 914, 916, 915, 905, 909, 912, 959, 974, 967, 963, 964, 966)
#All donors
#All parts (Part I only available)
#Gross ODA (240)
#Constant prices (D)
#2000-2020
api_dac2a_cmo <- "https://stats.oecd.org/SDMX-JSON/data/TABLE2A/913+914+916+915+905+909+912+959+974+967+963+964+966..1.240.D/all?startTime=2000&endTime=2020"
dac2a_cmo <- tabulate_dac_api(api_dac2a_cmo)

dac2a_cmo <- melt(dac2a_cmo, id.vars = c("Recipient", "Donor", "Part", "Aid type", "Amount type"))

##DAC2a Multilateral ODA and HA
#Humanitarian and Gross ODA (216, 240)
#Selected multi donors
#Developing countries, total
#All parts (Part I only available)
#Constant prices (D)
#2000-2020
api_dac2a_moha <- "https://stats.oecd.org/SDMX-JSON/data/TABLE2A/10100.918+913+914+915+909+1013+976+932+940+923+959+807+974+967+963+964+966+928+905+1012+953+921+1020+1011+1016+104+951.1.216+240.D/all?startTime=2000&endTime=2020"
dac2a_moha <- tabulate_dac_api(api_dac2a_moha)

dac2a_moha <- melt(dac2a_moha, id.vars = c("Recipient", "Donor", "Part", "Aid type", "Amount type"))
dac2a_moha_share <- dac2a_moha[, .(ha_share = value[`Aid type` == "Humanitarian Aid"]/value[`Aid type` == "Memo: ODA Total, Gross disbursements"]), by = .(variable, Donor)]

#Manually set UNHCR and UNRWA to 100%
dac2a_moha_share[Donor %in% c("UNHCR", "UNRWA"), ha_share := 1]

#Standardise dac2a names
{dac2a_cmo[Recipient == "Af. D B", Recipient := "African Development Bank [AfDB]"]
dac2a_cmo[Recipient == "Asian Dev. Bank", Recipient := "Asian Development Bank [AsDB]"]
dac2a_cmo[Recipient == "IDA", Recipient := "International Development Association [IDA]"]
dac2a_cmo[Recipient == "IDB Special Oper. Fund", Recipient := "Inter-American Development Bank [IDB]"]
dac2a_cmo[Recipient == "AsDB Special Funds", Recipient := "Asian Development Bank [AsDB]"]
dac2a_cmo[Recipient == "African Dev. Fund", Recipient := "African Development Fund [AfDF]"]
dac2a_cmo[Recipient == "IDB", Recipient := "Inter-American Development Bank [IDB]"]}

##Calculate imputed multilateral HA from DAC2A
dac2a_cmo <- merge(dac2a_cmo, dac2a_moha_share, by.x = c("variable", "Recipient"), by.y = c("variable", "Donor"), all.x = T)
dac2a_imha <- dac2a_cmo[, .(dac2a_imputed_multi_ha = sum(value*ha_share, na.rm = T)), by = .(variable, Donor)]

##MUMS 
#All donors
#Developing countries, total (10100)
#Selected multi donors
#Gross disbursements
#Core contributions to
#Constant prices (D)
#2011-2020
api_mums <- "https://stats.oecd.org/SDMX-JSON/data/MULTISYSTEM/.10100.1000.41301+47066+41122+41114+41116+41127+41121+41141+41119+41130+41140+41307+41143+44002+46002+46003+46004+46005+46024+46013+46012+47111+47134+47129+47130+47044+47128+47142+47135.10.112.D/all?startTime=2011&endTime=2020"
mums <- tabulate_dac_api(api_mums)

mums <- melt(mums, id.vars = c("Donor", "Recipient", "Sector", "Channel", "AidToThru", "Flow type", "Amount type"))

#Standardise MUMS names
{mums[Channel == "Food and Agricultural Organisation", Channel := "Food and Agriculture Organisation [FAO]"]
mums[Channel == "United Nations Children’s Fund", Channel := "UNICEF"]
mums[Channel == "United Nations Development Programme", Channel := "UNDP"]
mums[Channel == "United Nations Environment Programme", Channel := "UNEP"]
mums[Channel == "United Nations Office of the United Nations High Commissioner for Refugees", Channel := "UNHCR"]
mums[Channel == "United Nations Peacebuilding Fund", Channel := "UN Peacebuilding Fund [UNPBF]"]
mums[Channel == "United Nations Population Fund", Channel := "UNFPA"]
mums[Channel == "United Nations Relief and Works Agency for Palestine Refugees in the Near East", Channel := "UNRWA"]
mums[Channel == "World Food Programme", Channel := "WFP"]
mums[Channel == "World Health Organisation - assessed contributions", Channel := "World Health Organisation [WHO]"]
mums[Channel == "World Health Organisation - core voluntary contributions account", Channel := "World Health Organisation [WHO]"]
mums[Channel == "International Development Association", Channel := "International Development Association [IDA]"]
mums[Channel == "African Development Bank", Channel := "African Development Bank [AfDB]"]
mums[Channel == "African Development Fund", Channel := "African Development Fund [AfDF]"]
mums[Channel == "Asian Development Bank", Channel := "Asian Development Bank [AsDB]"]
mums[Channel == "Asian Development Fund", Channel := "Asian Development Bank [AsDB]"]
mums[Channel == "Council of Europe Development Bank", Channel := "Council of Europe Development Bank [CEB]"]
mums[Channel == "Inter-American Development Bank, Fund for Special Operations", Channel := "Inter-American Development Bank [IDB]"]
mums[Channel == "Inter-American Development Bank, Inter-American Investment Corporation and Multilateral Investment Fund", Channel := "Inter-American Development Bank [IDB]"]
mums[Channel == "Clean Technology Fund", Channel := "Climate Investment Funds [CIF]"]
mums[Channel == "Nordic Development Fund", Channel := "Nordic Development Fund [NDF]"]
mums[Channel == "Strategic Climate Fund", Channel := "Climate Investment Funds [CIF]"]
mums[Channel == "OPEC Fund for International Development", Channel := "OPEC Fund for International Development [OPEC Fund]"]}

#Remove multilats which are already accounted for in DAC2A
mums_exc_dac2a <- mums[!(Channel %in% dac2a_cmo$Recipient)]

#Calculate imputed multilateral HA from MUMS
mums_exc_dac2a <- merge(mums_exc_dac2a, dac2a_moha_share, by.x = c("variable", "Channel"), by.y = c("variable", "Donor"), all.x = T)

#Manually set UNOCHA to 100%
mums_exc_dac2a[Channel == "United Nations Office of Co-ordination of Humanitarian Affairs", ha_share := 1]

mums_imha <- mums_exc_dac2a[, .(mums_imputed_multi_ha = sum(value*ha_share, na.rm = T)), by = .(variable, Donor)]

# ##CERF
# cerf_years <- 2010:2020
# cerf_list <- list()
# for(i in 1:length(cerf_years)){
#   url <- paste0("https://cerf.un.org/contributionsByDonor/", cerf_years[i])
#   cerf_list[[i]] <- read_json(url, simplifyVector = T)$data
# }
# cerf <- rbindlist(cerf_list)
# 
# #Standardise CERF names
# {cerf[donor == "Slovakia", donor := "Slovak Republic"]
# cerf[donor == "United States of America", donor := "United States"]
# cerf[donor == "Russian Federation", donor := "Russia"]
# cerf[donor == "Hyogo Prefecture (Japan)", donor := "Japan"]
# cerf[donor == "Belgian Government of Flanders", donor := "Belgium"]
# cerf[donor == "State of South Australia", donor := "Australia"]
# cerf[donor == "Catalan Agency for Development Cooperation", donor := "Spain"]}
# 
# ##TODO INCLUDE CERF

####
##Total Imputed HA
total_imha <- merge(dac2a_imha, mums_imha, all = T)
#total_imha <- merge(total_ihma, cerf_ihma, all = T)
total_imha[is.na(total_imha)] <- 0
total_imha[, total_imha := (dac2a_imputed_multi_ha + mums_imputed_multi_ha)]
#total_imha[, total_imha := (total_ihma + cerf_imputed_multi_ha)]

##EU imputations
dac1_eu <- dac1[`Aid type` == "I.B.1.2. EU institutions"]
dac1_eu[, eu_value_share := value/(as.numeric(value[Donor == "DAC Countries, Total"]) + (value[Donor == "Non-DAC Countries, Total"])), by = variable]

total_eu <- merge(dac1[Donor == "EU Institutions"], total_imha[Donor == "EU Institutions", .(variable, total_imha)])
total_eu <- total_eu[, .(total_eu = value + total_imha), by = "variable"]

dac1_eu <- merge(dac1_eu, total_eu, by = "variable", all.x = T)
eu_imha <- dac1_eu[, .(eu_imha = eu_value_share*total_eu), by = .(variable, Donor)]

###
##Total IHA
total_iha <- merge(dac1_ha, total_imha, by = c("variable", "Donor"), all = T)
total_iha <- merge(total_iha, eu_imha, by = c("variable", "Donor"), all = T)
total_iha[is.na(total_iha)] <- 0
total_iha <- total_iha[, .(total_iha = value + total_imha + eu_imha), by = .(variable, Donor)]

fwrite(total_iha, "IHA/output/dac_aggregate_donors.csv")