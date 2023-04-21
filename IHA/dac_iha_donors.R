suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "httr", "readxl", "XML"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R", "https://raw.githubusercontent.com/devinit/di_script_repo/main/general/deflators.R"), source))
isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv")

#Current year
current_year <- year(Sys.Date())

#Establish maximum DAC1 year
tl_dac1 <- tabulate_dac_api("TABLE1", list("", 1, 1010, 1140, "D"), 2000, current_year)
dac1_max_year <- max(as.numeric(names(tl_dac1)), na.rm = T)

#Establish maximum DAC2a year
tl_dac2a <- tabulate_dac_api("TABLE2A", list(10200, 20001, 1, 216, "D"), 2000, current_year)
dac2a_max_year <- max(as.numeric(names(tl_dac2a)), na.rm = T)

#If preliminary HA ODA data is available, load it
if(dac1_max_year > dac2a_max_year){
  oda_prelim <- GET(paste0("https://www.oecd.org/dac/financing-sustainable-development/development-finance-data/ADV", dac1_max_year, ".xlsx"))
  if(status_code(oda_prelim) == 404){
    
    errorCondition(paste("No preliminary ODA data exists for", dac1_max_year, "yet."))
  } else {
    
    tmp <- tempfile()
    writeBin(content(oda_prelim), tmp)
    oda_prelim <- data.table(read_excel(tmp, skip = 3))
    
    ha_oda_prelim <- oda_prelim[grepl("Humanitarian", `...2`)]
    ha_oda_prelim <- suppressWarnings(melt(ha_oda_prelim[, -c("...1", "...2"), with = F], id.vars = NULL))
    
    ha_oda_prelim <- ha_oda_prelim[variable != "Total DAC", .(Donor = variable, Recipient = "Developing Countries, Total", Part = "1 : Part I - Developing Countries", `Aid type` = "Humanitarian Aid", `Amount type` = "Constant Prices", variable = dac1_max_year, value)]
  }
}
  
#Establish current DAC base year
dac_base_year <- paste0("https://stats.oecd.org/SDMX-JSON/data/TABLE2A/10200.20001.1.216.D/all?startTime=2000&endTime=", current_year)
dac_base_year <- as.numeric(data.table(read_json(dac_base_year, simplifyVector = T)$structure$attributes$series)[name == "Reference period"]$values[[1]]$name)

#Load deflators
defl <- get_deflators(base_year = dac_base_year)
defl <- merge(defl, isos[, .(iso3, countryname_oecd)], by.x = "ISO", by.y = "iso3", all.x = T)
defl[, year := as.character(year)]
defl[countryname_oecd == "Russian Federation", countryname_oecd := "Russia"]
defl[countryname_oecd == "Slovakia", countryname_oecd := "Slovak Republic"]

#Establish country donors
donors <- rbindlist(lapply(xmlToList(htmlParse(GET("https://stats.oecd.org/restsdmx/sdmx.ashx/GetDataStructure/TABLE2A")))$body$structure$codelists[[2]], function(x) data.frame(cbind(as.data.table(x)[1, ], as.data.table(x)[2, ]))), fill = T)
country_donors <- c(iconv(iconv(unlist(donors[.attrs.1 %in% c(20001, 20006)]$description), "UTF-8"), "UTF-8"), "EU Institutions")

##DAC2a Gross ODA and HA
#Humanitarian and Gross ODA (216, 240)
#All donors
#Developing countries, total (10100)
#All parts (Part I only available)
#Constant prices (D)
#2000-2020
dac2a <- tabulate_dac_api("TABLE2A", list(10100, "", 1, c(216, 240), "D"), 2000, dac2a_max_year)
dac2a <- melt(dac2a, id.vars = c("Donor", "Recipient", "Part", "Aid type", "Amount type"))
dac2a_ha <- dac2a[`Aid type` == "Humanitarian Aid" & Donor %in% country_donors]

if(exists("ha_oda_prelim")) dac2a_ha <- rbind(dac2a_ha, ha_oda_prelim)

##DAC1 EU disb
#All donors
#All parts (Part I only available)
#Disbursements to EU (2102)
#Net disbursement flows (1140)
#Constant prices (D)
#2000-2020
dac1_eu <- tabulate_dac_api("TABLE1", list( "", "", c(2102), 1140, "D"), 2000, dac1_max_year)
dac1_eu <- melt(dac1_eu, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))

##DAC2a Core multilateral ODA
#Disbursements to AfDB, AfDF, AsDBSF, AsDB, IDA, IDB, IDB SOF, UNDP, UNFPA, UNHCR, UNICEF, UNRWA, WFP (913, 914, 916, 915, 905, 909, 912, 959, 974, 967, 963, 964, 966)
#All donors
#All parts (Part I only available)
#Gross ODA (240)
#Constant prices (D)
#2000-2020
dac2a_cmo <- tabulate_dac_api("TABLE2A", list( c(905,909,912,913,914,915,916,959,963,964,966,967,974), "", 1, 240, "D"), 2000, dac2a_max_year)
dac2a_cmo <- melt(dac2a_cmo, id.vars = c("Recipient", "Donor", "Part", "Aid type", "Amount type"))

##DAC2a Multilateral HA share
dac2a_moha_share <- dac2a[, .(ha_share = value[`Aid type` == "Humanitarian Aid"]/value[`Aid type` == "Memo: ODA Total, Gross disbursements"]), by = .(variable, Donor)]

#Manually set UNHCR, UNRWA and CERF to 100%
dac2a_moha_share[Donor %in% c("UNHCR", "UNRWA", "Central Emergency Response Fund [CERF]"), ha_share := 1]

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
mums <- tabulate_dac_api("MULTISYSTEM", list("", 10100, 1000, c(41147, 41301,41302,47066,41122,41114,41116,41127,41121,41141,41144,41119,41130,41140,41307,41143,44002,46002,46003,46004,46005,46024,46013,46012,47111,47134,47129,47130,47044,47128,47142,47135), 10, 112, "D"), 2000, dac2a_max_year)

mums_max_year <-  max(as.numeric(names(mums)), na.rm = T)

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
  mums[Channel == "Central Emergency Response Fund", Channel := "Central Emergency Response Fund [CERF]"]
  mums[grepl("Global Environment Facility", Channel), Channel := "Global Environment Facility [GEF]"]
  mums[grepl("International Labour Organisation", Channel), Channel := "International Labour Organisation [ILO]"]
}

#Remove multilats which are already accounted for in DAC2A
mums_exc_dac2a <- mums[!(Channel %in% dac2a_cmo$Recipient)]

#Calculate imputed multilateral HA from MUMS
mums_exc_dac2a <- merge(mums_exc_dac2a, dac2a_moha_share, by.x = c("variable", "Channel"), by.y = c("variable", "Donor"), all.x = T)

#Manually set UNOCHA to 100%
mums_exc_dac2a[Channel == "United Nations Office of Co-ordination of Humanitarian Affairs", ha_share := 1]

mums_imha <- mums_exc_dac2a[, .(mums_imputed_multi_ha = sum(value*ha_share, na.rm = T)), by = .(variable, Donor)]

#Estimate DAC2a and MUMS imputed for recent missing year(s)
if(max(as.character(dac2a_moha_share$variable)) < (dac1_max_year)){
  
  missing_years <- max(as.character(dac2a_moha_share$variable)):(dac1_max_year)
  
  #UN HA
  
  ##DAC1 Bilateral ODA to UN agencies
  #All donors
  #All parts (Part I only available)
  #Disbursements to UN agencies (2101)
  #Net disbursement flows (1140)
  #Constant prices (D)
  #2000-2020
  dac1_un <- tabulate_dac_api("TABLE1", list( "", "", 2101, 1140, "D"), 2000, dac1_max_year)
  dac1_un <- melt(dac1_un, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))
  
  dac1_un <- dac1_un[variable %in% missing_years][order(Donor, variable)]
  dac1_un_growth <- dac1_un[, growth := value/shift(value), by = .(Donor)][!is.na(growth)][, value := NULL][]
  
  UN_dac2a_agencies <- c("UNDP", "UNFPA", "UNHCR", "UNICEF", "UNRWA", "WFP")
  dac2a_un <- dac2a_cmo[Recipient %in% UN_dac2a_agencies]
  dac2a_un_imputed <- dac2a_un[variable %in% missing_years, .(dac2a_imputed_un_ha = sum(value*ha_share, na.rm = T)), by = .(variable, Donor)][, variable := as.character(as.numeric(as.character(variable)) + 1)][]
  
  UN_mums_agencies <- c("Food and Agriculture Organisation [FAO]", "International Labour Organisation [ILO]", "International Organisation for Migration", "UNEP", "United Nations Office of Co-ordination of Humanitarian Affairs", "UN Peacebuilding Fund [UNPBF]", "World Health Organisation [WHO]")
  mums_un <- mums_exc_dac2a[Channel %in% UN_mums_agencies]
  mums_un_imputed <- mums_un[variable %in% missing_years, .(mums_imputed_un_ha = sum(value*ha_share, na.rm = T)), by = .(variable, Donor)][, variable := as.character(as.numeric(as.character(variable)) + 1)][]
  
  dac2a_missing_un <- merge(dac1_un_growth, dac2a_un_imputed, by = c("Donor", "variable"), all = T)
  dac2a_missing_un <- merge(dac2a_missing_un, mums_un_imputed, by = c("Donor", "variable"), all = T)
  dac2a_mums_missing_un <- dac2a_missing_un[, .(dac2a_mums_missing_un = growth*(dac2a_imputed_un_ha + mums_imputed_un_ha)), by = .(Donor, variable)]
  
  #IDA HA
  ##DAC1 Bilateral ODA to World Bank and IDA
  #All donors
  #All parts (Part I only available)
  #Disbursements to World Bank and IDA (547, 2103)
  #Net disbursement flows (1140)
  #Constant prices (D)
  #2000-2020
  dac1_ida <- tabulate_dac_api("TABLE1", list( "", "", c(547, 2103), 1140, "D"), 2000, dac1_max_year)
  dac1_ida <- melt(dac1_ida, id.vars = c("Donor", "Part", "Aid type", "Fund flows", "Amount type"))
  
  dac1_ida <- dac1_ida[variable %in% missing_years][, ida_share := value[`Aid type` == "I.B.1.3. IDA" & variable == as.character(dac_base_year)]/value[`Aid type` == "      Memo: World Bank, Total (I.B.1.3. + I.B.1.4.)" & variable == as.character(dac_base_year)], by = .(Donor)][]
  dac1_ida <- dac1_ida[variable %in% missing_years & variable != as.character(dac_base_year) & `Aid type` == "      Memo: World Bank, Total (I.B.1.3. + I.B.1.4.)", .(dac1_imputed_ida_oda = value*ida_share), by = .(Donor, variable)]
  
  dac2a_ida_ha_share <- dac2a_moha_share[Donor == "International Development Association [IDA]"]
  missing_ida_share <- cbind(variable = as.character(missing_years[missing_years != dac_base_year]), ha_share = as.numeric(missing_years[missing_years != dac_base_year]*glm(dac2a_ida_ha_share$ha_share ~ as.numeric(as.character(dac2a_ida_ha_share$variable)))$coefficients[2]+glm(dac2a_ida_ha_share$ha_share ~ as.numeric(as.character(dac2a_ida_ha_share$variable)))$coefficients[1]))
  
  dac2a_missing_ida <- merge(dac1_ida, missing_ida_share, by = "variable")
  dac2a_missing_ida <- dac2a_missing_ida[, .(dac2a_missing_ida = dac1_imputed_ida_oda*as.numeric(ha_share)), by = .(Donor, variable)]
  
  ##CERF
  cerf_list <- list()
  for(i in 1:length(missing_years[missing_years != dac2a_max_year])){
    url <- paste0("https://cerf.un.org/contributionsByDonor/", missing_years[missing_years != dac_base_year][i])
    cerf_list[[i]] <- cbind(read_json(url, simplifyVector = T)$data, variable = missing_years[missing_years != dac_base_year][i])
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
  
  dac2a_missing_cerf <- missing_cerf[, .(dac2a_missing_cerf = sum(cerf_ha/gdp_defl, na.rm = T)), by = .(Donor, variable)][!is.na(dac2a_missing_cerf)]
  
  #Total recent missing year(s)
  dac2a_total_missing <- merge(dac2a_mums_missing_un, dac2a_missing_ida, all.x = T, by = c("Donor", "variable"))
  dac2a_total_missing <- merge(dac2a_total_missing, dac2a_missing_cerf, all.x = T, by = c("Donor", "variable"))
  dac2a_total_missing[is.na(dac2a_total_missing)] <- 0
  
  dac2a_total_missing <- dac2a_total_missing[, .(dac2a_imputed_multi_ha = dac2a_mums_missing_un + dac2a_missing_ida + dac2a_missing_cerf), by = .(Donor, variable)]
  
  dac2a_imha <- rbind(dac2a_imha, dac2a_total_missing)
}

#Estimate MUMS for completely missing year(s)
if(as.numeric(mums_max_year) < as.numeric(dac2a_max_year)){
  message("Using previous years' MUMS data as latest year isn't available.")
  mums_missing_guess <- mums_imha[as.character(variable) == mums_max_year][rep(1:nrow(mums_imha[as.character(variable) == mums_max_year]), (dac2a_max_year - as.numeric(mums_max_year)))]
  mums_missing_guess[, variable := as.character(rep((as.numeric(mums_max_year) + 1 ):(as.numeric(dac2a_max_year)), each = nrow(mums_missing_guess)/(dac2a_max_year + 1 - as.numeric(mums_max_year))))]
  
  mums_imha <- rbind(mums_imha[as.character(variable) <= mums_max_year], mums_missing_guess)
  
  if(mums_max_year != dac2a_max_year){
    mums_imha <- merge(mums_imha, defl[year == mums_max_year, .(countryname_oecd, year, gdp_defl)], by.x = c("Donor"), by.y = c("countryname_oecd"), all.x = T)
    mums_imha[is.na(gdp_defl)]$gdp_defl <- merge(mums_imha[is.na(gdp_defl), .(Donor, variable)], defl[ISO == "DAC"], by.x = "variable", by.y = "year")$gdp_defl
    
    mums_imha[, `:=` (mums_imputed_multi_ha = mums_imputed_multi_ha/gdp_defl, gdp_defl = NULL)]
  }
  
}

####
##Total Imputed HA
total_imha <- merge(dac2a_imha, mums_imha, by = c("Donor", "variable"), all = T)
#total_imha <- merge(total_ihma, cerf_ihma, all = T)
total_imha[is.na(total_imha)] <- 0
total_imha[, total_imha := (dac2a_imputed_multi_ha + mums_imputed_multi_ha)]
#total_imha[, total_imha := (total_ihma + cerf_imputed_multi_ha)]

##EU imputations
dac1_eu[, eu_value_share := value/(as.numeric(value[Donor == "DAC Countries, Total"]) + (value[Donor == "Non-DAC Countries, Total"])), by = variable]

total_eu <- merge(dac2a_ha[Donor == "EU Institutions"], total_imha[Donor == "EU Institutions", .(variable, total_imha)])
total_eu <- total_eu[, .(total_eu = value + total_imha), by = "variable"]

dac1_eu <- merge(dac1_eu, total_eu, by = "variable", all.x = T)
eu_imha <- dac1_eu[, .(eu_imha = eu_value_share*total_eu), by = .(variable, Donor)]

###
##Total IHA
total_iha <- merge(dac2a_ha, total_imha, by = c("variable", "Donor"), all = T)
total_iha <- merge(total_iha, eu_imha, by = c("variable", "Donor"), all = T)
total_iha[is.na(total_iha)] <- 0
total_iha_sep <- total_iha[, .(total_bilat = value, total_imha, eu_imha), by = .(variable, Donor)]
total_iha <- total_iha[, .(total_iha = value + total_imha + eu_imha), by = .(variable, Donor)]
total_iha <- total_iha[!grepl(", Total", Donor)]
fwrite(total_iha, "IHA/output/dac_aggregate_donors.csv")
fwrite(total_iha_sep, "IHA/output/dac_aggregate_donors_bimulti.csv")

##Debug inputs
de_dac2a_ha <- dac2a_ha
de_dac2a_missing_un <- dac2a_mums_missing_un
de_dac2a_missing_ida <- dac2a_missing_ida
de_dac2a_missing_cerf <- dac2a_missing_cerf
de_eu_imha <- eu_imha
debug <- merge(merge(merge(merge(de_dac2a_ha[, .(variable, Donor, dac2a = value)], de_dac2a_missing_un, by = c("variable", "Donor")), de_dac2a_missing_ida), de_dac2a_missing_cerf), de_eu_imha)
