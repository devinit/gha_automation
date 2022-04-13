suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi", "imfr"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

years <- as.character(2000:2021)

invisible(lapply(c("https://raw.githubusercontent.com/devinit/di_script_repo/main/general/tabulate_dac_api.R"), source))

isos <- fread("https://raw.githubusercontent.com/devinit/gha_automation/main/reference_datasets/isos.csv", encoding = "UTF-8", na.strings = "")

all_ex <- data.table(expand.grid(iso3 = c(isos$iso3, "EUI"), year = years))

##OECD
oecd_ex <- tabulate_dac_api("SNA_TABLE4", list("", "EXC", "CD"), 2000, 2021)
oecd_ex <- merge(isos[, .(countryname_oecd, iso3)], oecd_ex, by.x = "countryname_oecd", by.y = "Country", all.y = T)

{oecd_ex[countryname_oecd == "Euro area (19 countries)", iso3 := "EUI"]
oecd_ex[countryname_oecd == "Hong Kong, China", iso3 := "HKG"]
oecd_ex[countryname_oecd == "Russia", iso3 := "RUS"]
oecd_ex[countryname_oecd == "Slovak Republic", iso3 := "SVK"]}

oecd_ex <- melt(cbind(oecd_ex[, .(iso3 = iso3)], oecd_ex[, ..years]), id.vars = "iso3")

oecd_ex <- oecd_ex[!is.na(value) & value != 0]

##WORLD BANK
wb_ex <- data.table(fromJSON("https://api.worldbank.org/v2/country/all/indicator/PA.NUS.ATLS?date=2000:2021&format=json&per_page=10000")[[2]])
wb_ex <- wb_ex[, .(iso3 = countryiso3code, variable = date, value = value)]

wb_ex <- wb_ex[!is.na(value) & (!(paste0(iso3, variable) %in% oecd_ex[, paste0(iso3, variable)]))]

##IFS
ifs_ex <- data.table(imf_data("IFS", "ENDA_XDC_USD_RATE", "all", 2000, 2021, "A"))
ifs_ex <- merge(isos[, .(iso3, iso2)], ifs_ex[!is.na(`ENDA_XDC_USD_RATE`)], by.x= "iso2", by.y = "iso2c", all = T)[, .(iso3, variable = year, value = `ENDA_XDC_USD_RATE`)]

ifs_ex <- ifs_ex[!is.na(value) & !(paste0(iso3, variable) %in% c(oecd_ex[, paste0(iso3, variable)], wb_ex[, paste0(iso3, variable)]))]

##All
all_wd_ex <- rbind(oecd_ex, wb_ex, ifs_ex)[, .(iso3, year = variable, value = value)]

all_ex <- merge(all_ex, all_wd_ex, all.x = T)

fwrite(all_ex, "reference_datasets/usd_exchange_rates.csv")
