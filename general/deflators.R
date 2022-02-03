get_deflators <- function(base_year = 2020, currency = "USD", weo_ver = "Oct2021"){
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  ##WEO data
  weo_year <- year(as.Date(paste0("1", weo_ver), "%d%b%Y"))
  url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year, "/WEO", weo_ver ,"all.ashx")
  weo <- fread(url, na.strings=c("n/a", "--"), showProgress = F)
  
  country_codes <- unique(weo[, .(ISO, Country)])
  
  data_cols <- c("ISO", "WEO Subject Code", grep("^\\d{4}$", names(weo), value = T))
  
  weo <- melt(weo[, ..data_cols], id.vars = c("ISO", "WEO Subject Code"), variable.factor = F)
  weo[, value := as.numeric(gsub(",", "", value))]
  
  #Fix PSE ISO code
  weo[ISO == "WBG", ISO := "PSE"]
  
  #GDP in current prices
  if(currency == "USD"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "NGDPD"]
  }
  if(currency == "LCU"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "NGDP"]
  }
  if(currency == "PPP"){
    weo_gdp_cur <- weo[`WEO Subject Code` == "PPPGDP"]
  }
  
  weo_gdp_cur <- weo_gdp_cur[, .(ISO, variable, gdp_cur = value)]
  
  #GDP real growth rates
  weo_gdp_pcg <- weo[`WEO Subject Code` == "NGDP_RPCH"]
  
  #GDP cumulative growth rates
  weo_gdp_pcg <- weo_gdp_pcg[, gdp_cg := 1+ifelse(is.na(value), 0, value/100), by = ISO]
  weo_gdp_pcg[, gdp_cg := ifelse(!(!is.na(value) | !is.na(shift(value, -1))), NA, cumprod(gdp_cg)), by = ISO]
  weo_gdp_pcg[, gdp_cg := gdp_cg/gdp_cg[variable == base_year], by = ISO][, value := NULL]
  
  #GDP in constant prices
  weo_gdp_con <- merge(weo_gdp_pcg[, .(ISO, variable, gdp_cg)], weo_gdp_cur)
  weo_gdp_con[, `:=` (gdp_con = gdp_cg*gdp_cur[variable == base_year]), by= ISO]
  
  #GDP deflators from WEO
  weo_deflators <- weo_gdp_con[, .(gdp_defl = gdp_cur/gdp_con), by = .(ISO, variable)]
  weo_deflators <- cbind(weo_deflators, source = "WEO", ver = weo_ver)
  
  ##Other data sources
  
  #OECD data
  if(currency == "USD"){
    
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
    
    api_dacdefl <- paste0("https://stats.oecd.org/SDMX-JSON/data/DACDEFL/./all?startTime=", min(weo_deflators$variable), "&endTime=", max(weo_deflators$variable))
    dacdefl <- tabulate_dac_api(api_dacdefl)
    
    dacdefl <- dacdefl[dacdefl[, .I[which.max(`Deflator base year`)], by = Donor]$V1]
    dacdefl <- melt(dacdefl[, -"Deflator base year"], id.vars = "Donor")
    
    dacdefl[, gdp_defl := value/value[variable == base_year], by = Donor]
    
    dacdefl <- merge(dacdefl, country_codes, by.x = "Donor", by.y = "Country", all.x = T)
    
    dacdefl[Donor == "Total DAC", ISO := "DAC"]
    dacdefl[Donor == "EU Institutions", ISO := "EUI"]
    
    if(nrow(dacdefl[is.na(ISO)]) > 0) warning("Country name mismatch between WEO and OECD.")
    
    #Calculate Total DAC for missing data years
    weo_gdp_con_dac <- weo_gdp_con[ISO %in% dacdefl$ISO & !(variable %in% dacdefl[ISO == "DAC" & !is.na(gdp_defl)]$variable)]
    weo_totaldac_defl <- weo_gdp_con_dac[, .(ISO = "DAC", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]
    
    weo_deflators <- rbind(weo_deflators, weo_totaldac_defl)
    
    #Replace WEO DAC data with OECD data
    dacdefl <- dacdefl[!is.na(gdp_defl), .(ISO, variable, gdp_defl, source = "OECD", ver = format(Sys.Date(), "%b%Y"))]
    deflators <- rbind(weo_deflators[!(ISO %in% dacdefl$ISO & variable %in% dacdefl$variable)], dacdefl)
    
    if(!(base_year %in% dacdefl$variable)) warning("Cannot return OECD deflators for this base year; using all WEO data.")
  } else {
    
    deflators <- weo_deflators
  }
  
  #GBR copies
  GBR_copies <- c("AIA", "MSR", "SHN")
  deflators <- rbind(deflators[!(ISO %in% GBR_copies)], rbindlist(lapply(GBR_copies, function(x) copy(deflators)[ISO == "GBR"][, ISO := x])))
  
  #NZL copies
  NZL_copies <- c("COK", "NIU", "TKL")
  deflators <- rbind(deflators[!(ISO %in% NZL_copies)], rbindlist(lapply(NZL_copies, function(x) copy(deflators)[ISO == "NZL"][, ISO := x])))
  
  #FRA copies
  FRA_copies <- c("WLF")
  deflators <- rbind(deflators[!(ISO %in% FRA_copies)], rbindlist(lapply(FRA_copies, function(x) copy(deflators)[ISO == "FRA"][, ISO := x])))
  
  #DAC copies
  if("DAC" %in% deflators$ISO){
    DAC_copies <- c("CUB", "PRK", "SYR")
    deflators <- rbind(deflators[!(ISO %in% DAC_copies)], rbindlist(lapply(DAC_copies, function(x) copy(deflators)[ISO == "DAC"][, ISO := x])))
  }
  
  #Final out
  deflators <- deflators[, .(ISO, year = variable, base_year, currency, source, ver, gdp_defl)][order(ISO, year)]
  return(deflators)
}
