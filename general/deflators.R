get_deflators <- function(base_year = 2020, currency = "USD", weo_ver = NULL, approximate_missing = T){
  suppressPackageStartupMessages(lapply(c("data.table"), require, character.only=T))
  
  if(is.null(weo_ver)){
    
    tyear <- year(Sys.Date())
    tmonth <- month(Sys.Date())
    
    weo_month <- ifelse(tmonth <= 10 & tmonth >= 4, 4, 10)
    weo_year <- ifelse(tmonth < 4, tyear-1, tyear)
    
    weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
  }
  
  ##WEO data
  pweo_ver <- as.Date(paste0("1", weo_ver), "%d%b%Y")
  weo_year <- year(pweo_ver)
  weo_month <- month(pweo_ver)
  
  while(T){
    url <- paste0("https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/", weo_year, "/WEO", weo_ver ,"all.ashx")
    response <- GET(url)
    if(response$headers$`content-type` == "application/vnd.ms-excel") break
    
    if(weo_month <= 10 & weo_month > 4){
      weo_month <- 4
    } else {
      if(weo_month <= 4){
        weo_year <- weo_year - 1
      }
      weo_month <- 10
    }
    weo_ver <- format(as.Date(paste("1", weo_month, weo_year, sep = "-"), "%d-%m-%Y"), "%b%Y")
  }
  
  message("Using IMF WEO version ", weo_ver, ".")
  
  content <- response$content
  weo <- suppressWarnings(fread(rawToChar(content[content !='00']), na.strings=c("n/a", "--")))
  
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
  dacdefl <- melt(dacdefl[, -"Deflator base year"], id.vars = "Donor", variable.factor = F)
  
  dacdefl[, gdp_defl := value/value[variable == base_year], by = Donor]
  
  dacdefl <- merge(dacdefl, country_codes, by.x = "Donor", by.y = "Country", all.x = T)
  
  dacdefl[Donor == "Total DAC", ISO := "DAC"]
  dacdefl[Donor == "EU Institutions", ISO := "EUI"]
  
  if(nrow(dacdefl[is.na(ISO)]) > 0) warning("Country name mismatch between WEO and OECD.")
  
  if(currency != "USD"){
    dacdefl <- dacdefl[ISO %in% c("DAC", "USA")]
  }
  
  #Calculate Total DAC for missing data years
  weo_gdp_con_dac <- weo_gdp_con[ISO %in% dacdefl$ISO & !(variable %in% dacdefl[ISO == "DAC" & !is.na(gdp_defl)]$variable)]
  weo_totaldac_defl <- weo_gdp_con_dac[, .(ISO = "DAC", gdp_defl = sum(gdp_cur, na.rm = T)/sum(gdp_con, na.rm = T), source = "WEO", ver = weo_ver), by = .(variable)]
  
  weo_deflators <- rbind(weo_deflators, weo_totaldac_defl)
  
  #Replace WEO DAC data with OECD data
  dacdefl <- dacdefl[!is.na(gdp_defl), .(ISO, variable, gdp_defl, source = "OECD", ver = format(Sys.Date(), "%b%Y"))]
  deflators <- rbind(weo_deflators[!(paste0(ISO, variable) %in% paste0(dacdefl$ISO, dacdefl$variable))], dacdefl)
  
  if(!(base_year %in% dacdefl$variable)) warning("Cannot return OECD deflators for this base year; using all WEO data.")
  
  #
  
  deflators[, variable := as.numeric(variable)]
  
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
  
  ##Approximate missing
  if(approximate_missing){
    missing <- deflators[, .SD[any(is.na(gdp_defl))], by = ISO]
    missing_weo_gdp <- weo_gdp_con[ISO %in% missing$ISO]
    missing_weo_gdp[, variable := as.numeric(variable)]
    missing_weo_gr <- suppressWarnings(missing_weo_gdp[, .(gdp_avg_curg = (gdp_cur[!is.na(gdp_cur) & variable == max(variable[!is.na(gdp_cur)])]/gdp_cur[!is.na(gdp_cur) & variable == min(variable[!is.na(gdp_cur)])])^(1/(max(variable[!is.na(gdp_cur)])-min(variable[!is.na(gdp_cur)]))),
                                                           gdp_avg_cong = (gdp_con[!is.na(gdp_con) & variable == max(variable[!is.na(gdp_con)])]/gdp_con[!is.na(gdp_con) & variable == min(variable[!is.na(gdp_con)])])^(1/(max(variable[!is.na(gdp_con)])-min(variable[!is.na(gdp_con)]))))
                                                       , by = ISO])
    missing_weo_gr <- missing_weo_gr[, .(defg = gdp_avg_curg/gdp_avg_cong), by = ISO]
    
    missing_defl <- merge(deflators[ISO %in% missing$ISO], missing_weo_gr, by = "ISO")
    
    missing_defl_f <- suppressWarnings(missing_defl[, .SD[is.na(gdp_defl) & variable > max(variable[!is.na(gdp_defl)])], by = ISO])
    missing_defl_b <- suppressWarnings(missing_defl[, .SD[is.na(gdp_defl) & variable < min(variable[!is.na(gdp_defl)])], by = ISO])
    
    missing_defl_b[, defg := rev(cumprod(1/defg)), by = ISO]
    missing_defl_f[, defg := cumprod(defg), by = ISO]
    
    missing_defl_b <- merge(missing_defl_b[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_b$ISO, .SD[variable == min(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO")
    missing_defl_f <- merge(missing_defl_f[, -"gdp_defl"], missing_defl[ISO %in% missing_defl_f$ISO, .SD[variable == max(variable[!is.na(gdp_defl)])], by = ISO][, .(ISO, gdp_defl)], by = "ISO")
    
    missing_defl <- rbind(missing_defl_b[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)], missing_defl_f[, `:=` (gdp_defl = gdp_defl*defg, defg = NULL)])
    
    missing_defl[, `:=` (source = paste0(source, "_est"))]
    
    deflators <- rbind(deflators[!(paste0(ISO, variable) %in% paste0(missing_defl$ISO, missing_defl$variable))], missing_defl)
  }
  
  #Final out
  deflators <- deflators[, .(ISO, year = variable, base_year, currency, source, ver, gdp_defl)][order(ISO, year)]
  return(deflators)
}
