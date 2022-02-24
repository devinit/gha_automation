wup_get <- function(){
  lapply(c("data.table", "jsonlite", "httr"), require, character.only = T)
  
  wup_urb_q <- '{"IndicatorListID":2,"IndicatorIDs":[3],"SourceIDs":["4"],"VariantIDs":[],"LocationIDs":[],"TimeIDs":[],"GenderIDs":[],"AgeIDs":[],"XMLFilters":[],"PivotBy":"Time"}'
  wup_rur_q <- '{"IndicatorListID":2,"IndicatorIDs":[2],"SourceIDs":["4"],"VariantIDs":[],"LocationIDs":[],"TimeIDs":[],"GenderIDs":[],"AgeIDs":[],"XMLFilters":[],"PivotBy":"Time"}'
  
  wup_data <- function(query_body){
    wup_url <- "https://population.un.org/PEPxplorer2017/api/queryweb/webdisplay"
    data_out <- data.table(fromJSON(rawToChar(POST(wup_url, body = query_body, config = content_type("application/json; charset=UTF-8"), encode = "json")$content))[[1]]$Data)
    data_out <- melt(data_out, measure.vars = grep("^Data_", names(data_out)))
    return(data_out[, `:=` (year = gsub("Data_", "", variable), variable = NULL)])
  }
  
  wup_urb <- wup_data(wup_urb_q)
  wup_rur <- wup_data(wup_rur_q)
  
  wup_pop <- rbind(wup_urb[!is.na(ISO3166_1_3), .(ISO3 = ISO3166_1_3, area = "urban", year, population = value*1000)], wup_rur[!is.na(ISO3166_1_3), .(ISO3 = ISO3166_1_3, area = "rural", year, population = value*1000)])
  
  wup_tot <- wup_pop[, .(area = "total", population = sum(population)), by = .(ISO3, year)]
  
  wup_pop <- rbind(wup_tot, wup_pop)
  return(wup_pop)
}
