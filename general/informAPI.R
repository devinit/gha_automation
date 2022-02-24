inform_get <- function(){
  lapply(c("data.table", "jsonlite"), require, character.only = T)
  
  year <- year(Sys.Date())
  wf_url <- "https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/Workflows/GetByYear/"
  year_wf_id <- data.table(fromJSON(paste0(wf_url, year)))
  wf_id <- year_wf_id[which.max(as.Date(WorkflowDate))]$WorkflowId
  
  inform_out <- data.table(fromJSON(paste0("https://drmkc.jrc.ec.europa.eu/inform-index/API/InformAPI/countries/Scores/?WorkflowId=", wf_id)))
  inform_out <- inform_out[ValidityYear != 0]
  
  return(inform_out)
}