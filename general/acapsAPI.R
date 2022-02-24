token <- "Token 92452d125b77c41a81ff2592d65db1466c85bd34"

acaps_get <- function(database, dataset=NULL, date=NULL, parameters=NULL, token){
  lapply(c("data.table", "jsonlite"), require, character.only = T)
  
  api <- "https://api.acaps.org/api/v1"
  query <- paste(api, database, sep = "/")
  if(!is.null(dataset)) query <- paste(query, dataset, sep = "/")
  if(!is.null(date)) query <- paste(query, date, sep = "/")
  if(!is.null(parameters)) query <- paste(query, parameters, sep = "/")
  result <- data.table()
  while(T){
    response <- fromJSON(content(GET(query, add_headers(Authorization = token)), "text", encoding = "UTF-8"))
    result_temp <- response$results
    result <- rbind(result, result_temp)
    if("next" %in% names(response) & !is.null(response[["next"]])){
      query <- response[["next"]]
      Sys.sleep(1)
    } else {
      break
    }
  }
  return(result)
}
