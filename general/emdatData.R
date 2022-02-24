#Description: Downloads the most recent EMDAT release (all disasters types)
#Author: Dan W
#Creation: Oct 2021
#Last revision: NA
#Notes: Sourced from public.emdat.be. Uses Dan W's auth to access the database. Outputted table columns are all type 'character'.

emdat_get <- function(){
  required.packages <- c("data.table", "jsonlite", "httr", "readxl")
  install.packages(required.packages[!(required.packages %in% installed.packages())])
  lapply(required.packages, require, character.only=T)
  tmp <- tempfile()
  t <- GET(fromJSON(rawToChar(POST(url = "https://public.emdat.be/api/graphql", body = "{\"operationName\":\"emdat_public\",\"variables\":{},\"query\":\"mutation emdat_public() {\\n  emdat_public() {link}\\n}\\n\"}")$content))$data$emdat_public$link,
                    config = add_headers(auth = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo3NjEwLCJ1c2VybmFtZSI6ImRhbi53YWx0b24ifQ.HHif5bKDEtDH_IhqtS-87KNQUxg6vYF5iau3edMCaKA"), write_disk(tmp))
  em_dat <- data.table(read_excel(tmp, skip = 6, col_types = "text"))
  return(em_dat)
}
