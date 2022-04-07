hiik_get <- function(){
  lapply(c("data.table", "jsonlite", "xml2", "httr"), require, character.only = T)
  
  hiik_datasets <- read_html("https://hiik.de/data-and-maps/datasets/?lang=en")
  hiik_download <- xml_attr(xml_find_first(hiik_datasets, "//*[contains(@data-downloadurl, 'https://hiik.de/download/')]"), 'data-downloadurl')
  
  hiik_data <- GET(hiik_download)$content
  tmp <- tempfile()
  writeBin(hiik_data, tmp)
  
  hiik <- data.table(read.csv(unz(tmp, data.table(unzip(tmp, list = T))[grepl("[.]csv", Name) & !grepl("_long", Name)]$Name), sep = ";"))
  return(hiik)
}