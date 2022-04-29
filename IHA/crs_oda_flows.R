suppressPackageStartupMessages(lapply(c("data.table", "jsonlite","rstudioapi"), require, character.only=T))

#Load FTS utility functions
setwd(dirname(getActiveDocumentContext()$path))
setwd("..")

source("https://raw.githubusercontent.com/devinit/di_script_repo/main/crs/crs_complete_download.R")

years <- 2000:2020
update_years <- 2000:2020
dataset_path <- "IHA/datasets"

if(!dir.exists(dataset_path)){
  dir.create(dataset_path)
}
crs_files <- list.files(path = dataset_path, pattern = "crs_")
crs_list <- list()
for(i in 1:length(years)){
  run <- T
  if(!(paste0("crs_", years[i], ".gz") %in% crs_files) | years[i] %in% update_years){
    while(run){
      tryCatch({
        crs <- crs_get(years[i], years[i])
        run <- F
      },
      error = function(e) e
      )
      break
    }
    fwrite(crs, paste0(dataset_path, "/crs_", years[i], ".gz"))
  }
}
