required.packages <- c("data.table", "rstudioapi", "jsonlite")
lapply(required.packages, require, character.only=T)
setwd(dirname(dirname(getActiveDocumentContext()$path)))

hpc_api_all <- function(api_out, data_type = "caseLoad", by_sector = T){
  top <- api_out$data$attachments
  sectors <- api_out$data$governingEntities$attachments
  
  melt_hpc_api <- function(attachments){
    if(length(attachments) == 0) return(NULL)
    
    #Check which index data is stored in
    index <- (1:length(attachments$type))[attachments$type == data_type]
    if(identical(index, integer(0)) | length(index) != 1) return(NULL)
    
    #Check if data is disaggregated
    disag <- attachments$attachmentVersion$hasDisaggregatedData[[index]]
    
    if(disag){
      #Metrics by categories (columns of dataMatrix)
      categories <- attachments$attachmentVersion$value$metrics$values$disaggregated$categories[[index]]
      metrics <- categories$metrics
      if(length(categories) == 0) return(NULL)
      categories_metrics <- rbindlist(lapply(1:length(metrics), function(i) data.table(category_type = categories[i,"name"], category_name = categories[i,"label"], setnames(as.data.table(metrics[[i]]), c("metric_label", "metric_id")))))
      
      #Total metrics
      total_metrics <- data.table(category_type = "Total", category_name = "Total", setnames(unique(rbindlist(lapply(metrics, as.data.table))), c("metric_label", "metric_id")))
      categories_metrics <- rbind(categories_metrics, total_metrics)
      
      #Locations
      locations <- as.data.table(attachments$attachmentVersion$value$metrics$values$disaggregated$locations[[index]])
      names(locations) <- paste0("location_", names(locations))
      
      #dataMatrix
      dataMatrix <- tail(data.table(attachments$attachmentVersion$value$metrics$values$disaggregated$dataMatrix[[index]]), -1)
      names(dataMatrix) <- apply(categories_metrics, 1, paste0, collapse = ";")
      dataMatrix <- cbind(locations, dataMatrix)
      
      #dataMelt
      data_melt <- melt(dataMatrix, id.vars = names(locations))
      data_melt[, names(categories_metrics) := tstrsplit(variable, ";"), by = names(locations)]
      data_melt[, variable := NULL]
      
    } else {
      #Metrics
      data_melt <- data.table(attachments$attachmentVersion$value$metrics$values$totals[[index]])
      names(data_melt)[names(data_melt) != "value"] <- paste0("metric_", names(data_melt)[names(data_melt) != "value"])
      data_melt[, `:=` (category_id = "Total", category_label = "Total")]
    }
    
    names(data_melt) <- gsub("_type", "_id", names(data_melt))
    names(data_melt) <- gsub("_label|_en", "_name", names(data_melt))
    suppressWarnings(data_melt[, value := as.numeric(value)])
    
    return(data_melt)
  }
  
  if(by_sector){
    sectors_out <- list()
    for(i in 1:length(sectors)){
      sector_name <- api_out$data$governingEntities$governingEntityVersion$name[[i]]
      sector_id <- api_out$data$governingEntities$governingEntityVersion$clusterNumber[[i]]
      sectors_temp <- melt_hpc_api(sectors[[i]])
      if(length(sectors_temp > 0)){
        sectors_out[[i]] <- cbind(sector_id = sector_id, sector = sector_name, sectors_temp)
      }
    }
    sectors_out <- rbindlist(sectors_out, fill = T)
  } else {
    sectors_out <- NULL
  }
    
  top_out <- melt_hpc_api(top)
  if(!is.null(top_out)) top_out <- cbind(sector_id = 0, sector = "Total", top_out)
  
  all_out <- rbind(top_out, sectors_out, fill = T)
  
  return(all_out)
}

plans <- data.table(fromJSON("https://api.hpc.tools/v2/public/plan")$data)
plans[, year := lapply(years, function(x) x$year), by = id]

plans <- plans[year %in% c(2020, 2021)]

plan_caseloads <- list()
pb <- txtProgressBar(0, nrow(plans), style = 3)
for(i in 1:nrow(plans)){
  plan_id <- plans$id[[i]]
  plan_name <- plans$planVersion.name[[i]]
  location <- paste0(plans$locations[[i]][["name"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  iso <- paste0(plans$locations[[i]][["iso3"]][plans$locations[[i]]$adminLevel == 0], collapse = "; ")
  year <- paste0(plans$years[[i]][["year"]], collapse = "; ")
  
  metadata <- cbind(plan_id, plan_name, location, iso, year)
  
  api <- paste0("https://api.hpc.tools/v2/public/plan/", plan_id, "?content=entities&disaggregation=true")
  api_out <- fromJSON(api)
  
  caseload_temp <- hpc_api_all(api_out, data_type = "caseLoad", by_sector = T)
  
  if(nrow(caseload_temp) != 0) plan_caseloads[[i]] <- cbind(metadata, caseload_temp)
  setTxtProgressBar(pb, i)
}
plan_caseloads <- rbindlist(plan_caseloads, fill = T)

sex_f <- c("women", "girl", "female", "fille", "femme", "féminin", "feminin", "niña", "nina", "mujere")
sex_m <- c("\\bmen\\b", "boy", "\\bmale\\b", "garçon", "garcon", "homme", "masculin", "niño", "\\bnino\\b", "hombre")

plan_caseloads[grepl(paste0(sex_f, collapse = "|"), category_id, ignore.case = T), sex := "F"]
plan_caseloads[grepl(paste0(sex_m, collapse = "|"), category_id, ignore.case = T), sex := "M"]

by_sex <- plan_caseloads[metric_id %in% c("inNeed", "target") & !is.na(sex) & sector == "Total" & !is.na(value), sum(value), by = .(year, iso, plan_name, metric_id, sex)]
