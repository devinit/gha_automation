hpc_api_all <- function(plan_id, data_type = "caseLoad", by_sector = T, disaggregations = T){
  
  required.packages <- c("data.table", "rstudioapi", "jsonlite")
  lapply(required.packages, require, character.only=T)
  
  dis <- "false"
  if(disaggregations) dis <- "true"
  
  api <- paste0("https://api.hpc.tools/v2/public/plan/", plan_id, "?content=entities&disaggregation=", dis)
  api_out <- fromJSON(api)
  
  top <- api_out$data$attachments
  sectors <- api_out$data$governingEntities$attachments
  
  melt_hpc_api <- function(attachments){
    if(length(attachments) == 0) return(NULL)
    
    #Check which index data is stored in
    index <- (1:length(attachments$type))[attachments$type == data_type]
    if(identical(index, integer(0))) return(NULL)
    
    index_out <- list()
      for(j in 1:length(index)){
      
        #Check if data is disaggregated
        disag <- attachments$attachmentVersion$hasDisaggregatedData[[index[j]]]
        
        if(disag & disaggregations){
          #Metrics by categories (columns of dataMatrix)
          categories <- attachments$attachmentVersion$value$metrics$values$disaggregated$categories[[index[j]]]
          metrics <- categories$metrics
          
          if(length(categories) != 0){
            categories_metrics <- rbindlist(lapply(1:length(metrics), function(i) data.table(category_type = categories[i,"name"], category_name = categories[i,"label"], setnames(as.data.table(metrics[[i]]), c("metric_label", "metric_id")))))
            
            #Total metrics
            total_metrics <- data.table(category_type = "Total", category_name = "Total", setnames(unique(rbindlist(lapply(metrics, as.data.table))), c("metric_label", "metric_id")))
            categories_metrics <- rbind(categories_metrics, total_metrics)
          } else {
            
            categories_metrics <- data.table(category_type = "Total", category_name = "Total", setnames(as.data.table(attachments$attachmentVersion$value$metrics$values$totals[[index[j]]])[, c(1,2)], c("metric_label", "metric_id")))
          }
            
          #Locations
          locations <- as.data.table(attachments$attachmentVersion$value$metrics$values$disaggregated$locations[[index[j]]])
          names(locations) <- paste0("location_", names(locations))
          
          #dataMatrix
          dataMatrix <- tail(data.table(attachments$attachmentVersion$value$metrics$values$disaggregated$dataMatrix[[index[j]]]), -1)
          names(dataMatrix) <- apply(categories_metrics, 1, paste0, collapse = ";")
          dataMatrix <- cbind(locations, dataMatrix)
          
          #dataMelt
          data_melt <- melt(dataMatrix, id.vars = names(locations))
          data_melt[, names(categories_metrics) := tstrsplit(variable, ";"), by = names(locations)]
          data_melt[, variable := NULL]
          
        } else {
          #Metrics
          data_melt <- data.table(attachments$attachmentVersion$value$metrics$values$totals[[index[j]]])
          names(data_melt)[names(data_melt) != "value"] <- paste0("metric_", names(data_melt)[names(data_melt) != "value"])
          data_melt[, `:=` (category_id = "Total", category_label = "Total")]
        }
        
        names(data_melt) <- gsub("_type", "_id", names(data_melt))
        names(data_melt) <- gsub("_label|_en", "_name", names(data_melt))
        suppressWarnings(data_melt[, value := as.numeric(trimws(gsub(",", "", value)))])
        
        description <- attachments$attachmentVersion$value$description[[index[j]]]
        source <- attachments$attachmentVersion$value$source[[index[j]]]
        
        index_out[[index[j]]] <- cbind(data_melt, description, source)
      }
    
    index_all <- rbindlist(index_out, fill = T)
    return(index_all)
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
  
  all_out <- NULL
  if(!is.null(top_out) | !is.null(sectors_out)) all_out <- rbind(top_out, sectors_out, fill = T)
  
  return(all_out)
}
