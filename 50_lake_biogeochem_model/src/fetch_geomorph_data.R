#' lake specific input data fetch 
#' @name fetch_geomorph_data
#' 
#' @param lakeID National Hydrography Dataset permanent ID for waterbody
#' 
#' @author Jacob Zwart 
#' 
#' @description 
#' Grabs lake specific initial geomorphology data  
#' 
#' @import LakeMetabolizer 

fetch_geomorph_data <- function(lake_id,in_dir='../40_constituent_load_model/out'){
  
  stopifnot(!is.null(lake_id))
  
  lake_id = as.character(lake_id) 
  
  data = readRDS(file.path(in_dir, paste(lake_id, '.rds', sep=''))) # read in geomorph data 
  
  return(data)
}
