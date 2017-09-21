#' lake specific input data fetch 
#' @name fetch_load_data
#' 
#' @param startTime start date of model run in format YYYY-MM-DD; default to 1900-01-01
#' @param endTime end date of model run in format YYYY-MM-DD; default to current date 
#' @param lakeID National Hydrography Dataset permanent ID for waterbody
#' 
#' @author Jacob Zwart 
#' 
#' @description 
#' Grabs lake specific data from lake water energy 
#' budget model and load consistuent model 
#' 
#' @import LakeMetabolizer 

fetch_load_data <- function(start_time='1900-01-01',end_time=Sys.Date(),
                            lake_id,in_dir='../40_constituent_load_model/out'){
  
  stopifnot(!is.null(lake_id))
  
  start_time = as.Date(as.character(start_time))
  end_time = as.Date(as.character(end_time))
  lake_id = as.character(lake_id) 
  
  data = readRDS(file.path(in_dir, paste(lake_id, '.rds', sep=''))) # read in constituent load load data 
  
  data = data[as.Date(data$datetime) >= start_time & as.Date(data$datetime) <= end_time,] 
  
  return(data)
}

