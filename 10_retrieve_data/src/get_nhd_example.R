#' FedData example 
#' JAZ; 2017-12-20
#' http://zevross.com/blog/2016/03/15/using-the-new-r-package-feddata-to-access-federal-open-datasets-including-interactive-graphics/

# FedData Tester
library(FedData)
library(magrittr)
library(ggmap)
library(geoknife)

# madcity <- get_map('Madison, WI', zoom = 11)
# windows()
# ggmap(madcity)
# 
# 
# # Extract data for the Village Ecodynamics Project "VEPIIN" study area:
# # http://veparchaeology.org
# vepPolygon <- polygon_from_extent(raster::extent(672800, 740000, 4102000, 4170000),
#                                   proj4string = "+proj=utm +datum=NAD83 +zone=12")
# 
# (stencil)
# 
# # Get the NHD (USA ONLY)
# NHD <- get_nhd(template = vepPolygon, 
#                label = "VEPIIN")
# 
# NHD <- get_nhd(template = stencil, 
#                label = "VEPIIN")
# 
# 
# # Plot the NED again
# raster::plot(NED)
# # Plot the NHD data
# NHD$Waterbody %>%
#   lapply(sp::plot,
#          col = 'black')
# 
# 
# sp::plot(NHD$Waterbody)

## downloading subregions of HUC4's 
cur_huc8 <- '07050002' # 
cur_huc4 <- substr(cur_huc8,1,4)

raw_dir <- '/Users/jzwart/lakeScale/10_retrieve_data/out/nhd/'

# downloading NHD 
nhd <- FedData::get_nhd_subregion(area = cur_huc4,raw.dir = raw_dir)

wb_sp <- nhd$NHDWaterbody

# subsetting to HUC8 
wb_sp <- subset(wb_sp,substr(as.character(ReachCode),1,8)==cur_huc8)


# extract the bounding box coords of NHD subset 
bb <- FedData::polygon_from_extent(raster::extent(wb_sp),
                            proj4string = '+proj=longlat +datum=NAD83 +zone=12')

raw_dir <- '/Users/jzwart/lakeScale/10_retrieve_data/out/dem/'

# downloading DEM from NED
dem <- FedData::get_ned(template = bb, label = cur_huc8, res = '1', raw.dir = raw_dir, 
                        extraction.dir = raw_dir)

raw_dir <- '/Users/jzwart/lakeScale/10_retrieve_data/out/nlcd/'
# downloading NLCD data 
nlcd <- FedData::get_nlcd(template = bb, label = cur_huc8, year = 2011, dataset = 'landcover',
                          raw.dir = raw_dir, extraction.dir = raw_dir)


raw_dir <- '/Users/jzwart/lakeScale/10_retrieve_data/out/ssurgo/'
# downloading SSURGO data (soil data)
ssurgo <- FedData::get_ssurgo(template = bb, label = cur_huc8,
                          raw.dir = raw_dir, extraction.dir = raw_dir)



windows()
sp::plot(dem)

sp::plot(wb_sp, add = T)
sp::plot(bb, add = T)
raster::plot(nlcd)




get_nhd_huc8 <- function(huc8 = NULL){
  ## downloading subregions 
  cur_huc8 <- '07050002' # 
  cur_huc4 <- substr(cur_huc8,1,4)
  
  raw_dir <- '/Users/jzwart/lakeScale/10_retrieve_data/out/nhd/'
  
  nhd <- FedData::get_nhd_subregion(area = cur_huc4,raw.dir = raw_dir)
  
  wb_sp <- nhd$NHDWaterbody
  
  # subsetting to HUC8 
  wb_sp <- subset(wb_sp,substr(as.character(ReachCode),1,8)==cur_huc8)
  
  windows()
  sp::plot(wb_sp)
  
  # extract the bounding box coords of NHD subset 
  bb <- FedData::polygon_from_extent(raster::extent(wb_sp),
                                     proj4string = '+proj=utm +datum=WGS84')
  
  dem <- FedData::get_ned(template = bb, label = paste(cur_huc8,'dem',sep='_'))
  
  
}



# 
# # define a stencil for slicing data; using HUC12 
# stencil <- geoknife::webgeom(paste('HUC8::',cur_huc8,sep=''))
# 
# webgeom('HUC8::07050002')



