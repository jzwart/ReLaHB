# JAZ, 2017-09-15 
# geoknife examples 

library(geoknife)

# define a stencil for slicing data; using HUC12 
stencil <- webgeom('HUC8::07060004')

dataSets <- query('webdata')
dataSets

# from the prism dataset:
fabric <- webdata('prism')
# -- or --
# explicitly define webdata from a list:
fabric <- webdata(list(
  times = as.POSIXct(c('2000-01-01','2010-01-01')),
  url = 'https://cida.usgs.gov/thredds/dodsC/prism_v2',
  variables = 'ppt'))

fabric <- webdata('daymet')

job <- geoknife(stencil, fabric, wait = TRUE)

# use existing convienence functions to check on the job:
check(job)

data <- result(job)





