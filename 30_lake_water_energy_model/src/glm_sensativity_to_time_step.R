# JAZ; testing GLM initialization sensativity to starting and stopping at different time intervals
# 2017-09-27


library(glmtools)

tmp = file.path('./30_lake_water_energy_model/temp', 'glm_orig') # file path to temp directory 

dir.create(tmp) # create tmp dir 

run_example_sim(sim_folder = tmp, verbose = F) # creates example simulation files in 


nml_file = file.path(tmp, 'glm2.nml')
nml <- read_nml(nml_file)
nml # nml file => config for glm; lake specific 

get_nml_value(nml, 'Kw') # returns specifiec parameter 

run_glm(tmp, verbose = T) # runs model based on nml and data; saves output to folder path 

nc_file <- file.path(tmp, 'output.nc')

# plot_temp(nc_file)
# get_temp(nc_file)

nDays <- as.numeric(as.Date(get_nml_value(nml, 'stop')) - as.Date(get_nml_value(nml, 'start'))) # total length of sim 

# time step by days 
time_step <- seq(2,nDays,by = 6)

for(step in time_step){
  # create start and stop dates based on time step for glm runs 
  starts <- seq(as.Date(get_nml_value(nml, 'start')), as.Date(get_nml_value(nml, 'start'))+nDays-step, by = step)
  stops <- seq(as.Date(starts[2]), as.Date(get_nml_value(nml, 'start'))+nDays, by = step)
  stops[length(stops)] <- as.Date(get_nml_value(nml, 'start'))+nDays
  
  starts <- as.character(starts) #nml needs character class 
  stops <- as.character(stops)
  
  # create tmp dir 
  tmp2 <- file.path('./30_lake_water_energy_model/temp/', 'glm_temp')
  dir.create(tmp2, showWarnings = F)
  run_example_sim(sim_folder = tmp2, verbose = F)
  
  # create output dir 
  out_path <- file.path('./30_lake_water_energy_model/temp/', paste('output', step, 'days', sep = '_'))
  dir.create(out_path, showWarnings = F)
  
  # read original nml 
  nml_orig_file <- file.path(tmp, 'glm2.nml') # original cfg file path
  nml_orig <- read_nml(nml_orig_file)
  
  # make elevation higher and huge area 
  nml_tmp_file <- file.path(tmp2, 'glm2.nml')
  nml_tmp <- read_nml(nml_tmp_file) # cur nml file in temp run folder 
  
  # change nml H and A 
  H <- nml_tmp$morphometry$H
  A <- nml_tmp$morphometry$A
  H <- c(H,tail(H,1)+20)
  A <- c(A, tail(A,1)*10)

  nml_tmp <- set_nml(nml_tmp, arg_name = 'H', arg_val = H)
  nml_tmp <- set_nml(nml_tmp, arg_name = 'A', arg_val = A)
  write_nml(glm_nml = nml_tmp, file = nml_tmp_file)
  
  for(breaks in 1:length(stops)){
    nml_tmp_file <- file.path(tmp2, 'glm2.nml')
    nml_tmp <- read_nml(nml_tmp_file) # cur nml file in temp run folder 
    
    # change nml start / stop 
    nml_tmp <- set_nml(nml_tmp, arg_name = 'start', arg_val = paste(starts[breaks], '00:00:00'))
    nml_tmp <- set_nml(nml_tmp, arg_name = 'stop', arg_val = paste(stops[breaks], '00:00:00'))
    write_nml(glm_nml = nml_tmp, file = nml_tmp_file)
    
    # run with modified start / stop 
    run_glm(sim_folder = tmp2, verbose = T)
    
    nc_tmp_file <- file.path(tmp2, 'output.nc')
    moveTo <- file.path(out_path, paste('output_', breaks, '.nc', sep=''))
    
    file.rename(from = nc_tmp_file, to = moveTo)
    
    # change nml to save to out dir instead of moving files around *********************************************
    
    # use sim output to set init_profiles of nml file 
    # n_depths <- seq() # vector of depths out for get_temp  func 
    
    cur_temp <- get_temp(moveTo, reference = 'surface', t_out = stops[breaks])[,-1L]
    nml_tmp <- set_nml(nml_tmp, arg_name = 'num_depths', arg_val = ncol(cur_temp))
    
    round_digits <- 0.0001 # how many digits we want to round down to
    depths <- floor(get.offsets(cur_temp)/round_digits)*round_digits
    depths[length(depths)] <- floor(tail(get_surface_height(moveTo)[,2],1)/round_digits)*round_digits
    temps <- as.numeric(get_temp(moveTo, reference = 'surface', z_out = depths, t_out = stops[breaks])[,-1L])
    
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_depths', arg_val = depths)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'lake_depth', arg_val = depths[length(depths)])
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_temps', arg_val = temps)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_sals', arg_val = rep(0,length(depths)))
    
    write_nml(glm_nml = nml_tmp, file = nml_tmp_file)
    
  }
}


compile_output <- function(dir, nLayers){
  files <- list.files(dir)
  
  out <- c()
  for(i in 1:length(files)){
    nc_file <- file.path(dir, files[i])
    temp <- get_temp(nc_file, reference = 'surface')
    depths <- get.offsets(temp)
    
    out <- c(out, depths)
  }
  max_z <- max(out) # max depth throughout sim 
  min_z <- min(out) # min depth throughout sim 
  
  depths_out <- seq(min_z, max_z, length.out = nLayers) # fixed layers based min / max depth in sim and nLayers 
  
  out <- data.frame()
  for(i in 1:length(files)){
    nc_file <- file.path(dir, files[i])
    temp <- get_temp(nc_file, reference = 'surface', z_out = depths_out)
    
    out <- rbind(out, temp)
  }
  
  out <- out[sort.list(out$DateTime),]
  return(out)
} # function for compiling glm temperature output and fixing depths returned (nLayers) 

nLayers <- 14 
d_10 <- compile_output('./30_lake_water_energy_model/temp/output_10_days/',nLayers = nLayers)
d_2 <- compile_output('./30_lake_water_energy_model/temp/output_2_days/', nLayers = nLayers)
d_40 <- compile_output('./30_lake_water_energy_model/temp/output_40_days/', nLayers = nLayers)
d_100 <- compile_output('./30_lake_water_energy_model/temp/output_100_days/', nLayers = nLayers)
d_20 <- compile_output('./30_lake_water_energy_model/temp/output_20_days/', nLayers = nLayers)
d_5 <- compile_output('./30_lake_water_energy_model/temp/output_5_days/', nLayers = nLayers)

windows()
ylim=c(min(c(d_2$temp_0,d_10$temp_0,d_40$temp_0,d_100$temp_0),na.rm = T),
       max(c(d_2$temp_0,d_10$temp_0,d_40$temp_0,d_100$temp_0),na.rm = T))
par(mar=c(5,6,3,3))
lwd=3
cex=2
plot(d_2$temp_0~d_2$DateTime,type='l', ylim=ylim, lwd=lwd, cex.axis= cex, ylab='Temp', xlab='', cex.lab=cex)
lines(d_10$temp_0~d_10$DateTime,type='l', col='red',lwd=lwd)
lines(d_40$temp_0~d_40$DateTime, col='blue',lwd=lwd)
lines(d_100$temp_0~d_100$DateTime, col='green',lwd=lwd)
lines(d_20$temp_0~d_20$DateTime, col='orange',lwd=lwd)
lines(d_5$temp_0~d_5$DateTime, col='pink',lwd=lwd)
legend('topright',legend = c('2 days','5 days', '10 days', '20 days', '40 days',' 100 days'),col = c('black','pink','red','orange', 'blue','green'),
       lwd=lwd , cex = 2,bty = 'n')

windows()
colnames(d_2)[1]='datetime'
wtr.heat.map(d_2)
windows()
colnames(d_5)[1]='datetime'
wtr.heat.map(d_5)
windows()
colnames(d_10)[1]='datetime'
wtr.heat.map(d_10)



