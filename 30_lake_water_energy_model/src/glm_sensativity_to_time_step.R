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
    
    # use sim output to set init_profiles of nml file 
    cur_temp <- get_temp(moveTo)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'num_depths', arg_val = (ncol(cur_temp)-1))
    
    depths <- round(as.numeric(na.omit(as.numeric(unlist(strsplit(names(cur_temp)[2:ncol(cur_temp)], split = 'temp.elv_'))))),digits = 5)
    depths[length(depths)] <- round(get_surface_height(moveTo)[nrow(cur_temp),2],digits = 5)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_depths', arg_val = depths)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'lake_depth', arg_val = depths[length(depths)])
    temps <- get_temp(moveTo, z_out = get_surface_height(moveTo)[nrow(cur_temp),2])[nrow(cur_temp),2] # gets temp for surface; ensures no NA's 
    temps <- c(as.numeric(cur_temp[nrow(cur_temp),2:(ncol(cur_temp)-1)]),temps)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_temps', arg_val = temps)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'the_sals', arg_val = rep(0,length(depths)))
    
    # have to change morphometry based on most recent lake depth 
    max_z <- nml_tmp$init_profiles$lake_depth
    x <- nml_tmp$morphometry$H
    y <- nml_tmp$morphometry$A
    xout <- c(nml_tmp$morphometry$H[1:(nml_tmp$morphometry$bsn_vals-1)],nml_tmp$morphometry$H[1]+max_z)
    interp_area <- approx(x = x, y = y, xout = xout)$y
    if(is.na(interp_area[length(interp_area)])){ # if interpolation has NA 
      slope <- (y[length(y)]-y[(length(y)-1)])/(x[length(x)]-x[(length(x)-1)])
      interp_area[length(interp_area)] <- y[(length(y)-1)]+slope*(xout[length(xout)]-xout[(length(xout)-1)])
    }
    
    xout <- xout[sort.list(xout)]
    interp_area <- interp_area[sort.list(interp_area)]
    
    nml_tmp <- set_nml(nml_tmp, arg_name = 'H', arg_val = xout)
    nml_tmp <- set_nml(nml_tmp, arg_name = 'A', arg_val = interp_area)
    
    write_nml(glm_nml = nml_tmp, file = nml_tmp_file)
    
    # plot_temp(nc_tmp_file)
    # get_temp(nc_tmp_file)
  }
}



compile_output <- function(dir, nLayers){
  files <- list.files(dir)
  
  out <- c()
  for(i in 1:length(files)){
    nc_file <- file.path(dir, files[i])
    temp <- get_temp(nc_file)
    depths <- as.numeric(na.omit(as.numeric(unlist(strsplit(names(temp)[2:ncol(temp)], split = 'temp.elv_')))))
    
    out <- c(out, depths)
  }
  max_z <- max(out) # max depth throughout sim 
  min_z <- min(out) # min depth throughout sim 
  
  depths_out <- seq(min_z, max_z, length.out = nLayers) # fixed layers based min / max depth in sim and nLayers 
  
  out <- data.frame()
  for(i in 1:length(files)){
    nc_file <- file.path(dir, files[i])
    temp <- get_temp(nc_file)
    depths <- as.numeric(na.omit(as.numeric(unlist(strsplit(names(temp)[2:ncol(temp)], split = 'temp.elv_')))))
    
    temp_out <- data.frame()
    for(j in 1:nrow(temp)){
      interp_temp <- approx(x = depths, y = temp[j,2:ncol(temp)], xout = depths_out)  # linearly interpolate to set depth intervals 
      cur <- data.frame(matrix(interp_temp$y, ncol = length(depths_out)))
      colnames(cur) <- paste('temp.elv_', depths_out, sep='')
      cur$DateTime <- temp$DateTime[j]
      cur <- cur[,c(ncol(cur),1:(ncol(cur)-1))]
      
      temp_out <- rbind(temp_out, cur)
    }
    
    out <- rbind(out, temp_out)
  }
  
  out <- out[sort.list(out$DateTime),]
  return(out)
} # function for compiling glm temperature output and fixing depths returned (nLayers) 

d_10 <- compile_output('./30_lake_water_energy_model/temp/output_10_days/',nLayers = 14)
d_2 <- compile_output('./30_lake_water_energy_model/temp/output_2_days/', nLayers = 14)
d_40 <- compile_output('./30_lake_water_energy_model/temp/output_40_days/', nLayers = 14)
d_100 <- compile_output('./30_lake_water_energy_model/temp/output_100_days/', nLayers = 14)


windows()
ylim=c(min(c(d_2$temp.elv_0,d_10$temp.elv_0,d_40$temp.elv_0,d_100$temp.elv_0)),max(c(d_2$temp.elv_0,d_10$temp.elv_0,d_40$temp.elv_0,d_100$temp.elv_0)))
plot(d_2$temp.elv_0~d_2$DateTime,type='l', ylim=ylim)
lines(d_10$temp.elv_0~d_10$DateTime,type='l', col='red')
lines(d_40$temp.elv_0~d_40$DateTime, col='blue')
lines(d_10$temp.elv_0~d_100$DateTime, col='green')

windows()
ylim=c(min(c(d_2[,14],d_10[,14],d_40[,14],d_100[,14]),na.rm = T),max(c(d_2[,14],d_10[,14],d_40[,14],d_100[,14]),na.rm = T))
lwd=3
cex=2
plot(d_2[,14]~d_2$DateTime,type='l', ylim=ylim, lwd=lwd, cex.axis= cex, ylab='Temp', xlab='', cex.lab=cex)
lines(d_10[,14]~d_10$DateTime,type='l', col='red',lwd=lwd)
lines(d_40[,14]~d_40$DateTime, col='blue',lwd=lwd)
lines(d_100[,14]~d_100$DateTime, col='green',lwd=lwd)
legend('topright',legend = c('2 days','10 days', '40 days',' 100 days'),col = c('black','red','blue','green'),
       lwd=lwd , cex = 2,bty = 'n')


