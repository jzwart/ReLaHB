# test GLM 
# JAZ; 2017-09-14 

library(glmtools)

tmp = file.path(tempdir(), 'glm_egs') # file path to temp directory 

dir.create(tmp) # create tmp dir 

run_example_sim(sim_folder = tmp, verbose = F) # creates example simulation files in 


nml_file = file.path(tmp, 'glm2.nml')
nml <- read_nml(nml_file)
nml # nml file => config for glm; lake specific 

get_nml_value(nml, 'Kw') # returns specifiec parameter 

run_glm(tmp, verbose = T) # runs model based on nml and data; saves output to folder path 

nc_file <- file.path(tmp, 'output.nc')

plot_temp(nc_file)


nml <- set_nml(nml, arg_name = 'Kw', arg_val = 1) # changes specified value in nml file 

write_nml(glm_nml = nml, file = nml_file) # writes modified nml file 

run_glm(tmp) # run with modified parameters 

nc_file <- file.path(tmp, 'output.nc')

plot_temp(nc_file)

sim_vars(nc_file) #list of simulation values 

get_temp(nc_file)

get_surface_height(nc_file)
get_ice(nc_file)


###################








