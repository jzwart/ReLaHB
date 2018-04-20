


library(ncdf4)

ncin = nc_open('D:/2015/201501010000_streamflow.nc')

print(ncin)

lon = ncvar_get(ncin,'lon')

ncin$groups

