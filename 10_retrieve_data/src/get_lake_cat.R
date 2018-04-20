

# use lakeCat from EPA for lake catchment 


sf::st_read('/Users/jzwart/lakeScale/10_retrieve_data/out/lakecat/LkCat_Frame_min/shps/')

basins = maptools::readShapeSpatial('/Users/jzwart/lakeScale/10_retrieve_data/out/lakecat/LkCat_Frame_min/shps/allBasins.shp')

sub = basins[1:100,]

plot(sub$AreaSqKM)

plot(basins)

sp::spplot(sub)


