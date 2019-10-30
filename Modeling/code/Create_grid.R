# create and export grid cells for fixed effects. cs defines the size of polygons in degree

cell_shp <- readOGR(dsn = 'E:/R-code/Modeling/data/shp/median_onset_cell', layer = 'median_onset_cell_SHP')

cs <- c(1, 1) # degrees
grdpts <- makegrid(cell_shp, cellsize = cs)
spgrd <- SpatialPoints(grdpts, proj4string = CRS(proj4string(cell_shp)))
plot(cell_shp)
spgrdWithin <- SpatialPixels(spgrd[cell_shp,])
spgrdWithin <- as(spgrdWithin, "SpatialPolygons")
plot(spgrdWithin, add = T, col = "red")

# turn SpatialPolygon into SpatialPolygonsDataFrame
# Create a dataframe and display default rownames
spgrdWithin.df <- data.frame( ID=1:length(spgrdWithin))
rownames(spgrdWithin.df)

# Extract polygon ID's
pid <- sapply(slot(spgrdWithin, "polygons"), function(x) slot(x, "ID"))

# Create dataframe with correct rownames
spgrdWithin.df <- data.frame( ID=1:length(spgrdWithin), row.names = pid)  

#coersion 
spgrdWithinDF <- SpatialPolygonsDataFrame(spgrdWithin, spgrdWithin.df)

writeOGR(spgrdWithinDF, dsn = '.', layer = 'grid_1deg', driver = "ESRI Shapefile")