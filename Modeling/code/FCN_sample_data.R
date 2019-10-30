get_sampled_data <- function(full_data, plant_stat, grid_size, lat_offset, lon_offset, agg_scheme, plot_samples, year_oi) {
  # agg_scheme is FALSE for using only individual cells at grid point locations, TRUE for aggregating to grid polygons by mean cell values. plot_samples is TRUE or FALSE, for showing where samples were taken for SC and DC for a given year_oi. year_oi is only for plotting purposes
  
  full_data <- full_data[full_data$plant_stat_type == plant_stat, ]
  # order cells so the slowest-changing row is cell_ID
  full_data <- full_data[order(full_data$cell_ID) , ]
  full_data$index <- 1:nrow(full_data) #unique row index to join later
  full_data_sp <- as(full_data, 'Spatial')
  

  
  # generate regular grid of points
  samplePoints <- makegrid(MT_outline, cellsize = grid_size)
  samplePoints$x1 <- samplePoints$x1 + lon_offset
  samplePoints$x2 <- samplePoints$x2 + lat_offset
  samplePoints <- SpatialPoints(samplePoints, proj4string = CRS(proj4string(MT_outline)))
  

  # sampling only the desired 
  if(!agg_scheme) {
    # sample from grid of points, delete NA's
    samples <- sp::over(samplePoints, full_data_sp, returnList = TRUE) %>%
      bind_rows()
    samples <- samples[complete.cases(samples),] # data.frame
    sampled_data <- merge(data.frame(index = samples[, "index"]), full_data, by.x = "index", by.y = "index")
    sampled_data <- st_as_sf(sampled_data)
  }
  
  if(agg_scheme) {
    samplePixels <- SpatialPixels(samplePoints[MT_outline,])
    samplePixels <- as(samplePixels, "SpatialPolygons")
    
    # turn SpatialPolygon into SpatialPolygonsDataFrame
    # Create a dataframe and display default rownames
    samplePixels.df <- data.frame( ID=1:length(samplePixels))
    rownames(samplePixels.df)
    
    # Extract polygon ID's
    pid <- sapply(slot(samplePixels, "polygons"), function(x) slot(x, "ID"))
    
    # Create dataframe with correct rownames
    samplePixels.df <- data.frame( ID=1:length(samplePixels), row.names = pid)  
    
    #coersion 
    samplePixels.spdf <- SpatialPolygonsDataFrame(samplePixels, samplePixels.df)
    
    # separate full_data_sp by intensity because can't take mean of intensity. also get rid of cell_ID, year_factor
    full_data_sp_DC <- full_data_sp[full_data_sp$intensity == "DC",
                                    !(names(full_data_sp) %in% c("cell_ID", "intensity", "year_factor"))]
    full_data_sp_SC <- full_data_sp[full_data_sp$intensity == "SC",
                                    !(names(full_data_sp) %in% c("cell_ID", "intensity", "year_factor"))]
    
    # 'empty' SpatialPolysDataFrame to store per-year info
    sampled_data_DC <- full_data_sp_DC[1,]
    sampled_data_SC <- full_data_sp_SC[1,]
    
    # aggregate 25km cells to grid cell. do by year and intensity separately
    for (year in 2004:2014) {
      sampled_year_DC <- aggregate(full_data_sp_DC[full_data_sp_DC$year == year,], by = samplePixels.spdf, FUN = mean) 
      sampled_year_SC <- aggregate(full_data_sp_SC[full_data_sp_SC$year == year,], by = samplePixels.spdf, FUN = mean) 
      
      sampled_data_DC <- rbind(sampled_data_DC, sampled_year_DC)
      sampled_data_SC <- rbind(sampled_data_SC, sampled_year_SC)
    }
    
    # get rid of first row
    sampled_data_DC <- sampled_data_DC[2:nrow(sampled_data_DC),]
    sampled_data_SC <- sampled_data_SC[2:nrow(sampled_data_SC),]
    
    # add intensity, year_factor as a new row
    sampled_data_DC$intensity <- rep("DC", nrow(sampled_data_DC))
    sampled_data_SC$intensity <- rep("SC", nrow(sampled_data_SC))
    sampled_data_DC$year_factor <- as.factor(sampled_data_DC$year)
    sampled_data_SC$year_factor <- as.factor(sampled_data_SC$year)
    
    # combine SC and DC
    sampled_data <- rbind(sampled_data_DC, sampled_data_SC)
    sampled_data <- st_as_sf(sampled_data)
    
    sampled_data <- sampled_data %>% drop_na
    
  }
  
  if (plot_samples & agg_scheme) {
    sample_year <- subset(sampled_data, year == year_oi)
    full_year <- subset(full_data, year == year_oi)
    
    DC_sampled_map <- ggplot() +
      geom_sf(data = subset(full_year, intensity == "DC"), color = "gray") +
      geom_sf(data = subset(sample_year, intensity == "DC"), color = "blue", alpha = 0) +
      ggtitle(paste("Sampled cells, for DC", year_oi)) +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    
    SC_sampled_map <- ggplot() +
      geom_sf(data = subset(full_year, intensity == "SC"), color = "gray") +
      geom_sf(data = subset(sample_year, intensity == "SC"), color = "blue", alpha = 0) +
      ggtitle(paste("Sampled cells, for DC", year_oi)) +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    
    print(DC_sampled_map)
    print(SC_sampled_map)
  }
  
  if (plot_samples & !agg_scheme) {
    # plot the samples over the full dataset for a given year
    full_year <- subset(full_data, year == year_oi)
    sample_year <- subset(sampled_data, year == year_oi)
    
    DC_sampled_map <- ggplot() +
      geom_sf(data = subset(full_year, intensity == "DC"), color = "gray") +
      geom_sf(data = subset(sample_year, intensity == "DC"), color = "blue") +
      ggtitle(paste("Sampled grid, for DC", year_oi)) +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    
    SC_sampled_map <- ggplot() +
      geom_sf(data = subset(full_year, intensity == "SC"), color = "gray") +
      geom_sf(data = subset(sample_year, intensity == "SC"), color = "blue") +
      ggtitle(paste("Sampled grid, for SC", year_oi)) +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    
    print(DC_sampled_map)
    print(SC_sampled_map)
  }
  
  # output: data.frame to input to models
  return(sampled_data)
  
}

# test output
#sampled_data <- get_sampled_data(cell_sf_tidy, grid_size = 1, lat_offset = 0.5, lon_offset = 0.5, agg_scheme = TRUE, plot_samples = TRUE, year_oi = 2005)