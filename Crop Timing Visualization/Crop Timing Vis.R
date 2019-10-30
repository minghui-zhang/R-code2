library(ggplot2)
library(ggridges)
library(viridis)

# REDO FOR EVERY POLYGON

# mac: 
# ~/Documents/R-code/Crop Timing Visualization/input/...
# windows:
#E:\\R-code\\Crop Timing Visualization\\input\\...

specificPoly_data <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\FOI_stats.csv')
mydata_double_plant <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\double_plant_50km_MT.csv')
mydata_double_harvest <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\double_harvest_50km_MT.csv')
mydata_single_plant <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\single_plant_50km_MT.csv')
mydata_single_harvest <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\single_harvest_50km_MT.csv')
mydata_double <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\double_50km_MT.csv') #just pasted plant and harvest csvs together
mydata_single <- read.csv('E:\\R-code\\Crop Timing Visualization\\input\\single_50km_MT.csv')

# DOUBLE PLANT AND HARVEST

ggplot(mydata_double, aes(x = mydata_double$DOY, y = mydata_double$Year, height = mydata_double$Count, group = mydata_double$Year, fill = mydata_double$DOY)) + 
  xlim(0, 250) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Double crop") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = double_plant_median, y = year, group = year, xend = double_plant_median, yend = year + 0.9, height = rep(100, 15), fill = double_plant_median), color = "red") +
  geom_segment(data = specificPoly_data, aes(x = double_harvest_median, y = year, group = year, xend = double_harvest_median, yend = year + 0.9, height = rep(100, 15), fill = double_harvest_median), color = "red") +
  geom_segment(data = specificPoly_data, aes(x = onset_median, y = year, group = year, xend = onset_median, yend = year + 0.9, height = rep(100, 15), fill = onset_median), color = "blue") 


# SINGLE PLANT AND HARVEST

single <- ggplot(mydata_single, aes(x = mydata_single$DOY, y = mydata_single$Year, height = mydata_single$Count, group = mydata_single$Year, fill = mydata_single$DOY)) + 
  xlim(0, 250) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Single crop") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = single_plant_median, y = year, group = year, xend = single_plant_median, yend = year + 0.9, height = rep(100, 15), fill = single_plant_median), color = "red") +
  geom_segment(data = specificPoly_data, aes(x = single_harvest_median, y = year, group = year, xend = single_harvest_median, yend = year + 0.9, height = rep(100, 15), fill = single_harvest_median), color = "red") +
  geom_segment(data = specificPoly_data, aes(x = onset_median, y = year, group = year, xend = onset_median, yend = year + 0.9, height = rep(100, 15), fill = onset_median), color = "blue") 

print(single)

# DOUBLE PLANT

double <- ggplot(mydata_double_plant, aes(x = mydata_double_plant$DOY, y = mydata_double_plant$Year, height = mydata_double_plant$Count, group = mydata_double_plant$Year, fill = mydata_double_plant$DOY)) + 
  xlim(50, 150) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Double crop planting") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = double_plant_median, y = year, group = year, xend = double_plant_median, yend = year + 0.9, height = rep(100, 15), fill = double_plant_median), color = "red") 

print(double)

# DOUBLE HARVEST

ggplot(mydata_double_harvest, aes(x = mydata_double_harvest$DOY, y = mydata_double_harvest$Year, height = mydata_double_harvest$Count, group = mydata_double_harvest$Year, fill = mydata_double_harvest$DOY)) + 
  xlim(150, 250) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Double crop harvest") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = double_harvest_median, y = year, group = year, xend = double_harvest_median, yend = year + 0.9, height = rep(100, 15), fill = double_harvest_median), color = "red") 


# SINGLE PLANT

ggplot(mydata_single_plant, aes(x = mydata_single_plant$DOY, y = mydata_single_plant$Year, height = mydata_single_plant$Count, group = mydata_single_plant$Year, fill = mydata_single_plant$DOY)) + 
  xlim(50, 150) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Single crop planting") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = single_plant_median, y = year, group = year, xend = single_plant_median, yend = year + 0.9, height = rep(100, 15), fill = single_plant_median), color = "red") 


# SINGLE HARVEST

ggplot(mydata_single_harvest, aes(x = mydata_single_harvest$DOY, y = mydata_single_harvest$Year, height = mydata_single_harvest$Count, group = mydata_single_harvest$Year, fill = mydata_single_harvest$DOY)) + 
  xlim(150, 250) +
  xlab("DOY from Aug 1") +
  ylab("Year") +
  ggtitle("Single crop harvest") +
  geom_density_ridges_gradient(stat = "identity", scale = 1) +
  scale_fill_viridis(name = 'Days after Aug 1') +
  geom_segment(data = specificPoly_data, aes(x = single_harvest_median, y = year, group = year, xend = single_harvest_median, yend = year + 0.9, height = rep(100, 15), fill = single_harvest_median), color = "red") 

