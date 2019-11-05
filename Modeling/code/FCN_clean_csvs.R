# add cell_ID
clean_cell_ID <- function(cell_ID) {
  strsplit(cell_ID, "_")[[1]][2]
}

delete_cols_median_cell <- function(median_cell_raw) {
  output = subset(median_cell_raw, select = -c(double_area_km2_median, 
                                               double_delay_sum, double_harvest_sum, double_plant_sum,
                                               label, latitude_sum, longitude_sum, 
                                               onset_sum, single_area_km2_median, #onset_historicalRange_sum, 
                                               single_delay_sum, single_harvest_sum, single_plant_sum,
                                               total_planted_area_km2_median, year_sum, Muni_code_sum) )
  return(output)
}

rename_cols_median_cell <- function(median_cell) {
  output = median_cell %>% 
    dplyr::rename(
      cell_ID = system.index,
      double_area_km2 = double_area_km2_sum,
      double_delay = double_delay_median,
      double_harvest = double_harvest_median,
      double_plant = double_plant_median,
      latitude = latitude_median,
      longitude = longitude_median,
      #onset_historicalRange = onset_historicalRange_median,
      onset = onset_median,
      single_area_km2 = single_area_km2_sum,
      single_delay = single_delay_median,
      single_harvest = single_harvest_median,
      single_plant = single_plant_median,
      total_planted_area_km2 = total_planted_area_km2_sum,
      Muni_code = Muni_code_median,
      year = year_median
    ) %>%
    filter(year > 0)
  return(output)
}

rename_cols_median_muni <- function(median_muni_raw) {
  output = median_muni_raw %>%
    rename(
      double_area_km2 = double_area_km2_sum,
      double_delay = double_delay_median,
      double_harvest = double_harvest_median,
      double_plant = double_plant_median,
      latitude = latitude_median,
      longitude = longitude_median,
      onset_historicalRange = onset_historicalRange_median,
      onset = onset_median,
      single_area_km2 = single_area_km2_sum,
      single_delay = single_delay_median,
      single_harvest = single_harvest_median,
      single_plant = single_plant_median,
      total_planted_area_km2 = total_planted_area_km2_sum,
      year = year_median
    ) %>%
    filter(year > 0)
  return(output)
}

rename_cols_percentile_cell <- function(percentile_cell) {
  output = percentile_cell %>% 
    dplyr::rename(
      cell_ID = system.index,
      DC_area_km = double_area_km2,
      DC_delay = double_delay,
      DC_harvest = double_harvest,
      DC_plant = double_plant,
      lat = latitude,
      lon = longitude,
      #onset_rang = onset_historicalRange,
      onset = onset,
      SC_area_km = single_area_km2,
      SC_delay = single_delay,
      SC_harvest = single_harvest,
      SC_plant = single_plant,
      soy_area_k = total_planted_area_km2,
      Muni_code = Muni_code,
      year = year
    ) %>%
    filter(year > 0)
  return(output)
}

tidy_by_intensity_plant <- function(data, SC_name, DC_name) {
  output = data %>%
    gather(DC_name, SC_name, key = "intensity", value = "plant") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = DC_name, replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = SC_name, replacement = "SC")
  
  return(output)
}

tidy_by_intensity_harvest <- function(data, SC_name, DC_name) {
  output = data %>%
    gather(DC_name, SC_name, key = "intensity", value = "harvest") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = DC_name, replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = SC_name, replacement = "SC")
  
  return(output)
}

tidy_by_intensity_delay <- function(data, SC_name, DC_name) {
  output = data %>%
    gather(DC_name, SC_name, key = "intensity", value = "delay") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = DC_name, replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = SC_name, replacement = "SC")
  
  return(output)
}

combine_variables_cell <- function(median_cell_plant, median_cell_delay,
                                   percentile5_cell_plant, percentile5_cell_delay,
                                   percentile95_cell_plant, percentile95_cell_delay) {
  output <- cbind(median_cell_plant, median_cell_delay$delay, 
                  percentile5_cell_plant$plant, percentile5_cell_delay$delay,
                  percentile95_cell_plant$plant, percentile95_cell_delay$delay)
  colnames(output)[colnames(output)=="plant"] <- "plant_median"
  colnames(output)[colnames(output)=="percentile5_cell_plant$plant"] <- "plant_percentile5"
  colnames(output)[colnames(output)=="percentile95_cell_plant$plant"] <- "plant_percentile95"
  colnames(output)[colnames(output)=="median_cell_delay$delay"] <- "delay_median"
  colnames(output)[colnames(output)=="percentile5_cell_delay$delay"] <- "delay_percentile5"
  colnames(output)[colnames(output)=="percentile95_cell_delay$delay"] <- "delay_percentile95"
  
  output$intensity_num[output$intensity == "DC"] <- 1
  output$intensity_num[output$intensity == "SC"] <- 0
  
  return(output)
}

combine_variables_muni <- function(median_muni_plant, median_muni_delay,
                                   percentile5_muni_plant, percentile5_muni_delay,
                                   percentile95_muni_plant, percentile95_muni_delay) {
  
  output <- cbind(median_muni_plant, median_muni_delay$delay, 
                  percentile5_muni_plant$plant, percentile5_muni_delay$delay,
                  percentile95_muni_plant$plant, percentile95_muni_delay$delay)
  
  colnames(output)[colnames(output)=="plant"] <- "plant_median"
  colnames(output)[colnames(output)=="percentile5_muni_plant$plant"] <- "plant_percentile5"
  colnames(output)[colnames(output)=="percentile95_muni_plant$plant"] <- "plant_percentile95"
  colnames(output)[colnames(output)=="median_muni_delay$delay"] <- "delay_median"
  colnames(output)[colnames(output)=="percentile5_muni_delay$delay"] <- "delay_percentile5"
  colnames(output)[colnames(output)=="percentile95_muni_delay$delay"] <- "delay_percentile95"
  output$intensity_num[output$intensity == "DC"] <- 1
  output$intensity_num[output$intensity == "SC"] <- 0
  
  return(output)
}

combine_variables_CARpoly <- function(median_CARpoly_plant, median_CARpoly_delay,
                                      percentile5_CARpoly_plant, percentile5_CARpoly_delay,
                                      percentile95_CARpoly_plant, percentile95_CARpoly_delay) {
  output <- cbind(median_CARpoly_plant, median_CARpoly_delay$delay, 
                  percentile5_CARpoly_plant$plant, percentile5_CARpoly_delay$delay,
                  percentile95_CARpoly_plant$plant, percentile95_CARpoly_delay$delay)
  
  colnames(output)[colnames(output)=="plant"] <- "plant_median"
  colnames(output)[colnames(output)=="percentile5_CARpoly_plant$plant"] <- "plant_percentile5"
  colnames(output)[colnames(output)=="percentile95_CARpoly_plant$plant"] <- "plant_percentile95"
  colnames(output)[colnames(output)=="median_CARpoly_delay$delay"] <- "delay_median"
  colnames(output)[colnames(output)=="percentile5_CARpoly_delay$delay"] <- "delay_percentile5"
  colnames(output)[colnames(output)=="percentile95_CARpoly_delay$delay"] <- "delay_percentile95"
  output$intensity_num[output$intensity == "DC"] <- 1
  output$intensity_num[output$intensity == "SC"] <- 0
  
  return(output)
}

tidy_combine_cell <- function(median_cell, percentile5_cell, percentile95_cell) {
  median_cell_plant <- median_cell %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  median_cell_delay <- median_cell %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest))
  
  percentile5_cell_plant <- percentile5_cell %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  percentile5_cell_delay <- percentile5_cell %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest))
  
  percentile95_cell_plant <- percentile95_cell %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  percentile95_cell_delay <- percentile95_cell %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest))
  
  # combine together
  cell_tidy <- combine_variables_cell(median_cell_plant, median_cell_delay,
                                      percentile5_cell_plant, percentile5_cell_delay,
                                      percentile95_cell_plant, percentile95_cell_delay)
  
  return(cell_tidy)
}

tidy_combine_muni <- function(median_muni, percentile5_muni, percentile95_muni) {
  
  median_muni_plant <- median_muni %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  median_muni_delay <- median_muni %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest))
  
  percentile5_muni_plant <- percentile5_muni %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  percentile5_muni_delay <- percentile5_muni %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest))
  
  percentile95_muni_plant <- percentile95_muni %>% tidy_by_intensity_plant("single_plant", "double_plant") %>%
    dplyr::select(-c(single_delay, double_delay, single_harvest, double_harvest))
  
  percentile95_muni_delay <- percentile95_muni %>% tidy_by_intensity_delay("single_delay", "double_delay") %>%
    dplyr::select(-c(single_plant, double_plant, single_harvest, double_harvest)) 
  
  # combine together
  muni_tidy <- combine_variables_muni(median_muni_plant, median_muni_delay,
                                      percentile5_muni_plant, percentile5_muni_delay,
                                      percentile95_muni_plant, percentile95_muni_delay)
  
  return(muni_tidy)
}

categorize_vars_cell_tidy <- function(cell_tidy) {
  
  cell_onset_cutoffs = quantile(cell_tidy$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  cell_onset_cutoffs[1] = cell_onset_cutoffs[1]-1
  cell_delay_cutoffs = quantile(cell_tidy$delay_median, c(0,1/3,2/3,1), na.rm = TRUE)
  cell_delay_cutoffs[1] = cell_delay_cutoffs[1]-1
  
  regions <- character(length = length(cell_tidy$latitude))
  regions[cell_tidy$latitude < -15] <- "south"
  regions[cell_tidy$latitude >= -15 & cell_tidy$longitude < -56.5] <- "west"
  regions[cell_tidy$latitude >= -15 & cell_tidy$longitude >= -56.5 & cell_tidy$longitude < -53.2] <- "central"
  regions[cell_tidy$latitude >= -15 & cell_tidy$longitude >= -53.2] <- "east"
  cell_tidy$region <- regions
  
  cell_tidy <- cell_tidy %>% mutate(onset_category=cut(onset, breaks = cell_onset_cutoffs, 
                                                       labels=c("early_onset","middle_onset","late_onset"))) %>%
    mutate(delay_category=cut(delay_median, breaks = cell_delay_cutoffs, 
                              labels=c("short_delay","middle_delay","long_delay")))
  
  return(cell_tidy)
}

categorize_regions_cell_sf_tidy <- function(cell_tidy) {
  
  regions <- character(length = length(cell_tidy$lat))
  regions[cell_tidy$lat < -15] <- "south"
  regions[cell_tidy$lat >= -15 & cell_tidy$lon < -56.5] <- "west"
  regions[cell_tidy$lat >= -15 & cell_tidy$lon >= -56.5 & cell_tidy$lon < -53.2] <- "central"
  regions[cell_tidy$lat >= -15 & cell_tidy$lon >= -53.2] <- "east"
  cell_tidy$region <- regions
  
  return(cell_tidy)
}

categorize_vars_cell_untidy <- function(median_cell) {
  cell_onset_cutoffs = quantile(median_cell$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  cell_onset_cutoffs[1] = cell_onset_cutoffs[1]-1
  
  regions <- character(length = length(median_cell$latitude))
  regions[median_cell$latitude < -15] <- "south"
  regions[median_cell$latitude >= -15 & median_cell$longitude < -56.5] <- "west"
  regions[median_cell$latitude >= -15 & median_cell$longitude >= -56.5 & median_cell$longitude < -53.2] <- "central"
  regions[median_cell$latitude >= -15 & median_cell$longitude >= -53.2] <- "east"
  median_cell$region <- regions
  
  median_cell <- median_cell %>% mutate(onset_category=cut(onset, breaks = cell_onset_cutoffs, 
                                                           labels=c("early_onset","middle_onset","late_onset"))) 
  
  return(median_cell)
}

categorize_vars_muni_tidy <- function(muni_tidy) {
  
  muni_onset_cutoffs = quantile(muni_tidy$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  muni_onset_cutoffs[1] = muni_onset_cutoffs[1]-1
  muni_delay_cutoffs = quantile(muni_tidy$delay_median, c(0,1/3,2/3,1), na.rm = TRUE)
  muni_delay_cutoffs[1] = muni_delay_cutoffs[1]-1
  
  regions <- character(length = length(muni_tidy$latitude))
  regions[muni_tidy$latitude < -15] <- "south"
  regions[muni_tidy$latitude >= -15 & muni_tidy$longitude < -56.5] <- "west"
  regions[muni_tidy$latitude >= -15 & muni_tidy$longitude >= -56.5 & muni_tidy$longitude < -53.2] <- "central"
  regions[muni_tidy$latitude >= -15 & muni_tidy$longitude >= -53.2] <- "east"
  muni_tidy$region <- regions
  
  muni_tidy <- muni_tidy %>% mutate(onset_category=cut(onset, breaks = muni_onset_cutoffs, 
                                                       labels=c("early_onset","middle_onset","late_onset"))) %>%
    mutate(delay_category=cut(delay_median, breaks = muni_delay_cutoffs, 
                              labels=c("short_delay","middle_delay","long_delay")))
  
  return(muni_tidy)
}

categorize_vars_muni_untidy <- function(median_muni) {
  muni_onset_cutoffs = quantile(median_muni$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  muni_onset_cutoffs[1] = muni_onset_cutoffs[1]-1
  muni_lat_cutoffs = quantile(median_muni$latitude, c(0,1/3,2/3,1), na.rm = TRUE)
  muni_lat_cutoffs[1] = muni_lat_cutoffs[1]-1
  
  regions <- character(length = length(median_muni$latitude))
  regions[median_muni$latitude < -15] <- "south"
  regions[median_muni$latitude >= -15 & median_muni$longitude < -56.5] <- "west"
  regions[median_muni$latitude >= -15 & median_muni$longitude >= -56.5 & median_muni$longitude < -53.2] <- "central"
  regions[median_muni$latitude >= -15 & median_muni$longitude >= -53.2] <- "east"
  median_muni$region <- regions
  
  median_muni <- median_muni %>% mutate(onset_category=cut(onset, breaks = muni_onset_cutoffs, 
                                                           labels=c("early_onset","middle_onset","late_onset")))
  
  return(median_muni)
}

categorize_vars_CARpoly_tidy <- function(CARpoly_tidy) {
  CARpoly_onset_cutoffs = quantile(CARpoly_tidy$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  CARpoly_onset_cutoffs[1] = CARpoly_onset_cutoffs[1]-1
  CARpoly_delay_cutoffs = quantile(CARpoly_tidy$delay_median, c(0,1/3,2/3,1), na.rm = TRUE)
  CARpoly_delay_cutoffs[1] = CARpoly_delay_cutoffs[1]-1
  CARpoly_area_cutoffs = quantile(CARpoly_tidy$poly_area_km2, c(0,1/3,2/3,1), na.rm = TRUE)
  CARpoly_area_cutoffs[1] = CARpoly_area_cutoffs[1]-1
  
  regions <- character(length = length(CARpoly_tidy$latitude))
  regions[CARpoly_tidy$latitude < -15] <- "south"
  regions[CARpoly_tidy$latitude >= -15 & CARpoly_tidy$longitude < -56.5] <- "west"
  regions[CARpoly_tidy$latitude >= -15 & CARpoly_tidy$longitude >= -56.5 & CARpoly_tidy$longitude < -53.2] <- "central"
  regions[CARpoly_tidy$latitude >= -15 & CARpoly_tidy$longitude >= -53.2] <- "east"
  CARpoly_tidy$region <- regions
  
  CARpoly_tidy <- CARpoly_tidy %>% mutate(onset_category=cut(onset, breaks = CARpoly_onset_cutoffs, 
                                                             labels=c("early_onset","middle_onset","late_onset"))) %>%
    mutate(delay_category=cut(delay_median, breaks = CARpoly_delay_cutoffs, 
                              labels=c("short_delay","middle_delay","long_delay"))) %>%
    mutate(area_category=cut(poly_area_km2, breaks = CARpoly_area_cutoffs, 
                             labels=c("small_property","middle_property","large_property")))
  
  return(CARpoly_tidy)
}

categorize_vars_CARpoly_untidy <- function(median_CARpoly) {
  CARpoly_onset_cutoffs = quantile(median_CARpoly$onset, c(0,1/3,2/3,1), na.rm = TRUE)
  CARpoly_onset_cutoffs[1] = CARpoly_onset_cutoffs[1]-1
  CARpoly_area_cutoffs = quantile(median_CARpoly$poly_area_km2, c(0,1/3,2/3,1), na.rm = TRUE)
  CARpoly_area_cutoffs[1] = CARpoly_area_cutoffs[1]-1
  
  regions <- character(length = length(median_CARpoly$latitude))
  regions[median_CARpoly$latitude < -15] <- "south"
  regions[median_CARpoly$latitude >= -15 & median_CARpoly$longitude < -56.5] <- "west"
  regions[median_CARpoly$latitude >= -15 & median_CARpoly$longitude >= -56.5 & median_CARpoly$longitude < -53.2] <- "central"
  regions[median_CARpoly$latitude >= -15 & median_CARpoly$longitude >= -53.2] <- "east"
  median_CARpoly$region <- regions
  
  median_CARpoly <- median_CARpoly %>% mutate(onset_category=cut(onset, breaks = CARpoly_onset_cutoffs, 
                                                                 labels=c("early_onset","middle_onset","late_onset"))) %>%
    mutate(area_category=cut(poly_area_km2, breaks = CARpoly_area_cutoffs, 
                             labels=c("small_property","middle_property","large_property")))
  
  return(median_CARpoly)
}

# categorize cells as new or old or neither in planted soy age
cell_categorize_soy_age <- function(cell) {
  
  soy_age <- rep("z_neither", sum(cell$year == "2004"))
  cell_area_2004 <- cell[cell$year == "2004", "total_planted_area_km2"]
  cell_area_2014 <- cell[cell$year == "2014", "total_planted_area_km2"]
  soy_age[cell_area_2004 < min_soy_area & cell_area_2014 >= min_soy_area] <- "new"
  soy_age[cell_area_2004 >= min_soy_area & cell_area_2014 >= min_soy_area] <- "old"
  cell$soy_age <- rep(soy_age, 11)
  
  return(cell)
}

# CAR poly functions
rename_cols_median_CARpoly <- function(median_CARpoly_raw) {
  output = median_CARpoly_raw %>%
    rename(
      latitude = latitude_median,
      longitude = longitude_median,
      onset = onset_median,
      onset_historicalRange = onset_historicalRange_median,
      double_area_km2 = double_area_km2_sum,
      single_area_km2 = single_area_km2_sum,
      total_planted_area_km2 = total_planted_area_km2_sum,
      year = year_median
    ) %>%
    filter(year > 0)
  
  return(output)
}

rename_cols_percentile5_CARpoly <- function(percentile5_CARpoly_raw) {
  output = percentile5_CARpoly_raw %>%
    rename(
      double_delay_percentile5 = double_delay,
      double_harvest_percentile5 = double_harvest,
      double_plant_percentile5 = double_plant,
      single_delay_percentile5 = single_delay,
      single_harvest_percentile5 = single_harvest,
      single_plant_percentile5 = single_plant
    ) %>%
    filter(year > 0)
  
  return(output)
}

rename_cols_percentile95_CARpoly <- function(percentile95_CARpoly_raw) {
  output = percentile95_CARpoly_raw %>%
    rename(
      double_delay_percentile95 = double_delay,
      double_harvest_percentile95 = double_harvest,
      double_plant_percentile95 = double_plant,
      single_delay_percentile95 = single_delay,
      single_harvest_percentile95 = single_harvest,
      single_plant_percentile95 = single_plant
    ) %>%
    filter(year > 0)
  
  return(output)
}

create_CARpoly_raw <- function(median_CARpoly_raw, percentile5_CARpoly_raw, percentile95_CARpoly_raw) {
  # join as single dataset and rename
  CARpoly_raw <- cbind(median_CARpoly_raw, 
                       percentile5_CARpoly_raw$double_delay_percentile5,
                       percentile5_CARpoly_raw$double_harvest_percentile5,
                       percentile5_CARpoly_raw$double_plant_percentile5,
                       percentile95_CARpoly_raw$double_delay_percentile95,
                       percentile95_CARpoly_raw$double_harvest_percentile95,
                       percentile95_CARpoly_raw$double_plant_percentile95,
                       
                       percentile5_CARpoly_raw$single_delay_percentile5,
                       percentile5_CARpoly_raw$single_harvest_percentile5,
                       percentile5_CARpoly_raw$single_plant_percentile5,
                       percentile95_CARpoly_raw$single_delay_percentile95,
                       percentile95_CARpoly_raw$single_harvest_percentile95,
                       percentile95_CARpoly_raw$single_plant_percentile95)
  
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$single_delay_percentile5"] <- "single_delay_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$single_harvest_percentile5"] <- "single_harvest_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$single_plant_percentile5"] <- "single_plant_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$single_delay_percentile95"] <- "single_delay_percentile95"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$single_harvest_percentile95"] <- "single_harvest_percentile95"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$single_plant_percentile95"] <- "single_plant_percentile95"
  
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$double_delay_percentile5"] <- "double_delay_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$double_harvest_percentile5"] <- "double_harvest_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile5_CARpoly_raw$double_plant_percentile5"] <- "double_plant_percentile5"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$double_delay_percentile95"] <- "double_delay_percentile95"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$double_harvest_percentile95"] <- "double_harvest_percentile95"
  colnames(CARpoly_raw)[colnames(CARpoly_raw)=="percentile95_CARpoly_raw$double_plant_percentile95"] <- "double_plant_percentile95"
  
  return(CARpoly_raw)
}

tidy_CARpoly <- function(CARpoly) {
  
  # medians
  CARpoly_delay_median <- CARpoly %>% 
    gather("double_delay_median", "single_delay_median", key = "intensity", value = "delay_median") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_delay_median", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_delay_median", replacement = "SC")
   
  CARpoly_harvest_median <- CARpoly %>% 
    gather("double_harvest_median", "single_harvest_median", key = "intensity", value = "harvest_median") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_harvest_median", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_harvest_median", replacement = "SC")
  
  CARpoly_plant_median <- CARpoly %>% 
    gather("double_plant_median", "single_plant_median", key = "intensity", value = "plant_median") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_plant_median", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_plant_median", replacement = "SC")
  
  # percentile5
  CARpoly_delay_percentile5 <- CARpoly %>% 
    gather("double_delay_percentile5", "single_delay_percentile5", key = "intensity", value = "delay_percentile5") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_delay_percentile5", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_delay_percentile5", replacement = "SC")
  
  CARpoly_harvest_percentile5 <- CARpoly %>% 
    gather("double_harvest_percentile5", "single_harvest_percentile5", key = "intensity", value = "harvest_percentile5") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_harvest_percentile5", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_harvest_percentile5", replacement = "SC")
  
  CARpoly_plant_percentile5 <- CARpoly %>% 
    gather("double_plant_percentile5", "single_plant_percentile5", key = "intensity", value = "plant_percentile5") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_plant_percentile5", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_plant_percentile5", replacement = "SC")
  
  # percentile95
  CARpoly_delay_percentile95 <- CARpoly %>% 
    gather("double_delay_percentile95", "single_delay_percentile95", key = "intensity", value = "delay_percentile95") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_delay_percentile95", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_delay_percentile95", replacement = "SC")
  
  CARpoly_harvest_percentile95 <- CARpoly %>% 
    gather("double_harvest_percentile95", "single_harvest_percentile95", key = "intensity", value = "harvest_percentile95") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_harvest_percentile95", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_harvest_percentile95", replacement = "SC")
  
  CARpoly_plant_percentile95 <- CARpoly %>% 
    gather("double_plant_percentile95", "single_plant_percentile95", key = "intensity", value = "plant_percentile95") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "double_plant_percentile95", replacement = "DC") %>%
    mutate_if(is.character, 
              str_replace_all, pattern = "single_plant_percentile95", replacement = "SC")
  
  # combine and rename
  
  CARpoly_tidy <- CARpoly_raw <- cbind(CARpoly_delay_median, 
                                       CARpoly_harvest_median$harvest_median,
                                       CARpoly_plant_median$plant_median,
                                       CARpoly_delay_percentile5$delay_percentile5,
                                       CARpoly_harvest_percentile5$harvest_percentile5,
                                       CARpoly_plant_percentile5$plant_percentile5,
                                       CARpoly_delay_percentile95$delay_percentile95,
                                       CARpoly_harvest_percentile95$harvest_percentile95,
                                       CARpoly_plant_percentile95$plant_percentile95)
  
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_harvest_median$harvest_median"] <- "harvest_median"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_plant_median$plant_median"] <- "plant_median"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_delay_percentile5$delay_percentile5"] <- "delay_percentile5"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_harvest_percentile5$harvest_percentile5"] <- "harvest_percentile5"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_plant_percentile5$plant_percentile5"] <- "plant_percentile5"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_delay_percentile95$delay_percentile95"] <- "delay_percentile95"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_harvest_percentile95$harvest_percentile95"] <- "harvest_percentile95"
  colnames(CARpoly_tidy)[colnames(CARpoly_tidy)=="CARpoly_plant_percentile95$plant_percentile95"] <- "plant_percentile95"
  
  return(CARpoly_tidy)
}

delete_cols_CARpoly_tidy <- function(CARpoly_tidy) {
  output = CARpoly_tidy %>% subset(select = -c(double_plant_median, single_plant_median,
                                               double_harvest_median, single_harvest_median,
                                               double_plant_percentile5, single_plant_percentile5,
                                               double_harvest_percentile5, single_harvest_percentile5,
                                               double_delay_percentile5, single_delay_percentile5,
                                               double_plant_percentile95, single_plant_percentile95,
                                               double_harvest_percentile95, single_harvest_percentile95,
                                               double_delay_percentile95, single_delay_percentile95) )
  return(output)
}

rename_cols_CARpoly_untidy <- function(CARpoly_untidy) {
  output = CARpoly_untidy %>% rename(
    double_plant = double_plant_median,
    double_harvest = double_harvest_median,
    double_delay = double_delay_median,
    single_plant = single_plant_median,
    single_harvest = single_harvest_median,
    single_delay = single_delay_median
  )
  return(output)
}

delete_cols_CARpoly_untidy <- function(CARpoly_untidy) {
  output = CARpoly_untidy %>% subset(select = -c(double_plant_percentile5, single_plant_percentile5,
                                                 double_harvest_percentile5, single_harvest_percentile5,
                                                 double_delay_percentile5, single_delay_percentile5,
                                                 double_plant_percentile95, single_plant_percentile95,
                                                 double_harvest_percentile95, single_harvest_percentile95,
                                                 double_delay_percentile95, single_delay_percentile95) )
  return(output)
}

# spatial analysis
join_CARpoly_to_muni <- function(CARpoly_raw) {
  
  # turn CARpoly info into spatial points data frame
  CARpoly_spdf <- CARpoly_raw %>% filter(year > 0)
  CARpoly_spdf$latitude_1 <- CARpoly_spdf$latitude
  CARpoly_spdf$longitude_1 <- CARpoly_spdf$longitude
  
  CARpoly_spdf$poly.ids <- 1:nrow(CARpoly_spdf) 
  
  coordinates(CARpoly_spdf)<-~longitude_1+latitude_1
  crs(CARpoly_spdf) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  
  # turn spatialxxxDF into sf objects
  munis_sf <- st_as_sf(munis)
  CARpoly_sf <- st_as_sf(CARpoly_spdf)
  
  # join and rename
  CARpoly_joined <- st_join(CARpoly_sf, left = FALSE, munis_sf) 
  CARpoly_raw <- CARpoly_joined
  st_geometry(CARpoly_raw) <- NULL
  
  return(CARpoly_raw)
}