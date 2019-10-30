# Spectral comparison of soy vs nonsoy agri


library(ggplot2)
setwd("~/Documents/R-code/LandCover/Soy-vs-Nonsoy-Agri")
#agsat <- read.csv("modis_table_agsat_partial.csv")
#plos <- read.csv("modis_table_plos_partial.csv")



# turns date in millis into date
ms_to_date = function(ms, t0="1970-01-01", timezone) {
  sec = ms / 1000
  as.POSIXct(sec, origin=t0, tz=timezone)
}

# turn class into a factor
agsat$class <- as.factor(agsat$class)

agsat_2010 <- agsat[agsat$year == 2010,]

# turn system.time_start into date, month, day
agsat_2010$date <- ms_to_date(agsat$system.time_start)

initial_plot <- ggplot(data = agsat_2010, aes(x = sur_refl_b01, y = sur_refl_b02, col = class)) +
                  geom_point(alpha = 0.1)

print(initial_plot)