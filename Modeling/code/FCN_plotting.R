# test the models
test_plots <- function(model, model_name) {
  
  plot(model$residuals, main = paste("residual vs index", model_name))
  
  plot(model$fitted.values, model$residuals, main = paste("fitted values vs residuals", model_name))
  
  # normal probability plot
  qqnorm(model$residuals, pch = 1, frame = FALSE, main = model_name)
  qqline(model$residuals, col = "steelblue", lwd = 2)
}

# plot data
plot_cell_onset <- function(year, cell_data) {
  cell_year <- cell_data[cell_data$year == year, ]
  
  ggplot(cell_year) +
    geom_sf(aes(fill = onset), colour = NA) +
    scale_fill_viridis() +
    ggtitle(paste("Onset for spatial sampling", year)) +
    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    theme_bw()
}

plot_cell_residuals <- function(year, cell_data, intensity) {
  cell_year <- cell_data[cell_data$year == year, ]
  
  ggplot(cell_year) +
    geom_sf(aes(fill = residuals), colour = NA) +
    scale_fill_viridis() +
    ggtitle(paste(intensity, "residuals for spatial sampling", year)) +
    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    theme_bw()
}

plot_cell_plant <- function(year, cell_data, intensity) {
  cell_year <- cell_data[cell_data$year == year, ]
  
  ggplot(cell_year) +
    geom_sf(aes(fill = plant), colour = NA) +
    scale_fill_viridis() +
    ggtitle(paste(intensity, "plant for spatial sampling", year)) +
    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    theme_bw()
}

plot_cell_tempAuto <- function(year, cell_data, intensity) {
  cell_year <- cell_data[cell_data$year == year, ]
  
  ggplot(cell_year) +
    geom_sf(aes(fill = dwi.autocorr.p.value), colour = NA) +
    scale_fill_viridis() +
    ggtitle(paste(intensity, "signif temporal autocorrelation for spatial panel", year)) +
    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    theme_bw()
}