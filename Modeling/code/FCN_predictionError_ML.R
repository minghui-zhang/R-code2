test_elimyear <- function(elim_year, cell_df, cell_sf, f, model_type) {
  
  print('model_type')
  print(model_type)
  
  # eliminate a year and test it; model_type is ols or rf
  train_elimyear <- cell_df[cell_df$year != elim_year,]
  valid_elimyear <- cell_df[cell_df$year == elim_year,]
  cell_sf_elimyear <- cell_sf[cell_sf$year == elim_year,]
  
  # fit model
  if (model_type == "rf") {
    model <- randomForest(f, data = train_elimyear, importance = TRUE, ntree = 500, mtry = 2)
  }
  
  if (model_type == "ols") {
    model <- lm(f, data = train_elimyear)
  }
  
  
  if (model_type == "svm") {
    model <- svm(f, data = train_elimyear)
  }
  
  if (model_type == "boost") {
    model <- gbm(f, data = train_elimyear, distribution = "gaussian", 
                 n.trees = 5000, interaction.depth = 2, shrinkage = 0.01)
  }
  
  if (model_type != "boost") {
    valid_elimyear$predicted_plant <- predict(model, valid_elimyear)
  }
  
  if (model_type == "boost") {
    valid_elimyear$predicted_plant <- predict(model, valid_elimyear, n.trees = 5000)
  }
  
  plot(valid_elimyear$plant, valid_elimyear$predicted_plant, main = paste('eliminated', elim_year, model_type))
  abline(a = 0, b = 1)
  
  rmse_elimyear <- rmse(valid_elimyear$plant, valid_elimyear$predicted_plant)
  error_elimyear <- mean(valid_elimyear$predicted_plant - valid_elimyear$plant)
  
  # map error
  # cell_sf_elimyear$error <- valid_elimyear$predicted_plant - valid_elimyear$plant
  # elimyear_error_map <- ggplot(cell_sf_elimyear) +
  #   geom_sf(aes(fill = error)) +
  #   scale_fill_viridis() +
  #   ggtitle(paste("Plant prediction error, eliminated ", elim_year, model_type)) +
  #   geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
  #   theme_bw()
  # print(elimyear_error_map)
  
  print('year done')
  print(elim_year)
  
  return(data.frame(elim_year = c(elim_year),
                    rmse_elimyear = c(rmse_elimyear),
                    error_elimyear = c(error_elimyear)))
  
}

test_elimcell <- function(elim_cell, cell_df, cell_sf, model_type, f) {
  
  print('model_type')
  print(model_type)
  train_elimcell <- cell_df[cell_df$cell_ID != elim_cell,]
  valid_elimcell <- cell_df[cell_df$cell_ID == elim_cell,]
  
  # for mapping
  cell_sf_elimcell <- cell_sf[cell_sf$cell_ID == elim_cell, ]
  
  # fit model
  if (model_type == "rf") {
    model <- randomForest(f, data = train_elimcell, importance = TRUE, ntree = 600, mtry = 2)
  }
  
  if (model_type == "ols") {
    model <- lm(f, data = train_elimcell)
  }
  
  if (model_type == "svm") {
    model <- svm(f, data = train_elimcell)
  }
  
  if (model_type == "boost") {
    model <- gbm(f, data = train_elimcell, distribution = "gaussian", 
                 n.trees = 5000, interaction.depth = 2, shrinkage = 0.01)
  }
  
  if (model_type != "boost") {
    valid_elimcell$predicted_plant <- predict(model, valid_elimcell)
  }
  
  if (model_type == "boost") {
    valid_elimcell$predicted_plant <- predict(model, valid_elimcell, n.trees = 5000)
  }
  
  #plot(valid_elimcell$plant, valid_elimcell$predicted_plant, main = paste('eliminated cell', elim_cell, model_type))
  #abline(a = 0, b = 1)
  
  rmse_elimcell <- rmse(valid_elimcell$plant, valid_elimcell$predicted_plant)
  error_elimcell <- mean(valid_elimcell$predicted_plant - valid_elimcell$plant)
  
  print('cell done')
  print(elim_cell)
  
  return(data.frame(elim_cell = c(elim_cell),
                    rmse_elimcell = c(rmse_elimcell),
                    error_elimcell = c(error_elimcell)))
  
}
