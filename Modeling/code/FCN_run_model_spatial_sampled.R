
run_OLS <- function(full_data, predictors, y.var, plant_stat,
                    grid_size, lat_offset, lon_offset, agg_scheme, 
                    plot_samples, plot_model_evals, year_oi, do_elim_year,
                    chosen_intensity) {
  
  # note, chosen_intensity is simply used to create spatial autocorrelation neighbor info, not for modeling
  
  # get sampled data
  full_data <- full_data[full_data$plant_stat_type == plant_stat,]
  sampled_data <- get_sampled_data(full_data = full_data, plant_stat = plant_stat, grid_size = grid_size, 
                                   lat_offset = lat_offset, lon_offset = lon_offset, agg_scheme = agg_scheme, 
                                   plot_samples = plot_samples, year_oi = year_oi)
  
  
  # map of the input data used (for specific year and intensity)
  if (plot_samples) {
    print(plot_cell_onset(year_oi, sampled_data))
    print(plot_cell_plant(year_oi, sampled_data, "DC"))
  }
  
  # define the formula
  formula.string <- paste(y.var, paste(predictors, collapse = " + "), sep = " ~ ")
  
  f <- as.formula(formula.string)
  
  # do the model
  
  model = lm(f, data=sampled_data) 
  sampled_data$residuals <- residuals(model)
  sampled_data$fitted.values <- fitted.values(model)
  
  # map of residuals
  if (plot_model_evals) {print(plot_cell_residuals(year_oi, sampled_data, "DC"))}
  
  # evaluation
  
  # 1. coefficients and their p values
  coefficients <- model$coefficients
  onset_coef <- coefficients["onset"]
  intensity_coef <- coefficients["intensitySC"]
  lat_coef <- coefficients["lat"]
  year_coef <- coefficients["year_index"]
  
  p_vals <- summary(model)$coefficients[, ncol(summary(model)$coefficients)]
  onset_pval <- p_vals["onset"]
  intensity_pval <- p_vals["intensitySC"]
  #lat_pval <- p_vals["lat"]
  
  std_errors <- summary(model)$coefficients[,2]
  onset_std_error <- std_errors["onset"]
  intensity_std_error <- std_errors["intensitySC"]
  lat_std_error <- std_errors["lat"]
  year_std_error <- std_errors["year_index"]
  
  # 2. percent data used
  percent_data_used <- 100*nrow(sampled_data)/nrow(full_data)
  
  # 3. R2
  SST <- sum((sampled_data$plant - mean(sampled_data$plant))^2)
  SSE <- sum((sampled_data$residuals - mean(sampled_data$residuals))^2)
  R2 <- 1 - SSE/SST
  
  # 4. exogeneity plots (residual vs predictors)
  if (plot_model_evals) {
    plot(sampled_data$onset, sampled_data$residuals, main = "onset vs residual (exogeneity)")
    #plot(sampled_data$lat, sampled_data$residuals, main = "latitude vs residual (exogeneity)")
    
    plot(sampled_data$year, sampled_data$residuals, main = "year vs residual (exogeneity)") 
    abline(h = 0)
  }
  
  # 5. spatial autocorrelation
  
  # need 'one layer': one year, one intensity, set up weights
  to_autocorrelation <- sampled_data[sampled_data$year == year_oi & sampled_data$intensity == chosen_intensity, ]
  to_autocorrelation_sp <- as(to_autocorrelation, "Spatial")
  st_geometry(to_autocorrelation) <- NULL # turn to_autocorrelation into data frame
  centroids <- coordinates(to_autocorrelation_sp)
  to_autocorrelation_points <- SpatialPointsDataFrame(coords = centroids, data = to_autocorrelation)
  nb<-knn2nb(knearneigh(to_autocorrelation_points)) 
  lw <- nb2listw(nb, zero.policy = TRUE)
  
  # calculate spatial autocorrelation
  moran_residual <- moran.mc(to_autocorrelation_points$residuals, lw, 500, zero.policy = TRUE)
  moran_onset <- moran.mc(to_autocorrelation_points$onset, lw, 500, zero.policy = TRUE)
  moran_plant <- moran.mc(to_autocorrelation_points$plant, lw, 500, zero.policy = TRUE)
  
  residual_moran <- moran_residual$statistic
  residual_moran_pval <- moran_residual$p.value
  onset_moran <- moran_onset$statistic
  onset_moran_pval <- moran_onset$p.value
  plant_moran <- moran_plant$statistic
  plant_moran_pval <- moran_plant$p.value
  
  # 6. residual plots (homoscedasticity, normality of error, residual-fitted value correlation)
  if (plot_model_evals) {
    test_plots(model, paste("grid size", grid_size, "lat offset", lat_offset, 
                            "lon_offset", lon_offset, "aggregated", agg_scheme))
    plot(sampled_data$plant, sampled_data$fitted.values, 
         main = "fitted value vs actual value of plant", ylab = "fitted.values")
    abline(h = mean(sampled_data$fitted.values), col = "blue")
    abline(v = mean(sampled_data$plant), col = "blue")
    abline(0,1, col = "gray", lwd = 3)
  }
  
  # 7. multicollinearity: look at max absolute correlation between predictors
  predictors <- sampled_data[,c("onset", "year")]
  st_geometry(predictors) <- NULL
  correlations <- cor(predictors)
  diag(correlations) <- 0 # place 1 with 0 on diagonal
  highest_predictor_correlation <- max(abs(correlations))
  
  
  # 8. predictive ability, when eliminate one year
  
  if (do_elim_year) { # set do_elim_year to FALSE if using year as fixed effect
    prediction_results_elimyear <- data.frame()
    
    for (year in 2004:2014) {
      
      result <- run_model_for_prediction_elimyear(sampled_data, year, TRUE, f)
      prediction_results_elimyear <- rbind(prediction_results_elimyear, result)
    }
    
    names(prediction_results_elimyear) <- names(result)
    
    if (plot_model_evals) {
      plot(prediction_results_elimyear$eliminated_year, prediction_results_elimyear$RMSE, 
           ylab = "RMSE", xlab = "eliminated year", ylim = c(5, 25), type = "l", 
           main = "Prediction RMSE, spatial sampling")
      
      
      # plot(prediction_results_elimyear$eliminated_year, prediction_results_elimyear$RMSE_improvement, 
      #      ylab = "RMSE", xlab = "eliminated year", ylim = c(0, 10), type = "l", 
      #      main = "Prediction RMSE improvement over intercept, spatial sampling")
      
      # plot(prediction_results_elimyear$eliminated_year, prediction_results_elimyear$onset, type = "l", col = "red", 
      #      ylab = "coef or R2", xlab = "eliminated year", ylim = c(0.1, 0.7), 
      #      main = "Prediction onset and R2, spatial sampling")
      # lines(prediction_results_elimyear$eliminated_year, prediction_results_elimyear$R2, col = "blue")
      # legend(2004, 0.65, legend = c("onset coef", "R2"), col = c("red", "blue"), lty= c(1,1))
    }
  }
  
  # 9. predictive ability, location cross validation
  
  prediction_results_elimlocation <- run_model_for_prediction_elimlocation(full_data = sampled_data,
                                                                           formula = f,
                                                                           year_for_location = 2004, year_oi = 2011,
                                                                           plot_model_evals = plot_model_evals)
  
  if (do_elim_year) {
    output_list <- list(onset_coef = onset_coef, 
                        intensity_coef = intensity_coef,
                        lat_coef = lat_coef,
                        year_coef = year_coef,
                        onset_pval = onset_pval,
                        intensity_pval = intensity_pval,
                        onset_std_error = onset_std_error,
                        intensity_std_error = intensity_std_error,
                        lat_std_error = lat_std_error,
                        year_std_error = year_std_error,
                        #lat_pval = lat_pval,
                        percent_data_used = percent_data_used,
                        R2 = R2,
                        residual_moran = residual_moran,
                        residual_moran_pval = residual_moran_pval,
                        onset_moran = onset_moran,
                        onset_moran_pval = onset_moran_pval,
                        plant_moran = plant_moran,
                        plant_moran_pval = plant_moran_pval,
                        highest_predictor_correlation = highest_predictor_correlation,
                        prediction_results_elimyear = prediction_results_elimyear,
                        prediction_results_elimlocation = prediction_results_elimlocation
    )
  }
  
  if (!do_elim_year) {
    output_list <- list(onset_coef = onset_coef, 
                        intensity_coef = intensity_coef,
                        lat_coef = lat_coef,
                        year_coef = year_coef,
                        onset_pval = onset_pval,
                        intensity_pval = intensity_pval,
                        onset_std_error = onset_std_error,
                        intensity_std_error = intensity_std_error,
                        lat_std_error = lat_std_error,
                        year_std_error = year_std_error,
                        #lat_pval = lat_pval,
                        percent_data_used = percent_data_used,
                        R2 = R2,
                        residual_moran = residual_moran,
                        residual_moran_pval = residual_moran_pval,
                        onset_moran = onset_moran,
                        onset_moran_pval = onset_moran_pval,
                        plant_moran = plant_moran,
                        plant_moran_pval = plant_moran_pval,
                        highest_predictor_correlation = highest_predictor_correlation,
                        prediction_results_elimlocation = prediction_results_elimlocation
    )
  }
  
  return(output_list)
}

###############################################################################################################################################

stepwise_model_selection <- function(max_variables, y.var, predictor_pool, data) {
  
  formula.string <- paste(y.var, paste(predictor_pool, collapse = " + "), sep = " ~ ")
  f <- as.formula(formula.string)
  
  step_result <- leaps::regsubsets(f, 
                                   data=data, 
                                   nbest=1, # number of subsets of each size to record
                                   nvmax=max_variables, # maximum size of subsets to examine
                                   force.in = c(1,2) # make sure onset and intensity are in all models
  )
  step_result_summary <- summary(step_result)
  kept_predictors <- step_result_summary$which
  
  #print(kept_predictors)
  
  # get the chosen predictors for the max number of variables; get rid of 2 index for onset and intensity
  final_predictors <- kept_predictors[max_variables - 2,] 
  final_predictors <- names(final_predictors[final_predictors])
  
  return(final_predictors)
}

###############################################################################################################################################

# cross validation by location
run_model_for_prediction_elimlocation <- function(full_data, formula, year_for_location, year_oi, plot_model_evals) {
  
  # year_oi is for mapping and visualization only
  # year_for_location is the year whose locations are looped through in cross validation
  
  # use only data points that existed in one year (2004)
  data_in_year <- full_data[full_data$year == year_for_location,]
  
  prediction_results <- data.frame()
  
  for (row_index in 1:nrow(data_in_year)) {
    test_location_sf <- data_in_year[row_index,]
    
    # turn test location into point, otherwise will get multiple cells as test location
    test_location_sp <- as(test_location_sf, "Spatial")
    st_geometry(test_location_sf) <- NULL # turn into data frame
    centroids <- coordinates(test_location_sp)
    test_location_point <- SpatialPointsDataFrame(coords = centroids, data = test_location_sf)
    test_location_sf <- st_as_sf(test_location_point)
    st_crs(test_location_sf) <- st_crs(full_data)
    
    # get test data for all years
    test_sgbp = st_intersects(x = full_data, y = test_location_sf)
    test_logical = lengths(test_sgbp) > 0
    test_data = full_data[test_logical, ]
    
    # get train data for all years 
    train_data <- full_data[test_location_sf, , op = st_disjoint]
    
    model = lm(formula, data=train_data) 
    model_intercept = lm(plant ~ 1, data=train_data)
    
    # extract onset coef and R2 from model
    onset_coef <- model$coefficients['onset']
    train_data$residuals <- residuals(model)
    SST <- sum((train_data$plant - mean(train_data$plant))^2)
    SSE <- sum((train_data$residuals - mean(train_data$residuals))^2)
    R2 <- 1 - SSE/SST
    
    # prediction
    prediction <- predict(model, test_data)
    RMSE <- rmse(prediction, test_data$plant)
    error <- mean(prediction - test_data$plant, na.rm = TRUE)
    
    prediction_intercept <- predict(model_intercept, test_data)
    error_intercept <- rmse(prediction_intercept, test_data$plant)
    
    diff_RMSE <- error_intercept - RMSE
    
    # output
    output <- c(onset_coef, R2, RMSE, diff_RMSE, error)
    names(output) <- c("onset", "R2", "RMSE", "RMSE_improvement", "error")
    
    prediction_results <- rbind(prediction_results, output)
    
    # plot train and test data for a specific year
    
    # test_year <- test_data[test_data$year == year_oi & test_data$intensity == "DC", ]
    # ggplot(test_year) +
    #    geom_sf(aes(fill = plant)) +
    #    scale_fill_viridis() +
    #    ggtitle(paste("test data for ", year_oi)) +
    #    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    #    theme_bw()
    # 
    # train_year <- train_data[train_data$year == year_oi & train_data$intensity == "DC", ]
    # ggplot(train_year) +
    #    geom_sf(aes(fill = plant)) +
    #    scale_fill_viridis() +
    #    ggtitle(paste("train data for ", year_oi)) +
    #    geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
    #    theme_bw()
  }
  
  names(prediction_results) <- names(output)
  prediction_results$index <- 1:nrow(prediction_results)
  
  if (plot_model_evals) {
    plot(prediction_results$index, prediction_results$RMSE, 
         ylab = "RMSE", xlab = "location index", ylim = c(5, 25), type = "l", main = "Prediction RMSE, spatial sampling")
    
    plot(prediction_results$index, prediction_results$error, 
         ylab = "error", xlab = "location index", ylim = c(-25, 25), type = "l", main = "Prediction error, spatial sampling")
    
    # plot(prediction_results$index, prediction_results$RMSE_improvement, 
    #      ylab = "RMSE", xlab = "location index", ylim = c(0, 10), type = "l", main = "Prediction RMSE improvement over intercept, spatial sampling")
    # 
    # plot(prediction_results$index, prediction_results$onset, type = "l", col = "red", ylab = "coef or R2", xlab = "location index", ylim = c(0.1, 0.7), main = "Prediction onset and R2, spatial sampling")
    # lines(prediction_results$index, prediction_results$R2, col = "blue")
    # legend(1, 0.65, legend = c("onset coef", "R2"), col = c("red", "blue"), lty= c(1,1))
  }
  
  # map of prediction error
  
  data_in_year$prediction_rmse <- prediction_results$RMSE
  data_in_year$prediction_error <- prediction_results$error
  data_in_year_DC <- data_in_year[data_in_year$intensity == "DC", ]
  
  if (plot_model_evals) {
    rmse_map <- ggplot(data_in_year_DC) +
      geom_sf(aes(fill = prediction_rmse)) +
      scale_fill_viridis() +
      ggtitle("prediction rmse") +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    print(rmse_map)
    
    error_map <- ggplot(data_in_year_DC) +
      geom_sf(aes(fill = prediction_error)) +
      scale_fill_viridis() +
      ggtitle("prediction error") +
      geom_polygon(data = MT_outline, aes(x = long, y = lat), color = "black", alpha = 0, linetype = 1) +
      theme_bw()
    print(error_map)
  }
  
  return(prediction_results)
  
}

##############################################################################################################################################

# runs the model with options to eliminate a year
run_model_for_prediction_elimyear <- function(full_data, year_to_elim, elim_year, formula) {
  
  
  # separate training and test data
  if (elim_year) {
    train_data <- full_data[full_data$year != year_to_elim,]
    test_data <- full_data[full_data$year == year_to_elim,]
  }
  else {
    train_data <- full_data
    test_data <- full_data
  }
  
  model = lm(formula, data=train_data) 
  model_intercept = lm(plant ~ 1, data=train_data)
  
  # extract onset coef and R2 from model
  onset_coef <- model$coefficients['onset']
  train_data$residuals <- residuals(model)
  SST <- sum((train_data$plant - mean(train_data$plant))^2)
  SSE <- sum((train_data$residuals - mean(train_data$residuals))^2)
  R2 <- 1 - SSE/SST
  
  # prediction
  prediction <- predict(model, test_data)
  error <- rmse(prediction, test_data$plant)
  
  prediction_intercept <- predict(model_intercept, test_data)
  error_intercept <- rmse(prediction_intercept, test_data$plant)
  
  diff_error <- error_intercept - error
  
  # output
  output <- c(as.integer(year_to_elim), onset_coef, R2, error, diff_error)
  names(output) <- c("eliminated_year", "onset", "R2", "RMSE", "RMSE_improvement")
  return(output)
}