# Load packages
library(purrr)
library(dplyr)

# Prepare data
  # Create a dataframe: Price, Quantity, Marginal cost
  p_q_mc_obs <- function(prod_clus_data, prod_data, i) {
    # Price response
    p_q <- prod_clus_data %>%
      # Round prices
      mutate(price = round(price, 1)) %>% 
      # Aggregate quantity for a given price
      count(price, wt = quantity) 
    # Marginal cost
    mc <- prod_data %>%
      summarise(marginal_cost = round(mean(cost), 1)) %>% 
      as.numeric()
    # Combine price, quantity, marginal cost
    return(cbind(p_q, 
                 marginal_cost = rep(mc, nrow(p_q)),
                 cluster = rep(i, nrow(p_q))))
  }
  # Result
    # Health and beauty
    list_hb_data <- map(1:3, 
                        ~ list_prod_data_kmeans_clustered[["Health and beauty"]] %>% 
                          filter(cluster == .x))
    list_hb_data_p_q_mc <- map2(list_hb_data, 1:3, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Health and beauty"]], 
                                                                .y))
    list_hb_data_p_q_mc <- list_hb_data_p_q_mc %>% set_names(c('hb_cluster_1',
                                                               'hb_cluster_2',
                                                               'hb_cluster_3'))
    # Electronic accessories
    list_ea_data <- map(1:2, 
                        ~ list_prod_data_kmeans_clustered[["Electronic accessories"]] %>% 
                          filter(cluster == .x))
    list_ea_data_p_q_mc <- map2(list_ea_data, 1:2, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Electronic accessories"]], 
                                                                .y))
    list_ea_data_p_q_mc <- list_ea_data_p_q_mc %>% set_names(c('ea_cluster_1',
                                                               'ea_cluster_2'))
    # Home and lifestyle
    list_hl_data <- map(1:4, 
                        ~ list_prod_data_kmeans_clustered[["Home and lifestyle"]] %>% 
                          filter(cluster == .x))
    list_hl_data_p_q_mc <- map2(list_hl_data, 1:4, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Home and lifestyle"]], 
                                                                .y))
    list_hl_data_p_q_mc <- list_hl_data_p_q_mc %>% set_names(c('hl_cluster_1',
                                                               'hl_cluster_2',
                                                               'hl_cluster_3',
                                                               'hl_cluster_4'))
    # Sports and travel
    list_st_data <- map(1:3, 
                        ~ list_prod_data_kmeans_clustered[["Sports and travel"]] %>% 
                          filter(cluster == .x))
    list_st_data_p_q_mc <- map2(list_st_data, 1:3, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Sports and travel"]], 
                                                                .y))
    list_st_data_p_q_mc <- list_st_data_p_q_mc %>% set_names(c('st_cluster_1',
                                                               'st_cluster_2',
                                                               'st_cluster_3'))
    # Food and beverages
    list_fb_data <- map(1:3, 
                        ~ list_prod_data_kmeans_clustered[["Food and beverages"]] %>% 
                          filter(cluster == .x))
    list_fb_data_p_q_mc <- map2(list_fb_data, 1:3, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Food and beverages"]], 
                                                                .y))
    list_fb_data_p_q_mc <- list_fb_data_p_q_mc %>% set_names(c('fb_cluster_1',
                                                               'fb_cluster_2',
                                                               'fb_cluster_3'))
    # Fashion accessories
    list_fa_data <- map(1:3, 
                        ~ list_prod_data_kmeans_clustered[["Fashion accessories"]] %>% 
                          filter(cluster == .x))
    list_fa_data_p_q_mc <- map2(list_fa_data, 1:3, ~ p_q_mc_obs(.x, 
                                                                list_prod_data_kmeans_clustered[["Fashion accessories"]], 
                                                                .y))
    list_fa_data_p_q_mc <- list_fa_data_p_q_mc %>% set_names(c('fa_cluster_1',
                                                               'fa_cluster_2',
                                                               'fa_cluster_3'))

# Split data
  # Shuffle the order of rows
  data_reshuffled <- function(data) {data %>% sample_frac(size = 1, replace = FALSE)}
  # Train set
  data_train <- function(data_reshuffled) {data_reshuffled %>% slice(1:round(nrow(.) * 0.7))}
  # Test set
  data_test <- function(data_reshuffled) {data_reshuffled %>% slice((round(nrow(.) * 0.7) + 1):nrow(.))}
  # Result
    # Health and beauty
    set.seed(100)
    list_hb_reshuffled_data <- map(list_hb_data_p_q_mc, ~ data_reshuffled(.x))
    list_hb_train_data <- map(list_hb_reshuffled_data, ~ data_train(.x))
    list_hb_test_data <- map(list_hb_reshuffled_data, ~ data_test(.x))
    # Electronic accessories
    set.seed(110)
    list_ea_reshuffled_data <- map(list_ea_data_p_q_mc, ~ data_reshuffled(.x))
    list_ea_train_data <- map(list_ea_reshuffled_data, ~ data_train(.x))
    list_ea_test_data <- map(list_ea_reshuffled_data, ~ data_test(.x))
    # Home and lifestyle
    set.seed(120)
    list_hl_reshuffled_data <- map(list_hl_data_p_q_mc, ~ data_reshuffled(.x))
    list_hl_train_data <- map(list_hl_reshuffled_data, ~ data_train(.x))
    list_hl_test_data <- map(list_hl_reshuffled_data, ~ data_test(.x))
    # Sports and travel
    set.seed(130)
    list_st_reshuffled_data <- map(list_st_data_p_q_mc, ~ data_reshuffled(.x))
    list_st_train_data <- map(list_st_reshuffled_data, ~ data_train(.x))
    list_st_test_data <- map(list_st_reshuffled_data, ~ data_test(.x))
    # Food and beverages 
    set.seed(140)
    list_fb_reshuffled_data <- map(list_fb_data_p_q_mc, ~ data_reshuffled(.x))
    list_fb_train_data <- map(list_fb_reshuffled_data, ~ data_train(.x))
    list_fb_test_data <- map(list_fb_reshuffled_data, ~ data_test(.x))
    # Fashion accessories
    set.seed(150)
    list_fa_reshuffled_data <- map(list_fa_data_p_q_mc, ~ data_reshuffled(.x))
    list_fa_train_data <- map(list_fa_reshuffled_data, ~ data_train(.x))
    list_fa_test_data <- map(list_fa_reshuffled_data, ~ data_test(.x))

# Fit model: Model the price-response relationships
  # Model-level object
    # Cross validation: Choose the optimal number of degrees
    library(caret)
    fit_control = trainControl(method = "repeatedcv",
                               number = 5,
                               repeats = 5)
    degree <- 1:5
    r_sq <- rep(0, 5)
    rmse <- rep(0, 5)
    poly_models_cv <- function(train_data) {
      for(d in degree) {
        poly_reg <- bquote(n ~ poly(price, .(d)))
        poly_models <- train(as.formula(poly_reg), 
                             data = train_data, 
                             method = 'lm', 
                             trControl = fit_control)
        r_sq[d] <- poly_models$results$Rsquared
        rmse[d] <- poly_models$results$RMSE
      } 
      return(data.frame(degree, r_sq, rmse))
    }
    # Model functions
      # One-liner function
      lr_model_mod <- function(train_data) {
        map(train_data, function(.x) map(1:5, function(.y) lm(n ~ poly(price, .y), data = .x)))
      }
      # Classical function
      nested_model_loop <- function(x) {
        map(1:5, function(y) {
          lm(n ~ poly(price, y), data = x)
        })
      }
      lr_model_mod <- function(train_data) {map(train_data, ~ nested_model_loop(.x))}
  # Observation-level object 
  library(moderndive)
  lr_model_obs <- function(model_object, test_data) {
    df <- model_object %>%
      get_regression_points(newdata = test_data) %>%
      mutate(marginal_cost = test_data$marginal_cost,
             profit = n * (price - marginal_cost), 
             profit_fitted = n_hat * (price - marginal_cost)) %>%
      mutate(rmse_demand = sqrt(mean(residual ^ 2)),
             rmse_profit = sqrt(mean((profit - profit_fitted) ^ 2))) 
    p_max_profit <- df %>%
      subset(profit_fitted == max(profit_fitted), price) %>% 
      as.numeric() 
    df %>%  
      mutate(optimal_price = p_max_profit)
  }
  # Price-response plot
  plot_price_response <- function(lr_model_obs, formula = formula) {
    ggplot(lr_model_obs, aes(x = price, y = n)) +
      geom_point() + 
      geom_smooth(method = 'lm', formula = formula, se = FALSE) +
      labs(x = "Price", y = "Demand (Response)") +
      theme_bw()
  }
  # Price-profit plot
  plot_price_profit <- function(lr_model_obs) {
    ggplot(lr_model_obs, aes(x = price, y = profit)) + 
      geom_point() +
      geom_line(aes(x = price, y = profit_fitted), color = 'blue') +
      geom_vline(xintercept = as.numeric(subset(lr_model_obs, profit_fitted == max(profit_fitted), price)),
                 color = 'red', 
                 lty = 'dashed') + 
      geom_hline(yintercept = 0, lty = 'dotted') +
      labs(x = "Price", y = "Profit") +
      theme_bw()
  }
  # Combine the plots
  library(gridExtra)
  plots_combined <- function(plot1, plot2) {
    grid.arrange(plot1, plot2, nrow = 2, ncol = 1)
  }
  # Total profit
  total_profit <- function(cluster1, cluster2 = 0, cluster3 = 0, cluster4 = 0, cluster5 = 0) {
    seq <- c( max(cluster1), max(cluster2), max(cluster3), max(cluster4), max(cluster5) )
    return(sum(seq))
  }
  # Results
    # Health and beauty
    list_hb_cv <- map(list_hb_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_hb_models <- lr_model_mod(list_hb_train_data)
      ## Cluster 1: Optimal degree = 1
      # Predictions
      hb_cluster_1_model <- lm(n ~ poly(price, 1), data = list_hb_train_data$hb_cluster_1) 
      hb_cluster_1_pred <- lr_model_obs(hb_cluster_1_model, list_hb_test_data$hb_cluster_1)
      # Plot
      plots_combined(plot_price_response(hb_cluster_1_pred, formula = y ~ x), 
                     plot_price_profit(hb_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions
      hb_cluster_2_model <- lm(n ~ poly(price, 1), data = list_hb_train_data$hb_cluster_2) 
      hb_cluster_2_pred <- lr_model_obs(hb_cluster_2_model, list_hb_test_data$hb_cluster_2)
      # Plot
      plots_combined(plot_price_response(hb_cluster_2_pred, formula = y ~ x), 
                     plot_price_profit(hb_cluster_2_pred))
      ## Cluster 3: Optimal degree = 1
      # Predictions
      hb_cluster_3_model <- lm(n ~ poly(price, 1), data = list_hb_train_data$hb_cluster_3) 
      hb_cluster_3_pred <- lr_model_obs(hb_cluster_3_model, list_hb_test_data$hb_cluster_3)
      # Plot
      plots_combined(plot_price_response(hb_cluster_3_pred, formula = y ~ x), 
                     plot_price_profit(hb_cluster_3_pred))
      ## Total profit
      total_profit(hb_cluster_1_pred$profit_fitted, 
                   hb_cluster_2_pred$profit_fitted, 
                   hb_cluster_3_pred$profit_fitted)
    # Electronic accessories
    list_ea_cv <- map(list_ea_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_ea_models <- lr_model_mod(list_ea_train_data)
      ## Cluster 1: Optimal degree = 4
      # Predictions
      ea_cluster_1_model <- lm(n ~ poly(price, 4), data = list_ea_train_data$ea_cluster_1) 
      ea_cluster_1_pred <- lr_model_obs(ea_cluster_1_model, list_ea_test_data$ea_cluster_1)
      # Plot
      plots_combined(plot_price_response(ea_cluster_1_pred, formula = y ~ poly(x, 4)), 
                     plot_price_profit(ea_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions
      ea_cluster_2_model <- lm(n ~ poly(price, 1), data = list_ea_train_data$ea_cluster_2) 
      ea_cluster_2_pred <- lr_model_obs(ea_cluster_2_model, list_ea_test_data$ea_cluster_2)  
      # Plot
      plots_combined(plot_price_response(ea_cluster_2_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(ea_cluster_2_pred))
      ## Total profit
      total_profit(ea_cluster_1_pred$profit_fitted, 
                   ea_cluster_2_pred$profit_fitted)
    # Home and lifestyle
    list_hl_cv <- map(list_hl_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_hl_models <- lr_model_mod(list_hl_train_data)
      ## Cluster 1: Optimal degree = 2
      # Predictions
      hl_cluster_1_model <- lm(n ~ poly(price, 2), data = list_hl_train_data$hl_cluster_1) 
      hl_cluster_1_pred <- lr_model_obs(hl_cluster_1_model, list_hl_test_data$hl_cluster_1)    
      # Plot
      plots_combined(plot_price_response(hl_cluster_1_pred, formula = y ~ poly(x, 2)), 
                     plot_price_profit(hl_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions 
      hl_cluster_2_model <- lm(n ~ poly(price, 1), data = list_hl_train_data$hl_cluster_2) 
      hl_cluster_2_pred <- lr_model_obs(hl_cluster_2_model, list_hl_test_data$hl_cluster_2)      
      # Plot
      plots_combined(plot_price_response(hl_cluster_2_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(hl_cluster_2_pred))
      ## Cluster 3: Optimal degree = 1
      # Predictions
      hl_cluster_3_model <- lm(n ~ poly(price, 1), data = list_hl_train_data$hl_cluster_3) 
      hl_cluster_3_pred <- lr_model_obs(hl_cluster_3_model, list_hl_test_data$hl_cluster_3)     
      # Plot
      plots_combined(plot_price_response(hl_cluster_3_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(hl_cluster_3_pred))
      ## Cluster 4: Optimal degree = 3
      # Predictions
      hl_cluster_4_model <- lm(n ~ poly(price, 3), data = list_hl_train_data$hl_cluster_4) 
      hl_cluster_4_pred <- lr_model_obs(hl_cluster_4_model, list_hl_test_data$hl_cluster_4)       
      # Plot
      plots_combined(plot_price_response(hl_cluster_4_pred, formula = y ~ poly(x, 3)), 
                     plot_price_profit(hl_cluster_4_pred))
      ## Total profit
      total_profit(hl_cluster_1_pred$profit_fitted, 
                   hl_cluster_2_pred$profit_fitted, 
                   hl_cluster_3_pred$profit_fitted,
                   hl_cluster_4_pred$profit_fitted)
    # Sports and travel
    list_st_cv <- map(list_st_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_st_models <- lr_model_mod(list_st_train_data)
      ## Cluster 1: Optimal degree = 4
      # Predictions
      st_cluster_1_model <- lm(n ~ poly(price, 4), data = list_st_train_data$st_cluster_1) 
      st_cluster_1_pred <- lr_model_obs(st_cluster_1_model, list_st_test_data$st_cluster_1)         
      # Plot
      plots_combined(plot_price_response(st_cluster_1_pred, formula = y ~ poly(x, 4)), 
                     plot_price_profit(st_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions
      st_cluster_2_model <- lm(n ~ poly(price, 1), data = list_st_train_data$st_cluster_2) 
      st_cluster_2_pred <- lr_model_obs(st_cluster_2_model, list_st_test_data$st_cluster_2)          
      # Plot
      plots_combined(plot_price_response(st_cluster_2_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(st_cluster_2_pred))
      ## Cluster 3: Optimal degree = 1
      # Predictions
      st_cluster_3_model <- lm(n ~ poly(price, 1), data = list_st_train_data$st_cluster_3) 
      st_cluster_3_pred <- lr_model_obs(st_cluster_3_model, list_st_test_data$st_cluster_3)            
      # Plot
      plots_combined(plot_price_response(st_cluster_3_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(st_cluster_3_pred))
      ## Total profit
      total_profit(st_cluster_1_pred$profit_fitted, 
                   st_cluster_2_pred$profit_fitted, 
                   st_cluster_3_pred$profit_fitted)
    # Food and beverages
    list_fb_cv <- map(list_fb_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_fb_models <- lr_model_mod(list_fb_train_data)
      ## Cluster 1: Optimal degree = 2
      # Predictions
      fb_cluster_1_model <- lm(n ~ poly(price, 2), data = list_fb_train_data$fb_cluster_1) 
      fb_cluster_1_pred <- lr_model_obs(fb_cluster_1_model, list_fb_test_data$fb_cluster_1)              
      # Plot
      plots_combined(plot_price_response(fb_cluster_1_pred, formula = y ~ poly(x, 2)), 
                     plot_price_profit(fb_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions
      fb_cluster_2_model <- lm(n ~ poly(price, 1), data = list_fb_train_data$fb_cluster_2) 
      fb_cluster_2_pred <- lr_model_obs(fb_cluster_2_model, list_fb_test_data$fb_cluster_2)    
      # Plot
      plots_combined(plot_price_response(fb_cluster_2_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(fb_cluster_2_pred))
      ## Cluster 3: Optimal degree = 1
      # Predictions
      fb_cluster_3_model <- lm(n ~ poly(price, 1), data = list_fb_train_data$fb_cluster_3) 
      fb_cluster_3_pred <- lr_model_obs(fb_cluster_3_model, list_fb_test_data$fb_cluster_3)      
      # Plot
      plots_combined(plot_price_response(fb_cluster_3_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(fb_cluster_3_pred))
      ## Total profit
      total_profit(fb_cluster_1_pred$profit_fitted, 
                   fb_cluster_2_pred$profit_fitted, 
                   fb_cluster_3_pred$profit_fitted)
    # Fashion accessories
    list_fa_cv <- map(list_fa_train_data, ~ poly_models_cv(.x) %>% round(2))
    list_fa_models <- lr_model_mod(list_fa_train_data)
      ## Cluster 1: Optimal degree = 1
      # Predictions
      fa_cluster_1_model <- lm(n ~ poly(price, 1), data = list_fa_train_data$fa_cluster_1) 
      fa_cluster_1_pred <- lr_model_obs(fa_cluster_1_model, list_fa_test_data$fa_cluster_1)      
      # Plot
      plots_combined(plot_price_response(fa_cluster_1_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(fa_cluster_1_pred))
      ## Cluster 2: Optimal degree = 1
      # Predictions
      fa_cluster_2_model <- lm(n ~ poly(price, 1), data = list_fa_train_data$fa_cluster_2) 
      fa_cluster_2_pred <- lr_model_obs(fa_cluster_2_model, list_fa_test_data$fa_cluster_2)        
      # Plot
      plots_combined(plot_price_response(fa_cluster_2_pred, formula = y ~ poly(x, 1)), 
                     plot_price_profit(fa_cluster_2_pred))
      ## Cluster 3: Optimal degree = 3
      # Predictions
      fa_cluster_3_model <- lm(n ~ poly(price, 3), data = list_fa_train_data$fa_cluster_3) 
      fa_cluster_3_pred <- lr_model_obs(fa_cluster_3_model, list_fa_test_data$fa_cluster_3)         
      # Plot
      plots_combined(plot_price_response(fa_cluster_3_pred, formula = y ~ poly(x, 3)), 
                     plot_price_profit(fa_cluster_3_pred))
      ## Total profit
      total_profit(fa_cluster_1_pred$profit_fitted, 
                   fa_cluster_2_pred$profit_fitted, 
                   fa_cluster_3_pred$profit_fitted)