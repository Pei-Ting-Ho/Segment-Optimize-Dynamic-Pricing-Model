# Load packages
library(tidyverse)
library(lubridate)
library(janitor)

# Preprocess data
  # Consistent column names
  sales_data <- sales_data %>% clean_names(case = "snake")
  # Date/Time variables
  sales_data <- sales_data %>% mutate(date = mdy(date), time = hms(time),
                                      weekday = wday(date),
                                      weekend = if_else(between(weekday, 2, 6), FALSE, TRUE),
                                      morning = am(time))
  # Continuous variables
  sales_data <- sales_data %>% mutate(price = unit_price * 1.05,
                                      cost = cogs / quantity)
  # Factor variables
  sales_data <- sales_data %>% mutate(customer_type = as.factor(customer_type),
                                      gender = as.factor(gender),
                                      product_line = as.factor(product_line),
                                      payment = as.factor(payment),
                                      weekday = as.factor(weekday),
                                      weekend = as.factor(weekend),
                                      morning = as.factor(morning))

# Encode values
  # Data preparation: Data selected for each product category
  prod_names <- c("Health and beauty", "Electronic accessories", "Home and lifestyle",
                  "Sports and travel", "Food and beverages", "Fashion accessories")
  data_selected <- function(x) {
    sales_data %>%
      filter(product_line == x) %>%
      as.data.frame()
  }
  list_prod_data <- map(prod_names, ~ data_selected(.x)) %>% 
    set_names(prod_names)

  # Method_1: Hierarchical clustering (Binary dummification)
    # Binary-encoded data
      # Dummification: The binary-encoded function
      library(dummies)
      data_bi_encoded <- function(data) {
        data %>%
          select(customer_type, gender, payment, weekend, morning) %>%
          dummy.data.frame(sep = "_")
      }
      # Result
      list_prod_data_bi_encoded <- map(list_prod_data, ~ data_bi_encoded(.x))
    # Hierarchical clustering
      # Cluster operation
      model_hclust <- function(bi_encoded_data) {
        bi_encoded_data %>%
          dist(method = "binary") %>%
          hclust(method = "complete") 
      }
      # Result
        # Health and beauty: k = 3 (0.9) or k = 6 (0.8)
        # Electronic accessories: k = 3 (0.9) or k = 5 (0.8)
        # Home and lifestyle: k = 4 (0.9) or k = 8 (0.8)
        # Sports and travel: k = 4 (0.9) or k = 6 (0.8)
        # Food and beverages: k = 3 (0.9) or k = 6 (0.8)
        # Fashion accessories: k = 2 (0.9) or k = 5 (0.8)
        dend_plot <- map(list_prod_data_bi_encoded, ~ model_hclust(.x) %>% plot()) 
        walk(dend_plot, print)
      # Cluster assignment
      hc_k_3 <- partial(cutree, k = 3)
      hc_k_4 <- partial(cutree, k = 4)
      hc_cluster_assign_3 <- compose(hc_k_3, model_hclust)
      hc_cluster_assign_4 <- compose(hc_k_4, model_hclust)
      # Result
      set.seed(10)
      hc_prod_cluster <- map_if(list_prod_data_bi_encoded, 
                                names(list_prod_data_bi_encoded) == "Home and lifestyle", 
                                ~ hc_cluster_assign_4(.x), 
                                .else = ~ hc_cluster_assign_3(.x))
      list_prod_data_hc_clustered <- map2(list_prod_data, 
                                          hc_prod_cluster, 
                                          ~ .x %>% mutate(cluster = .y))

  # Method_2: K-means (Frequency-based dummification) > K-means (Binary dummification) 
    # Nested loop with map function
      # Lambda function
      map(1:2, function(.x) map(1:3, function(.y) paste(.x, .y, sep = "-")))
      # Classical function
      nested_loop <- function(x) {
        map(1:3, function(y) {
          paste(x, y, sep = '-')
        })
      }
      map(1:2, ~ nested_loop(.x))
    # Frequency-based encoded data
      # Customer type: Member; Normal 
      # Gender: Female; Male 
      # Payment: Cash; Credit card; Ewallet 
      # Weekend: True; False 
      # Morning: True; False 
      customer_type <- function(data) {
        data %>% 
          count(customer_type) %>% 
          mutate(fraction = n / sum(n)) %>% 
          as.data.frame()
      }

      gender <- function(data) {
        data %>% 
          count(gender) %>% 
          mutate(fraction = n / sum(n)) %>% 
          as.data.frame()
      }

      payment <- function(data) {
        data %>% 
          count(payment) %>% 
          mutate(fraction = n / sum(n)) %>% 
          as.data.frame()
      }

      weekend <- function(data) {
        data %>% 
          count(weekend) %>% 
          mutate(fraction = n / sum(n)) %>% 
          as.data.frame()
      }

      morning <- function(data) {
        data %>% 
          count(morning) %>% 
          mutate(fraction = n / sum(n)) %>% 
          as.data.frame()
      }

      data_freq_encoded <- function(data) {
        data %>% 
          transmute(customer_type = recode(customer_type, 
                                           "Member" = customer_type(data)[1, 3], 
                                           "Normal" = customer_type(data)[2, 3]),
                    gender = recode(gender, 
                                    "Female" = gender(data)[1, 3],
                                    "Male" = gender(data)[2, 3]),
                    payment = recode(payment,
                                     "Cash" = payment(data)[1, 3],                                     
                                     "Credit card" = payment(data)[2, 3],
                                     "Ewallet" = payment(data)[3, 3]),
                    weekend = recode(weekend,
                                     "FALSE" = weekend(data)[1, 3],
                                     "TRUE" = weekend(data)[2, 3]),
                    morning = recode(morning,
                                     "FALSE" = morning(data)[1, 3],
                                     "TRUE" = morning(data)[2, 3])) 
      }
      # Result
      list_prod_data_freq_encoded <- map(list_prod_data, ~ data_freq_encoded(.x))
    # K-means clustering
      # Visualize k-means clustering
      library(factoextra)
      library(gridExtra)
      visual_assess <- function(data) {
        p1 <- kmeans(data, centers = 2) %>%
          fviz_cluster(geom = 'point', data = data) + ggtitle('k = 2') + theme_bw()
        p2 <- kmeans(data, centers = 3) %>%
          fviz_cluster(geom = 'point', data = data) + ggtitle('k = 3') + theme_bw()
        p3 <- kmeans(data, centers = 4) %>%
          fviz_cluster(geom = 'point', data = data) + ggtitle('k = 4') + theme_bw()
        p4 <- kmeans(data, centers = 5) %>%
          fviz_cluster(geom = 'point', data = data) + ggtitle('k = 5') + theme_bw()
        grid.arrange(p1, p2, p3, p4, nrow = 2, ncol = 2)
      }
      # Result
      set.seed(20)
      list_prod_data_visual_plot <- map(list_prod_data_freq_encoded, ~ visual_assess(.x))
      # Determine the optimal number of clusters: Elbow method
      kmeans_elbow_plot <- function(data) {
        # Extract the total within cluster sum of squares for each possible value of k
        total_withinss_values <- map_dbl(1:10, function(k) {
          model_kmeans <- kmeans(data, centers = k)
          model_kmeans$tot.withinss
        }
        )
        # Create the dataframe: k, within cluster sum of squares
        elbow_df_kmeans <- data.frame(k = 1:10, total_within_ss = total_withinss_values)
        # Create the elbow plot
        ggplot(elbow_df_kmeans, aes(x = k, y = total_within_ss)) + 
          geom_point() +
          geom_line() +
          scale_x_continuous(breaks = 1:10) + 
          labs(x = 'Cluster: K', y = 'Total Within Clusters SS') +
          theme_bw()
      }
      # Result
        # Health and beauty: k = 3
        # Electronic accessories: k = 3
        # Home and lifestyle: k = 2, 3
        # Sports and travel: k = 2, 3
        # Food and beverages: k = 2, 3
        # Fashion accessories: k = 3
        set.seed(30)
        list_prod_data_kmeans_elbow_plot <- map(list_prod_data_freq_encoded, ~ kmeans_elbow_plot(.x))
      # Determine the optimal number of clusters: Silhouette width method
      library(cluster)
      avg_sil_width_plot <- function(data) {
        # Extract the average silhouette width for each possible value of k
        avg_sil_width_values <- map_dbl(2:10, function(k) {
          model_kmeans <- kmeans(data, centers = k)
          sil_width <- silhouette(model_kmeans$cluster, dist = dist(data))
          mean(sil_width[, 3])
        }
        )
        # Create the dataframe: k, average silhouette width 
        sil_df_kmeans <- data.frame(k = 2:10, avg_sil_width = avg_sil_width_values)
        # Create the silhouette width plot
        ggplot(sil_df_kmeans, aes(x = k, y = avg_sil_width)) + 
          geom_point() +
          geom_line() +
          scale_x_continuous(breaks = 2:10) + 
          labs(x = 'Cluster: K', y = 'Average Silhouettes') +
          theme_bw()
      }
      # Result
        # Health and beauty: k = 3, 4
        # Electronic accessories: k = 3
        # Home and lifestyle: k = 4
        # Sports and travel: k = 3, 4
        # Food and beverages: k = 3, 4, 5
        # Fashion accessories: k = 3
        set.seed(40)
        list_prod_data_kmeans_sil_plot <- map(list_prod_data_freq_encoded, ~ avg_sil_width_plot(.x))
      # Cluster operation
      model_kmeans <- function(data, k) {
        kmeans(data, centers = k)
      }
      # Cluster assignment
      kmeans_k_2 <- partial(model_kmeans, k = 2)
      kmeans_k_3 <- partial(model_kmeans, k = 3)
      kmeans_k_4 <- partial(model_kmeans, k = 4)
      # Result
      set.seed(50)
      kmeans_prod_cluster <- map_if(list_prod_data_freq_encoded,
                                    names(list_prod_data_freq_encoded) == "Electronic accessories",
                                    ~ kmeans_k_2(.x)$cluster)
      set.seed(60)
      kmeans_prod_cluster <- map_if(kmeans_prod_cluster,
                                    names(kmeans_prod_cluster) == "Home and lifestyle",
                                    ~ kmeans_k_4(.x)$cluster)
      set.seed(70)
      kmeans_prod_cluster <- map_if(kmeans_prod_cluster,
                                    (names(kmeans_prod_cluster) != "Electronic accessories") & 
                                    (names(kmeans_prod_cluster) != "Home and lifestyle"),
                                    ~ kmeans_k_3(.x)$cluster)
      list_prod_data_kmeans_clustered <- map2(list_prod_data, 
                                              kmeans_prod_cluster, 
                                              ~ .x %>% mutate(cluster = .y))
    
  # Method_3: K-modes (Categorical attributes)
    # Categorical data
      # Selection
      categorical_data_selected <- function(data) {
        data %>% 
          select(customer_type, gender, payment, weekend, morning)
      }
      # Result
      list_prod_data_cat_selected <- map(list_prod_data, ~ categorical_data_selected(.x))
    # K-modes clustering
      # Determine the optimal number of clusters: Elbow method
      library(klaR)
      kmodes_elbow_plot <- function(data) {
        # Extract the within-cluster simple-matching distance for each possible value of k
        total_within_diff_values <- map_dbl(1:10, function(k) {
          model_kmodes <- kmodes(data, modes = k)
          sum(model_kmodes$withindiff)
        }
        )
        # Create the dataframe: k, within-cluster simple-matching distance
        elbow_df_kmodes <- data.frame(k = 1:10, total_within_diff = total_within_diff_values)
        # Create the elbow plot
        ggplot(elbow_df_kmodes, aes(x = k, y = total_within_diff)) + 
          geom_point() +
          geom_line() +
          scale_x_continuous(breaks = 1:10) + 
          labs(x = 'Cluster: K', y = 'Total Within Clusters Simple-Matching Distance')
      }
      # Result
        # Health and beauty: k = 3
        # Electronic accessories: k = 2 or 3
        # Home and lifestyle: k = 2 or 3
        # Sports and travel: k = 2 or 3
        # Food and beverages: k = 2
        # Fashion accessories: k = 2 or 3
        set.seed(80)
        list_prod_data_kmodes_sil_plot <- map(list_prod_data_cat_selected, ~ kmodes_elbow_plot(.x))
      # Cluster operation
      model_kmodes <- function(data, k) {
        kmodes(data, modes = k)
      }
      # Cluster assignment
      kmodes_k_3 <- partial(model_kmodes, k = 3)
      kmodes_k_4 <- partial(model_kmodes, k = 4)
      # Result
      set.seed(90)
      kmodes_prod_cluster <- map_if(list_prod_data_cat_selected, 
                                    names(list_prod_data_cat_selected) == "Home and lifestyle", 
                                    ~ kmodes_k_4(.x)$cluster, 
                                    .else = ~ kmodes_k_3(.x)$cluster)
      list_prod_data_kmodes_clustered <- map2(list_prod_data, 
                                              kmodes_prod_cluster, 
                                              ~ .x %>% mutate(cluster = .y))

      
