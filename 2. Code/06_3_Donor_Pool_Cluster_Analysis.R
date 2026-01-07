# import pacakges
pacman::p_load(haven, cluster, factoextra, mclust)

# Using K-Means cluster analysis to select our donor pool J
# Only select data for pre-intervention, note I need to lag all my yearly variables
# so that I can have it run up to the time before the hurricane but this will be good enough for now

df_cluster <- df_ian %>% filter(year==2021)

# Step 1: Create unit-level summary data for clustering
unit_features <- df_cluster %>%
  group_by(County) %>%
  summarise(
    mean_unr = mean(unr),
    sd_unr = sd(unr),
    rgdp = mean(ln_rgdp_per_cap_lag1y),
    pop_growth = mean(pop_growth_rate),
    black = mean(black),
    native = mean(native_american),
    hispanic = mean(hispanic),
    white = mean(white),
    asian = mean(asian),
    other = mean(other),
    pacific_islander = mean(pacific_islander),
    pct_18p = mean(pct_18p),
    pct_less_hs_18p = mean(pct_less_hs_18p),
    pcg_hs_18p = mean(pct_hs_18p),
    pct_some_college_18p = mean(pct_some_college_18p),
    pct_bachelorh_18p = mean(pct_bachelorh_18p),
    bb_coverage_pct = mean(bb_coverage_pct),
    ln_med_inc = mean(ln_med_inc),
    renter_occ_pct = mean(renter_occ_pct),
    owner_occ_pct = mean(owner_occ_pct),
    unr_lag1m = mean(unr_lag1m),
    unr_lag1y = mean(unr_lag1y)
  ) %>%
  ungroup()

# Optional: Standardize features
unit_features_scaled <- unit_features %>%
  column_to_rownames("County") %>%
  scale()

# Step 2: Determine optimal number of clusters 
set.seed(123)

# using the elbow method
elbow_plot <- fviz_nbclust(unit_features_scaled, kmeans, method = "wss") +  
  labs(title = "Elbow Method for Optimal K") + theme(
    plot.background = element_rect(fill = "white", color = NA),    # <- force white
    panel.background = element_rect(fill = "white", color = NA)    # <- force white
  )
ggsave("3. Graphs/elbow_method.png", elbow_plot, width = 8, height = 6, dpi = 300)


# using the silhouette method
silhouette_plot <- fviz_nbclust(unit_features_scaled, kmeans, method = "silhouette") + 
  labs(title = "Silhouette Method for Optimal K") + theme(
    plot.background = element_rect(fill = "white", color = NA),    # <- force white
    panel.background = element_rect(fill = "white", color = NA)    # <- force white
  )
ggsave("3. Graphs/silhouette_method.png", silhouette_plot, width = 8, height = 6, dpi = 300)

# Choose number of clusters (e.g., k = 4)
# Identify row index of Lee County
# You might need to adapt this depending on how your data is structured
lee_index <- which(rownames(unit_features_scaled) == "Lee")

for (k in c(2, 3, 4)) {
  km_model <- kmeans(unit_features_scaled, centers = k, nstart = 25)
  
  cluster_plot <- fviz_cluster(km_model, 
                               data = unit_features_scaled,
                               geom = "point",
                               ellipse.type = "euclid", 
                               palette = "jco",
                               ggtheme = theme_minimal(),
                               repel = TRUE) +
    labs(title = paste("K-Means Clustering of Donor Units (K =", k, ")")) + 
    theme(
      plot.background = element_rect(fill = "white", color = NA),    # <- force white
      panel.background = element_rect(fill = "white", color = NA)    # <- force white
    )
  
  # Extract plotting data
  plot_data <- cluster_plot$data
  
  # Get coordinates for Lee County
  lee_coords <- plot_data[lee_index, ]
  
  # Add label for Lee County
  cluster_plot <- cluster_plot +
    geom_text(data = lee_coords, aes(x = x, y = y, label = "L"),
              color = "black", fontface = "bold", size = 4, vjust = -1)
  
  print(cluster_plot)
  
  ggsave(filename = paste0("3. Graphs/cluster_plot_k", k, "_lee_labeled.png"),
         plot = cluster_plot,
         width = 8, height = 6, dpi = 300)
  
  
  # Add cluster assignment to data
  unit_features$cluster <- km_model$cluster
  
  # Step 3: Identify treated unit's cluster
  treated_unit <- unit$County  # replace with your actual treated unit ID
  treated_cluster <- unit_features %>%
    filter(County == treated_unit) %>%
    pull(cluster)
  
  # Step 4: Select donor pool as all units in the same cluster (excluding treated unit)
  donor_pool <- unit_features %>%
    filter(cluster == treated_cluster, County != treated_unit) %>%
    pull(County)
  
  print(paste("Donor pool for K =", k))
  print(donor_pool)
  
  # --- NEW LINE: Export donor pool CSV ---
  write.csv(
    donor_pool,
    file = paste0("1. Data/donor_pool_k", k, ".csv"),
    row.names = FALSE
  )
  
  donor_pool <- append(donor_pool, treated_unit)
  
  # Create a list of different data frames based on the cluster create by the value of K
  # create each entry of the list, this is like a python dictionary or hash 
  if (k==2){
    # create an empty list  
    df_k <- list()
    df_k[[paste0("df_k", k)]] <- df_ian %>%
      filter(County %in% donor_pool)
    
  }
  else{
    df_k[[paste0("df_k", k)]] <- df_ian %>%
      filter(County %in% donor_pool)
  }
}

rm(cluster_plot,elbow_plot, km_model, plot_data, silhouette_plot, 
   unit_features, unit_features_scaled, donor_pool, k, 
   lee_index, treated_cluster, lee_coords, treated_unit, df_cluster)
