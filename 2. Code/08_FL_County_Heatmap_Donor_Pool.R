# Florida Donor Pool Heat Map
# Created by Claude, Sonnet 4.5 through my prompt
# This script creates a heat map of Florida counties showing synthetic control donor weights

# Load required libraries
pacman::p_load('tidyverse', 
               'sf', 
               'ggplot2',
               'scales',
               'tigris'# Get Florida county boundaries from the tigris package
               )


# Read the donor weights data
donor_weights <- read_csv("1. Data/Synth_Results/Synth_Lee_UNR_Donor_Weights.csv")


# Download Florida county boundaries
fl_counties <- counties(state = "FL", cb = TRUE, year = 2021)

# Convert to sf object and ensure correct projection
fl_counties <- st_as_sf(fl_counties)

# Join the donor weights to the county spatial data
fl_map_data <- fl_counties %>%
  left_join(donor_weights, by = c("NAME" = "unit"))

# Create categories for coloring
fl_map_data <- fl_map_data %>%
  mutate(
    map_category = case_when(
      NAME == "Lee" ~ "Lee County",
      !is.na(weight) ~ "Donor Pool",
      TRUE ~ "Other"
    )
  )

# Create the heat map
p <- ggplot() +
  # Plot all counties with borders
  geom_sf(data = fl_map_data %>% filter(map_category == "Other"), 
          fill = "grey85", color = "white", size = 0.3) +
  
  # Plot donor pool counties with blue gradient based on weights
  geom_sf(data = fl_map_data %>% filter(map_category == "Donor Pool"), 
          aes(fill = weight), color = "white", size = 0.3) +
  
  # Plot Lee County in red
  geom_sf(data = fl_map_data %>% filter(map_category == "Lee County"), 
          fill = "red", color = "white", size = 0.3) +
  
  # Apply blue gradient for donor pool
  scale_fill_gradient(
    low = "#E3F2FD",    # Light blue
    high = "#0D47A1",   # Dark blue
    na.value = "grey85",
    name = "Donor Weight",
    labels = label_number(accuracy = 0.01)
  ) +
  
  # Styling
  theme_void() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 9)
  ) +
  
  labs(
    title = "Synthetic Control Donor Pool Weights",
    subtitle = "Florida Counties - Lee County Analysis"
  )

# Display the plot
print(p)

# Save the plot
ggsave("3. Graphs/florida_donor_heatmap.png", p, 
       width = 10, height = 8, dpi = 300, bg = "transparent")

# Optional: Save as PDF for publication quality
ggsave("3. Graphs/florida_donor_heatmap.pdf", p, 
       width = 10, height = 8, bg = "transparent")

cat("Heat map created successfully!\n")
cat("Saved as: florida_donor_heatmap.png and florida_donor_heatmap.pdf\n")

# Print summary statistics
cat("\nDonor Pool Summary:\n")
cat("Number of donor counties:", nrow(donor_weights), "\n")
cat("Total weight:", sum(donor_weights$weight), "\n")
cat("Top 5 donors by weight:\n")
print(donor_weights %>% arrange(desc(weight)) %>% head(5))
