# In-Space Placebo Analysis ----------
ian_lee %>% plot_placebos()
# Graph the below in Python

donor_pool <- unique(df_k[[k_use$df_k]]$County)

collect_placebos(ian_lee, "UNR", "Lee", county_seq=seq(from = 1, to = length(donor_pool), by = 2))

rm(donor_pool)