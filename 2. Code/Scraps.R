# Plotting the results --------------------

Plotting <- ian_lee[[6]][[1]]
Plotting$diff <- Plotting$real_y - Plotting$synth_y
Meta_data <- ian_lee[[10]][[1]]

scm_plotting(df=Plotting, unit_name = 'Lee County',  
             title_name = "Hurricane Ian (September 2022)", 
             x_name= "Month-Year", y_name= "Unemployment Rate", file_name='scm_plot')

# better plot of the difference
scm_delta <- ggplot(Plotting, aes(x = time_unit)) +
  # trend lines
  geom_line(aes(y = diff, color = "diff", linetype = "diff"), size = 1) +
  # add dots for each point
  geom_point(aes(y = diff, color = "diff"), size = 2) +
  # addes a line for when the intervention occured
  geom_vline(xintercept = Meta_data$treatment_time, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("diff" = "navy")) +
  scale_linetype_manual(values = c("diff" = "solid")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "5 months") +
  labs(
    title = "Difference in the synthetic control and observed Lee County",
    x = "Month-Year",
    y = "Percentage Point Difference in Unemployment Rate",
    color = "Series",
    linetype = "Series"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position="none",
    plot.background = element_rect(fill = "white", color = NA),    # <- force white
    panel.background = element_rect(fill = "white", color = NA)    # <- force white
  )

print(scm_delta)
ggsave("Graphs/scm_delta.png", scm_delta, width = 8, height = 6, dpi = 300)

