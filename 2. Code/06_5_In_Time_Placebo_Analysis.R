# Finding a structural break validates day of hurricane Ian-------

Lee_unr <- df_ian  %>% filter(County == "Lee" & (month_year < ian_lee[[10]][[1]][4]))

# structureal break function not working
#unr_time <- structural_break(df_ian, variable = 'unr', graph=FALSE)

Lee_unr$time <- Lee_unr$year + ((Lee_unr$month/12)-1)

# Time series from Jan 2021 to Dec 2024
y_ts <- ts(Lee_unr$unr, start = c(2021, 1), frequency = 12)

# 3. Run Zivot-Andrews test (model "both" intercept and trend break)
za_test <- ur.za(y_ts, model = "both")

# 4. Show summary
summary(za_test)

# 5. Extract the break date
# Extract the break index (position in time series)
breakpoint <- za_test@bpoint

# Map index to year and month
break_time <- time(y_ts)[breakpoint]
cat("Estimated structural break at:", break_time, "\n")

row_index <- which(Lee_unr$time == break_time)  # find row(s) where time == break_time

# Extract month_year from that row
break_date <- Lee_unr$month_year[row_index]
break_date

lee_unr <- run_synth(df_ian, in_time_placebo=TRUE, date=break_date)
lee_unr %>% plot_trends()
export_results(lee_unr, "In_Time_Placebo_UNR")

# 6. Plot time series with break date line
# Convert ts object to a dataframe for ggplot
df_plot <- data.frame(
  date = seq(as.Date("2021-01-01"), by = "month", length.out = length(y_ts)),
  value = as.numeric(y_ts)
)

# # Plot with ggplot
# p <- ggplot(df_plot, aes(x = date, y = value)) +
#   geom_line(color = "navy", size = 1) +
#   geom_point(aes(y = value), size = 2) +
#   geom_vline(xintercept = as.numeric(break_date), color = "red", linetype = "dashed") +
#   labs(title = "Lee County Time Series with Structural Break",
#        x = "Month-Year",
#        y = "Unemployment Rate") +
#   scale_x_date(date_labels = "%b %Y", date_breaks = "5 months") +
#   theme_minimal() + 
#   theme(plot.background = element_rect(fill = "white", color = NA),    # <- force white
#                           panel.background = element_rect(fill = "white", color = NA))  # <- force white)
# 
# # Print plot
# print(p)

# Don't need to save since all Graphs are created in Python
# To save:
#ggsave("3. Graphs/LeeCounty_BreakPlot.png", plot = p, width = 8, height = 4)


# In Time Placebo Analysis -------------------

in_time_placebo <- run_synth(df_k[[k_use$df_k]], in_time_placebo=TRUE, date='2021-08-01')

#export data to excel
in_time_placebo %>% export_results("UNR", in_time_placebo=T)

rm(in_time_placebo, za_test, Lee_unr, lee_unr,
   break_date, break_time, breakpoint, row_index, y_ts, df_plot)

#Plotting <- in_time_placebo[[6]][[1]]
#Plotting$diff <- Plotting$real_y - Plotting$synth_y
#Meta_data <- in_time_placebo[[10]][[1]]


# scm_plotting(df=Plotting, unit_name = 'Lee County',  
#              title_name = "In-Time Placebo Analysis (August 2021)", 
#              x_name= "Month-Year", y_name= "Unemployment Rate", file_name='in-time_placebo_scm_plot')

