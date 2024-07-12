source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/R/MEA Analysis.R")
library(ggplot2)
library(dplyr)
library(patchwork)


# Trajectory: 5 min, 30 min, 60 min...


df1 <- read_csv('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(000).csv')

df1_treatment_averages <- find_treatment_averages(df1)

df1_sample_assignments <- find_sample_assignments(df1)

df1_samples <- get_treatment_list(df1_sample_assignments)

print(df1_treatment_averages)

mean_firing_rate_treatment_average(df1_treatment_averages)


df2 <- read_csv('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 1/6-05-24 nxn DIV 12 PLATE 1(001)(000).csv')

df2_treatment_averages <- find_treatment_averages(df2)

df2_sample_assignments <- find_sample_assignments(df2)

df2_samples <- get_treatment_list(df2_sample_assignments)

print(df2_treatment_averages)

print(mean_firing_rate_treatment_average(df2_treatment_averages))


# Updated function to create two separate plots using faceting
time_comparison_plots <- function(time1_df, time2_df, control, treatment) {
  # Add a column to each dataframe to indicate the time point
  time1_df <- time1_df %>% mutate(Time = "Time1")
  time2_df <- time2_df %>% mutate(Time = "Time2")
  
  # Combine the two dataframes
  combined_df <- bind_rows(time1_df, time2_df)
  
  # Filter for the specified control and treatment
  filtered_df <- combined_df %>% filter(Sample %in% c(control, treatment))
  
  # Add a Condition column to distinguish between control and treatment
  filtered_df <- filtered_df %>%
    mutate(Condition = case_when(
      Sample == control ~ control,
      Sample == treatment ~ treatment
    ))
  
  # Create the plot using facet_wrap
  p <- ggplot(filtered_df, aes(x = Time, y = Avg, fill = Time)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Avg - Std, ymax = Avg + Std), width = 0.2, position = position_dodge(0.9)) +
    theme_minimal() +
    labs(x = "Time Point", y = "Mean Firing Rate (Hz) (Avg ± Std)") +
    facet_wrap(~ Condition, ncol = 1, )
  
  return(p)
}

plot <- time_comparison_plots(mean_firing_rate_treatment_average(df1_treatment_averages), mean_firing_rate_treatment_average(df2_treatment_averages), "NEG Control-HET", "NEG Control-WT")
print(plot)