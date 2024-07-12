source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/R/MEA Analysis.R")
library(ggplot2)
library(dplyr)
library(patchwork)
library(tools)

#Challenge1: dealing with file names metadata
#Challenge2: dealing with sample assignment names

#focus on Plate 1 for now

data <-c('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(001).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 1/6-05-24 nxn DIV 12 PLATE 1(001)(001).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 1/6-06-24 nxn DIV 13 PLATE 1(000)(001).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 1/div 14 nxn 6-7-24 plate 1(000)(000).csv',

         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 1/ra nxn div 15 plate 1(000)(000).csv',
    
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 1/ra nxn div 16 plate 1(003)(000).csv',

         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 1/ra nxn div 17 plate1 spontaneous(001)(000).csv',

         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/ra nxn div 18 plate 1 spontaneous 20min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 1/ra nxn div 18 plate 1 spontaneous 30min(000)(000).csv',
 
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 5min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 30min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/ra nxn div 20 plate1 spontaneous 60min(000)(000).csv',

         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 5min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 30min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 1/ra nxn div 21 spontaneous 50min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1 spontaneous 30min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1 spontaneous(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 1/ra nxn div 22 plate1spontaneaous 60min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 1/ra nxn plate1cspontaneous 30min(001)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 30min(000)(000).csv',
         '/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 30min(001)(000).csv'
         )


 process_file_name <- function(file_path) {
  # Split the string by '/'
 path_parts <- unlist(strsplit(file_path, "/"))

  # Extract the part after the 7th '/'
  part_after_7th_slash <- path_parts[8]

  # Remove the file extension
 result <- file_path_sans_ext(part_after_7th_slash)

 return(result)
 }

 for (file in data) {
 df <- read_csv(file)
df_treatment_averages <- find_treatment_averages(df)
# #
samples <- get_treatment_list(find_sample_assignments(df))
t_test_results <- perform_t_tests(df_treatment_averages, "Luciferase-HET")
 p <- treatment_averages_t_test_plot(df_treatment_averages, samples)
 p <- p + ggtitle(process_file_name(file))

print(p)

 setwd("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/Luc HET vs Depletion HET Plots")
ggsave(paste(process_file_name(file), '.png', sep = ""), plot = p, width = 20, height = 15, dpi = 300)
 }
# 
# df <- read_csv('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(001).csv')
# df_treatment_averages <- find_treatment_averages(df)
# 
# samples <- get_treatment_list(find_sample_assignments(df))
# t_test_results <- perform_t_tests(df_treatment_averages, "NEG Control-WT")
# treatment_averages_t_test_plot(df_treatment_averages, samples)
