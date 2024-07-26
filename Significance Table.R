source("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/MEA Analysis.R")
library(ggplot2)
library(dplyr)
library(patchwork)
library(tools)

#Challenge1: dealing with file names metadata
#Challenge2: dealing with sample assignment names

#focus on Plate 1 for now

plate1 <-c('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 1/6-04-24 nxn DIV 11 PLATE 1(002)(001).csv',
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

plate3 <- c(
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 3/6-04-24 nxn DIV 11 PLATE 3(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-4-24/Plate 3/6-04-24 nxn DIV 11 PLATE 3(001)(001).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 3/6-05-24 nxn DIV 12 PLATE 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-5-24/Plate 3/6-05-24 nxn DIV 12 PLATE 3(000)(001).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 3/6-06-24 nxn DIV 13 PLATE 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-6-24/Plate 3/6-06-24 nxn DIV 13 PLATE 3(000)(001).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 3/div 14 nxn 6-7-24 plate 3(000)(000).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-8-24/Plate 3/ra nxn div 15 plate 3(002)(000).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 3/ra nxn div 16 plate 3(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-9-24/Plate 3/ra nxn div 16 plate 3(001)(000).csv",
  
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 3/ra nxn div 17 plate3 spontaneous 30min in(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-10-24/Plate 3/ra nxn div 17 plate3 spontaneous 30min in(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 broadband30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 field potential 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 spontaeous 3 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 spontaeous 25 min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-11-24/Plate 3/ra nxn div 18 plate 3 spontaneou 5 min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 field potential 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 3/ra nxn div 20 plate3 spontaneous 60min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 field potential 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-14-24 DIV 21/Plate 3/ra nxn div 21 plate 3 spontaneous 30min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 field potential 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 spontaneaous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 spontaneaous 5min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-15-24 DIV 22/Plate 3/ra nxn div 22 plate3 spontaneaous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 field potential 30min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-17-24/Plate 3/ra nxn plate3 spontaneous 35min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 field potential 30min(001)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 spontaneous 5min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 spontaneous 30min(000)(000).csv",
  "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 3/ra nxn plate3 spontaneous 35min(000)(000).csv"
  
)



get_strings_ending_with_WT_and_NEG_HET_and_short <- function(input_array) {
  # Use grep to find strings ending with "WT"
  WT_strings <- grep("WT$", input_array, value = TRUE)
  
  # Find "NEG Control-HET" in the input array
  NEG_HET <- input_array[input_array == "NEG Control-HET"]
  
  # Find strings with less than 3 characters
  short_strings <- input_array[nchar(input_array) < 3]
  
  # Combine WT strings, NEG Control-HET, and short strings
  result <- unique(c(WT_strings, NEG_HET, short_strings))
  
  return(result)
}


get_array_excluding_values <- function(input_array, exclude_value1, exclude_value2) {
  # Use logical indexing to select elements not equal to either exclude_value
  result <- input_array[input_array != exclude_value1 & input_array != exclude_value2]
  
  return(result)
}


# Define your control group
control_group <- "Luciferase-HET"

# Define groups to omit
groups_to_omit <- get_strings_ending_with_WT_and_NEG_HET_and_short(get_treatment_list(find_sample_assignments(read_csv(plate3[1]))))

#groups_to_omit <- get_array_excluding_values(get_treatment_list(find_sample_assignments(read_csv(plate3[1]))), "U", "NEG Control-HET")

specific_metric <- "Mean Firing Rate (Hz)"

# Generate the significance table
significance_table <- generate_significance_table(plate3, control_group, groups_to_omit)

# View the table
print(significance_table)

setwd('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/Results')

write.csv(significance_table, file = "Plate 3 Luc HET vs Depletion HET Sig Table.csv", row.names = FALSE)
