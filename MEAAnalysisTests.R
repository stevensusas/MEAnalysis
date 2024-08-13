setwd("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA")
source('MEAAnalysisClass.R')


#Test1: Rili's Data Plate 1
analysis <- MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv")

sample_assignments <- analysis$get_sample_assignments()
print(sample_assignments)

treatment_averages <- analysis$get_treatment_averages()
print(treatment_averages)

well_averages <- analysis$get_well_averages()
print(well_averages)

electrode_averages <- analysis$get_electrode_averages()
print(electrode_averages)

treatment_list <- analysis$get_treatment_list()
print(treatment_list)

control_group <- "Luciferase-HET"
groups_to_plot <- c("KMT2C-HET", "NSD1-HET", "ASH1L-HET", "MED13L-HET", "CHD8-HET", "TBR1-HET", "DNMT3A-HET", "KDM6B-HET", "MBD5-HET", "SETD5-HET", "Luciferase-HET")

t_test_plot <- analysis$treatment_averages_t_test_plot(control_group, groups_to_plot)

print(t_test_plot)


#Test 2: Emily's Data

analysis_2 <- MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/Emily/24h.csv")

sample_assignments <- analysis_2$get_sample_assignments()
print(sample_assignments)

treatment_averages <- analysis_2$get_treatment_averages()
print(treatment_averages)

well_averages <- analysis_2$get_well_averages()
print(well_averages)

electrode_averages <- analysis_2$get_electrode_averages()
print(electrode_averages)

treatment_list <- analysis_2$get_treatment_list()
print(treatment_list)

control_group <- "WT_control"
groups_to_plot <- c("KO_control", "KO_BDNF", "KO_TTX", "WT_control", "WT_BDNF", "WT_TTX")

t_test_plot <- analysis_2$treatment_averages_t_test_plot(control_group, groups_to_plot)

print(t_test_plot)

#Test3: Rili's Data Plate 1 Different Comparison

analysis <- MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-18-24 div 25/Plate 1/ra nxn plate1cspontaneous 5min(000)(000).csv")

sample_assignments <- analysis$get_sample_assignments()
print(sample_assignments)

treatment_averages <- analysis$get_treatment_averages()
print(treatment_averages)

well_averages <- analysis$get_well_averages()
print(well_averages)

electrode_averages <- analysis$get_electrode_averages()
print(electrode_averages)

treatment_list <- analysis$get_treatment_list()
print(treatment_list)

control_group <- "NEG Control-WT"
groups_to_plot <- c("NEG Control-HET", "NEG Control-WT")

t_test_plot <- analysis$treatment_averages_t_test_plot(control_group, groups_to_plot)

print(t_test_plot)


#Test 4: Rili's Data Plate 3:

#Test1: Rili's Data Plate 1
analysis <- MEAnalysis$new("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-7-24/Plate 3/div 14 nxn 6-7-24 plate 3(000)(000).csv")

treatment_averages <- analysis$get_treatment_averages()
print(treatment_averages)



control_group <- "Luciferase-HET"
groups_to_plot <- c("KMT2C-HET", "NSD1-HET", "ASH1L-HET", "MED13L-HET", "CHD8-HET", "TBR1-HET", "DNMT3A-HET", "KDM6B-HET", "MBD5-HET", "SETD5-HET", "Luciferase-HET")

t_test_plot <- analysis$treatment_averages_t_test_plot(control_group, groups_to_plot)

print(t_test_plot)


