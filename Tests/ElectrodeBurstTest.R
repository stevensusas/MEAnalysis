source('/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Korb-MEA/Class/ElectrodeBurstsClass.R')

# Creating an object of the class Person
bursts <- ElectrodeBursts$new("/Users/stevensu/Desktop/Korb Lab/MEA Analysis/Steven_MEA/ra nxn and Sean 6-13-24 DIV 20/Plate 1/Plate 1 DIV 20 Min 30 Bursts.csv")
control = "Luciferase-WT"
conditions = c("ASH1L-WT", "CHD8-WT")
comparison_plot <- bursts$create_comparison_raster_plot(control, conditions, "Hello")

# Define the file path
file_path <- "/Users/stevensu/Desktop/Korb Lab/MEA Analysis/example.pdf"

# Save the plot to the specified path
print(comparison_plot)
