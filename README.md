# MEA Analysis Package

This package provides tools for analyzing Microelectrode Array (MEA) data. It includes three main classes: `MEAnalysis`, `BatchMEAnalysis`, and `ElectrodeBursts`.

## Classes

### MEAnalysis

The `MEAnalysis` class is the core of the package, providing methods for processing and analyzing MEA data.

Key methods:

1. `treatment_averages_t_test_plot(control, groups_to_plot, title = NULL)`
   - Parameters:
     - `control`: character - The name of the control group
     - `groups_to_plot`: character vector - Names of groups to include in the plot
     - `title`: character (optional) - The title of the plot
   - Returns: ggplot object - A plot representing the t-test results for treatment averages

### BatchMEAnalysis

The `BatchMEAnalysis` class extends `MEAnalysis` to handle batch processing of multiple MEA data files.

Key methods:

1. `run_mea_analysis(conditions, metric, control_condition = NULL, title = NULL)`
   - Parameters:
     - `conditions`: character vector - Conditions to analyze
     - `metric`: character - The specific metric to analyze
     - `control_condition`: character (optional) - The control condition to compare against
     - `title`: character (optional) - The title of the plot
   - Returns: ggplot object - A plot representing the MEA analysis results

2. `generate_significance_overview(control_group, groups_to_include = NULL, metrics_to_visualize = NULL, title)`
   - Parameters:
     - `control_group`: character - The control group to compare against
     - `groups_to_include`: character vector (optional) - Specific groups to include in the analysis
     - `metrics_to_visualize`: character vector (optional) - Specific metrics to visualize
     - `title`: character - The title of the heatmap
   - Returns: data.frame - A significance overview table

### ElectrodeBursts

The `ElectrodeBursts` class focuses on processing and visualizing electrode burst data.

Key methods:

1. `create_comparison_raster_plot(control_group, treatments_array, plot_title)`
   - Parameters:
     - `control_group`: character - The control group to be included in the comparison
     - `treatments_array`: character vector - An array of treatment names to compare
     - `plot_title`: character - The title of the plot
   - Returns: ggpubr object - A combined raster plot comparing multiple treatments

## Usage

To use this package, start by initializing the appropriate class with your data file(s):

```R
# For single file analysis
mea_analysis <- MEAnalysis$new("path/to/your/file.csv")

# For batch analysis
batch_analysis <- BatchMEAnalysis$new(list_of_file_paths)

# For electrode burst analysis
electrode_bursts <- ElectrodeBursts$new("path/to/your/file.csv")
```

Then, you can use the various methods provided by each class to analyze your data and generate plots. Here are some examples:

### MEAnalysis

```R

# Create a treatment averages t-test plot
plot <- mea_analysis$treatment_averages_t_test_plot(
  "Control",
  c("Treatment1", "Treatment2"),
  "Treatment Averages T-Test Plot"
)
# plot is a ggplot object
```

### BatchMEAnalysis

```R

# Run MEA analysis
plot <- batch_analysis$run_mea_analysis(
  c("Control", "Treatment1", "Treatment2"),
  "MeanFiringRate",
  "Control",
  "MEA Analysis Results"
)
# plot is a ggplot object

# Generate significance overview
overview <- batch_analysis$generate_significance_overview(
  "Control",
  c("Treatment1", "Treatment2"),
  c("MeanFiringRate", "BurstFrequency"),
  "Significance Overview"
)
# overview is a data.frame containing the significance overview
```

### ElectrodeBursts

```R
# Create a comparison raster plot
plot <- electrode_bursts$create_comparison_raster_plot(
  "Control",
  c("Treatment1", "Treatment2"),
  "Comparison Raster Plot"
)
# plot is a ggpubr object
```

## Requirements

This package requires R and the following libraries:
- ggplot2
- dplyr
- tidyr
- ggpubr

Make sure to install these dependencies before using the package:

```R
install.packages(c("ggplot2", "dplyr", "tidyr", "ggpubr"))
```

## Contributing

Contributions to improve the package are welcome. Please submit issues and pull requests on the project's GitHub repository.

## License

[Insert your chosen license information here]
