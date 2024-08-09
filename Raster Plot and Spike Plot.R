library(ggplot2)
library(patchwork)

# Example data for raster plot
set.seed(123)
time <- sort(runif(400, min=0, max=60))
electrode <- sample(1:12, 400, replace=TRUE)
condition <- rep(c("A", "B", "C", "D"), each=100)
df_raster <- data.frame(time=time, electrode=electrode, condition=condition)

# Example data for spike plot
df_spike <- data.frame(
  start_time = c(1, 3, 5, 7),
  duration = c(1, 0.5, 0.7, 1.2),
  height = c(2, 1.5, 2.5, 2)
)

# Function to create spike coordinates
spike_coords <- function(start_time, duration, height) {
  data.frame(
    x = c(start_time, start_time + duration / 2, start_time + duration),
    y = c(0, height, 0)
  )
}

# Create spike data
spike_data <- do.call(rbind, lapply(1:nrow(df_spike), function(i) {
  coords <- spike_coords(df_spike$start_time[i], df_spike$duration[i], df_spike$height[i])
  coords$group <- i
  coords
}))

# Create border data for spikes
border_data <- do.call(rbind, lapply(1:nrow(df_spike), function(i) {
  start_time <- df_spike$start_time[i]
  duration <- df_spike$duration[i]
  height <- df_spike$height[i]
  data.frame(
    x = c(start_time, start_time + duration / 2, start_time + duration / 2),
    y = c(0, height, height),
    xend = c(start_time + duration / 2, start_time, start_time + duration),
    yend = c(height, 0, 0),
    group = c(i, i, i)
  )
}))

# Create spike plot
spike_plot <- ggplot() +
  geom_polygon(data=spike_data, aes(x=x, y=y, group=group), fill="transparent") +
  geom_segment(data=border_data, aes(x=x, y=y, xend=xend, yend=yend, group=group), color="black", size=0.5) +
  labs(x="", y="Spike Height") +
  theme_minimal() +
  theme(
    axis.title.y = element_text(size=12),
    axis.text = element_text(size=10),
    plot.margin = margin(b = 0)
  )

# Create raster plot
raster_plot <- ggplot(df_raster, aes(x=time, y=electrode, color=condition)) +
  geom_point(shape=124) +
  scale_y_reverse() +
  labs(x="Time (s)", y="Electrode") +
  theme_minimal() +
  theme(
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    legend.position = "bottom",
    plot.margin = margin(t = 0)
  )

# Combine plots
combined_plot <- spike_plot / raster_plot +
  plot_layout(heights = c(1, 1))  # Adjust the ratio as needed

# Display the combined plot
print(combined_plot)
