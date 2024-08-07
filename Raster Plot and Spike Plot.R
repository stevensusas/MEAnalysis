# Example data with conditions
set.seed(123)
time <- sort(runif(400, min=0, max=60))
electrode <- sample(1:12, 400, replace=TRUE)
condition <- rep(c("A", "B", "C", "D"), each=100)
df <- data.frame(time=time, electrode=electrode, condition=condition)

print(df)
library(ggplot2)

# Plot with conditions
ggplot(df, aes(x=time, y=electrode, color=condition)) +
  geom_point(shape=124) +
  scale_y_reverse() +
  labs(x="Time (s)", y="Electrode") +
  theme_minimal() +
  facet_wrap(~condition, ncol=1)

# Example data
df <- data.frame(
  start_time = c(1, 3, 5, 7),
  duration = c(1, 0.5, 0.7, 1.2),
  height = c(2, 1.5, 2.5, 2)
)

spike_coords <- function(start_time, duration, height) {
  data.frame(
    x = c(start_time, start_time + duration / 2, start_time + duration),
    y = c(0, height, 0)
  )
}


# Create a data frame to store all the spike coordinates
spike_data <- do.call(rbind, lapply(1:nrow(df), function(i) {
  coords <- spike_coords(df$start_time[i], df$duration[i], df$height[i])
  coords$group <- i
  coords
}))

# Create segments for the bold borders (excluding the base)
border_data <- do.call(rbind, lapply(1:nrow(df), function(i) {
  start_time <- df$start_time[i]
  duration <- df$duration[i]
  height <- df$height[i]
  data.frame(
    x = c(start_time, start_time + duration / 2, start_time + duration / 2),
    y = c(0, height, height),
    xend = c(start_time + duration / 2, start_time, start_time + duration),
    yend = c(height, 0, 0),
    group = c(i, i, i)
  )
}))

# Plot the spike plot with transparent triangles and bold borders
ggplot() +
  geom_polygon(data=spike_data, aes(x=x, y=y, group=group), fill="transparent") +
  geom_segment(data=border_data, aes(x=x, y=y, xend=xend, yend=yend, group=group), color="black", size=0.5) +
  labs(x="Time (s)", y="Spike Height") +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12),
    axis.text = element_text(size=10)
  )

