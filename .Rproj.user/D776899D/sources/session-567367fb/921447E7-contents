library(tidyverse)

# Read the existing data
existing_earthquakes <- read_csv("earthquakes.csv")

# Fetch the latest daily data from USGS
daily_earthquakes <- read_csv("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.csv")

# Combine and remove duplicates based on the 'id' column
all_earthquakes <- bind_rows(existing_earthquakes, daily_earthquakes) |>
  distinct(id, .keep_all = TRUE)

# Save the updated dataset
write_csv(all_earthquakes, "earthquakes.csv")

# Filter based on the criteria AND past 24 hours
filtered_earthquakes <- all_earthquakes |>
  filter(
    mag >= 5 &
      str_detect(place, "Japan") &
      time >= (now() - days(1))
  )

# Generate sentences for each earthquake
earthquake_sentences <- filtered_earthquakes |>
  mutate(
    date = format(time, "%B %d, %Y"),
    time_formatted = format(time, "%H:%M:%S UTC"),
    sentence = glue::glue("At {time_formatted} on {date}, an earthquake of {mag} {magType} was recorded {place}.")
  )

# Create summary notification
if (nrow(earthquake_sentences) == 0) {
  cat("No earthquakes of magnitude 5 or greater were recorded near Japan in the past 24 hours.\n")
} else {
  num_quakes <- nrow(earthquake_sentences)
  cat(glue::glue("There {if(num_quakes == 1) 'was' else 'were'} {num_quakes} earthquake{if(num_quakes > 1) 's' else ''} of magnitude 5 or greater recorded in Japan in the past 24 hours:\n\n"))
  earthquake_sentences |>
    pull(sentence) |>
    walk(~cat(.x, "\n\n"))
}

# Define time periods
week_ago <- now() - weeks(1)
two_weeks_ago <- now() - weeks(2)

# Count earthquakes in each period
recent_week <- all_earthquakes |>
  filter(time >= week_ago & time < now()) |>
  nrow()

previous_week <- all_earthquakes |>
  filter(time >= two_weeks_ago & time < week_ago) |>
  nrow()

# Calculate percentage change
pct_change <- ((recent_week - previous_week) / previous_week) * 100

# Generate analysis sentence
cat(glue::glue("In the past week, there were {recent_week} earthquakes
                compared to {previous_week} the week before,
                a {round(pct_change, 1)}% {if(pct_change > 0) 'increase' else 'decrease'}.\n"))

# Categorize earthquakes by intensity
earthquakes_categorized <- all_earthquakes |>
  filter(time >= (now() - months(1))) |>
  mutate(
    intensity = case_when(
      mag < 2.0 ~ "Micro",
      mag < 4.0 ~ "Minor",
      mag < 5.0 ~ "Light",
      mag < 6.0 ~ "Moderate",
      mag < 7.0 ~ "Strong",
      mag < 8.0 ~ "Major",
      mag >= 8.0 ~ "Great",
      TRUE ~ "Unknown"
    )
  )

# Count by category
intensity_counts <- earthquakes_categorized |>
  count(intensity, sort = TRUE) |>
  mutate(percentage = round(n / sum(n) * 100, 1))

# Display the results
cat("Earthquake distribution by intensity (past month):\n")
cat(paste(rep("-", 50), collapse = ""), "\n")
for (i in 1:nrow(intensity_counts)) {
  cat(sprintf("%-12s: %5d (%5.1f%%)\n",
              intensity_counts$intensity[i],
              intensity_counts$n[i],
              intensity_counts$percentage[i]))
}
cat("\n")

# Create histogram visualization
magnitude_plot <- earthquakes_categorized |>
  ggplot(aes(x = mag, fill = intensity)) +
  geom_histogram(binwidth = 0.5, color = "white") +
  scale_fill_manual(
    values = c(
      "Micro" = "#E0F0E0",
      "Minor" = "#90EE90",
      "Light" = "#FFD700",
      "Moderate" = "#FFA500",
      "Strong" = "#FF6347",
      "Major" = "#DC143C",
      "Great" = "#8B0000",
      "Unknown" = "#808080"
    )
  ) +
  labs(
    title = "Earthquake Distribution by Magnitude (Past Month)",
    subtitle = glue::glue("Total earthquakes: {nrow(earthquakes_categorized)}"),
    x = "Magnitude",
    y = "Count",
    fill = "Intensity"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right"
  )
# Save the plot
ggsave("earthquake_histogram.png", magnitude_plot, width = 10, height = 6, dpi = 300)
cat("Histogram saved as 'earthquake_histogram.png'\n")

# Generate R Markdown report
cat("\nGenerating HTML report...\n")
rmarkdown::render("earthquake_report.Rmd",
                  output_file = "earthquake_report.html",
                  quiet = TRUE)
cat("Report saved as 'earthquake_report.html'\n")
