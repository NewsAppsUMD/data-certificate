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
