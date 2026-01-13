library(tidyverse)

# Read the data
earthquakes <- read_csv("all_month.csv")

# Filter based on the criteria
filtered_earthquakes <- earthquakes |>
  filter(mag >= 5 & str_detect(place, "Japan"))

# Generate sentences for each earthquake
earthquake_sentences <- filtered_earthquakes |>
  mutate(
    date = format(time, "%B %d, %Y"),
    time_formatted = format(time, "%H:%M:%S UTC"),
    sentence = glue::glue("At {time_formatted} on {date}, an earthquake of {mag} was recorded {place}.")
  )

# View the sentences
earthquake_sentences |>
  select(sentence) |>
  pull()
