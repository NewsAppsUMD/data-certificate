library(tidyverse)
library(ellmer)

# Create a single chat object with examples
create_classifier_chat <- function() {
  prompt <- "
You are an AI model trained to classify text.

I will provide the name of a professional soccer team. You will reply with the league in which they compete.

Your responses must come from the following list:
- English Premier League (EPL)
- La Liga (Spain)
- Serie A (Italy)
- Bundesliga (Germany)
- Ligue 1 (France)
- Major League Soccer (MLS)

If the team's league is not on the list, you should label them as 'Other'.

Examples:
- Manchester United: English Premier League (EPL)
- Real Madrid: La Liga (Spain)
- AC Milan: Serie A (Italy)
- Bayern Munich: Bundesliga (Germany)
- Paris Saint-Germain: Ligue 1 (France)
- LA Galaxy: Major League Soccer (MLS)
- Celtic FC: Other
"

  chat_groq(
    system_prompt = prompt,
    model = "openai/gpt-oss-120b"
  )
}

# Function to classify international soccer teams
classify_team <- function(chat, name) {

  # Get response for the team name
  response <- chat$chat(name)

  # The response is already just the text string
  answer <- as.character(response)

  # Define acceptable answers
  acceptable_answers <- c(
    "English Premier League (EPL)",
    "La Liga (Spain)",
    "Serie A (Italy)",
    "Bundesliga (Germany)",
    "Ligue 1 (France)",
    "Major League Soccer (MLS)",
    "Other"
  )

  # Validate response
  if (!answer %in% acceptable_answers) {
    stop(paste0(answer, " not in list of acceptable answers"))
  }

  return(answer)
}

# Create the chat object once
chat <- create_classifier_chat()

# Example usage
teams <- c("Arsenal", "Barcelona", "Juventus", "Borussia Dortmund",
           "Lyon", "Seattle Sounders", "Ajax Amsterdam")

# Use map_chr with the chat object passed to each call
team_classifications <- tibble(
  team = teams,
  league = map_chr(teams, ~classify_team(chat, .x))
)

print(team_classifications)
