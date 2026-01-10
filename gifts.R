library(tidyverse)
library(ellmer)
library(jsonlite)

# Set up the Anthropic client
chat <- chat_anthropic(
  model = "claude-4.5-sonnet",
  system_prompt = "create only valid JSON objects based on the provided text and example. Never include any additional text or explanation. Always use double-quotes for every key and value. No yapping, no hallucinations.",
  api_key = Sys.getenv("CLAUDE_API_KEY")
)

# Define the example JSON structure
example_json <- list(
  name_and_title = "name and title of the recipient",
  gift_description = "the gift",
  received = "the date received in yyyy-mm-dd format",
  estimated_value = "the dollar value only, no dollar sign",
  disposition = "the disposition of the gift, not including 'Disposition-'",
  foreign_donor = "name and title of foreign donor",
  circumstances = "why the gift was accepted"
)

# Function to extract information from text
extract_info <- function(text, example) {
  if (str_detect(text, "Honorable")) {
    # Create the prompt
    prompt <- str_glue(
      "Extract all Tangible Gifts contained in the following text into individual JSON objects based on this example: {toJSON(example, auto_unbox = TRUE)}\n\n{text}"
    )
    
    # Get response from Claude
    response <- chat$chat(prompt, echo = FALSE)
    return(response)
  } else {
    return("")
  }
}

# Function to read text and extract data
read_text_and_extract_data <- function(input_txt_path, output_json_path) {
  # Read the input text file
  content <- read_file(input_txt_path)
  
  # Split the text into sections
  sections <- str_split(content, "Federal Register / Vol\\. ")[[1]]
  
  # Initialize lists for data and failed extractions
  all_data <- list()
  failed_extractions <- c()
  
  # Iterate over sections and extract data
  for (section in sections) {
    if (str_trim(section) != "") {  # Skip empty sections
      extracted_info <- extract_info(section, example_json)
      
      if (extracted_info != "") {  # Check if extracted_info is not empty
        tryCatch({
          # Try to parse the JSON
          section_json <- fromJSON(extracted_info, simplifyVector = FALSE)
          
          # Handle both list and single object cases
          if (is.list(section_json) && !is.null(names(section_json[[1]]))) {
            # It's a list of objects
            all_data <- c(all_data, section_json)
          } else if (is.list(section_json) && is.null(names(section_json))) {
            # It's already a list
            all_data <- c(all_data, section_json)
          } else {
            # It's a single object
            all_data <- c(all_data, list(section_json))
          }
        }, error = function(e) {
          # Error handling for JSON parsing
          message(paste("Error decoding JSON:", e$message))
          message(paste("JSON string:", extracted_info))
          failed_extractions <<- c(failed_extractions, extracted_info)
        })
      }
    }
  }
  
  # Write the JSON output
  write_json(all_data, output_json_path, pretty = TRUE, auto_unbox = TRUE)
  
  # Save failed extractions if any
  if (length(failed_extractions) > 0) {
    save_failed_extractions_to_file(output_json_path, failed_extractions)
  }
  
  return(all_data)
}

# Function to save failed extractions
save_failed_extractions_to_file <- function(json_file_path, failed_extractions) {
  if (length(failed_extractions) > 0) {
    txt_file_path <- str_replace(json_file_path, "\\.json$", ".txt")
    
    # Write failed extractions
    write_lines(
      map_chr(failed_extractions, ~paste0("Failed extraction:\n", .x, "\n")),
      txt_file_path,
      append = TRUE
    )
  }
}

# Process files
files <- c("2020-03722", "2021-16751", "2022-07641", "2023-03806")

walk(files, function(file) {
  input_txt_path <- glue::glue("text/{file}.txt")
  output_json_path <- glue::glue("json/{file}.json")
  
  message(paste("Processing", file))
  read_text_and_extract_data(input_txt_path, output_json_path)
})
