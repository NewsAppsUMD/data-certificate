import os, sys, re
import subprocess
import tempfile

sys.path.append('/usr/share/codio/assessments')
from lib.grade import send_partial_v2, FORMAT_V2_MD, FORMAT_V2_HTML, FORMAT_V2_TXT

passed = 0

with open('r-files/eval1-text-gifts.Rmd') as response:
    answer = response.read()

# Extract the prompt creation code block
prompt_section = re.search(r'```\{r create-prompt\}(.*?)```', answer, re.DOTALL)

if not prompt_section:
    print("Could not find the create-prompt code block.")
    sys.exit(1)

prompt_code = prompt_section.group(1)

# Check that they filled in all six keys
name_check = re.search(r'name_and_title', prompt_code, re.IGNORECASE)
gift_check = re.search(r'gift_description', prompt_code, re.IGNORECASE)
date_check = re.search(r'date', prompt_code, re.IGNORECASE)
value_check = re.search(r'value', prompt_code, re.IGNORECASE)
donor_check = re.search(r'foreign_donor', prompt_code, re.IGNORECASE)
circumstances_check = re.search(r'circumstances', prompt_code, re.IGNORECASE)

if not name_check:
    print("Make sure you included 'name_and_title' as one of the keys in your prompt.")
    sys.exit(1)

if not gift_check:
    print("Make sure you included 'gift_description' as one of the keys in your prompt.")
    sys.exit(1)

if not date_check:
    print("Make sure you included 'date' as one of the keys in your prompt.")
    sys.exit(1)

if not value_check:
    print("Make sure you included 'value' as one of the keys in your prompt.")
    sys.exit(1)

if not donor_check:
    print("Make sure you included 'foreign_donor' as one of the keys in your prompt.")
    sys.exit(1)

if not circumstances_check:
    print("Make sure you included 'circumstances' as one of the keys in your prompt.")
    sys.exit(1)

# Try to run the R code and check the dataframe
try:
    # Extract all necessary code blocks
    api_setup_match = re.search(r'```\{r api-setup\}(.*?)```', answer, re.DOTALL)
    read_data_match = re.search(r'```\{r read-data\}(.*?)```', answer, re.DOTALL)
    prompt_match = re.search(r'```\{r create-prompt\}(.*?)```', answer, re.DOTALL)
    
    if not all([api_setup_match, read_data_match, prompt_match]):
        print("Could not find all required code blocks. Make sure you haven't deleted any code.")
        sys.exit(1)
    
    api_setup_code = api_setup_match.group(1).strip()
    read_data_code = read_data_match.group(1).strip()
    prompt_student_code = prompt_match.group(1).strip()
    
    # Check if API key is still placeholder
    if 'YOUR GROQ API KEY HERE' in api_setup_code:
        print("Please replace 'YOUR GROQ API KEY HERE' with your actual Groq API key.")
        sys.exit(1)
    
    # Create a temporary R script that runs their code and outputs results
    r_script = f"""
    setwd('/home/codio/workspace')
    suppressPackageStartupMessages({{
      library(tidyverse)
      library(ellmer)
      library(jsonlite)
      library(lubridate)
    }})
    
    # API setup
    {api_setup_code}
    
    # Read data
    gifts_text <- readLines("data/foreign_gifts.txt", warn = FALSE)
    gifts_content <- paste(gifts_text, collapse = "\\n")
    
    # Create prompt and get response
    {prompt_student_code}
    
    # Clean the response - remove markdown code fences and any trailing text
    clean_response <- structured_response
    clean_response <- gsub("```json|```", "", clean_response)
    clean_response <- trimws(clean_response)
    
    # Extract just the JSON array (from first [ to last ])
    json_start <- regexpr("\\\\[", clean_response)[1]
    json_end <- tail(gregexpr("\\\\]", clean_response)[[1]], 1)
    
    if (json_start > 0 && json_end > 0) {{
      clean_response <- substr(clean_response, json_start, json_end)
    }}
    
    # Create dataframe with cleaned response
    gifts_df <- fromJSON(clean_response) |>
      as_tibble() |>
      mutate(
        date = ymd(date),
        value = as.numeric(value)
      )
    
    # Output results with clear markers
    cat("ROWCOUNT:", nrow(gifts_df), "\\n")
    cat("FIRSTNAME:", gifts_df$name_and_title[1], "\\n")
    """
    
    # Write to temp file and execute
    with tempfile.NamedTemporaryFile(mode='w', suffix='.R', delete=False) as f:
        f.write(r_script)
        temp_file = f.name
    
    result = subprocess.run(['Rscript', temp_file], capture_output=True, text=True, timeout=60)
    
    # Keep the temp file for debugging if there's an error
    if result.returncode != 0:
        print("There was an error running your R code:")
        print(result.stderr)
        print("\nGenerated R script saved to:", temp_file)
        print("You can inspect it for debugging.")
        sys.exit(1)
    
    os.unlink(temp_file)
    
    if result.returncode == 0:
        try:
            # Parse output looking for our markers
            output = result.stdout
            
            # Extract row count
            rowcount_match = re.search(r'ROWCOUNT:\s*(\d+)', output)
            firstname_match = re.search(r'FIRSTNAME:\s*(.+?)(?:\n|$)', output)
            
            if not rowcount_match or not firstname_match:
                print("Could not parse the output from your code.")
                print("\nDebug output:")
                print(output)
                sys.exit(1)
            
            num_rows = int(rowcount_match.group(1))
            first_name = firstname_match.group(1).strip()
            
            expected_name = "The Honorable Joseph R. Biden Jr., President of the United States."
            
            if num_rows == 38 and first_name == expected_name:
                print(f"Excellent! Your extraction successfully created a dataframe with {num_rows} rows.")
                print(f"The first record correctly shows: {first_name}")
                print("Your structured data extraction is working correctly!")
                sys.exit(0)
            elif num_rows == 38:
                print(f"Your dataframe has the correct number of rows ({num_rows}).")
                print(f"However, the first record's name doesn't match.")
                print(f"Expected: {expected_name}")
                print(f"Got: {first_name}")
                print("\nCheck your prompt to ensure proper extraction.")
                sys.exit(1)
            elif num_rows > 0:
                print(f"Your code created a dataframe with {num_rows} rows, but expected 38.")
                print("Check your prompt to ensure it's extracting all the gifts correctly.")
                sys.exit(1)
            else:
                print("Your dataframe has 0 rows. The extraction may have failed.")
                print("Check that your API key is correct and your prompt keys are properly formatted.")
                sys.exit(1)
        except (ValueError, IndexError) as e:
            print("Could not determine the dataframe details.")
            print("Make sure your code successfully creates the gifts_df dataframe.")
            print("\nDebug output:")
            print(result.stdout)
            print(f"\nError: {e}")
            sys.exit(1)
        
except Exception as e:
    print(f"Error checking your code: {str(e)}")
    print("\nMake sure your code follows the template structure.")
    sys.exit(1)
