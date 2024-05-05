import json
import re

def correct_missing_quotes(json_string):
    # Define a function to add missing single quotes
    def add_missing_quotes(match):
        # Extract the value and add a single quote if missing
        value = match.group(1)
        if value.endswith('"') and not value.endswith("'"):
            return f'"{value}\'"'
        return value
    
    # Use a regular expression to find values within quotes
    corrected_json_string = re.sub(r'"([^"]*)"([^"]*)"', add_missing_quotes, json_string)
    return corrected_json_string

# Read the original JSON file
with open('/users/usumusu/pubmed_medline_articles_aff.json', 'r') as file:
    original_json = file.read()

# Correct the JSON string
corrected_json = correct_missing_quotes(original_json)

# Write the corrected JSON back to a file
with open('/users/usumusu/pubmed_medline_articles_aff.json', 'w') as file:
    file.write(corrected_json)

print("JSON file has been corrected.")
