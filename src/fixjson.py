import json

def fix_json_line(line):
    try:
        # Parse the JSON string
        data = json.loads(line)
        
        # Function to recursively traverse and modify the JSON object
        def replace_quotes(obj):
            if isinstance(obj, dict):
                for key, value in obj.items():
                    obj[key] = replace_quotes(value)
            elif isinstance(obj, list):
                for item in obj:
                    obj[item] = replace_quotes(item)
            else:
                # Check if the value is a string and contains single quotes
                if isinstance(obj, str) and "'" in obj:
                    # Assuming apostrophes are surrounded by alphanumeric or spaces
                    if all(c.isalnum() or c.isspace() for c in obj):
                        # Replace single quotes with double quotes
                        obj = obj.replace("'", '"')
            return obj
        
        # Recursively replace quotes in the parsed JSON
        fixed_data = replace_quotes(data)
        
        # Reconstruct the JSON string
        fixed_line = json.dumps(fixed_data)
        
        return fixed_line
    except json.JSONDecodeError:
        # Handle cases where the line is not valid JSON
        return line

# Example usage
with open('/users/usumusu/pubmed_medline_articles_aff.json', 'r') as file:
    lines = file.readlines()

fixed_lines = [fix_json_line(line) for line in lines]

with open('/users/usumusu/pubmed_medline_articles_aff.json', 'w') as file:
    file.writelines(fixed_lines)
