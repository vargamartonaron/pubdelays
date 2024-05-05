import re
import json

def escape_single_quotes_in_value_pairs(line):
    # Regular expression to match single quotes within value pairs
    pattern = r'(?<=\{)([^"]*"(?:\\.|[^"\\])*"([^"]*)\})'
    # Replace single quotes with escaped single quotes
    return re.sub(pattern, r'\1"\2"', line)

def replace_single_quotes_with_double_quotes(line):
    # Escape single quotes in value pairs
    escaped_line = escape_single_quotes_in_value_pairs(line)
    # Replace single quotes with double quotes
    return escaped_line.replace("'", '"')

def process_json_file(input_file, output_file):
    with open(input_file, 'r') as file:
        lines = file.readlines()
    
    corrected_lines = [replace_single_quotes_with_double_quotes(line) for line in lines]
    
    with open(output_file, 'w') as file:
        file.writelines(corrected_lines)

# Example usage
process_json_file('/users/usumusu/pubmed_medline_articles_aff.json', '/users/usumusu/pubmed_medline_articles_aff.json')
