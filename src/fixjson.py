import json

def fix_json_line(line):
    # This function should be customized based on the specific issues in your JSON data
    # For simplicity, this example just replaces single quotes with double quotes
    return line.replace("'", '"')

with open('/usumusu/pubmed_medline_articles_aff.json', 'r') as file:
    lines = file.readlines()

fixed_lines = [fix_json_line(line) for line in lines]

with open('/usumusu/pubmed_medline_articles_aff.json', 'w') as file:
    file.writelines(fixed_lines)
