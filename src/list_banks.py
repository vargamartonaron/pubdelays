import json

json_data = '/home/martonaronvarga/GitHub/pubdelays/Data/pubmed_medline_articles_aff.json'

# Load the JSON data
with open(json_data, 'r') as file:
    data = json.load(file)

# Extract affiliations
affiliations = data.get('affiliations', [])

# Find matches containing "Bank" or "bank"
matches = set()
for affiliation in affiliations:
    if "Bank" in affiliation or "bank" in affiliation:
        matches.add(affiliation)

# List unique matches
print("Unique matches containing 'Bank' or 'bank':")
for match in matches:
    print(match)
    # save to txt
    with open('list_banks.txt', 'w') as f:
        f.write(match)