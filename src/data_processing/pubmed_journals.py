import urllib.request
import csv

# FTP URL
ftp_url = 'ftp://ftp.ncbi.nih.gov/pubmed/J_Medline.txt'

# Fetch data from FTP
with urllib.request.urlopen(ftp_url) as response:
    data = response.read().decode('utf-8')

# Initialize CSV writer
keys = ['JrId', 'JournalTitle', 'MedAbbr', 'ISSN (Print)', 'ISSN (Online)', 'IsoAbbr', 'NlmId']
output_csv = open('pubmed-journals.csv', 'w', newline='')
csv_writer = csv.writer(output_csv)
csv_writer.writerow(keys)

# Process data
data_lines = data.split('\n')
data_dict = dict.fromkeys(keys, None)

for line in data_lines:
    if line.startswith('-'):
        if any(data_dict.values()):
            csv_writer.writerow(list(data_dict.values()))
            data_dict = dict.fromkeys(keys, None)
        continue

    if ':' in line:
        key, value = map(str.strip, line.split(':', 1))
        data_dict[key] = value

# Close CSV file
output_csv.close()
