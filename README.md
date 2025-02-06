# Publication Delays
## Dominik Dianovics, Marton A. Varga, Balazs Aczel

This project aims to analyze the latest trends in accelarating reseach
by looking through the PubMed MEDLINE dataset.

# To replicate, run:
- ./src/get_and_verify_pubmed_xml.sh
- *note: change relevant directories in your script*
- move medline_parser.py to your pubmed_parser package directory
- and overwrite existing medline_parser.py
- python3 xmls2json.py
- Rscript process_data.R
- Rscript aggregate.R
- proceed with plotting or analyses
