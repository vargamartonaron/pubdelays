# Publication Delays
## Dominik Dianovics, Marton A. Varga, Miklos Bognar, Balazs Aczel

This project aims to analyze the latest trends in accelarating reseach
by looking through the PubMed MEDLINE dataset.

# To replicate, run:
- ./src/get_and_verify_pubmed_xml.sh
- *note: change relevant directories in your script*
- move medline_parser.py to your pubmed_parser package directory
- and overwrite existing medline_parser.py
- python3 xmls2json.py
- Run raw_data processing scripts: scimago.R, wos.R, retraction_watch.R, npi.R, doaj.R
- Rscript process_data.R
- Rscript aggregate.R
- Rscript validate.R

# Analysis and respective figures were removed, as they do not represent the final version.

Beware that downloading and processing all data requires significant time, 
storage and computational resources.
