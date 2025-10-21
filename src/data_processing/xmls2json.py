import pubmed_parser as pp
import json
from concurrent.futures import ProcessPoolExecutor
import logging
import multiprocessing
import os
import threading

logging.basicConfig(filename='parsing_log.txt', level=logging.DEBUG)
progress_lock = threading.Lock()

def get_remaining_files(parsed_files, all_files):
    return [xml_file for xml_file in all_files if os.path.basename(xml_file) not in parsed_files]

def list_xml_path(path_dir):
    fullpath = [
        os.path.join(dp, f)
        for dp, dn, fn in os.walk(os.path.expanduser(path_dir))
        for f in fn
    ]
    path_list = [
        folder
        for folder in fullpath
        if os.path.splitext(folder)[-1] in (".nxml", ".xml") or folder.endswith(".xml.gz")
    ]
    return path_list

def print_progress(parsed_files, all_files):
    parsed_count = len(parsed_files)
    total_count = len(all_files)
    remaining_count = total_count - parsed_count
    print(f"Progress {parsed_count}/{total_count} files parsed")
    print(f"Remaining: {remaining_count} files to be parsed")
    logging.info(f"Progress: {parsed_count}/{total_count} files parsed")
    logging.info(f"Remaining: {remaining_count} files to be parsed")

def parse_and_save_to_json(xml_file, progress_file, parsed_files, path_xml_list):

    try:
        logging.info(f"Parsing {xml_file}")

        with open(progress_file, 'r') as progress:
            parsed_files = progress.read().splitlines()
            if os.path.basename(xml_file) in parsed_files:
                logging.info(f"Skipping {xml_file} (already parsed)")
                return

        pubmed_dicts = pp.parse_medline_xml(xml_file, year_info_only=False, nlm_category=False, reference_list=False, parse_downto_mesh_subterms=True)
        data = [obj for obj in pubmed_dicts]

        logging.info(f"Finished parsing {xml_file}")

        with open(f"/mnt/st04pool/users/zsimi/pubdelays/data/raw_data/pubmed/jsons/{os.path.basename(xml_file)}.json", "w") as f:
            json.dump(data, f, indent=4)


        # Update progress
        parsed_files.append(os.path.basename(xml_file))

        with progress_lock:
            with open(progress_file, 'a') as progress:
                progress.write(os.path.basename(xml_file) + '\n')
            

        print_progress(parsed_files, path_xml_list)

    except Exception as e:
        error_msg = f"Error parsing {xml_file}: {str(e)}"
        logging.error(error_msg)
        print(error_msg)
    

if __name__ == "__main__":

    # Available CPU cores
    num_cores = multiprocessing.cpu_count() - 1
    print(f"Number of CPU cores available and using one less: {num_cores}")

    # Define the directory containing XML files
    xml_dir = '/mnt/st04pool/users/zsimi/pubdelays/data/raw_data/pubmed/xmls'
    print(f"Defined XML files in the following directory: {xml_dir}")

    # List all XML files in the directory
    path_xml_list = list_xml_path(xml_dir)

    # Path to progress file
    progress_file = '/mnt/st04pool/users/zsimi/pubdelays/data/raw_data/progress.txt'
    
    # Init lists
    parsed_files = []
    

    try:
        with open(progress_file, 'r+') as progress:
            parsed_files = progress.read().splitlines()
            print(f"Opened progress file: {progress_file}")

    except FileNotFoundError:
    # Handle the case where the file doesn't exist
        parsed_files = []  # Initialize with an empty list
        with open(progress_file, 'w') as progress:
            print(f"Created progress file: {progress_file}")
            pass  # Create an empty file  

    
    remaining_files = get_remaining_files(parsed_files, path_xml_list)
    
    # Get initial progress
    print_progress(parsed_files, path_xml_list)

    # Create a ProcessPoolExecutor to parallelize the parsing
    with ProcessPoolExecutor(max_workers=num_cores) as executor:
        # Submit all parsing tasks to the executor
        futures = [executor.submit(parse_and_save_to_json, xml_file, progress_file, parsed_files, path_xml_list) for xml_file in remaining_files]

        # Wait for all tasks to complete
        for future in futures:
            try:
                future.result()
            except Exception as e:
                logging.error(f"Child process excpetion: {str(e)}")