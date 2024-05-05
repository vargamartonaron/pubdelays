import pubmed_parser as pp
import json
from concurrent.futures import ProcessPoolExecutor
import gc
import logging
import resource
import multiprocessing
import os
import threading

logging.basicConfig(filename='parsing_log.txt', level=logging.DEBUG)
progress_lock = threading.Lock()

def get_remaining_files(parsed_files, all_files):
    return [xml_file for xml_file in all_files if os.path.basename(xml_file) not in parsed_files]

def print_progress(parsed_files, all_files):
    parsed_count = len(parsed_files)
    total_count = len(all_files)
    remaining_count = total_count - parsed_count
    print(f"Progress {parsed_count}/{total_count} files parsed")
    print(f"Remaining: {remaining_count} files to be parsed")
    logging.info(f"Progress: {parsed_count}/{total_count} files parsed")
    logging.info(f"Remaining: {remaining_count} files to be parsed")

def parse_and_save_to_json(xml_file, json_file, progress_file, parsed_files, path_xml_list):
    articles_list = []

    try:
        logging.info(f"Parsing {xml_file}")
        print(f"Parsing {xml_file}")

        with open(progress_file, 'r') as progress:
            parsed_files = progress.read().splitlines()
            if os.path.basename(xml_file) in parsed_files:
                logging.info(f"Skipping {xml_file} (already parsed)")
                return

        pubmed_dict = pp.parse_medline_xml(xml_file, year_info_only=False, nlm_category=False, reference_list=True, parse_downto_mesh_subterms=True)

        for article in pubmed_dict:
            articles_list.append(article)
        
        logging.info(f"Finished parsing {xml_file}")
        print(f"Finished parsing {xml_file}")

        # Clear variables and run garbage collection to free up memory
        del pubmed_dict
        gc.collect()
        
        # Export articles to JSON as separate objects within an array
        with open(json_file, 'a+') as json_file:
            for article in articles_list:
                if json_file.tell() > 2:
                    json_file.write(',\n')  # Add a comma and a new line for all but the first article
                json.dump(article, json_file, indent=4)

        # Update progress
        parsed_files.append(os.path.basename(xml_file))
        print(f"So far parsed: {parsed_files}")

        with progress_lock:
            with open(progress_file, 'a') as progress:
                progress.write(os.path.basename(xml_file) + '\n')
            

        print_progress(parsed_files, path_xml_list)

    except Exception as e:
        error_msg = f"Error parsing {xml_file}: {str(e)}"
        logging.error(error_msg)
        print(error_msg)
    

    # Clear articles_list explicitly at each iter
    del articles_list
    gc.collect()


if __name__ == "__main__":
    # Set memory limit
    memory_limit = 12 * 1024 * 1024 * 1024
    resource.setrlimit(resource.RLIMIT_AS, (memory_limit, memory_limit))

    # Available CPU cores
    num_cores = multiprocessing.cpu_count() - 1
    print(f"Number of CPU cores available and using one less: {num_cores}")

    # Define the directory containing XML files
    xml_dir = '/home/martonaronvarga/GitHub/pubdelays/Data/ftp.ncbi.nlm.nih.gov/pubmed/baseline'

    # List all XML files in the directory
    path_xml_list = pp.list_xml_path(xml_dir)

    # Path to output JSON
    json_file_path = '/home/martonaronvarga/GitHub/pubdelays/Data/pubmed_medline_articles_aff.json'
    
    # Path to progress file
    progress_file = '/home/martonaronvarga/GitHub/pubdelays/Data/progress.txt'
    
    # Init lists
    parsed_files = []
    
    # writing array to json 

    with open(json_file_path, 'w') as json_file:
        json_file.write('[\n')

    try:
        with open(progress_file, 'r+') as progress:
            parsed_files = progress.read().splitlines()
            print(f"Opened progress file: {progress_file}")
            print(f"Parsed files: {parsed_files}")

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
        futures = [executor.submit(parse_and_save_to_json, xml_file, json_file_path, progress_file, parsed_files, path_xml_list) for xml_file in remaining_files]

        # Wait for all tasks to complete
        for future in futures:
            try:
                future.result()
            except Exception as e:
                logging.error(f"Child process excpetion: {str(e)}")

    with open(json_file_path, 'a') as json_file:
        json_file.write('\n]')

    print(f"Saved articles to {json_file_path}")
