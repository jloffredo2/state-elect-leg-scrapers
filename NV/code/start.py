from index_to_json import bill_index_to_json
from basedata import process_basedata
from metadata import process_metadata
from sponsors import process_sponsors
from sponsorsearch import sponsor_search
from history import process_history
from votes import process_votes
from combiner import process_combiner


if __name__ == '__main__':
    #Create the Base Set of JSON files
    bill_index_to_json(script_dir="NV/intermediate/index_to_json")
    process_basedata(input_dir="NV/intermediate/index_to_json", output_dir="NV/intermediate/basedata")




    #Then u can run any or all of the following functions to process the data.
    #Just make sure to run sponsor_search before process_sponsors

    process_metadata(input_dir="NV/intermediate/basedata", output_dir="NV/intermediate/metadata")
    sponsor_search(input_dir="NV/intermediate/basedata", output_dir="NV/intermediate/sponsors")
    process_sponsors(input_dir="NV/intermediate/basedata", output_dir="NV/intermediate/sponsors")
    process_history(input_dir="NV/intermediate/basedata", output_dir="NV/intermediate/history")
    process_votes(input_dir="NV/intermediate/basedata", output_dir="NV/intermediate/votes")
    
    
    # Finally, combine all the JSON files into a single output directory
    process_combiner(input_dir="NV/intermediate", output_dir="NV/output")

    print("All processing complete. Output files are in the 'output' directory.")