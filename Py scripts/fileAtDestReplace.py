import os
import shutil

def copy_existing_files(source_dir, destination_dir):

    # Get the list of files in the source directory
    source_files = os.listdir(source_dir)
    
    # Iterate through the files in the source directory
    for file_name in source_files:
        source_file_path = os.path.join(source_dir, file_name)
        destination_file_path = os.path.join(destination_dir, file_name)
        
        # Check if the file exists in the destination directory
        if os.path.isfile(destination_file_path):
            # Replace the existing file with the one from the source directory
            shutil.copy2(source_file_path, destination_file_path)
            print(f"Replaced: {destination_file_path}")
            
        else:
            print(f"Skipping: {destination_file_path} (File does not exist)")

# Example usage

source_directory = "WildID/Raw/KAMUK_1"

destination_directory = "WildID/AnimalsNew/KAMUK_1"

copy_existing_files(source_directory, destination_directory)




