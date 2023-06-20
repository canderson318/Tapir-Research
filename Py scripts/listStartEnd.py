import os
import datetime
from PIL import Image
from PIL.ExifTags import TAGS

# def get_image_datetime(image_path):
#     # Extract the creation time of the image file
#     timestamp = os.path.getctime(image_path)
#     return datetime.datetime.fromtimestamp(timestamp)

def get_capture_date(image_path):
    with Image.open(image_path) as img:
        exif_data = img._getexif()
        if exif_data is not None:
            for tag_id, value in exif_data.items():
                tag_name = TAGS.get(tag_id)
                if tag_name == "DateTimeOriginal":
                    return value
    return None


def process_image_folders(root_folder):

    # Iterate over each subfolder in the root folder
    mydict= {}
    for folder_name in os.listdir(root_folder):
        folder_path = os.path.join(root_folder, folder_name)
        mydict[folder_name] = {}
        if os.path.isdir(folder_path):
            
            # Iterate over each image file in the folder
            for image_name in os.listdir(folder_path):
                image_path = os.path.join(folder_path, image_name)
                
                if os.path.isfile(image_path):
                    image_datetime = get_capture_date(image_path)
                    
                    #datetime.fromtimestamp(timestamp).strftime("%Y-%m-%d")
                    mydict[folder_name].update({image_name: image_datetime})
    return mydict
               

def print_dict_to_txt(dictionary, file_path):
    with open(file_path, 'w') as file:
        for key, value in dictionary.items():
            file.write(f"{key}: {value}\n")

                
# Example usage
root_folder = "WildID\\StartEnd"
dictionary = process_image_folders(root_folder)

print_dict_to_txt(dictionary, 'startEnd.txt')

