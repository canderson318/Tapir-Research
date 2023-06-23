from mutagen.mp4 import MP4

def extract_metadata(file_path):
    try:
        metadata = MP4(file_path)
        return metadata
    except Exception as e:
        print(f"Error extracting metadata: {str(e)}")
        return None

# Example usage
file_path = "C:/Users/chris/Documents/Research/WildID/VALLEdeSILENCIO_2023(mp4_jpg)/La Amistad monitoring Oko jaguara/1 km - No 1 - 9.0322142N, 83.0011695W/VD_00006.MP4"
metadata = extract_metadata(file_path)

if metadata is not None:
    print("Metadata:", MP4(file_path))

    # for key, value in metadata.items():
    #     print(f"{key}: {value}")