# This program takes mp4 files in a shared folder and creates a new sibling folder of each video's first frame as jpg
# Christian Anderson, 6/20/2023, taken from https://note.nkmk.me/en/python-opencv-video-to-still-image/
# python version 3.10.11
import cv2
import os

#define frame extraction function
def save_frame(video_path, frame_num, result_path): #takes in path for the video, which frame you want, and where to put image file
    #save the video to a cv2 data type 
    cap = cv2.VideoCapture(video_path)
    
    #check if the video was successfully read
    if not cap.isOpened():
        return print("cap not open")

    #make location for new file where image will go
    os.makedirs(os.path.dirname(result_path), exist_ok=True)

    #use set function to change the video to a specific frame
    cap.set(cv2.CAP_PROP_POS_FRAMES, frame_num)
    
    #read frame to another object from 
    ret, frame = cap.read()

    if ret:
        cv2.imwrite(result_path, frame) #write new image to new path
        print(result_path, "created")

    cap.release()#close opened file
    cv2.destroyAllWindows() #clear data?

#--------------------------------------------------------------------------------------------------------------------------

#Run function on files in for a video directory 

#----------------------------------------------------------------------------------------------------------------

path =\
    "C:/Users/chris/Documents/Research/WildID/VALLEdeSILENCIO_2023(mp4_jpg)/VALLE DE SILENCIO/La Amistad monitoring Oko jaguara/10 km - No 16 - 9.0928181N, 82.9778376W"

if path[-1] != "/":
        path += "/" #add slash if not there (not included with paste)

#new folder for images
image_dir_name= "10 km - No 16_stills/" #make sure there's a "/"

files= os.listdir(path)#make list of files within video directory

for file_name in files: #iterate through each filename

#build paths
        vid_path= path+file_name #append filename to path

        #go up a level to be sibling of image directory (this makes an image folder separate from vid folder for convenience)
        new_path = path
#if vid dir in subfolder
        #new_path = path.rsplit("/", 2 )[0] #split path string two times, working from the right until first occurance of "/". And then use the first item of split [0]. 

        if new_path[-1] != "/":
                new_path += "/" #make sure end slash

        new_path +=  image_dir_name #make new folder as sibling to vid folder
        new_path += file_name[:-4] + ".jpg" #make same name with different tag

#apply function to paths
        save_frame(vid_path, 1, new_path) #run function on video path for first frame; output to new path
        print(file_name+" created", sep= '')