import cv2
import os


def save_frame(video_path, frame_num, result_path):
    
    cap = cv2.VideoCapture(video_path)

    if not cap.isOpened():
        return print("cap not opened")


    os.makedirs(os.path.dirname(result_path), exist_ok=True)

    cap.set(cv2.CAP_PROP_POS_FRAMES, frame_num)

    ret, frame = cap.read()

    if ret:
        cv2.imwrite(result_path, frame)

vid_name= "._VIDEO-2022-10-04-14-22-57.mp4"
vid_dir = "WildID/VALLEdeSILENCIO_2023(mp4&jpg)/VALLE DE SILENCIO/" 
vid_path = vid_dir + vid_name
which_frame = 1
img_path = vid_dir + vid_name[:-4]+".jpg" #adds video directory to file name -.mp4 and adds ".jpg"

save_frame(vid_path, which_frame, img_path)

# C:\Users\chris\Documents\Research\WildID\VALLEdeSILENCIO_2023(mp4&jpg)\VALLE DE SILENCIO\._VIDEO-2022-10-04-14-22-57.mp4