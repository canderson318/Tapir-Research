import os

files = os.listdir("C:/Users/chris/Documents/Research/WildID/VALLEdeSILENCIO_2023(mp4_jpg)/Animals/La Amistad monitoring Oko jaguara")

with open("valleSilencioFiles.txt", "w") as fout:
    for file in files:
        fout.write(file + '\n')
