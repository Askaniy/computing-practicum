from pathlib import Path
#from PIL import Image, ImageDraw
import numpy as np
from array2gif import write_gif

def colorizing(value):
    return [value] * 3
color = np.vectorize(colorizing)


data = []

for file in Path('./data/').iterdir():
    if file.suffix == '.dat' and not file.is_dir():
        with open(file) as f:
            n = int(f.readline().split('#')[-1])
            file_data = np.loadtxt(f)
            print(f'{file} прочитан')
            data.append(np.reshape([file_data]*3, (3,n,n)))


data = np.array(data) / np.max(data) * 255

#Image.fromarray(data)

write_gif(data.astype('uint8'), 'test.gif', fps=5)