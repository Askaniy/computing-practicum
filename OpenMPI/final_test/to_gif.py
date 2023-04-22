from pathlib import Path
from PIL import Image
import numpy as np

def to_img(arr, mx):
    img = Image.fromarray((arr * 255 / mx).astype('int8'))
    return img.resize(tuple(np.array(img.size)*16), resample=Image.Resampling.NEAREST)

data = []

for file in Path('./data/').iterdir():
    if file.suffix == '.dat' and not file.is_dir():
        with open(file) as f:
            file_data = np.loadtxt(f)
            data.append(file_data)
            print(f'{file} прочитан')

name = 'cat.gif'

mx = np.max(data)
img0 = to_img(data[0], mx)
img0.save(name, save_all=True, loop=0, append_images=[to_img(frame, mx) for frame in data[1:]])
print(f'Анимация {name} успешно составлена и сохранена')