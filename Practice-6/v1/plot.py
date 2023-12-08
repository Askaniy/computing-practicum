import numpy as np
import matplotlib.pyplot as plt
from pathlib import Path

# Определение папки с запущенным скриптом
path = Path(__file__).parent.resolve()

# Настройки форматирования
text_color = '#FFFFFF'
muted_color = '#A3A3A3'
highlight_color = '#5A5A5A'
bg_color = '#333333'
inputON_color = '#424242'
plt.rcParams |= {
    'text.color': text_color, 'axes.labelcolor': text_color,
    'axes.edgecolor': muted_color, 'xtick.color': muted_color, 'ytick.color': muted_color,
    'figure.facecolor': bg_color, 'axes.facecolor': bg_color,
    'axes.grid': True, 'grid.color': highlight_color
    }

def test_func_1(x, y, z):
    return np.array([x, y*y, z*z*z]) - 2

def test_func_3(x, y, z):
    return np.array([2*x+y+3*z, -x+y, -x+y])

def test_func_4(x, y, z): # default
    return np.array([x*x + y*y - 1, y*y - z*z, x - y + z])

solution = np.loadtxt(path/'result.dat')

cube_size = 5
arrow_scale = cube_size / 10
half_size = cube_size // 2

linspace = np.linspace(-half_size, half_size, 7)
x, y, z = np.meshgrid(linspace, linspace, linspace)
u, v, w = test_func_4(x, y, z)
lengths = np.sqrt(u**2 + v**2 + w**2)
lengths /= np.max(lengths)
lengths = lengths.flatten()
colors = np.zeros((lengths.size, 4))
colors[:,0] = lengths # red
colors[:,1] = lengths / 5 # green
colors[:,3] = lengths # alpha

fig = plt.figure(figsize=(10, 10), dpi=100)
ax = fig.add_subplot(111, projection='3d')
ax.quiver(x, y, z, u, v, w, length=arrow_scale, normalize=True, color=colors, label='Векторное поле')
ax.scatter3D(0, -1, -1, color='#00FF00', label='f(0, -1, -1) = 0')
ax.scatter3D(0, 1, 1, color='#00FF00', label='f(0, 1, 1) = 0')
ax.scatter3D(*solution, color='#00FFFF', label='Решение')
ax.set_xlabel('X axis')
ax.set_ylabel('Y axis')
ax.set_zlabel('Z axis')
ax.legend()
ax.view_init(elev=8, azim=-8, roll=0)

fig.tight_layout()
fig.savefig(path/'plot.png', dpi=120)
plt.show()
