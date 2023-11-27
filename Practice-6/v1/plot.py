import warnings
import numpy as np
import matplotlib.pyplot as plt

# Скопированные настройки из другого проекта, TCT

# Filling empty space on a plot
warnings.simplefilter('ignore', UserWarning)
plt.rcParams['figure.autolayout'] = True

# MatPlotLib custom theme
# https://matplotlib.org/stable/tutorials/introductory/customizing.html
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

def test_func_2(x, y, z):
    return np.array([2*x+y+3*z, -x+y, -x+y])

solution = np.loadtxt('result.dat')

linspace = np.linspace(-10, 10, 10)
x, y, z = np.meshgrid(linspace, linspace, linspace)
u, v, w = test_func_2(x, y, z)
lengths = np.sqrt(u**2 + v**2 + w**2)
lengths /= np.max(lengths)
lengths = lengths.flatten()
colors = np.zeros((lengths.size, 4))
colors[:,0] = lengths # red
colors[:,1] = lengths / 5 # green
colors[:,3] = lengths # alpha

fig = plt.figure(figsize=(10, 10), dpi=100)
ax = fig.add_subplot(111, projection='3d')
ax.quiver(x, y, z, u, v, w, length=2, normalize=True, color=colors, label='Векторное поле')
ax.plot(linspace, linspace, -linspace, color='#108BB4', label='Линия решений')
ax.scatter3D(*solution, color='#00FF00', label='Решение')
ax.set_xlabel('X axis')
ax.set_ylabel('Y axis')
ax.set_zlabel('Z axis')
ax.legend()
fig.savefig('plot.png', dpi=100)
plt.show()
