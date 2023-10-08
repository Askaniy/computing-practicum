import warnings
import numpy as np
import matplotlib.pyplot as plt

# Скопированные настройки из другого проекта TCT

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
    'figure.facecolor': bg_color, 'axes.facecolor': inputON_color,
    'axes.grid': True, 'grid.color': highlight_color
    }

x0, y0, p0 = np.loadtxt('data.dat').transpose()
e0 = 1 / p0 # визуализация погрешности
x1, y1 = np.loadtxt('result.dat').transpose()

fig = plt.Figure(figsize=(9, 6), dpi=100)
ax = fig.add_subplot(111)
ax.errorbar(x0, y0, e0, fmt='o', label='data', color='#00FFFF', ecolor='#00AAAA')
ax.plot(x1, y1, label='result', color='#108BB4')
ax.legend()
fig.savefig('plot.png', dpi=100)