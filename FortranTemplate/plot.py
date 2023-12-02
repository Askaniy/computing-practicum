import numpy as np
import matplotlib.pyplot as plt
from warnings import simplefilter

# Настройки форматирования

# Filling empty space on a plot
simplefilter('ignore', UserWarning)
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

x1, y1 = np.loadtxt('result.dat').transpose()

fig, ax1 = plt.subplots(1, 1, figsize=(8, 6), dpi=100)
ax1.set_title('Результат')
ax1.plot(x1, y1, color='#108BB4')
fig.savefig('plot.png', dpi=100)
#plt.show()
