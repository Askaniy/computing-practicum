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

t0, x0, y0 = np.loadtxt('sp.dat').transpose()
t1, x1, y1 = np.loadtxt('rk.dat').transpose()
t2, x2, y2 = np.loadtxt('ae.dat').transpose()
t3, x3, y3 = np.loadtxt('ai.dat').transpose()

fig, ((ax0, ax1), (ax2, ax3)) = plt.subplots(2, 2, figsize=(8, 6), dpi=100)
ax0.set_title('Интуитивное интегрирование')
ax0.plot(x0, y0, color='#108BB4')
ax1.set_title('Метод Рунге-Кутты')
ax1.plot(x1, y1, color='#108BB4')
ax2.set_title('Экстраполяционный метод Адамса')
ax2.plot(x2, y2, color='#108BB4')
ax3.set_title('Интерполяционный метод Адамса')
ax3.plot(x3, y3, color='#108BB4')
fig.savefig('plot.png', dpi=100)
#plt.show()
