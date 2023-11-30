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


# Генерируем сигнал
step = 0.01
x0 = np.arange(0,1,step)
y0 = 3*np.sin(2*np.pi*x0 * 1) + np.cos(2*np.pi*x0 * 4) + 0.5*np.sin(2*np.pi*x0 * 7)
with open('data.dat', 'w') as file:
    file.write(f'# {len(x0)}\n')
    for y in y0:
        file.write(f'{y}\t0.\n')

# Читаем спектр
y1_re, y1_im = np.loadtxt('result.dat', skiprows=1).transpose()
y1_abs = np.loadtxt('abs.dat')
x1 = np.arange(0, y1_abs.size*step, step)

# Рисуем график
fig, (ax0, ax1) = plt.subplots(2, 1, figsize=(7, 5), dpi=100)
ax0.plot(x0, y0, label='Исходный сигнал', color='#108BB4')
ax0.legend()
ax1.plot(x1, y1_re, label='Вещественная часть', color='#00FF99')
ax1.plot(x1, y1_im, label='Мнимая часть', color='#0099FF')
#ax1.plot(x0, y1_abs, label='Absolute', color='#FFFFFF')
ax1.legend()
fig.savefig('plot.png', dpi=150)
#plt.show()
