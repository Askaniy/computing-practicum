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


def amplitude_generator():
    sr = 100 # sampling rate
    ts = 1. / sr # sampling interval
    t = np.arange(0,1,ts)
    freq = 1.
    x = 3*np.sin(2*np.pi*freq*t)
    freq = 4
    x += np.sin(2*np.pi*freq*t)
    freq = 7   
    x += 0.5* np.sin(2*np.pi*freq*t)
    return t, x

# Генерируем сигнал
x0, y0 = amplitude_generator()
with open('data.dat', 'w') as file:
    file.write(f'# {len(x0)}\n')
    for y in y0:
        file.write(f'{y}\t0.\n')

# Читаем спектр
#y1_re, y1_im = np.loadtxt('result.dat').transpose()
#y1_abs = np.loadtxt('abs.dat')

def dft(x, sign):
    N = len(x)
    n = np.arange(N)
    k = n.reshape((N, 1))
    e = np.exp(sign * 2j * np.pi * k * n / N)
    return np.dot(e, x) / np.sqrt(N)

y1 = dft(y0, sign=-1)
y1_re = y1.real
y1_im = y1.imag
y1_abs = abs(y1)

#y0_ = dft(y1, sign=1)

fig, (ax0, ax1) = plt.subplots(2, 1, figsize=(7, 5), dpi=100)
ax0.plot(x0, y0, label='Amplitude', color='#108BB4')
#ax0.plot(x0, y0_, label='Amplitude recalc', color='#B48B10')
ax0.legend()
ax1.plot(x0, y1_re, label='Real part', color='#00FF99')
ax1.plot(x0, y1_im, label='Imaginary part', color='#0099FF')
#ax1.plot(x0, y1_abs, label='Absolute', color='#FFFFFF')
ax1.legend()
fig.savefig('plot.png', dpi=150)
plt.show()
