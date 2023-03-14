import fortran as f
import numpy as np
import pygame
import time

def next_frame(matrix):
    new_matrix = np.zeros_like(matrix)
    rows, cols = matrix.shape
    for i in range(rows):
        for j in range(cols):
            count = np.sum(matrix[max(0, i-1):min(rows, i+2), max(0, j-1):min(cols, j+2)]) - matrix[i, j]
            if matrix[i, j] == 1:
                if count < 2 or count > 3:
                    new_matrix[i, j] = 0
                else:
                    new_matrix[i, j] = 1
            else:
                if count == 3:
                    new_matrix[i, j] = 1
    return new_matrix


# Инициализация Pygame
pygame.init()
WINDOW_SIZE = (600, 400)
WHITE = (197, 197, 197)
BLACK = (30, 30, 30)
CELL_SIZE = 2
screen = pygame.display.set_mode(WINDOW_SIZE)

# Вывод поля на экран
def print_matrix(matrix):
    screen.fill(BLACK)
    rows, cols = matrix.shape
    for y in range(rows):
        for x in range(cols):
            rect = pygame.Rect(y*CELL_SIZE, x*CELL_SIZE, CELL_SIZE, CELL_SIZE)
            if matrix[y, x] == 1:
                pygame.draw.rect(screen, WHITE, rect)
            else:
                pygame.draw.rect(screen, BLACK, rect, 1)
    pygame.display.flip()

# Создаем начальную матрицу

# Запускаем игру

    #for event in pygame.event.get():
    #    if event.type == pygame.QUIT:
    #        running = False
#running = True

matrix = np.random.randint(2, size=(int(WINDOW_SIZE[0]/CELL_SIZE), int(WINDOW_SIZE[1]/CELL_SIZE)))
n = 0
start = time.time()
while n < 10:
    n += 1
    print_matrix(matrix)
    #time.sleep(0.1)
    matrix = f.game_of_life.next_frame(matrix)
print(f'Фортран: {time.time()-start} сек')

matrix = np.random.randint(2, size=(int(WINDOW_SIZE[0]/CELL_SIZE), int(WINDOW_SIZE[1]/CELL_SIZE)))
n = 0
start = time.time()
while n < 10:
    n += 1
    print_matrix(matrix)
    #time.sleep(0.1)
    matrix = next_frame(matrix)
print(f'Питон: {time.time()-start} сек')