import fortran as f
import numpy as np
import pygame
import time

testing = False

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

def next_frame2(matrix):
    rows, cols = matrix.shape
    temp_matrix = np.zeros((rows+2, cols+2), int)
    temp_matrix[1:rows+1, 1:cols+1] = matrix
    temp_matrix[0, 0] = matrix[-1, -1]
    temp_matrix[0, 1:cols+1] = matrix[-1, :]
    temp_matrix[0, -1] = matrix[-1, 0]
    temp_matrix[1:rows+1, -1] = matrix[:, 0]
    temp_matrix[-1, -1] = matrix[0, 0]
    temp_matrix[-1, 1:cols+1] = matrix[0, :]
    temp_matrix[-1, 0] = matrix[0, -1]
    temp_matrix[1:rows+1, 0] = matrix[:, -1]
    matrix = np.zeros_like(matrix)
    for i in range(1,rows+1):
        for j in range(1,cols+1):
            count = np.sum(temp_matrix[i-1:i+2, j-1:j+2]) - temp_matrix[i, j]
            if count == 3:
                matrix[i-1, j-1] = 1
            elif count == 2 and temp_matrix[i, j] == 1:
                matrix[i-1, j-1] = 1
    return matrix


# Инициализация Pygame
pygame.init()
WINDOW_SIZE = (1200, 720)
WHITE = (197, 197, 197)
BLACK = (30, 30, 30)
CELL_SIZE = 4
MATRIX_SIZE = (int(WINDOW_SIZE[0]/CELL_SIZE), int(WINDOW_SIZE[1]/CELL_SIZE))
if not testing:
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


if not testing:
    # Запускаем игру
    matrix = np.random.randint(2, size=MATRIX_SIZE, dtype='i1')
    running = True
    while running:
        print_matrix(matrix)
        #time.sleep(0.1)
        matrix = f.game_of_life.next_frame2(matrix)
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False
else:
    matrix = np.random.randint(2, size=MATRIX_SIZE)
    start = time.time()
    for i in range(100):
        matrix = next_frame(matrix)
    time1 = time.time()-start
    print(f'Python 1: {time1} s')

    matrix = np.random.randint(2, size=MATRIX_SIZE)
    start = time.time()
    for i in range(100):
        matrix = f.game_of_life.next_frame(matrix)
    time3 = time.time()-start
    print(f'Fortran 1: {time3} s')

    matrix = np.random.randint(2, size=MATRIX_SIZE, dtype='i1')
    start = time.time()
    for i in range(100):
        matrix = next_frame2(matrix)
    time2 = time.time()-start
    print(f'Python 2: {time2} s')

    matrix = np.random.randint(2, size=MATRIX_SIZE, dtype='i1')
    start = time.time()
    for i in range(100):
        matrix = f.game_of_life.next_frame2(matrix)
    time4 = time.time()-start
    print(f'Fortran 2: {time4} s')

    print(f'ratio P1/F1 = {time1 / time3}')
    print(f'ratio P2/F2 = {time2 / time4}')