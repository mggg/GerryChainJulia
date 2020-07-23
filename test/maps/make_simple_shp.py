import shapefile
import os

# Goal: create a simple graph of 4 squares in a 2x2 grid
w = shapefile.Writer('simple_squares')
w.field("population", "N")
w.field("assignment", "N")

populations = [
    [2, 4],
    [6, 8]
]

assignments = [
    [1, 2],
    [3, 4]
]

# upper left of grid is (0, 0)
for r in range(2):
    for c in range(2):
        upper_left = (r, c)
        upper_right = (r, c+1)
        lower_left = (r+1, c)
        lower_right = (r+1, c+1)
        w.poly([[upper_left, upper_right, lower_right, lower_left, upper_left]])
        w.record(populations[r][c], assignments[r][c])

w.close()
os.remove("simple_squares.shx") # remove shx file
