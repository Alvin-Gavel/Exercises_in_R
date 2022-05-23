# This is an implementation of Conway's game of life, played on a periodic
# grid. The rules of the game are fairly simple, and go as follows:
#
# The game is played on a square grid where each square is called a 'cell',
# and can be either in the state of being 'alive' or 'dead'. On each turn,
# the state of each cell is determined by the state of itself and its
# eight neighbours on the previous turn, so that:
# 1. If the cell was alive and had two or three live neighbours, then it is
#    still alive.
# 2. If the cell was alive and had less than two or more than three live
#    neighbours, then it is now dead.
# 3. If the cell was dead and has exactly three live neighbours, then it is
#    now alive.

library(pracma)

update <- function(alive) {
   count_live_neighbours = circshift(alive, c(0,1)) + circshift(alive, c(0,-1)) + circshift(alive, c(1,0)) + circshift(alive, c(-1,0)) + circshift(alive, c(1,1)) + circshift(alive, c(1,-1)) + circshift(alive, c(-1,1)) + circshift(alive, c(-1,-1))
   updated_alive = (alive & (count_live_neighbours >= 2 & count_live_neighbours <= 3)) | ((!alive) & count_live_neighbours == 3)
   return(updated_alive)
}

make_random_board <- function(x, y, P) {
   alive_vector = runif(x * y) < P
   alive = matrix(alive_vector, nrow = x, ncol = y)
   return(alive)
}
