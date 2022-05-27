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

# This makes a warning about overlapping names (I think), I'll try to figure
# out a fix
library(pracma)
library(caTools)


### The central game mechanic

tick <- function(alive) {
   count_live_neighbours = circshift(alive, c( 0, 1)) +
                           circshift(alive, c( 0,-1)) +
                           circshift(alive, c( 1, 0)) +
                           circshift(alive, c(-1, 0)) +
                           circshift(alive, c( 1, 1)) +
                           circshift(alive, c( 1,-1)) +
                           circshift(alive, c(-1, 1)) +
                           circshift(alive, c(-1,-1))
   updated_alive = (alive & (count_live_neighbours >= 2 & count_live_neighbours <= 3)) | ((!alive) & count_live_neighbours == 3)
   return(updated_alive)
}

### Visualisation functions

print_board <- function(board) {
   zero_one_board = matrix(as.integer(board), nrow=nrow(board), ncol=ncol(board))
   write.table(zero_one_board, row.names=F, col.names=F)
}

# This plotting is based on that used in the Wikipedia article on R. In
# the binomial_experiment module I use a different approach. I don't
# know which is best.
plot_n_generations <- function(alive, n, filepath) {
   X <- array(0, c(nrow(alive), ncol(alive), n))
   for (i in 1:n) {
     alive <- tick(alive)
     X[, , i] <- alive
   }
   write.gif(X, filepath, col=rainbow(2), delay = 50)
}

### Board creation functions

make_random_board <- function(x, y, P) {
   alive_vector = runif(x * y) < P
   alive = matrix(alive_vector, nrow = x, ncol = y)
   return(alive)
}

make_still_life <- function(name) {
   if (name == 'block') {
      alive = matrix(as.logical(matrix(c(0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0 ,0, 0))), nrow=4, ncol=4)
   } else {
      print("Cannot recognise pattern")
   }
   return(alive)
}

# At some point I will have to make a function for reading text input
# as a board
