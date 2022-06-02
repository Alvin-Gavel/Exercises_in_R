# This will fit a binomial distribution to a given number of trials and
# successes, and plot the fit. The intent is to give an idea of how the
# distribution successively narrows as more data is supplied. This is
# based on a demonstration in Chap. 2 of Statistical Rethinking by 
# McElreath, and some things I had to figure out at work for a
# statistical analysis that involved comparing to binomial processes.

library(animation)


## Magic numbers

n_steps <- 100
step_length <- 1. / n_steps
binom_p <- seq(from=0, to=1, length.out=n_steps)
binom_d <-seq(from=-1, to=1, length.out=2*n_steps-1)

## Statistical calculations

# Calculate p(P|n, k), given a prior p(P)
binomial_update <- function(n, k, pP) {
   L <- dbinom(k, size = n, prob = binom_p)
   pPnk <- L * pP
   norm <- sum(pPnk * step_length)
   pPnk <- pPnk / norm
   return(pPnk)
}

# Calculate p(P|n, k), starting with a flat prior
binomial_fit <- function (n, k) {
   pP <- seq(from=1, to=1, length.out=n_steps)
   pPnk <- binomial_update(n, k, pP)
   return(pPnk)
}

# Assume you have two binomial processes with success probabilities P1
# and P2. Define the difference D = P1 - P2. Then the probability
# distribution d(D|n1, k1, n2, k2) is given by the convolution between
# the probability distributions p(P1|n1,k1) and p(P2|n2,k2)
difference <- function (n1, k1, n2, k2) {
   pP1 <- binomial_fit(n1, k1)
   pP2 <- binomial_fit(n2, k2)
   dD <- convolve(pP1,pP2, type = 'open')
   norm <- sum(dD * step_length)
   dD <- dD / norm
   return(dD)
}

## Plotting

# Plot p(P|n, k) and save the resulting figure
plot_pP <- function(pPnk, file_path, frame = TRUE) {
   png(file=file_path)
   if (frame) {
      plot(binom_p, pPnk, type="l", xlab="P", ylab="p(P|n, k)", xlim = c(0,1), lwd = 2)
   } else {
      par(mar=c(0,0,0,0))
      plot(binom_p, pPnk, type="l", axes=FALSE, xlab="", ylab="", bty="n", xlim = c(0,1), lwd = 2)
   }
   dev.off()
}

# Plot d(D| n1, k1, n2, k2) and save the resulting figure
plot_dD <- function(dD, file_path, frame = TRUE) {
   png(file=file_path)
   if (frame) {
      plot(binom_d, dD, type="l", xlab="P", ylab="d(D|n1, k1, n1, k2)", xlim = c(-1,1), lwd = 2)
   } else {
      par(mar=c(0,0,0,0))
      plot(binom_d, dD, type="l", axes=FALSE, xlab="", ylab="", bty="n", xlim = c(-1,1), lwd = 2)
   }
   dev.off()
}

# Plot fits of p(P|n, k) with a flat prior, saving them in a given folder
plot_all_pP_below_n <- function(n_max, folder_path) {
   for (n in 0:n_max) {
      for (k in 0:n) {
         pPnk = binomial_fit(n, k)
         plot_pP(pPnk, paste0(folder_path, "/Binomial_fit_", n, "_", k, "_no_frame", ".png"), FALSE)
         plot_pP(pPnk, paste0(folder_path, "/Binomial_fit_", n, "_", k, ".png"), TRUE)
      }
   }
}

## GIFs

# Make a gif showing the binomial fits as they are updated over n
# binomial draws with a probability P of success
gif_pP_updating <- function(n_max, P, fadeout = TRUE) {
   tosses <- runif(n_max)
   successes <- tosses < P
   
   successive_pPnk <- array(0, c(n_steps, n_max))
   for (n in 0:n_max) {
      k = sum(successes[0:n], na.rm = TRUE)
      pPnk = binomial_fit(n, k)
      successive_pPnk[, n] <- pPnk
   }
   
   saveGIF(
   for (n in 1:n_max) {
      k = sum(successes[0:n], na.rm = TRUE)
      plot(1, type="l", xlab="P", ylab = "p(P)", main=paste0("p(P|", n, ", ", k, ")"), xlim = c(0,1), ylim = c(0,max(pPnk)), lwd = 2)
      if (fadeout) {
         if (n > 3) {
            lines(binom_p, y = successive_pPnk[, n-3], col = "gray88")
         }
         if (n > 2) {
            lines(binom_p, y = successive_pPnk[, n-2], col = "gray75")
         }
         if (n > 1) {
            lines(binom_p, y = successive_pPnk[, n-1], col = "gray50")
         }
      }
      lines(binom_p, y = successive_pPnk[, n], col = "gray0")
   },
   movie.name = paste0("Updating_n_", n_max, "_P_", P, ".gif")
   )
   # I have to figure out how to save it in some other folder. Right now this
   # is specifically the file *name* not a file path
}

# Make a gif showing the estimated difference between the probabilities
# of two binomial distributions as they are updated over n draws with
# probabilities P1 and P2 of success
gif_dD_updating <- function(n_max, P1, P2, fadeout = TRUE) {
   tosses_1 <- runif(n_max)
   successes_1 <- tosses_1 < P1
   tosses_2 <- runif(n_max)
   successes_2 <- tosses_2 < P2
   
   successive_dD <- array(0, c(2*n_steps-1, n_max))
   for (n in 0:n_max) {
      k1 = sum(successes_1[0:n], na.rm = TRUE)
      k2 = sum(successes_2[0:n], na.rm = TRUE)
      dD = difference(n, k1, n, k2) 
      successive_dD[, n] <- dD
   }
   
   saveGIF(
   for (n in 1:n_max) {
      k1 = sum(successes_1[0:n], na.rm = TRUE)
      k2 = sum(successes_2[0:n], na.rm = TRUE)
      plot(1, type="l", xlab="D", ylab = "d(D)", main=paste0("d(D|", n, ", ", k1, ", ", n, ", ", k2, ")"), xlim = c(-1,1), ylim = c(0,max(dD)), lwd = 2)
      if (fadeout) {
         if (n > 3) {
            lines(binom_d, y = successive_dD[, n-3], col = "gray88")
         }
         if (n > 2) {
            lines(binom_d, y = successive_dD[, n-2], col = "gray75")
         }
         if (n > 1) {
            lines(binom_d, y = successive_dD[, n-1], col = "gray50")
         }
      }
      lines(binom_d, y = successive_dD[, n], col = "gray0")
   },
   movie.name = paste0("Updating_n_", n_max, "_P1_", P1, "_P2_", P2, ".gif")
   )
   # I have to figure out how to save it in some other folder. Right now this
   # is specifically the file *name* not a file path
}
