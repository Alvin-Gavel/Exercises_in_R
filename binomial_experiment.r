# This will fit a binomial distribution to a given number of trials and
# successes, and plot the fit. The intent is to give an idea of how the
# distribution successively narrows as more data is supplied. This is
# based on a demonstration in Chap. 2 of Statistical Rethinking by 
# McElreath

library(animation)


## Magic numbers

n_steps <- 100
step_length <- 1. / n_steps
binom_p <- seq(from=0, to=1, length.out=n_steps)


## Statistical calculations

# Calculate p(P|n, k), given a prior p(P)
binomial_update <- function(n, k, pP) {
   L <- dbinom(k, size = n, prob = binom_p)
   pPnk <- L * pP
   norm = sum(pPnk * step_length)
   pPnk = pPnk / norm
   return(pPnk)
}

# Calculate p(P|n, k), starting with a flat prior
binomial_fit <- function (n, k) {
   pP <- seq(from=1, to=1, length.out=n_steps)
   pPnk <- binomial_update(n, k, pP)
   return(pPnk)
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

# Make a gif containing the binomial fits as they are updated over n
# binomial draws with a probability P of success
gif_random_sequence <- function(n_max, P, fadeout = TRUE) {
   tosses <- runif(n_max)
   successes <- tosses < P
   
   successive_pPnk <- array(0, c(n_steps, n_max))
   for (n in 0:n_max) {
      k = sum(successes[0:n], na.rm = TRUE)
      pPnk = binomial_fit(n, k)
      successive_pPnk[, n] <-pPnk
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
