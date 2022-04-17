# This will fit a binomial distribution to a given number of trials and
# successes, and plot the fit. The intent is to give an idea of how the
# distribution successively narrows as more data is supplied. This is
# based on a demonstration in Chap. 2 of Statistical Rethinking by 
# McElreath


n_steps <- 100
binom_p <- seq(from=0, to=1, length.out=n_steps)

# Calculate p(P|n, k), given a prior p(P)
binomial_update <- function(n, k, pP) {
   L <- dbinom(k, size = n, prob = binom_p)
   pPnk <- L * pP
   return(pPnk)
}

# Calculate p(P|n, k), starting with a flat prior
binomial_fit <- function (n, k) {
   pP <- seq(from=1, to=1, length.out=n_steps)
   pPnk <- binomial_update(n, k, pP)
   return(pPnk)
}

# Plot p(P|n, k) and save the resulting figure
plot_pP <- function(pPnk, file_path) {
   jpeg(file=file_path)
   plot(binom_p, pPnk, type="l", xlab="p", ylab="p(P|n, k)", xlim = c(0,1))
   dev.off()
}

# Plot fits of p(P|n, k) with a flat prior, saving them in a given folder
plot_all_pP_below_n <- function(n_max, folder_path) {
   for (n in 0:n_max) {
      for (k in 0:n) {
         pPnk = binomial_fit(n, k)
         plot_pP(pPnk, paste0(folder_path, "/binomial_fit_", n, "_", k))
      }
   }
}

n_max <- 10
plot_all_pP_below_n(n_max, "binomial_experiment_plots")
