# This will fit a binomial distribution to a given number of trials and
# successes, and plot the fit. The intent is to give an idea of how the
# distribution successively narrows as more data is supplied. This is
# based on a demonstration in Chap. 2 of Statistical Rethinking by 
# McElreath


n_steps <- 100
binom_p <- seq(from=0, to=1, length.out=n_steps)

# Fit a binomial distribution to the given number of trials and
# successes and plot it, dropping the plot in a folder named
# binomial_experiment_plots
binomial_fit <- function(trials, successes) {
   binom_L <- dbinom(successes, size = trials, prob = binom_p)
   jpeg(file=paste0("binomial_experiment_plots/Binomial_fit_", trials, "_", successes, ".jpg"))
   plot(binom_p, binom_L, type="l", xlab="p", ylab="P(p|data)", xlim = c(0,1))
   dev.off()
}

# Fit each possible number of successes given a number of trials
binomial_fits_by_trials <- function(trials) {
   for (i in 0:trials) {
      binomial_fit(trials, i)
   }
}

# Fit each possible number of successes for a number of trials below a
# maximum
binomial_fits_below_trials <- function(max_trials) {
   for (trials in 0:max_trials) {
      binomial_fits_by_trials(trials)
   }
}

trials <- 10
binomial_fits_below_trials(trials)
