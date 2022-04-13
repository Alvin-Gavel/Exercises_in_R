# This is based on a demonstration in Chap. 2 of Statistical Rethinking
# 
# The script will try to save 10 plots in a folder named
# binomial_experiment_plots, which contain the successively updated
# estimates of the probability of success in a binomial experiment, as
# more and more samples are drawn

n_steps <- 100
p <- seq(from=0, to=1, length.out=n_steps)
prior <- rep(1, n_steps)

true_p <- 0.7
trials <- 10

successes <- rbinom(trials, 1, true_p)
so_far <- 0

print(successes)
for (i in 1:trials) {
   so_far <- so_far + successes[i]
   L <- dbinom(so_far, size=i, prob=p)

   unnorm_posterior <- L * prior
   posterior <- unnorm_posterior / sum(unnorm_posterior)

   filepath <- paste0("binomial_experiment_plots/Plot_", i ,".jpg")
   jpeg(file=filepath)
   plot(p, posterior, type="l", xlab="p", ylab="P(p|data)")
   dev.off()
}
