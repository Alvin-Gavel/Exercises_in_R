# This script will use the binomial_experiment to plot fits of binomial
# distributions for all values of k below n and all values of n below 10.
# It assumes that there is a directory named binomial_experiment_plots
# in the same directory as where the script is run.

source("binomial_experiment.r")

n_max = 10
plot_all_pP_below_n(n_max, "binomial_experiment_plots")
