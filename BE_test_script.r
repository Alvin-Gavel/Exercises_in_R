# This script will use the binomial_experiment to plot fits of binomial
# distributions for all values of k below n and all values of n below 10.
# It assumes that there is a directory named binomial_experiment_plots
# in the same directory as where the script is run.

source("binomial_experiment.r")

plot_all_pP_below_n(10, "binomial_experiment_plots")

gif_pP_updating(100, 0.35, "binomial_experiment_plots", 5)

gif_dD_updating(100, 0.48, 0.52, "binomial_experiment_plots", 5)
