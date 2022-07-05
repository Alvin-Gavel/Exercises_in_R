# There is a widespread misconception among Bayesian statisticians that
# when fitting parameters to data, it is possible to "avoid making
# assumptions" by using a flat prior. There is a corresponding
# misconception among frequentist statisticians that frequentist
# statistics avoids making assumptions since it does not make use of a
# prior. These misconceptions are equivalent, since the frequentist
# fitting procudure is equivalent to a Bayesian procedure that uses a
# flat prior and discards everything but the maximum of the posterior.
#
# This module is intended to demonstrate the misconception, by a toy
# model were two statisticians, Alice and Bob fit the same model to the
# same data and end up with different results. The trick is that the
# parametrisation of the model, which should just be a matter of
# convention, actually hides some information. A flat prior with one
# parametrisation may not be flat with another.
#
# The data-generating process is that points are selected with uniform
# probability along a line segment of unit length that runs into the
# origin, and then gaussian noise with some standard deviation sigma is
# added.
#
# Alice uses a parametrisation where the line is described in terms of
# the angle alpha w.r.t. the x-axis. Bob uses a parametrisation where the
# line is described in terms of the linear coefficient a.
#
# The toy model is from von Toussaint (2011).

library(animation)

# Magic numbers

parameter_steps = 100

# The data generation matches the model used by Alice, but this has no
# importance
generate_data <- function(n, alpha, sigma) {
   d <- runif(n, 0, 1)
   x_noiseless <- d * cos(alpha)
   y_noiseless <- d * sin(alpha)
   x <- x_noiseless + rnorm(n, 0, sigma)
   y <- y_noiseless + rnorm(n, 0, sigma)
   return(rbind(x, y))
}

P_x_y_from_xtrue_ytrue_sigma <- function(x, y, xtrue, ytrue, sigma) {
   (1. / (sigma * sqrt(2 * pi))) * exp()
}

alice_fit_bayesian <- function(data) {
   alphas = linspace(0, 2 * pi, n = parameter_steps)
   alpha_prior = ones(parameter_steps)
   likelihood = 
}

plot_data <- function(data, file_path) {
   png(file=file_path)
   plot(data[1,], data[2,], xlab="x", ylab="y", xlim = c(-1,1), ylim = c(-1,1))
   dev.off()
}
