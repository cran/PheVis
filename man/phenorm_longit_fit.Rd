% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phenorm_longit_fit.R
\name{phenorm_longit_fit}
\alias{phenorm_longit_fit}
\title{phenorm_longit_fit}
\usage{
phenorm_longit_fit(
  x_matrix,
  y_sur,
  ID,
  size = 10^5,
  seed = 1,
  p.noise = 0.3,
  do_sampling = TRUE,
  do_noise = TRUE,
  prob = NULL,
  calc.prob = TRUE,
  nAGQ = 0,
  glmer.control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e+05))
)
}
\arguments{
\item{x_matrix}{x matrix to sample, noise and predict on}

\item{y_sur}{surrogate with 3 values (0 and 1 the extremes and 3 middle patients)}

\item{ID}{Vector of patient ID}

\item{size}{size of sampling. default is 10^5}

\item{seed}{seed. default is 1.}

\item{p.noise}{noise probability parameter. default is .3.}

\item{do_sampling}{should algorithm do sampling. default is TRUE.}

\item{do_noise}{should algorithm do noise. default is TRUE.}

\item{prob}{sampling probability during noising denoising step}

\item{calc.prob}{should the `prob` argument be calculated}

\item{nAGQ}{glmer parameter}

\item{glmer.control}{glmer parameter}
}
\value{
A list with the fixed effects, the predicted responses and the model used (mixed effect or logistic regression)
}
\description{
Apply simplified 'PheNorm' algorithm on longitudinal data with bootstrap and noise.
}
