% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/build_qantsur.R
\name{build_qantsur}
\alias{build_qantsur}
\title{build_qantsur}
\usage{
build_qantsur(df, var.icd, omega)
}
\arguments{
\item{df}{the dataframe containing the icd codes.}

\item{var.icd}{the main icd codes}

\item{omega}{the constant to define the extrema populations}
}
\value{
A numeric vector with the thresholds for the extrema populations.
}
\description{
build quantile threshold based on icd variables and omega constant
}
