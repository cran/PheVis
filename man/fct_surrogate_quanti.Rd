% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_surrogate_quanti.R
\name{fct_surrogate_quanti}
\alias{fct_surrogate_quanti}
\title{fct_surrogate_quanti}
\usage{
fct_surrogate_quanti(
  main_icd,
  main_cui,
  df,
  half_life,
  date,
  patient_id,
  encounter_id,
  omega = 2,
  param = NULL
)
}
\arguments{
\item{main_icd}{Character vector of the column names of the main ICD codes.}

\item{main_cui}{Character vector of the column names of the main CUIs.}

\item{df}{Dataframe containing all variables.}

\item{half_life}{Duration of accumulation. For a chronic disease you might chose Inf, for acute disease you might chose the duration of the disease.}

\item{date}{Column name of the time column. The time column should be numeric}

\item{patient_id}{Column name of the patient id column.}

\item{encounter_id}{Column name of the encounter id column.}

\item{omega}{Constant for the extrema population definition.}

\item{param}{param of a previous train_phevis() result.}
}
\value{
A list
\itemize{
 \item table - Main result: \code{data.frame} with the rolling variables and the surrogates
 \item param - the parameters for the standardisation of ICD and CUI
 \item roll_all - a subset of table with the rolling variables only
 \item quantile_vec - the quantile defining the extrema populations
}
}
\description{
Compute the quantitative surrogate and then apply thresholds to get the qualitative surrogate.
}
