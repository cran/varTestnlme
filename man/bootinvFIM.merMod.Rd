% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lme4_utilities.R
\name{bootinvFIM.merMod}
\alias{bootinvFIM.merMod}
\title{Compute the inverse of the Fisher Information Matrix using parametric bootstrap}
\usage{
\method{bootinvFIM}{merMod}(m, B = 1000, seed = 0)
}
\arguments{
\item{m}{the model under H1}

\item{B}{the bootstrap sample size}

\item{seed}{a seed for the random generator}
}
\description{
Compute the inverse of the Fisher Information Matrix using parametric bootstrap
}
