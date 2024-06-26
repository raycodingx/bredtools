% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gibbs.R
\name{gibbs}
\alias{gibbs}
\title{Given the components of MME, perform Gibbs sampler.}
\usage{
gibbs(Ainv, y, X, Z, N, burnin, inits, varE, varA, ve, va, s2e, s2a, disp)
}
\arguments{
\item{Ainv}{an inverse of additive relationship matrix}

\item{y}{Given the components of MME, perform Gibbs sampler.}

\item{X}{incidence matrix for systematic effects}

\item{Z}{a incidence matrix for individual effects}

\item{N}{a number of gibbs samples}

\item{burnin}{a number of burnin}

\item{inits}{a vecotor of initial values for systematic and individual effects}

\item{varE}{initial value for residual variance}

\item{varA}{initial value for additive genetic variance}

\item{ve}{a degree of belief for residual variance}

\item{va}{a degree of belief for additive genetic variance}

\item{s2e}{a prior value for the residual variance}

\item{s2a}{a prior value for the additive genetic variance}

\item{disp}{a logical value. If true, display estimates of variance components at each iteration.}
}
\value{
a list of result
}
\description{
Given the components of MME, perform Gibbs sampler.
}
