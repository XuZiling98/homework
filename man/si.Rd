% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/si.R
\name{si}
\alias{si}
\title{Simulation of SI models using R}
\usage{
si(i0, beta, t_range = seq(1, 100, 1))
}
\arguments{
\item{i0}{The proportion of the population initially infected.}

\item{beta}{The average daily effective contact of infected persons.}

\item{t_range}{a vector:Time step.If null,the default is t_range=seq(1,100,1).}
}
\value{
a list including 'infe':Percentage of people infected per day; 'susc':Percentage of susceptible persons per day; 'I':The total number of people infected; 'S':The total number of people susceptible; 'in.time':The fastest time to get all infected.
}
\description{
In SI models, once infected, it remains infected.After the initial value of SI model is given, the figure of the model is drawn and the simulation results are given.
}
\examples{
\dontrun{
SI <- si(i0=1e-4,beta=1.2,t_range=seq(0,100,1))
plot(SI$infe,type='l',col='red',xlab = 'time' , ylab='percentage',
main='SI model')
  lines(SI$susc,type='l',col='blue')
  legend('right',c('infective percentage','susceptible percentage'),
  col=c('red','blue'),lty=c(1,1))
}
}
