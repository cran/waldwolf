\name{waldwolf.test}
\alias{waldwolf.test}

\title{
Wald-Wolfowitz trend test (Sequence Test)
}
\description{
Sequence Test through of the median.

The Wald-Wolfowitz test is a non-parametric statistical test that checks a randomness hypothesis for a two-valued data sequence.
}
\usage{
waldwolf.test(x)
}

\arguments{
  \item{x}{numeric vector;variable that will be checked trend, can be shaped column. Only the most suitable time series is fomato \code{\link{ts}}.}
  }
  

\references{
Pedro A. Morettin e Clelia M. C. Toloi, Analise de
Series Temporais, Segunda edicao, 2006.
}
\author{
Author: Jodavid Ferreira, Claudio Souza

Maintainer: Jodavid Ferreira <contato@jodavidferreira.com>


}

\examples{


library(waldwolf)

#Zest is Times Serie

x<-c(1,2,3,4,5)

Zest<-ts(x)

waldwolf.test(Zest)
}