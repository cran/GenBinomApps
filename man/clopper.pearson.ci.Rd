\name{clopper.pearson.ci}
\alias{clopper.pearson.ci}
\title{Clopper-Pearson Confidence Interval}
\description{ Computing upper, lower or two-sided Clopper-Pearson confidence limits for a given confidence  level.
}
\usage{
clopper.pearson.ci(k, n, alpha = 0.1, CI = "upper")
}
\arguments{
  \item{k}{
number of failures/successes.
}
  \item{n}{
number of trials.
}
  \item{alpha}{
significance level for the \eqn{(1-\alpha)\cdot 100}{(1-alpha)* 100}\%  confidence level (default \eqn{\alpha=0.1}{alpha=0.1}).
}
  \item{CI}{
indicates the kind of the confidence interval, options: "upper" (default), "lower",  "two.sided". 
}
}
\details{
Computes the confidence limits for the \eqn{p} of a binomial distribution.
Confidence intervals are obtained by the definition of Clopper and Pearson.
}
\value{
A data frame containing the kind of the confidence interval, upper and lower limits and the used significance level \code{alpha}.
}
\references{
D.Kurz, H.Lewitschnig, J.Pilz, \emph{Decision-Theoretical Model for Failures which are Tackled by Countermeasures}, IEEE Transactions on Reliability, 2013, submitted.

C.J.Clopper and E.S. Pearson, \emph{The use of confidence or fiducial limits illustrated in the case of the binomial}, Biometrika, vol. 26, 404-413, 1934.
}

\examples{
clopper.pearson.ci(5,100000,alpha=0.05)
# Confidence.Interval = upper
# Lower.limit = 0
# Upper.limit = 0.0001051275
# alpha = 0.05

clopper.pearson.ci(5,100000,CI="two.sided")
# Confidence.Interval =  two.sided
# Lower.limit = 1.97017e-05
# Upper.limit = 0.0001051275
# alpha = 0.1
}
