\name{does_not_matter_either}
\alias{fun1}
\alias{fun3}
\title{A duplicate alias}
\description{
  This file documents \code{fun2}, which is already documented in \link{fun1}.
}
\usage{
fun2(x, ...)
fun3(x, ...)
}
\arguments{
  \item{x}{An object.}
  \item{\dots}{Extra arguments.}
}
\details{
  The existence of this help page should trigger a WARNING during
  \code{R CMD check}.

  We should still look into the lookup for package-local links and
  figure out if there are any potential problems.

}
\keyword{documentation}
