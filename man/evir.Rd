% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/evir.R
\name{evir}
\alias{evir}
\alias{alogLik.gev}
\alias{alogLik.gpd}
\alias{alogLik.potd}
\title{Loglikelihood adjustment for evir fits}
\usage{
\method{alogLik}{gev}(x, cluster = NULL, use_vcov = TRUE, ...)

\method{alogLik}{gpd}(x, cluster = NULL, use_vcov = TRUE, ...)

\method{alogLik}{potd}(x, cluster = NULL, use_vcov = TRUE, ...)
}
\arguments{
\item{x}{A fitted model object with certain associated S3 methods.
See \strong{Details}.}

\item{cluster}{A vector or factor indicating from which cluster the
respective loglikelihood contributions from \code{loglik} originate.
This must have the same length as the vector returned by the
\code{logLikVec} method for an object like \code{x}.
If \code{cluster} is not supplied (i.e. is \code{NULL}) then it is
assumed that each observation forms its own cluster.
See \strong{Details}.}

\item{use_vcov}{A logical scalar.  Should we use the \code{vcov} S3 method
for \code{x} (if this exists) to estimate the Hessian of the independence
loglikelihood to be passed as the argument \code{H} to
\code{\link[chandwich]{adjust_loglik}}?
Otherwise, \code{H} is estimated inside
\code{\link[chandwich]{adjust_loglik}} using
\code{\link[stats:optim]{optimHess}}.}

\item{...}{Further arguments to be passed to the functions in the
sandwich package \code{\link[sandwich]{meat}} (if \code{cluster = NULL}),
or \code{\link[sandwich:vcovCL]{meatCL}} (if \code{cluster} is not
\code{NULL}).}
}
\value{
An object inheriting from class \code{"chandwich"}.  See
  \code{\link[chandwich]{adjust_loglik}}.
  \code{class(x)} is a vector of length 5. The first 3 components are
  \code{c("lax", "chandwich", "evir")}.
  The remaining 2 components depend on the model that was fitted.
  If \code{\link[evir]{gev}} was used then these components are
  \code{c("gev", "stat")}.
  If \code{\link[evir]{gpd}} was used then these components are
  \code{c("gpd", "stat")}.
  If \code{\link{pot_refit}} was used then these components are
  \code{c("potd", "stat")}.
}
\description{
S3 \code{alogLik} method to perform loglikelihood adjustment for fitted
extreme value model objects returned from the functions
\code{\link[evir]{gev}}, \code{\link[evir]{gpd}} and \code{\link[evir]{pot}}
in the evir package.
If \code{x} was returned from \code{\link[evir]{pot}} then the model will
need to be re-fitted using \code{\link{pot_refit}}.
}
\details{
Object \code{x} \emph{must} have the following S3
  methods:
  \itemize{
    \item{\code{logLikVec: }}{returns a vector of the contributions to the
      independence loglikelihood from individual observations;}
    \item{\code{coef: }}{returns a vector of model coefficients, see
      \code{\link[stats]{coef}};}
    \item{\code{nobs: }}{returns the number of (non-missing) observations
      used in a model fit, see \code{\link[stats]{nobs}}};
  }
  and \emph{may} have the following S3 methods
  \itemize{
    \item{\code{vcov: }}{returns the estimated variance-covariance matrix of
      the (main) parameters of a fitted model, see
      \code{\link[stats]{vcov}};}
    \item{\code{estfun: }}{returns an \eqn{n x k} matrix, in which each
      column gives the derivative of the loglikelihood at each of \eqn{n}
      observation with respect to the \eqn{k} parameters of the model, see
      \code{\link[sandwich]{estfun}}.}
  }
  Loglikelihood adjustment is performed using the
  \code{\link[chandwich]{adjust_loglik}} function in the
  \code{\link[chandwich]{chandwich}} package.
  The relevant arguments to \code{\link[chandwich]{adjust_loglik}}, namely
  \code{loglik, mle, H} and \code{V}, are created based on the class of
  the object \code{x}.

  If a \code{vcov} method is not available, or if \code{use_vcov = FALSE},
  then the variance-covariance matrix of the MLE (from which \code{H} is
  calculated) is estimated inside \code{\link[chandwich]{adjust_loglik}}
  using \code{\link[stats:optim]{optimHess}}.

  The \code{sandwich} package is used to estimate the variance matrix
  \code{V} of the score vector: \code{\link[sandwich]{meat}} is used if
  \code{cluster = NULL}; \code{\link[sandwich:vcovCL]{meatCL}} is used if
  \code{cluster} is not \code{NULL}.
  If \code{cluster} is \code{NULL} then any arguments of
  \code{\link[sandwich:vcovCL]{meatCL}} present in \dots will be ignored.
  Similarly, if \code{cluster} is not \code{NULL} then any arguments of
  \code{\link[sandwich]{meat}} present in \dots will be ignored.
  \code{\link[sandwich]{meat}} and \code{\link[sandwich:vcovCL]{meatCL}}
  require an \code{\link[sandwich]{estfun}} method to be available, which,
  in the current context, provides matrix of score contributions.
  If a bespoke \code{estfun} method is not provided then this is constructed
  by estimating the score contributions using \code{\link[numDeriv]{jacobian}}.
}
\examples{
# We need the evir package
got_evir <- requireNamespace("evir", quietly = TRUE)
if (got_evir) {
  library(evir)
  # An example from the evir::gev documentation
  data(bmw)
  out <- gev(bmw, "month")
  adj_out <- alogLik(out)
  summary(adj_out)

  # An example from the evir::gpd documentation
  data(danish)
  out <- gpd(danish, 10)
  adj_out <- alogLik(out)
  summary(adj_out)

  # An example from the evir::pot documentation
  # We use lax::pot_refit() to return the input data
  out <- pot_refit(danish, 10)
  adj_out <- alogLik(out)
  summary(adj_out)
}
}
\references{
Chandler, R. E. and Bate, S. (2007). Inference for clustered
  data using the independence loglikelihood. \emph{Biometrika},
  \strong{94}(1), 167-183. \url{http://doi.org/10.1093/biomet/asm015}

Zeleis (2006) Object-Oriented Computation and Sandwich
  Estimators.  \emph{Journal of Statistical Software}, \strong{16}, 1-16.
  \url{http://doi.org/10.18637/jss.v016.i09}
}
\seealso{
\code{\link{alogLik}}: loglikelihood adjustment for model fits.
}