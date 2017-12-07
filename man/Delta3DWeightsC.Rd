\name{Delta3DWeightsC}
\alias{Delta3DWeightsC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
intern function
}
\description{
implementation of the main formula of SOM, ESOM,sESOM algorithms}
\usage{
Delta3DWeightsC(vx,Datasample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vx}{ array of weights [1:Lines,1:Columns,1:Weights]}
    \item{Datasample}{NumericVector of one Datapoint[1:n]}
}
\details{
intern function in case of \code{ComputeInR==FALSE} in \code{\link{GeneralizedUmatrix}}
}
\value{
modified array of weights [1:Lines,1:Columns,1:]
}
\references{
[Thrun, 2017]   Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2016.

}
\author{
Michael Thrun
}