\name{Delta3DWeightsC}
\alias{Delta3DWeightsC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
intern function
}
\description{
Thr implementation of the main formula of SOM, ESOM, sESOM algorithms.}
\usage{
Delta3DWeightsC(vx,Datasample)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{vx}{Numeric array of weights [1:Lines,1:Columns,1:Weights]}
\item{Datasample}{Numeric vector of one datapoint[1:n]}
}
\details{
intern function in case of \code{ComputeInR==FALSE} in \code{\link{GeneralizedUmatrix}}
}
\value{
modified array of weights [1:Lines,1:Columns,1:Weights]
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 
}
\author{
Michael Thrun
}