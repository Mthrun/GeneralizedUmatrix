\name{trainstepC2}
\alias{trainstepC2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal function for s-esom
}
\description{
Does the training for fixed bestmatches in one epoch of the sESOM.
}
\usage{
trainstepC2(esomwts,aux, DataSampled,BMUsampled,Lines,Columns, Weights, Radius,
toroid, NoCases)
}
\arguments{
\item{esomwts}{array [1:Lines,1:Columns,1:Weights], WeightVectors that will be trained, internally transformed von NumericVector to cube}
\item{aux}{array [1:Lines,1:Columns,1:2], meshgrid for output distance computation}
\item{DataSampled}{NumericMatrix, n cases shuffled Dataset[1:n,1:d] by \code{sample} }
\item{BMUsampled}{NumericMatrix, n cases shuffled BestMatches[1:n,1:2] by \code{sample} in the same way as \code{DataSampled} }
\item{Lines}{double, Height of the grid}
\item{Columns}{double, Width of the grid}
\item{Weights}{double, number of weights}
\item{Radius}{double, The current Radius that should be used to define neighbours to the bm}
\item{toroid}{bool, Should the grid be considered with cyclically connected borders?}
\item{NoCases}{int, number of samples in the given non-sampled dataset}
}
\value{WeightVectors, array[1:Lines,1:Columns,1:weights] with the adjusted Weights}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 
}
\author{
Michael Thrun
}
\details{

Algorithm is described in [Thrun, 2018, p. 48, Listing 5.1].
}
\note{Usually not for seperated usage!}
