\name{trainstepC}
\alias{trainstepC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
internal function for s-esom
}
\description{
Does the training for fixed bestmatches in one epoch of the s-esom.
}
\usage{
trainstepC(vx,vy, DataSampled,BMUsampled,Lines,Columns, Radius, toroid)
}
\arguments{
\item{vx}{array (1:Lines,1:Columns,1:Weights), WeightVectors that will be trained, internally transformed von NumericVector to cube}
\item{vy}{array (1:Lines,1:Columns,1:2), meshgrid for output distance computation}
\item{DataSampled}{NumericMatrix, n cases shuffled Dataset[1:n,1:d] by \code{sample} }
\item{BMUsampled}{NumericMatrix, n cases shuffled BestMatches[1:n,1:2] by \code{sample} in the same way as \code{DataSampled} }
\item{Lines}{double, Height of the grid}
\item{Columns}{double, Width of the grid}
\item{Radius}{double, The current Radius that should be used to define neighbours to the bm}
\item{toroid}{bool, Should the grid be considered with cyclically connected borders?}
}
\value{WeightVectors, array[1:Lines,1:Columns,1:weights] with the adjusted Weights}
\references{
[Thrun, 2017]   Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universitaet Marburg, Marburg, 2017.

}
\author{
Michael Thrun
}