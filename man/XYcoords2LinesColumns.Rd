\name{XYcoords2LinesColumns}
\alias{XYcoords2LinesColumns}
\title{ XYcoords2LinesColumns(X,Y) Converts points given as x(i),y(i) coordinates to integer coordinates Columns(i),Lines(i)}
\arguments{
\item{X(1:n), Y(1:n)}{coordinates: x(i),y(i) is the i-th point on a plane}
\item{minNeurons}{minimal size of the corresponding grid i.e max(Lines)*max(Columns)>=MinGridSize , default MinGridSize = 4096 defined by the numer of neurons}
\item{MaxDifferentPoints}{TRUE: the discretization error is minimal FALSE: number of Lines and Columns is minimal}
\item{PlotIt}{Plots the result}
}
\value{GridConvertedPoints[1:Columns,1:Lines,2]  IntegerPositions on a grid corresponding to x,y}
\description{
 XYcoords2LinesColumns(X,Y) Converts points given as x(i),y(i) coordinates to integer coordinates Columns(i),Lines(i)}
\author{
Michael Thrun}
\examples{
data("Lsun3D")
Data=Lsun3D$Data
InputDistances=as.matrix(dist(Data))
res=cmdscale(d=InputDistances, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
ProjectedPoints=as.matrix(res$points)
GridConvertedPoints=XYcoords2LinesColumns(ProjectedPoints[,1],ProjectedPoints[,2],PlotIt=FALSE)
}