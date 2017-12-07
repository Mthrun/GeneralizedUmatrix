\name{plotTopographicMap}
\alias{plotTopographicMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
shows generelized Umatrix visualization
}
\description{
visualizes high-dimensional distance and density based structurs in two-dimensional scatter plots using a atopographic map with hypsometric tints
}
\usage{
plotTopographicMap(GeneralizedUmatrix, BestMatchingUnits=NULL,

Cls=NULL,ClsColors=NULL,Imx=NULL, Tiled=FALSE, BmSize=0.5,ShowAxis=F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{GeneralizedUmatrix}{(1:Lines,1:Columns), Umatrix to be plotted}
\item{BestMatchingUnits}{(1:n,1:2), Positions of bestmatches to be plotted onto the Umatrix}
  \item{Cls}{(1:n), Class identifier for the bestmatch at the given point}
  \item{ClsColors}{Vector of colors that will be used to colorize the different classes}
 \item{Imx}{a mask (Imx) that will be used to cut out the umatrix}
 \item{Tiled}{Should the Umatrix be drawn 4times?}

\item{BmSize}{size(diameter) of the points in the visualizations. The points represent the BestMatchingUnits}
\item{ShowAxis}{shall the axis be shown?}

}
\details{
see chapter 5 of [Thrun, 2017] for descriptive details.

}

\references{
[Thrun, 2017] Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2017.


[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.

}
\author{
Michael Thrun
}
\note{
Algorithm is partly based on the Umatrix package.
}

\seealso{
\code{\link{GeneralizedUmatrix}}}

\examples{

data("Lsun3D")
Data=Lsun3D$Data
Cls=Lsun3D$Cls
InputDistances=as.matrix(dist(Data))
res=cmdscale(d=InputDistances, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
ProjectedPoints=as.matrix(res$points)
#see also ProjectionBasedClustering package for other common projection methods
\donttest{
resUmatrix=GeneralizedUmatrix(Data,ProjectedPoints)
## visualization
plotTopographicMap(GeneralizedUmatrix = resUmatrix$Umatrix,resUmatrix$Bestmatches)
## To save as STL for 3D printing
 rgl::writeSTL("GenerelizedUmatrix_3d_model.stl")

## Save the visualization as a picture with
library(rgl)
rgl.snapshot('test.png')
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{topographic}% use one of  RShowDoc("KEYWORDS")
\keyword{map}% __ONLY ONE__ keyword per line
\keyword{visualization}% __ONLY ONE__ keyword per line
\keyword{U-Matrix}% __ONLY ONE__ keyword per line
\keyword{Umatrix}% __ONLY ONE__ keyword per line
\keyword{GeneralizedUmatrix}% __ONLY ONE__ keyword per line