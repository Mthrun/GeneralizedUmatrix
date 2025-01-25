\name{TopviewTopographicMap}
\alias{TopviewTopographicMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Top view of the topographic map in 2D
}
\description{
Fast visualization of the generalized U-matrix in 2D which visualizes high-dimensional distance and density based structurs of the combination two-dimensional scatter plots (projections) with high-dimensional data.
}
\usage{
TopviewTopographicMap(GeneralizedUmatrix, BestMatchingUnits,

Cls, ClsColors = NULL, Imx = NULL,

ClsNames = NULL, BmSize = 6, DotLineWidth = 2,

alpha = 1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{GeneralizedUmatrix}{[1:Lines,1:Columns] U-matrix to be plotted, numerical
matrix storing the U-heights, see [Thrun, 2018] for definition.}
\item{BestMatchingUnits}{[1:n,1:2], Positions of bestmatches to be plotted onto
the U-matrix}
\item{Cls}{[1:n], numerical vector of classification defining the labels defined as digits of the [1:k] classes. See details}
\item{ClsColors}{Optional, [1:k] character vector of colors that will be used to colorize the different classes, vector can have names that define the mapping of the k classes, see details}
\item{Imx}{a mask (Imx) that will be used to cut out the U-matrix}
\item{ClsNames}{Optional, [1:k] character vector naming the k classes for the
legend. Vector can have names that define the mapping of the k classes, see details. In this case, further parameters with the possibility to adjust are:
\code{LegendCex}: (size);
\code{NamesOrientation}: Legend position "v" or "h";
\code{NamesTitle}: title of legend.
}
\item{BmSize}{size(diameter) of the points in the visualizations. The points
represent the BestMatchingUnits}
\item{DotLineWidth}{...}
\item{alpha}{...}
\item{\dots}{
\describe{
\item{Tiled}{Should the U-matrix be drawn 4times?}
\item{main}{set specific title in plot}
\item{ExtendBorders}{scalar, extends U-matrix by toroidal continuation of the given U-matrix}
\item{MainCex}{scalar, magnification to be used for legend}
\item{LegendCex}{scalar, magnification to be used for main titles}
\item{_}{Further Arguments relevant for interactive shiny application}
}
}
}
\details{
In \code{Cls} each the bestmatch that will be visualized as a colored point gets one label, and the mappping is consecutive, i.e. first bestmatch in \code{BestMatchingUnits} gets first label stored in \code{Cls}. Please note, that the there will be k labels stored in \code{Cls} but depending on the user input the digits in the k-labels do not need to be consecutive. For example, if an algorithm find three clusters the labels do not need to be 1,2,3 but can also be 5,99,1.
 
if \code{ClsColors} or \code{ClsNames} is given but the vector is not named, than internally the mapping of \code{names(ClsColors)=sort(unique(Cls))} is assumed, meaning that the lowest digit number of the k classes gets the first color stored in the first element of the \code{ClsColors} vector. The same is true for \code{ClsNames}. The user can specify antoher non-consecutive mapping between colors/names and labels with \code{names(ClsColors)=...}. In the above example, one could define the mapping between colors and classes with \code{names(ClsColors)=c(5,99,1)}, after the vector is initialized with three colors for the three clusters.

Please see also \code{\link{plotTopographicMap}}. 
}
\value{
plotly handler
}
\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \doi{10.1007/978-3-658-20540-9}, 2018. 

[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.

}
\author{
Tim Schreier, Luis Winckelmann, Michael Thrun
}
\note{
Names are currently under development, Imx in testing phase.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plotTopographicMap}}
}
\examples{

data("Chainlink")
Data=Chainlink$Data
Cls=Chainlink$Cls
InputDistances=as.matrix(dist(Data))
res=cmdscale(d=InputDistances, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
ProjectedPoints=as.matrix(res$points)
#see also ProjectionBasedClustering package for other common projection methods
\donttest{
resUmatrix=GeneralizedUmatrix(Data,ProjectedPoints)
## visualization
TopviewTopographicMap(GeneralizedUmatrix = resUmatrix$Umatrix,resUmatrix$Bestmatches)
}
\dontshow{
if(requireNamespace('deldir',quietly = TRUE)) {
resUmatrix=GeneralizedUmatrix(Data[1:2,],ProjectedPoints[1:2,])
TopviewTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches)
}
}
}
\keyword{topographic}% use one of  RShowDoc("KEYWORDS")
\keyword{map}% __ONLY ONE__ keyword per line
\keyword{visualization}% __ONLY ONE__ keyword per line
\keyword{U-Matrix}% __ONLY ONE__ keyword per line
\keyword{Umatrix}% __ONLY ONE__ keyword per line
\keyword{GeneralizedUmatrix}% __ONLY ONE__ keyword per line
\concept{topographic map}
\concept{Generalized U-matrix}