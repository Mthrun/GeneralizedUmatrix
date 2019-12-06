\name{plotTopographicMap}
\alias{plotTopographicMap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualizes th Generalized U-matrix
}
\description{
visualizes high-dimensional distance and density based structurs of the combination two-dimensional scatter plots (projections) with high-dimensional data using a topographic map with hypsometric tints.
}
\usage{
plotTopographicMap(GeneralizedUmatrix, BestMatchingUnits,

Cls=NULL,ClsColors=NULL,Imx=NULL,Names=NULL, BmSize=0.5,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{GeneralizedUmatrix}{(1:Lines,1:Columns), Umatrix to be plotted}
\item{BestMatchingUnits}{(1:n,1:2), Positions of bestmatches to be plotted onto the Umatrix}
  \item{Cls}{(1:n), numerical vector of classification of \code{k} classes for the bestmatch at the given point}
  \item{ClsColors}{Vector of colors that will be used to colorize the different classes}
 \item{Imx}{a mask (Imx) that will be used to cut out the umatrix}
\item{Names}{If set: [1:k] character vector naming the k classes for the legend.
. In this case, further parameters with the possibility to adjust are:
\code{NamesCex}: (size); \code{NamesPosition}: Legend position; \code{NamesTitle}: title of legend; \code{NamesColors}: colors if ClsColors are not default (NULL).
}

\item{BmSize}{size(diameter) of the points in the visualizations. The points represent the BestMatchingUnits}

\item{\dots}{

 Besides the legend/names parameter the list of further parameters, use only of you know what you are doing:
\describe{
 \item{Tiled}{Should the Umatrix be drawn 4times?}
 \item{ShowAxis}{shall the axis be shown?}
 \item{NoLevels}{number of contour lines}
 \item{Colormap}{in the case of density p matrix...}
\item{title}{same as main}
   \item{main}{same as title}
      \item{sub}{same as in \code{\link{plot}}}
         \item{xlab}{same as in \code{\link{plot}}}   
         \item{ylab}{same as in \code{\link{plot}}}
         \item{zlab}{same as in \code{\link{plot}}}
 }
 
}
}
\details{
The visualization and result of this function is a topographic map with hypsometric tints (Thrun, Lerch, L?tsch, & Ultsch, 2016). Hypsometric tints are surface colors that represent ranges of elevation (see (Thrun et al., 2016)). Here, contour lines are combined with a specific color scale. The color scale is chosen to display various valleys, ridges, and basins: blue colors indicate small distances (sea level), green and brown colors indicate middle distances (low hills), and shades of white colors indicate vast distances (high mountains covered with snow and ice). Valleys and basins represent clusters, and the watersheds of hills and mountains represent the borders between clusters. In this 3D landscape, the borders of the visualization are cyclically connected with a periodicity (L,C). A central problem in clustering is the correct estimation of the number of clusters. This is addressed by the topographic map which allows assessing the number of clusters (Thrun et al., 2016). Please see chapter 5 of [Thrun, 2018] for further details.
}

\references{
[Thrun, 2018]  Thrun, M. C.: Projection Based Clustering through Self-Organization and Swarm Intelligence, doctoral dissertation 2017, Springer, Heidelberg, ISBN: 978-3-658-20539-3, \url{https://doi.org/10.1007/978-3-658-20540-9}, 2018. 

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
plotTopographicMap(GeneralizedUmatrix = resUmatrix$Umatrix,resUmatrix$Bestmatches)
## To save as STL for 3D printing
 rgl::writeSTL("GenerelizedUmatrix_3d_model.stl")

## Save the visualization as a picture with
library(rgl)
rgl.snapshot('test.png')
}
\dontshow{
	resUmatrix=GeneralizedUmatrix(Data[1:2,],ProjectedPoints[1:2,])
	#plotTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches)#testing takes longer than 5 secs
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