\name{GeneralizedUmatrix}
\alias{GeneralizedUmatrix}
\alias{getUmatrix4Projection}
\title{ Generalized U-Matrix for projection methods }
\usage{
GeneralizedUmatrix(Data,ProjectedPoints,
PlotIt=TRUE,Cls=NULL,Toroid=TRUE,Tiled=FALSE,ComputeInR=FALSE)
}
\arguments{
\item{Data}{[1:n,1:d] array of data: n cases in rows, d variables in columns}
\item{ProjectedPoints}{[1:n,2]n by 2 matrix containing coordinates of the Projection: A matrix of the fitted configuration.}
\item{PlotIt}{
 Optional,bool, defaut=FALSE, if =TRUE: U-Marix of every current Position of Databots will be shown
}
\item{Cls}{
Optional, For plotting, see \code{plotUmatrix} in package Umatrix
}
\item{Toroid}{
Optional, Default=FALSE, 

==FALSE planar computation

==TRUE: toroid borderless computation, set so only if projection method is also toroidal
}
\item{Tiled}{
Optional,For plotting see \code{plotUmatrix} in package Umatrix
}
\item{ComputeInR}{
Optional,  =T: Rcode, =F Cpp Code
}

}
\value{List with
\item{Umatrix}{
 [1:Lines,1:Columns] (see \code{ReadUMX} in package DataIO)
}
\item{EsomNeurons}{
[Lines,Columns,weights]       3-dimensional numeric array (wide format), not wts (long format)
}
\item{Bestmatches}{
[1:n,OutputDimension]   GridConverted Projected Points information converted by convertProjectionProjectedPoints() to predefined Grid by Lines and Columns
}
\item{gplotres}{
 Ausgabe von ggplot
}

\item{unbesetztePositionen}{
 Umatrix[unbesetztePositionen] =NA
}

}
\description{Generalized U-Matrix visualizes high-dimensional distance and density based structurs in two-dimensional scatter plots of projectios methods like CCA,MDS,PCA or NeRV
with the help of a topographic map with hypsometrioc tints [Thrun et al. 2016] based on the Umatrix method for emergent SOMs [Ultsch 2003], for further explanation see [Thrun,2017]}
\author{Michael Thrun}

\references{
[Ultsch, 2003]  Ultsch, A.: Maps for the visualization of high-dimensional data spaces, Proc. Workshop on Self organizing Maps (WSOM), pp. 225-230, Kyushu, Japan, 2003.


[Thrun et al., 2016]  Thrun, M. C., Lerch, F., Loetsch, J., & Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision (WSCG), Vol. 24, Plzen, http://wscg.zcu.cz/wscg2016/short/A43-full.pdf, 2016.


[Thrun, 2017]   Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2016.


[Ultsch/Thrun, 2017]  Ultsch, A., & Thrun, M. C.: Credible Visualizations for Planar Projections, in Cottrell, M. (Ed.), 12th International Workshop on Self-Organizing Maps and Learning Vector Quantization, Clustering and Data Visualization (WSOM), IEEE Xplore, France, 2017.

}
\examples{
data("Lsun3D")
Data=Lsun3D$Data
Cls=Lsun3D$Cls
InputDistances=as.matrix(dist(Data))
res=cmdscale(d=InputDistances, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE)
ProjectedPoints=as.matrix(res$points)
# Stress = KruskalStress(InputDistances, as.matrix(dist(ProjectedPoints)))
#resUmatrix=GeneralizedUmatrix(Data,ProjectedPoints)
#plotTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches,Cls)
}
\keyword{topographic}% use one of  RShowDoc("KEYWORDS")
\keyword{map}% __ONLY ONE__ keyword per line
\keyword{visualization}% __ONLY ONE__ keyword per line
\keyword{U-Matrix}% __ONLY ONE__ keyword per line
\keyword{Umatrix}% __ONLY ONE__ keyword per line
\keyword{GeneralizedUmatrix}% __ONLY ONE__ keyword per line