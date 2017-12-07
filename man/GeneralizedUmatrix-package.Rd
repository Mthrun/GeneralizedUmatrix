\name{GeneralizedUmatrix-package}
\alias{GeneralizedUmatrix-package}
\docType{package}
\title{
GeneralizedUmatrix
}
\description{
 Projections from a high dimensional data space onto a two dimensional plane are used to detect structures, such as clusters, in multivariate data. The generalized Umatrix is able to visualize errors of these two-dimensioal scatter plots by using a 3D topographic map.
}
\details{
\tabular{ll}{
Package: \tab GeneralizedUmatrix\cr
Type: \tab Package\cr
Version: \tab 0.9.4\cr
Date: \tab 2017-05-15\cr
License: \tab CC BY-NC-SA 4.0\cr
}

Generalized Umatrix see Phd thesis, chapter 4-5
}
\author{
Michal Thrun

Maintainer: Michael Thrun <mthrun@informatik.uni-marburg.de>
}
\references{
[Thrun, 2017]  Thrun, M. C.:A System for Projection Based Clustering through Self-Organization and Swarm Intelligence, (Doctoral dissertation), Philipps-Universität Marburg, Marburg, 2017.


[Ultsch/Thrun, 2017]  Ultsch, A., & Thrun, M. C.: Credible Visualizations for Planar Projections, in Cottrell, M. (Ed.), 12th International Workshop on Self-Organizing Maps and Learning Vector Quantization, Clustering and Data Visualization (WSOM), IEEE Xplore, France, 2017.

}
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
plotTopographicMap(resUmatrix$Umatrix,resUmatrix$Bestmatches,Cls)
}
##Interactive Island Generation 
## from a tiled Umatrix (toroidal assumption)
\dontrun{
	Imx = interactiveGeneralizedUmatrixIsland(resUmatrix$Umatrix,

	resUmatrix$Bestmatches)
	plotTopographicMap(resUmatrix$Umatrix,

	resUmatrix$Bestmatches, Imx = Imx)
}
}
\keyword{GeneralizedUmatrix}
\keyword{U-matrix}
\keyword{Umatrix}
\keyword{U-Matrix}
\keyword{Visualization}
\keyword{DimensionalityReduction}