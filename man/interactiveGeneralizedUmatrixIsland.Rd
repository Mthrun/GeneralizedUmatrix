\name{interactiveGeneralizedUmatrixIsland}
\alias{interactiveGeneralizedUmatrixIsland}
\title{GUI for cutting out an Island.}

\usage{
interactiveGeneralizedUmatrixIsland(Umatrix, Bestmatches=NULL, Cls=NULL)
}

\description{The toroid Umatrix is usually drawn 4 times, so that connected areas on borders can be seen as a whole. An island is a manual cutout of such a tiled visualization, that is selected such that all connected areas stay intact. This shiny tool allows the user to do this manually.}

\arguments{
  \item{Umatrix}{[1:Lines,1:Columns] Matrix of Umatrix Heights}
  \item{Bestmatches}{Array with positions of Bestmatches}
  \item{Cls}{Classification of the Bestmatches}
}
\value{Boolean Matrix that represents the island within the tiled Umatrix.}

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

\author{Michael Thrun}
\note{
This function is a depricated version of a function from the Umatrix packages created by Florian Lerch and Michael Thrun
}
\references{
Thrun, M. C., Lerch, F., Loetsch, J., Ultsch, A.: Visualization and 3D Printing of Multivariate Data of Biomarkers, in Skala, V. (Ed.), International Conference in Central Europe on Computer Graphics, Visualization and Computer Vision,Plzen, 2016. 
}
