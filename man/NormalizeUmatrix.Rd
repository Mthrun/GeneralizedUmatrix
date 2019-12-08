\name{NormalizeUmatrix}
\alias{NormalizeUmatrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize Umatrix
}
\description{
Normalizing the U-matrix using the abstact U-Matrix concept [Loetsch/Ultsch, 2014].
}
\usage{
NormalizeUmatrix(Data, Umatrix, BestMatches)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Data}{
[1:n,1:d] numerical matrix of data with n cases and d variables
}
  \item{Umatrix}{
[1:lines,1:Columns] matrix of U-heights
}
  \item{BestMatches}{
[1:n,1:2] Bestmatching units.
}
}
\details{
see publication [Loetsch/Ultsch, 2014]..
}
\value{
Normalized Umatrix[1:lines,1:Columns] using the abstact U-Matrix concept.
}
\references{
Loetsch, J., Ultsch, A.: Exploiting the structures of the U-matrix, in Villmann, T., Schleif, F.-M., Kaden, M. & Lange, M. (eds.), Proc. Advances in Self-Organizing Maps and Learning Vector Quantization, pp. 249-257, Springer International Publishing, Mittweida, Germany, 2014.

}
\author{
Felix Pape, Michael Thrun
}
\note{
Development Ongoing.
}

\examples{
#ToDo
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{abstract U-matrix}% use one of  RShowDoc("KEYWORDS")
\keyword{AU-Matrix}% __ONLY ONE__ keyword per line
