\name{UniqueBestMatchingUnits}
\alias{UniqueBestMatchingUnits}
\title{UniqueBestMatchingUnits}
\description{
UniqueBestMatchingUnits
}
\usage{
UniqueBestMatchingUnits(NonUniqueBestMatchingUnits)
}
\arguments{
\item{NonUniqueBestMatchingUnits}{(1:n,1:n,1:n) UniqueBestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]}
}
\value{
\item{UniqueBM}{(1:u,1:u,1:u) UniqueBM =[UBMkey, UBMLineCoords, UBMColCoords]}
\item{UniqueInd}{Index such that UniqueBM = UniqueBestMatchingUnits(UniqeInd,:)}
\item{Uniq2AllInd}{Index such that UniqueBestMatchingUnits    = UniqueBM(Uniq2AllInd,:)}
}
\author{
ALU 2021 in matlab, MCT reimplemented in R
}