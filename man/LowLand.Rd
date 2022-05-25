\name{LowLand}
\alias{LowLand}
\title{LowLand}
\description{
LowLand
}
\usage{
LowLand(BestMatchingUnits, GeneralizedUmatrix, Data, Cls, Key, LowLimit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{BestMatchingUnits}{(1:n,1:n,1:n) BestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]}
\item{GeneralizedUmatrix}{(1:l,1:c) U-Matrix heights in Matrix form}
\item{Data}{(1:n,1:d) data cases in lines, variables in Columns or [] or 0}
\item{Cls}{(1:n) a possible classif( ication of the data or [] or 0}
\item{Key}{(1:n) the keys of the data or [] or 0}
\item{LowLimit}{GeneralizedUmatrix heights up to this are considered to lie in
the low lands default: LowLimit = prctile(Uheights,80) nur die 80# tiefsten}
}
\value{
\item{LowLandBM}{the unique BestMatchingUnits in the low lands of an u-Matrix}
\item{LowLandInd}{index such that UniqueBM = BestMatchingUnits(UniqueInd,]}
\item{LowLandData}{Data reduced to LowLand:   LowLandData = Data(LowLandInd,]}
\item{LowLandCls}{Cls  reduced to LowLand:   LowLandCls  = Cls(LowLandInd)}
\item{LowLandKey}{Key  reduced to LowLand:   LowLandKey  = Key(LowLandInd)}
}
\author{
 ALU 2021 in matlab, MCT reimplemented in R
}
