Uheights4Data = function(BestMatchingUnits,GeneralizedUmatrix){
# [Uheights,BMLineCoords,BMColCoords] =Uheights4Data(BestMatchingUnits,GeneralizedUmatrix)
# get the Uheights  = heights in the U-Matrix for all BestMatchingUnits 
# 
# INPUT
# BestMatchingUnits(1:n,1:d)            BMKey        = BestMatchingUnits[,1)
# GeneralizedUmatrix(1:Lines,1:Columns)      a GeneralizedUmatrix
#
# OUTPUT
# Uheights
# BMLineCoords
# BMColCoords
# ALU 2021 in matlab, MCT reimplemented in R
# EINGANGSKONTROLLE
  V = dim(GeneralizedUmatrix)
  Lines = V[1]
  Columns = V[2]
  
  BMKey        = BestMatchingUnits[, 1]
  BMLineCoords = BestMatchingUnits[, 2]
  BMColCoords  = BestMatchingUnits[, 3]
  if (min(BMLineCoords) < 1) {
    stop('Uheights4Data:min(BMLineCoords) < 1')
  }
  if (max(BMLineCoords) > Lines) {
    stop('Uheights4Data: max(BMLineCoords) > Lines')
  }
  if (min(BMColCoords) < 1) {
    stop('Uheights4Data:min(BMColCoords) < 1')
  }
  if (max(BMColCoords) > Columns) {
    stop('Uheights4Data: max(BMColCoordss) > Columns')
  }
  n = length(BMKey)
  Uheights = vector(mode = "numeric", length = n)
  # init
  for (i in 1:n) {
    l = BMLineCoords[i]
    c = BMColCoords[i]
    Uheights[i] = GeneralizedUmatrix[l, c]
  }
  # for(i in 1:n
  V = list(Uheights, BMLineCoords, BMColCoords)
  names(V) = c('Uheights', 'BMLineCoords', 'BMColCoords')
  return(V)}