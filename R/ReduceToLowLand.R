ReduceToLowLand = function(BestMatchingUnits,GeneralizedUmatrix,Data=NULL,Cls=NULL,Key=NULL,LowLimit,Force=FALSE){
# wenn zuviele BM dann werden die Auf die LowLandBM reduziert
# BestMatchingUnits =  ReduceToLowLand(BestMatchingUnits,GeneralizedUmatrix)
# [BestMatchingUnits,Data,Cls,Key,LowLandInd] =  ReduceToLowLand(BestMatchingUnits,GeneralizedUmatrix,Data,Cls,Key,LowLimit)
#
# INPUT
# BestMatchingUnits(1:n,1:n,1:n)   BestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]  
# GeneralizedUmatrix(1:l,1:c)           U-Matrix heights in Matrix form
#
# OPTIONAL
# Data(1:n,1:d)              data cases in lines, variables in Columns or [] or 0
# Cls(1:n)                   a possible classif( ication of the data or [] or 0
# Key(1:n)                   the keys of the data or [] or 0
# LowLimit                   GeneralizedUmatrix heights up to this are considered to lie in the low lands
#                            default: LowLimit = prctile(Uheights,80)
#                            nur die 80# tiefsten
# Force                     ==TRUE: Always perform reduction
#
# OUTPUT 
# LowLandBM                 the unique BestMatchingUnits in the low lands of an u-Matrix           
# LowLandInd                index such that UniqueBM = BestMatchingUnits(UniqueInd,]
#
# LowLandData               Data reduced to LowLand:   LowLandData = Data(LowLandInd,]
# LowLandCls                Cls  reduced to LowLand:   LowLandCls  = Cls(LowLandInd)
# LowLandKey                Key  reduced to LowLand:   LowLandKey  = Key(LowLandInd)
# ALU 2021 in matlab, MCT reimplemented in R
# DEFAULTS 
# BestMatchingUnits,GeneralizedUmatrix,Data,Cls,Key,LowLimit
#    1           2      3   4   5     6       
  if(is.null(Data)) {
    Data = BestMatchingUnits[, 1]*0
  }
  # init Output to zeros(n,1)
  if (is.null(Cls)) {
    Cls  = BestMatchingUnits[, 1]*0
  }
  # init Output to zeros(n,1)
  if (is.null(Key)) {
    Key  = BestMatchingUnits[, 1]*0
  }

  # alle U-hoehen
  if (missing(LowLimit)) {
    Uheights = Uheights4Data(BestMatchingUnits, GeneralizedUmatrix)$Uheights
    LowLimit = quantile(Uheights,
                        p = 80 / 100,
                        type = 5,
                        na.rm = TRUE)
  }
  # init Output to zeros(n,1)
#print(LowLimit)

  # nur die 80# tiefsten
  V=dim(GeneralizedUmatrix)
  l = V[1]
  c = V[2]
  AnzNeuronen = l*c
  # anzahl Neuronen
  BMKey        = BestMatchingUnits[, 1]
  AnzBM        = length(BMKey)
  #  soviele BM
  if (AnzBM >   AnzNeuronen |isTRUE(Force)) {
    # reduktion noetig
   # 'NOTE: BestMatchingUnits reduced to Lowlands!'                                                    # ansage!
    V =  LowLand(BestMatchingUnits = BestMatchingUnits, 
                 GeneralizedUmatrix = GeneralizedUmatrix, 
                 Data = Data, Cls = Cls, Key = Key, LowLimit = LowLimit)
  
    BestMatchingUnitsOut = V$LowLandBM
    LowLandInd = V$LowLandInd
    DataOut = V$LowLandData
    ClsOut = V$LowLandCls
    KeyOut = V$LowLandKey
    LowLimit = V$LowLimit
    # reduktion auf LowLands
  } else{
    LowLandInd = c(1:AnzBM)
    BestMatchingUnitsOut=BestMatchingUnits
    DataOut=Data
    ClsOut=Cls
    KeyOut=Key
    message("No Reduction performed")
    # keine Reduktion
  }
  V=list(BestMatchingUnitsOut, DataOut, ClsOut, KeyOut, LowLandInd,LowLimit)
  names(V) = c('BestMatchingUnits', 'Data', 'Cls', 'Key', 'LowLandInd','LowLimit')
  return(V
  )
}