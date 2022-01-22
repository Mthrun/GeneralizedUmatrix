LowLand = function(BestMatchingUnits,GeneralizedUmatrix,Data=NULL,Cls=NULL,Key=NULL,LowLimit){
# Reduction der BestMatchingUnits auf diejenigen, die in U-matrix nicht auf hohen Bergen liegen und auf 1 Datensatz pro BM
# [LowLandBM,LowLandInd]   =  LowLand(BestMatchingUnits,GeneralizedUmatrix)
# [LowLandBM,LowLandInd,LowLandData,LowLandCls,LowLandKey]  =  LowLand(BestMatchingUnits,GeneralizedUmatrix,Data,Cls,Key)
#
# INPUT
# BestMatchingUnits(1:n,1:n,1:n)   BestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]  
# GeneralizedUmatrix(1:l,1:c)           U-Matrix heights in Matrix form
#
# OPTINAL
# Data(1:n,1:d)              data cases in lines, variables in Columns or [] or 0
# Cls(1:n)                   a possible classif( ication of the data or [] or 0
# Key(1:n)                   the keys of the data or [] or 0
# LowLimit                   GeneralizedUmatrix heights up to this are considered ti lie in the low lands
#                            default: LowLimit = prctile(Uheights,80)
       # nur die 80# tiefsten
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
  if (is.null(Data)) {
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
  Uheights = Uheights4Data(BestMatchingUnits, GeneralizedUmatrix)$Uheights
  # alle U-hoehen
  if (missing(LowLimit)) {
    LowLimit = quantile(Uheights,
                        p = 80 / 100,
                        type = 5,
                        na.rm = TRUE)
  }
  # init Output to zeros(n,1)
  print(LowLimit)
  
  # nur die 80# tiefsten
  V = UniqueBestMatchingUnits(BestMatchingUnits)
  UniqueBM = V$UniqueBM
  UniqueInd = V$UniqueInd
  
  # BM unique machen
  # reduktion auf die in den Low lands
  V = Uheights4Data(UniqueBM, GeneralizedUmatrix)
  UniqUheights = V$Uheights
  UniqBMLineCoords = V$BMLineCoords
  UniqBMColCoords = V$BMColCoords
  
  # die U-hoehen besorgen
  LowUniqInd = which(UniqUheights <= LowLimit)
  # welce sind in den Low Lands
  LowLandInd = UniqueInd[LowUniqInd]
  # dies ist der Index des gesuchten LowLand values
  LowLandBM   = BestMatchingUnits[LowLandInd, ]
  LowLandData = Data[LowLandInd, ]
  LowLandCls  = Cls[LowLandInd]
  LowLandKey  = Key[LowLandInd]
  V = list(LowLandBM, LowLandInd, LowLandData, LowLandCls, LowLandKey)
  names(V) = c('LowLandBM',
               'LowLandInd',
               'LowLandData',
               'LowLandCls',
               'LowLandKey')
return(V)}