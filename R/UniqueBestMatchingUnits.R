UniqueBestMatchingUnits=function(UniqueBestMatchingUnits){
# [UniqueBM,UniqueInd,Uniq2AllInd] =UniqueUniqueBestMatchingUnits(UniqueBestMatchingUnits);
# doppelte UniqueBestMatchingUnits weglassen
# 
# INPUT
# UniqueBestMatchingUnits(1:n,1:n,1:n)   UniqueBestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]
#
# OUTPUT
# UniqueBM(1:u,1:u,1:u)      UniqueBM =[UBMkey, UBMLineCoords, UBMColCoords]
# UniqueInd                  Index such that UniqueBM       = UniqueBestMatchingUnits(UniqeInd,:);
# Uniq2AllInd                Index such that UniqueBestMatchingUnits    = UniqueBM(Uniq2AllInd,:)

# ALU 2019, reimplemented by MCT in 2021
#ToDo implement Uniq2AllInd
  
BMKey        = UniqueBestMatchingUnits[,1]
BMLineCoords = UniqueBestMatchingUnits[,2]
BMColCoords  = UniqueBestMatchingUnits[,3]

HashCode =  BMLineCoords*100000 +BMColCoords ; # damit Uniq anwendbar wird

# uniqueanwenden

UniqHash=unique(HashCode,fromLast = F)
UniqueInd = which(!duplicated(HashCode,fromLast = FALSE))

#[UniqHash,UniqueInd,Uniq2AllInd] = unique(HashCode)

    
# auf  UniqueBestMatchingUnits uebertragen
UniqueBM= UniqueBestMatchingUnits[UniqueInd,];


return(list(UniqueBM=UniqueBM,UniqueInd=UniqueInd,Uniq2AllInd=NULL))
 }

