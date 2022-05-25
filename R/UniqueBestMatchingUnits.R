  UniqueBestMatchingUnits = function(NonUniqueBestMatchingUnits){
  # [UniqueBM,UniqueInd,Uniq2AllInd] =UniqueUniqueBestMatchingUnits(NonUniqueBestMatchingUnits);
  # doppelte NonUniqueBestMatchingUnits weglassen
  # 
  # INPUT
  # NonUniqueBestMatchingUnits(1:n,1:n,1:n)   NonUniqueBestMatchingUnits =[BMkey, BMLineCoords, BMColCoords]
  #
  # OUTPUT
  # UniqueBM(1:u,1:u,1:u)      UniqueBM =[UBMkey, UBMLineCoords, UBMColCoords]
  # UniqueInd                  Index such that UniqueBM       = NonUniqueBestMatchingUnits(UniqeInd,:);
  # Uniq2AllInd                Index such that NonUniqueBestMatchingUnits    = UniqueBM(Uniq2AllInd,:)
  
  # ALU 2019, reimplemented by MCT in 2021
  #ToDo implement Uniq2AllInd
    
  BMKey        = NonUniqueBestMatchingUnits[,1]
  BMLineCoords = NonUniqueBestMatchingUnits[,2]
  BMColCoords  = NonUniqueBestMatchingUnits[,3]
  
  HashCode =  BMLineCoords*100000 +BMColCoords ; # damit Uniq anwendbar wird
  
  UniqHash=unique(HashCode,fromLast = F)
  UniqueInd = which(!duplicated(HashCode,fromLast = FALSE))
  Uniq2AllInd = unlist(lapply(HashCode, function(x) UniqueInd[which(UniqHash == x)]))
  
  #[UniqHash,UniqueInd,Uniq2AllInd] = unique(HashCode)
  
      
  # auf  NonUniqueBestMatchingUnits uebertragen
  UniqueBM= NonUniqueBestMatchingUnits[UniqueInd,];


  return(list("UniqueBM"    = UniqueBM,
              "UniqueInd"   = UniqueInd,
              "Uniq2AllInd" = Uniq2AllInd))
}

