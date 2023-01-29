tileGUM=function(Umatrix, BestMatches = NULL, Cls = NULL,Separate=FALSE) 
{
  rows = nrow(Umatrix)
  cols = ncol(Umatrix)
  Umatrix <- Umatrix[c(1:rows, 1:rows), c(1:cols, 1:cols)]
  bm_keys = NULL
  if (!is.null(BestMatches)) {
    if (ncol(BestMatches) == 3) {
      bm_keys = BestMatches[, 1]
      BestMatches = BestMatches[, c(2, 3)]
    }
    else {
      bm_keys = 1:nrow(BestMatches)
    }
    bmRow <- nrow(BestMatches)
    BestMatches <- BestMatches[rep(1:bmRow, 4), ]
    BestMatches[(bmRow + 1):(2 * bmRow), 1] <- BestMatches[(bmRow + 
                                                              1):(2 * bmRow), 1] + rows
    BestMatches[(2 * bmRow + 1):(3 * bmRow), 2] <- BestMatches[(2 * 
                                                                  bmRow + 1):(3 * bmRow), 2] + cols
    BestMatches[(3 * bmRow + 1):(4 * bmRow), 1] <- BestMatches[(3 * 
                                                                  bmRow + 1):(4 * bmRow), 1] + rows
    BestMatches[(3 * bmRow + 1):(4 * bmRow), 2] <- BestMatches[(3 * 
                                                                  bmRow + 1):(4 * bmRow), 2] + cols
  }
  if (!is.null(Cls)) {
    if(isTRUE(Separate)){
    TiledCls = c(Cls+1000,Cls+2000,Cls+3000,Cls+4000)
    }
    Cls <- rep(Cls, 4)
  }
  if (!is.null(bm_keys)) 
    BestMatches = cbind(rep(bm_keys, 4), BestMatches)
  
  if(isFALSE(Separate)){
    return(list(GeneralizedUmatrix = Umatrix, BestMatchingUnits = BestMatches, Cls = Cls))
  }else{
   
    return(list(GeneralizedUmatrix = Umatrix, BestMatchingUnits = BestMatches, Cls = Cls,TiledCls=TiledCls))
  }
    
}