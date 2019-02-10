normUmatrix <- function(aumx, umx){
  # Normiert eine Umatrix mithilfe einer abstrakten Umatrix (aus getAbstractUMatrix)
  #
  # INPUT
  #   aumx      Abstrakte Umatrix, Übergeben als Matrix die ein toroides Gitter repräsentiert
  #   umx       Umatrix, Übergeben als Matrix die ein toroides Gitter repräsentiert
  #
  # OUTPUT
  #     Normierte Umatrix
  
  
  if(any(dim(aumx) != dim(umx))){
    stop("Abstract umatrix and umatrix must have the same size")
  }
  
  meana <- meanrobust(as.vector(aumx))
  meanu <- meanrobust(as.vector( umx))
  
  stda <- as.numeric(stdrobust(as.vector(aumx)))
  stdu <- as.numeric(stdrobust(as.vector( umx)))
  
  normaumx <- ((aumx - meana)/stda)
  normumx <- ((umx - meanu)/stdu)
  
  nas = which(is.na(normaumx),arr.ind = T)
  normaumx[nas] = normumx[nas]
  
  return(normaumx)
}

BorderToGridwalkCoord <- function(Border, Size) {
  #
  # Berechnet aus aus einem Vektor der Start- und Endkoordinaten einer Linie enthält die Koordinaten 
  # aller Punkte in einem toroiden Gitter aus denen diese Linie besteht
  #
  # INPUT
  # Border    Vektor der Länge 4 benötigt der Anfangs und Endpunkte der Linie in x und y Achse beschreibt.
  # Size      Vektor der Länge 2 der die größe des Gitters angibt.
  # 
  # OUTPUT
  # points    Matrix mit 2 dimensionalen Gitterkoordinaten.
  #
  # AUTHOR
  # Felix Pape 10/2016
  
  round <- function(x, n = 0){
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }
  
  start <- round(c(as.numeric(Border[1]), as.numeric(Border[2])))
  end   <- round(c(as.numeric(Border[3]), as.numeric(Border[4])))
  
  dx <- end[1] - start[1]
  dy <- end[2] - start[2]
  
  nx <- abs(dx)
  ny <- abs(dy)
  
  sign_x = (dx > 0) * 2 - 1 # R Type magic. TRUE essentally == 1, FALSE == 0
  sign_y = (dy > 0) * 2 - 1
  
  points <- matrix(NA, nrow = nx + ny + 1, ncol = 2)
  points[1,] <- start
  
  ix <- 0
  iy <- 0
  i <- 2
  
  while(ix < nx || iy < ny){
    if((0.5+ix)/nx < (0.5+iy)/ny){
      # next step horizontal
      points[i,] <- points[i-1,] + c(sign_x, 0)
      ix <- ix + 1
    } else {
      # next step vertical
      points[i,] <- points[i-1,] + c(0, sign_y)
      iy <- iy + 1
    }
    i <- i + 1
  }
  
  points[,1] = ((points[,1] - 1) %% (Size[1])) + 1
  points[,2] = ((points[,2] - 1) %% (Size[2])) + 1
  
  return(points)
}
getToroidVoronoiBorders <- function(BestMatches, Size){
  # Creates a matrix containing the start and end coordinates of the borders of the toroid voronoi cells
  #
  # INPUT
  # BestMatches     Matrix containing the Index and Positions of the best matching units
  # Size            Vector of length 2. Size of the grid to be used. Probably size(Umatrix)
  #
  # OUTPUT
  # borders         stripped down version of the dirsgs data.frame returned by deldir, containing
  #                           the beginning and end points of toroid voronoi borders [,1:4] and
  #                           the corresponding BestMatch ids of the border [,5:6]
  #
  # AUTHOR
  # Felix Pape 10/2016
  
  ninetileBM <- function(BestMatches, Size){
    Lines = Size[2]
    Columns = Size[1]
    
    b = BestMatches[,2:3]
    b = rbind(b, cbind(b[,1] + Columns, b[,2]), cbind(b[,1] - Columns, b[,2]))
    b = rbind(b, cbind(b[,1], b[,2] + Lines), cbind(b[,1], b[,2] - Lines))
    return(b)
  }
  originds = BestMatches[,1]
  bmulen = dim(BestMatches)[1]
  b9     = ninetileBM(BestMatches,Size)
  
  # Hierbei fallen Bestmatches mit gleicher Position im Gitter raus
  # Das ist tendentiell schlecht, da sp?ter Distanzen zwischen Bestmatches
  # mit gemeinsamen Kanten berechnet werden muessen.
  # Moeglichkeit diese Bestmatches zu finden:
  # Die Indizes von 1 bis bmulen die in do$ind.orig NICHT auftreten.
  # Fragen:
  # Wie soll mit diesen Bestmatches umgegangen werden?
  # Wie wird die Distanz zwischen 2 Voronoi Zellen berechnet, wenn mindestens eine 2 unterschiedliche Kerne hat?
  # Derzeit wird dieses "Problem" ignoriert.
  do     = deldir::deldir(b9[,1],b9[,2])
  
  inds   = which(do$ind.orig[do$dirsgs[,5]] %in% 1:length(originds) | do$ind.orig[do$dirsgs[,6]]  %in% 1:length(originds))
  
  borders= do$dirsgs[inds,]
  
  borders[,5] = originds[((do$ind.orig[do$dirsgs[inds,5]] - 1) %% (bmulen )) + 1]
  borders[,6] = originds[((do$ind.orig[do$dirsgs[inds,6]] - 1) %% (bmulen )) + 1]
  
  return(borders[,1:6])
}
getAbstractUMatrix <- function(BestMatches, Size, Data, BorderAlg = BorderToGridwalkCoord){
  #
  # Generate abstract umatrix. Be aware: Not all points in the resulting grid have a value assigned to them and stay NA.
  #
  # INPUT
  # BestMatches     Matrix containing the coordinates of the best matching units on the grid
  # Size            Vector of 2: Size of the grid to create the abstract umatrix upon. Should be size(umatrix) of the corresponding umatrix
  # Data            Matrix of data for which the abstract umatrix is going to be created
  # BorderAlg       Function used to create grid coordinates from border coordinates. BorderToGridwalkCoord (grid walking algorithm), or BorderToGridCoord (linear interpolation)
  #
  # OUTPUT
  # A matrix representing a 2 dimensional toroid grid filled with all edges of the abstract umatrix. All other points are NA.
  #
  # AUTHOR
  # Felix Pape 11/2016
  
  if(ncol(BestMatches) != 3)
    stop("BestMatches must be in the usual format. 3 columns: ID, Line, Column")
  if(!is.vector(Size) || length(Size) != 2)
    stop("Size expects a vector of length 2. See the result of size for a (u)matrix")
  
  uni = uniquePoints(BestMatches[,2:3])
  BestMatches = cbind(uni$sortind, uni$unique[,1], uni$unique[,2])
  
  borders = getToroidVoronoiBorders(BestMatches, Size)
  
  gridcoords = apply(borders[,1:4], 1, BorderAlg, Size = Size)
  
  #gridcordmat = do.call(rbind, gridcoords)
  
  grid = matrix(NA, nrow = Size[1], ncol = Size[2])
  
  for (i in 1:length(gridcoords)) {
    if(!is.vector(gridcoords[[i]]))
      curbor = (gridcoords[[i]])
    else
      curbor = t(matrix(gridcoords[[i]], nrow = 2))
    curbms = borders[names(gridcoords)[i], 5:6]
    
    d = as.numeric(dist(rbind(Data[curbms$ind1, ], Data[curbms$ind2, ])))
    
    grid[curbor] = apply(cbind(d,grid[curbor]), MARGIN = 1, max, na.rm=T)
  }
  
  return(grid);
}

NormalizeUmatrix = function(Data, Umatrix, BestMatches) {
  if (!is.matrix(BestMatches))
    stop('Bestmatches have to be a matrix')
  else
    b = dim(BestMatches)
  
  if (b[2] > 3 | b[2] < 2)
    stop(paste0('Wrong number of Columns of Bestmatches: ', b[2]))
  if (b[2] == 2) {
    Points = BestMatches
    #With Key
    BestMatches = cbind(1:b[1], BestMatches)
  } else{
    Points = BestMatches[, 2:3]
  }
  d = dim(Umatrix)
  if (is.null(d)) {
    stop('Umatrix Dimension is null. Please check Input')
  }
  
  requireNamespace('matrixStats')
  mini = matrixStats::colMins(Points, na.rm = TRUE)
  maxi = matrixStats::colMaxs(Points, na.rm = TRUE)
  if (sum(mini) < 2) {
    stop('Some Bestmatches are below 1 in X or Y/Columns or Lines')
  }
  if (d[1] < maxi[1]) {
    stop(paste0(
      'Range of Bestmatches',
      maxi[1],
      ' is higher than Range of Umatrix',
      d[1]
    ))
  }
  if (d[2] < maxi[2]) {
    stop(paste0(
      'Range of Bestmatches',
      maxi[2],
      ' is higher than Range of Umatrix',
      d[2]
    ))
  }
  if (b[1] != nrow(Data))
    stop('Number of Data cases do not equal Number of BestMatches')
  aumx = getAbstractUMatrix(BestMatches, Size = size(Umatrix), Data = Data)
  normalized = normUmatrix(aumx, Umatrix)
  
  return(normalized)
}