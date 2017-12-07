plotTopographicMap <- function(GeneralizedUmatrix, BestMatchingUnits=NULL, Cls=NULL, ClsColors=NULL,Imx=NULL, Tiled=FALSE, BmSize=0.5,ShowAxis=F){
# plotTopographicMap(GeneralizedUmatrix, BestMatchingUnits, Cls, Tiled)
# Draws a plot of given GeneralizedUmatrix
# INPUT
# OPTIONAL
# GeneralizedUmatrix(1:Lines,1:Columns)	      GeneralizedUmatrix to be plotted
# BestMatchingUnits(1:n,1:2)		      Positions of BestMatchingUnits to be plotted onto the GeneralizedUmatrix
# Cls(1:n)			            class identifier for the bestmatch at the given point
# ClsColors(1:m)			Vector of colors that will be used to colorize the different classes
# Tiled				                  should the GeneralizedUmatrix be drawn 4times?
# HeightScale                 how high should the mountains be?
# TextureRendering            use the 2D GeneralizedUmatrix Function to create a Texture and use this
              
# author: MT
  #Bei einer Insel muss die Umatrix vervierfacht werden damit funktion funktioniert
if(!is.null(Imx))
  Tiled=TRUE

  #TextureRendering=F only with Umatrix package
  if(missing(GeneralizedUmatrix)) stop('GeneralizedUmatrix is missing.')
  # OUTPUT
	if(!requireNamespace("rgl", quietly = T)) stop("Package Rgl could not be loaded.")

  ## MT: Normalization der GeneralizedUmatrix werte
  # Milligan, Copper 1988 A Study of Standadization of Variables in Cluster Analysis,
  # robust Normalization Z_5 :"Z_5 is bounded by 0.0 and 1.0 with at least one observed value at each of these end points"

  quants=quantile(as.vector(GeneralizedUmatrix),c(0.01,0.5,0.99))
  minU=quants[1]
  maxU=quants[3]

  GeneralizedUmatrix=(GeneralizedUmatrix-minU)/(maxU-minU)

  quants2=quantile(as.vector(GeneralizedUmatrix),c(0.01,0.5,0.99))
  minU2=quants2[1]
  maxU2=quants2[3]

  ### Hoehe aus GeneralizedUmatrix schaetzen
  #Verhaeltnis zwischen minhoehe/maxHoehe=1/HeightScale
      HeightScale=round(maxU2/(2*max(minU2,0.05)),0) #Factor 2 damit die GeneralizedUmatrix Hoehe nicht zu riesig wird

      # proportionen sind fuer standard GeneralizedUmatrix der groesse 80x50 gedacht. fuer andere groessen linear skalieren
      stretchFactor = sqrt(nrow(GeneralizedUmatrix)^2 + ncol(GeneralizedUmatrix)^2) / sqrt(50^2 + 80^2)

  #MT: Die Level muessen vor der Begrenzung der Werte auf 0 und 1 gesetz werden,
  #aber nachdem der Wertebereich umgeschoben wurde, damit die Dichte in den Grenzbereichen abgeschetzt werden kann

  # proportionen sind fuer standard GeneralizedUmatrix der groesse 80x50 gedacht. fuer andere groessen linear skalieren
  stretchFactor = sqrt(nrow(GeneralizedUmatrix)^2 + ncol(GeneralizedUmatrix)^2) / sqrt(50^2 + 80^2)

  indMax=which(GeneralizedUmatrix>1,arr.ind=T)
  indMin=which(GeneralizedUmatrix<0,arr.ind=T)
  if(length(indMax)>0)
    GeneralizedUmatrix[indMax]=1
  if(length(indMin)>0)
    GeneralizedUmatrix[indMin]=0
##########################################################################################
  #diverse Checks
##########################################################################################
  
  if (!is.matrix(BestMatchingUnits))
    stop('Bestmatches have to be a matrix')
  else
    b = dim(BestMatchingUnits)
  
  if (b[2] > 3 | b[2] < 2)
    stop(paste0('Wrong number of Columns of Bestmatches: ', b[2]))
  if (b[2] == 2) {
    Points = BestMatchingUnits
    #With Key
    BestMatchingUnits = cbind(1:b[1], BestMatchingUnits)
  } else{
    Points = BestMatchingUnits[, 2:3]
  }
  d = dim(GeneralizedUmatrix)
  if (is.null(d)) {
    stop('GeneralizedUmatrix Dimension is null. Please check Input')
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
      ' is higher than Range of GeneralizedUmatrix',
      d[1]
    ))
  }
  if (d[2] < maxi[2]) {
    stop(paste0(
      'Range of Bestmatches',
      maxi[2],
      ' is higher than Range of GeneralizedUmatrix',
      d[2]
    ))
  }
if(is.null(Cls)) Cls=rep(1,b[1])
 
if(is.null(ClsColors)){
 ClsColors=DefaultColorSequence()
 }else{
	if(length(unique(Cls))!=length(ClsColors)){
		stop('Length of vector of Clscolor does not match the number of unique Clusters in Cls.')
	}
} 
##########################################################################################
## Textur generieren
##########################################################################################
 #requires Umatrix packages 
  # if(TextureRendering){ #Aus showUmatrix3d, package Umatrix
  #   print("Please wait while the GeneralizedUmatrix texture gets generated")
  # 
  #   rotate <- function(x) t(apply(x, 2, rev))
  # 
  #   #rotatedGeneralizedUmatrix = apply(GeneralizedUmatrix, 2, function(x)rev(x))
  #   rotatedGeneralizedUmatrix = t(GeneralizedUmatrix)
  #   if(is.null(Imx)) rotatedImx = NULL
  # 
  #   if(!is.null(Imx)){
  #     rotatedImx = t(Imx)
  #     oceanLine = apply(rotatedImx, 1, function(x) all(x==1))
  #     startLine = min(which(!oceanLine),na.rm=T)
  #     endLine = length(oceanLine) - min(which(rev(!oceanLine)),na.rm=T) + 1
  # 
  #     oceanCol = apply(rotatedImx, 2, function(x) all(x==1))
  #     startCol = min(which(!oceanCol),na.rm=T)
  #     endCol = length(oceanCol) - min(which(rev(!oceanCol)),na.rm=T) + 1
  # 
  #     Height = (endLine - startLine) * 8
  #     Width = (endCol - startCol) * 8
  #   }
  #   else{
  #     Width = ncol(rotatedGeneralizedUmatrix)*8
  #     if(Tiled) Width = Width*2
  #     ratio = nrow(rotatedGeneralizedUmatrix)/ncol(rotatedGeneralizedUmatrix)
  #     Height = Width*ratio
  #   }
  # 
  #   png("tmpGeneralizedUmatrix.png", width=Width, height = Height )
  #   print(plotUmatrix(rotatedGeneralizedUmatrix, Fast=F, Clean = T, Tiled = Tiled,
  #                     RemoveOcean = T, TransparentOcean = T,
  #                     Imx=rotatedImx,Nrlevels = 35)) #MT: hier konstanten Wert nehmen, damits klappt
  #   dev.off()
  # 
  #   print("Texture generation finished")
  # }


##########################################################################################
## 4fach Kachelung (tiling) durchfuehren
##########################################################################################
  
  TileGUM=function(Umatrix, BestMatches = NULL, Cls = NULL) 
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
      Cls <- rep(Cls, 4)
    }
    if (!is.null(bm_keys)) 
      BestMatches = cbind(rep(bm_keys, 4), BestMatches)
    list(GeneralizedUmatrix = Umatrix, BestMatchingUnits = BestMatches, Cls = Cls)
  }
  
  if(Tiled){
    tU <- TileGUM(GeneralizedUmatrix,BestMatchingUnits,Cls)
    GeneralizedUmatrix <- tU$GeneralizedUmatrix
    BestMatchingUnits <- tU$BestMatchingUnits
    Cls <- tU$Cls
  }


##########################################################################################
## groessere imx bauen
#########
  bigImx = Imx

  ##########
  ## an GeneralizedUmatrix rumrechnen
  ##########
  #Aus showUmatrix3d, package Umatrix
  zcol = cut(GeneralizedUmatrix,128) # divide the Height into 128 different levels for coloring

  lines = seq(1, nrow(GeneralizedUmatrix), len = nrow(GeneralizedUmatrix))
  columns = seq(1, ncol(GeneralizedUmatrix), len = ncol(GeneralizedUmatrix))


  Colormap=c(rep("#3C6DF0",2),c("#3C6DF0", "#006602", "#006A02", "#006D01", "#007101", 
                                "#007501", "#007901", "#007C00", "#008000", "#068103", 
                                "#118408", "#0B8305", "#17860A", "#1D870D", "#228810", 
                                "#288A12", "#2E8B15", "#348D18", "#398E1A", "#3F8F1D", 
                                "#45911F", "#4A9222", "#509325", "#569527", "#5C962A", 
                                "#61982C", "#67992F", "#6D9A32", "#729C34", "#789D37", 
                                "#7E9F39", "#84A03C", "#89A13F", "#8FA341", "#95A444", 
                                "#9AA547", "#A0A749", "#A6A84C", "#ACAA4E", "#B1AB51", 
                                "#B7AC54", "#BDAE56", "#C3AF59", "#C8B15B", "#CEB25E", 
                                "#CBAF5C", "#C8AC59", "#C5A957", "#C3A654", "#C0A352", 
                                "#BDA050", "#BA9D4D", "#B7994B", "#B49648", "#B29346", 
                                "#AF9044", "#AC8D41", "#A98A3F", "#A6873C", "#A3843A", 
                                "#A08138", "#9E7E35", "#9B7B33", "#987830", "#95752E", 
                                "#92722B", "#8F6E29", "#8C6B27", "#8A6824", "#876522", 
                                "#84621F", "#815F1D", "#7E5C1B", "#7B5918", "#795616", 
                                "#765313", "#714E0F", "#6C480B", "#674307", "#6F4D15", 
                                "#785822", "#806230", "#896D3E", "#91774C", "#998159", 
                                "#A28C67", "#AA9675", "#B3A183", "#BBAB90", "#C3B59E", 
                                "#CCC0AC", "#D4CABA", "#DDD5C7", "#E5DFD5", "#E7E1D8", 
                                "#E9E4DB", "#EBE6DE", "#ECE8E1", "#EEEAE4", "#F0EDE7", 
                                "#F2EFEA", "#F4F1ED", "#F6F4F0", "#F8F6F3", "#F9F8F6", 
                                "#FBFAF9", "#FDFDFC", "#FFFFFF", "#FFFFFF", "#FEFEFE", 
                                "#FEFEFE", "#FEFEFE", "#FDFDFD", "#FDFDFD", "#FDFDFD", 
                                "#FCFCFC", "#FCFCFC", "#FCFCFC", "#FBFBFB", "#FBFBFB", 
                                "#FBFBFB", "#FAFAFA", "#FAFAFA", "#FAFAFA", "#F9F9F9", 
                                "#F9F9F9", "#FFFFFF", "#FFFFFF"))
  Nrlevels2 = 2*HeightScale*stretchFactor #MT Farbintervalle gleich 2*hoehenintervalle, siehe oben
  levelBreaks <- seq(0,1.000001,length.out=(Nrlevels2+1))



##########################################################################################
## remove Heights by island
##########################################################################################
  
  if(!is.null(Imx)){#Aus showUmatrix3d, package Umatrix
    GeneralizedUmatrix[which(Imx == 1)] = 0
    #GeneralizedUmatrix[which(bigImx == 1)] = 0

    if(!is.null(BestMatchingUnits)){
      BestMatchingUnitsFilter = rep(T,nrow(BestMatchingUnits)) # every Bestmatch stays
      for(i in 1:nrow(Imx)){
        for(j in 1:ncol(Imx)){
          if(Imx[i,j] == 1){
            if(!is.null(BestMatchingUnits))
              BestMatchingUnitsFilter[(BestMatchingUnits[,2] == i) & (BestMatchingUnits[,3] == j)] = F
          }
        }
      }
      BestMatchingUnits = BestMatchingUnits[BestMatchingUnitsFilter,]
      if(!is.null(Cls)) Cls = Cls[BestMatchingUnitsFilter]
    }
      #### remove ocean around GeneralizedUmatrix
      oceanLine = apply(GeneralizedUmatrix, 1, function(x) all(x==0))
      startLine = min(which(!oceanLine),na.rm=T)
      endLine = length(oceanLine) - min(which(rev(!oceanLine)),na.rm=T) + 1

      oceanCol = apply(GeneralizedUmatrix, 2, function(x) all(x==0))
      startCol = min(which(!oceanCol),na.rm=T)
      endCol = length(oceanCol) - min(which(rev(!oceanCol)),na.rm=T) + 1

      if(!is.null(BestMatchingUnits)){
        BestMatchingUnits <- BestMatchingUnits - cbind(rep(0,nrow(BestMatchingUnits)),startLine-1,startCol-1)
      }
      GeneralizedUmatrix <- GeneralizedUmatrix[startLine:endLine,startCol:endCol]
      Imx <- Imx[startLine:endLine,startCol:endCol]
      bigImx <- bigImx[startLine:endLine,startCol:endCol]
  }




  # change GeneralizedUmatrix into heights
  splittedGeneralizedUmatrix = GeneralizedUmatrix
  for(i in 1:Nrlevels2){
    splittedGeneralizedUmatrix[ (GeneralizedUmatrix >= levelBreaks[i]) & (GeneralizedUmatrix <= levelBreaks[i+1]) ] = levelBreaks[i]
  }
  splittedGeneralizedUmatrix=(floor(splittedGeneralizedUmatrix * length(Colormap)))+1
  color = Colormap[splittedGeneralizedUmatrix]
  #if(!is.null(Imx)) color[which(Imx == 1)] = NA
  if(!is.null(Imx)) color[which(bigImx == 1)] = NA

  z<-GeneralizedUmatrix*HeightScale*stretchFactor #MT: Die Hoehe entspricht den GeneralizedUmatrixwerten, siehe vorne Proportion

  lines = seq(1, nrow(z), len = nrow(z))
  columns = seq(1, ncol(z), len = ncol(z))



##########################################################################################
## GeneralizedUmatrix darstellen
##########################################################################################
 #Aus showUmatrix3d, package Umatrix
   rgl::open3d()
  if(ShowAxis){
    rgl::material3d(col = "black")

    #if(!TextureRendering)
      rgl::persp3d(x=lines, y=columns, z=z, color=color, aspect=FALSE, lit=F,box=F, texmagfilter="nearest", texminfilter="nearest", texenvmap=TRUE)
   # else
    #  rgl::persp3d(x=lines, y=columns, z=z, col="white", texture="tmpGeneralizedUmatrix.png", textype="rgb",lit=F)
  }
  else{
    #if(!TextureRendering)
      rgl::surface3d(x=lines, y=columns, z=z, color=color, aspect=FALSE, lit=F)
   # else
     # rgl::surface3d(x=lines, y=columns, z=z, col="white", texture="tmpGeneralizedUmatrix.png", textype="rgb",lit=F)

  }

##########################################################################################
## BestMatchingUnits darstellen
##########################################################################################
#Aus showUmatrix3d, package Umatrix   
  if(!is.null(BestMatchingUnits)){

    ColorClass = c()
    for(i in 1:length(unique(Cls))) ColorClass[Cls == sort(unique(Cls))[i]] = i

    BestMatchingUnitsHeights <- sapply(1:nrow(BestMatchingUnits), function(x) z[BestMatchingUnits[x,2],BestMatchingUnits[x,3]])

    if(is.list(BestMatchingUnitsHeights))
      BestMatchingUnitsHeights = unlist(BestMatchingUnitsHeights)

    # DefaultColorSeq <- c('magenta','yellow','black','red','green', 'blue','cyan',
    #                      # jetzt kommen einfach alle R farben in random sequenz
    #                      'burlywood4',
    #                      'gray28',
    #                      'darksalmon',
    #                      'orchid2',
    #                      'plum4',
    #                      'gray41',
    #                      'plum',
    #                      'pink1',
    #                      'coral1',
    #                      'gray39',
    #                      'gray',
    #                      'gray67',
    #                      'slategray1',
    #                      'peachpuff1',
    #                      'floralwhite',
    #                      'hotpink',
    #                      'gray12',
    #                      'gray88',
    #                      'orange1',
    #                      'rosybrown1',
    #                      'royalblue',
    #                      'gray78',
    #                      'mediumturquoise',
    #                      'darkolivegreen',
    #                      'gray45',
    #                      'azure',
    #                      'peachpuff3',
    #                      'hotpink3',
    #                      'blue1',
    #                      'chocolate1',
    #                      'tomato',
    #                      'lightyellow',
    #                      'gainsboro',
    #                      'steelblue1',
    #                      'gray95',
    #                      'blue4',
    #                      'slategrey')
	DefaultColorSeq=ClsColors
    rgl::spheres3d(x=BestMatchingUnits[,2],BestMatchingUnits[,3], BestMatchingUnitsHeights, col = DefaultColorSeq[ColorClass], radius = BmSize)
  }

  # konturlinien zeichnen
 # if(!TextureRendering){
    lines = contourLines(lines,columns,z, nlevels = 35)
     for (i in seq_along(lines)) {
       x <- lines[[i]]$x
       y <- lines[[i]]$y
       z <- rep(lines[[i]]$level, length(x))
       rgl::lines3d(x, y, z)
     }
 # }

}
