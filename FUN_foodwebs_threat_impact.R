'%!in%' <- function(x,y)!('%in%'(x,y))

### correct taxonomy ####
## returns a vector with corrected names based on correspondance csv table "taxonomic_synonyms.csv"
  correct_sppnames <- function(sppnames2correct, pathtoBioticData ="~/LECA/PhD/Data/BioticData/Taxonomy/"){
  sppnames2correct <- gsub(" ", "_", x = sppnames2correct)
  taxo_syn <- read.csv(file = paste0(pathtoBioticData, "2022_Taxonomic_Synonyms.csv"), sep = ';')
  taxo_syn[, 1] <- gsub(" ", "_", x = taxo_syn[, 1])
  taxo_syn[, 2] <- gsub(" ", "_", x = taxo_syn[, 2])
    for (i in 1:nrow(taxo_syn)){
    j <- taxo_syn$Synonyms[i]
    sppnames2correct[sppnames2correct == j] <- taxo_syn$In.Maiorano[i]
  }
  return(sppnames2correct)
}

### given a food web. (predators in rows, prey in columns)
### given threats x species matrix
### compute total number of interactions that are threatened in the food web

## I want: the number of interactions that are threatened, by each threat type. 
count.threatened.interactions <- function(adjm, threattable, species = intersect(rownames(adjm), rownames(threattable))){
  ### restrict to intersect list of species
  adjm <- adjm[species, ]
  adjm <- adjm[, species]
  diag(adjm) <- 0 # remove cannibalism!
  threattable <- threattable[species, ]
  ### initialize count of threatened links per threat 
  sum.threatened.links <- rep(NA, ncol(threattable))
  names(sum.threatened.links) <- colnames(threattable)
  foodweb <- adjm
  ## retrieve ID of all the species threatened by threat i
  for (threat.index in 1:ncol(threattable)){
    thr.spp <- rownames(threattable[which(threattable[, threat.index] == 1),]) ### list of threatened species
    for (sppindex in 1:length(thr.spp)){
      S1 <- sum(foodweb[thr.spp[sppindex],]) ### number of interactions where thr.spp number i is the predator
      S2 <- sum(foodweb[,thr.spp[sppindex]]) ### number of interactions where thr.spp number i is the prey
      sum.threatened.links[threat.index] <- sum(c(sum.threatened.links[threat.index], (S1+S2)), na.rm = T)
      # set to zero this species' links so as to not count the same link twice (if the interacting species are also threatened). 
      foodweb[thr.spp[sppindex],] <- 0
      foodweb[,thr.spp[sppindex]] <- 0 
    }
    ### reload the entire food web for the next threat
    foodweb <- adjm
  }
  return(sum.threatened.links)
}



### create list of local food webs for given list of pixels 

create_local_foodwebs <- function(px_sp, g.metaweb, pixels = rownames(px_sp)){
  px_sp$PAGENAME <- NULL ### this won't change anything if column Pagename does not exist
  species = intersect(V(g.metaweb)$name, colnames(px_sp))
  px_sp <- px_sp[pixels, species]
  g.metaweb <- induced.subgraph(g.metaweb, V(g.metaweb)$name %in% species)

  # initializing the list of local graphs
  g.list <- c()
  
  for (p in 1:nrow(px_sp)){
    local_sp <- colnames(px_sp[p, ])[apply(px_sp[p, ], 2, function(u) any(u==1))]
    g_p <- induced_subgraph(g.metaweb, V(g.metaweb)$name %in% local_sp)
    g.list <- c(g.list, list(g_p))
    print(p)
  }
  return(g.list)
}


#################################################
# Function to transform spp distribution database files into rasters
fun.dbf2raster <- function(SPPPA, mask.dir = NULL){
  # SPPPA must be in this format - first colmun with CELL ID and second column with the value (to plot)
  if(is.null(mask.dir)) stop("Must specify the mask file directory")
  library(raster)
  
  maskID <- read.dbf(list.files(path = mask.dir, full.names = TRUE, pattern = ".img.vat.dbf$"))
  maskk <- raster(x = list.files(path = mask.dir, full.names = TRUE, pattern = ".img$"))
  
  spp <- maskID
  spp$val <- NA
  spp$PageName <- as.character(spp$PageName)
  row.names(spp) <- spp$PageName
  
  SPPPA[, 1] <- as.character(SPPPA[, 1])
  SPPPA[,2] <- as.numeric(as.character(SPPPA[,2]))
  row.names(SPPPA) <- SPPPA[, 1]
  
  cellID <- as.character(SPPPA[, 1])
  if( nrow(spp[cellID,]) != nrow(SPPPA[cellID,])) stop("Cell IDs do not match")
  spp[cellID,"val"] <- SPPPA[cellID,2]

  #### keep only pixels in cellID  
  cells2keep <- rownames(maskID)[which(maskID$PageName %in% cellID)]
  xx <- raster::values(maskk)
  xx[which(xx %!in% as.numeric(cells2keep)) ] <- NA

  # fill in the values 
  xx[!is.na(xx)] <- spp$val[xx[!is.na(xx)]]
  
  raster::values(maskk) <- xx
  return(maskk)
}
