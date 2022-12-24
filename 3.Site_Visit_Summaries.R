## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~~~~~~~~~~~~ Make Summary Objects ~~~~~~~~~~~~~ 

## Making siteinfo and roundinfo
## These are summary objects where each row is a site or round
## respectively. This is where we will store our habitat covariates!
siteinfo  <- unique(trapinfo[,c("FireYear","Plot", "SiteType","siteID")])
roundinfo <- unique(trapinfo[,c("FireYear","Plot", "Round", "TrapJulian","siteID","roundID")])

## Burn Status (referred to as "sitetype" in the code) and
## canopyopenness are both site characteristics and will thus go into
## siteinfo.

## Floral species richness and floral abundance are both visit (round) level
## variables, as we collected that data for each of the times we went
## to each site. These will go into roundinfo. 

## Finally, we will average across rounds to get an average site level
## floral abundance and floral species richness that we will put into
## siteinfo. This will be useful for modeling, becasue site occupancy
## (our metric of interest) is only modeled at the site level (we
## assume that the occupancy of a site doesnt change from visit to
## visit).




## ~~~~~~~~~~~~~~ Adding information to those Summary Obects   ~~~~~~~~~~~~~~ 

## ~~~~~~~~ Siteinfo ~~~~~~~~

## To get our information from our full dataset to our summary
## datasets, we will use custom functions applied via mapply().


## ~~~ Get the average latitude of each site ~~~

## To explain how this works, I'll use the below example of the
##   getlatitude() function. In our mapply call, we give it the vector of
##   FireYear and Plot, which means that every site will be iterated
##   through, For example, in the first iteration it will take the first
##   element of each of those vectors, which will mean that x <- 2009 and
##   y <- F1. Then, with those variables asigned, it executes the code
##   within the function. It finds all entries in the dataset with that
##   FireYear and Plot values and then returns the mean of all of their
##   latitudes into siteinfo! It will do this for every row of siteinfo,
##   for each combination of FireYear and Plot!

##      Note: Since originally writing this, I've learned that you can make one
##      function instead of many individual functions by using
##      do.call(), and as fun as that is, I beleive it would make this
##      already somewhat conceptually difficult code even less
##      understandable to people not familiar with this style of coding.  

getlatitude <- function(x,y){
  use <- which(trapinfo$FireYear == x ## Eg. x = 2009 
               &
               trapinfo$Plot == y)    ## Eg. y = "F1" 
  if (length(use) == 0) return (0)
  return(mean(trapinfo$TrapLat[use],na.rm=TRUE))
}
siteinfo[,"latitude"] <- mapply(getlatitude,
                                x = siteinfo$FireYear, ## ex. getlatitude(x=2009, y="F1")
                                y = siteinfo$Plot)


## ~~~~~ Get the total trap hours out at each site ~~~~~
gettimeout <- function(x,y){  
  use1 <- which(trapinfo$FireYear == x
                &
                trapinfo$Plot == y)  
  if (length(use1) == 0) return (0)
  return(sum(trapinfo$HoursOut[use1], na.rm=TRUE))
}

siteinfo[,"HoursOut"] <- mapply(gettimeout,
                                x = siteinfo$FireYear, 
                                y = siteinfo$Plot)

## ~~~~~ Get the average sampling day ~~~~~ 

getjulianday <- function(x,y){  
  use1 <- which(trapinfo$FireYear == x
                &
                trapinfo$Plot == y) 
  if (length(use1) == 0) return (0)
  return(mean(trapinfo$TrapJulian[use1], na.rm=TRUE))
}
siteinfo[,'trapjulian'] <- mapply(getjulianday,
                                  x = siteinfo$FireYear,
                                  y = siteinfo$Plot)


## ~~~~~ Get the total abundance of bumble bees at each site  ~~~~~ 

get.site.abundance <- function(yy,pp){ 
  keep <- which(trapbumbles$FireYear==yy
                &
                trapbumbles$Plot==pp)
  if(length(keep)==0)return(0)
  return(sum(trapbumbles$Abundance[keep], na.rm=TRUE))
}

siteinfo[,"Abundance"] <- mapply(get.site.abundance,
                                 yy = siteinfo$FireYear,
                                 pp = siteinfo$Plot)


## ~~~~~ Get the abundance of each species at each site ~~~~~

get.site.species.abund <- function(yy, pp, xx) {
  keep <- which(trapbumbles$FireYear == yy
                &
                trapbumbles$Plot == pp
                &
                trapbumbles$Species == xx) 
  if(length(keep) == 0) return(0)
  return(sum(trapbumbles[keep,"Abundance"]))  
}

##   Get the abundance for each species and put it into a new column
##   of species info for each species! 
for (ii in species.names){
  siteinfo[,ii] <- mapply(get.site.species.abund,
                          yy = siteinfo$FireYear,
                          pp = siteinfo$Plot,
                          xx = ii)
}


## ~~~~~ Turn those columns of siteinfo spp abundance into spp richness ~~~~~

## Get the columns that are labeled with our species names 
index <- which(colnames(siteinfo) %in% species.names)

## Get a vector of TRUEs and FALSEs that indicate for each species, if
## it's abundance is greater than zero at that site. Then sum that up,
## which outputs the number of TRUEs (recall: In R, TRUE + TRUE = 2) 
siteinfo[,"SpeciesRichness"] <- apply(siteinfo[, index] > 0,
                                      1,
                                      sum)

## ~~~~~ Get canopy openness of each site ~~~~~ 
get.canopy.openness <- function(x,y){
  keep <- which(GLA$FireYear == x
                &
                GLA$Plot==y)
  if(length(keep)==0) return(0)
  return(mean(GLA$CanopyOpenness[keep],na.rm=TRUE))
}
siteinfo[,"canopyopenness"] <- mapply(get.canopy.openness,
                                      x = siteinfo$FireYear,
                                      y = siteinfo$Plot)

## ~~~~~ Get the variance in the canopy openness ~~~~~ 
get.variance.canopy.openness <- function(x,y){
  keep <- which(GLA$FireYear == x
                &
                GLA$Plot == y)
  if(length(keep)==0) return(0)
  return(sd(GLA$CanopyOpenness[keep],na.rm=TRUE))
}
siteinfo[,"varcanopyopenness"] <- mapply(get.variance.canopy.openness,
                                     x = siteinfo$FireYear,
                                     y = siteinfo$Plot)

## ~~~~~ Get the floral abundance of each site ~~~~~ 
getfloralabundance <- function(yy,pp){
  keep <- which(veg$FireYear==yy
                &
                veg$Plot==pp)
  if(length(keep)==0)return(0)
  return(sum(veg$FlowerCount[keep]))
}
siteinfo[,"openflowerabund"] <- mapply(getfloralabundance,
                                   yy = siteinfo$FireYear,
                                   pp = siteinfo$Plot)


## ~~~~~ Get the number of flowering plants ~~~~~ 
getflowerplantabundance <- function(yy,pp){
  keep <- which(veg$FireYear == yy
              &
              veg$Plot == pp)
  if(length(keep) == 0)return(0)
  return(sum(veg$PlantCount[keep]))
}
siteinfo[,"flowerplantabundance"] <- mapply(getflowerplantabundance,
                                            yy = siteinfo$FireYear,
                                            pp = siteinfo$Plot)

## ~~~~~ Add Flowers per Plant ~~~~~
siteinfo$flowersperplant <- (siteinfo$openflowerabund)/(siteinfo$flowerplantabundance)


## ~~~~~ Add Plant Spp Richness ~~~~~  
SR.site <- aggregate(x  = veg$PlantLatinName,
                     by = list(veg$siteID),
                     function(x) length(unique(x[!is.na(x)])))
SR.site$flowerSRsite <- SR.site$x
SR.site              <- SR.site[order(SR.site$Group.1),]
index3               <- match(SR.site$Group.1,
                              siteinfo$siteID)
siteinfo$flowerSRsite <- SR.site$flowerSRsite[index3]
SR.site <- NULL


## ~~~~~ Add "region" to siteinfo ~~~~~
## set all value to zero then change the southern sites to 1 
siteinfo[,"region"]                          <- rep(0, nrow(siteinfo))
siteinfo[siteinfo$latitude < 52.45,'region'] <- 1


## ~~~~~ Adding Site Colours ~~~~~
## (useful to have for plotting later) 
siteinfo$col.vec.sitetype <- rep('red',length(siteinfo$SiteType))
siteinfo$col.vec.sitetype[siteinfo$SiteType=='Forest'] <- 'green4'



## ~~~ Roundinfo ~~~

## Now we add things to roundinfo!

## ~~~~~ Get total trap time out ~~~~~
gettimeout2 <- function(yy,pp,rr){ 
  keep <- which(trapinfo$FireYear == yy
                &
                trapinfo$Plot == pp
                &
                trapinfo$Round == rr)
  if(length(keep) == 0)return(0) 
  return(sum(trapinfo$HoursOut[keep], na.rm = TRUE))
}
roundinfo$HoursOut <- mapply(gettimeout2,
                             yy = roundinfo$FireYear,
                             pp = roundinfo$Plot,
                             rr = roundinfo$Round)

## ~~~~~ Abundance of bumble bees ~~~~~
getabundance2 <- function(yy,pp,rr){ 
  keep <- which(trapbumbles$FireYear == yy
                &
                trapbumbles$Plot == pp
                &
                trapbumbles$Round == rr)
  if(length(keep) == 0)return(0) 
  return(sum(trapbumbles$Abundance[keep]))
}
roundinfo$Abundance <- mapply(getabundance2,yy=roundinfo$FireYear,pp=roundinfo$Plot, rr=roundinfo$Round)


## ~~~~~ Get floral abundance ~~~~~ 
getfloralabundance2 <- function(yy,pp,rr){
  keep <- which(veg$FireYear == yy
                &
                veg$Plot == pp
                &
                veg$Round == rr)
  if(length(keep) == 0)return(0) #ex 12 entries for 2017 F2 round 1 
  return(sum(veg$FlowerCount[keep]))
}
roundinfo$openflowerabund <- mapply(getfloralabundance2,
                                    yy = roundinfo$FireYear,
                                    pp = roundinfo$Plot,
                                    rr = roundinfo$Round)


## ~~~~~ Add plant species richness ~~~~~

## Aggrefate the plant names by their round and then get the length of
## the unique names for each site visit (round) 
SR.visit <- aggregate(x  = veg$PlantLatinName,
                      by = list(veg$roundID),
                      function(x) length(unique(x[!is.na(x)]))
                      )
## Rename for clairity
SR.visit$flowerSRvisit <- SR.visit$x

## Make sure theyre in order and add to roundinfo
index                   <- match(SR.visit$Group.1, roundinfo$roundID)
roundinfo$flowerSRvisit <- SR.visit$flowerSRvisit[index]



## ~~~~~~ Total bees per hour caught at each site ~~~~~
siteinfo$abundanceperhour  <- siteinfo $Abundance / siteinfo$HoursOut
roundinfo$abundanceperhour <- roundinfo$Abundance /roundinfo$HoursOut
