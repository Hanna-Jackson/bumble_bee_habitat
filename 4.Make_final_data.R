## Written by: Hanna Jackson hmj2@sfu.ca

## ~~~~~~~~~~~~~~ Preparing the data to go into our multidimensional array ~~~~~~~~~~~~~~

## Create a data frame with all unique site round AND species 
site.visit.spp <- unique(trapbumbles[,c("siteID",
                                        "Round",
                                        "Species")])

## Reorder the rows 
site.visit.spp <- site.visit.spp[order(site.visit.spp$siteID),]

## Get the abundance for each row (number of each species found in each round)
get.abundance4 <- function(xx,yy,ss){
  keep <- which(trapbumbles[,'Round']   == xx &
                trapbumbles[,'Species'] == ss &
                trapbumbles[,'siteID']  == yy )
  if(length(keep)==0)return(0)
  return(sum(trapbumbles[keep,'Abundance']))
}
site.visit.spp['Abundance'] <- mapply(get.abundance4,
                                      xx = site.visit.spp$Round,
                                      ss = site.visit.spp$Species,
                                      yy = site.visit.spp$siteID)

## Reshape to wide so species each get their own column with their abundance as the values 
site.visit.spp <- reshape(site.visit.spp,
                          timevar   = "Species",            ## Each species gets its own column
                          v.names   = "Abundance",          ## What to put in the new species columns
                          idvar     = c("siteID", "Round"), ## Variables that remain the same
                          direction = "wide")

## Now split it so each column is a unique species-round 
site.visit.spp <- reshape(site.visit.spp ,
                          timevar   = "Round" ,
                          idvar     = c("siteID"),
                          direction = "wide")

## Set all NAs to zeros - round that didnt have that species 
site.visit.spp[is.na(site.visit.spp)] <- 0

## Drop the unneeded columns and reorder those left over
drop           <- c("Abundance.NA.1","Abundance.NA.2","siteID")
site.visit.spp <- site.visit.spp[, !(names(site.visit.spp) %in% drop)]
site.visit.spp <- site.visit.spp[,order(colnames(site.visit.spp))]

## Convert to matrix
site.visit.spp <- data.matrix(site.visit.spp)



## ~~~~~~~~~~~~~~ Make and fill the multidimensional occupancy array ~~~~~~~~~~~~~~

## Use that object to make a 3D array (site x visit x species)
JAGS.arr  <- array(site.visit.spp,
                   dim = c(nsite    = 26,
                           nvisit   = 2,
                           nspecies = 10))

## Turn all non-zero values to 1 (1 = site is occupied) 
JAGS.arr  <- (JAGS.arr > 0) * 1

## Rename the dimensions
dimnames(JAGS.arr) <- list(site    = siteinfo$siteID,
                           visit   = c("v.1", "v.2"),
                           species = species.names[order(species.names)])



## ~~~~~~~~~~~~~~ Prep site variables (make JAGS.site) ~~~~~~~~~~~~~~

## Make JAGS.site, the matrix that all of our data will go into
JAGS.site <- matrix(NA,
                    nrow = nrow(siteinfo),
                    ncol = 1)
JAGS.site <- data.frame(JAGS.site)

## Ensure that siteinfo and JAGS.site rows are in the same order 
index     <- match(siteinfo$siteID, rownames(JAGS.arr)) 
siteinfo  <- siteinfo[index,]

## Add into JAGS.site
JAGS.site[,"siteID"]              <- siteinfo[,"siteID"]
JAGS.site[,"sitetype"]            <- as.integer(as.factor(siteinfo[,'SiteType'])) - 1 ## 0 is fire and 1 is forest
JAGS.site[,"canopyopenness"]      <- siteinfo[,'canopyopenness']
JAGS.site[,"openflowerabundsite"] <- siteinfo[,'openflowerabund']
JAGS.site[,"flowerSRsite"]        <- siteinfo[,'flowerSRsite']
JAGS.site[,"latitude"]            <- siteinfo[,'latitude']
JAGS.site[,"region"]              <- siteinfo[,'region']

JAGS.site <- JAGS.site[,-1]



## ~~~~~~~~~~~~~~ Prep visit variables (make JAGS.visit) ~~~~~~~~~~~~~~ 

V1 <- roundinfo[roundinfo$Round==1,]
V2 <- roundinfo[roundinfo$Round==2,]

index <- match(V1$siteID,
                row.names(JAGS.arr))

## Making JAGS.visit 
temp <- data.frame(trapjulian.V1           = V1$TrapJulian      [index],
                   trapjulian.V2           = V2$TrapJulian      [index], 
                   hoursout.V1             = V1$HoursOut        [index],
                   hoursout.V2             = V2$HoursOut        [index], 
                   openflowerabundvisit.V1 = V1$openflowerabund [index],
                   openflowerabundvisit.V2 = V2$openflowerabund [index], 
                   flowerSRvisit.V1        = V1$flowerSRvisit   [index],
                   flowerSRvisit.V2        = V2$flowerSRvisit   [index])
temp <- data.matrix(temp)

JAGS.visit <- array(temp,
                    dim = c(nsite      = 26,
                            nvisit     = 2,
                            nvariables = 4))

dimnames(JAGS.visit) <- list(site      = siteinfo$siteID,
                             visit     = c("v.1","v.2"),
                             variables = c("trapjulian","hoursout","openflowerabundvisit", "flowerSRvisit"))




## ~~~~~~~~~~~~~~ Saving ~~~~~~~~~~~~~~                                                                                                                                                                            

save(JAGS.arr,   file = 'data_for_run/JAGS.arr.RData')
save(JAGS.visit, file = 'data_for_run/JAGS.visit.RData')
save(JAGS.site,  file = 'data_for_run/JAGS.site.RData')


