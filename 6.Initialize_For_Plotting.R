## Written by: Hanna Jackson hmj2@sfu.ca

expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))

load("data_for_run/JAGS.arr.RData"  , verbose = TRUE)
load('data_for_run/JAGS.visit.RData', verbose = TRUE)
load('data_for_run/JAGS.site.RData' , verbose = TRUE)

## Do the necessary modifications: 
JAGS.visit[,,"openflowerabundvisit"] <- log(JAGS.visit[,,"openflowerabundvisit"]+1)
JAGS.site[,"openflowerabundsite"]    <- log(JAGS.site[,"openflowerabundsite"]   +1)

## ~~~ Making a flat JAGS.visit for plotting later ~~~ 
JAGS.visit.flat <- rbind(JAGS.visit[,1,],JAGS.visit[,2,])
site.names      <- rownames(JAGS.visit.flat)
JAGS.visit.flat <- as.data.frame(rbind(JAGS.visit[,1,],JAGS.visit[,2,]))
JAGS.visit.flat$siteID <- site.names
JAGS.site.sub          <- JAGS.site[,c('siteID', 'sitetype', 'canopyopenness')]
visit.flat   <- merge(JAGS.visit.flat, JAGS.site.sub, by='siteID')


## Saving the unscaled data 
unscaled <- list(X                 = JAGS.arr,
                 nsite             = dim(JAGS.arr)[1],
                 nvisit            = dim(JAGS.arr)[2],
                 nspecies          = dim(JAGS.arr)[3],
                 ## Site
                 sitetype            = JAGS.site$sitetype,           
                 canopyopenness      = as.numeric(JAGS.site$canopyopenness),     
                 varcanopyopenness   = as.numeric(JAGS.site$varcanopyopenness),  
                 fireyear            = as.numeric(as.character(2019-(JAGS.site$fireyear))), 
                 openflowerabundsite = as.numeric(JAGS.site$openflowerabundsite),
                 flowerSRsite        = as.numeric(JAGS.site$flowerSRsite),
                 ## Visit
                 hoursout             = JAGS.visit[,,"hoursout"],     
                 trapjulian           = JAGS.visit[,,"trapjulian"],    
                 openflowerabundvisit = JAGS.visit[,,"openflowerabundvisit"],
                 flowerSRvisit        = JAGS.visit[,,"flowerSRvisit"]
                 )

## Making objects 
summ     <- bugs$BUGSoutput$summary
sims.arr <- bugs$BUGSoutput$sims.array
sims.mat <- bugs$BUGSoutput$sims.matrix  
columns  <- c('mean','2.5%','97.5%', 'Rhat')

## Make a colour transparent 
makeTransparent <- function(..., alpha=0.5) {
   if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
   alpha = floor(255*alpha)  
   newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)
   .makeTransparent = function(col, alpha) {
      rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
   }
   apply(newColor, 2, .makeTransparent, alpha=alpha)
}


## Nice little pdf function
pdf.f <- function(f, file, ...) {
  cat(sprintf('Writing %s\n', file))
  pdf(file, ...)
  on.exit(dev.off())
  f()
}


## Choosing and Exploring Colours 
cex.lab <- 1.2

## Fire colours 
fire.col       <-"darkgoldenrod1"
light.fire.col <-"lightgoldenrod"
dark.fire.col  <-"darkgoldenrod3"

forest.col        <-"darkslategray4"
dark.forest.col.2 <-"darkslategray"
dark.forest.col   <-"cadetblue4"

generic.col     <-"black" 
dark.generic.col<-"steelblue4"
overlap.col     <-"grey80"

overlap.col     <- "gray63"

JAGS.site$sitetypecol <- NA
JAGS.site[which(JAGS.site$sitetype==0),"sitetypecol"] <-fire.col
JAGS.site[which(JAGS.site$sitetype==1),"sitetypecol"] <-forest.col

JAGS.site$darksitetypecol <- NA
JAGS.site[which(JAGS.site$sitetype==0),"darksitetypecol"] <-dark.fire.col
JAGS.site[which(JAGS.site$sitetype==1),"darksitetypecol"] <-dark.forest.col

visit.flat$sitetypecol <- NA
visit.flat[which(visit.flat$sitetype==0),"sitetypecol"] <-fire.col
visit.flat[which(visit.flat$sitetype==1),"sitetypecol"] <-forest.col


## Subsetting for ease of use in plotting later 
fire  <- JAGS.site[JAGS.site$sitetype==0,]
forest <- JAGS.site[JAGS.site$sitetype==1,]

## Summarize trap hours out to the site level 
hoursout.site <- apply(JAGS.visit[,,'hoursout'],1,sum)

if( all(names(hoursout.site) == JAGS.site$siteID) ){
  hoursout.fire   <- hoursout.site[JAGS.site$sitetype==0] 
  hoursout.forest <- hoursout.site[JAGS.site$sitetype==1]
} else {
  stop("Indexing is incorrect") }
