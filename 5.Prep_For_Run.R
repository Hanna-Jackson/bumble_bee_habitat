## Written by: Hanna Jackson hmj2@sfu.ca

## Lets make sure you have the right packages installed!
for (ii in  c("coda","rjags","R2jags")){
  if( isFALSE(ii %in% installed.packages()[,"Package"]) ){
    install.package(ii)
  }
}

## Load the required packages 
library(coda)
library(rjags)
library(R2jags)

## ~~~ Some functions we'll need ~~~ 
expit <- function(x) 1/(1+exp(-x))
logit <- function(x) log(x/(1-x))  

## My initial values - JAGS needs this 
my.inits <- function(){
  list(Z = array(1,
                 dim = c(length(dimnames(JAGS.arr)[["site"]]),
                         length(dimnames(JAGS.arr)[["species"]]))))
}

my.scale <- function(x){
   return((x-mean(x))/sd(x))
}

#Taking the log of open flower abundance 
JAGS.visit[,,"openflowerabundvisit"] <- log(JAGS.visit[,,"openflowerabundvisit"]+1)
JAGS.site [,"openflowerabundsite"]   <- log(JAGS.site [,"openflowerabundsite"]  +1)

## Package everything into a list for use in JAGS! 
my.data <- list(X                 = JAGS.arr,
                nsite             = dim(JAGS.arr)[1],
                nvisit            = dim(JAGS.arr)[2],
                nspecies          = dim(JAGS.arr)[3],
                ## Site Variables
                sitetype            = JAGS.site$sitetype,           
                canopyopenness      = as.numeric(my.scale(JAGS.site$canopyopenness)),
                region              = JAGS.site$region,
                latitude            = as.numeric(my.scale(JAGS.site$latitude)),
                fireyear            = as.numeric(as.character(2019-(JAGS.site$fireyear))), 
                openflowerabundsite = as.numeric(my.scale(JAGS.site$openflowerabundsite)),
                flowerSRsite        = as.numeric(my.scale(JAGS.site$flowerSRsite)),
                ## Visit Variables 
                hoursout             = my.scale(JAGS.visit[,,"hoursout"]),     
                trapjulian           = my.scale(JAGS.visit[,,"trapjulian"]),    
                openflowerabundvisit = my.scale(JAGS.visit[,,"openflowerabundvisit"]),
                flowerSRvisit        = my.scale(JAGS.visit[,,"flowerSRvisit"])
                )
