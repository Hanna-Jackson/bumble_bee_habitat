## Written by Hanna Jackson hmj2@sfu.ca


## See Model_Main.R for comments describing how to interpret the code
## and what each section means

## This file is a copy-paste of Model_Main.R, with the only changes
## occuring by commenting out lines and adding the interaction between
## site type and canopy openness. This is to make it easier to
## comapre which parameters have been removed from which models. 


my.model <- function() {
  ## ~~~~~~~~~~~~ PRIORS ~~~~~~~~~~~~~~ 
  psi.0                            ~ dnorm(0,0.001) 
  psi.sitetype                     ~ dnorm(0,0.001)
  psi.canopyopenness               ~ dnorm(0,0.001)
  psi.openflowerabundsite          ~ dnorm(0,0.001)
  ## psi.flowerSRsite              ~ dnorm(0,0.001)
  psi.sitetypexcanopyopenness      ~ dnorm(0,0.001)
  psi.sitetypexopenflowerabundsite ~ dnorm(0,0.001) ## Added for this model

  p.0                    ~ dnorm(0,0.001)
  p.sitetype             ~ dnorm(0,0.001)
  p.canopyopenness       ~ dnorm(0,0.001)
  p.openflowerabundvisit ~ dnorm(0,0.001)
  p.flowerSRvisit        ~ dnorm(0,0.001)
  p.hoursout             ~ dnorm(0,0.001) 
  p.trapjulian           ~ dnorm(0,0.001) 

  ## ~~~ Random effect of species on occupancy ~~~
  sigma.psi.sp   ~ dunif(0,10)
  tau.psi.sp    <- 1/(sigma.psi.sp*sigma.psi.sp)
  for(species in 1:nspecies) {
    psi.sp[species] ~ dnorm(0, tau.psi.sp) 
  } 

  
  ## ~~~~~~~~~~~~~~~ MODEL ~~~~~~~~~~~~~~~ 
  for(site in 1:nsite) {
    
    for(species in 1:nspecies){
      logit(psi[site, species]) <- psi.0                        +
        psi.sp[species]                                         +
        psi.sitetype                * sitetype           [site] +
        psi.canopyopenness          * canopyopenness     [site] +
        psi.openflowerabundsite     * openflowerabundsite[site] +
        ## psi.flowerSRsite         * flowerSRsite       [site] +
        psi.sitetypexcanopyopenness * sitetype[site] * canopyopenness[site] +
        psi.sitetypexopenflowerabundsite * sitetype[site] * openflowerabundsite[site]  ## Added for this model
      
      for(visit in 1:nvisit){ 
        logit(p[site,visit, species]) <- p.0                       +
          p.sitetype             * sitetype            [site]       +
          p.hoursout             * hoursout            [site,visit] +
          p.trapjulian           * trapjulian          [site,visit] +
          p.openflowerabundvisit * openflowerabundvisit[site,visit] +
          p.flowerSRvisit        * flowerSRvisit       [site,visit] +
          p.canopyopenness       * canopyopenness      [site]       
      } 
    } 
  }  
  

  ## ~~~~~~~~~~~~~~ LIKELIHOOD ~~~~~~~~~~~~~~~
  for(site in 1:nsite) {
    for(species in 1:nspecies){
      ## Occurrence
      Z[site, species] ~ dbern(psi[site, species]) 
      for(visit in 1:nvisit) {
        p.eff[site, visit, species] <- Z[site, species] * p[site,visit,species]
        ## Detection
        X[site,visit,species] ~ dbern(p.eff[site,visit, species])
      }
    }
  }
  
} ## End of model 

## Specify the parameters that JAGS will store the values of
my.params <- c('p.0',
               'p.sitetype',
               'p.hoursout', 
               'p.trapjulian',
               'p.canopyopenness',
               "p.openflowerabundvisit", 
               "p.flowerSRvisit",
               'p.sitetype',
               
               'psi.0', 
               'psi.sp',
               'sigma.psi.sp',
               'psi.sitetype',
               'psi.canopyopenness',
               "psi.openflowerabundsite",
               "psi.sitetypexopenflowerabundsite", ## Added for this model
               ## "psi.flowerSRsite",
               'psi.sitetypexcanopyopenness'
               )


